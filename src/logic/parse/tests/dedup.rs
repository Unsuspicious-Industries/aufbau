//! Tests for deduplication invariants in the agenda parser.

use super::*;
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{AltRange, ArenaNode, NodeId, NodeStatus, Span};
use crate::logic::parse::parser::Completion;
use crate::logic::parse::Item;
use crate::logic::typing::{ContextTransition, Obligations};

fn push_test_node(
    parser: &mut TypedParser<impl crate::logic::typing::TypingRuntime>,
    nt: usize,
    start: usize,
    end: usize,
) -> NodeId {
    parser.arena.push_node(
        ArenaNode {
            nt,
            span: Span {
                start: start as u32,
                end: end as u32,
            },
            status: NodeStatus::Complete,
            ty: 0,
            open: false,
            ctr: Some(ContextTransition::identity(0)),
            bindings: vec![],
            alts: AltRange { start: 0, len: 0 },
        },
        vec![],
    )
}

fn test_item(prod: ProdId, pos: usize) -> Item {
    Item {
        prod,
        dot: 0,
        start: pos,
        pos,
        ctr: ContextTransition::identity(0),
        obligations: Obligations::empty(),
        children: vec![],
    }
}

#[test]
fn enqueue_process_deduplicates_identical_key() {
    let grammar = Grammar::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("a").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();
    let item = test_item((start_nt, 0), 0);

    parser.enqueue_process_for_test(item.clone());
    parser.enqueue_process_for_test(item.clone());

    assert_eq!(parser.tables.agenda.len(), 1);
    assert_eq!(parser.tables.seen_process.len(), 1);
}

#[test]
fn enqueue_process_allows_different_syntactic_positions() {
    let grammar = Grammar::load("Start ::= 'a' 'b'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("a b").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();

    let item_dot0 = test_item((start_nt, 0), 0);
    let mut item_dot1 = item_dot0.clone();
    item_dot1.dot = 1;
    item_dot1.pos = 1;

    parser.enqueue_process_for_test(item_dot0);
    parser.enqueue_process_for_test(item_dot1);

    assert_eq!(parser.tables.agenda.len(), 2);
}

#[test]
fn enqueue_process_distinguishes_different_ctx() {
    let grammar = Grammar::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("a").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();

    let item1 = test_item((start_nt, 0), 0);
    let mut item2 = item1.clone();
    item2.ctr = ContextTransition::identity(99);

    parser.enqueue_process_for_test(item1);
    parser.enqueue_process_for_test(item2);

    assert_eq!(parser.tables.agenda.len(), 2);
}

#[test]
fn complete_inserts_into_seen_complete_on_first_call() {
    let grammar = Grammar::load("A ::= 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("x").unwrap();

    let a_nt = parser.grammar.nt_index("A").unwrap();
    let node_id = push_test_node(&mut parser, a_nt, 0, 1);

    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node_id,
        })
        .unwrap();

    // Main loop dedup is span-based: (nt, start, end).
    // Type-aware propagation happens in lift_frontier, not here.
    assert!(parser.tables.seen_complete.contains(&(a_nt, 0, 1)));
    assert_eq!(parser.tables.seen_complete.len(), 1);
}

#[test]
fn complete_deduplicates_same_span() {
    // Main loop: two nodes at the same span are dedup'd — only the first
    // triggers waiter resumption. This is classic Earley behavior.
    // Type-distinct propagation is handled by lift_frontier separately.
    let grammar = Grammar::load("A ::= 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("x").unwrap();

    let a_nt = parser.grammar.nt_index("A").unwrap();
    let node1 = push_test_node(&mut parser, a_nt, 0, 1);
    let node2 = push_test_node(&mut parser, a_nt, 0, 1);

    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node1,
        })
        .unwrap();
    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node2,
        })
        .unwrap();

    // Same span → one entry in seen_complete
    assert_eq!(parser.tables.seen_complete.len(), 1);
}

#[test]
fn completed_nodes_records_all_nodes_regardless_of_seen_complete() {
    let grammar = Grammar::load("A ::= 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("x").unwrap();

    let a_nt = parser.grammar.nt_index("A").unwrap();
    let node1 = push_test_node(&mut parser, a_nt, 0, 1);
    let node2 = push_test_node(&mut parser, a_nt, 0, 1);
    assert_ne!(node1, node2);

    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node1,
        })
        .unwrap();
    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node2,
        })
        .unwrap();

    let recorded = parser.tables.completed_nodes.get(&(a_nt, 0, 1)).unwrap();
    assert_eq!(recorded.len(), 2);
    assert!(recorded.contains(&node1));
    assert!(recorded.contains(&node2));
}

#[test]
fn complete_wakes_waiter_only_on_first_call() {
    // Main loop uses span-based dedup: only the first completion at a span
    // wakes waiters. Type-distinct propagation is handled by lift_frontier.
    let grammar = Grammar::load("A ::= 'x'\nStart ::= A 'y'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("x y").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();
    let a_nt = parser.grammar.nt_index("A").unwrap();

    parser
        .process_for_test(test_item((start_nt, 0), 0))
        .unwrap();

    parser.tables.agenda.clear();
    parser.tables.seen_process.clear();

    let node1 = push_test_node(&mut parser, a_nt, 0, 1);
    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node1,
        })
        .unwrap();
    let items_after_first = parser.tables.agenda.len();

    parser.tables.agenda.clear();
    parser.tables.seen_process.clear();

    let node2 = push_test_node(&mut parser, a_nt, 0, 1);
    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node2,
        })
        .unwrap();
    let items_after_second = parser.tables.agenda.len();

    assert!(items_after_first > 0);
    assert_eq!(
        items_after_second, 0,
        "same span should not re-wake in main loop"
    );
}

#[test]
fn results_table_records_end_position_once() {
    let grammar = Grammar::load("A ::= 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    parser.set_input("x").unwrap();

    let a_nt = parser.grammar.nt_index("A").unwrap();

    for _ in 0..3 {
        let node = push_test_node(&mut parser, a_nt, 0, 1);
        parser
            .complete_for_test(Completion {
                nt: a_nt,
                start: 0,
                end: 1,
                node,
            })
            .unwrap();
    }

    let ends = parser.tables.results.get(&(a_nt, 0)).unwrap();
    assert_eq!(ends.len(), 1);
    assert_eq!(ends[0], 1);
}

#[test]
fn parse_left_recursive_produces_bounded_node_count() {
    let grammar = Grammar::load("A ::= 'x' | A 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("x x x x x", 0).unwrap();
    assert!(ast.is_complete());
    assert!(ast.node_count() < 50);
}
