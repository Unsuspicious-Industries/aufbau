//! Tests for deduplication invariants in the agenda parser.

use super::*;
use crate::domains::typing::TypingDomain;
use crate::engine::grammar::SPG;
use crate::engine::parse::arena::{AltRange, ArenaNode, NodeId, NodeStatus, Span};
use crate::engine::parse::parser::Completion;
use crate::engine::parse::Item;
use crate::semantics::Obligations;
use std::collections::HashMap;

fn push_test_node<D: crate::semantics::domain::ConstraintDomain>(
    parser: &mut TypedParser<D, impl crate::semantics::SemanticRuntime>,
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
            status: NodeStatus::Exact,
            semantic_complete: true,
            evidence: 0,
            effect: None,
            binding_map: HashMap::new(),
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
        ctx: 0,
        obligations: Obligations::empty(),
        children: vec![],
    }
}

#[test]
fn enqueue_process_deduplicates_identical_key() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a'").unwrap();
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
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a' 'b'").unwrap();
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
fn complete_records_result_span_on_first_call() {
    let grammar = SPG::<TypingDomain>::load("A ::= 'x'\nStart ::= A").unwrap();
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
            ctx: 0,
        })
        .unwrap();

    let spans = parser.tables.results.get(&(a_nt, 0, 0)).unwrap();
    assert_eq!(spans, &vec![1]);
}

#[test]
fn complete_keeps_result_span_unique_but_allows_distinct_nodes() {
    // Property: result spans are memoized once, but every completed node may
    // still wake waiters because resumption depends on child type/effect.
    let grammar = SPG::<TypingDomain>::load("A ::= 'x'\nStart ::= A").unwrap();
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
            ctx: 0,
        })
        .unwrap();
    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node2,
            ctx: 0,
        })
        .unwrap();

    let spans = parser.tables.results.get(&(a_nt, 0, 0)).unwrap();
    assert_eq!(spans, &vec![1]);
}

#[test]
fn completed_nodes_records_all_semantic_nodes_at_same_span() {
    let grammar = SPG::<TypingDomain>::load("A ::= 'x'\nStart ::= A").unwrap();
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
            ctx: 0,
        })
        .unwrap();
    parser
        .complete_for_test(Completion {
            nt: a_nt,
            start: 0,
            end: 1,
            node: node2,
            ctx: 0,
        })
        .unwrap();

    let recorded = parser.tables.completed_nodes.get(&(a_nt, 0, 1, 0)).unwrap();
    assert_eq!(recorded.len(), 2);
    assert!(recorded.contains(&node1));
    assert!(recorded.contains(&node2));
}

#[test]
fn complete_wakes_waiter_for_each_distinct_completed_node() {
    // Property: semantically distinct completed nodes at the same span may all
    // wake waiters because resumption depends on child summaries, not span only.
    let grammar = SPG::<TypingDomain>::load("A ::= 'x'\nStart ::= A 'y'").unwrap();
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
            ctx: 0,
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
            ctx: 0,
        })
        .unwrap();
    let items_after_second = parser.tables.agenda.len();

    assert!(items_after_first > 0);
    assert!(
        items_after_second > 0,
        "same span may re-wake when node differs"
    );
}

#[test]
fn results_table_records_end_position_once() {
    let grammar = SPG::<TypingDomain>::load("A ::= 'x'\nStart ::= A").unwrap();
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
                ctx: 0,
            })
            .unwrap();
    }

    let ends = parser.tables.results.get(&(a_nt, 0, 0)).unwrap();
    assert_eq!(ends.len(), 1);
    assert_eq!(ends[0], 1);
}

#[test]
fn parse_left_recursive_produces_bounded_node_count() {
    let grammar = SPG::<TypingDomain>::load("A ::= 'x' | A 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("x x x x x", 0).unwrap();
    assert!(ast.is_complete());
    assert!(ast.node_count() < 50);
}
