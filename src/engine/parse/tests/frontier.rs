//! Tests for frontier items and partial parse behavior.

use super::*;
use crate::engine::grammar::SPG;
use crate::engine::parse::Task;

#[test]
fn items_at_eof_go_to_frontier() {
    let grammar = SPG::load("Start ::= 'x' 'y'").unwrap();
    let mut parser = stub_parser(grammar);
    parser.set_input("x").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();
    parser.seed_for_test(start_nt, 0, 0);

    while let Some(task) = parser.tables.agenda.pop_front() {
        match task {
            Task::Process(item) => parser.process_for_test(item).unwrap(),
            Task::Complete(c) => parser.complete_for_test(c).unwrap(),
        }
    }

    assert!(
        !parser.tables.frontier.is_empty(),
        "item expecting 'y' at EOF should land in the frontier"
    );
    assert!(
        parser.tables.frontier.iter().any(|item| item.dot == 1),
        "frontier item should have dot=1 (consumed 'x', needs 'y')"
    );
}

#[test]
fn item_at_beginning_of_input_does_not_go_to_frontier() {
    let grammar = SPG::load("Start ::= 'x' 'y'").unwrap();
    let mut parser = stub_parser(grammar);
    parser.set_input("").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();
    parser.seed_for_test(start_nt, 0, 0);

    while let Some(task) = parser.tables.agenda.pop_front() {
        match task {
            Task::Process(item) => parser.process_for_test(item).unwrap(),
            Task::Complete(c) => parser.complete_for_test(c).unwrap(),
        }
    }

    assert!(
        !parser.tables.frontier.is_empty(),
        "even an un-started item goes to frontier at EOF"
    );
    assert!(
        parser.tables.frontier.iter().any(|item| item.dot == 0),
        "frontier item should have dot=0 (nothing consumed)"
    );
}

#[test]
fn partial_input_produces_partial_node() {
    let grammar = SPG::load("Start ::= 'x' 'y'").unwrap();
    let mut parser = stub_parser(grammar);
    let ast = parser.parse("x", 0).unwrap();
    assert!(!ast.is_complete());
    assert!(!ast.is_empty());
}

#[test]
fn empty_input_against_non_nullable_grammar_produces_partial_node() {
    let grammar = SPG::load("Start ::= 'a'").unwrap();
    let mut parser = stub_parser(grammar);
    let ast = parser.parse("", 0).unwrap();
    assert!(!ast.is_complete());
    assert!(!ast.is_empty());
}

#[test]
fn partial_node_has_partial_status() {
    let grammar = SPG::load("Start ::= 'x' 'y'").unwrap();
    let mut parser = stub_parser(grammar);
    let ast = parser.parse("x", 0).unwrap();

    assert!(!ast.is_empty());
    let arena = ast.arena();
    for &root_id in ast.root_ids() {
        let node = arena.node(root_id).unwrap();
        assert_eq!(node.status, crate::engine::parse::NodeStatus::Prefix);
    }
}

#[test]
fn frontier_item_cascades_completion_to_parent_waiter() {
    let grammar = SPG::load("A ::= 'x' 'y'\nStart ::= A 'z'").unwrap();
    let mut parser = stub_parser(grammar);
    let ast = parser.parse("x", 0).unwrap();

    assert!(!ast.is_empty());
    assert!(!ast.is_complete());
}

#[test]
fn full_input_has_no_frontier_items() {
    let grammar = SPG::load("Start ::= 'a' 'b'").unwrap();
    let mut parser = stub_parser(grammar);
    parser.set_input("a b").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();
    parser.seed_for_test(start_nt, 0, 0);

    while let Some(task) = parser.tables.agenda.pop_front() {
        match task {
            Task::Process(item) => parser.process_for_test(item).unwrap(),
            Task::Complete(c) => parser.complete_for_test(c).unwrap(),
        }
    }

    assert!(
        parser
            .tables
            .frontier
            .iter()
            .all(|item| item.pos == parser.segs().len()),
        "any remaining frontier items should all be at the end of input"
    );
}

#[test]
fn partial_node_has_concrete_type() {
    let grammar = SPG::load("Start ::= 'a' 'b'").unwrap();
    let mut parser = stub_parser(grammar);
    let ast = parser.parse("a", 0).unwrap();

    assert!(!ast.is_empty());
    let arena = ast.arena();
    for &root_id in ast.root_ids() {
        let node = arena.node(root_id).unwrap();
        assert_eq!(node.status, crate::engine::parse::NodeStatus::Prefix);
    }
}
