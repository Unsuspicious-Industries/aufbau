//! Tests for obligation-based pruning and seeding.

use super::*;
use crate::engine::grammar::SPG;

#[test]
fn seed_with_no_obligations_enqueues_all_alternatives() {
    let grammar = SPG::load("A ::= 'x' | 'y' | 'z'\nStart ::= A").unwrap();
    let mut parser = stub_parser(grammar);
    parser.set_input("x").unwrap();

    let a_nt = parser.grammar.nt_index("A").unwrap();
    parser.seed_for_test(a_nt, 0, 0);

    assert_eq!(parser.tables.agenda.len(), 3);
}

#[test]
fn seed_single_alt_enqueues_one() {
    let grammar = SPG::load("Start ::= 'a'").unwrap();
    let mut parser = stub_parser(grammar);
    parser.set_input("a").unwrap();

    let start_nt = parser.grammar.nt_index("Start").unwrap();
    parser.seed_for_test(start_nt, 0, 0);

    assert_eq!(parser.tables.agenda.len(), 1);
}

#[test]
fn prune_does_not_introduce_alternatives_not_in_grammar() {
    let grammar = SPG::load("A ::= 'x' | 'y'\nStart ::= A").unwrap();
    let mut parser = stub_parser(grammar);
    parser.set_input("x").unwrap();

    let a_nt = parser.grammar.nt_index("A").unwrap();
    let num_alts = parser
        .grammar
        .productions_at(a_nt)
        .map(|v| v.len())
        .unwrap_or(0);

    parser.seed_for_test(a_nt, 0, 0);

    for task in &parser.tables.agenda {
        if let crate::engine::parse::Task::Process(item) = task {
            assert!(item.prod.1 < num_alts);
        }
    }
}
