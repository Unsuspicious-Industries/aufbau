use super::*;
use crate::engine::grammar::SPG;
use crate::engine::parse::arena::{ChildRef, Lexeme};
use crate::engine::structure::ast::FusionAST;
use crate::typing::Context;
use crate::typing::TypingSynth;

fn assert_status_model(ast: &FusionAST, grammar: &SPG) {
    let arena = ast.arena();
    let input_end = ast.segs().len();

    for id in 0..arena.node_count() {
        let node = arena.node(id).expect("arena node should exist");
        let status = node.status;
        let alts = arena.alts_for(id).expect("node should have alternatives");

        assert!(!alts.is_empty(), "node {id} has no packed alternatives");

        for alt in alts.iter() {
            let rhs_len = grammar
                .prod(alt.prod)
                .map(|prod| prod.rhs.len())
                .expect("packed alternative should reference a production");
            let all_children_complete = alt.children.iter().all(|child| arena.complete(child));
            let production_fully_recognized =
                alt.children.len() == rhs_len && all_children_complete;
            let any_child_open = alt.children.iter().any(|child| arena.open(child));

            for child in &alt.children {
                if let ChildRef::Terminal(Lexeme {
                    matched,
                    open: true,
                    ..
                }) = child
                {
                    assert_eq!(
                        matched.end as usize, input_end,
                        "terminal openness must only occur at the right frontier"
                    );
                }
            }

            match status {
                NodeStatus::Exact => {
                    assert!(
                        production_fully_recognized && !any_child_open,
                        "Closed node {id} must be fully recognized with no open child"
                    );
                }
                NodeStatus::Extensible => {
                    assert!(
                        production_fully_recognized && any_child_open,
                        "Extensible node {id} must be fully recognized with an open child"
                    );
                }
                NodeStatus::Prefix => {
                    assert!(
                        !production_fully_recognized,
                        "Partial node {id} must not be fully recognized"
                    );
                }
            }
        }
    }
}

#[test]
fn interior_extensible_terminal_does_not_make_wrapper_open() {
    let grammar = SPG::load("Word ::= /[a-z]+/\nStart ::= Word ':' '!'").unwrap();
    let mut parser = stub_parser(grammar);
    let ast = parser.parse("foo : !", 0).unwrap();
    let arena = ast.arena();
    let root_statuses: Vec<_> = ast
        .root_ids()
        .iter()
        .filter_map(|&id| arena.node(id).map(|node| node.status))
        .collect();

    assert!(
        ast.is_complete(),
        "interior regex terminal should not force inexact root: {:?}",
        root_statuses
    );

    assert!(
        (0..arena.node_count()).any(|id| {
            arena.node(id).is_some_and(|node| {
                node.span.start == 0 && node.span.end == 1 && node.status == NodeStatus::Exact
            })
        }),
        "interior regex child should be closed away from EOF"
    );
    assert!(ast.root_ids().iter().any(|&id| {
        arena
            .node(id)
            .is_some_and(|node| node.status == NodeStatus::Exact)
    }));
}

#[test]
fn eof_extensible_terminal_is_accepted_but_not_exact() {
    let grammar = SPG::load("Start ::= /[a-z]+/").unwrap();
    let mut parser = stub_parser(grammar);
    let ast = parser.parse("foo", 0).unwrap();

    assert!(
        ast.is_complete(),
        "single extensible token at EOF should count as complete prefix"
    );
    assert!(
        !ast.is_exact(),
        "single extensible token at EOF is not exact"
    );

    let arena = ast.arena();
    assert!(ast.root_ids().iter().any(|&id| {
        arena
            .node(id)
            .is_some_and(|node| node.status == NodeStatus::Extensible)
    }));
}

#[test]
fn prefix_match_inside_input_is_dead_not_frontier_open() {
    let grammar = SPG::load("Start ::= 'false' '!' ").unwrap();
    let mut parser = stub_parser(grammar);
    assert!(
        parser.parse("f !", 0).is_err(),
        "mid-input prefix of a terminal should be dead, not a frontier candidate"
    );
}

#[test]
fn exact_parse_requires_closed_root() {
    let grammar = SPG::load("Start ::= 'x'").unwrap();
    let mut synth = TypingSynth::new(grammar, "x");
    let ast = synth.parse_with(&Context::new()).unwrap();
    let arena = ast.arena();
    let root_statuses: Vec<_> = ast
        .root_ids()
        .iter()
        .filter_map(|&id| arena.node(id).map(|node| node.status))
        .collect();
    assert!(
        ast.is_complete(),
        "expected closed literal root, got {:?}",
        root_statuses
    );
    assert!(ast.is_exact(), "closed literal root should be exact");
}

#[test]
fn parser_nodes_satisfy_documented_status_model() {
    let cases = [
        ("Start ::= 'x'", "x"),
        ("Start ::= /[a-z]+/", "foo"),
        ("Start ::= 'x' 'y'", "x"),
        ("Word ::= /[a-z]+/\nStart ::= Word ':' Word", "foo : bar"),
    ];

    for (spec, input) in cases {
        let grammar = SPG::load(spec).unwrap();
        let mut parser = stub_parser(grammar.clone());
        let ast = parser.parse(input, 0).unwrap();

        assert_status_model(&ast, &grammar);
    }
}
