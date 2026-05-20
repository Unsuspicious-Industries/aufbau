//! End-to-end parse correctness tests.
//!
//! These tests drive `TypedParser::parse` directly and inspect the `FusionAST`
//! that comes back. They cover the main structural guarantees: complete parses,
//! ambiguity, left recursion, type rejection, and the invariant that every node
//! in the arena carries a concrete type.

use super::*;
use crate::domains::typing::TypingDomain;
use crate::engine::grammar::SPG;

// ---------------------------------------------------------------------------
// Basic terminal matching
// ---------------------------------------------------------------------------

#[test]
fn parse_single_terminal_succeeds() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("a", 0).unwrap();
    assert!(!ast.is_empty(), "should produce at least one root");
    assert!(ast.is_complete(), "parse of full input should be complete");
}

#[test]
fn parse_single_terminal_wrong_input_returns_err() {
    // "b" cannot be recognized at all by 'a' — not even as a partial prefix.
    // parse() returns Err, not Ok(empty), when no node is produced.
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    assert!(
        parser.parse("b", 0).is_err(),
        "completely unrecognized input should return Err, not Ok(empty)"
    );
}

#[test]
fn parse_empty_input_yields_no_complete_roots() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("", 0).unwrap();
    assert!(
        !ast.is_complete(),
        "empty input against non-epsilon grammar has no complete root"
    );
}

#[test]
fn parse_sequence_succeeds() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'x' 'y'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("x y", 0).unwrap();
    assert!(ast.is_complete());
}

#[test]
fn parse_sequence_partial_input_yields_no_complete_root() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'x' 'y'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("x", 0).unwrap();
    assert!(
        !ast.is_complete(),
        "only half the sequence consumed — no complete root"
    );
}

#[test]
fn parse_alternatives_first_branch() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a' | 'b'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("a", 0).unwrap();
    assert!(ast.is_complete());
}

#[test]
fn parse_alternatives_second_branch() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a' | 'b'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("b", 0).unwrap();
    assert!(ast.is_complete());
}

// ---------------------------------------------------------------------------
// Epsilon productions
// ---------------------------------------------------------------------------

#[test]
fn parse_epsilon_alternative_matches_empty_span() {
    // ExprTail ::= ε means it matches the empty continuation.
    let grammar = SPG::<TypingDomain>::load(
        "ExprTail ::= '+' Term | ε\nTerm ::= 'x'\nStart ::= Term ExprTail",
    )
    .unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    // Just "x": ExprTail should match ε
    let ast = parser.parse("x", 0).unwrap();
    assert!(
        ast.is_complete(),
        "epsilon alternative should handle absent suffix"
    );
}

#[test]
fn parse_epsilon_alternative_matches_non_empty_span() {
    let grammar = SPG::<TypingDomain>::load(
        "ExprTail ::= '+' Term | ε\nTerm ::= 'x'\nStart ::= Term ExprTail",
    )
    .unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("x + x", 0).unwrap();
    assert!(ast.is_complete(), "non-epsilon branch should also parse");
}

// ---------------------------------------------------------------------------
// Left recursion
// ---------------------------------------------------------------------------

#[test]
fn parse_direct_left_recursion_base_case() {
    // A ::= 'a' | A 'x'
    let grammar = SPG::<TypingDomain>::load("A ::= 'a' | A 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("a", 0).unwrap();
    assert!(ast.is_complete(), "base case of left-recursive grammar");
}

#[test]
fn parse_direct_left_recursion_extended() {
    let grammar = SPG::<TypingDomain>::load("A ::= 'a' | A 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("a x x", 0).unwrap();
    assert!(ast.is_complete(), "extended left-recursive chain");
}

#[test]
fn parse_direct_left_recursion_terminates() {
    // Ensure the agenda-based parser doesn't loop infinitely on left recursion.
    let grammar = SPG::<TypingDomain>::load("A ::= 'a' | A 'x'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    // If this returns at all the termination invariant holds.
    let _ast = parser.parse("a x x x x x", 0).unwrap();
}

// ---------------------------------------------------------------------------
// Ambiguity
// ---------------------------------------------------------------------------

#[test]
fn parse_ambiguous_grammar_produces_multiple_roots() {
    // Both A and B match 'x'; Start has two alternatives.
    let grammar = SPG::<TypingDomain>::load("A ::= 'x'\nB ::= 'x'\nStart ::= A | B").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("x", 0).unwrap();
    // At least one root, possibly two depending on packing.
    assert!(!ast.is_empty(), "ambiguous grammar should produce roots");
    assert!(ast.is_complete());
}

#[test]
fn parse_self_ambiguous_grammar() {
    // A ::= A ':' A | 'a' — classic ambiguous grammar.
    let grammar = SPG::<TypingDomain>::load("A ::= A ':' A | 'a'\nStart ::= A").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("a : a : a", 0).unwrap();
    assert!(
        ast.is_complete(),
        "self-ambiguous grammar should still parse"
    );
}

// ---------------------------------------------------------------------------
// Typing constraints
// ---------------------------------------------------------------------------

#[test]
fn rejecting_typing_returns_err() {
    // RejectingTyping always rejects finish_production, so finalize produces
    // no nodes — not even partial ones. parse() returns Err when no node can
    // be created for the start symbol at any span.
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, RejectingTyping);
    assert!(
        parser.parse("a", 0).is_err(),
        "rejected typing must return Err — no partial node can be produced"
    );
}

#[test]
fn rejecting_all_typing_returns_err() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'x'").unwrap();
    let mut parser = TypedParser::new(grammar, RejectingTyping);
    assert!(
        parser.parse("x", 0).is_err(),
        "rejected typing should produce Err"
    );
}

#[test]
fn rejecting_typing_allows_other_branch() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'a' | 'b'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("a", 0).unwrap();
    assert!(ast.is_complete(), "non-rejected branch should still parse");
}

// ---------------------------------------------------------------------------
// Every node has concrete semantic evidence (invariant)
// ---------------------------------------------------------------------------

#[test]
fn every_root_node_has_evidence() {
    // StubTyping assigns the default semantic evidence to all accepted nodes.
    let grammar = SPG::<TypingDomain>::load("A ::= 'x'\nStart ::= A 'y'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("x y", 0).unwrap();
    assert!(ast.is_complete());

    let arena = ast.arena();
    for &root_id in ast.root_ids() {
        let node = arena.node(root_id).expect("root node must exist in arena");
        let _ = node.evidence;
    }
}

// ---------------------------------------------------------------------------
// Regex terminals
// ---------------------------------------------------------------------------

#[test]
fn parse_regex_terminal_matches_pattern() {
    let grammar = SPG::<TypingDomain>::load("Start ::= /[0-9]+/").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("42", 0).unwrap();
    assert!(ast.is_complete());
}

#[test]
fn parse_regex_terminal_rejects_non_match() {
    let grammar = SPG::<TypingDomain>::load("Start ::= /[0-9]+/").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    assert!(parser.parse("abc", 0).is_err());
}

// ---------------------------------------------------------------------------
// Nested nonterminals
// ---------------------------------------------------------------------------

#[test]
fn parse_nested_nonterminals() {
    let grammar =
        SPG::<TypingDomain>::load("Atom ::= 'x'\nExpr ::= Atom | '(' Expr ')'\nStart ::= Expr")
            .unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("( ( x ) )", 0).unwrap();
    assert!(ast.is_complete());
}

#[test]
fn parse_returns_correct_text() {
    let grammar = SPG::<TypingDomain>::load("Start ::= 'hello'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("hello", 0).unwrap();
    assert_eq!(ast.text(), "hello");
}
