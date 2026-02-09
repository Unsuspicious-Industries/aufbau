//! Fun Language Constrained Completion Tests
//!
//! Focused behavioral tests for constrained generation on the `fun` grammar.
//! These use bounded search to stay practical on low-resource machines.

#![allow(dead_code)]

use super::*;
use crate::logic::partial::parse::Parser;

fn fun_grammar() -> Grammar {
    load_example_grammar("fun")
}

fn is_complete_and_typed(input: &str) -> bool {
    let grammar = fun_grammar();
    let mut parser = Parser::new(grammar.clone());
    match parser.partial(input) {
        Ok(ast) => ast.typed_complete(&grammar).is_ok(),
        Err(_) => false,
    }
}

fn append_and_check(input: &str, suffix: &str, with_space: bool) -> bool {
    let extended = if with_space {
        format!("{} {}", input, suffix)
    } else {
        format!("{}{}", input, suffix)
    };
    is_complete_and_typed(&extended)
}

#[test]
fn fun_constrained_completion_candidates_yield_typed_behavior() {
    // Simulate one constrained-generation step from partial prefixes.
    assert!(
        append_and_check("tru", "e", false),
        "Expected 'tru' + 'e' -> 'true' to be typed"
    );
    assert!(
        append_and_check("(1", ")", false),
        "Expected '(1' + ')' to become typed"
    );
    assert!(
        append_and_check("let x: Int = 1;", "x", false),
        "Expected let-prefix plus body variable to become typed"
    );
}

#[test]
fn fun_constrained_completion_rejects_type_broken_prefixes() {
    assert!(
        !is_complete_and_typed("1(2)"),
        "apply-non-function must be rejected"
    );
    assert!(
        !is_complete_and_typed("1 + 2.0"),
        "int/float operator mismatch must be rejected"
    );
    assert!(
        !is_complete_and_typed("let x: Float = 1; x"),
        "declared/actual let type mismatch must be rejected"
    );
}

