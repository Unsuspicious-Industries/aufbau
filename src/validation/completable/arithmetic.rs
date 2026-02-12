//! Arithmetic Expression Tests
//!
//! Tests typed completion for simple arithmetic expressions:
//! - Numbers and identifiers
//! - Binary operators (+, -, *, /)
//! - Parenthesized expressions

#![allow(dead_code)]

use super::*;

// ============================================================================
// Suite Definitions (used by validate binary)
// ============================================================================

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let g = arithmetic_grammar();
    vec![
        ("arithmetic::completable", g.clone(), completable_cases()),
        ("arithmetic::fail", g, fail_cases()),
    ]
}

use TypedCompletionTestCase as T;

fn completable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("empty", "", 3),
        T::ok("single digit", "1", 1),
        T::ok("multi digit", "42", 1),
        T::ok("large number", "9999", 1),
        T::ok("simple var", "x", 1),
        T::ok("longer var", "abc", 1),
        T::ok("var with digits", "x1", 1),
        T::ok("add prefix", "1 +", 2),
        T::ok("sub prefix", "x -", 2),
        T::ok("mul prefix", "2 *", 2),
        T::ok("div prefix", "y /", 2),
        T::ok("simple add", "1 + 2", 1),
        T::ok("chain ops", "1 + 2 * 3", 1),
        T::ok("open paren", "(", 3),
        T::ok("paren number", "(42", 2),
        T::ok("closed paren", "(42)", 1),
        T::ok("nested parens", "((1))", 2),
        T::ok("complex paren", "(x + y) * z", 2),
    ]
}

fn fail_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("close paren first", ")"),
        T::fail("operator first", "+ 1"),
        T::fail("double operator", "1 ++"),
        T::fail("invalid char", "@"),
        T::fail("percent op", "1 %"),
        T::fail("extra close", "1)"),
        T::fail("misplaced close", "(1))"),
    ]
}

// ============================================================================
// Grammar
// ============================================================================

/// Simple arithmetic grammar - no typing rules
const ARITHMETIC_GRAMMAR: &str = r#"
    Number ::= /[0-9]+/
    Identifier ::= /[a-z][a-zA-Z0-9]*/
    Literal ::= Number
    Variable ::= Identifier
    Operator ::= '+' | '-' | '*' | '/'
    Primary ::= Literal | Variable | '(' Expression ')'
    Expression ::= Primary | Primary Operator Expression
"#;

fn arithmetic_grammar() -> Grammar {
    load_inline_grammar(ARITHMETIC_GRAMMAR)
}

// ============================================================================
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let grammar = arithmetic_grammar();
    let res = run_test_batch(&grammar, &completable_cases());
    res.assert_all_passed();
}

#[test]
fn check_fail() {
    let grammar = arithmetic_grammar();
    let res = run_test_batch(&grammar, &fail_cases());
    res.assert_all_passed();
}
