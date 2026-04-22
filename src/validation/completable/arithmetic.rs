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
    vec![("arithmetic::completable", g, completable_cases())]
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
        T::ok("simple add", "1 + 2", 2),
        T::ok("chain ops", "1 + 2 * 3", 3),
        T::ok("open paren", "(", 3),
        T::ok("paren number", "(42", 2),
        T::ok("paren operator prefix", "(1 +", 3),
        T::ok("closed paren", "(42)", 2),
        T::ok("nested parens", "((1))", 3),
        T::ok("complex paren", "(x + y) * z", 4),
        T::ok("deep operator prefix", "x * (y +", 4),
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
fn repro_chain_operator_prefix_is_completable() {
    let grammar = arithmetic_grammar();
    let result = crate::validation::completability::sound_complete(&grammar, "1 + 2 *", None);

    assert!(
        result.is_sound,
        "expected chain operator prefix to replay through feed, got {result:?}"
    );
}

#[test]
fn repro_chain_operator_expression_stays_prefix_sound() {
    let mut grammar = arithmetic_grammar();
    let result = crate::validation::completability::sound_complete(&mut grammar, "1 + 2 * 3", None);

    assert!(
        result.is_sound,
        "expected chain operator expression to stay sound, failing_prefix={:?}",
        result.failing_prefix
    );
}

#[test]
fn repro_complex_paren_operator_prefix_is_completable() {
    let grammar = arithmetic_grammar();
    let result = crate::validation::completability::sound_complete(&grammar, "(x + y) *", None);

    assert!(
        result.is_sound,
        "expected complex paren operator prefix to replay through feed, got {result:?}"
    );
}
