//! Arithmetic Expression Tests
//!
//! Tests typed completion for simple arithmetic expressions:
//! - Numbers and identifiers
//! - Binary operators (+, -, *, /)
//! - Parenthesized expressions

#![allow(dead_code)]

use super::*;

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
    let cases = vec![
        // Numbers - complete
        TypedCompletionTestCase::new("empty", "", false).with_depth(3),
        TypedCompletionTestCase::new("single digit", "1", false).with_depth(1),
        TypedCompletionTestCase::new("multi digit", "42", false).with_depth(1),
        TypedCompletionTestCase::new("large number", "9999", false).with_depth(1),
        // Variables - complete
        TypedCompletionTestCase::new("simple var", "x", false).with_depth(1),
        TypedCompletionTestCase::new("longer var", "abc", false).with_depth(1),
        TypedCompletionTestCase::new("var with digits", "x1", false).with_depth(1),
        // Binary ops - partial
        TypedCompletionTestCase::new("add prefix", "1 +", false).with_depth(2),
        TypedCompletionTestCase::new("sub prefix", "x -", false).with_depth(2),
        TypedCompletionTestCase::new("mul prefix", "2 *", false).with_depth(2),
        TypedCompletionTestCase::new("div prefix", "y /", false).with_depth(2),
        // Complete expressions
        TypedCompletionTestCase::new("simple add", "1 + 2", false).with_depth(1),
        TypedCompletionTestCase::new("chain ops", "1 + 2 * 3", false).with_depth(1),
        // Parentheses
        TypedCompletionTestCase::new("open paren", "(", false).with_depth(3),
        TypedCompletionTestCase::new("paren number", "(42", false).with_depth(2),
        TypedCompletionTestCase::new("closed paren", "(42)", false).with_depth(1),
        TypedCompletionTestCase::new("nested parens", "((1))", false).with_depth(2),
        TypedCompletionTestCase::new("complex paren", "(x + y) * z", false).with_depth(2),
    ];

    let grammar = arithmetic_grammar();
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_fail() {
    let cases = vec![
        // Syntax errors
        TypedCompletionTestCase::new("close paren first", ")", true),
        TypedCompletionTestCase::new("operator first", "+ 1", true),
        TypedCompletionTestCase::new("double operator", "1 ++", true),
        TypedCompletionTestCase::new("invalid char", "@", true),
        TypedCompletionTestCase::new("percent op", "1 %", true),
        // Unmatched parens
        TypedCompletionTestCase::new("extra close", "1)", true),
        TypedCompletionTestCase::new("misplaced close", "(1))", true),
    ];

    let grammar = arithmetic_grammar();
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}
