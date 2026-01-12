//! STLC Parseability Tests
//!
//! Fast tests for Simply Typed Lambda Calculus that verify
//! all prefixes are parseable without doing full completion search.
//! Focuses on left-recursive application parsing.
//!
//! ## Performance Characteristics
//!
//! These tests demonstrate the parser's ability to handle left-recursive grammars
//! efficiently through memoization. The STLC grammar has the left-recursive rule:
//! Application -> Term BaseTerm
//! Term -> Application | BaseTerm
//!
//! Without memoization, this would cause exponential time complexity.
//! With memoization, we achieve polynomial time complexity.
//!
//! ## Expected Time Complexity
//!
//! For an input of length n:
//! - Without memoization: O(2^n) - exponential due to left-recursion
//! - With memoization: O(n^2 * |G|) - polynomial, where |G| is grammar size
//!
//! The n^2 factor comes from checking all prefixes (n positions) and
//! each prefix taking O(n * |G|) time with memoization.

use super::*;

/// Note: These tests focus on parsing only, not type checking.
/// For type checking tests, see the typing module tests.
///
/// The valid() method is used instead of valid() to avoid type checking
/// overhead and focus on the core parsing performance.

fn stlc_grammar() -> Grammar {
    load_example_grammar("stlc")
}

#[test]
fn valid_expressions_stlc() {
    let grammar = stlc_grammar();
    let cases = vec![
        // === Simple partial cases ===
        ParseTestCase::valid("empty lambda", "λ"),
        ParseTestCase::valid("lambda var", "λx"),
        ParseTestCase::valid("lambda colon", "λx:"),
        ParseTestCase::valid("partial no body after dot", "λx:A."),
        // === Identity functions ===
        ParseTestCase::valid("identity A", "λx:A.x"),
        ParseTestCase::valid("identity B", "λx:B.x"),
        ParseTestCase::valid("identity C", "λx:C.x"),
        ParseTestCase::valid("identity with parens", "(λx:A.x)"),
        // === Nested lambdas (2 deep) ===
        ParseTestCase::valid("nested return outer", "λx:A.λy:B.x"),
        ParseTestCase::valid("nested return inner", "λx:A.λy:B.y"),
        // === Nested lambdas (3 deep) ===
        ParseTestCase::valid("triple return first", "λx:A.λy:B.λz:C.x"),
        ParseTestCase::valid("triple return third", "λx:A.λy:B.λz:C.z"),
        // === Simple arrow types ===
        ParseTestCase::valid("arrow A->B", "λf:A->B.f"),
        ParseTestCase::valid("arrow B->C", "λf:B->C.f"),
        // === Nested arrow types (right associative) ===
        ParseTestCase::valid("arrow A->B->C", "λf:A->B->C.f"),
        ParseTestCase::valid("arrow A->B->C->D", "λf:A->B->C->D.f"),
        // === Applications in lambda bodies ===
        ParseTestCase::valid("lambda with app", "λf:A->B.λx:A.f x"),
        ParseTestCase::valid("lambda with double app", "λf:A->B->C.λx:A.λy:B.f x y"),
        // === Complex nested cases ===
        ParseTestCase::valid("nested lambda with app", "λx:A.λy:B.f x y"),
        // === Parenthesized applications ===
        // non typed 
        ParseTestCase::structural("paren app", "(f x)"),
        ParseTestCase::structural("paren double app", "((f x) y)"),
        ParseTestCase::structural("paren nested app", "x t y u r"),
    ];

    println!("\n=== STLC Valid Expressions ({} cases) ===", cases.len());
    let res = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "✓ All {} cases passed in {:?} (avg {:?})\n",
        res.passed, res.total_duration, res.avg_duration
    );
    println!(
        "  Performance: {:?} per test, {} tests\n",
        res.avg_duration, res.passed
    );
}

#[test]
fn invalid_expressions_stlc() {
    let grammar = stlc_grammar();
    let cases = vec![
        // === Invalid variable names ===
        ParseTestCase::invalid("number only", "123"),
        ParseTestCase::invalid("symbol only", "!@#"),
        // === Invalid lambda syntax ===
        ParseTestCase::invalid("lambda no var", "λ:"),
        // === Invalid type syntax ===
        ParseTestCase::invalid("arrow no rhs", "λf:A->.f"),
        ParseTestCase::invalid("arrow no lhs", "λf:->B.f"),
        // === Invalid parentheses ===
        ParseTestCase::invalid("empty paren", "()"),
        ParseTestCase::invalid("paren no content", "( )"), 

        ParseTestCase::type_error("unbound variable", "x"),
        ParseTestCase::type_error("unbound in app", "f x"),
        

    ];

    println!("\n=== STLC Invalid Expressions ({} cases) ===", cases.len());
    let res = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "✓ All {} cases passed in {:?} (avg {:?})\n",
        res.passed, res.total_duration, res.avg_duration
    );
}

#[test]
fn left_recursive_application_tests() {
    let grammar = stlc_grammar();
    let cases = vec![
        // === Test left-associative application parsing ===
        ParseTestCase::valid("simple left app", "f x"),
        ParseTestCase::valid("chained left app", "f x y"),
        ParseTestCase::valid("long chain left app", "f x y z w v u"),
        // === Test in lambda contexts ===
        ParseTestCase::valid("lambda with chain", "λf:A->B->C.λx:A.λy:B.f x y"),
        // === Test with parentheses ===
    ];

    println!(
        "\n=== STLC Left-Recursive Applications ({} cases) ===",
        cases.len()
    );
    let res = run_parse_batch(&grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "✓ All {} cases passed in {:?} (avg {:?})\n",
        res.passed, res.total_duration, res.avg_duration
    );
    println!(
        "  Performance: {:?} per test, {} tests\n",
        res.avg_duration, res.passed
    );
    println!("  This demonstrates efficient left-recursion handling via memoization.\n");
}
