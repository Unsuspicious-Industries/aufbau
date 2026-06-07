//! Sums parseability tests.
//!
//! Exercises the rewrite/normalization model: sum types as ordinary
//! constructors, and ascription unifying *modulo* the theory `Bool ⇝ Unit + Unit`
//! (`examples/sums.auf`). The `~` form forces two annotations to share a type, so
//! a normalization-only equality (`Bool` vs `Unit + Unit`) must succeed while a
//! genuine clash must be rejected.

use super::ParseTestCase;
#[cfg(test)]
use {
    super::{load_example_grammar, run_parse_batch},
    crate::engine::grammar::SPG,
};

#[cfg(test)]
fn sums_grammar() -> SPG {
    load_example_grammar("sums")
}

#[must_use]
pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        // === Partial annotations (prefix acceptance) ===
        ParseTestCase::valid("open paren", "("),
        ParseTestCase::valid("word", "(x"),
        ParseTestCase::valid("colon", "(x :"),
        ParseTestCase::valid("partial type", "(x : A"),
        // === Annotations ===
        ParseTestCase::valid("base type", "(x : A)"),
        ParseTestCase::valid("synonym base", "(x : Bool)"),
        ParseTestCase::valid("unit", "(y : Unit)"),
        ParseTestCase::valid("sum", "(x : Unit + Unit)"),
        ParseTestCase::valid("sum of vars", "(x : A + B)"),
        ParseTestCase::valid("parenthesized", "(x : (A))"),
        ParseTestCase::valid("left-nested sum", "(x : (A + B) + C)"),
        // === Same-type (unification) ===
        ParseTestCase::valid("same base", "(x : A) ~ (y : A)"),
        ParseTestCase::valid("same sum", "(x : A + B) ~ (y : A + B)"),
        // === Normalization-driven equality ===
        ParseTestCase::valid("synonym vs expansion", "(x : Bool) ~ (y : Unit + Unit)"),
        // === Partial same-type ===
        ParseTestCase::valid("partial tilde", "(x : A) ~"),
        ParseTestCase::valid("partial second annot", "(x : A) ~ ("),
    ]
}

#[must_use]
pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        // === Malformed annotations ===
        ParseTestCase::invalid("close paren first", ")"),
        ParseTestCase::invalid("empty paren", "()"),
        ParseTestCase::invalid("missing word", "(: A)"),
        ParseTestCase::invalid("missing colon", "(x A)"),
        ParseTestCase::invalid("missing type", "(x : )"),
        ParseTestCase::invalid("unparenthesized", "x : A"),
        ParseTestCase::invalid("extra close", "(x : A))"),
        ParseTestCase::invalid("operator type", "(x : +)"),
        // === Malformed same-type ===
        ParseTestCase::invalid("tilde alone", "~"),
        ParseTestCase::invalid("plus alone", "+ A"),
        ParseTestCase::invalid("double tilde", "(x : A) ~ ~ (y : A)"),
        // === Type clashes (rejected by typing) ===
        ParseTestCase::invalid("base clash", "(x : A) ~ (y : B)"),
        ParseTestCase::invalid("sum clash", "(x : Bool) ~ (y : Unit + Nat)"),
    ]
}

#[test]
fn valid_expressions_sums() {
    let mut grammar = sums_grammar();
    let cases = valid_expressions_cases();
    println!("\n=== Sums Valid Expressions ({} cases) ===", cases.len());
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!("✓ All {} cases passed in {:?}\n", res.passed, res.total_duration);
}

#[test]
fn invalid_expressions_sums() {
    let mut grammar = sums_grammar();
    let cases = invalid_expressions_cases();
    println!("\n=== Sums Invalid Expressions ({} cases) ===", cases.len());
    let (res, _) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!("✓ All {} cases passed in {:?}\n", res.passed, res.total_duration);
}
