//! STLC Parseability Tests
//!
//! Fast tests for Simply Typed Lambda Calculus that verify
//! all prefixes are parseable without doing full completion search.
//! Focuses on left-recursive application parsing.

use super::*;

#[cfg(test)]
fn stlc_grammar() -> Grammar {
    load_example_grammar("stlc")
}

pub fn valid_expressions_cases() -> Vec<ParseTestCase> {
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
        // Left-recursive application chains need extra depth: each application
        // step adds ~2 recursion levels, so depth ≈ 10 + 2*num_args.
        ParseTestCase::valid("lambda with app", "λf:A->B.λx:A.f x"),
        ParseTestCase::valid("lambda with double app", "λf:A->B->C.λx:A.λy:B.f x y"),
        // === Complex nested cases ===
        ParseTestCase::valid("nested lambda with app", "λx:A.λy:B.f x y")
            .with_context(vec![("f", "A->B->C")]),
        // === Parenthesized applications ===
        // These require environments because the fusion parser is type-directed.
        ParseTestCase::valid("paren app", "(f x)").with_context(vec![("f", "A->B"), ("x", "A")]),
        ParseTestCase::valid("paren double app", "((f x) y)").with_context(vec![
            ("f", "A->B->C"),
            ("x", "A"),
            ("y", "B"),
        ]),
        ParseTestCase::valid("paren nested app", "x t y u r").with_context(vec![
            ("x", "A->B->C->D->E"),
            ("t", "A"),
            ("y", "B"),
            ("u", "C"),
            ("r", "D"),
        ]),
    ];

    cases
}

pub fn invalid_expressions_cases() -> Vec<ParseTestCase> {
    vec![
        // === Invalid variable names ===
        ParseTestCase::invalid("number only", "123"),
        ParseTestCase::invalid("symbol only", "!@#"),
        // === Invalid lambda syntax ===
        ParseTestCase::invalid("lambda no var", "λ:"),
        ParseTestCase::invalid("double colon", "λx::A"),
        ParseTestCase::invalid("missing var", "λ:A"),
        ParseTestCase::invalid("missing type", "λx:.x"),
        ParseTestCase::invalid("double dot", "λx:A..x"),
        ParseTestCase::invalid("double lambda", "λλx:A.x"),
        ParseTestCase::invalid("colon before lambda", ":λx:A.x"),
        ParseTestCase::invalid("dot before lambda", ".λx:A.x"),
        // === Invalid type syntax ===
        ParseTestCase::invalid("arrow no rhs", "λf:A->.f"),
        ParseTestCase::invalid("arrow no lhs", "λf:->B.f"),
        ParseTestCase::invalid("arrow first", "->A"),
        ParseTestCase::invalid("double arrow", "λx:A-->B.x"),
        ParseTestCase::invalid("just arrow", "->"),
        // === Invalid parentheses ===
        ParseTestCase::invalid("empty paren", "()"),
        ParseTestCase::invalid("paren no content", "( )"),
        ParseTestCase::invalid("close paren first", ")"),
        ParseTestCase::invalid("extra close paren", "(λx:A.x))"),
        ParseTestCase::invalid("close in type", "λx:).x"),
        ParseTestCase::invalid("just colon", ":"),
        ParseTestCase::invalid("just dot", "."),
        ParseTestCase::invalid("semicolon", "λx:A;x"),
        ParseTestCase::invalid("hash", "#x"),
        ParseTestCase::invalid("dollar", "$x"),
        ParseTestCase::invalid("backslash", "\\x"),
        ParseTestCase::type_error("unbound variable", "x"),
        ParseTestCase::type_error("unbound in app", "f x"),
        ParseTestCase::type_error("unbound func", "f"),
        ParseTestCase::type_error("var outside scope", "λx:A.y"),
        ParseTestCase::type_error("shadowed var used outside", "λx:A.(λy:B.x) y"),
    ]
}

pub fn left_recursive_application_cases() -> Vec<ParseTestCase> {
    let cases = vec![
        // === Test left-associative application parsing ===
        // Each application step adds ~2 recursion levels in the left-recursive grammar,
        // so we set depth = 10 + 2 * num_args as a safe budget.
        ParseTestCase::valid("simple left app", "f x")
            .with_context(vec![("f", "A->B"), ("x", "A")]),
        ParseTestCase::valid("chained left app", "f x y").with_context(vec![
            ("f", "A->B->C"),
            ("x", "A"),
            ("y", "B"),
        ]),
        ParseTestCase::valid("long chain left app", "f x y z w v u").with_context(vec![
            ("f", "A->B->C->D->E->F->G"),
            ("x", "A"),
            ("y", "B"),
            ("z", "C"),
            ("w", "D"),
            ("v", "E"),
            ("u", "F"),
        ]),
        // === Test in lambda contexts ===
        ParseTestCase::valid("lambda with chain", "λf:A->B->C.λx:A.λy:B.f x y"),
    ];

    cases
}

#[test]
fn valid_expressions_stlc() {
    let mut grammar = stlc_grammar();
    let cases = valid_expressions_cases();
    println!("\n=== STLC Valid Expressions ({} cases) ===", cases.len());
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
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
    let mut grammar = stlc_grammar();
    let cases = invalid_expressions_cases();
    println!("\n=== STLC Invalid Expressions ({} cases) ===", cases.len());
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
    assert_eq!(res.failed, 0, "{}", res.format_failures());
    println!(
        "✓ All {} cases passed in {:?} (avg {:?})\n",
        res.passed, res.total_duration, res.avg_duration
    );
}

#[test]
fn left_recursive_application_tests() {
    let mut grammar = stlc_grammar();
    let cases = left_recursive_application_cases();
    println!(
        "\n=== STLC Left-Recursive Applications ({} cases) ===",
        cases.len()
    );
    let (res, _cases_json) = run_parse_batch(&mut grammar, &cases);
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
