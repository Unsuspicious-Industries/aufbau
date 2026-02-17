//! Simply Typed Lambda Calculus (STLC) Completion Tests
//!
//! Tests typed completion for lambda calculus with:
//! - Variable scoping via (var) rule
//! - Lambda abstractions with type annotations
//! - Function application with type checking

use super::*;

/// Load STLC grammar from examples/stlc.auf
pub fn stlc_grammar() -> Grammar {
    load_example_grammar("stlc")
}

// ============================================================================
// Suite Definitions (used by validate binary)
// ============================================================================

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let g = stlc_grammar();
    vec![
        ("stlc::completable", g.clone(), completable_cases()),
        ("stlc::fail", g, fail_cases()),
    ]
}

use TypedCompletionTestCase as T;

fn completable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        // Complete lambda expressions
        T::ok("identity", "λx:A.x", 1),
        T::ok("nested lambdas", "λx:A.λy:B.x", 1),
        T::ok("triple nested", "λx:A.λy:B.λz:C.x", 1),
        T::ok("use inner var", "λx:A.λy:B.y", 1),
        // Partial lambda expressions
        T::ok("lambda prefix", "λ", 7),
        T::ok("lambda with var", "λx", 6),
        T::ok("lambda with colon", "λx:", 5),
        T::ok("lambda with type", "λx:A", 4),
        T::ok("lambda with dot", "λx:A.", 3),
        // Function types
        T::ok("function type annotation", "λf:A->B.f", 1),
        T::ok("nested function type", "λf:(A->B)->C.f", 1),
        T::ok("curried type", "λf:A->B->C.f", 1),
        // Application expressions
        T::ok("simple app", "λf:A->B.f x", 2).with_context(vec![("x", "'A'")]),
        T::ok("app in body", "λf:A->B.λx:A.f x", 2),
        T::ok("partial app", "λf:A->B.f", 2),
        // Parenthesized expressions
        T::ok("paren var", "λx:A.(x)", 1),
        T::ok("paren lambda", "(λx:A.x)", 1),
        T::ok("nested parens", "((λx:A.x))", 1),
        T::ok("open paren prefix", "(", 8),
        T::ok("paren then lambda", "(λ", 7),
        // Variables with context
        T::ok("bound var in ctx", "x", 1).with_context(vec![("x", "'A'")]),
        T::ok("multiple vars in ctx", "x", 1).with_context(vec![("x", "'A'"), ("y", "'B'")]),
        T::ok("app with ctx", "f x", 1).with_context(vec![("f", "'A'->'B'"), ("x", "'A'")]),
        // Complex type annotations
        T::ok("paren type", "λx:(A).x", 1),
        T::ok("complex arrow", "λf:((A->B)->C).f", 1),
    ]
}

fn fail_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        // Syntax errors - malformed lambda
        T::fail("double colon", "λx::A"),
        T::fail("missing var", "λ:A"),
        T::fail("missing type", "λx:.x"),
        T::fail("double dot", "λx:A..x"),
        T::fail("double lambda", "λλx:A.x"),
        T::fail("colon before lambda", ":λx:A.x"),
        T::fail("dot before lambda", ".λx:A.x"),
        // Syntax errors - malformed arrows
        T::fail("arrow first", "->A"),
        T::fail("double arrow", "λx:A-->B.x"),
        T::fail("trailing arrow", "λx:A->.x"),
        T::fail("arrow only type", "λx:->.x"),
        // Syntax errors - parentheses
        T::fail("close paren first", ")"),
        T::fail("extra close paren", "(λx:A.x))"),
        T::fail("mismatched parens", "(λx:A.x))"),
        T::fail("close in type", "λx:).x"),
        // Type errors - unbound variables
        T::fail("unbound x", "x"),
        T::fail("unbound in app", "f x"),
        T::fail("unbound func", "f"),
        T::fail("var outside scope", "λx:A.y"),
        T::fail("shadowed var used outside", "λx:A.(λy:B.x) y"),
        // Invalid characters
        T::fail("at sign", "@"),
        T::fail("hash", "#x"),
        T::fail("dollar", "$x"),
        T::fail("backslash", "\\x"),
        T::fail("semicolon", "λx:A;x"),
        // Empty/whitespace issues
        T::fail("just colon", ":"),
        T::fail("just dot", "."),
        T::fail("just arrow", "->"),
    ]
}

// ============================================================================
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let grammar = stlc_grammar();
    let res = run_test_batch(&grammar, &completable_cases());
    res.assert_all_passed();
}

#[test]
fn check_fail() {
    let grammar = stlc_grammar();
    let res = run_test_batch(&grammar, &fail_cases());
    res.assert_all_passed();
}
