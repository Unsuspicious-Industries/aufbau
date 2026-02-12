//! Simply Typed Lambda Calculus (STLC) Completion Tests
//!
//! Tests typed completion for lambda calculus with:
//! - Variable scoping via (var) rule
//! - Lambda abstractions with type annotations
//! - Function application with type checking

use super::*;

/// Load STLC grammar from examples/stlc.spec
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
        T::sound("identity", "λx:A.x", 1),
        T::sound("nested lambdas", "λx:A.λy:B.x", 1),
        T::sound("triple nested", "λx:A.λy:B.λz:C.x", 1),
        T::sound("use inner var", "λx:A.λy:B.y", 1),
        // Partial lambda expressions
        T::sound("lambda prefix", "λ", 6),
        T::sound("lambda with var", "λx", 5),
        T::sound("lambda with colon", "λx:", 4),
        T::sound("lambda with type", "λx:A", 3),
        T::sound("lambda with dot", "λx:A.", 2),
        // Function types
        T::sound("function type annotation", "λf:A->B.f", 1),
        T::sound("nested function type", "λf:(A->B)->C.f", 1),
        T::sound("curried type", "λf:A->B->C.f", 1),
        // Application expressions
        T::sound("simple app", "λf:A->B.f x", 2),
        T::sound("app in body", "λf:A->B.λx:A.f x", 2),
        T::sound("partial app", "λf:A->B.f", 2),
        // Parenthesized expressions
        T::sound("paren var", "λx:A.(x)", 1),
        T::sound("paren lambda", "(λx:A.x)", 1),
        T::sound("nested parens", "((λx:A.x))", 1),
        T::sound("open paren prefix", "(", 4),
        T::sound("paren then lambda", "(λ", 4),
        // Variables with context
        T::sound("bound var in ctx", "x", 1)
            .with_context(vec![("x", "A")]),
        T::sound("multiple vars in ctx", "x", 1)
            .with_context(vec![("x", "A"), ("y", "B")]),
        T::sound("app with ctx", "f x", 1)
            .with_context(vec![("f", "A->B"), ("x", "A")]),
        // Complex type annotations
        T::sound("paren type", "λx:(A).x", 1),
        T::sound("complex arrow", "λf:((A->B)->C).f", 1),
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
