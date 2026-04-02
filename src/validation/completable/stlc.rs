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
    vec![("stlc::completable", g, completable_cases())]
}

use TypedCompletionTestCase as T;

fn completable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        // Complete lambda expressions
        T::ok("identity", "λx:A.x", 10),
        T::ok("nested lambdas", "λx:A.λy:B.x", 10),
        T::ok("triple nested", "λx:A.λy:B.λz:C.x", 10),
        T::ok("use inner var", "λx:A.λy:B.y", 10),
        // Partial lambda expressions
        // A bit redundant because of soundness
        T::ok("lambda prefix", "λ", 10),
        T::ok("lambda with var", "λx", 10),
        T::ok("lambda with colon", "λx:", 10),
        T::ok("lambda with type", "λx:A", 10),
        T::ok("lambda with dot", "λx:A.", 10),
        // long veriables
        T::ok("lambda long var", "λfoo:A0->B0.foo", 10),
        // Function types
        T::ok("function type annotation", "λf:A->B.f", 10),
        T::ok("nested function type", "λf:(A->B)->C.f", 10),
        T::ok("curried type", "λf:A->B->C.f", 10),
        // Application expressions
        T::ok("simple app", "λf:A->B.f x", 10).with_context(vec![("x", "'A'")]),
        T::ok("app in body", "λf:A->B.λx:A.f x", 10),
        T::ok("partial app", "λf:A->B.f", 10),
        // Parenthesized expressions
        T::ok("paren var", "λx:A.(x)", 10),
        T::ok("paren lambda", "(λx:A.x)", 10),
        T::ok("nested parens", "((λx:A.x))", 10),
        T::ok("open paren prefix", "(", 8),
        T::ok("paren then lambda", "(λ", 7),
        // Variables with context
        T::ok("bound var in ctx", "x", 10).with_context(vec![("x", "'A'")]),
        T::ok("multiple vars in ctx", "x", 10).with_context(vec![("x", "'A'"), ("y", "'B'")]),
        T::ok("app with ctx", "f x", 10).with_context(vec![("f", "'A'->'B'"), ("x", "'A'")]),
        // Complex type annotations
        T::ok("paren type", "λx:(A).x", 10),
        T::ok("complex arrow", "λf:((A->B)->C).f", 10),
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
