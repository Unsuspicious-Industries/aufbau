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
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let cases = vec![
        // ========== Complete lambda expressions ==========
        TypedCompletionTestCase::new("identity", "λx:A.x", false).with_depth(1),
        TypedCompletionTestCase::new("nested lambdas", "λx:A.λy:B.x", false).with_depth(1),
        TypedCompletionTestCase::new("triple nested", "λx:A.λy:B.λz:C.x", false).with_depth(1),
        TypedCompletionTestCase::new("use inner var", "λx:A.λy:B.y", false).with_depth(1),
        
        // ========== Partial lambda expressions ==========
        TypedCompletionTestCase::new("lambda prefix", "λ", false).with_depth(4),
        TypedCompletionTestCase::new("lambda with var", "λx", false).with_depth(3),
        TypedCompletionTestCase::new("lambda with colon", "λx:", false).with_depth(3),
        TypedCompletionTestCase::new("lambda with type", "λx:A", false).with_depth(2),
        TypedCompletionTestCase::new("lambda with dot", "λx:A.", false).with_depth(2),
        
        // ========== Function types ==========
        TypedCompletionTestCase::new("function type annotation", "λf:A->B.f", false).with_depth(1),
        TypedCompletionTestCase::new("nested function type", "λf:(A->B)->C.f", false).with_depth(1),
        TypedCompletionTestCase::new("curried type", "λf:A->B->C.f", false).with_depth(1),
        
        // ========== Application expressions ==========
        TypedCompletionTestCase::new("simple app", "λf:A->B.f x", false).with_depth(2),
        TypedCompletionTestCase::new("app in body", "λf:A->B.λx:A.f x", false).with_depth(2),
        TypedCompletionTestCase::new("partial app", "λf:A->B.f", false).with_depth(2),
        
        // ========== Parenthesized expressions ==========
        TypedCompletionTestCase::new("paren var", "λx:A.(x)", false).with_depth(1),
        TypedCompletionTestCase::new("paren lambda", "(λx:A.x)", false).with_depth(1),
        TypedCompletionTestCase::new("nested parens", "((λx:A.x))", false).with_depth(1),
        TypedCompletionTestCase::new("open paren prefix", "(", false).with_depth(4),
        TypedCompletionTestCase::new("paren then lambda", "(λ", false).with_depth(4),
        
        // ========== Variables with context ==========
        TypedCompletionTestCase::new("bound var in ctx", "x", false)
            .with_context(vec![("x", "A")])
            .with_depth(1),
        TypedCompletionTestCase::new("multiple vars in ctx", "x", false)
            .with_context(vec![("x", "A"), ("y", "B")])
            .with_depth(1),
        TypedCompletionTestCase::new("app with ctx", "f x", false)
            .with_context(vec![("f", "A->B"), ("x", "A")])
            .with_depth(1),
        
        // ========== Complex type annotations ==========
        TypedCompletionTestCase::new("paren type", "λx:(A).x", false).with_depth(1),
        TypedCompletionTestCase::new("complex arrow", "λf:((A->B)->C).f", false).with_depth(1),
    ];

    let grammar = stlc_grammar();
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_fail() {
    let cases = vec![
        // ========== Syntax errors - malformed lambda ==========
        TypedCompletionTestCase::new("double colon", "λx::A", true),
        TypedCompletionTestCase::new("missing var", "λ:A", true),
        TypedCompletionTestCase::new("missing type", "λx:.x", true),
        TypedCompletionTestCase::new("double dot", "λx:A..x", true),
        TypedCompletionTestCase::new("double lambda", "λλx:A.x", true),
        TypedCompletionTestCase::new("colon before lambda", ":λx:A.x", true),
        TypedCompletionTestCase::new("dot before lambda", ".λx:A.x", true),
        
        // ========== Syntax errors - malformed arrows ==========
        TypedCompletionTestCase::new("arrow first", "->A", true),
        TypedCompletionTestCase::new("double arrow", "λx:A-->B.x", true),
        TypedCompletionTestCase::new("trailing arrow", "λx:A->.x", true),
        TypedCompletionTestCase::new("arrow only type", "λx:->.x", true),
        
        // ========== Syntax errors - parentheses ==========
        TypedCompletionTestCase::new("close paren first", ")", true),
        TypedCompletionTestCase::new("extra close paren", "(λx:A.x))", true),
        TypedCompletionTestCase::new("mismatched parens", "(λx:A.x))", true),
        TypedCompletionTestCase::new("close in type", "λx:).x", true),
        
        // ========== Type errors - unbound variables ==========
        TypedCompletionTestCase::new("unbound x", "x", true),
        TypedCompletionTestCase::new("unbound in app", "f x", true),
        TypedCompletionTestCase::new("unbound func", "f", true),
        TypedCompletionTestCase::new("var outside scope", "λx:A.y", true),
        TypedCompletionTestCase::new("shadowed var used outside", "λx:A.(λy:B.x) y", true),
        
        // ========== Invalid characters ==========
        TypedCompletionTestCase::new("at sign", "@", true),
        TypedCompletionTestCase::new("hash", "#x", true),
        TypedCompletionTestCase::new("dollar", "$x", true),
        TypedCompletionTestCase::new("backslash", "\\x", true),
        TypedCompletionTestCase::new("semicolon", "λx:A;x", true),
        
        // ========== Empty/whitespace issues ==========
        TypedCompletionTestCase::new("just colon", ":", true),
        TypedCompletionTestCase::new("just dot", ".", true),
        TypedCompletionTestCase::new("just arrow", "->", true),
    ];

    let grammar = stlc_grammar();
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}
