//! Pathological and Edge Case Grammar Tests
//!
//! Tests completion behavior on unusual grammars:
//! - Infinite/unbounded recursion
//! - Ambiguous grammars
//! - Empty productions (epsilon)
//! - Deeply nested structures
//! - Grammars with complex typing rules

#![allow(dead_code)]
#![allow(unused_imports)]

use super::*;

// ============================================================================
// Pathological Grammars
// ============================================================================

/// Infinite right-recursion (no termination without 'b')
const INFINITE_RIGHT_RECURSIVE: &str = r#"
    A ::= 'a' A | 'b'
    start ::= A
"#;

/// Highly ambiguous grammar
const HIGHLY_AMBIGUOUS: &str = r#"
    A ::= 'x' | 'x' B
    B ::= 'y' | 'y' A
    start ::= A | B
"#;

/// Grammar with multiple epsilon productions
const EPSILON_HEAVY: &str = r#"
    A ::= 'a' B | ε
    B ::= 'b' C | ε
    C ::= 'c' | ε
    start ::= A B C
"#;

/// Deeply nested structure
const DEEP_NESTING: &str = r#"
    Atom ::= 'x'
    L1 ::= '(' L2 ')' | Atom
    L2 ::= '(' L3 ')' | L1
    L3 ::= '(' L4 ')' | L2
    L4 ::= '(' L5 ')' | L3
    L5 ::= '(' Atom ')' | L4
    start ::= L5
"#;

/// Grammar with very long productions
const LONG_PRODUCTION: &str = r#"
    A ::= 'a'
    B ::= 'b'
    C ::= 'c'
    Long ::= A B C A B C A B C
    start ::= Long
"#;

/// Grammar with cyclic but terminating alternatives
const CYCLIC_TERMINABLE: &str = r#"
    X ::= 'x' Y | 'done'
    Y ::= 'y' X | 'stop'
    start ::= X
"#;

/// Simple typing rule grammar
const TYPED_SIMPLE: &str = r#"
    Identifier ::= /[a-z]+/
    Variable(var) ::= Identifier[x]
    Expression ::= Variable
    
    x ∈ Γ
    ----------- (var)
    Γ(x)
"#;

/// Grammar with context extension
const CONTEXT_EXTENDING: &str = r#"
    Identifier ::= /[a-z]+/
    Type ::= 'int' | 'bool'
    Variable(var) ::= Identifier[x]
    Let(let) ::= 'let' Identifier[x] ':' Type[τ] 'in' Expression[e]
    Expression ::= Variable | Let
    
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ[x:τ] ⊢ e : ?T
    ------------------------ (let)
    ?T
"#;

// ============================================================================
// Batch Test Cases - Right Recursive Grammar
// ============================================================================

#[test]
fn check_right_recursive_completable() {
    let cases = vec![
        TypedCompletionTestCase::new("base case", "b", false).with_depth(1),
        TypedCompletionTestCase::new("one step", "a b", false).with_depth(2),
        TypedCompletionTestCase::new("chain", "a a b", false).with_depth(3),
        TypedCompletionTestCase::new("long chain", "a a a a b", false).with_depth(6),
        TypedCompletionTestCase::new("partial", "a", false).with_depth(3),
        TypedCompletionTestCase::new("partial chain", "a a", false).with_depth(4),
    ];

    let grammar = load_inline_grammar(INFINITE_RIGHT_RECURSIVE);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_right_recursive_fail() {
    let cases = vec![
        TypedCompletionTestCase::new("invalid char", "c", true),
        TypedCompletionTestCase::new("wrong order", "b a", true),
    ];

    let grammar = load_inline_grammar(INFINITE_RIGHT_RECURSIVE);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

// ============================================================================
// Batch Test Cases - Epsilon Grammar
// ============================================================================

#[test]
fn check_epsilon_completable() {
    let cases = vec![
        TypedCompletionTestCase::new("empty", "", false).with_depth(1),
        TypedCompletionTestCase::new("a only", "a", false).with_depth(2),
        TypedCompletionTestCase::new("a b", "a b", false).with_depth(2),
        TypedCompletionTestCase::new("a b c", "a b c", false).with_depth(1),
        TypedCompletionTestCase::new("b only", "b", false).with_depth(2),
        TypedCompletionTestCase::new("c only", "c", false).with_depth(1),
    ];

    let grammar = load_inline_grammar(EPSILON_HEAVY);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_epsilon_fail() {
    let cases = vec![
        TypedCompletionTestCase::new("invalid char", "x", true),
        TypedCompletionTestCase::new("wrong order", "c b a", true),
    ];

    let grammar = load_inline_grammar(EPSILON_HEAVY);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

// ============================================================================
// Batch Test Cases - Deep Nesting Grammar
// ============================================================================

#[test]
fn check_deep_nesting_completable() {
    let cases = vec![
        TypedCompletionTestCase::new("simple", "x", false).with_depth(1),
        TypedCompletionTestCase::new("one level", "(x)", false).with_depth(3),
        TypedCompletionTestCase::new("two levels", "((x))", false).with_depth(5),
        TypedCompletionTestCase::new("three levels", "(((x)))", false).with_depth(7),
        TypedCompletionTestCase::new("max levels", "(((((x)))))", false).with_depth(12),
        TypedCompletionTestCase::new("partial", "((", false).with_depth(8),
    ];

    let grammar = load_inline_grammar(DEEP_NESTING);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_deep_nesting_fail() {
    let cases = vec![
        TypedCompletionTestCase::new("close first", ")", true),
        TypedCompletionTestCase::new("invalid char", "y", true),
        TypedCompletionTestCase::new("extra close", "x)", true),
    ];

    let grammar = load_inline_grammar(DEEP_NESTING);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

// ============================================================================
// Batch Test Cases - Cyclic Grammar
// ============================================================================

#[test]
fn check_cyclic_completable() {
    let cases = vec![
        TypedCompletionTestCase::new("done", "done", false).with_depth(1),
        TypedCompletionTestCase::new("x stop", "x stop", false).with_depth(3),
        TypedCompletionTestCase::new("x y done", "x y done", false).with_depth(4),
        TypedCompletionTestCase::new("x y x stop", "x y x stop", false).with_depth(5),
        TypedCompletionTestCase::new("partial x", "x", false).with_depth(4),
        TypedCompletionTestCase::new("partial x y", "x y", false).with_depth(5),
    ];

    let grammar = load_inline_grammar(CYCLIC_TERMINABLE);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_cyclic_fail() {
    let cases = vec![
        TypedCompletionTestCase::new("y first", "y", true),
        TypedCompletionTestCase::new("stop first", "stop", true),
        TypedCompletionTestCase::new("invalid", "z", true),
    ];

    let grammar = load_inline_grammar(CYCLIC_TERMINABLE);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

// ============================================================================
// Batch Test Cases - Long Production Grammar
// ============================================================================

#[test]
fn check_long_production_completable() {
    let cases = vec![
        TypedCompletionTestCase::new("complete", "a b c a b c a b c", false).with_depth(1),
        TypedCompletionTestCase::new("partial 1", "a", false).with_depth(10),
        TypedCompletionTestCase::new("partial 3", "a b c", false).with_depth(8),
        TypedCompletionTestCase::new("partial 6", "a b c a b c", false).with_depth(5),
    ];

    let grammar = load_inline_grammar(LONG_PRODUCTION);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_long_production_fail() {
    let cases = vec![
        TypedCompletionTestCase::new("wrong start", "b", true),
        TypedCompletionTestCase::new("invalid", "x", true),
    ];

    let grammar = load_inline_grammar(LONG_PRODUCTION);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

// ============================================================================
// Batch Test Cases - Ambiguous Grammar
// ============================================================================

#[test]
fn check_ambiguous_completable() {
    let cases = vec![
        TypedCompletionTestCase::new("x only", "x", false).with_depth(1),
        TypedCompletionTestCase::new("x y", "x y", false).with_depth(3),
        TypedCompletionTestCase::new("x y x", "x y x", false).with_depth(4),
    ];

    let grammar = load_inline_grammar(HIGHLY_AMBIGUOUS);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_ambiguous_fail() {
    let cases = vec![TypedCompletionTestCase::new("invalid", "z", true)];

    let grammar = load_inline_grammar(HIGHLY_AMBIGUOUS);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

// ============================================================================
// Batch Test Cases - Typed Simple Grammar
// ============================================================================

#[test]
fn check_typed_simple_completable() {
    let cases = vec![
        // Variable in context should complete
        TypedCompletionTestCase::new("x in context", "x", false)
            .with_context(vec![("x", "int")])
            .with_depth(1),
        TypedCompletionTestCase::new("foo in context", "foo", false)
            .with_context(vec![("foo", "bool")])
            .with_depth(1),
    ];

    let grammar = load_inline_grammar(TYPED_SIMPLE);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_typed_simple_fail() {
    let cases = vec![
        // Unbound variables should fail
        TypedCompletionTestCase::new("unbound x", "x", true),
        TypedCompletionTestCase::new("unbound foo", "foo", true),
    ];

    let grammar = load_inline_grammar(TYPED_SIMPLE);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

// ============================================================================
// Batch Test Cases - Context Extending Grammar
// ============================================================================

#[test]
fn check_context_extending_completable() {
    let cases = vec![
        TypedCompletionTestCase::new("let x in x", "let x : int in x", false).with_depth(5),
        TypedCompletionTestCase::new("nested let", "let x : int in let y : bool in x", false)
            .with_depth(8),
        TypedCompletionTestCase::new(
            "nested let inner",
            "let x : int in let y : bool in y",
            false,
        )
        .with_depth(8),
    ];

    let grammar = load_inline_grammar(CONTEXT_EXTENDING);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_context_extending_fail() {
    let cases = vec![
        // Unbound before let
        TypedCompletionTestCase::new("unbound x", "x", true),
        // Wrong variable in scope
        TypedCompletionTestCase::new("wrong var", "let x : int in y", true),
    ];

    let grammar = load_inline_grammar(CONTEXT_EXTENDING);
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}
