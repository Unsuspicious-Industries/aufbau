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
use TypedCompletionTestCase as T;

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
// Suite Definitions (used by validate binary)
// ============================================================================

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    vec![
        ("weird::right_recursive_ok", load_inline_grammar(INFINITE_RIGHT_RECURSIVE), right_recursive_ok()),
        ("weird::right_recursive_fail", load_inline_grammar(INFINITE_RIGHT_RECURSIVE), right_recursive_fail()),
        ("weird::epsilon_ok", load_inline_grammar(EPSILON_HEAVY), epsilon_ok()),
        ("weird::epsilon_fail", load_inline_grammar(EPSILON_HEAVY), epsilon_fail()),
        ("weird::deep_nesting_ok", load_inline_grammar(DEEP_NESTING), deep_nesting_ok()),
        ("weird::deep_nesting_fail", load_inline_grammar(DEEP_NESTING), deep_nesting_fail()),
        ("weird::cyclic_ok", load_inline_grammar(CYCLIC_TERMINABLE), cyclic_ok()),
        ("weird::cyclic_fail", load_inline_grammar(CYCLIC_TERMINABLE), cyclic_fail()),
        ("weird::long_production_ok", load_inline_grammar(LONG_PRODUCTION), long_production_ok()),
        ("weird::long_production_fail", load_inline_grammar(LONG_PRODUCTION), long_production_fail()),
        ("weird::ambiguous_ok", load_inline_grammar(HIGHLY_AMBIGUOUS), ambiguous_ok()),
        ("weird::ambiguous_fail", load_inline_grammar(HIGHLY_AMBIGUOUS), ambiguous_fail()),
        ("weird::typed_simple_ok", load_inline_grammar(TYPED_SIMPLE), typed_simple_ok()),
        ("weird::typed_simple_fail", load_inline_grammar(TYPED_SIMPLE), typed_simple_fail()),
        ("weird::context_extending_ok", load_inline_grammar(CONTEXT_EXTENDING), context_extending_ok()),
        ("weird::context_extending_fail", load_inline_grammar(CONTEXT_EXTENDING), context_extending_fail()),
    ]
}

// ============================================================================
// Case Definitions
// ============================================================================

fn right_recursive_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("base case", "b", 1),
        T::ok("one step", "a b", 2),
        T::ok("chain", "a a b", 3),
        T::ok("long chain", "a a a a b", 6),
        T::ok("partial", "a", 3),
        T::ok("partial chain", "a a", 4),
    ]
}

fn right_recursive_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("invalid char", "c"),
        T::fail("wrong order", "b a"),
    ]
}

fn epsilon_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("empty", "", 1),
        T::ok("a only", "a", 2),
        T::ok("a b", "a b", 2),
        T::ok("a b c", "a b c", 1),
        T::ok("b only", "b", 2),
        T::ok("c only", "c", 1),
    ]
}

fn epsilon_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("invalid char", "x"),
        T::fail("wrong order", "c b a"),
    ]
}

fn deep_nesting_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("simple", "x", 1),
        T::ok("one level", "(x)", 3),
        T::ok("two levels", "((x))", 5),
        T::ok("three levels", "(((x)))", 7),
        T::ok("max levels", "(((((x)))))", 12),
        T::ok("partial", "((", 8),
    ]
}

fn deep_nesting_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("close first", ")"),
        T::fail("invalid char", "y"),
        T::fail("extra close", "x)"),
    ]
}

fn cyclic_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("done", "done", 1),
        T::ok("x stop", "x stop", 3),
        T::ok("x y done", "x y done", 4),
        T::ok("x y x stop", "x y x stop", 5),
        T::ok("partial x", "x", 4),
        T::ok("partial x y", "x y", 5),
    ]
}

fn cyclic_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("y first", "y"),
        T::fail("stop first", "stop"),
        T::fail("invalid", "z"),
    ]
}

fn long_production_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("complete", "a b c a b c a b c", 1),
        T::ok("partial 1", "a", 10),
        T::ok("partial 3", "a b c", 8),
        T::ok("partial 6", "a b c a b c", 5),
    ]
}

fn long_production_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("wrong start", "b"),
        T::fail("invalid", "x"),
    ]
}

fn ambiguous_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("x only", "x", 1),
        T::ok("x y", "x y", 3),
        T::ok("x y x", "x y x", 4),
    ]
}

fn ambiguous_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("invalid", "z"),
    ]
}

fn typed_simple_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("x in context", "x", 1).with_context(vec![("x", "int")]),
        T::ok("foo in context", "foo", 1).with_context(vec![("foo", "bool")]),
    ]
}

fn typed_simple_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("unbound x", "x"),
        T::fail("unbound foo", "foo"),
    ]
}

fn context_extending_ok() -> Vec<TypedCompletionTestCase> {
    vec![
        T::ok("let x in x", "let x : int in x", 5),
        T::ok("nested let", "let x : int in let y : bool in x", 8),
        T::ok("nested let inner", "let x : int in let y : bool in y", 8),
    ]
}

fn context_extending_fail() -> Vec<TypedCompletionTestCase> {
    vec![
        T::fail("unbound x", "x"),
        T::fail("wrong var", "let x : int in y"),
    ]
}

// ============================================================================
// Tests (delegates to shared case definitions)
// ============================================================================

#[test]
fn check_right_recursive_completable() {
    let grammar = load_inline_grammar(INFINITE_RIGHT_RECURSIVE);
    let res = run_test_batch(&grammar, &right_recursive_ok());
    res.assert_all_passed();
}

#[test]
fn check_right_recursive_fail() {
    let grammar = load_inline_grammar(INFINITE_RIGHT_RECURSIVE);
    let res = run_test_batch(&grammar, &right_recursive_fail());
    res.assert_all_passed();
}

#[test]
fn check_epsilon_completable() {
    let grammar = load_inline_grammar(EPSILON_HEAVY);
    let res = run_test_batch(&grammar, &epsilon_ok());
    res.assert_all_passed();
}

#[test]
fn check_epsilon_fail() {
    let grammar = load_inline_grammar(EPSILON_HEAVY);
    let res = run_test_batch(&grammar, &epsilon_fail());
    res.assert_all_passed();
}

#[test]
fn check_deep_nesting_completable() {
    let grammar = load_inline_grammar(DEEP_NESTING);
    let res = run_test_batch(&grammar, &deep_nesting_ok());
    res.assert_all_passed();
}

#[test]
fn check_deep_nesting_fail() {
    let grammar = load_inline_grammar(DEEP_NESTING);
    let res = run_test_batch(&grammar, &deep_nesting_fail());
    res.assert_all_passed();
}

#[test]
fn check_cyclic_completable() {
    let grammar = load_inline_grammar(CYCLIC_TERMINABLE);
    let res = run_test_batch(&grammar, &cyclic_ok());
    res.assert_all_passed();
}

#[test]
fn check_cyclic_fail() {
    let grammar = load_inline_grammar(CYCLIC_TERMINABLE);
    let res = run_test_batch(&grammar, &cyclic_fail());
    res.assert_all_passed();
}

#[test]
fn check_long_production_completable() {
    let grammar = load_inline_grammar(LONG_PRODUCTION);
    let res = run_test_batch(&grammar, &long_production_ok());
    res.assert_all_passed();
}

#[test]
fn check_long_production_fail() {
    let grammar = load_inline_grammar(LONG_PRODUCTION);
    let res = run_test_batch(&grammar, &long_production_fail());
    res.assert_all_passed();
}

#[test]
fn check_ambiguous_completable() {
    let grammar = load_inline_grammar(HIGHLY_AMBIGUOUS);
    let res = run_test_batch(&grammar, &ambiguous_ok());
    res.assert_all_passed();
}

#[test]
fn check_ambiguous_fail() {
    let grammar = load_inline_grammar(HIGHLY_AMBIGUOUS);
    let res = run_test_batch(&grammar, &ambiguous_fail());
    res.assert_all_passed();
}

#[test]
fn check_typed_simple_completable() {
    let grammar = load_inline_grammar(TYPED_SIMPLE);
    let res = run_test_batch(&grammar, &typed_simple_ok());
    res.assert_all_passed();
}

#[test]
fn check_typed_simple_fail() {
    let grammar = load_inline_grammar(TYPED_SIMPLE);
    let res = run_test_batch(&grammar, &typed_simple_fail());
    res.assert_all_passed();
}

#[test]
fn check_context_extending_completable() {
    let grammar = load_inline_grammar(CONTEXT_EXTENDING);
    let res = run_test_batch(&grammar, &context_extending_ok());
    res.assert_all_passed();
}

#[test]
fn check_context_extending_fail() {
    let grammar = load_inline_grammar(CONTEXT_EXTENDING);
    let res = run_test_batch(&grammar, &context_extending_fail());
    res.assert_all_passed();
}
