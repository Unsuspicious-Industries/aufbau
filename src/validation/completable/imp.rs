//! IMP Language Completion Tests
//!
//! Tests typed completion for the IMP imperative language with:
//! - Variable declarations with type annotations
//! - Integer arithmetic operations
//! - While loops with conditions
//! - Boolean expressions
//! - Variable scoping

use super::*;

/// Load IMP grammar from examples/imp.spec
pub fn imp_grammar() -> Grammar {
    load_example_grammar("imp")
}

// ============================================================================
// Suite Definitions (used by validate binary)
// ============================================================================

pub fn suites() -> Vec<(&'static str, Grammar, Vec<TypedCompletionTestCase>)> {
    let g = imp_grammar();
    vec![
        ("imp::completable", g.clone(), completable_cases()),
        ("imp::fail", g, fail_cases()),
    ]
}

use TypedCompletionTestCase as T;

fn completable_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        // Already complete
        T::ok("var decl", "x:Int=5;", 2),
        T::ok("var init zero", "x:Int=0;", 2),
        T::ok("var negative", "x:Int=-5;", 2),
        T::ok("var large", "x:Int=999;", 2),
        T::ok("bool decl", "flag:Bool=true;", 2),
        T::ok("union decl", "u:Int|Bool=true;", 2),
        // Nearly complete
        T::ok("var no semicolon", "x:Int=5", 2),
        T::ok("var no equals", "x:Int", 3),
        T::ok("var no value", "x:Int=", 2),
        // Partial
        T::ok("var type only", "x:Int", 3),
        T::ok("empty", "", 3),
        // Sequences
        T::ok("two decls", "x:Int=5; y:Int=3;", 2),
        T::ok("sequence partial", "x:Int=5; y", 5),
        // Arithmetic expressions
        T::ok("simple add", "x:Int=1+2;", 2),
        T::ok("add chain", "x:Int=1+2+3;", 2),
        T::ok("subtract", "x:Int=10-5;", 2),
        T::ok("multiply", "x:Int=2*3;", 2),
        T::ok("divide", "x:Int=6/2;", 2),
        // Parentheses in expressions
        T::ok("paren add", "x:Int=(1+2);", 2),
        T::ok("nested parens", "x:Int=((1+2));", 2),
        T::ok("paren partial", "x:Int=(1", 3),
        // Complex expressions
        T::ok("mixed ops", "x:Int=1+2*3;", 1),
        T::ok("all ops", "x:Int=1+2-3*4/5;", 1),
        // Variable references in expressions
        T::ok("use var", "x:Int=5; y:Int=x;", 1),
        T::ok("use in expr", "x:Int=5; y:Int=x+1;", 1),
        // Control flow
        T::ok("if statement", "if 1==1 { x:Int=1; } else { x:Int=2; }", 2),
        T::ok("while statement", "while 1==1 { x:Int=1; }", 2),
    ]
}

fn fail_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        // Syntax errors
        T::fail("assign before decl", "x=5;"),
        T::fail("missing type", "x=5;"),
        T::fail("missing value", "x:Int;"),
        // Invalid types
        T::fail("wrong type", "x:String=5;"),
        T::fail("lowercase type", "x:int=5;"),
        // Unbound variables
        T::fail("unbound var", "y:Int=x;"),
        T::fail("use before decl", "y:Int=x+1; x:Int=5;"),
        // Type errors
        T::fail("union used as int", "u:Int|Bool=true; u+1;"),
        // Syntax errors in expressions
        T::fail("invalid operator", "x:Int=5%2;"),
        T::fail("operator first", "x:Int=+5;"),
        T::fail("double operator", "x:Int=1++2;"),
        // Mismatched parens
        T::fail("extra close paren", "x:Int=(1+2));"),
        T::fail("missing close paren", "x:Int=(1+2;"),
        // Invalid syntax
        T::fail("close brace first", "}"),
        T::fail("random chars", "@#$;"),
    ]
}

// ============================================================================
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let grammar = imp_grammar();
    let res = run_test_batch(&grammar, &completable_cases());
    res.assert_all_passed();
}

/// Standalone test: ensure bare identifier has ':' as a completion path
#[test]
fn bare_identifier_has_assignment_completion_path() {
    let grammar = imp_grammar();
    let completions = get_completions(&grammar, "x");
    let has_colon = completions.iter().any(|token| token.matches(":"));
    assert!(
        has_colon,
        "expected ':' completion for bare identifier to allow assignment"
    );
}

#[test]
fn check_fail() {
    let grammar = imp_grammar();
    let res = run_test_batch(&grammar, &fail_cases());
    res.assert_all_passed();
}
