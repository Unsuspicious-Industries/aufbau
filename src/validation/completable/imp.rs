//! IMP Language Completion Tests
//!
//! Tests typed completion for the IMP imperative language with:
//! - Variable declarations with type annotations
//! - Integer arithmetic operations
//! - While loops with conditions
//! - Boolean expressions
//! - Variable scoping

use super::*;

/// Load IMP grammar from examples/imp.auf
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
        // Already complete (wrapped as a top-level Block)
        T::ok("var decl", "{ let x:Int=5; }", 2),
        T::ok("var init zero", "{ let x:Int=0; }", 2),
        T::ok("var negative", "{ let x:Int=-5; }", 2),
        T::ok("var large", "{ let x:Int=999; }", 2),
        T::ok("bool decl", "{ let flag:Bool=true; }", 2),
        T::ok("union decl", "{ let u:Int|Bool=true; }", 2),
        // Nearly complete
        T::ok("var no semicolon", "{ let x:Int=5", 2),
        T::ok("var no equals", "{ let x:Int", 3),
        T::ok("var no value", "{ let x:Int=", 2),
        // Partial
        T::ok("var type only", "{ let x:Int", 3),
        T::ok("empty", "", 3),
        // Sequences
        T::ok("two decls", "{ let x:Int=5; let y:Int=3; }", 2),
        T::ok("sequence partial", "{ let x:Int=5; y", 5),
        // Arithmetic expressions
        T::ok("simple add", "{ let x:Int=1+2; }", 2),
        T::ok("add chain", "{ let x:Int=1+2+3; }", 2),
        T::ok("subtract", "{ let x:Int=10-5; }", 2),
        T::ok("multiply", "{ let x:Int=2*3; }", 2),
        T::ok("divide", "{ let x:Int=6/2; }", 2),
        // Parentheses in expressions
        T::ok("paren add", "{ let x:Int=(1+2); }", 2),
        T::ok("nested parens", "{ let x:Int=((1+2)); }", 2),
        T::ok("paren partial", "{ let x:Int=(1", 3),
        // Complex expressions
        T::ok("mixed ops", "{ let x:Int=1+2*3; }", 1),
        T::ok("all ops", "{ let x:Int=1+2-3*4/5; }", 1),
        // Variable references in expressions
        T::ok("use var", "{ let x:Int=5; let y:Int=x; }", 1),
        T::ok("use in expr", "{ let x:Int=5; let y:Int=x+1; }", 1),
        // Control flow
        T::ok(
            "if statement",
            "{ if (1==1) { let x:Int=1; } else { let x:Int=2; } }",
            2,
        ),
        T::ok("while statement", "{ while (1==1) { let x:Int=1; } }", 2),
    ]
}

fn fail_cases() -> Vec<TypedCompletionTestCase> {
    vec![
        // Syntax errors
        T::fail("assign before decl", "{x=5;"),
        T::fail("missing type for declaration", "{let x=5;"),
        T::fail("missing value", "{let x:Int;"),
        // Invalid types
        T::fail("wrong type", "{let x:String=5;"),
        T::fail("lowercase type", "{let x:int=5;"),
        // Unbound variables
        T::fail("unbound var", "{let y:Int=x;"),
        T::fail("use before decl", "{let y:Int=x+1; let x:Int=5;"),
        // Type errors
        T::fail("union used as int", "{let u:Int|Bool=true; u+1;"),
        // Syntax errors in expressions
        T::fail("invalid operator", "{let x:Int=5%2;"),
        T::fail("operator first", "{let x:Int=+5;"),
        T::fail("double operator", "{let x:Int=1++2;"),
        // Mismatched parens
        T::fail("extra close paren", "{let x:Int=(1+2));"),
        T::fail("missing close paren", "{let x:Int=(1+2;"),
        T::fail("missing open brace", "{let"),
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

/// Standalone test: ensure identifier in block has '=' as a completion path
#[test]
fn bare_identifier_has_assignment_completion_path() {
    let grammar = imp_grammar();
    let completions = get_completions(&grammar, "{ x");

    let mut ctx = Context::new();
    ctx.add("x".to_string(), Type::Raw("Int".to_string()));

    let has_eq = completions.iter().any(|token| {
        token.matches("=")
            && token
                .example()
                .map(|example| extend_input_checked(&grammar, "{ x", &example, &ctx).is_ok())
                .unwrap_or(false)
    });
    assert!(
        has_eq,
        "expected '=' completion for bare identifier to allow assignment"
    );
}

#[test]
fn identifier_in_block_has_assignment_completion_only() {
    let grammar = imp_grammar();
    let completions = get_completions(&grammar, "{a");

    let mut ctx = Context::new();
    ctx.add("a".to_string(), Type::Raw("Int".to_string()));

    let has_eq = completions.iter().any(|token| {
        token.matches("=")
            && token
                .example()
                .map(|example| extend_input_checked(&grammar, "{a", &example, &ctx).is_ok())
                .unwrap_or(false)
    });
    let has_let = completions.iter().any(|token| token.matches("let"));
    let has_if = completions.iter().any(|token| token.matches("if"));
    let has_while = completions.iter().any(|token| token.matches("while"));

    assert!(has_eq, "expected '=' completion after identifier in block");
    assert!(!has_let, "did not expect 'let' after identifier in block");
    assert!(!has_if, "did not expect 'if' after identifier in block");
    assert!(
        !has_while,
        "did not expect 'while' after identifier in block"
    );
}

#[test]
fn check_fail() {
    let grammar = imp_grammar();
    let res = run_test_batch(&grammar, &fail_cases());
    res.assert_all_passed();
}
