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
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() {
    let ok = |description, input, depth| {
        TypedCompletionTestCase::new(description, input, false)
            .with_depth(depth)
            .without_soundness()
    };

    let cases = vec![
        // Already complete - should be fast (depth 0 or 1)
        ok("var decl", "x:Int=5;", 2),
        ok("var init zero", "x:Int=0;", 2),
        ok("var negative", "x:Int=-5;", 2),
        ok("var large", "x:Int=999;", 2),
        ok("bool decl", "flag:Bool=true;", 2),
        ok("union decl", "u:Int|Bool=true;", 2),
        // Nearly complete - need 1-2 tokens
        ok("var no semicolon", "x:Int=5", 2),
        ok("var no equals", "x:Int", 3),
        ok("var no value", "x:Int=", 2),
        // Partial - need more tokens
        ok("var type only", "x:Int", 3),
        ok("empty", "", 3),
        // Sequences
        ok("two decls", "x:Int=5; y:Int=3;", 2),
        ok("sequence partial", "x:Int=5; y", 5),
        // Arithmetic expressions
        ok("simple add", "x:Int=1+2;", 2),
        ok("add chain", "x:Int=1+2+3;", 2),
        ok("subtract", "x:Int=10-5;", 2),
        ok("multiply", "x:Int=2*3;", 2),
        ok("divide", "x:Int=6/2;", 2),
        // Parentheses in expressions
        ok("paren add", "x:Int=(1+2);", 2),
        ok("nested parens", "x:Int=((1+2));", 2),
        ok("paren partial", "x:Int=(1", 3),
        // Complex expressions
        ok("mixed ops", "x:Int=1+2*3;", 1),
        ok("all ops", "x:Int=1+2-3*4/5;", 1),
        // Variable references in expressions
        ok("use var", "x:Int=5; y:Int=x;", 1),
        ok("use in expr", "x:Int=5; y:Int=x+1;", 1),
        // Control flow
        ok("if statement", "if 1==1 { x:Int=1; } else { x:Int=2; }", 2),
        ok("while statement", "while 1==1 { x:Int=1; }", 2),
    ];

    let grammar = imp_grammar();
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

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
    let cases = vec![
        // Syntax errors
        TypedCompletionTestCase::new("assign before decl", "x=5;", true),
        TypedCompletionTestCase::new("missing type", "x=5;", true),
        TypedCompletionTestCase::new("missing value", "x:Int;", true),
        // Invalid types
        TypedCompletionTestCase::new("wrong type", "x:String=5;", true),
        TypedCompletionTestCase::new("lowercase type", "x:int=5;", true),
        // Unbound variables
        TypedCompletionTestCase::new("unbound var", "y:Int=x;", true),
        TypedCompletionTestCase::new("use before decl", "y:Int=x+1; x:Int=5;", true),
        // Type errors
        TypedCompletionTestCase::new("union used as int", "u:Int|Bool=true; u+1;", true),
        // Syntax errors in expressions
        TypedCompletionTestCase::new("invalid operator", "x:Int=5%2;", true),
        TypedCompletionTestCase::new("operator first", "x:Int=+5;", true),
        TypedCompletionTestCase::new("double operator", "x:Int=1++2;", true),
        // Mismatched parens
        TypedCompletionTestCase::new("extra close paren", "x:Int=(1+2));", true),
        TypedCompletionTestCase::new("missing close paren", "x:Int=(1+2;", true),
        // Invalid syntax
        TypedCompletionTestCase::new("close brace first", "}", true),
        TypedCompletionTestCase::new("random chars", "@#$;", true),
    ];

    let grammar = imp_grammar();
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}
