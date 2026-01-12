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
    let cases = vec![
        // Already complete - should be fast (depth 0 or 1)
        TypedCompletionTestCase::new("var decl", "x:Int=5;", false).with_depth(1),
        TypedCompletionTestCase::new("var init zero", "x:Int=0;", false).with_depth(1),
        TypedCompletionTestCase::new("var negative", "x:Int=-5;", false).with_depth(1),
        TypedCompletionTestCase::new("var large", "x:Int=999;", false).with_depth(1),
        // Nearly complete - need 1-2 tokens
        TypedCompletionTestCase::new("var no semicolon", "x:Int=5", false).with_depth(2),
        TypedCompletionTestCase::new("var no equals", "x:Int", false).with_depth(2),
        TypedCompletionTestCase::new("var no value", "x:Int=", false).with_depth(2),
        // Partial - need more tokens
        TypedCompletionTestCase::new("var type only", "x:Int", false).with_depth(3),
        TypedCompletionTestCase::new("var name only", "x", false).with_depth(4),
        TypedCompletionTestCase::new("empty", "", false).with_depth(3),
        // Sequences
        TypedCompletionTestCase::new("two decls", "x:Int=5; y:Int=3;", false).with_depth(1),
        TypedCompletionTestCase::new("sequence partial", "x:Int=5; y", false).with_depth(4),
        // Arithmetic expressions
        TypedCompletionTestCase::new("simple add", "x:Int=1+2;", false).with_depth(1),
        TypedCompletionTestCase::new("add chain", "x:Int=1+2+3;", false).with_depth(1),
        TypedCompletionTestCase::new("subtract", "x:Int=10-5;", false).with_depth(1),
        TypedCompletionTestCase::new("multiply", "x:Int=2*3;", false).with_depth(1),
        TypedCompletionTestCase::new("divide", "x:Int=6/2;", false).with_depth(1),
        // Parentheses in expressions
        TypedCompletionTestCase::new("paren add", "x:Int=(1+2);", false).with_depth(1),
        TypedCompletionTestCase::new("nested parens", "x:Int=((1+2));", false).with_depth(2),
        TypedCompletionTestCase::new("paren partial", "x:Int=(1", false).with_depth(3),
        // Complex expressions
        TypedCompletionTestCase::new("mixed ops", "x:Int=1+2*3;", false).with_depth(1),
        TypedCompletionTestCase::new("all ops", "x:Int=1+2-3*4/5;", false).with_depth(1),
        // While loops
        TypedCompletionTestCase::new("while true", "while(true) {}", false).with_depth(1),
        TypedCompletionTestCase::new("while partial", "while(true", false).with_depth(3),
        TypedCompletionTestCase::new("while no paren", "while", false).with_depth(4),
        TypedCompletionTestCase::new("while with decl", "x:Int=5; while(x<10) {}", false)
            .with_depth(1),
        // While with body
        TypedCompletionTestCase::new("while body", "while(true) {x:Int=1;}", false).with_depth(1),
        TypedCompletionTestCase::new("while body partial", "while(true) {x:Int=1", false)
            .with_depth(3),
        // Boolean conditions
        TypedCompletionTestCase::new("while less", "while(x<10) {}", false).with_depth(1),
        TypedCompletionTestCase::new("while greater", "while(x>0) {}", false).with_depth(1),
        TypedCompletionTestCase::new("condition partial", "while(x", false).with_depth(4),
        // Variable references in expressions
        TypedCompletionTestCase::new("use var", "x:Int=5; y:Int=x;", false).with_depth(1),
        TypedCompletionTestCase::new("use in expr", "x:Int=5; y:Int=x+1;", false).with_depth(1),
    ];

    let grammar = imp_grammar();
    let res = run_test_batch(&grammar, &cases);
    res.assert_all_passed();
}

#[test]
fn check_fail() {
    let cases = vec![
        // Syntax errors
        TypedCompletionTestCase::new("assign before decl", "x=5;", true),
        TypedCompletionTestCase::new("missing type", "x=5;", true),
        TypedCompletionTestCase::new("missing value", "x:Int;", true),
        TypedCompletionTestCase::new("no semicolon", "x:Int=5", true),
        // Invalid types
        TypedCompletionTestCase::new("wrong type", "x:String=5;", true),
        TypedCompletionTestCase::new("lowercase type", "x:int=5;", true),
        // Unbound variables
        TypedCompletionTestCase::new("unbound var", "y:Int=x;", true),
        TypedCompletionTestCase::new("use before decl", "y:Int=x+1; x:Int=5;", true),
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
