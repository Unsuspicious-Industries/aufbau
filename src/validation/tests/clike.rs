//! C-Like Language Tests
//!
//! Tests typed completion for an imperative C-like language with:
//! - Variable declarations with/without initializers
//! - Function definitions with parameters
//! - Control flow (if, while, for)
//! - Expression typing (arithmetic, boolean)
//! - Block scoping
//!
//! This grammar tests context propagation through statements
//! and complex nested scopes.

use super::*;

/// Load the C-like grammar from examples/clike.spec
fn clike_grammar() -> Grammar {
    load_example_grammar("clike")
}

// ============================================================================
// Batch Test Cases
// ============================================================================

#[test]
fn check_completable() -> () {
    let cases = vec![
        // Already complete - should be fast (depth 0 or 1)
        TypedCompletionTestCase::new("var decl complete", "int x = 5;", false).with_depth(1),
        TypedCompletionTestCase::new("var no init", "int x;", false).with_depth(1),
        TypedCompletionTestCase::new("func empty body", "int main() {}", false).with_depth(1),
        TypedCompletionTestCase::new("func with return", "int main() {return 0;}", false).with_depth(1),
        
        // Nearly complete - need 1-2 tokens
        TypedCompletionTestCase::new("var decl value", "int x = 5", false).with_depth(2),
        TypedCompletionTestCase::new("func params", "int main()", false).with_depth(3),
        
        // Partial - use moderate depth (C-like has huge branching factor!)
        TypedCompletionTestCase::new("var decl start", "int x", false).with_depth(2),
        TypedCompletionTestCase::new("type keyword", "int", false).with_depth(3),
        TypedCompletionTestCase::new("func body open", "int main() {", false).with_depth(3),
        
        // More complex statements
        TypedCompletionTestCase::new("if statement", "int main() {if (1) {", false).with_depth(5),
        TypedCompletionTestCase::new("for loop", "int main() {for (int i = 0; i < 10; i = i + 1) {", false).with_depth(7),
        TypedCompletionTestCase::new("while loop", "int main() {while (x < 10) {", false).with_depth(5),
        
    ];

    let grammar = clike_grammar();
    let res = run_test_batch(&grammar, &cases);
    assert!(res.passed == cases.len(), "{} out of {} tests passed", res.passed, cases.len());
    println!("Average duration: {:?}", res.avg_duration);

}


#[test]
fn check_fail() -> () {
    let cases = vec![
        // Syntax errors
        TypedCompletionTestCase::new("assign before var", "= 5", true),
        TypedCompletionTestCase::new("close brace first", "}", true),
        TypedCompletionTestCase::new("close paren first", ")", true),
        TypedCompletionTestCase::new("semicolon first", ";", true),
        // Invalid keywords/identifiers
        TypedCompletionTestCase::new("unknown type", "foo x = 5;", true),
    ];

    let grammar = clike_grammar();
    let res = run_test_batch(&grammar, &cases);
    assert!(res.passed == cases.len(), "{} out of {} tests passed", res.passed, cases.len());
    println!("Average duration: {:?}", res.avg_duration);


}

