/*
Tests for variable redeclaration prevention.

The type system enforces strong typing where:
1. Variables are fixed types (can't be rebound)
2. Context extension fails if variable already exists
3. This is enforced during evaluation
*/

use crate::logic::Parser;
use crate::logic::grammar::Grammar;
use crate::logic::typing::Type;
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::eval::check_tree;
use crate::validation::completable::load_example_grammar;

fn stlc() -> Grammar {
    load_example_grammar("stlc")
}

#[test]
fn test_context_extend_rejects_duplicate() {
    // Direct test of Context::extend rejecting duplicate variable names
    let ctx = Context::new();

    // First binding should succeed
    let ctx = ctx
        .extend("x".to_string(), Type::Raw("Int".to_string()))
        .expect("First binding should succeed");

    // Second binding with same name should fail
    let result = ctx.extend("x".to_string(), Type::Raw("Bool".to_string()));

    assert!(
        result.is_err(),
        "Context should reject duplicate variable 'x'"
    );
    assert!(
        result
            .unwrap_err()
            .contains("already contains binding for 'x'"),
        "Error message should mention duplicate binding"
    );
}

#[test]
fn test_context_extend_allows_different_variables() {
    // Verify that different variables can be added
    let ctx = Context::new();

    let ctx = ctx
        .extend("x".to_string(), Type::Raw("Int".to_string()))
        .expect("First binding should succeed");

    let ctx = ctx
        .extend("y".to_string(), Type::Raw("Bool".to_string()))
        .expect("Second binding with different name should succeed");

    let ctx = ctx
        .extend("z".to_string(), Type::Raw("String".to_string()))
        .expect("Third binding with different name should succeed");

    // Verify all bindings are present
    assert!(ctx.lookup("x").is_some());
    assert!(ctx.lookup("y").is_some());
    assert!(ctx.lookup("z").is_some());
}

#[test]
fn test_nested_lambda_same_variable_rejected() {
    // λx : Int. λx : Bool. x
    // This should fail because inner lambda tries to rebind 'x'
    let g = stlc();
    let mut p = Parser::new(g.clone());

    let ast = p.partial("λx : Int. λx : Bool. x").unwrap();

    // Parse should succeed but typing should fail
    let completes = ast.completes();

    for tree in completes {
        let status = check_tree(&tree, &g);

        // Should be malformed because inner lambda tries to rebind 'x'
        match status {
            TreeStatus::Malformed => {
                // Expected - variable shadowing/redeclaration is not allowed
                return;
            }
            TreeStatus::Valid(_) | TreeStatus::Partial(_) => {
                panic!(
                    "Expected Malformed status for nested lambda with duplicate variable name, got: {:?}",
                    status
                );
            }
            TreeStatus::TooDeep => {
                panic!("Unexpected TooDeep status");
            }
        }
    }
}

#[test]
fn test_nested_lambda_different_variables_accepted() {
    // λx : Int. λy : Bool. x
    // This should succeed because variables have different names
    let g = stlc();
    let mut p = Parser::new(g.clone());

    let ast = p.partial("λx : Int. λy : Bool. x").unwrap();

    let completes = ast.completes();
    assert!(
        !completes.is_empty(),
        "Should have at least one complete parse"
    );

    for tree in completes {
        let status = check_tree(&tree, &g);

        // Should be valid or partial, not malformed
        match status {
            TreeStatus::Valid(_) | TreeStatus::Partial(_) => {
                // Expected - different variable names are allowed
                return;
            }
            TreeStatus::Malformed => {
                panic!(
                    "Expected Valid/Partial status for nested lambda with different variable names, got Malformed"
                );
            }
            TreeStatus::TooDeep => {
                panic!("Unexpected TooDeep status");
            }
        }
    }
}

#[test]
fn test_triple_nested_lambda_duplicate_rejected() {
    // λx : Int. λy : Bool. λx : String. x
    // Middle variable is different, but innermost tries to rebind 'x'
    let g = stlc();
    let mut p = Parser::new(g.clone());

    let ast = p.partial("λx : Int. λy : Bool. λx : String. x").unwrap();

    let completes = ast.completes();

    for tree in completes {
        let status = check_tree(&tree, &g);

        // Should be malformed - innermost lambda tries to rebind 'x' which is already in context
        match status {
            TreeStatus::Malformed => {
                // Expected
                return;
            }
            TreeStatus::Valid(_) | TreeStatus::Partial(_) => {
                panic!(
                    "Expected Malformed status for triple nested lambda with duplicate 'x', got: {:?}",
                    status
                );
            }
            TreeStatus::TooDeep => {
                panic!("Unexpected TooDeep status");
            }
        }
    }
}

#[test]
fn test_triple_nested_lambda_all_different_accepted() {
    // λx : Int. λy : Bool. λz : String. x
    // All different variables - should succeed
    let g = stlc();
    let mut p = Parser::new(g.clone());

    let ast = p.partial("λx : Int. λy : Bool. λz : String. x").unwrap();

    let completes = ast.completes();
    assert!(
        !completes.is_empty(),
        "Should have at least one complete parse"
    );

    for tree in completes {
        let status = check_tree(&tree, &g);

        // Should be valid or partial
        match status {
            TreeStatus::Valid(_) | TreeStatus::Partial(_) => {
                // Expected - all variables have different names
                return;
            }
            TreeStatus::Malformed => {
                panic!(
                    "Expected Valid/Partial status for triple nested lambda with all different variables, got Malformed"
                );
            }
            TreeStatus::TooDeep => {
                panic!("Unexpected TooDeep status");
            }
        }
    }
}

#[test]
fn test_lambda_with_application_duplicate_rejected() {
    // λx : Int. (λx : Bool. x) x
    // Inner lambda rebinds x, then applies it to outer x
    // The inner lambda should fail to type check due to redeclaration
    let g = stlc();
    let mut p = Parser::new(g.clone());

    let ast = p.partial("λx : Int. (λx : Bool. x) x").unwrap();

    let completes = ast.completes();

    for tree in completes {
        let status = check_tree(&tree, &g);

        // Should be malformed due to inner lambda rebinding x
        match status {
            TreeStatus::Malformed => {
                // Expected
                return;
            }
            TreeStatus::Valid(_) | TreeStatus::Partial(_) => {
                panic!(
                    "Expected Malformed status for lambda application with duplicate binding, got: {:?}",
                    status
                );
            }
            TreeStatus::TooDeep => {
                panic!("Unexpected TooDeep status");
            }
        }
    }
}

#[test]
fn test_context_multiple_duplicates() {
    // Test that multiple attempts to add the same variable all fail
    let ctx = Context::new();

    let ctx = ctx
        .extend("x".to_string(), Type::Raw("Int".to_string()))
        .expect("First binding should succeed");

    // Try multiple times with different types - all should fail
    assert!(
        ctx.extend("x".to_string(), Type::Raw("Bool".to_string()))
            .is_err()
    );
    assert!(
        ctx.extend("x".to_string(), Type::Raw("String".to_string()))
            .is_err()
    );
    assert!(
        ctx.extend("x".to_string(), Type::Atom("A".to_string()))
            .is_err()
    );

    // Original binding should still be intact
    match ctx.lookup("x") {
        Some(Type::Raw(s)) if s == "Int" => {} // Expected
        other => panic!("Expected Raw(Int), got: {:?}", other),
    }
}

#[test]
fn test_sequential_scopes_same_variable_rejected() {
    // Test that even in sequential contexts (not nested),
    // we can't redeclare variables since context is cumulative
    let ctx = Context::new();

    // Add x in first "scope"
    let ctx1 = ctx
        .extend("x".to_string(), Type::Raw("Int".to_string()))
        .expect("First x should work");

    // Add y in second "scope" (based on ctx1)
    let ctx2 = ctx1
        .extend("y".to_string(), Type::Raw("Bool".to_string()))
        .expect("y should work");

    // Try to add x again in third "scope" (based on ctx2) - should fail
    let result = ctx2.extend("x".to_string(), Type::Raw("String".to_string()));

    assert!(
        result.is_err(),
        "Should not be able to rebind x in cumulative context"
    );
}
