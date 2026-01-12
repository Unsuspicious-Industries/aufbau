use crate::logic::grammar::Grammar;
use crate::testing::*;
use super::Parser;

/// The STLC grammar spec loaded from the examples directory
fn stlc_grammar() -> &'static Grammar {
    grammars::stlc()
}

/// Helper to check that parsing produces the expected serialized AST
/// Uses structural matching (ignores derivative extensions/remainders)
fn assert_stlc_parse_matches(input: &str, expected_serialized: &str) {
    assert_parse_structurally_matches(stlc_grammar(), input, expected_serialized);
}


#[test]
fn test_stlc_simple_variable() {
    let g = stlc_grammar();
    let mut p = Parser::new(g.clone());

    let ast = p.parse("x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("foo").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("bar123").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_simple_variable_with_serialization() {
    assert_stlc_parse_matches(
        "x",
        r#"(Expression @0 #1
  (AtomicExpression @0 #1
    (Variable @0 #1
      (Identifier @0 $x #1
        (T "x")))))"#
    );
}

#[test]
fn test_stlc_simple_application_with_serialization() {
    assert_stlc_parse_matches(
        "f x",
                r#"(Expression @1 #2
    (Application @0 #2
    (AtomicExpression @0 #1
      (Variable @0 #1
        (Identifier @0 $x #1
                    (T "f"))))
    (Expression @0 $r #1
      (AtomicExpression @0 #1
        (Variable @0 #1
          (Identifier @0 $x #1
                        (T "x"))))))"#
    );
}

#[test]
fn test_stlc_identity_lambda_with_serialization() {
    // λx:A.x
    assert_stlc_parse_matches(
        "λx:A.x",
                r#"(Expression @0 #6
    (AtomicExpression @2 #6
        (Lambda @0 #6
            (T "λ")
      (Identifier @0 $a #1
                (T "x"))
            (T ":")
      (Type @0 $τ #1
        (AtomicType @0 #1
          (BaseType @0 #1
            (TypeName @0 #1
                            (T "A")))))
            (T ".")
            (Expression @1 $e #1
                (Application @0 #1
                    (AtomicExpression @0 $l #1
                        (Variable @0 #1
                            (Identifier @0 $x #1
                                (T "x"))))
                    (Expression @1 $r #0
                        (Application @0 #0
                            (AtomicExpression @1 $l #0
                                (T~ "")))))))))"#
    );
}


#[test]
fn test_stlc_parenthesized_variable() {
    let g = stlc_grammar();
    let mut p = Parser::new(g.clone());

    let ast = p.parse("(x)").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("((x))").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("(((x)))").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 2: LAMBDA ABSTRACTIONS - No spaces between tokens!
// ============================================================================

#[test]
fn test_stlc_simple_lambda() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Identity function - NO SPACES
    let ast = p.parse("λx:A.x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_lambda_with_function_type() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Lambda with function type annotation
    let ast = p.parse("λf:A->B.f").unwrap();
    assert!(ast.is_complete());

    // Lambda with nested function type (right-associative: A -> (B -> C))
    let ast = p.parse("λf:A->B->C.f").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_nested_lambdas() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Curried function: λx. λy. x
    let ast = p.parse("λx:A.λy:B.x").unwrap();
    assert!(ast.is_complete());

    // Triple nested
    let ast = p.parse("λx:A.λy:B.λz:C.x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_lambda_with_parenthesized_type() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    let ast = p.parse("λ x : ( A -> B ) . x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("λ x : ( ( A -> B ) -> C ) . x").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 3: FUNCTION TYPES - Right-associativity and nesting
// ============================================================================

#[test]
fn test_stlc_simple_function_type() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Simple function type in lambda annotation
    let ast = p.parse("λ x : A -> B . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_function_type_right_associative() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // A -> B -> C should parse as A -> (B -> C) (right-associative)
    let ast = p.parse("λ x : A -> B -> C . x").unwrap();
    assert!(ast.is_complete());

    // Four-level chain
    let ast = p.parse("λ x : A -> B -> C -> D . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_function_type_with_parens() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // (A -> B) -> C  (different from A -> B -> C)
    let ast = p.parse("λ x : ( A -> B ) -> C . x").unwrap();
    assert!(ast.is_complete());

    // A -> (B -> C) explicit (same as A -> B -> C but explicit)
    let ast = p.parse("λ x : A -> ( B -> C ) . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_complex_nested_types() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // ((A -> B) -> C) -> D
    let ast = p.parse("λ x : ( ( A -> B ) -> C ) -> D . x").unwrap();
    assert!(ast.is_complete());

    // (A -> (B -> C)) -> (D -> E)
    let ast = p.parse("λ x : ( A -> ( B -> C ) ) -> ( D -> E ) . x").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 4: APPLICATIONS - Left-associativity (the tricky part!)
// ============================================================================

#[test]
fn test_stlc_simple_application() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // f x
    let ast = p.parse("f x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_chained_application() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // f x y should parse as (f x) y (left-associative)
    let ast = p.parse("f x y").unwrap();
    assert!(ast.is_complete());

    // f x y z should parse as ((f x) y) z
    let ast = p.parse("f x y z").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_application_long_chain() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Long application chain - stress test left recursion handling
    let ast = p.parse("f a b c d e").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_application_with_parentheses() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // (f x) y - explicit left assoc
    let ast = p.parse("( f x ) y").unwrap();
    assert!(ast.is_complete());

    // f (x y) - right-nested application
    let ast = p.parse("f ( x y )").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_nested_parenthesized_applications() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // ((f x) y) z
    let ast = p.parse("( ( f x ) y ) z").unwrap();
    assert!(ast.is_complete());

    // f (x (y z))
    let ast = p.parse("f ( x ( y z ) )").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 5: LAMBDA + APPLICATION COMBINATIONS
// ============================================================================

#[test]
fn test_stlc_apply_lambda() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Apply a lambda to an argument
    let ast = p.parse("( λ x : A . x ) y").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_lambda_with_application_body() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Lambda whose body is an application
    let ast = p.parse("λ f : A -> B . f x").unwrap();
    assert!(ast.is_complete());

    // Lambda with chained application in body
    let ast = p.parse("λ f : A -> B -> C . f x y").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_apply_lambda_to_lambda() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Apply lambda to lambda
    let ast = p.parse("( λ x : A . x ) ( λ y : B . y )").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_lambda_returning_lambda() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Curried function that returns a lambda
    let ast = p.parse("λ x : A . λ y : B . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_nested_lambda_application() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Apply nested lambda
    let ast = p.parse("( λ x : A . λ y : B . x ) a b").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 6: CLASSIC LAMBDA CALCULUS TERMS
// ============================================================================

#[test]
fn test_stlc_identity() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Identity function: λx.x
    let ast = p.parse("λ x : A . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_const() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Const: λx.λy.x
    let ast = p.parse("λ x : A . λ y : B . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_apply() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Apply: λf.λx.f x
    let ast = p.parse("λ f : A -> B . λ x : A . f x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_flip() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Flip: λf.λx.λy.f y x
    let ast = p.parse("λ f : A -> B -> C . λ x : A . λ y : B . f y x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_compose() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Compose: λf.λg.λx.f (g x)
    let ast = p.parse("λ f : B -> C . λ g : A -> B . λ x : A . f ( g x )").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_s_combinator() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // S combinator: λx.λy.λz.x z (y z)
    let ast = p.parse("λ x : A -> B -> C . λ y : A -> B . λ z : A . x z ( y z )").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 7: PARTIAL PARSING - Incomplete expressions
// ============================================================================

#[test]
fn test_stlc_partial_lambda() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Incomplete lambda
    let ast = p.partial("λ x :").unwrap();
    assert!(!ast.is_complete());

    let ast = p.partial("λ x : A").unwrap();
    assert!(!ast.is_complete());

    let ast = p.partial("λ x : A .").unwrap();
    assert!(!ast.is_complete());
}

#[test]
fn test_stlc_application() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Partial application (might be complete as single var, or partial as application)
    let ast = p.partial("f").unwrap();
    // 'f' by itself is complete as a variable
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_partial_type() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Incomplete function type
    let ast = p.partial("λ x : A ->").unwrap();
    assert!(!ast.is_complete());
}

#[test]
fn test_stlc_partial_nested_parens() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    let ast = p.partial("( ( x").unwrap();
    assert!(!ast.is_complete());

    let ast = p.partial("( x )").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 8: STRESS TESTS - Deep nesting and long chains
// ============================================================================

#[test]
fn test_stlc_deep_nested_lambdas() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Very deep lambda nesting
    let ast = p.parse("λ a : A . λ b : B . λ c : C . λ d : D . λ e : E . a").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_deep_nested_types() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Deep type nesting
    let ast = p.parse("λ x : ( ( ( A -> B ) -> C ) -> D ) -> E . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_long_application_chain() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Long application chain - tests left recursion handling
    let ast = p.parse("f a b c d e f g h").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_complex_mixed_expression() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Complex expression mixing all features
    let ast = p.parse("( λ f : ( A -> B ) -> C . λ x : A -> B . f x ) ( λ y : A -> B . y )").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_deeply_nested_application_in_lambda() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Lambda with deeply nested application body
    let ast = p.parse("λ f : A -> B -> C -> D . f x y z").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 9: EDGE CASES
// ============================================================================

#[test]
fn test_stlc_single_char_identifiers() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    let ast = p.parse("x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("λ x : A . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_underscore_identifiers() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    let ast = p.parse("_x").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("λ _unused : A . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_application_of_parenthesized_expr() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Apply parenthesized expression
    let ast = p.parse("( x ) y").unwrap();
    assert!(ast.is_complete());

    let ast = p.parse("( ( x ) ) ( ( y ) )").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_multiple_parens_in_type() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    let ast = p.parse("λ x : ( ( ( A ) ) ) . x").unwrap();
    assert!(ast.is_complete());
}

// ============================================================================
// CATEGORY 10: REGRESSION TESTS - Common parsing pitfalls
// ============================================================================

#[test]
fn test_stlc_app_vs_lambda_ambiguity() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Lambda should extend as far right as possible
    // λx.f x means λx.(f x), not (λx.f) x
    let ast = p.parse("λ x : A . f x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_type_arrow_vs_expr() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Arrow in type position
    let ast = p.parse("λ x : A -> B . x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_consecutive_lambdas_applied() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Two lambdas in application
    let ast = p.parse("( λ x : A . x ) ( λ y : B . y ) z").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_stlc_lambda_in_application_chain() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);

    // Lambda as argument in chain
    let ast = p.parse("f ( λ x : A . x ) y").unwrap();
    assert!(ast.is_complete());
}


// PArtial parsing tests 

#[test]
fn test_stlc_partial_application() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);
    // Partial application
    let ast = p.partial("(f ").unwrap();
    println!("Partial AST: {}", ast);

}

#[test]
fn test_partial_typing() {
    let g = stlc_grammar().clone();
    let mut p = Parser::new(g);
    // Partial type annotation
    let ast = p.partial("λ x : A -> ").unwrap();
    println!("Partial AST: {}", ast);
}
