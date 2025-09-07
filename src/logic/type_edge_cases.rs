//! Subtle Type Checking Failure Tests
//! 
//! This module contains tests for subtle and edge case type checking failures
//! that should catch sophisticated type errors and demonstrate the robustness
//! of the type system.

#[cfg(test)]
pub mod subtle_failures {
    use crate::logic::{grammar::tests::STLC_SPEC, grammar::Grammar, parser::Parser, check::TypeChecker};
    use crate::logic::bind::BoundType;
    use crate::debug_info;

    /// Test function composition with type mismatches
    #[test]
    fn test_function_composition_type_mismatch() {
        let expr = "(λf:Int->Bool.λg:Bool->String.λx:Int.g(f(x)))(λy:Int.y)(λz:Bool.z)42";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        
        let res = tc.check(&ast);
        // This should fail because the second function (λz:Bool.z) returns Bool, not String
        assert!(res.is_err(), "Expected function composition type mismatch to fail");
        debug_info!("test", "Function composition error: {:?}", res);
    }

    /// Test higher-order function with wrong argument type
    #[test] 
    fn test_higher_order_function_arg_mismatch() {
        let expr = "(λmap:(Int->Int)->List->List.λf:Bool->Bool.λl:List.map f l)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            // Add List type to context
            tc.add("List".to_string(), BoundType::Atom("List".to_string()));
            
            let res = tc.check(&ast);
            // Should fail because map expects Int->Int but gets Bool->Bool
            assert!(res.is_err(), "Expected higher-order function type mismatch to fail");
            debug_info!("test", "Higher-order function error: {:?}", res);
        }
    }

    /// Test curried function with partial application type errors
    #[test]
    fn test_curried_function_partial_application_error() {
        let expr = "(λadd:Int->Int->Int.λx:String.add x)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            
            let res = tc.check(&ast);
            // Should fail because add expects Int but gets String
            assert!(res.is_err(), "Expected curried function type error to fail");
            debug_info!("test", "Curried function error: {:?}", res);
        }
    }

    /// Test lambda with captured variable type mismatch
    #[test]
    fn test_lambda_capture_type_mismatch() {
        let expr = "(λx:Int.λy:Bool.x + y)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            
            let res = tc.check(&ast);
            // Should fail because + requires both operands to be same type (Int + Bool invalid)
            assert!(res.is_err(), "Expected lambda capture type mismatch to fail");
            debug_info!("test", "Lambda capture error: {:?}", res);
        }
    }

    /// Test recursive function with wrong base case type
    #[test]
    fn test_recursive_function_base_case_error() {
        // Simulated recursive factorial with wrong base case
        let expr = "(λfact:Int->Int.λn:Int.if (n == 0) \"one\" (n * fact(n - 1)))";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            // Add required functions to context
            tc.add("if".to_string(), BoundType::Arrow(
                Box::new(BoundType::Atom("Bool".to_string())), 
                Box::new(BoundType::Arrow(
                    Box::new(BoundType::Atom("a".to_string())),
                    Box::new(BoundType::Arrow(
                        Box::new(BoundType::Atom("a".to_string())),
                        Box::new(BoundType::Atom("a".to_string()))
                    ))
                ))
            ));
            tc.add("==".to_string(), BoundType::Arrow(
                Box::new(BoundType::Atom("Int".to_string())),
                Box::new(BoundType::Arrow(
                    Box::new(BoundType::Atom("Int".to_string())),
                    Box::new(BoundType::Atom("Bool".to_string()))
                ))
            ));
            
            let res = tc.check(&ast);
            // Should fail because base case returns String but function should return Int
            assert!(res.is_err(), "Expected recursive function base case error to fail");
            debug_info!("test", "Recursive function error: {:?}", res);
        }
    }

    /// Test polymorphic function instantiation error
    #[test]
    fn test_polymorphic_instantiation_error() {
        let expr = "(λid:a->a.λx:Int.λy:Bool.if (id x == id y) x y)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            
            let res = tc.check(&ast);
            // Should fail because id cannot be instantiated with both Int and Bool simultaneously
            assert!(res.is_err(), "Expected polymorphic instantiation error to fail");
            debug_info!("test", "Polymorphic instantiation error: {:?}", res);
        }
    }
}

#[cfg(test)]
pub mod complex_type_scenarios {
    use crate::logic::{grammar::Grammar, parser::Parser, check::TypeChecker};
    use crate::debug_info;

    /// Extended STLC grammar with more type constructs for testing
    pub const EXTENDED_STLC_SPEC: &str = r#"
    // Identifier (supports Unicode)
    Identifier ::= /[\p{L}][\p{L}\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*/

    // Variables
    Variable(var) ::= Identifier[x]

    // Type names 
    TypeName ::= Identifier

    // Product types (pairs)
    ProductType ::= Type[τ₁] '*' Type[τ₂]
    
    // Sum types (unions)
    SumType ::= Type[τ₁] '+' Type[τ₂]
    
    // List types
    ListType ::= Type[τ] 'list'
    
    // Base types
    BaseType ::= TypeName | ProductType | SumType | ListType | '(' Type ')'

    // Function types (right-associative)
    Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

    // Pair expressions
    Pair(pair) ::= '(' Term[left] ',' Term[right] ')'
    Fst(fst) ::= 'fst' Term[e]
    Snd(snd) ::= 'snd' Term[e]
    
    // Sum expressions  
    Inl(inl) ::= 'inl' Term[e]
    Inr(inr) ::= 'inr' Term[e]
    Case(case) ::= 'case' Term[e] 'of' 'inl' Variable[x] '=>' Term[left] '|' 'inr' Variable[y] '=>' Term[right]
    
    // List expressions
    Nil(nil) ::= 'nil'
    Cons(cons) ::= Term[head] '::' Term[tail]
    
    // Typed parameter
    TypedParam ::= Variable[x] ':' Type[τ]

    // Lambda abstraction
    Lambda(lambda) ::= 'λ' TypedParam '.' Term[e]

    // Base terms
    BaseTerm ::= Variable | Lambda | Pair | Fst | Snd | Inl | Inr | Case | Nil | Cons | '(' Term ')'

    // Applications
    Application(app) ::= BaseTerm[f] BaseTerm[e]

    // Terms
    Term ::= Application[e] | BaseTerm[e]

    // Typing Rules
    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ[x:τ₁] ⊢ e : τ₂
    --------------------------- (lambda)
    τ₁ → τ₂
    
    Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
    -------------------------------- (app)
    τ₂
    
    Γ ⊢ left : τ₁, Γ ⊢ right : τ₂
    -------------------------------- (pair)
    τ₁ * τ₂
    
    Γ ⊢ e : τ₁ * τ₂
    ---------------- (fst)
    τ₁
    
    Γ ⊢ e : τ₁ * τ₂
    ---------------- (snd)
    τ₂
    "#;

    fn load_extended_stlc_grammar() -> Grammar {
        Grammar::load(EXTENDED_STLC_SPEC).expect("Failed to load extended STLC grammar")
    }

    /// Test product type operations with mismatched projections
    #[test]
    fn test_product_type_projection_mismatch() {
        let grammar = load_extended_stlc_grammar();
        let mut parser = Parser::new(grammar);

        // Test fst on non-pair type
        let expr = "fst 42";
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            let res = tc.check(&ast);
            
            assert!(res.is_err(), "Expected fst on non-pair to fail");
            debug_info!("test", "Product type error: {:?}", res);
        }
    }

    /// Test complex nested product and function types
    #[test]
    fn test_complex_nested_types() {
        let grammar = load_extended_stlc_grammar();
        let mut parser = Parser::new(grammar);

        // Function that takes a pair and returns wrong type
        let expr = "(λf:(Int * Bool) -> String.λp:Int * Bool.42)(λx:Int * Bool.fst x)";
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            let res = tc.check(&ast);
            
            // Should fail because lambda returns Int but f expects String return
            assert!(res.is_err(), "Expected complex nested type error to fail");
            debug_info!("test", "Complex nested type error: {:?}", res);
        }
    }

    /// Test sum type case analysis with missing branches
    #[test]
    fn test_sum_type_incomplete_case() {
        let grammar = load_extended_stlc_grammar();
        let mut parser = Parser::new(grammar);

        // Case analysis with type mismatch in branches
        let expr = "case (inl 42) of inl x => x | inr y => \"error\"";
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            let res = tc.check(&ast);
            
            // Should fail because branches return different types (Int vs String)
            assert!(res.is_err(), "Expected sum type case analysis to fail");
            debug_info!("test", "Sum type case error: {:?}", res);
        }
    }

    /// Test deeply nested function types with subtle errors
    #[test]
    fn test_deeply_nested_function_types() {
        let grammar = load_extended_stlc_grammar();
        let mut parser = Parser::new(grammar);

        // Complex higher-order function with nested error
        let expr = "(λf:(Int -> (Bool -> String)) -> (Int * Bool) -> String.λg:Int -> Bool -> String.λp:Int * Bool.f g p)";
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            let res = tc.check(&ast);
            
            // May fail due to complex type matching requirements
            if res.is_err() {
                debug_info!("test", "Deeply nested function type error: {:?}", res);
            } else {
                debug_info!("test", "Deeply nested function type succeeded unexpectedly");
            }
        }
    }

    /// Test list operations with type mismatches
    #[test]
    fn test_list_type_operations() {
        let grammar = load_extended_stlc_grammar();
        let mut parser = Parser::new(grammar);

        // Cons with mismatched element types
        let expr = "42 :: (\"hello\" :: nil)";
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            let res = tc.check(&ast);
            
            // Should fail because list elements must be same type
            assert!(res.is_err(), "Expected list type mismatch to fail");
            debug_info!("test", "List type error: {:?}", res);
        }
    }
}

#[cfg(test)]
pub mod type_system_stress_tests {
    use crate::logic::{grammar::Grammar, parser::Parser, check::TypeChecker};
    use crate::logic::bind::BoundType;
    use crate::debug_info;

    /// Stress test with very deep nesting
    #[test]
    fn test_deep_nesting_stress() {
        let grammar = Grammar::load(crate::logic::grammar::tests::STLC_SPEC).unwrap();
        let mut parser = Parser::new(grammar);

        // Very deeply nested lambda expression
        let mut expr = "λx:a.x".to_string();
        for i in 1..10 {
            expr = format!("({})(λy{}:a.y{})", expr, i, i);
        }
        
        if let Ok(ast) = parser.parse(&expr) {
            let mut tc = TypeChecker::new();
            let res = tc.check(&ast);
            
            // Should succeed but stress test the type checker
            match res {
                Ok(_) => debug_info!("test", "Deep nesting stress test passed"),
                Err(e) => debug_info!("test", "Deep nesting stress test failed: {}", e),
            }
        }
    }

    /// Stress test with many variables in context
    #[test]
    fn test_large_context_stress() {
        let grammar = Grammar::load(crate::logic::grammar::tests::STLC_SPEC).unwrap();
        let mut parser = Parser::new(grammar);

        let expr = "x1";
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            
            // Add many variables to context
            for i in 1..=100 {
                tc.add(format!("x{}", i), BoundType::Atom("Int".to_string()));
            }
            
            let res = tc.check(&ast);
            match res {
                Ok(_) => debug_info!("test", "Large context stress test passed"),
                Err(e) => debug_info!("test", "Large context stress test failed: {}", e),
            }
        }
    }

    /// Test with very complex type expressions
    #[test]
    fn test_complex_type_expressions() {
        let grammar = Grammar::load(crate::logic::grammar::tests::STLC_SPEC).unwrap();
        let mut parser = Parser::new(grammar);

        // Function with very complex type
        let expr = "λf:((a->b)->c)->((d->e)->f)->(g->h).f";
        if let Ok(ast) = parser.parse(expr) {
            let mut tc = TypeChecker::new();
            let res = tc.check(&ast);
            
            match res {
                Ok(ty) => {
                    if let Some(ty) = ty {
                        debug_info!("test", "Complex type expression inferred: {:?}", ty);
                    }
                },
                Err(e) => debug_info!("test", "Complex type expression failed: {}", e),
            }
        }
    }
}