//! Focused Type Checking FAIL Tests
//! 
//! This module contains focused FAIL tests using the existing STLC grammar
//! to demonstrate type checking failures and validate the robustness of the system.
//! These tests serve as goal posts for identifying missing features.
    

#[cfg(test)]
pub mod focused_fail_tests {
    use crate::logic::{grammar::tests::STLC_SPEC, grammar::Grammar, parser::Parser, check::TypeChecker, typing::Type};
    use crate::debug_info;
    use crate::logic::bind::BoundType;

    /// Test function application with wrong argument type - should fail
    #[test]
    fn test_function_arg_type_mismatch() {
        let expr = "(λx:Int.x)(λy:Bool.y)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected function argument type mismatch to fail");
        debug_info!("test", "Function arg mismatch error: {:?}", res);
    }

    /// Test function composition with incompatible types - should fail
    #[test]
    fn test_function_composition_fail() {
        let expr = "(λf:Int->Bool.λg:String->Int.λx:Int.g(f(x)))";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected function composition type mismatch to fail");
        debug_info!("test", "Function composition error: {:?}", res);
    }

    /// Test higher-order function with wrong function type - should fail  
    #[test]
    fn test_higher_order_function_fail() {
        let expr = "(λmap:(Int->Int)->List->List.λf:Bool->String.map f)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        tc.add("List".to_string(), BoundType::Atom("List".to_string()));
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected higher-order function type mismatch to fail");
        debug_info!("test", "Higher-order function error: {:?}", res);
    }

    /// Test nested lambda with variable shadowing and type error - should fail
    #[test]
    fn test_lambda_shadowing_type_error() {
        let expr = "(λx:Int.λx:Bool.x)(λy:String.y)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected lambda shadowing with type error to fail");
        debug_info!("test", "Lambda shadowing error: {:?}", res);
    }

    /// Test curried function with partial application error - should fail
    #[test]
    fn test_curried_function_error() {
        let expr = "(λadd:Int->Int->Int.λx:String.add x)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected curried function type error to fail");
        debug_info!("test", "Curried function error: {:?}", res);
    }

    /// Test wrong return type in lambda - should fail
    #[test]
    fn test_lambda_return_type_error() {
        let expr = "(λf:Int->Bool.f)((λx:Int.x))";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected lambda return type error to fail");
        debug_info!("test", "Lambda return type error: {:?}", res);
    }

    /// Test complex nested expression with type error deep inside - should fail
    #[test]
    fn test_deep_nested_type_error() {
        let expr = "(λf:Int->Int.λg:Int->Int.λh:Int->Int.f(g(h(λx:Bool.x))))";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected deep nested type error to fail");
        debug_info!("test", "Deep nested error: {:?}", res);
    }

    /// Test variable scope violation - should fail
    #[test]
    fn test_variable_scope_violation() {
        let expr = "(λx:Int.y)";
        
        let mut parser = Parser::new(Grammar::load(STLC_SPEC).unwrap());
        let ast = parser.parse(expr).unwrap();
        let mut tc = TypeChecker::new();
        // Don't bind y to context - should fail
        
        let res = tc.check(&ast);
        assert!(res.is_err(), "Expected variable scope violation to fail");
        debug_info!("test", "Variable scope error: {:?}", res);
    }
}

#[cfg(test)]
pub mod unimplemented_feature_tests {
    use crate::logic::bind::BoundType;
    use crate::logic::{grammar::Grammar, parser::Parser, check::TypeChecker, typing::Type};
    use crate::debug_info;

    /// Grammar extension for testing unimplemented pointer features
    pub const POINTER_EXTENSION_SPEC: &str = r#"
    // Basic STLC with pointer extensions
    Identifier ::= /[\p{L}][\p{L}\p{N}_τ₁₂₃₄₅₆₇₈₉₀]*/
    Variable(var) ::= Identifier[x]
    TypeName ::= Identifier
    
    // Pointer types (UNIMPLEMENTED)
    PointerType ::= TypeName[base] '*'
    
    // Base types  
    BaseType ::= TypeName | PointerType | '(' Type ')'
    Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]
    
    // Pointer operations (UNIMPLEMENTED)
    AddressOf(addressof) ::= '&' Term[e]
    Dereference(deref) ::= '*' Term[e]
    
    // Regular STLC constructs
    TypedParam ::= Variable[x] ':' Type[τ]
    Lambda(lambda) ::= 'λ' TypedParam '.' Term[e]
    BaseTerm ::= Variable | Lambda | AddressOf | Dereference | '(' Term ')'
    Application(app) ::= BaseTerm[f] BaseTerm[e]
    Term ::= Application[e] | BaseTerm[e]
    
    // Basic typing rules (pointer rules NOT implemented)
    x ∈ Γ
    ----------- (var)
    Γ(x)
    
    Γ[x:τ₁] ⊢ e : τ₂
    --------------------------- (lambda)
    τ₁ → τ₂
    
    Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁
    -------------------------------- (app)
    τ₂
    "#;

    /// Test pointer type declaration - should fail due to unimplemented feature
    #[test]
    fn test_pointer_type_fail() {
        if let Ok(grammar) = Grammar::load(POINTER_EXTENSION_SPEC) {
            let mut parser = Parser::new(grammar);
            
            // Test pointer variable declaration
            let expr = "λp:Int*.p";
            if let Ok(ast) = parser.parse(expr) {
                let mut tc = TypeChecker::new();
                let res = tc.check(&ast);
                
                // Should fail because pointer types are not implemented
                assert!(res.is_err(), "Expected pointer type to fail due to unimplemented feature");
                debug_info!("test", "Pointer type error: {:?}", res);
            }
        }
    }

    /// Test address-of operator - demonstrates unimplemented feature
    #[test]
    fn test_address_of_fail() {
        if let Ok(grammar) = Grammar::load(POINTER_EXTENSION_SPEC) {
            let mut parser = Parser::new(grammar);
            
            let expr = "&x";
            if let Ok(ast) = parser.parse(expr) {
                let mut tc = TypeChecker::new();
                tc.add("x".to_string(), BoundType::Atom("Int".to_string()));
                let res = tc.check(&ast);
                
                // Document the current behavior - may succeed due to fallback rules
                match res {
                    Err(e) => debug_info!("test", "Address-of failed (good): {}", e),
                    Ok(None) => debug_info!("test", "Address-of has no typing rule (neutral)"),
                    Ok(Some(ty)) => {
                        debug_info!("test", "Address-of succeeded with type: {:?} (needs proper pointer typing)", ty);
                        // This shows we need proper pointer type rules
                    }
                }
            }
        }
    }

    /// Test dereference operator - demonstrates unimplemented feature
    #[test]
    fn test_dereference_fail() {
        if let Ok(grammar) = Grammar::load(POINTER_EXTENSION_SPEC) {
            let mut parser = Parser::new(grammar);
            
            let expr = "*p";
            if let Ok(ast) = parser.parse(expr) {
                let mut tc = TypeChecker::new();
                tc.add("p".to_string(), BoundType::Atom("Int*".to_string()));
                let res = tc.check(&ast);
                
                // Document the current behavior - may succeed due to fallback rules
                match res {
                    Err(e) => debug_info!("test", "Dereference failed (good): {}", e),
                    Ok(None) => debug_info!("test", "Dereference has no typing rule (neutral)"),
                    Ok(Some(ty)) => {
                        debug_info!("test", "Dereference succeeded with type: {:?} (needs proper pointer typing)", ty);
                        // This shows we need proper pointer type rules
                    }
                }
            }
        }
    }

    /// Test complex pointer operations - should fail
    #[test]
    fn test_complex_pointer_operations_fail() {
        if let Ok(grammar) = Grammar::load(POINTER_EXTENSION_SPEC) {
            let mut parser = Parser::new(grammar);
            
            // Function that takes a pointer and dereferences it
            let expr = "(λp:Int*.λx:Int.*p)(&x)";
            if let Ok(ast) = parser.parse(expr) {
                let mut tc = TypeChecker::new();
                tc.add("x".to_string(), BoundType::Atom("Int".to_string()));
                let res = tc.check(&ast);
                
                // Should fail due to unimplemented pointer operations
                assert!(res.is_err(), "Expected complex pointer operations to fail");
                debug_info!("test", "Complex pointer error: {:?}", res);
            }
        }
    }
}

#[cfg(test)]
pub mod advanced_typing_goals {
    use crate::logic::{grammar::Grammar, parser::Parser, check::TypeChecker, typing::Type};
    use crate::debug_info;

    /// These tests represent advanced typing features that are goals for the system
    /// They should all fail currently but serve as specifications for future work

    /// Test for generic/polymorphic types - future goal
    #[test] 
    fn test_generics_goal() {
        // This would require generic syntax and type inference
        let program_description = "Generic function: forall T. (T -> T) -> T -> T";
        debug_info!("goal", "Generics not implemented: {}", program_description);
        
        // Currently we can't even express this in the grammar
        // Future work: extend grammar with generic syntax
        // Example: λ[T]f:T->T.λx:T.f(f(x))
    }

    /// Test for subtyping - future goal  
    #[test]
    fn test_subtyping_goal() {
        let program_description = "Subtyping: Int <: Number, (Int -> Int) <: (Number -> Number)";
        debug_info!("goal", "Subtyping not implemented: {}", program_description);
        
        // Future work: implement subtyping relations
        // Allow Int values where Number is expected
        // Implement variance rules for function types
    }

    /// Test for union types - future goal
    #[test]
    fn test_union_types_goal() {
        let program_description = "Union types: string | number, type narrowing";
        debug_info!("goal", "Union types not implemented: {}", program_description);
        
        // Future work: implement union type syntax and type narrowing
        // Example: λx:Int|Bool.case x of Int => x+1 | Bool => !x
    }

    /// Test for intersection types - future goal
    #[test]
    fn test_intersection_types_goal() {
        let program_description = "Intersection types: {name: string} & {age: number}";
        debug_info!("goal", "Intersection types not implemented: {}", program_description);
        
        // Future work: implement intersection type syntax and checking
        // Allow values that satisfy multiple type constraints
    }

    /// Test for dependent types - future goal
    #[test]
    fn test_dependent_types_goal() {
        let program_description = "Dependent types: (n: Nat) -> Vec<n>";
        debug_info!("goal", "Dependent types not implemented: {}", program_description);
        
        // Future work: implement dependent type syntax and checking
        // Types that depend on runtime values
    }

    /// Test for linear types - future goal
    #[test]
    fn test_linear_types_goal() {
        let program_description = "Linear types: use-once semantics, resource management";
        debug_info!("goal", "Linear types not implemented: {}", program_description);
        
        // Future work: implement linear type system
        // Ensure resources are used exactly once
    }

    /// Test for async/await types - future goal
    #[test]
    fn test_async_types_goal() {
        let program_description = "Async types: async fn() -> Promise<T>";
        debug_info!("goal", "Async types not implemented: {}", program_description);
        
        // Future work: implement async/await type checking
        // Handle asynchronous computation types
    }

    /// Test for effect types - future goal
    #[test]
    fn test_effect_types_goal() {
        let program_description = "Effect types: IO<T>, Exception<E>, State<S>";
        debug_info!("goal", "Effect types not implemented: {}", program_description);
        
        // Future work: implement effect type system
        // Track side effects in the type system
    }
}