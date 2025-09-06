//! Simple test cases for debugging grammar issues
//! 
//! This module contains minimal test cases to identify and fix
//! any fundamental grammar or parsing issues.

#[cfg(test)]
pub mod debug_tests {
    use crate::logic::{grammar::Grammar, parser::Parser};

    // Very simple arithmetic grammar for testing
    const SIMPLE_ARITH: &str = r#"
    Number ::= /\d+/
    Expr ::= Number | '(' Expr '+' Expr ')'
    "#;

    // Minimal lambda calculus without complex features
    const MINIMAL_LAMBDA: &str = r#"
    Variable(var) ::= /[a-z]+/[x]
    Lambda(lambda) ::= 'lam' Variable[x] '.' Expr[e]
    App(app) ::= '(' Expr[f] Expr[e] ')'
    Expr ::= Variable | Lambda | App
    
    x ∈ Γ
    ---------- (var)
    Γ(x)
    
    Γ[x:τ] ⊢ e : σ
    --------------- (lambda)
    τ → σ
    
    Γ ⊢ f : τ → σ, Γ ⊢ e : τ
    -------------------------- (app)
    σ
    "#;

    #[test]
    fn test_simple_arithmetic() {
        let grammar = Grammar::load(SIMPLE_ARITH).expect("Failed to load simple arithmetic");
        let mut parser = Parser::new(grammar.clone());

        // Test basic number
        let ast = parser.parse("42").expect("Failed to parse number");
        println!("Start nonterminal: {:?}", grammar.start_nonterminal());
        println!("Parsed AST value: {}", ast.value());
        // The start nonterminal is typically "Expr" based on the first production
        assert_eq!(ast.value(), "Expr");

        // Test parenthesized expression  
        let ast = parser.parse("(1 + 2)").expect("Failed to parse expression");
        assert_eq!(ast.value(), "Expr");
    }

    #[test] 
    fn test_minimal_lambda() {
        let grammar = Grammar::load(MINIMAL_LAMBDA).expect("Failed to load minimal lambda");
        let mut parser = Parser::new(grammar.clone());

        // Test variable
        let ast = parser.parse("x").expect("Failed to parse variable");
        println!("Start nonterminal: {:?}", grammar.start_nonterminal());
        println!("Parsed AST value: {}", ast.value());
        // The start nonterminal should be "Variable" (first production) or "Expr"
        // Let's check what it actually is
        assert!(ast.value() == "Variable" || ast.value() == "Expr");

        // Test lambda
        let ast = parser.parse("lam x.x").expect("Failed to parse lambda");
        assert!(ast.value() == "Lambda" || ast.value() == "Expr");

        // Test application
        let ast = parser.parse("(f x)").expect("Failed to parse application");
        assert!(ast.value() == "App" || ast.value() == "Expr");
    }

    #[test]
    fn test_grammar_loading_individually() {
        // Test each grammar can be loaded without issues
        let test_cases = vec![
            ("Simple Arithmetic", SIMPLE_ARITH),
            ("Minimal Lambda", MINIMAL_LAMBDA),
        ];

        for (name, spec) in test_cases {
            match Grammar::load(spec) {
                Ok(grammar) => {
                    println!("✅ {} loaded: {} productions, {} typing rules", 
                        name, grammar.productions.len(), grammar.typing_rules.len());
                }
                Err(e) => {
                    panic!("❌ {} failed to load: {}", name, e);
                }
            }
        }
    }
}