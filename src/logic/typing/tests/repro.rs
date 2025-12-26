//! Reproduction tests for specific parsing issues

use crate::logic::typing::Type;
use crate::logic::typing::eval::{check_tree, check_tree_with_context};
use crate::logic::typing::core::Context;
use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::structure::NonTerminal;

#[test]
fn test_parse_silent_var() {
    let t = Type::parse("?A");
    assert!(t.is_ok(), "Failed to parse ?A: {:?}", t.err());
    // Meta variables like ?A now parse as Type::Meta("A")
    if let Ok(Type::Meta(s)) = t {
        assert_eq!(s, "A");
    } else {
        panic!("Expected Meta(\"A\"), got {:?}", t);
    }
}

#[test]
fn test_parse_silent_var_arrow() {
    let t = Type::parse("?A -> ?B");
    assert!(t.is_ok(), "Failed to parse ?A -> ?B: {:?}", t.err());
}

fn xtlc_grammar() -> Grammar {
    let path = std::path::PathBuf::from("examples/xtlc.spec");
    let content = std::fs::read_to_string(&path).expect("read xtlc.spec");
    Grammar::load(&content).expect("parse xtlc grammar")
}

fn display_tree(nt: &NonTerminal, indent: usize) -> String {
    use crate::logic::partial::structure::{Node, Terminal};
    let prefix = "  ".repeat(indent);
    let rule_info = nt.production.rule.as_ref().map(|r| format!("({})", r)).unwrap_or_default();
    let mut result = format!("{}{}{}[alt={}]\n", prefix, nt.name, rule_info, nt.alternative_index);
    for child in &nt.children {
        match child {
            Node::NonTerminal(child_nt) => {
                result.push_str(&display_tree(child_nt, indent + 1));
            }
            Node::Terminal(Terminal::Complete { value, .. }) => {
                result.push_str(&format!("{}  '{}'\n", prefix, value));
            }
            Node::Terminal(Terminal::Partial { value, .. }) => {
                result.push_str(&format!("{}  '{}'...\n", prefix, value));
            }
        }
    }
    result
}

#[test]
fn test_double_apply_debug() {
    let grammar = xtlc_grammar();
    
    // This works:
    let working = "λf:A->B->C.λx:A.λy:B.(f x) y";
    // This doesn't:
    let failing = "λf:A->B->C.λx:A.λy:B.f x y";
    
    let mut parser = Parser::new(grammar.clone());
    let working_result = parser.partial(working).expect("parse working");
    
    println!("\n=== Working case: {} ===", working);
    println!("Trees: {}", working_result.roots.len());
    for (i, root) in working_result.roots.iter().enumerate() {
        let status = check_tree(root, &grammar);
        println!("  Tree {}: {:?}", i, status);
        if status.is_ok() {
            println!("    (well-typed!)");
        }
    }
    
    let mut parser = Parser::new(grammar.clone());
    let failing_result = parser.partial(failing).expect("parse failing");
    
    println!("\n=== Failing case: {} ===", failing);
    println!("Trees: {}", failing_result.roots.len());
    for (i, root) in failing_result.roots.iter().enumerate() {
        let status = check_tree(root, &grammar);
        println!("  Tree {}: {:?}", i, status);
        // Print tree structure
        println!("{}", display_tree(root, 2));
    }
    
    // Check the simpler case: just f x
    let simpler = "λf:A->B.λx:A.f x";
    let mut parser = Parser::new(grammar.clone());
    let simpler_result = parser.partial(simpler).expect("parse simpler");
    
    println!("\n=== Simpler case: {} ===", simpler);
    println!("Trees: {}", simpler_result.roots.len());
    for (i, root) in simpler_result.roots.iter().enumerate() {
        let status = check_tree(root, &grammar);
        println!("  Tree {}: {:?}", i, status);
    }
    
    // Test with context already set up
    println!("\n=== Direct application test ===");
    let just_app = "f x";
    let mut parser = Parser::new(grammar.clone());
    let app_result = parser.partial(just_app).expect("parse app");
    
    let ctx = Context::new()
        .extend("f".to_string(), Type::parse("A->B").unwrap())
        .extend("x".to_string(), Type::parse("A").unwrap());
    
    println!("Context: f:A->B, x:A");
    println!("Input: {}", just_app);
    for (i, root) in app_result.roots.iter().enumerate() {
        let status = check_tree_with_context(root, &grammar, &ctx);
        println!("  Tree {}: {:?}", i, status);
    }
    
    // Now test double application  
    println!("\n=== Double application test ===");
    let double_app = "f x y";
    let mut parser = Parser::new(grammar.clone());
    let double_result = parser.partial(double_app).expect("parse double");
    
    let ctx2 = Context::new()
        .extend("f".to_string(), Type::parse("A->B->C").unwrap())
        .extend("x".to_string(), Type::parse("A").unwrap())
        .extend("y".to_string(), Type::parse("B").unwrap());
    
    println!("Context: f:A->B->C, x:A, y:B");
    println!("Input: {}", double_app);
    for (i, root) in double_result.roots.iter().enumerate() {
        let status = check_tree_with_context(root, &grammar, &ctx2);
        println!("  Tree {}: {:?}", i, status);
        println!("{}", display_tree(root, 2));
    }
    
    // Also test the parens version
    println!("\n=== Parenthesized double application test ===");
    let parens_app = "(f x) y";
    let mut parser = Parser::new(grammar.clone());
    let parens_result = parser.partial(parens_app).expect("parse parens");
    
    println!("Context: f:A->B->C, x:A, y:B");
    println!("Input: {}", parens_app);
    for (i, root) in parens_result.roots.iter().enumerate() {
        let status = check_tree_with_context(root, &grammar, &ctx2);
        println!("  Tree {}: {:?}", i, status);
        println!("{}", display_tree(root, 2));
    }
    
    // Test the deeply nested case - this was failing
    println!("\n=== Deeply nested app test (WRONG TYPE) ===");
    let nested_wrong = "f (f (f x))";
    let mut parser = Parser::new(grammar.clone());
    let nested_result = parser.partial(nested_wrong).expect("parse nested");
    
    // With f : A -> A -> A -> A and x : A, f (f x) is a type error!
    // (f x) : A -> A -> A, but f expects A as first arg
    let ctx_wrong = Context::new()
        .extend("f".to_string(), Type::parse("A->A->A->A").unwrap())
        .extend("x".to_string(), Type::parse("A").unwrap());
    
    println!("Context: f:A->A->A->A, x:A");
    println!("Input: {} (SHOULD FAIL - type error)", nested_wrong);
    let has_valid = nested_result.roots.iter().any(|r| check_tree_with_context(r, &grammar, &ctx_wrong).is_ok());
    println!("Has valid tree: {}", has_valid);
    
    // Now test with CORRECT types: f : A -> A
    println!("\n=== Deeply nested app test (CORRECT TYPE) ===");
    let ctx_correct = Context::new()
        .extend("f".to_string(), Type::parse("A->A").unwrap())
        .extend("x".to_string(), Type::parse("A").unwrap());
    
    println!("Context: f:A->A, x:A");
    println!("Input: {}", nested_wrong);
    let has_valid_correct = nested_result.roots.iter().any(|r| check_tree_with_context(r, &grammar, &ctx_correct).is_ok());
    println!("Has valid tree: {}", has_valid_correct);
}

