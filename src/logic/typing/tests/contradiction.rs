// Type contradiction tests

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::typing::eval::check_tree;
use crate::logic::typing::core::TreeStatus;

fn load_grammar() -> Grammar {
    let spec = include_str!("../../../../examples/stlc.spec");
    Grammar::load(spec).expect("Failed to load STLC grammar")
}

#[test]
fn test_valid_app() {
    // (λx:X.x) should be valid (identity function)
    let grammar = load_grammar();
    let mut parser = Parser::new(grammar.clone());
    
    let input = "(λx:X.x)";
    println!("\n=== Valid Application Test ===");
    println!("Input: {}", input);
    
    let ast = parser.partial(input).expect("Failed to parse");
    let complete_trees: Vec<_> = ast.roots.iter()
        .filter(|r| r.is_complete())
        .collect();
    
    let valid = complete_trees.iter().any(|tree| {
        let status = check_tree(tree, &grammar);
        !matches!(status, TreeStatus::Malformed)
    });
    
    assert!(valid, "Identity function should type-check");
}

#[test]
fn test_simple_lambda() {
    let grammar = load_grammar();
    let mut parser = Parser::new(grammar.clone());
    
    let input = "λx:A.x";
    println!("\n=== Simple Lambda Test ===");
    println!("Input: {}", input);
    
    let ast = parser.partial(input).expect("Failed to parse");
    let complete_trees: Vec<_> = ast.roots.iter()
        .filter(|r| r.is_complete())
        .collect();
    
    for tree in &complete_trees {
        let status = check_tree(tree, &grammar);
        println!("Status: {:?}", status);
    }
    
    let valid = complete_trees.iter().any(|tree| {
        !matches!(check_tree(tree, &grammar), TreeStatus::Malformed)
    });
    
    assert!(valid, "Simple lambda should type-check");
}

#[test]
fn test_let_context_extension() {
    // Test that let expressions properly extend the context
    // {a:X}((λx:X.x)a) should be valid
    let grammar = load_grammar();
    let mut parser = Parser::new(grammar.clone());
    
    let input = "{a:X}((λx:X.x)a)";
    println!("\n=== Testing let context extension ===");
    println!("Input: {}", input);
    
    let ast = parser.partial(input).expect("Failed to parse");
    let complete_trees: Vec<_> = ast.roots.iter()
        .filter(|r| r.is_complete())
        .collect();
    
    println!("Complete trees: {}", complete_trees.len());
    for (i, tree) in complete_trees.iter().enumerate() {
        println!("\n--- Tree {} ---", i);
        let status = check_tree(tree, &grammar);
        println!("Type status: {:?}", status);
    }
    
    let any_valid = complete_trees.iter().any(|tree| {
        !matches!(check_tree(tree, &grammar), TreeStatus::Malformed)
    });
    
    assert!(any_valid, "Expression with let should be well-typed");
}

#[test]
fn test_unbound_variable() {
    // (λx:X.i) should fail - 'i' is unbound
    let grammar = load_grammar();
    let mut parser = Parser::new(grammar.clone());
    
    let input = "(λx:X.i)";
    println!("\n=== Unbound variable test ===");
    println!("Input: {}", input);
    
    let ast = parser.partial(input).expect("Failed to parse");
    let complete_trees: Vec<_> = ast.roots.iter()
        .filter(|r| r.is_complete())
        .collect();
    
    // All complete trees should be malformed (unbound variable)
    let all_malformed = complete_trees.iter().all(|tree| {
        matches!(check_tree(tree, &grammar), TreeStatus::Malformed)
    });
    
    assert!(all_malformed, "Unbound variable 'i' should cause type error");
}

