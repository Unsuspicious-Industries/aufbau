// Type contradiction tests

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::set_debug_level;

fn load_grammar() -> Grammar {
    let spec = include_str!("../../../../examples/stlc.auf");
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
    assert!(
        ast.has_well_typed(&grammar),
        "Identity function should type-check"
    );
}

#[test]
fn test_simple_lambda() {
    let grammar = load_grammar();
    let mut parser = Parser::new(grammar.clone());

    let input = "λx:A.x";
    println!("\n=== Simple Lambda Test ===");
    println!("Input: {}", input);

    let ast = parser.partial(input).expect("Failed to parse");
    assert!(
        ast.has_well_typed(&grammar),
        "Simple lambda should type-check"
    );
}

#[test]
fn test_unbound_variable() {
    // (λx:X.i) should fail - 'i' is unbound
    let grammar = load_grammar();
    let mut parser = Parser::new(grammar.clone());

    let input = "(λx:X.i)";
    println!("\n=== Unbound variable test ===");
    println!("Input: {}", input);

    set_debug_level(crate::DebugLevel::Trace);
    let ast = parser.partial(input).expect("Failed to parse");
    assert!(
        !ast.has_well_typed(&grammar),
        "Unbound variable 'i' should cause type error"
    );
}
