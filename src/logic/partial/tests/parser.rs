//! Parser Unit Tests
//!
//! Basic functionality tests for the partial parser implementation.
//! These tests verify core parsing behavior including:
//! - Literal matching
//! - Alternative handling
//! - Partial parse states
//! - Binding preservation
//! - Special token handling

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::structure::Node;
use crate::set_debug_level;

#[test]
fn test_simple_literal() {
    let spec = r#"
    start ::= 'hello'
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.partial("hello").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_partial_literal() {
    let spec = r#"
    start ::= 'hello'
    "#;
    set_debug_level(crate::DebugLevel::Debug);
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.partial("hel").unwrap();
    assert!(!ast.is_complete());
}

#[test]
fn test_alternatives() {
    let spec = r#"
    A ::= 'a'
    B ::= 'b'
    start ::= A | B
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.partial("a").unwrap();
    assert!(ast.is_complete());
    assert_eq!(ast.roots.len(), 1);
    assert_eq!(ast.roots[0].name, "start");

    // Verify child structure
    let child = &ast.roots[0].children[0];
    if let Node::NonTerminal(nt) = child {
        assert_eq!(nt.name, "A");
    } else {
        panic!("Expected NonTerminal A");
    }
}

#[test]
fn test_partial_alternatives() {
    let spec = r#"
    A ::= 'a'
    B ::= 'a' 'b'
    start ::= A | B
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.partial("a").unwrap();
    // A: complete (matched 'a')
    // B: partial (matched 'a', missing 'b')
    // Both should be present as roots
    assert_eq!(ast.roots.len(), 2);
}

#[test]
fn test_partial_at_end() {
    let spec = r#"
    start ::= 'hello' 'world'
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.partial("hello wor").unwrap();
    assert!(!ast.is_complete());
}

#[test]
fn test_mismatch_rejection() {
    let spec = r#"
    start ::= 'hello'
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let _ast = p.partial("goodbye").unwrap_err();
}

#[test]
fn test_complex_grammar() {
    let spec = r#"
    Number ::= /[0-9]+/
    Op ::= '+' | '-'
    Expr ::= Number | Number Op Expr
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.partial("1 + 2 - 3").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn test_binding_preservation() {
    let spec = r#"
    Number ::= /[0-9]+/
    start ::= Number[x]
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    let ast = p.partial("42").unwrap();
    assert!(ast.is_complete());

    let root = &ast.roots[0];
    let child = &root.children[0];
    if let Node::NonTerminal(nt) = child {
        assert_eq!(nt.binding, Some("x".to_string()));
    } else {
        panic!("Expected NonTerminal node");
    }
}

#[test]
fn test_partial_special_token_arrow() {
    // Test parsing partial input where "-" is the start of "->" special token
    // This tests the fix for tokenization of partial special tokens at end of input
    let spec = r#"
    Identifier ::= /[A-Za-z]+/
    BaseType ::= Identifier
    Type ::= BaseType '->' Type | BaseType
    start ::= Type
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    // "A-" should parse as partial, where "-" is a prefix of "->"
    let result = p.partial("A-");
    assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    let ast = result.unwrap();
    assert!(!ast.is_complete(), "AST should be partial (incomplete)");
}

#[test]
fn test_partial_lambda_arrow() {
    // Test a more realistic lambda calculus scenario
    let spec = r#"
    Identifier ::= /[A-Za-z]+/
    Variable ::= Identifier
    BaseType ::= Identifier | '(' Type ')'
    Type ::= BaseType '->' Type | BaseType
    Lambda ::= 'λ' Variable ':' Type '.' Expr
    Expr ::= Variable | Lambda | '(' Expr ')'
    start ::= Expr
    "#;
    let g = Grammar::load(spec).unwrap();
    let mut p = Parser::new(g);

    // "λf:(A-" should parse as partial
    // The "-" at the end is a prefix of the "->" special token
    let result = p.partial("λf:(A-");
    assert!(result.is_ok(), "Expected Ok, got: {:?}", result);
    let ast = result.unwrap();
    assert!(!ast.is_complete(), "AST should be partial (incomplete)");
}
