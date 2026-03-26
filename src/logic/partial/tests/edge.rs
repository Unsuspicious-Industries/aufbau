use crate::logic::grammar::Grammar;
use crate::logic::partial::{MetaParser, Parser};

#[test]
fn hard_left_recursive_application_chain_stays_parseable() {
    let spec = r#"
    Atom ::= 'x'
    Expr ::= Expr Atom | Atom
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar);

    let ast = parser.parse("x x x x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn mutual_recursion_with_parenthesized_escape_holds() {
    let spec = r#"
    Expr ::= Expr '+' Term | Term
    Term ::= '(' Expr ')' | 'n'
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar);

    let ast = parser.parse("( n + n ) + n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn partial_operator_prefix_keeps_future_shape() {
    let spec = r#"
    Number ::= /[0-9]+/
    Expr ::= Expr '+' Number | Number
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = Parser::new(grammar);

    let ast = parser.partial("1 +").unwrap();
    assert!(!ast.roots().is_empty());
    assert!(!ast.is_complete());
}

#[test]
fn append_only_prefix_state_roundtrip_is_conservative() {
    let spec = r#"
    Atom ::= 'x'
    Expr ::= Expr Atom | Atom
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = Parser::new(grammar.clone());
    let prefix = parser.prefix("x x").unwrap();
    let advanced = parser.advance(&prefix, "x x").unwrap();

    assert!(advanced.forest().is_complete());
    assert!(!advanced.forest().roots().is_empty());
    assert_eq!(advanced.input(), "x x");
}
