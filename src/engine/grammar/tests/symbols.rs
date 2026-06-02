use crate::engine::grammar::{SPG, Symbol};
use crate::regex::Regex;

fn literal_regex(pattern: &str) -> Regex {
    Regex::literal(pattern)
}

#[test]
fn literal_tokens_become_regex_symbols() {
    let spec = "A ::= 'foo'";
    let grammar = SPG::load(spec).expect("load literal grammar");
    let productions = grammar.productions.get("A").expect("production A");
    let symbols = &productions[0].rhs;
    assert_eq!(symbols.len(), 1);
    match &symbols[0] {
        Symbol::Terminal { regex, binding } => {
            assert!(regex.equiv(&literal_regex("foo")));
            assert!(binding.is_none());
        }
        other => panic!("expected regex symbol for literal, got {:?}", other),
    }
}

#[test]
fn regex_literals_round_trip() {
    let spec = "start ::= /[a-z]+/";
    let grammar = SPG::load(spec).unwrap();
    let productions = grammar.productions.get("start").unwrap();
    match &productions[0].rhs[0] {
        Symbol::Terminal { regex, .. } => {
            assert!(regex.equiv(&Regex::new("[a-z]+").unwrap()));
        }
        other => panic!("expected regex symbol, got {:?}", other),
    }
    let spec2 = grammar.to_spec_string();
    let reparsed = SPG::load(&spec2).unwrap();
    assert_eq!(grammar, reparsed);
}

#[test]
fn expression_bindings_are_preserved() {
    let spec = "start ::= Expr[val]\nExpr ::= /[0-9]+/";
    let grammar = SPG::load(spec).unwrap();
    let start_prod = grammar.productions.get("start").unwrap();
    match &start_prod[0].rhs[0] {
        Symbol::Nonterminal { name, binding, .. } => {
            assert_eq!(name, "Expr");
            assert_eq!(binding.as_deref(), Some("val"));
        }
        other => panic!("expected expression symbol, got {:?}", other),
    }
}

#[test]
fn grammar_tracks_special_tokens_for_literals() {
    let spec = "start ::= 'let' Identifier\nIdentifier ::= /[a-z]+/";
    let grammar = SPG::load(spec).unwrap();
    assert!(grammar.specials().unwrap().iter().any(|tok| tok == "let"));
}
