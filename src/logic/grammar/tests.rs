// These tests enforce the β(b, p) grammar-path invariants
// We are checking if the spec is respected
// For example `stlc_abs_binding_paths_match_spec` asserts β(τ₁, abs) = 3@1·0.
#![cfg(test)]

use super::*;
use crate::regex::Regex as DerivativeRegex;

fn literal_regex(pattern: &str) -> DerivativeRegex {
    DerivativeRegex::literal(pattern)
}

#[test]
fn literal_tokens_become_regex_symbols() {
    let spec = "A ::= 'foo'";
    let grammar = Grammar::load(spec).expect("load literal grammar");
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
    let grammar = Grammar::load(spec).unwrap();
    let productions = grammar.productions.get("start").unwrap();
    match &productions[0].rhs[0] {
        Symbol::Terminal { regex, .. } => {
            assert!(regex.equiv(&DerivativeRegex::new("[a-z]+").unwrap()));
        }
        other => panic!("expected regex symbol, got {:?}", other),
    }
    let spec2 = grammar.to_spec_string();
    let reparsed = Grammar::load(&spec2).unwrap();
    assert_eq!(grammar, reparsed);
}

#[test]
fn expression_bindings_are_preserved() {
    let spec = "start ::= Expr[val]\nExpr ::= /[0-9]+/";
    let grammar = Grammar::load(spec).unwrap();
    let start_prod = grammar.productions.get("start").unwrap();
    match &start_prod[0].rhs[0] {
        Symbol::Nonterminal { name, binding } => {
            assert_eq!(name, "Expr");
            assert_eq!(binding.as_deref(), Some("val"));
        }
        other => panic!("expected expression symbol, got {:?}", other),
    }
}

#[test]
fn grammar_tracks_special_tokens_for_literals() {
    let spec = "start ::= 'let' Identifier\nIdentifier ::= /[a-z]+/";
    let grammar = Grammar::load(spec).unwrap();
    assert!(grammar.special_tokens.iter().any(|tok| tok == "let"));
}

fn steps(path: &binding::GrammarPath) -> Vec<(usize, Option<usize>)> {
    path.steps().iter().map(|s| (s.i, s.a)).collect()
}

#[test]
fn stlc_abs_binding_paths_match_spec() {
    let spec = include_str!("../../../examples/stlc.spec");
    let grammar = Grammar::load(spec).expect("load stlc");

    let assert_path = |binding: &str, rule: &str, expected: Vec<Vec<(usize, Option<usize>)>>| {
        let paths = grammar
            .binding_map
            .get(binding, rule)
            .unwrap_or_else(|| panic!("missing paths for {}:{}", binding, rule));
        assert_eq!(
            paths.len(),
            expected.len(),
            "path count mismatch for {}:{}",
            binding,
            rule
        );
        for (path, expected_steps) in paths.iter().zip(expected.iter()) {
            assert_eq!(
                steps(path),
                *expected_steps,
                "unexpected path for {}:{}",
                binding,
                rule
            );
        }
    };

    assert_path("a", "lambda", vec![vec![(1, None)]]);
    assert_path("e", "lambda", vec![vec![(5, None)]]);
    assert_path("τ", "lambda", vec![vec![(3, None)]]);

    // Application rule binding paths (from examples/stlc.spec)
    // Application(app) ::= AtomicExpression[l] AtomicExpression[r]
    assert_path("l", "app", vec![vec![(0, None)]]);
    assert_path("r", "app", vec![vec![(1, None)]]);
}
