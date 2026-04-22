// These tests enforce the β(b, p) grammar-path invariants
// We are checking if the spec is respected
// For example `stlc_abs_binding_paths_match_spec` asserts β(τ₁, abs) = 3@1·0.
use super::*;
use crate::logic::typing::Type;
use crate::regex::Regex;

fn literal_regex(pattern: &str) -> Regex {
    Regex::literal(pattern)
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
            assert!(regex.equiv(&Regex::new("[a-z]+").unwrap()));
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
    let grammar = Grammar::load(spec).unwrap();
    assert!(grammar.specials().iter().any(|tok| tok == "let"));
}

fn steps(path: &binding::GrammarPath) -> Vec<(usize, usize)> {
    path.steps().iter().map(|s| (s.i, s.a)).collect()
}

#[test]
fn stlc_abs_binding_paths_match_spec() {
    let spec = include_str!("../../../examples/stlc.auf");
    let grammar = Grammar::load(spec).expect("load stlc");

    let assert_path = |binding: &str, rule: &str, expected: Vec<Vec<(usize, usize)>>| {
        let paths = grammar
            .bindings
            .as_ref()
            .unwrap()
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

    assert_path("a", "lambda", vec![vec![(1, 0)]]);
    assert_path("e", "lambda", vec![vec![(5, 0)]]);
    assert_path("τ", "lambda", vec![vec![(3, 0)]]);

    // Application rule binding paths (from examples/stlc.auf)
    // Application(app) ::= AtomicExpression[l] AtomicExpression[r]
    assert_path("l", "app", vec![vec![(0, 0)]]);
    assert_path("r", "app", vec![vec![(1, 0)]]);
}

#[test]
fn repeated_binding_produces_multiple_paths() {
    let spec = r#"
    Number(num) ::= /[0-9]+/
    Pair(pair) ::= Number[x] ',' Number[x]

    Γ ⊢ x : 'number'
    ----------------- (pair)
    'number'
    "#;

    let grammar = Grammar::load(spec).expect("load pair grammar");
    let paths = grammar
        .bindings
        .as_ref()
        .unwrap()
        .get("x", "pair")
        .expect("binding paths for repeated x");

    assert_eq!(paths.len(), 2, "repeated binding should keep both paths");
    assert_eq!(steps(&paths[0]), vec![(0, 0)]);
    assert_eq!(steps(&paths[1]), vec![(2, 0)]);
}

#[test]
fn repetition_helpers_preserve_inner_binding_paths() {
    let spec = r#"
    Item(item) ::= Number[x]
    Seq(seq) ::= Item*

    Γ ⊢ x : 'number'
    ----------------- (item)
    'number'
    "#;

    let grammar = Grammar::load(spec).expect("load repetition grammar");
    let paths = grammar
        .bindings
        .as_ref()
        .unwrap()
        .get("x", "item")
        .expect("binding paths for repeated item");

    assert_eq!(paths.len(), 1);
    assert_eq!(steps(&paths[0]), vec![(0, 0)]);
}

#[test]
fn extend_input_preserves_float_operator_spacing() {
    let mut grammar = Grammar::load(include_str!("../../../examples/fun.auf")).expect("load fun");
    assert_eq!(grammar.extend_input("1.0 +.", "0.0"), "1.0 +. 0.0");
}

#[test]
fn extend_input_preserves_call_argument_spacing() {
    let mut grammar =
        Grammar::load(include_str!("../../../examples/typescript.auf")).expect("load typescript");
    assert_eq!(
        grammar.extend_input(
            "function mix ( a : number , b : number [ ] , c : boolean ) : void { return ; } mix ( 1 , [ 2 , 3 ] ,",
            "true"
        ),
        "function mix ( a : number , b : number [ ] , c : boolean ) : void { return ; } mix ( 1 , [ 2 , 3 ] , true"
    );
}

#[test]
fn fill_injects_synthetic_type_rules_for_unary_nonterminal_wrappers() {
    let grammar = Grammar::load(
        "A ::= B\nA ::= C\nB ::= /[0-9]+/\nC ::= /[a-z]+/\n",
    )
    .expect("load unary wrapper grammar");

    let productions = grammar.productions.get("A").expect("A productions");
    assert_eq!(productions.len(), 2);

    let mut seen_bindings = std::collections::HashSet::new();

    let nt_idx = grammar.nt_index("A").expect("A exists");
    for (alt_idx, prod) in productions.iter().enumerate() {
        let rule_name = grammar
            .rule_for_prod((nt_idx, alt_idx))
            .expect("synthetic rule attached");
        let rule = grammar.rules().get(rule_name).expect("synthetic rule exists");
        let child_binding = prod.rhs[0].binding().unwrap();
        assert!(seen_bindings.insert(child_binding.clone()), "synthetic bindings must be unique for A");

        match &rule.conclusion.kind {
            crate::logic::typing::rule::ConclusionKind::Type(Type::Meta(name)) => {
                assert_eq!(name, child_binding);
            }
            other => panic!("unexpected conclusion kind: {:?}", other),
        }

        let premise = rule
            .premises
            .iter()
            .find(|p| p.judgment.is_some())
            .expect("rule has a premise");
        match &premise.judgment {
            Some(crate::logic::typing::rule::TypingJudgment::Ascription((term, ty))) => {
                assert_eq!(term, child_binding);
                assert_eq!(ty, &Type::Meta(child_binding.clone()));
            }
            _ => panic!("unexpected premise type"),
        }
    }
}

#[test]
fn fill_injects_synthetic_type_rules_for_individual_unary_wrapper_productions() {
    let grammar = Grammar::load(
        "A ::= B\nA ::= 'x'\nB ::= /[0-9]+/\n",
    )
    .expect("load mixed unary wrapper grammar");

    let productions = grammar.productions.get("A").expect("A productions");
    assert_eq!(productions.len(), 2);
    let nt_idx = grammar.nt_index("A").expect("A exists");
    assert!(grammar.rule_for_prod((nt_idx, 0)).is_some(), "wrapper production should get a synthetic rule");
    assert!(grammar.rule_for_prod((nt_idx, 1)).is_none(), "terminal production should not get a synthetic rule");
}
