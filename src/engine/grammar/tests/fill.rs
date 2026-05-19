use crate::domains::typing::{TypeExpr, TypingDomain};
use crate::engine::grammar::SPG;
use proptest::prelude::*;

#[test]
fn fill_injects_synthetic_type_rules_for_unary_nonterminal_wrappers() {
    let grammar = SPG::<TypingDomain>::load("A ::= B\nA ::= C\nB ::= /[0-9]+/\nC ::= /[a-z]+/\n")
        .expect("load unary wrapper grammar");

    let productions = grammar.productions.get("A").expect("A productions");
    assert_eq!(productions.len(), 2);

    let nt_idx = grammar.nt_index("A").expect("A exists");
    assert!(grammar.nt_rule("A").is_some());
    let rule_name = grammar
        .nt(nt_idx)
        .and_then(|nt| grammar.nt_rule(nt))
        .expect("synthetic rule attached")
        .clone();
    let rule = grammar
        .rules()
        .get(&rule_name)
        .expect("synthetic rule exists");
    let child_binding = productions[0].rhs[0].binding().unwrap().clone();

    for prod in productions.iter() {
        let alt_rule_name = grammar
            .nt(nt_idx)
            .and_then(|nt| grammar.nt_rule(nt))
            .expect("synthetic rule attached");
        assert_eq!(alt_rule_name, &rule_name);
        assert_eq!(prod.rhs[0].binding(), Some(&child_binding));

        match &rule.conclusion.kind {
            TypeExpr::TypeOf(name) => {
                assert_eq!(name, &child_binding);
            }
            other => panic!("unexpected conclusion kind: {:?}", other),
        }

        let premise = rule
            .premises
            .iter()
            .find(|p| p.judgment.is_some())
            .expect("rule has a premise");
        match &premise.judgment {
            Some(crate::domains::typing::rule::TypingJudgment::Ascription((term, ty))) => {
                assert_eq!(term, &child_binding);
                assert_eq!(ty, &TypeExpr::Meta(child_binding.clone()));
            }
            _ => panic!("unexpected premise type"),
        }
    }
}

#[test]
fn fill_injects_synthetic_type_rules_for_delimited_unary_wrappers() {
    let grammar = SPG::<TypingDomain>::load("A ::= '(' B ')'\nB ::= /[0-9]+/\n")
        .expect("load delimited wrapper grammar");

    let productions = grammar.productions.get("A").expect("A productions");
    assert_eq!(productions.len(), 1);

    let nt_idx = grammar.nt_index("A").expect("A exists");
    assert!(grammar.nt_rule("A").is_some());
    assert!(grammar.is_transparent_nt("A"));
    let rule_name = grammar
        .nt(nt_idx)
        .and_then(|nt| grammar.nt_rule(nt))
        .expect("synthetic rule attached")
        .clone();
    let rule = grammar
        .rules()
        .get(&rule_name)
        .expect("synthetic rule exists");
    let child_binding = productions[0].rhs[1].binding().unwrap().clone();

    match &rule.conclusion.kind {
        TypeExpr::TypeOf(name) => {
            assert_eq!(name, &child_binding);
        }
        other => panic!("unexpected conclusion kind: {:?}", other),
    }

    let premise = rule
        .premises
        .iter()
        .find(|p| p.judgment.is_some())
        .expect("rule has a premise");
    match &premise.judgment {
        Some(crate::domains::typing::rule::TypingJudgment::Ascription((term, ty))) => {
            assert_eq!(term, &child_binding);
            assert_eq!(ty, &TypeExpr::Meta(child_binding));
        }
        _ => panic!("unexpected premise type"),
    }
}

#[test]
fn fill_preserves_existing_transparent_child_binding() {
    let grammar = SPG::<TypingDomain>::load("A ::= '(' B[inner] ')'\nB ::= /[0-9]+/\n")
        .expect("load bound delimited wrapper grammar");

    let productions = grammar.productions.get("A").expect("A productions");
    let nt_idx = grammar.nt_index("A").expect("A exists");
    assert!(grammar.nt_rule("A").is_some());
    assert_eq!(
        productions[0].rhs[1].binding().map(String::as_str),
        Some("inner")
    );

    let rule_name = grammar
        .nt(nt_idx)
        .and_then(|nt| grammar.nt_rule(nt))
        .expect("synthetic rule attached");
    let rule = grammar
        .rules()
        .get(rule_name)
        .expect("synthetic rule exists");
    match &rule.conclusion.kind {
        TypeExpr::TypeOf(name) => {
            assert_eq!(name, "inner");
        }
        other => panic!("unexpected conclusion kind: {:?}", other),
    }
}

#[test]
fn fill_injects_synthetic_type_rules_for_individual_unary_wrapper_productions() {
    let grammar = SPG::<TypingDomain>::load("A ::= B\nA ::= 'x'\nB ::= /[0-9]+/\n")
        .expect("load mixed unary wrapper grammar");

    let productions = grammar.productions.get("A").expect("A productions");
    assert_eq!(productions.len(), 2);
    let nt_idx = grammar.nt_index("A").expect("A exists");
    assert!(
        grammar
            .nt(nt_idx)
            .and_then(|nt| grammar.nt_rule(nt))
            .is_none(),
        "mixed nonterminal should not get a synthetic rule"
    );
    assert!(
        grammar
            .nt(nt_idx)
            .and_then(|nt| grammar.nt_rule(nt))
            .is_none(),
        "mixed nonterminal should not get a synthetic rule"
    );
}

fn unary_wrapper_spec(children: &[String]) -> String {
    let mut spec = String::new();
    for child in children {
        spec.push_str(&format!("Root ::= {}\n", child));
    }
    for child in children {
        spec.push_str(&format!("{} ::= /[a-z]+/\n", child));
    }
    spec
}

proptest! {
    #[test]
    fn prop_fill_marks_pure_unary_wrapper_nonterminals(child_count in 1usize..5) {
        let children: Vec<String> = (0..child_count).map(|i| format!("Child{}", i)).collect();
        let spec = unary_wrapper_spec(&children);
        let grammar = SPG::<TypingDomain>::load(&spec).unwrap();

        prop_assert!(grammar.nt_rule("Root").is_some());

        let productions = grammar.productions.get("Root").unwrap();
        let nt_idx = grammar.nt_index("Root").unwrap();
        let rule_name = grammar.nt(nt_idx).and_then(|nt| grammar.nt_rule(nt)).unwrap().clone();
        let rule = grammar.rules().get(&rule_name).unwrap();
        let inherited_binding = productions[0].rhs[0].binding().unwrap().clone();

        for (alt_idx, prod) in productions.iter().enumerate() {
            prop_assert_eq!(grammar.nt(nt_idx).and_then(|nt| grammar.nt_rule(nt)), Some(&rule_name));
            prop_assert_eq!(prod.rhs[0].binding(), Some(&inherited_binding));
        }

        match &rule.conclusion.kind {
            TypeExpr::TypeOf(name) => {
                prop_assert_eq!(name, &inherited_binding);
            }
            other => prop_assert!(false, "unexpected conclusion: {:?}", other),
        }
    }

    #[test]
    fn prop_fill_does_not_mark_mixed_nonterminals(lit in "[a-z]{1,4}") {
        let spec = format!("Root ::= Child\nRoot ::= '{}'\nChild ::= /[a-z]+/\n", lit);
        let grammar = SPG::<TypingDomain>::load(&spec).unwrap();
        let nt_idx = grammar.nt_index("Root").unwrap();

        prop_assert!(grammar.nt_rule("Root").is_none());
        prop_assert_eq!(grammar.nt(nt_idx).and_then(|nt| grammar.nt_rule(nt)), None);
        prop_assert_eq!(grammar.nt(nt_idx).and_then(|nt| grammar.nt_rule(nt)), None);
    }
}

#[test]
fn print_filled_grammar_for_inspection() {
    let grammar =
        SPG::<TypingDomain>::load(include_str!("../../../../examples/stlc.auf")).expect("load stlc");
    println!("\n=== Filled Grammar ===\n{}", grammar.to_spec_string());
    assert!(!grammar.to_spec_string().is_empty());
}
