/*
Typing system tests
Main tests + submodules
*/

mod contradiction;
mod fun;
mod imp;
mod lc;
mod redeclaration;

use super::*;
use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;

#[test]
fn test_debug_quoted_type_parsing() {
    let test_cases = vec!["'int'", "'void'", "'string'"];

    for case in test_cases {
        println!("Testing: '{}'", case);
        println!("  Length: {}", case.len());
        println!("  Starts with ': {}", case.starts_with('\''));
        println!("  Ends with ': {}", case.ends_with('\''));
        println!("  Length > 2: {}", case.len() > 2);

        if case.starts_with('\'') && case.ends_with('\'') && case.len() > 2 {
            let raw_type = &case[1..case.len() - 1];
            println!("  Raw type after quote removal: '{}'", raw_type);
            println!(
                "  Raw type chars: {:?}",
                raw_type.chars().collect::<Vec<_>>()
            );
            println!(
                "  All chars alphanumeric or underscore: {}",
                raw_type.chars().all(|c| c.is_alphanumeric() || c == '_')
            );
        }

        match Type::parse(case) {
            Ok(ty) => println!("Parsed as: {:?}", ty),
            Err(e) => println!("Failed: {}", e),
        }
        println!();
    }
}

#[test]
fn stlc_type_parsing_and_display() {
    let int = Type::parse("Int").expect("parse Int");
    assert_eq!(format!("{}", int), "Int");

    let tau1 = Type::parse("τ₁").expect("parse τ₁");
    assert_eq!(format!("{}", tau1), "τ₁");

    let t1 = Type::parse("Int -> Int").expect("parse arrow");
    assert_eq!(format!("{}", t1), "Int → Int");

    let t2 = Type::parse("(Int -> Int) -> Int").expect("parse nested arrow");
    assert_eq!(format!("{}", t2), "Int → Int → Int");

    let t3 = Type::parse("τ₁ → τ₂").expect("parse unicode arrow");
    assert_eq!(format!("{}", t3), "τ₁ → τ₂");
}

#[test]
fn context_lookup_type_parsing_and_display() {
    let lookup = Type::parse("Γ(x)").expect("parse Γ(x)");
    match lookup {
        Type::ContextCall(ctx, var) => {
            assert_eq!(ctx, "Γ");
            assert_eq!(var, "x");
        }
        other => panic!("Expected ContextCall, got {:?}", other),
    }
    assert_eq!(format!("{}", Type::parse("Γ(x)").unwrap()), "lookup(x)");

    let lookup_keyword = Type::parse("lookup(y)").expect("parse lookup(y)");
    match &lookup_keyword {
        Type::ContextCall(ctx, var) => {
            assert_eq!(ctx, "lookup");
            assert_eq!(var, "y");
        }
        other => panic!("Expected ContextCall, got {:?}", other),
    }
    assert_eq!(format!("{}", lookup_keyword), "lookup(y)");
}

#[test]
fn stlc_lambda_rule_parse_and_inspect() {
    let premises = "Γ[x:τ₁] ⊢ e : τ₂".to_string();
    let conclusion = "τ₁ -> τ₂".to_string();
    let rule =
        TypingRule::new(premises, conclusion, "lambda".to_string()).expect("parse lambda rule");

    assert_eq!(rule.name, "lambda");
    assert_eq!(rule.premises.len(), 1);
    match &rule.premises[0] {
        Premise {
            setting,
            judgment: Some(TypingJudgment::Ascription((term, ty))),
        } => {
            let setting = setting.as_ref().expect("has setting");
            assert_eq!(setting.name, "Γ");
            assert_eq!(setting.extensions.len(), 1);
            assert_eq!(setting.extensions[0].0, "x");
            assert_eq!(format!("{}", setting.extensions[0].1), "τ₁");
            assert_eq!(term, "e");
            assert_eq!(format!("{}", ty), "τ₂");
        }
        _ => panic!("Expected ascription judgment for lambda rule"),
    }

    let disp = format!("{}", rule);
    assert!(disp.contains("[lambda]"));
    assert!(disp.contains("Γ[x:τ₁] ⊢ e : τ₂"));
    let expected_conclusion = format!("{}", Type::parse("τ₁ -> τ₂").unwrap());
    assert!(disp.contains(&format!("⇒ {}", expected_conclusion)));

    let pretty = rule.pretty(0);
    assert!(pretty.contains("Γ[x:τ₁] ⊢ e : τ₂"));
    assert!(pretty.contains("[lambda]"));
    assert!(pretty.split('\n').count() >= 3);
}

#[test]
fn stlc_app_rule_parse_and_inspect() {
    let premises = "Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁".to_string();
    let conclusion = "τ₂".to_string();
    let rule = TypingRule::new(premises, conclusion, "app".to_string()).expect("parse app rule");

    assert_eq!(rule.name, "app");
    assert_eq!(rule.premises.len(), 2);

    match &rule.premises[0] {
        Premise {
            setting,
            judgment: Some(TypingJudgment::Ascription((term, ty))),
        } => {
            assert!(setting.is_some());
            assert_eq!(setting.as_ref().unwrap().name, "Γ");
            assert!(setting.as_ref().unwrap().extensions.is_empty());
            assert_eq!(term, "f");
            assert_eq!(format!("{}", ty), "τ₁ → τ₂");
        }
        _ => panic!("Expected ascription judgment for app rule premise 0"),
    }
    match &rule.premises[1] {
        Premise {
            setting,
            judgment: Some(TypingJudgment::Ascription((term, ty))),
        } => {
            assert!(setting.is_some());
            assert_eq!(setting.as_ref().unwrap().name, "Γ");
            assert!(setting.as_ref().unwrap().extensions.is_empty());
            assert_eq!(term, "e");
            assert_eq!(format!("{}", ty), "τ₁");
        }
        _ => panic!("Expected ascription judgment for app rule premise 1"),
    }

    let disp = format!("{}", rule);
    assert!(disp.contains("[app]"));
    assert!(disp.contains("Γ ⊢ f : τ₁ → τ₂"));
    assert!(disp.contains("Γ ⊢ e : τ₁"));
    assert!(disp.contains("⇒ τ₂"));

    let pretty = rule.pretty(1);
    assert!(pretty.contains("Γ ⊢ f : τ₁ → τ₂"));
    assert!(pretty.contains("Γ ⊢ e : τ₁"));
    assert!(pretty.contains("[app]"));
}

#[test]
fn evaluate_typing_frontier_aware() {
    // New semantics: partial trees with bindings at frontier are VALID
    // Only trees with bindings missing in COMPLETE portions are malformed

    let spec = r#"
    start(rule) ::= 'a'[first] 'b'[second]
    "#;
    let grammar = Grammar::load(spec).expect("load simple binding grammar");

    // Partial parse: 'a' - binding 'second' is at frontier (ghost binding)
    // This is VALID under new semantics (partial but well-formed)
    let mut parser = Parser::new(grammar.clone());
    let partial = parser.partial("a").expect("partial parse succeeds");
    assert!(
        evaluate_typing(partial.roots(), &grammar),
        "partial tree with frontier bindings should be valid"
    );

    // Complete parse: 'a b' - all bindings resolve
    let mut parser = Parser::new(grammar.clone());
    let complete = parser.partial("a b").expect("complete parse succeeds");
    assert!(
        evaluate_typing(complete.roots(), &grammar),
        "complete tree with all bindings should be valid"
    );
}

#[test]
fn evaluate_typing_rejects_malformed() {
    // A tree is MALFORMED only if bindings fail in the complete portion
    // This happens when the parser chose a wrong alternative

    let spec = r#"
    A(ruleA) ::= 'x'[a] 'y'[b]
    B(ruleB) ::= 'x'[c]
    start ::= A | B
    "#;
    let grammar = Grammar::load(spec).expect("load grammar");

    // Input 'x' matches both A (partial) and B (complete)
    // Both should be considered valid
    let mut parser = Parser::new(grammar.clone());
    let result = parser.partial("x").expect("parse succeeds");
    assert!(
        evaluate_typing(result.roots(), &grammar),
        "should have at least one valid tree"
    );
}
