use super::*;

#[test]
fn test_debug_quoted_type_parsing() {
    // Debug the specific issue with quoted types
    use crate::logic::typing::syntax::TypeSyntaxConfig;

    let test_cases = vec!["'int'", "'void'", "'string'"];

    let cfg = TypeSyntaxConfig::default();
    println!("Config union operators: {:?}", cfg.union);
    println!("Config intersection operators: {:?}", cfg.intersection);
    println!("Config negation operators: {:?}", cfg.negation);
    println!("Config pointer operators: {:?}", cfg.pointer);
    println!();

    for case in test_cases {
        println!("Testing: '{}'", case);
        println!("  Length: {}", case.len());
        println!("  Starts with ': {}", case.starts_with('\''));
        println!("  Ends with ': {}", case.ends_with('\''));
        println!("  Length > 2: {}", case.len() > 2);

        // Check if any config operators might interfere
        for op in &cfg.union {
            if case.contains(op) {
                println!("union operator: '{}'", op);
            }
        }
        for op in &cfg.intersection {
            if case.contains(op) {
                println!("intersection operator: '{}'", op);
            }
        }
        for op in &cfg.negation {
            if case.contains(op) {
                println!("negation operator: '{}'", op);
            }
        }
        for op in &cfg.pointer {
            if case.contains(op) {
                println!("pointer operator: '{}'", op);
            }
        }

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
    // Base type
    let int = Type::parse("Int").expect("parse Int");
    assert_eq!(format!("{}", int), "Int");

    // Type variable with subscript
    let tau1 = Type::parse("τ₁").expect("parse τ₁");
    assert_eq!(format!("{}", tau1), "τ₁");

    // Arrow types with ascii and unicode arrows
    let t1 = Type::parse("Int -> Int").expect("parse arrow");
    assert_eq!(format!("{}", t1), "Int → Int");

    let t2 = Type::parse("(Int -> Int) -> Int").expect("parse nested arrow");
    assert_eq!(format!("{}", t2), "Int → Int → Int");

    let t3 = Type::parse("τ₁ → τ₂").expect("parse unicode arrow");
    assert_eq!(format!("{}", t3), "τ₁ → τ₂");
}

#[test]
fn stlc_lambda_rule_parse_and_inspect() {
    // Γ[x:τ₁] ⊢ e : τ₂  ───(lambda)───  τ₁ → τ₂
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

    // Display forms
    let disp = format!("{}", rule);
    assert!(disp.contains("[lambda]"));
    assert!(disp.contains("Γ[x:τ₁] ⊢ e : τ₂"));
    let expected_conclusion = format!("{}", Type::parse("τ₁ -> τ₂").unwrap());
    assert!(disp.contains(&format!("⇒ {}", expected_conclusion)));

    // Pretty form (single premise)
    let pretty = rule.pretty(0);
    // For single premise pretty, it prints the premise(s), a bar with [name], then the conclusion
    assert!(pretty.contains("Γ[x:τ₁] ⊢ e : τ₂"));
    assert!(pretty.contains("[lambda]"));
    assert!(pretty.split('\n').count() >= 3);
}

#[test]
fn stlc_app_rule_parse_and_inspect() {
    // Γ ⊢ f : τ₁ → τ₂, Γ ⊢ e : τ₁  ───(app)───  τ₂
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
            // Setting name Γ with empty extensions is accepted
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

    // Display
    let disp = format!("{}", rule);
    assert!(disp.contains("[app]"));
    assert!(disp.contains("Γ ⊢ f : τ₁ → τ₂"));
    assert!(disp.contains("Γ ⊢ e : τ₁"));
    assert!(disp.contains("⇒ τ₂"));

    // Pretty formatting
    let pretty = rule.pretty(1);
    assert!(pretty.contains("Γ ⊢ f : τ₁ → τ₂"));
    assert!(pretty.contains("Γ ⊢ e : τ₁"));
    assert!(pretty.contains("[app]"));
}

#[test]
fn test_tuple_meta_types() {
    // Test tuple types as meta types with (<id>...) syntax
    let tuple1 = Type::parse("(A...)").expect("parse (A...)");
    assert_eq!(tuple1, Type::Tuple("A".to_string()));
    assert_eq!(format!("{}", tuple1), "(A...)");

    let tuple2 = Type::parse("(tuple_id...)").expect("parse (tuple_id...)");
    assert_eq!(tuple2, Type::Tuple("tuple_id".to_string()));
    assert_eq!(format!("{}", tuple2), "(tuple_id...)");

    let tuple3 = Type::parse("(τ₁...)").expect("parse (τ₁...)");
    assert_eq!(tuple3, Type::Tuple("τ₁".to_string()));
    assert_eq!(format!("{}", tuple3), "(τ₁...)");

    // Test that regular parenthesized expressions work correctly
    let arrow_in_parens = Type::parse("(Int -> Bool)").expect("parse (Int -> Bool)");
    assert_eq!(
        arrow_in_parens,
        Type::Arrow(
            Box::new(Type::Atom("Int".to_string())),
            Box::new(Type::Atom("Bool".to_string()))
        )
    );
    assert_eq!(format!("{}", arrow_in_parens), "Int → Bool");

    // Test that simple atoms in parentheses are not confused with tuples
    let atom_in_parens = Type::parse("(Int)").expect("parse (Int)");
    assert_eq!(atom_in_parens, Type::Atom("Int".to_string()));
    assert_eq!(format!("{}", atom_in_parens), "Int");
}
