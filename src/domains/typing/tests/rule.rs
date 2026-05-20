use crate::domains::typing::compiler::compile_rule;
use crate::domains::typing::rule::TypingRule;
use crate::domains::typing::TypeExpr;

#[test]
fn parse_app_rule_with_metas() {
    let rule = TypingRule::new(
        "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A".into(),
        "?B".into(),
        "app".into(),
    )
    .unwrap();

    assert_eq!(rule.name, "app");
    assert_eq!(rule.premises.len(), 2);
    assert!(rule.conclusion.kind.has_metas());
}

#[test]
fn parse_var_rule() {
    let rule = TypingRule::new("x ∈ Γ".into(), "Γ(x)".into(), "var".into()).unwrap();

    assert_eq!(rule.name, "var");
    assert_eq!(rule.premises.len(), 1);
    assert_eq!(rule.conclusion.kind, TypeExpr::ContextExt("x".into()));
}

#[test]
fn parse_lambda_rule_with_context_extension() {
    let rule = TypingRule::new(
        "Γ[a:'A'] ⊢ e : ?R".into(),
        "'A' -> ?R".into(),
        "lambda".into(),
    )
    .unwrap();

    assert_eq!(rule.name, "lambda");
    assert_eq!(rule.premises.len(), 1);
    let premise = &rule.premises[0];
    let setting = premise.setting.as_ref().unwrap();
    assert_eq!(setting.name, "Γ");
    assert_eq!(setting.extensions.len(), 1);
    assert_eq!(setting.extensions[0].0, "a");
    assert_eq!(setting.extensions[0].1, TypeExpr::Lit("A".into()));
}

#[test]
fn parse_define_rule_with_context_transform() {
    let rule = TypingRule::new(
        "Γ ⊢ value : ?T".into(),
        "Γ → Γ[name:?T] ⊢ 'Unit'".into(),
        "define".into(),
    )
    .unwrap();

    assert_eq!(rule.name, "define");
    assert_eq!(rule.premises.len(), 1);
    let ctx = &rule.conclusion.context;
    assert_eq!(ctx.input, "Γ");
    let output = ctx.output.as_ref().unwrap();
    assert_eq!(output.extensions.len(), 1);
    assert_eq!(output.extensions[0].0, "name");
    assert_eq!(output.extensions[0].1, TypeExpr::Meta("T".into()));
    assert_eq!(rule.conclusion.kind, TypeExpr::Lit("Unit".into()));
}

#[test]
fn compilation_eliminates_metas() {
    let rule = TypingRule::new(
        "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A".into(),
        "?B".into(),
        "app".into(),
    )
    .unwrap();
    let compiled = compile_rule(&rule).unwrap();
    let all_metas = compiled.conclusion.kind.metas();
    assert!(
        !all_metas.iter().any(|m| *m == "A" || *m == "B"),
        "original user-named metas should be gone, got {:?}",
        all_metas
    );
}

#[test]
fn compiled_app_rule_has_typeof_constraints() {
    let rule = TypingRule::new(
        "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A".into(),
        "?B".into(),
        "app".into(),
    )
    .unwrap();
    let compiled = compile_rule(&rule).unwrap();
    assert!(!compiled.premises.is_empty(), "should have premises");
    let texts: Vec<_> = compiled.premises.iter().map(|p| format!("{}", p)).collect();
    let j = texts.join("\n");
    assert!(
        j.contains("=") || j.contains("typeof"),
        "premises should have constraints: {}",
        j
    );
}

#[test]
fn rule_with_single_meta_compiles() {
    let rule = TypingRule::new("Γ ⊢ e : ?R".into(), "?R".into(), "single".into()).unwrap();
    let compiled = compile_rule(&rule).unwrap();
    let has_typeof_e = compiled.premises.iter().any(|p| {
        let s = format!("{}", p);
        s.contains("typeof(e)")
    });
    assert!(has_typeof_e, "should have typeof(e) constraint");
}
