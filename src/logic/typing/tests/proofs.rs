use regex_syntax::ast::print;


#[test]
fn test_proof() {
    use crate::logic::typing::{TypingJudgment, TypingRule, Premise, Type};

    let premises = "Γ ⊢ e : τ₁ → τ₂, Γ ⊢ e₁ : τ₁".to_string();
    let conclusion = "τ₂".to_string();
    let rule = TypingRule::new(
        premises, 
        conclusion,
        "app".to_string()
    ).expect("parse app rule");

    println!("Typing Rule:\n{}", rule.pretty(0));
}