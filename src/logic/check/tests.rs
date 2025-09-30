use super::TypeChecker;
use crate::logic::bind::{
    BoundConclusion, BoundConclusionContext, BoundConclusionKind, BoundPremise, BoundType,
    BoundTypeAscription, BoundTypeSetting, BoundTypingJudgment, BoundTypingRule,
};
use crate::logic::partial::{PartialASTNode, PartialNonTerminal, PartialTerminal};

fn mk_var_pnt(name: &str) -> PartialNonTerminal {
    PartialNonTerminal {
        production: crate::logic::partial::PartialProduction::from_progress(
            crate::logic::grammar::Production {
                rule: None,
                rhs: vec![],
            },
            0,
            0,
        ),
        children: vec![PartialASTNode::Terminal(PartialTerminal {
            value: name.to_string(),
            span: None,
            binding: None,
        })],
        value: "Variable".to_string(),
        span: None,
        binding: None,
        bound_typing_rule: None,
    }
}

fn ascr(name: &str, ty: BoundType) -> BoundTypeAscription {
    BoundTypeAscription {
        node: mk_var_pnt(name),
        ty,
    }
}

#[test]
fn conclusion_identity_default() {
    let rule = BoundTypingRule {
        name: "id".to_string(),
        premises: vec![],
        conclusion: BoundConclusion {
            context: BoundConclusionContext::default(),
            kind: BoundConclusionKind::Type(BoundType::Atom("Result".into())),
        },
    };
    let dummy = mk_var_pnt("dummy");
    let mut tc = TypeChecker::new();
    let ty = tc
        .apply_bound_rule_partial(&rule, &dummy)
        .expect("rule should apply");
    assert_eq!(ty, BoundType::Atom("Result".into()))
}

#[test]
fn conclusion_commits_output_extensions() {
    let out = BoundTypeSetting {
        name: "Γ".into(),
        extensions: vec![ascr("x", BoundType::Atom("int".into()))],
    };
    let rule = BoundTypingRule {
        name: "decl".into(),
        premises: vec![],
        conclusion: BoundConclusion {
            context: BoundConclusionContext {
                input: "Γ".into(),
                output: Some(out),
            },
            kind: BoundConclusionKind::Type(BoundType::Atom("void".into())),
        },
    };
    let dummy = mk_var_pnt("dummy");
    let mut tc = TypeChecker::new();
    let _ = tc
        .apply_bound_rule_partial(&rule, &dummy)
        .expect("rule should apply");
    let got = tc.context.lookup("x").cloned();
    assert_eq!(got, Some(BoundType::Atom("int".into())));
}

#[test]
fn conclusion_input_only_does_not_commit() {
    let rule = BoundTypingRule {
        name: "expr".into(),
        premises: vec![],
        conclusion: BoundConclusion {
            context: BoundConclusionContext {
                input: "Γ".into(),
                output: None,
            },
            kind: BoundConclusionKind::Type(BoundType::Atom("unit".into())),
        },
    };
    let dummy = mk_var_pnt("dummy");
    let mut tc = TypeChecker::new();
    let _ = tc
        .apply_bound_rule_partial(&rule, &dummy)
        .expect("rule should apply");
    assert!(tc.context.lookup("y").is_none());
}

#[test]
fn conclusion_context_lookup_uses_ambient() {
    let var_node = mk_var_pnt("z");
    let rule = BoundTypingRule {
        name: "var".into(),
        premises: vec![],
        conclusion: BoundConclusion {
            context: BoundConclusionContext::default(),
            kind: BoundConclusionKind::ContextLookup("Γ".into(), var_node),
        },
    };
    let dummy = mk_var_pnt("dummy");
    let mut tc = TypeChecker::new();
    tc.context_mut()
        .add("z".into(), BoundType::Atom("T".into()));
    let ty = tc
        .apply_bound_rule_partial(&rule, &dummy)
        .expect("rule should apply");
    assert_eq!(ty, BoundType::Atom("T".into()));
}
