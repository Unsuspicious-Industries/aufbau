use super::TypeChecker;
use crate::logic::bind::{
    BoundTypingRule,
    BoundPremise,
    BoundTypingJudgment,
    BoundConclusion,
    BoundConclusionKind,
    BoundConclusionContext,
    BoundTypeSetting,
    BoundTypeAscription,
    BoundType,
};
use crate::logic::ast::{ASTNode, NonTerminal, Terminal};
use crate::logic::grammar::tests::STLC_SPEC;
use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use crate::logic::debug::{set_debug_level, set_debug_input, DebugLevel};
use crate::debug_info;

fn mk_var_nt(name: &str) -> NonTerminal {
    NonTerminal {
        value: "Variable".to_string(),
        span: None,
        binding: None,
        children: vec![ASTNode::Terminal(Terminal { value: name.to_string(), span: None, binding: None })],
        bound_typing_rule: None,
    }
}

fn ascr(name: &str, ty: BoundType) -> BoundTypeAscription {
    BoundTypeAscription { node: mk_var_nt(name), ty }
}

#[test] fn stlc_simple_ok() {
    let expr = "(λy:a->a.y)((λx:a->a.x)z)";

    // Enable debug output for this test
    set_debug_level(DebugLevel::Info);
    set_debug_input(Some(expr.to_string()));

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC)
                        .unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    debug_info!("test", "AST: {}", ast.pretty());

    let mut tc = TypeChecker::new();
    // Add the free variable z to the context
    tc.add("z".to_string(), crate::logic::bind::typing::BoundType::Atom("a".to_string()));
    let res = tc.check(&ast);
    match res {
        Ok(ty) => {
            if let Some(ty) = ty {
                debug_info!("test", "Type: {:?}", ty);
            } else {
                debug_info!("test", "No type inferred");
            }
        }
        Err(e) => {
            debug_info!("test", "Error: {}", e);
            panic!("Type checking failed: {}", e);
        }
    }
}

#[test] fn stlc_type_mismatch_fails() {
    // This should fail: applying a function expecting a->b to something of type c->d
    let expr = "(λf:a->b.f)((λx:c->d.x)z)";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::new();
    // Add z with a type that doesn't match what's expected
    tc.add("z".to_string(), crate::logic::bind::typing::BoundType::Atom("wrongtype".to_string()));

    let res = tc.check(&ast);
    assert!(res.is_err(), "Expected type mismatch error but type checking succeeded");
    
    if let Err(e) = res {
        debug_info!("test", "Expected error: {}", e);
        assert!(e.contains("Type mismatch") || e.contains("expected") || e.contains("found") || e.contains("Could not resolve"));
    }
}

#[test] fn stlc_unbound_variable_fails() {
    // This should fail: unbound variable z
    let expr = "(λx:a->a.x)((λy:a->a.y)z)";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::new();    
    // Don't add z to context - it should fail as unbound variable
    
    let res = tc.check(&ast);
    assert!(res.is_err(), "Expected unbound variable error but type checking succeeded");
    
    if let Err(e) = res {
        debug_info!("test", "Expected error: {}", e);
        assert!(e.contains("not found in context") || e.contains("unbound") || e.contains("Could not resolve"));
    }
}

#[test] fn stlc_simple_lambda_ok() {
    // This should pass: simple lambda application
    let expr = "(λx:a->a.x)((λy:a->a.y)z)";

    let mut parser = Parser::new(
        Grammar::load(STLC_SPEC).unwrap()
    );

    let ast = parser.parse(expr).unwrap();
    let mut tc = TypeChecker::new();
    // Add z with matching type
    tc.add("z".to_string(), crate::logic::bind::typing::BoundType::Atom("a".to_string()));

    let res = tc.check(&ast);
    assert!(res.is_ok(), "Expected type checking to succeed");
    
    if let Ok(Some(ty)) = res {
        assert_eq!(ty, crate::logic::bind::typing::BoundType::Atom("a".to_string()));
        debug_info!("test", "Successfully inferred type: {:?}", ty);
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
    let dummy = NonTerminal { value: "Dummy".into(), span: None, children: vec![], binding: None, bound_typing_rule: None };
    let mut tc = TypeChecker::new();
    let ty = tc.apply_bound_rule(&rule, &dummy).expect("rule should apply");
    assert_eq!(ty, BoundType::Atom("Result".into()));
    assert!(tc.context.lookup("any").is_none());
}

#[test]
fn conclusion_commits_output_extensions() {
    let out = BoundTypeSetting { name: "Γ".into(), extensions: vec![ascr("x", BoundType::Atom("int".into()))] };
    let rule = BoundTypingRule {
        name: "decl".into(),
        premises: vec![],
        conclusion: BoundConclusion { context: BoundConclusionContext { input: "Γ".into(), output: Some(out) }, kind: BoundConclusionKind::Type(BoundType::Atom("void".into())) },
    };
    let dummy = NonTerminal { value: "Dummy".into(), span: None, children: vec![], binding: None, bound_typing_rule: None };
    let mut tc = TypeChecker::new();
    let _ = tc.apply_bound_rule(&rule, &dummy).expect("rule should apply");
    let got = tc.context.lookup("x").cloned();
    assert_eq!(got, Some(BoundType::Atom("int".into())));
}

#[test]
fn conclusion_input_only_does_not_commit() {
    let input = BoundTypeSetting { name: "Γ".into(), extensions: vec![ascr("y", BoundType::Atom("Bool".into()))] };
    let rule = BoundTypingRule {
        name: "expr".into(),
        premises: vec![],
        conclusion: BoundConclusion { context: BoundConclusionContext { input: "Γ".into(), output: None }, kind: BoundConclusionKind::Type(BoundType::Atom("unit".into())) },
    };
    let dummy = NonTerminal { value: "Dummy".into(), span: None, children: vec![], binding: None, bound_typing_rule: None };
    let mut tc = TypeChecker::new();
    let _ = tc.apply_bound_rule(&rule, &dummy).expect("rule should apply");
    assert!(tc.context.lookup("y").is_none());
}

#[test]
fn conclusion_context_lookup_uses_ambient() {
    let var_node = mk_var_nt("z");
    let rule = BoundTypingRule {
        name: "var".into(),
        premises: vec![],
        conclusion: BoundConclusion { context: BoundConclusionContext::default(), kind: BoundConclusionKind::ContextLookup("Γ".into(), var_node) },
    };
    let dummy = NonTerminal { value: "Dummy".into(), span: None, children: vec![], binding: None, bound_typing_rule: None };
    let mut tc = TypeChecker::new();
    tc.add("z".into(), BoundType::Atom("T".into()));
    let ty = tc.apply_bound_rule(&rule, &dummy).expect("rule should apply");
    assert_eq!(ty, BoundType::Atom("T".into()));
}