use super::super::*;
use crate::logic::grammar::Grammar;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;

fn load(spec: &str) -> Grammar {
    Grammar::load(spec).unwrap()
}

#[test]
fn completes_structural_prefix() {
    let grammar = load(
        r#"
        Identifier ::= /[a-z]+/
        Expr ::= 'begin' Identifier ';' Identifier 'end' | "!" Identifier
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "begin", 6, Some(Context::new()));
    assert!(matches!(result, CompletionResult::Success { .. }));
}

#[test]
fn keeps_completion_generic() {
    let grammar = load(
        r#"
        Name ::= /[A-Za-z]+/
        Ty ::= /[A-Za-z]+/
        Expr ::= 'λ' Name ':' Ty '.' Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "λ", 8, Some(Context::new()));
    match result {
        CompletionResult::Success { complete_input, .. } => {
            assert!(complete_input.starts_with('λ'));
            assert!(complete_input.contains(':'));
            assert!(complete_input.contains('.'));
        }
        other => panic!("expected success, got {other:?}"),
    }
}

#[test]
fn uses_incremental_advance() {
    let grammar = load(
        r#"
        Start ::= 'x' 'y'
        "#,
    );
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar.clone(), "x");
    let ast = synth.parse_with(&ctx).unwrap();

    let next = synth.feed("y", &ctx).unwrap();

    assert!(next.is_complete());
}
