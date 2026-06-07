use crate::engine::grammar::SPG;
use crate::typing::Context;
use crate::typing::TypingSynth;

use super::token_texts;

fn assert_same_parse_shape(left: &mut TypingSynth, right: &mut TypingSynth) {
    let left_ast = left.ast().unwrap();
    let right_ast = right.ast().unwrap();

    assert_eq!(left.input(), right.input());
    assert_eq!(left_ast.text(), right_ast.text());
    assert_eq!(left_ast.is_complete(), right_ast.is_complete());
    assert_eq!(left_ast.len(), right_ast.len());
    assert_eq!(left_ast.bound_texts(), right_ast.bound_texts());
}

#[test]
fn feed_reparses_cached_ast_after_punctuation_token() {
    let grammar = SPG::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't' '=' Name
        "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(grammar.clone(), "let x");

    let prefix = synth.ast().unwrap();
    assert_eq!(prefix.text(), "let x");
    assert!(!prefix.is_complete());

    let fed = synth.feed(":").unwrap();
    let mut fresh = TypingSynth::new(grammar, synth.input());

    assert_eq!(synth.input(), "let x:");
    assert_eq!(fed.text(), "let x:");
    assert_same_parse_shape(&mut synth, &mut fresh);
}

#[test]
fn feed_avoids_separator_when_token_boundaries_are_unambiguous() {
    let grammar = SPG::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't'
        "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(grammar, "let name");

    let fed = synth.feed(":").unwrap();

    assert_eq!(synth.input(), "let name:");
    assert_eq!(fed.text(), "let name:");
}

#[test]
fn feed_preserves_token_sequence_for_original_input() {
    let grammar = SPG::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't' '=' Name
        "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(grammar.clone(), "");
    let expected = ["let", "x", ":", "t", "=", "y"];

    for token in expected {
        let _ = synth.feed(token).unwrap();
    }

    assert_eq!(token_texts(&grammar, synth.input()), expected);
}

#[test]
fn feed_with_context_uses_latest_bindings() {
    let grammar = SPG::load(
        r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Expression ::= Variable

        x ∈ Γ
        ----------- (var)
        Γ(x)
        "#,
    )
    .unwrap();
    let ctx = Context::new()
        .extend("foo".into(), crate::typing::Type::raw("bool"))
        .unwrap();
    let mut synth = TypingSynth::new(grammar.clone(), "");

    let fed = synth.feed_with("foo", &ctx).unwrap();
    let mut fresh = TypingSynth::new(grammar.clone(), synth.input());
    let _ = fresh.parse_with(&ctx).unwrap();

    assert_eq!(synth.input(), "foo");
    assert_eq!(fed.text(), "foo");
    assert!(fed.is_complete());
    assert_same_parse_shape(&mut synth, &mut fresh);
}

#[test]
fn feed_error_leaves_extended_input_visible() {
    let grammar = SPG::load("Start ::= 'x'").unwrap();
    let mut synth = TypingSynth::new(grammar, "x");

    let err = synth.feed("y").unwrap_err();

    assert!(err.starts_with("Parse error:"));
    assert_eq!(synth.input(), "xy");
}
