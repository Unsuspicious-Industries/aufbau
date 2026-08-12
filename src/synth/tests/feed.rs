use crate::grammar::SPG;
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
    // Adjacent word tokens (`let` then `x`) need a separator: under maximal munch
    // `letx` is one identifier, so a real token stream is whitespace-delimited.
    for token in ["let ", "x ", ": ", "t ", "= ", "y"] {
        let _ = synth.feed(token).unwrap();
    }

    let expected = ["let", "x", ":", "t", "=", "y"];
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
    let ctx = Context::new().shadow("foo".into(), crate::typing::Type::raw("bool"));
    let mut synth = TypingSynth::new(grammar.clone(), "");

    let fed = synth.feed_with("foo", &ctx).unwrap();
    let mut fresh = TypingSynth::new(grammar.clone(), synth.input());
    let _ = fresh.parse_with(&ctx).unwrap();

    assert_eq!(synth.input(), "foo");
    assert_eq!(fed.text(), "foo");
    assert!(fed.is_complete());
    assert_same_parse_shape(&mut synth, &mut fresh);
}

/// A rejected feed is a no-op. This previously asserted the opposite — that the
/// extended input stayed visible as `"xy"` — because `feed` installed the input
/// before parsing it. That left the synthesizer holding text it had already
/// rejected, with its tree dropped, so every later call worked off broken state.
#[test]
fn feed_error_leaves_state_unchanged() {
    let grammar = SPG::load("Start ::= 'x'").unwrap();
    let mut synth = TypingSynth::new(grammar, "x");

    let err = synth.feed("y").unwrap_err();

    assert!(err.starts_with("Parse error:"));
    assert_eq!(synth.input(), "x", "rejected token must not be installed");
    // The prior parse is still available, so the failure cost nothing.
    let ast = synth.ast().expect("state survives a rejected feed");
    assert!(ast.is_complete());
}

/// Whatever `try_feed` accepts, `feed` accepts from the same state, and whatever
/// it rejects, `feed` rejects. `mask` is built on `try_feed`, so a divergence
/// here would make the generation mask unusable.
#[test]
fn try_feed_agrees_with_feed() {
    let grammar = SPG::load("Start ::= 'x' 'y'").unwrap();
    for token in [" y", " z", "", "y"] {
        let mut probe = TypingSynth::new(grammar.clone(), "x");
        let predicted = probe.try_feed(token).is_ok();
        assert_eq!(probe.input(), "x", "try_feed must not mutate");

        let mut committed = TypingSynth::new(grammar.clone(), "x");
        assert_eq!(
            committed.feed(token).is_ok(),
            predicted,
            "try_feed and feed disagree on {token:?}"
        );
    }
}
