use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;

use super::{completion_fingerprint, parse_with_ctx, token_texts, Synthesizer};

fn assert_same_parse_shape(left: &mut Synthesizer, right: &mut Synthesizer) {
    let left_ast = left.ast().unwrap();
    let right_ast = right.ast().unwrap();

    assert_eq!(left.input(), right.input());
    assert_eq!(left_ast.text(), right_ast.text());
    assert_eq!(left_ast.is_complete(), right_ast.is_complete());
    assert_eq!(left_ast.len(), right_ast.len());
    assert_eq!(
        left_ast.min_open_slots(left.grammar()),
        right_ast.min_open_slots(right.grammar())
    );
    assert_eq!(left_ast.min_tree_depth(), right_ast.min_tree_depth());
    assert_eq!(left_ast.bound_texts(), right_ast.bound_texts());
    assert_eq!(
        completion_fingerprint(&left_ast, left.grammar()),
        completion_fingerprint(&right_ast, right.grammar())
    );
}

#[test]
fn feed_reparses_cached_ast_after_punctuation_token() {
    let grammar = Grammar::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't' '=' Name
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar.clone(), "let x");

    let prefix = synth.ast().unwrap();
    assert_eq!(prefix.text(), "let x");
    assert!(!prefix.is_complete());

    let fed = synth.feed(":").unwrap();
    let mut fresh = Synthesizer::new(grammar, synth.input());

    assert_eq!(synth.input(), "let x:");
    assert_eq!(fed.text(), "let x:");
    assert_same_parse_shape(&mut synth, &mut fresh);
}

#[test]
fn feed_reparses_cached_ast_after_identifier_token() {
    let grammar = Grammar::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar.clone(), "");

    let _ = synth.ast().unwrap();
    let _ = synth.feed("let").unwrap();
    let fed = synth.feed("value").unwrap();
    let mut fresh = Synthesizer::new(grammar, synth.input());

    assert_eq!(synth.input(), "let value");
    assert_eq!(fed.text(), "let value");
    assert!(fed.is_complete());
    assert_same_parse_shape(&mut synth, &mut fresh);
}

#[test]
fn feed_sequence_matches_fresh_parse_after_each_step() {
    let grammar = Grammar::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't' '=' Name
        "#,
    )
    .unwrap();
    let mut incremental = Synthesizer::new(grammar.clone(), "");

    for token in ["let", "x", ":", "t", "=", "y"] {
        let _ = incremental.feed(token).unwrap();
        let mut fresh = Synthesizer::new(grammar.clone(), incremental.input());
        assert_same_parse_shape(&mut incremental, &mut fresh);
    }

    assert_eq!(incremental.input(), "let x:t=y");
    assert!(incremental.ast().unwrap().is_complete());
}

#[test]
fn feed_inserts_separator_between_adjacent_word_tokens() {
    let grammar = Grammar::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar, "let");

    let fed = synth.feed("name").unwrap();

    assert_eq!(synth.input(), "let name");
    assert_eq!(fed.text(), "let name");
}

#[test]
fn feed_avoids_separator_when_token_boundaries_are_unambiguous() {
    let grammar = Grammar::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't'
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar, "let name");

    let fed = synth.feed(":").unwrap();

    assert_eq!(synth.input(), "let name:");
    assert_eq!(fed.text(), "let name:");
}

#[test]
fn feed_preserves_token_sequence_for_original_input() {
    let grammar = Grammar::load(
        r#"
        Name ::= /[a-z]+/
        Start ::= 'let' Name ':' 't' '=' Name
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar.clone(), "");
    let expected = ["let", "x", ":", "t", "=", "y"];

    for token in expected {
        let _ = synth.feed(token).unwrap();
    }

    assert_eq!(token_texts(&grammar, synth.input()), expected);
}

#[test]
fn feed_with_context_uses_latest_bindings() {
    let grammar = Grammar::load(
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
        .extend("foo".into(), crate::logic::typing::Type::Raw("bool".into()))
        .unwrap();
    let mut synth = Synthesizer::new(grammar.clone(), "");

    let fed = synth.feed_with("foo", &ctx).unwrap();
    let mut fresh = Synthesizer::new(grammar.clone(), synth.input());
    let _ = fresh.parse_with(&ctx).unwrap();

    assert_eq!(synth.input(), "foo");
    assert_eq!(fed.text(), "foo");
    assert!(fed.is_complete());
    assert_same_parse_shape(&mut synth, &mut fresh);
}

#[test]
fn feed_updates_completions_after_successful_step() {
    let grammar = Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Expr ::= Number | '(' Expr ')'
        Start ::= Expr '+' Expr
        "#,
    )
    .unwrap();
    let mut synth = Synthesizer::new(grammar.clone(), "1");
    let before = completion_fingerprint(&synth.ast().unwrap(), &grammar);

    let after_ast = synth.feed("+").unwrap();
    let after = completion_fingerprint(&after_ast, &grammar);

    assert_ne!(before, after);
    assert!(after
        .iter()
        .any(|(_, example)| example.as_deref() == Some("0")));
    assert!(after
        .iter()
        .any(|(_, example)| example.as_deref() == Some("(")));
}

#[test]
fn feed_error_leaves_extended_input_visible() {
    let grammar = Grammar::load("Start ::= 'x'").unwrap();
    let mut synth = Synthesizer::new(grammar, "x");

    let err = synth.feed("y").unwrap_err();

    assert!(err.starts_with("Parse error:"));
    assert_eq!(synth.input(), "x y");
}

#[test]
fn feed_and_feed_with_match_fresh_context_parse() {
    let grammar = Grammar::load(
        r#"
        Identifier ::= /[a-z]+/
        Type ::= 'int'
        Variable(var) ::= Identifier[x]
        Let(let) ::= 'let' Identifier[x] ':' Type[τ] 'in' Expression[e]
        Expression ::= Variable | Let

        x ∈ Γ
        ----------- (var)
        Γ(x)

        Γ[x:τ] ⊢ e : ?T
        ------------------------ (let)
        ?T
        "#,
    )
    .unwrap();
    let ctx = Context::new();
    let input = "let x:int in x";
    let expected = parse_with_ctx(&grammar, input, &ctx);

    let mut synth = Synthesizer::new(grammar.clone(), "");
    for token in ["let", "x", ":", "int", "in", "x"] {
        let _ = synth.feed_with(token, &ctx).unwrap();
    }

    let actual = synth.ast().unwrap();
    assert_eq!(actual.text(), expected.text());
    assert_eq!(actual.is_complete(), expected.is_complete());
}
