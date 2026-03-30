use crate::logic::grammar::Grammar;
use crate::logic::partial::Synthesizer;
use crate::logic::typing::Context;
use crate::testing::load_example_grammar;

const SCOPED_TYPED_SPEC: &str = r#"
    Identifier ::= /[a-z]+/
    Type ::= 'X' | 'Y'
    Variable(var) ::= Identifier[x]
    Num(num) ::= /[0-9]+/
    Let(letb) ::= 'def' Identifier[name] ':' Type[τ] '=' Atom[value] 'in' Expr[body]
    Scoped(scoped) ::= '{' Expr[inner] '}'
    Atom ::= Variable | Num | Scoped | '(' Expr ')'
    Expr ::= Let | Atom

    x ∈ Γ
    ----------- (var)
    Γ(x)

    ----------- (num)
    'X'

    Γ ⊢ value : τ, Γ[name:τ] ⊢ body : ?R
    ----------- (letb)
    ?R

    [Γ] ⊢ inner : ?T
    ----------- (scoped)
    ?T
"#;

/// "def a : X =" should have typed completions (the value position expects an Atom).
#[test]
fn typed_completions_for_def_after_equals() {
    let grammar = Grammar::load(SCOPED_TYPED_SPEC).unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar.clone(), "def a : X =");
    let tokens = synth.completions_ctx(&ctx);
    assert!(
        !tokens.is_empty(),
        "should have typed completions for 'def a : X ='"
    );
}

/// try_extend from "def a : X =" with a numeric literal must succeed.
#[test]
fn try_extend_def_after_equals_with_number() {
    let grammar = Grammar::load(SCOPED_TYPED_SPEC).unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar.clone(), "def a : X =");
    assert!(
        synth.try_extend("1", &ctx).is_ok(),
        "should extend 'def a : X =' with '1'"
    );
}

#[test]
fn extend_commits_input_and_tree() {
    let grammar = Grammar::load(SCOPED_TYPED_SPEC).unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar, "def a : X =");

    let typed = synth.extend("1", &ctx).expect("extend should succeed");

    assert_eq!(synth.input(), "def a : X = 1");
    assert_eq!(synth.tree().unwrap().text(), typed.text());
}

#[test]
fn extend_with_regex_commits_input_and_tree() {
    let grammar = Grammar::load(SCOPED_TYPED_SPEC).unwrap();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar, "def a : X =");
    let token = synth
        .completions_ctx(&ctx)
        .iter()
        .next()
        .cloned()
        .expect("expected at least one completion token");

    let (typed, extended) = synth
        .extend_with_regex(&token, &ctx)
        .expect("regex extension should succeed");

    assert_eq!(synth.input(), extended);
    assert_eq!(synth.tree().unwrap().text(), typed.text());
}

#[test]
fn feed_snapshot_matches_stepwise() {
    let grammar = Grammar::load(SCOPED_TYPED_SPEC).unwrap();
    let ctx = Context::new();

    let mut snapshot = Synthesizer::new(grammar.clone(), "def");
    let snapshot_tokens = snapshot.feed("def a : X = 1", &ctx);

    let mut stepwise = Synthesizer::new(grammar, "def");
    stepwise
        .extend("a", &ctx)
        .expect("binder extension should succeed");
    stepwise
        .extend(":", &ctx)
        .expect("type separator should succeed");
    stepwise
        .extend("X", &ctx)
        .expect("type name should succeed");
    stepwise.extend("=", &ctx).expect("equals should succeed");
    let stepwise_typed = stepwise
        .extend("1", &ctx)
        .expect("value extension should succeed");
    let stepwise_tokens = stepwise.completions_ctx(&ctx);

    assert_eq!(snapshot.input(), "def a : X = 1");
    assert_eq!(snapshot.tree().unwrap().text(), stepwise_typed.text());
    assert_eq!(
        snapshot_tokens
            .iter()
            .map(|token| token.to_pattern())
            .collect::<Vec<_>>(),
        stepwise_tokens
            .iter()
            .map(|token| token.to_pattern())
            .collect::<Vec<_>>(),
    );
}

#[test]
fn completions_stay_typed() {
    let grammar = load_example_grammar("fun");
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar, "true +");

    let tokens = synth.completions_ctx(&ctx);

    assert!(
        tokens.is_empty(),
        "typed completion must not fall back to structural completions for ill-typed prefixes",
    );
}
