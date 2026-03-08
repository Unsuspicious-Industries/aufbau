use crate::logic::grammar::Grammar;
use crate::logic::partial::Synthesizer;
use crate::logic::typing::Context;

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
