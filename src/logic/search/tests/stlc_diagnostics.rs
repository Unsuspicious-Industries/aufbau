use super::super::*;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;

fn stlc_grammar() -> crate::logic::grammar::Grammar {
    crate::logic::grammar::Grammar::load(include_str!("../../../../examples/stlc.auf"))
        .expect("load stlc grammar")
}

#[test]
fn stlc_lambda_type_prefix_token_candidates_are_actionable() {
    let grammar = stlc_grammar();
    let ctx = Context::new();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "λ x : A", 20);
    let typed = synth.parse_with(&ctx).expect("prefix parse");
    let tokens = typed.completions(&grammar);

    assert!(!tokens.is_empty(), "expected non-empty frontier tokens");
}

#[test]
fn stlc_lambda_type_prefix_one_step_extend_succeeds() {
    let grammar = stlc_grammar();
    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar.clone(), "λ x : A");

    let ast = synth.parse_with(&ctx).expect("initial parse");
    let tokens = ast.completions(&grammar);

    let mut success = false;
    for tok in tokens {
        if let Some(example) = tok.example() {
            if synth.feed(&example, &ctx).is_ok() {
                success = true;
                break;
            }
        }
        if success {
            break;
        }
    }

    assert!(success, "expected at least one successful one-step extend");
}
