use super::super::*;
use crate::logic::grammar::Grammar;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;

fn stlc_grammar() -> Grammar {
    Grammar::load(include_str!("../../../../examples/stlc.auf")).expect("load stlc grammar")
}

#[test]
fn stlc_lambda_type_prefix_has_completion_tokens() {
    // Goal: isolate empty-frontier failures reported by verify/completion for
    // parseable STLC prefixes. This guards against regressions where tokens()
    // silently returns empty and search cannot expand.
    let mut grammar = stlc_grammar();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "λ x : A", 20);
    let _typed = synth.parse_with(&Context::new()).expect("prefix parse");
    let tokens = synth.completions_with(&Context::new());

    assert!(
        !tokens.is_empty(),
        "expected non-empty completion frontier for prefix 'λ x : A'"
    );
}

#[test]
fn stlc_lambda_type_prefix_complete_k_not_empty() {
    // Goal: end-to-end guarantee that completion search can extend this prefix.
    // If this fails while the token-frontier test passes, the bug is in search
    // expansion/ranking rather than frontier token extraction.
    let mut grammar = stlc_grammar();
    let results = complete_k(&grammar, "λ x : A", 20, 3);
    assert!(
        !results.is_empty(),
        "expected complete_k to produce at least one completion for prefix 'λ x : A'"
    );
}

#[test]
fn stlc_complete_k_can_return_fewer_than_requested() {
    // Goal: `k` is an upper bound, not a guaranteed cardinality. This should
    // stay fast and still return any existing completions.
    let mut grammar = stlc_grammar();
    let results = complete_k(&grammar, "λ x : A", 20, 50);
    assert!(!results.is_empty(), "expected at least one completion");
    assert!(
        results.len() <= 50,
        "complete_k must not return more than requested"
    );
}
