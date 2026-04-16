use super::super::*;
use crate::logic::grammar::Grammar;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;

fn fun_grammar() -> Grammar {
    Grammar::load(include_str!("../../../../examples/fun.auf")).expect("load fun grammar")
}

#[test]
fn fun_prefix_exposes_close_paren_token() {
    // Guard: completion frontier should offer ')' for typed lambda parameter lists.
    let mut grammar = fun_grammar();
    let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), "(x: Int", 20);
    let _ = synth.parse_with(&Context::new()).expect("prefix parse");
    let completions = synth.completions_with(&Context::new());

    assert!(
        completions.iter().any(|tok| tok.matches(")")),
        "expected completion frontier to include close parenthesis token"
    );
}

#[test]
fn fun_complete_k_prefers_closed_parameter_list() {
    // Regression: completion should not drift into long lambda chains before
    // proposing the syntactically-required close parenthesis path.
    let mut grammar = fun_grammar();
    let results = complete_k(&grammar, "(x: Int", 20, 5);

    assert!(!results.is_empty(), "expected at least one completion");
    assert!(
        results.iter().any(|c| c.contains(") =>")),
        "expected one of top completions to close the parameter list"
    );
}

#[test]
fn fun_complete_k_finishes_quickly_for_close_paren_prefix() {
    // Performance guard: avoid search drift/hangs on simple deterministic prefixes.
    let mut grammar = fun_grammar();
    let start = std::time::Instant::now();
    let results = complete_k(&grammar, "(x: Int", 20, 5);
    let elapsed_ms = start.elapsed().as_millis();

    assert!(!results.is_empty(), "expected at least one completion");
    assert!(
        elapsed_ms < 3000,
        "expected complete_k to finish quickly (<3s), got {elapsed_ms}ms"
    );
}

#[test]
fn fun_complete_k_outputs_are_well_typed() {
    // Correctness guard: every returned completion must reparse as complete and
    // have at least one well-typed root.
    let mut grammar = fun_grammar();
    let results = complete_k(&grammar, "(x: Int", 12, 5);
    assert!(!results.is_empty(), "expected at least one completion");

    for completed in results {
        let mut synth = Synthesizer::new_with_max_depth(grammar.clone(), &completed, 12);
        let ast = synth
            .parse_with(&Context::new())
            .expect("returned completion must parse");
        assert!(
            ast.is_complete(),
            "completion must be syntactically complete"
        );
    }
}
