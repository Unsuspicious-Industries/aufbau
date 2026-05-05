use crate::logic::grammar::Grammar;
use crate::logic::structure::ast::FusionAST;
use crate::logic::typing::Context;

use super::Synthesizer;

mod feed;

pub(super) fn token_texts(grammar: &Grammar, input: &str) -> Vec<String> {
    let mut grammar = grammar.clone();
    grammar
        .tokenize(input)
        .unwrap()
        .into_iter()
        .map(|segment| segment.text().to_string())
        .collect()
}

pub(super) fn parse_with_ctx(grammar: &Grammar, input: &str, ctx: &Context) -> FusionAST {
    let mut synth = Synthesizer::new(grammar.clone(), input);
    synth.parse_with(ctx).unwrap()
}
