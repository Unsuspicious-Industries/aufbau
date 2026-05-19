use crate::domains::typing::TypingDomain;
use crate::engine::grammar::SPG;

mod feed;

pub(super) fn token_texts(grammar: &SPG<TypingDomain>, input: &str) -> Vec<String> {
    let mut grammar = grammar.clone();
    grammar
        .tokenize(input)
        .unwrap()
        .into_iter()
        .map(|segment| segment.text().to_string())
        .collect()
}
