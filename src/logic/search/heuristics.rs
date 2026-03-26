// HEURISTICS: BAD
//
// This module intentionally contains ad hoc ranking rules for completion search.
// Keep these isolated from the parser core so they can be audited, disabled,
// or replaced without changing parsing semantics.

use crate::logic::partial::CompletionSet;
use crate::regex::Regex as DerivativeRegex;

pub fn ordered_tokens(tokens: CompletionSet) -> Vec<DerivativeRegex> {
    let mut items = tokens.tokens;
    items.sort_by_key(token_priority);
    items
}

fn token_priority(token: &DerivativeRegex) -> (usize, usize, String) {
    let pattern = token.to_pattern();
    let structural = matches!(
        pattern.as_str(),
        ";" | ")" | "}" | "]" | ">" | ":" | "=" | "=>" | "in"
    ) as usize;
    let operator = matches!(
        pattern.as_str(),
        "+" | "-" | "*" | "/" | "+." | "-." | "*." | "/."
    ) as usize;
    let broad_regex =
        usize::from(pattern.contains('[') || pattern.contains('*') || pattern.contains('+'));

    (
        usize::MAX - structural,
        usize::MAX - operator + broad_regex,
        pattern,
    )
}
