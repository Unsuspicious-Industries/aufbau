use crate::logic::grammar::Grammar;
use crate::logic::partial::{MetaParser, PartialAST};

/// Try extending input with a token by parsing both concatenated and spaced forms.
/// Returns the parsed PartialAST and the exact extended input used.
pub fn parse_extended_input(
    grammar: &Grammar,
    input: &str,
    token: &str,
) -> Result<(PartialAST, String), String> {
    let mut parser = MetaParser::new(grammar.clone());

    let direct = format!("{}{}", input, token);
    if let Ok((partial, _depth)) = parser.meta_partial(&direct) {
        return Ok((partial, direct));
    }

    let spaced = format!("{} {}", input, token);
    if let Ok((partial, _depth)) = parser.meta_partial(&spaced) {
        return Ok((partial, spaced));
    }

    Err(format!(
        "Parse failed for input='{}' token='{}'",
        input, token
    ))
}
