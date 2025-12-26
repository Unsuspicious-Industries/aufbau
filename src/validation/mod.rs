/// Validation Module
///
/// This module provides validation and testing infrastructure for constrained generation.
///
/// ## Submodules
///
/// - `completability`: Core completion algorithm and prefix soundness checking
/// - `completable`: Slow but thorough completion tests (BFS exploration)
/// - `parseable`: Fast prefix-parseability tests (no completion search)
///
/// ## Key Guarantees
///
/// 1. **Prefix Soundness**: Any completable string has all prefixes completable
/// 2. **Type Safety**: Completions respect typing rules
/// 3. **Completeness**: Completion results are fully parsed (no holes)
///
use crate::{logic::debug, set_debug_level};

pub mod completability;

pub mod completable;
use crate::validation::completable::xtlc::xtlc_grammar;

#[cfg(test)]
pub mod parseable;

#[test]

fn debug() {
    let grammar = xtlc_grammar();
    let mut parser = crate::logic::Parser::new(grammar.clone());
    set_debug_level(crate::DebugLevel::Trace);
    let input = "λx";

    let parsed = parser.partial(input).unwrap();
    println!("Parsed: {}", parsed);

    // sort
    let mut roots = parsed.roots().to_vec();
    roots.sort();
    // typchek
    let typed_ast = parsed.typed(&grammar).unwrap();
    // smallest root
    let sr = roots.into_iter().min().unwrap();
    println!("Smallest root: {}", sr);
}
