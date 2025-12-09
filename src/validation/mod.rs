/// Validation Module
///
/// This module provides validation and testing infrastructure for constrained generation.
///
/// ## Submodules
///
/// - `completability`: Core completion algorithm and prefix soundness checking
/// - `binding`: Binding validation utilities
/// - `tests`: Comprehensive test suite for typed completions
///
/// ## Key Guarantees
///
/// 1. **Prefix Soundness**: Any completable string has all prefixes completable
/// 2. **Type Safety**: Completions respect typing rules
/// 3. **Completeness**: Completion results are fully parsed (no holes)
///
pub mod completability;

#[cfg(test)]
pub mod tests;

#[test]
fn debug() {
    let grammar = tests::xtlc::xtlc_grammar();
    let mut parser = crate::logic::Parser::new(grammar.clone());

    let input = "x";

    let parsed = parser.partial(input).unwrap();
    println!("Parsed: {}", parsed);

    // typchek
    let typed_ast = parsed.typed(&grammar).unwrap();
    println!("Typed AST: {}", typed_ast);
}
