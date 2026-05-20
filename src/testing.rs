//! Testing utilities for P7
//!
//! This module provides common testing utilities to reduce code duplication
//! across test modules. It includes grammar loading, parsing assertions,
//! type checking assertions, and tree comparison utilities.

use crate::domains::typing::TypingDomain;
use crate::engine::grammar::SPG;
use std::path::Path;

// ============================================================================
// Grammar Loading Utilities
// ============================================================================

/// Load a grammar from the examples directory
///
/// # Example
/// ```ignore
/// let g = load_example_grammar("stlc");
/// ```
pub fn load_example_grammar(name: &str) -> SPG<TypingDomain> {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir)
        .join("examples")
        .join(format!("{}.auf", name));
    let content = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
    SPG::<TypingDomain>::load(&content)
        .unwrap_or_else(|e| panic!("Failed to load grammar '{}': {}", name, e))
}

/// Load grammar from inline specification
///
/// # Example
/// ```ignore
/// let g = load_inline_grammar(r#"
///     expr ::= "x" | "y"
///     start ::= expr
/// "#);
/// ```
pub fn load_inline_grammar(spec: &str) -> SPG<TypingDomain> {
    SPG::<TypingDomain>::load(spec).expect("Failed to load inline grammar")
}

/// Common test grammars as lazy statics
pub mod grammars {
    use super::*;
    use std::sync::OnceLock;

    static STLC: OnceLock<SPG<TypingDomain>> = OnceLock::new();
    static CLIKE: OnceLock<SPG<TypingDomain>> = OnceLock::new();
    static IMP: OnceLock<SPG<TypingDomain>> = OnceLock::new();
    static FUN: OnceLock<SPG<TypingDomain>> = OnceLock::new();

    pub fn stlc() -> &'static SPG<TypingDomain> {
        STLC.get_or_init(|| load_example_grammar("stlc"))
    }

    pub fn clike() -> &'static SPG<TypingDomain> {
        CLIKE.get_or_init(|| load_example_grammar("clike"))
    }

    pub fn imp() -> &'static SPG<TypingDomain> {
        IMP.get_or_init(|| load_example_grammar("imp"))
    }

    pub fn fun() -> &'static SPG<TypingDomain> {
        FUN.get_or_init(|| load_example_grammar("fun"))
    }
}
