//! Testing utilities for P7
//!
//! This module provides common testing utilities to reduce code duplication
//! across test modules. It includes grammar loading, parsing assertions,
//! type checking assertions, and tree comparison utilities.
//!
//! # Equality Checking Strategy
//!
//! ## Full Equality
//!
//! Since serializations are injective (one-to-one), we use hashes of serializations
//! for efficient equality checking. This eliminates redundant tree traversal code.
//!
//! ## Structural Equality
//!
//! When we only want to check for equality of structure,
//! and not take into account extra info like continuations/derivatives,
//! we use the `serialize_structure` method to get a structure-only serialization
//! and compare hashes of those.

use crate::logic::grammar::Grammar;
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
pub fn load_example_grammar(name: &str) -> Grammar {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir)
        .join("examples")
        .join(format!("{}.auf", name));
    let content = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read {}: {}", path.display(), e));
    Grammar::load(&content).unwrap_or_else(|e| panic!("Failed to load grammar '{}': {}", name, e))
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
pub fn load_inline_grammar(spec: &str) -> Grammar {
    Grammar::load(spec).expect("Failed to load inline grammar")
}

/// Common test grammars as lazy statics
pub mod grammars {
    use super::*;
    use std::sync::OnceLock;

    static STLC: OnceLock<Grammar> = OnceLock::new();
    static CLIKE: OnceLock<Grammar> = OnceLock::new();
    static IMP: OnceLock<Grammar> = OnceLock::new();
    static FUN: OnceLock<Grammar> = OnceLock::new();

    pub fn stlc() -> &'static Grammar {
        STLC.get_or_init(|| load_example_grammar("stlc"))
    }

    pub fn clike() -> &'static Grammar {
        CLIKE.get_or_init(|| load_example_grammar("clike"))
    }

    pub fn imp() -> &'static Grammar {
        IMP.get_or_init(|| load_example_grammar("imp"))
    }

    pub fn fun() -> &'static Grammar {
        FUN.get_or_init(|| load_example_grammar("fun"))
    }
}
