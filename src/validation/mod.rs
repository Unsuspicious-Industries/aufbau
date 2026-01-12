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

pub mod completability;
pub mod complexity;

pub mod completable;

#[cfg(test)]
pub mod parseable;
