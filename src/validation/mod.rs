/// Validation Module
///
/// This module provides validation and testing infrastructure for constrained generation.
///
/// ## Submodules
///
/// - `completability`: Feed-replay and prefix soundness helpers
/// - `completable`: Slow but thorough feed-acceptance suites
/// - `parseable`: Fast prefix-parseability tests
///
/// ## Key Guarantees
///
/// 1. **Prefix Soundness**: Accepted strings stay feed-accepting at every prefix
/// 2. **Type Safety**: Accepted token steps respect typing rules
/// 3. **Closure**: Derived witnesses are fully parsed (no holes)
///
pub mod completability;

pub mod completable;

pub mod parseable;
