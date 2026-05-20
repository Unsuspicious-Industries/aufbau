//! Validation modules — empirical checks for parser correctness.
//!
//! `parseable` checks prefix acceptance over curated suites, exercising
//! `thm:completability-soundness` and `thm:prefix-monotonicity`.
//! `properties` houses proptest-based property tests.

pub mod parseable;
pub mod properties;
