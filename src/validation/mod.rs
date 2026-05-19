//! Validation modules — empirical checks for parser correctness.
//!
//! `parseable` checks prefix acceptance over curated suites.
//! `realizability` provides a domain-pluggable framework for testing
//! realizability invariants (witness existence, monotonicity, loss
//! soundness) over randomly generated grammars.
//! `properties` houses proptest-based property tests.

pub mod parseable;
pub mod properties;

