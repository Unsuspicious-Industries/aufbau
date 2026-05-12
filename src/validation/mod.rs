//! Empirical verification layer for the parser/type runtime.
//!
//! This module is not the semantics. It is a bounded falsification harness for
//! properties that should follow from the semantics.
//!
//! The validation story has three layers.
//! `parseable` checks prefix acceptance over curated suites. `completability`
//! checks feed replay and token-level stability. `completable` runs the slower
//! end-to-end suites built on those replay checks.
//!
//! The intended paper reading is:
//! if the formal model states a property, the validation layer supplies a
//! reproducible empirical approximation of that property on representative
//! grammars, contexts, and prefixes.
//!
//! Core empirical obligations:
//! accepted prefixes remain replayable under token feeding; invalid examples do
//! not yield complete parses; and representative typed prefixes remain accepted
//! under extension/replay.
///
pub mod completability;

pub mod completable;

pub mod parseable;

pub mod properties;
