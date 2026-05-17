//! Abstract SPG semantics — §1 of `draft/sections/01-abstract-spg-definition.tex`.
//!
//! This module contains:
//! - The constraint domain abstraction (`ConstraintDomain`, `Verdict`, `HasBindings`)
//! - The constraint loader abstraction (`ConstraintLoader`)
//! - The generic domain runtime bridge (`DomainRuntime<D>`)
//! - The parser-facing runtime trait (`SemanticRuntime`) and its summary type
//! - Obligations (`Obligation`, `Obligations`) — the parser-time realization
//!   of binding-resolution `β` (`def:beta-resolution`, `def:obligation-store`)

pub mod domain;
pub mod loader;
pub mod runtime;

pub use domain::{ConstraintDomain, HasBindings, Verdict};
pub use loader::ConstraintLoader;
