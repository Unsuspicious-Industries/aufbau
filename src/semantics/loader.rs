//! Constraint loader trait — the domain-specific half of grammar loading.
//!
//! `𝒯 : N ⇀ Θ` (from `sec:gram-def`) is split into two halves:
//! - The `NT → rule-name` map is declared in EBNF (lines like
//!   `Expr(rule) ::= …`) and handled by `engine::grammar::load_ebnf`.
//! - The `rule-name → Rule` body map is handled by this trait.
//!
//! `ConstraintLoader` is therefore NT-name-agnostic: it receives only the
//! non-EBNF blocks from the source file and returns a flat rule table.
//! The orchestrator `SPG::load_with` joins both halves.

use std::collections::HashMap;

use crate::semantics::domain::ConstraintDomain;

/// `𝒯 : N ⇀ Θ` (rule-body half) — `sec:gram-def`.
///
/// Receives the non-production blocks from the `.auf` source (everything
/// that is not a `::=` production) and returns the rule table keyed by
/// rule name. The `NT → rule-name` mapping is handled separately by the
/// EBNF loader and is not this trait's concern.
pub trait ConstraintLoader {
    type Domain: ConstraintDomain;

    fn load(
        &self,
        blocks: &[&str],
    ) -> Result<HashMap<String, <Self::Domain as ConstraintDomain>::Rule>, String>;
}
