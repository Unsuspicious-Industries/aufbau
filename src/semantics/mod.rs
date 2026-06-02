//! Shared semantic primitives.
//!
//! The parser owns the syntactic fixed point and the obligation-routing
//! backbone; the typing domain owns the meaning of evidence, contexts, and
//! right-bound effects. This module holds the pieces both sides share: the
//! evidence interner, the obligation store, the three-valued `Verdict`, and
//! the per-node `SemanticSummary` exported to the arena.

pub mod domain;
pub mod evidence;
pub mod obligation;
pub mod runtime;

use crate::engine::parse::arena::{EffectId, EvidenceId};

pub use domain::Verdict;
pub use evidence::EvidenceStore;
pub use obligation::{Obligation, Obligations};
pub use runtime::TypingRuntime;

/// Semantic information exported by a completed parser item.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SemanticSummary {
    /// Opaque evidence value (an interned `Type`).
    pub evidence: EvidenceId,
    /// Optional right-bound effect. Exact nodes may export it to right siblings.
    pub effect: Option<EffectId>,
    /// Whether the typing domain considers the node closed enough for roots.
    pub complete: bool,
}

impl SemanticSummary {
    #[must_use]
    pub fn new(evidence: EvidenceId, effect: Option<EffectId>, complete: bool) -> Self {
        Self {
            evidence,
            effect,
            complete,
        }
    }
}
