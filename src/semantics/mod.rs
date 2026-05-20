//! Parser-facing semantic domain interface — `sec:semantic-domains`.
//!
//! The parser owns the syntactic fixed point and the obligation-routing
//! backbone. Domain implementations own the meaning of evidence IDs, contexts,
//! and right-bound effects. The semantics module encodes `def:evidence-graph`
//! vertices as arena interned handles and edges as obligation constraints.

pub mod domain;
pub mod evidence;
pub mod loader;
pub mod obligation;
pub mod runtime;

use crate::engine::error::TransitionError;
use crate::engine::parse::arena::{CtxId, EffectId, EvidenceId, NodeStatus, ProdId};
use crate::engine::Segment;

pub use domain::{ConstraintDomain, HasBindings, Verdict};
pub use evidence::EvidenceStore;
pub use loader::ConstraintLoader;
pub use obligation::{Obligation, Obligations};
pub use runtime::DomainRuntime;

/// Semantic information exported by a completed parser item.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct SemanticSummary {
    /// Opaque evidence value in the active semantic domain.
    pub evidence: EvidenceId,
    /// Optional right-bound effect. Exact nodes may export it to right siblings.
    pub effect: Option<EffectId>,
    /// Whether the semantic domain considers the node closed enough for roots.
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

/// Runtime hooks used by the prefix parser.
pub trait SemanticRuntime {
    /// Context selected before entering the child at the current dot.
    fn descend(
        &self,
        prod: ProdId,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &Obligations,
    ) -> Result<CtxId, TransitionError>;

    /// Final semantic summary for a closed or prefix parser item.
    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &Obligations,
        status: NodeStatus,
    ) -> Result<SemanticSummary, TransitionError>;

    /// Update the input segmentation visible to evidence lexemes.
    fn load_segs(&mut self, _s: &[Segment]) {}

    /// Apply a right-bound effect exported by an exact left sibling.
    fn apply_effect(&self, ctx: CtxId, effect: EffectId) -> Result<CtxId, TransitionError>;

    /// Compose exact child effects left-to-right for transparent productions.
    fn compose_effects(&self, effects: Vec<EffectId>) -> Result<Option<EffectId>, TransitionError>;
}
