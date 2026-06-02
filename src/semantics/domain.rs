//! Three-valued constraint verdict — `def:constraint-domain`.

/// Three-valued verdict produced by the typing domain's `finalize`.
///
/// `DomainRuntime`/`TypingRuntime` translates `Lost` into
/// `Err(TransitionError::Rejected)` at the parser boundary.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Verdict {
    /// `G(s) ∈ Closed`. All constraints are satisfied; the node may export
    /// an effect if its status is `Exact`.
    Satisfied,
    /// `G(s) ∉ Closed ∧ D(G(s)) ≠ ∅`. Some constraint is still pending
    /// but a satisfying continuation exists. No effect is exported.
    Live,
    /// `D(G(s)) = ∅`. No continuation can satisfy the constraints.
    /// Corresponds to `lem:safe-pruning`: the parser may discard this branch.
    Lost,
}
