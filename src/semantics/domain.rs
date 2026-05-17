//! Abstract constraint domain — `def:constraint-domain` (post §6.A update).
//!
//! `D = (Rules, Closed, Ctx, eval, ⊕)`
//!
//! The theoretical `eval(G(s))` is not directly implementable online; the
//! parser builds `G(s)` incrementally. This trait exposes the operational
//! decomposition of `eval` into per-node hooks called as the arena grows.
//! See §6.B of `refactor.md` for the paper-level discussion.

use std::hash::Hash;

use crate::logic::grammar::Segment;
use crate::logic::parse::arena::NodeStatus;
use crate::logic::semantic::Obligations;

// ── Verdict ──────────────────────────────────────────────────────────────────

/// Three-valued verdict from `def:constraint-domain`.
///
/// Universal across all constraint domains — not an associated type.
/// `DomainRuntime<D>` translates `Lost` to `Err(TransitionError::Rejected)`
/// at the parser-facing `SemanticRuntime` boundary.
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

// ── HasBindings ───────────────────────────────────────────────────────────────

/// Exposes the binding names referenced by a semantic rule.
///
/// Required so that `engine::binding::build_binding_map` can construct the
/// binding map generically without knowing the concrete rule type.
/// `TypingRule` implements this by walking its premise list.
pub trait HasBindings {
    fn referenced_bindings(&self) -> Box<dyn Iterator<Item = &str> + '_>;
}

// ── ConstraintDomain ─────────────────────────────────────────────────────────

/// `D = (Rules, Closed, Ctx, eval, ⊕)` — `def:constraint-domain` (post-update).
///
/// ## Operational decomposition of `eval`
///
/// `eval(G(s))` from the paper operates on an already-built evidence graph.
/// This trait exposes the incremental form used by the online prefix parser:
/// - `descend` — `𝗱𝗲𝘀𝗰𝗲𝗻𝗱(p, b, Γ, Ω)` from §2 semantic interface
/// - `finalize` — `𝗳𝗶𝗻𝗮𝗹𝗶𝘇𝗲(p, Γ, Ω, ρ)` from §2 semantic interface
/// - `apply_effect` — `𝗮𝗽𝗽𝗹𝘆(Γ, ∇)` = `⊕` from §6.A
/// - `compose_effects` — left-to-right composition for transparent productions
///
/// ## Realizability obligations for implementers
///
/// Every impl SHOULD carry a module-level doc comment stating the proof
/// status of:
/// - monotonicity of evidence under input extension (`lem:evidence-monotone` analog)
/// - realizability of non-`Lost` premises (`lem:rule-realizable` analog)
/// - `eval_impl = eval` (`thm:typing-realizable` analog)
///
/// See `domains::typing` for the worked example.
pub trait ConstraintDomain {
    /// Elements of `Θ`. Must expose binding usage via `HasBindings` so that
    /// `engine::binding::build_binding_map` can be generic over the rule type.
    type Rule: HasBindings;

    /// `τ` in the evidence summary `ν = ⟨n,[i,j),ρ,τ,∇,β⟩`
    /// (`def:evidence-summary`). Interned into `EvidenceId` by `DomainRuntime`.
    type Evidence: Hash + Eq + Clone;

    /// `Ctx` from `def:constraint-domain` (post-update §6.A).
    /// Must be ordered (not `HashMap`) and implement `Hash + Eq + Clone` so
    /// that `DomainRuntime` can intern it into `CtxId`.
    type Context: Hash + Eq + Clone;

    /// `∇` in `def:evidence-summary`. Acts on `Ctx` via `apply_effect` (`⊕`).
    /// Interned into `EffectId` by `DomainRuntime`.
    type Effect: Hash + Eq + Clone;

    /// The empty context — identity for `apply_effect`.
    /// Distinguished element of `Ctx`; not listed as a separate tuple component
    /// because it is always an element of the `Ctx` set.
    fn empty_context(&self) -> Self::Context;

    /// The "no evidence yet" sentinel for nodes without an attached rule.
    /// Corresponds to `Type::Any` in the typing domain.
    fn any_evidence(&self) -> Self::Evidence;

    /// `𝗱𝗲𝘀𝗰𝗲𝗻𝗱(p, b, Γ, Ω)` — §2 semantic interface.
    ///
    /// Returns the context to use when entering child `binding` of the
    /// production whose rule is `rule`, given current obligations and segments.
    /// `binding = None` means the dot is not entering a named binding position.
    fn descend(
        &self,
        rule: &Self::Rule,
        binding: Option<&str>,
        ctx: &Self::Context,
        obligations: &Obligations,
        segs: &[Segment],
    ) -> Self::Context;

    /// `𝗳𝗶𝗻𝗮𝗹𝗶𝘇𝗲(p, Γ, Ω, ρ)` — §2 semantic interface.
    ///
    /// Operational embodiment of one node-level case of `eval(G(s))`.
    /// Returns `(verdict, τ, ∇)`:
    /// - `Satisfied`: all premises hold; effect may be `Some` only when
    ///   `status` is `Closed` or `Extensible`.
    /// - `Live`: some premise is still pending (`lem:rule-realizable` analog
    ///   guarantees a satisfying continuation exists); effect is always `None`.
    /// - `Lost`: no continuation can satisfy the premises; the parser may
    ///   prune this branch (`lem:safe-pruning`).
    fn finalize(
        &self,
        rule: &Self::Rule,
        ctx: &Self::Context,
        obligations: &Obligations,
        segs: &[Segment],
        status: NodeStatus,
    ) -> (Verdict, Self::Evidence, Option<Self::Effect>);

    /// `⊕` — `𝗮𝗽𝗽𝗹𝘆(Γ, ∇)` from §6.A.
    ///
    /// Apply a right-bound effect exported by an exact left sibling to the
    /// context of the next sibling.
    fn apply_effect(&self, ctx: Self::Context, effect: &Self::Effect) -> Self::Context;

    /// Left-to-right composition of effects for transparent productions.
    ///
    /// Returns `None` when the composed effect is the identity (no observable
    /// change to any context).
    fn compose_effects(&self, effects: &[&Self::Effect]) -> Option<Self::Effect>;
}
