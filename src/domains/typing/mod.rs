//! Typing constraint domain — `sec:typing-domain`, §3 of the draft.
//!
//! Instantiates `ConstraintDomain` with:
//! - `Rule`     = `TypingRule`
//! - `Evidence` = `Type` (interned as `TypeId = EvidenceId`)
//! - `Context`  = `Context` (ordered `Identifier → Type` map)
//! - `Effect`   = `ContextTransition` (extend/overwrite operations on `Context`)
//!
//! ## Realizability status
//!
//! ### Monotonicity (`lem:evidence-monotone`-analog)
//! Status: PROVEN — §3 Lemma (Type evidence is monotone).
//! Type evidence can only shrink under input extension via regex derivatives.
//!
//! ### Evidence realizability (`lem:evidence-realizable`-analog)
//! Status: PROVEN — §3 Lemma (Type evidence is realizable).
//!
//! ### Premise realizability (`lem:typeof-realizable`-analog)
//! Status: PROVEN — §3 Lemma (`typeof` is realizable).
//!
//! ### Rule realizability (`lem:rule-realizable`-analog)
//! Status: PROVEN — §3 Lemma (Rule realizability).
//!
//! ### `eval_impl` = eval (`thm:typing-realizable`-analog)
//! Status: PROVEN — §3 Theorem (Typing implementation computes ideal evaluator).

pub mod compiler;
pub mod context;
pub mod domain;
pub mod loader;
pub mod ops;
pub mod rule;
pub mod syntax;
pub mod types;

#[cfg(test)]
mod tests;

pub use compiler::{compile_rule, CompiledRule};
pub use context::{Context, ContextTransition, TreeStatus};
pub use domain::TypingDomain;
pub use loader::TypingRuleLoader;
pub use ops::{Unifier, UnifyResult, equal, subtype};
pub use types::{Type, TypeExpr};

pub use rule::{
    Conclusion, ConclusionContext, Premise, PremiseStatus, RuleParser, Term, TypeAscription,
    TypeSetting, TypingJudgment, TypingRule,
};

/// Concrete runtime for the typing domain.
pub type TypingRuntime = crate::semantics::runtime::DomainRuntime<TypingDomain>;

/// Concrete synthesizer for the typing domain.
pub type TypingSynth = crate::engine::synth::Synthesizer<TypingDomain>;
