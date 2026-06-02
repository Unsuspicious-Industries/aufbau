//! Typing constraint domain — `sec:typing-domain`, §3 of the draft.
//!
//! The constraint domain `D = (Rules, Closed, Ctx, eval, ⊕)` is realized by:
//! - rules    = `TypingRule`
//! - evidence = `Type` (interned as `TypeId = EvidenceId`)
//! - `Ctx`    = `Context` (ordered `Identifier → Type` map)
//! - `∇`      = `ContextTransition` (extend/overwrite operations on `Context`)
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
pub use ops::{Unifier, UnifyResult, equal, subtype};
pub use types::{Type, TypeExpr};

pub use rule::{
    Conclusion, ConclusionContext, Premise, PremiseStatus, RuleParser, Term, TypeAscription,
    TypeSetting, TypingJudgment, TypingRule,
};

pub use crate::engine::synth::Synthesizer as TypingSynth;
pub use crate::semantics::runtime::TypingRuntime;
