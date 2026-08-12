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

// Grouped by pipeline stage. These are *not* separate directories on purpose:
// they reference each other densely (`domain` → `ir` → `rule` → `types`), so a
// directory per stage would add a path level and lengthen every import without
// isolating anything. The cut itself is documented in `docs/architecture.md`.

// Before the cut — what a rule says, and how `.auf` text becomes one.
pub mod loader;
pub mod rule;
pub mod syntax;
pub mod types;

// At the cut — lowering to a schedule, and the static analysis that guards it.
pub mod check;
pub mod ir;

// After the cut — executing the schedule.
pub mod context;
pub mod domain;
pub mod trace;

// Substrate shared by all three: terms, unification, normalization.
pub mod complete;
pub mod normalize;
pub mod pattern;
pub mod term;

#[cfg(test)]
mod tests;

pub use complete::{Completeness, completeness};
pub use context::{Context, ContextTransition, Slot};
pub use domain::TypingDomain;
pub use ir::{Instr, Program, compile};
pub use normalize::{Normalizer, RewriteRule, unify_modulo};
pub use pattern::{Match, Pattern};
pub use syntax::render;
pub use term::{Evidence, Subst, Term};
pub use trace::{Step, Trace};
pub use types::{Atom, Key, TyExpr, Type, TypeExpr};

pub use rule::{Conclusion, Judgment, Premise, PremiseStatus, RuleParser, TypingRule};

pub use crate::semantics::runtime::TypingRuntime;
pub use crate::synth::Synthesizer as TypingSynth;
