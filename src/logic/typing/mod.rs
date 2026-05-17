// Type system core definitions and re-exports

pub mod context;
pub mod ops;
mod pool;
pub mod rule;
pub mod runtime;
pub mod types;

#[cfg(test)]
mod tests;

pub use crate::logic::{Obligation, Obligations, SemanticRuntime, SemanticSummary};
pub use context::{Context, ContextTransition, TreeStatus};
pub use ops::{Unifier, UnifyResult, equal, subtype};
pub use pool::{SharedType, intern_type};
pub use types::Type;

pub use rule::{
    Conclusion, Premise, Term, TypeAscription, TypeSetting, TypingJudgment, TypingRule,
};
