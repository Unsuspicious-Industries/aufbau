// Type system core definitions and re-exports

pub mod context;
pub mod obligation;
pub mod ops;
mod pool;
pub mod rule;
pub mod runtime;
pub mod types;
pub mod symbols;

#[cfg(test)]
mod tests;

pub use crate::logic::TypingRuntime;
pub use context::{Context, ContextEdit, ContextTransition, TreeStatus};
pub use obligation::{Obligation, Obligations};
pub use ops::{Unifier, UnifyResult, equal, subtype};
pub use pool::{SharedType, intern_type};
pub use symbols::{gather_raw_types, gather_type_symbols};
pub use types::Type;

pub use rule::{
    Conclusion, Premise, Term, TypeAscription, TypeSetting, TypingJudgment, TypingRule,
};
