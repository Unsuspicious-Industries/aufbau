// Type system core definitions and re-exports

pub mod core;
pub mod ops;
mod pool;
pub mod rule;
pub mod runtime;
pub mod state;
pub mod symbols;
pub mod syntax;

pub use core::Context;
pub use ops::{Unifier, UnifyResult, equal, subtype};
pub use pool::{SharedType, intern_type};
pub use symbols::{gather_raw_types, gather_type_symbols};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Meta(String),
    Raw(String),
    Arrow(Box<Type>, Box<Type>),
    Array(Box<Type>),
    Union(Vec<Type>),
    Not(Box<Type>),
    ContextCall(String, String),
    Any,
    None,
    Partial(Box<Type>, String),
    Path(TreePath),
    PathOf(Box<Type>, TreePath),
}

pub use rule::{
    Conclusion, Premise, Term, TypeAscription, TypeSetting, TypingJudgment, TypingRule,
};

use crate::logic::typing::core::TreePath;
