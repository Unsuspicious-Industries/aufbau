// Type system core definitions and re-exports

pub mod binding;
pub mod core;
pub mod eval;
pub mod ops;
pub mod rule;
pub mod symbols;
pub mod syntax;
pub mod tree;

pub use tree::*;

pub use core::{Context, TreeRef};
pub use eval::evaluate_typing;
pub use ops::{Unifier, UnifyResult, equal, subtype};
pub use symbols::{gather_raw_types, gather_terminal_nodes, gather_terminals, gather_type_symbols};
///---------------
/// Type Representation
///---------------

#[derive(Debug, Clone)]
pub enum Type {
    // Base types as bindable vars
    Atom(String),
    // Meta type variables for inference (e.g. ?A, ?B in typing rules)
    Meta(String),
    // Raw/concrete types (e.g., 'int', 'string') - literal types that don't need variable resolution
    Raw(String),
    // Function types (τ₁ → τ₂)
    Arrow(Box<Type>, Box<Type>),
    // Union types (τ₁ | τ₂ | ...)
    Union(Vec<Type>),
    // Negation type (¬τ) - "anything that is not τ"
    Not(Box<Type>),
    // Context call (Γ(x)) - lookup the type of variable x in context Γ
    ContextCall(String, String), // (context_name, variable_name)
    // Any type - accepts all inputs (⊤, the universal type)
    Any,
    // None type - rejects all inputs (∅, the empty type)
    None,

    // partial expression: parsed type-so-far plus original input string for continuation analysis
    Partial(Box<Type>, String),

    // "Fake" type for binding resolution
    Path(TreePath), // Absolute path to a binding location
    // path of the node at this location
    // type is a pattern
    PathOf(Box<Type>, TreePath),
}

// Re-export frequently used items for external users of the module.
pub use rule::{
    Conclusion, Premise, Term, TypeAscription, TypeSetting, TypingJudgment, TypingRule,
};

use crate::logic::typing::core::TreePath;

#[cfg(test)]
mod tests;
