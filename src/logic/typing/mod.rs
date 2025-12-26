// Type system core definitions and re-exports

pub mod binding;
pub mod core;
pub mod eval;
pub mod ops;
pub mod rule;
pub mod symbols;
pub mod syntax;

pub use core::Context;
pub use eval::evaluate_typing;
pub use ops::{Substitution, apply, equal, subtype, unify};
pub use symbols::{gather_raw_types, gather_terminal_nodes, gather_terminals, gather_type_symbols};

///---------------
/// Type Representation
///---------------

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Base types as bindable vars
    Atom(String),
    // Raw/concrete types (e.g., 'int', 'string') - literal types that don't need variable resolution
    Raw(String),
    // Meta varibable ?A, ?B from type rules
    Meta(String),
    // Function types (τ₁ → τ₂)
    Arrow(Box<Type>, Box<Type>),
    // Tuple
    Tuple(String),
    // Negation type (¬τ) - "anything that is not τ"
    Not(Box<Type>),
    // Context call (Γ(x)) - lookup the type of variable x in context Γ
    ContextCall(String, String), // (context_name, variable_name)
    // The universe of all types (needed for negation to make sense)
    Universe,
    // Empty type (∅)
    Empty,
}

// Re-export frequently used items for external users of the module.
pub use rule::{
    Conclusion, Premise, Term, TypeAscription, TypeSetting, TypingJudgment, TypingRule,
};
pub use syntax::{TypeSyntaxConfig, validate_type_expr};

#[cfg(test)]
mod tests;
