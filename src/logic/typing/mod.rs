// Type system core definitions and re-exports

pub mod rule;
pub mod syntax; // parsing / pretty-printing of types // typing rules (inference rule structures & parsing)

///---------------
/// Type Representation
///---------------

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Base types
    Atom(String),
    // Raw/concrete types (e.g., 'int', 'string') - literal types that don't need variable resolution
    Raw(String),
    // Function types (τ₁ → τ₂)
    Arrow(Box<Type>, Box<Type>),
    // Tuple
    Tuple(String),
    // Pointer types (*τ) - for C-like languages
    Pointer(Box<Type>),
    // Array types (τ[n], τ[], or τ[N]) - for C-like languages
    Array(Box<Type>, String),
    // Negation type (¬τ) - "anything that is not τ"
    Not(Box<Type>),
    // Intersection (τ₁ ∧ τ₂) - "both τ₁ and τ₂"
    Intersection(Box<Type>, Box<Type>),
    // Union (τ₁ ∨ τ₂) - "either τ₁ or τ₂"
    Union(Box<Type>, Box<Type>),
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
