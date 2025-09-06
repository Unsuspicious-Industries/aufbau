// Type system core definitions and re-exports

pub mod syntax; // parsing / pretty-printing of types
pub mod rule;   // typing rules (inference rule structures & parsing)

///---------------
/// Type Representation
///---------------

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Base types
    Atom(String),
    // Function types (τ₁ → τ₂)
    Arrow(Box<Type>, Box<Type>),
    // Negation type (¬τ) - "anything that is not τ"
    Not(Box<Type>),
    // Intersection (τ₁ ∧ τ₂) - "both τ₁ and τ₂"
    Intersection(Box<Type>, Box<Type>),
    // Union (τ₁ ∨ τ₂) - "either τ₁ or τ₂"  
    Union(Box<Type>, Box<Type>),
    // Refinement/subset
    Refinement { base: Box<Type>, predicate: String },
    // The universe of all types (needed for negation to make sense)
    Universe,
    // Empty type (∅)
    Empty,
}

///---------------
/// Type Unification
///---------------

// Removed unused import


// Re-export frequently used items for external users of the module.
pub use syntax::{TypeSyntaxConfig, validate_type_expr};
pub use rule::{TypingRule, Premise, TypingJudgment, TypeSetting, Term, TypeAscription, Conclusion};

#[cfg(test)]
mod tests;

