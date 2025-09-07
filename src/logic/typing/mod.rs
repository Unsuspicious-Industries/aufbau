// Type system core definitions and re-exports

pub mod syntax; // parsing / pretty-printing of types
pub mod rule;   // typing rules (inference rule structures & parsing)

///---------------
/// Type Representation
///---------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ArraySize {
    // Unknown/dynamic size (e.g., T[])
    Dynamic,
    // Concrete constant size
    Const(u64),
    // Symbolic size variable to be resolved via binding (e.g., T[N])
    Var(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Base types
    Atom(String),
    // Function types (τ₁ → τ₂)
    Arrow(Box<Type>, Box<Type>),
    // N-ary function types: (τ1, …, τn) → τret
    Fn { params: Vec<Type>, ret: Box<Type> },
    // Pointer types (*τ) - for C-like languages
    Pointer(Box<Type>),
    // Array types (τ[n], τ[], or τ[N]) - for C-like languages
    Array(Box<Type>, ArraySize),
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
/// Type Operations and Unification
///---------------

impl Type {
    /// Check if this type is compatible with another type
    /// This implements basic subtyping and type compatibility rules
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        self.is_subtype_of(other)
    }
    
    /// Check if this type is a subtype of another type
    pub fn is_subtype_of(&self, other: &Type) -> bool {
        use Type::*;
        use ArraySize::*;
        
        match (self, other) {
            // Reflexivity: τ <: τ
            (a, b) if a == b => true,
            
            // Universe is a supertype of everything: τ <: ⊤
            (_, Universe) => true,
            
            // Empty is a subtype of everything: ∅ <: τ
            (Empty, _) => true,
            
            // Arrow types: (τ₁ → τ₂) <: (σ₁ → σ₂) if σ₁ <: τ₁ and τ₂ <: σ₂ (contravariant in argument, covariant in result)
            (Arrow(a1, a2), Arrow(b1, b2)) => {
                b1.is_subtype_of(a1) && a2.is_subtype_of(b2)
            }
            
            // N-ary function types: (τ⃗) → ρ <: (σ⃗) → ρ'  iff  ρ <: ρ'  and  ∀i. σi <: τi
            (Fn { params: a_ps, ret: a_r }, Fn { params: b_ps, ret: b_r }) => {
                if a_ps.len() != b_ps.len() { return false; }
                let ret_ok = a_r.is_subtype_of(b_r);
                let params_ok = a_ps.iter().zip(b_ps.iter()).all(|(a, b)| b.is_subtype_of(a));
                ret_ok && params_ok
            }
            
            // Pointer types: *τ <: *σ if τ <: σ (covariant)
            (Pointer(a), Pointer(b)) => {
                a.is_subtype_of(b)
            }
            
            // Array types: element-wise and size-wise
            (Array(a, n), Array(b, m)) => {
                let size_ok = match (n, m) {
                    (Dynamic, Dynamic) => true,
                    (Const(x), Const(y)) => x == y,
                    (Var(x), Var(y)) => x == y,
                    // supertype with variable size accepts any subtype size
                    (_, Var(_)) => true,
                    // everything else must match exactly
                    _ => false,
                };
                a.is_subtype_of(b) && size_ok
            }
            
            // Union types: τ <: (σ₁ ∨ σ₂) if τ <: σ₁ or τ <: σ₂
            (t, Union(u1, u2)) => {
                t.is_subtype_of(u1) || t.is_subtype_of(u2)
            }
            
            // Union types: (τ₁ ∨ τ₂) <: σ if τ₁ <: σ and τ₂ <: σ
            (Union(u1, u2), t) => {
                u1.is_subtype_of(t) && u2.is_subtype_of(t)
            }
            
            // Intersection types: (τ₁ ∧ τ₂) <: σ if τ₁ <: σ or τ₂ <: σ
            (Intersection(i1, i2), t) => {
                i1.is_subtype_of(t) || i2.is_subtype_of(t)
            }
            
            // Intersection types: τ <: (σ₁ ∧ σ₂) if τ <: σ₁ and τ <: σ₂
            (t, Intersection(i1, i2)) => {
                t.is_subtype_of(i1) && t.is_subtype_of(i2)
            }
            
            // Negation types: τ <: ¬σ if τ and σ are disjoint
            (t, Not(n)) => {
                !t.overlaps_with(n)
            }
            
            // Refinement types: {x: τ | P} <: σ if τ <: σ
            (Refinement { base, .. }, other) => {
                base.is_subtype_of(other)
            }
            
            // Refinement types: τ <: {x: σ | P} if τ <: σ and τ satisfies P (we assume P for now)
            (other, Refinement { base, .. }) => {
                other.is_subtype_of(base)
            }
            
            // No other subtyping relations
            _ => false,
        }
    }
    
    /// Check if two types overlap (have common values)
    pub fn overlaps_with(&self, other: &Type) -> bool {
        use Type::*;
        use ArraySize::*;
        
        match (self, other) {
            // Same types always overlap
            (a, b) if a == b => true,
            
            // Universe overlaps with everything except Empty
            (Universe, Empty) | (Empty, Universe) => false,
            (Universe, _) | (_, Universe) => true,
            
            // Empty overlaps with nothing
            (Empty, _) | (_, Empty) => false,
            
            // Union types: (τ₁ ∨ τ₂) overlaps σ if τ₁ overlaps σ or τ₂ overlaps σ
            (Union(u1, u2), t) | (t, Union(u1, u2)) => {
                u1.overlaps_with(t) || u2.overlaps_with(t)
            }
            
            // Intersection types: (τ₁ ∧ τ₂) overlaps σ if τ₁ overlaps σ and τ₂ overlaps σ
            (Intersection(i1, i2), t) | (t, Intersection(i1, i2)) => {
                i1.overlaps_with(t) && i2.overlaps_with(t)
            }
            
            // Arrow types overlap if they have the same structure
            (Arrow(a1, a2), Arrow(b1, b2)) => {
                a1.overlaps_with(b1) && a2.overlaps_with(b2)
            }
            
            // N-ary function types overlap pairwise
            (Fn { params: a_ps, ret: a_r }, Fn { params: b_ps, ret: b_r }) => {
                a_ps.len() == b_ps.len()
                    && a_ps.iter().zip(b_ps.iter()).all(|(a, b)| a.overlaps_with(b))
                    && a_r.overlaps_with(b_r)
            }
            
            // Pointer types overlap if their pointed-to types overlap
            (Pointer(a), Pointer(b)) => {
                a.overlaps_with(b)
            }
            
            // Array types overlap if their element types overlap and sizes match strictly
            (Array(a, n), Array(b, m)) => {
                let size_ok = match (n, m) {
                    (Dynamic, Dynamic) => true,
                    (Const(x), Const(y)) => x == y,
                    (Var(x), Var(y)) => x == y,
                    _ => false,
                };
                a.overlaps_with(b) && size_ok
            }
            
            // Negation: τ overlaps ¬σ if τ doesn't overlap σ
            (t, Not(n)) | (Not(n), t) => {
                !t.overlaps_with(n)
            }
            
            // Refinement types overlap if base types overlap
            (Refinement { base: b1, .. }, Refinement { base: b2, .. }) => {
                b1.overlaps_with(b2)
            }
            (Refinement { base, .. }, other) | (other, Refinement { base, .. }) => {
                base.overlaps_with(other)
            }
            
            // Different atom types don't overlap
            (Atom(a), Atom(b)) => a == b,
            
            // By default, assume different constructors don't overlap
            _ => false,
        }
    }
    
    /// Get the union of two types
    pub fn union_with(self, other: Type) -> Type {
        if self == other {
            self
        } else if self.is_subtype_of(&other) {
            other
        } else if other.is_subtype_of(&self) {
            self
        } else {
            Type::Union(Box::new(self), Box::new(other))
        }
    }
    
    /// Get the intersection of two types
    pub fn intersection_with(self, other: Type) -> Type {
        if self == other {
            self
        } else if self.is_subtype_of(&other) {
            self
        } else if other.is_subtype_of(&self) {
            other
        } else {
            Type::Intersection(Box::new(self), Box::new(other))
        }
    }
}


// Re-export frequently used items for external users of the module.
pub use syntax::{TypeSyntaxConfig, validate_type_expr};
pub use rule::{TypingRule, Premise, TypingJudgment, TypeSetting, Term, TypeAscription, Conclusion};

// Export ArraySize for external modules
pub use ArraySize as ArrayLen;

#[cfg(test)]
mod tests;

#[cfg(test)]
mod advanced_tests;

