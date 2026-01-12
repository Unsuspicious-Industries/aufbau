//! Type Operations
//!
//! All operations between types: substitution, unification, equality, and subtyping.

use crate::logic::typing::{Context, Type};

pub fn is_unresolved(ty: &Type) -> bool {
    // return true for Path and pathof
    match ty {
        Type::Path(_) => true,
        Type::PathOf(_, _) => true,
        Type::ContextCall(_, _) => true,
        _ => false,
    }
}

// =============================================================================
// Equality: τ₁ = τ₂
// =============================================================================

/// Structural equality check (no unification).
///
/// This is intentionally **partial**: it returns `None` if checking equality would
/// require resolving information from the typing context (`ContextCall`) or from
/// unresolved tree paths (`Path`, `PathOf`).
pub fn equal(t1: &Type, t2: &Type) -> Option<bool> {
    match (t1, t2) {
        (Type::Raw(a), Type::Raw(b)) => Some(a == b),
        // Arrow types require structural equality
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
            Some(equal(l1, l2)? && equal(r1, r2)?)
        }
        (Type::Not(a), Type::Not(b)) => equal(a, b),

        // Any context-dependent equality is handled by the evaluator.
        // Returning None here makes the equality relation partial.
        (Type::ContextCall(_, _), _) | (_, Type::ContextCall(_, _)) => None,

        // Path-based types are placeholders for unresolved bindings and must be
        // resolved before equality can be decided.
        (Type::Path(_), _) | (_, Type::Path(_)) => None,
        (Type::PathOf(_, _), _) | (_, Type::PathOf(_, _)) => None,

        // funny
        (_, Type::Any) => Some(true),
        (Type::Any, _) => Some(true),
        // hehehe
        (_, Type::None) => Some(false),
        (Type::None, _) => Some(false),

        // Default: types are not equal
        _ => Some(false),
    }
}

// =============================================================================
// Subtyping: τ₁ ⊆ τ₂
// =============================================================================

/// Check if τ₁ is a subtype of τ₂.
///
/// Subtyping rules:
/// - ∅ ⊆ τ  (None is subtype of everything - rejects all, so compatible with any constraint)
/// - τ ⊆ ⊤  (Everything is subtype of Any - any constraint is satisfied by no constraint)
/// - τ ⊆ τ  (Reflexivity)
/// - Structural equality implies subtyping
///
/// For function types: τ₁ → τ₂ ⊆ σ₁ → σ₂ iff σ₁ ⊆ τ₁ and τ₂ ⊆ σ₂ (contravariant in domain)
pub fn subtype(t1: &Type, t2: &Type) -> bool {
    // ∅ ⊆ τ
    if matches!(t1, Type::None) {
        return true;
    }

    // τ ⊆ ⊤
    if matches!(t2, Type::Any) {
        return true;
    }

    // Reflexivity: τ ⊆ τ
    if let Some(true) = equal(&t1,   &t2) {
        return true;
    }

    // Structural subtyping
    match (&t1, &t2) {
        // Arrow: contravariant in domain, covariant in codomain
        (Type::Arrow(d1, c1), Type::Arrow(d2, c2)) => subtype(d2, d1) && subtype(c1, c2),

        _ => false,
    }
}

// =============================================================================
// Occurs Check
// =============================================================================

/// Check if an Atom variable name occurs in a type.
#[allow(dead_code)]
fn occurs_atom(name: &str, ty: &Type) -> bool {
    match ty {
        Type::Atom(n) => n == name,
        Type::Arrow(l, r) => occurs_atom(name, l) || occurs_atom(name, r),
        Type::Not(t) => occurs_atom(name, t),
        _ => false,
    }
}
