//! Type Operations
//!
//! All operations between types: substitution, unification, equality, and subtyping.

use crate::logic::typing::{Context, Type};

// =============================================================================
// Equality: τ₁ = τ₂
// =============================================================================

/// Structural equality check (no unification).
pub fn equal(t1: &Type, t2: &Type, ctx: &Context) -> bool {
    match (t1, t2) {
        (Type::Raw(a), Type::Raw(b)) => a == b,
        // Arrow types require structural equality
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => equal(l1, l2, ctx) && equal(r1, r2, ctx),
        (Type::Not(a), Type::Not(b)) => equal(a, b, ctx),
        // Context names are ignored during equality check
        (Type::ContextCall(_, v1), _) => match ctx.lookup(v1) {
            // inverse order to ensure context resolution
            Some(t3) => equal(t2, t3, ctx),
            None => false,
        },
        // Path comparison requires structural equality
        (Type::Path(p1), Type::Path(p2)) => p1 == p2,
        (Type::PathOf(t1_inner, p1), Type::PathOf(t2_inner, p2)) => {
            p1
             == p2 && equal(t1_inner, t2_inner, ctx)
        },
        // funny
        (_, Type::Any) => true,
        (Type::Any, _) => true,
        // hehehe
        (_, Type::None) => false,
        (Type::None, _) => false,

        // Default: types are not equal
        _ => false,
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
pub fn subtype(t1: &Type, t2: &Type, ctx: &Context) -> bool {
    // ∅ ⊆ τ
    if matches!(t1, Type::None) {
        return true;
    }

    // τ ⊆ ⊤
    if matches!(t2, Type::Any) {
        return true;
    }

    // Reflexivity: τ ⊆ τ
    if equal(&t1, &t2, ctx) {
        return true;
    }

    // Structural subtyping
    match (&t1, &t2) {
        // Arrow: contravariant in domain, covariant in codomain
        (Type::Arrow(d1, c1), Type::Arrow(d2, c2)) => subtype(d2, d1, ctx) && subtype(c1, c2, ctx),

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
