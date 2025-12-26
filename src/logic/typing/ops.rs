//! Type Operations
//!
//! All operations between types: substitution, unification, equality, and subtyping.

use crate::logic::typing::Type;
use std::collections::{HashMap, HashSet};

// =============================================================================
// Substitution: σ : MetaVar → Type
// =============================================================================

/// A substitution maps type variable names to types.
pub type Substitution = HashMap<String, Type>;

/// Apply substitution σ to type τ, returning σ(τ).
/// Handles cycle detection to prevent infinite loops.
pub fn apply(ty: &Type, sigma: &Substitution) -> Type {
    let mut visited = HashSet::new();
    apply_inner(ty, sigma, &mut visited)
}

fn apply_inner(ty: &Type, sigma: &Substitution, visited: &mut HashSet<String>) -> Type {
    match ty {
        // Atom variables: look up in substitution
        Type::Atom(name) => {
            if let Some(replacement) = sigma.get(name) {
                if visited.contains(name) {
                    return Type::Universe; // Cycle detected
                }
                visited.insert(name.clone());
                let result = apply_inner(replacement, sigma, visited);
                visited.remove(name);
                result
            } else {
                Type::Atom(name.clone())
            }
        }

        // Meta variables (?A): stored with ? prefix in substitution
        Type::Meta(name) => {
            let key = format!("?{}", name);
            if let Some(replacement) = sigma.get(&key) {
                if visited.contains(&key) {
                    return Type::Universe;
                }
                visited.insert(key.clone());
                let result = apply_inner(replacement, sigma, visited);
                visited.remove(&key);
                result
            } else if let Some(replacement) = sigma.get(name) {
                // Fallback: try without ? prefix
                if visited.contains(name) {
                    return Type::Universe;
                }
                visited.insert(name.clone());
                let result = apply_inner(replacement, sigma, visited);
                visited.remove(name);
                result
            } else {
                Type::Meta(name.clone())
            }
        }

        // Binary type constructors: recurse on both sides
        Type::Arrow(l, r) => Type::Arrow(
            Box::new(apply_inner(l, sigma, visited)),
            Box::new(apply_inner(r, sigma, visited)),
        ),

        // Unary type constructor
        Type::Not(t) => Type::Not(Box::new(apply_inner(t, sigma, visited))),

        // Ground types: no substitution needed
        Type::Raw(_) | Type::Tuple(_) | Type::ContextCall(_, _) | Type::Universe | Type::Empty => {
            ty.clone()
        }
    }
}

// =============================================================================
// Unification: τ₁ ≐ τ₂
// =============================================================================

/// Unify two types, extending the substitution.
/// Returns `true` if unification succeeds, `false` otherwise.
///
/// Unification finds a substitution σ such that σ(τ₁) = σ(τ₂).
pub fn unify(t1: &Type, t2: &Type, sigma: &mut Substitution) -> bool {
    // Apply current substitution first
    let t1 = apply(t1, sigma);
    let t2 = apply(t2, sigma);

    // Already equal after substitution
    if t1 == t2 {
        return true;
    }

    // Raw ↔ Atom with same name (nominal/structural bridge)
    if let (Type::Raw(n1), Type::Atom(n2)) | (Type::Atom(n1), Type::Raw(n2)) = (&t1, &t2) {
        if n1 == n2 {
            return true;
        }
    }

    // Meta variable binding
    if let Some(result) = try_bind_meta(&t1, &t2, sigma) {
        return result;
    }
    if let Some(result) = try_bind_meta(&t2, &t1, sigma) {
        return result;
    }

    // Legacy: Atom starting with ? as inference variable
    if let Some(result) = try_bind_inference_var(&t1, &t2, sigma) {
        return result;
    }
    if let Some(result) = try_bind_inference_var(&t2, &t1, sigma) {
        return result;
    }

    // Structural unification
    match (&t1, &t2) {
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => unify(l1, l2, sigma) && unify(r1, r2, sigma),
        (Type::Not(t1), Type::Not(t2)) => unify(t1, t2, sigma),

        // Universe is top type: unifies with anything
        (Type::Universe, _) | (_, Type::Universe) => true,

        // No unification possible
        _ => false,
    }
}

/// Try to bind a Meta variable to a type.
fn try_bind_meta(candidate: &Type, target: &Type, sigma: &mut Substitution) -> Option<bool> {
    if let Type::Meta(name) = candidate {
        let key = format!("?{}", name);
        if occurs_meta(name, target) {
            return Some(false); // Occurs check failed
        }
        sigma.insert(key, target.clone());
        return Some(true);
    }
    None
}

/// Try to bind a legacy inference variable (?X as Atom) to a type.
fn try_bind_inference_var(
    candidate: &Type,
    target: &Type,
    sigma: &mut Substitution,
) -> Option<bool> {
    if let Type::Atom(name) = candidate {
        if name.starts_with('?') {
            if occurs_atom(name, target) {
                return Some(false); // Occurs check failed
            }
            sigma.insert(name.clone(), target.clone());
            return Some(true);
        }
    }
    None
}

// =============================================================================
// Equality: τ₁ = τ₂
// =============================================================================

/// Check if two types are equal (modulo substitution).
/// This is unification without extending the substitution.
pub fn equal(t1: &Type, t2: &Type, sigma: &Substitution) -> bool {
    let t1 = apply(t1, sigma);
    let t2 = apply(t2, sigma);
    types_equal(&t1, &t2)
}

/// Structural equality check (no unification).
fn types_equal(t1: &Type, t2: &Type) -> bool {
    match (t1, t2) {
        (Type::Atom(a), Type::Atom(b)) => a == b,
        (Type::Raw(a), Type::Raw(b)) => a == b,
        (Type::Meta(a), Type::Meta(b)) => a == b,
        (Type::Tuple(a), Type::Tuple(b)) => a == b,
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => types_equal(l1, l2) && types_equal(r1, r2),
        (Type::Not(a), Type::Not(b)) => types_equal(a, b),
        (Type::ContextCall(c1, v1), Type::ContextCall(c2, v2)) => c1 == c2 && v1 == v2,
        (Type::Universe, Type::Universe) => true,
        (Type::Empty, Type::Empty) => true,
        // Raw and Atom with same name are considered equal
        (Type::Raw(a), Type::Atom(b)) | (Type::Atom(a), Type::Raw(b)) => a == b,
        _ => false,
    }
}

// =============================================================================
// Subtyping: τ₁ ⊆ τ₂
// =============================================================================

/// Check if τ₁ is a subtype of τ₂.
///
/// Subtyping rules:
/// - ∅ ⊆ τ  (Empty is subtype of everything)
/// - τ ⊆ ⊤  (Everything is subtype of Universe)
/// - τ ⊆ τ  (Reflexivity)
/// - Structural equality implies subtyping
///
/// For function types: τ₁ → τ₂ ⊆ σ₁ → σ₂ iff σ₁ ⊆ τ₁ and τ₂ ⊆ σ₂ (contravariant in domain)
pub fn subtype(t1: &Type, t2: &Type, sigma: &Substitution) -> bool {
    let t1 = apply(t1, sigma);
    let t2 = apply(t2, sigma);

    // ∅ ⊆ τ
    if matches!(t1, Type::Empty) {
        return true;
    }

    // τ ⊆ ⊤
    if matches!(t2, Type::Universe) {
        return true;
    }

    // Reflexivity: τ ⊆ τ
    if types_equal(&t1, &t2) {
        return true;
    }

    // Structural subtyping
    match (&t1, &t2) {
        // Arrow: contravariant in domain, covariant in codomain
        (Type::Arrow(d1, c1), Type::Arrow(d2, c2)) => {
            subtype(d2, d1, sigma) && subtype(c1, c2, sigma)
        }

        _ => false,
    }
}

// =============================================================================
// Occurs Check
// =============================================================================

/// Check if a Meta variable name occurs in a type.
fn occurs_meta(name: &str, ty: &Type) -> bool {
    match ty {
        Type::Meta(n) => n == name,
        Type::Atom(n) => n == &format!("?{}", name), // Legacy check
        Type::Arrow(l, r) => occurs_meta(name, l) || occurs_meta(name, r),
        Type::Not(t) => occurs_meta(name, t),
        _ => false,
    }
}

/// Check if an Atom variable name occurs in a type.
fn occurs_atom(name: &str, ty: &Type) -> bool {
    match ty {
        Type::Atom(n) => n == name,
        Type::Meta(n) => format!("?{}", n) == name,
        Type::Arrow(l, r) => occurs_atom(name, l) || occurs_atom(name, r),
        Type::Not(t) => occurs_atom(name, t),
        _ => false,
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_apply_substitution() {
        let mut sigma = Substitution::new();
        sigma.insert("?A".into(), Type::Atom("Int".into()));

        let ty = Type::Arrow(
            Box::new(Type::Atom("?A".into())),
            Box::new(Type::Atom("?A".into())),
        );

        let result = apply(&ty, &sigma);
        let expected = Type::Arrow(
            Box::new(Type::Atom("Int".into())),
            Box::new(Type::Atom("Int".into())),
        );

        assert_eq!(result, expected);
    }

    #[test]
    fn test_apply_meta_substitution() {
        let mut sigma = Substitution::new();
        sigma.insert("?A".into(), Type::Atom("Bool".into()));

        let ty = Type::Meta("A".into());
        let result = apply(&ty, &sigma);

        assert_eq!(result, Type::Atom("Bool".into()));
    }

    #[test]
    fn test_unify_simple() {
        let mut sigma = Substitution::new();
        let t1 = Type::Atom("?A".into());
        let t2 = Type::Atom("Int".into());

        assert!(unify(&t1, &t2, &mut sigma));
        assert_eq!(sigma.get("?A"), Some(&Type::Atom("Int".into())));
    }

    #[test]
    fn test_unify_meta() {
        let mut sigma = Substitution::new();
        let t1 = Type::Meta("X".into());
        let t2 = Type::Atom("String".into());

        assert!(unify(&t1, &t2, &mut sigma));
        assert_eq!(sigma.get("?X"), Some(&Type::Atom("String".into())));
    }

    #[test]
    fn test_unify_arrow() {
        let mut sigma = Substitution::new();
        let t1 = Type::Arrow(
            Box::new(Type::Meta("A".into())),
            Box::new(Type::Meta("B".into())),
        );
        let t2 = Type::Arrow(
            Box::new(Type::Atom("Int".into())),
            Box::new(Type::Atom("Bool".into())),
        );

        assert!(unify(&t1, &t2, &mut sigma));
        assert_eq!(sigma.get("?A"), Some(&Type::Atom("Int".into())));
        assert_eq!(sigma.get("?B"), Some(&Type::Atom("Bool".into())));
    }

    #[test]
    fn test_unify_occurs_check() {
        let mut sigma = Substitution::new();
        let t1 = Type::Meta("A".into());
        let t2 = Type::Arrow(
            Box::new(Type::Meta("A".into())),
            Box::new(Type::Atom("Int".into())),
        );

        // ?A = ?A → Int would create infinite type
        assert!(!unify(&t1, &t2, &mut sigma));
    }

    #[test]
    fn test_unify_universe() {
        let mut sigma = Substitution::new();
        assert!(unify(&Type::Universe, &Type::Atom("Int".into()), &mut sigma));
        assert!(unify(&Type::Atom("Bool".into()), &Type::Universe, &mut sigma));
    }

    #[test]
    fn test_subtype_empty() {
        let sigma = Substitution::new();
        assert!(subtype(&Type::Empty, &Type::Atom("Int".into()), &sigma));
        assert!(subtype(&Type::Empty, &Type::Universe, &sigma));
    }

    #[test]
    fn test_subtype_universe() {
        let sigma = Substitution::new();
        assert!(subtype(&Type::Atom("Int".into()), &Type::Universe, &sigma));
        assert!(subtype(&Type::Empty, &Type::Universe, &sigma));
    }

    #[test]
    fn test_subtype_reflexive() {
        let sigma = Substitution::new();
        let ty = Type::Arrow(
            Box::new(Type::Atom("Int".into())),
            Box::new(Type::Atom("Bool".into())),
        );
        assert!(subtype(&ty, &ty, &sigma));
    }

    #[test]
    fn test_subtype_arrow_contravariant() {
        let sigma = Substitution::new();

        // (⊤ → Int) ⊆ (Bool → Int)
        // Because Bool ⊆ ⊤ (contravariant in domain)
        let t1 = Type::Arrow(
            Box::new(Type::Universe),
            Box::new(Type::Atom("Int".into())),
        );
        let t2 = Type::Arrow(
            Box::new(Type::Atom("Bool".into())),
            Box::new(Type::Atom("Int".into())),
        );

        assert!(subtype(&t1, &t2, &sigma));
    }

    #[test]
    fn test_equal_after_substitution() {
        let mut sigma = Substitution::new();
        sigma.insert("?A".into(), Type::Atom("Int".into()));

        let t1 = Type::Atom("?A".into());
        let t2 = Type::Atom("Int".into());

        assert!(equal(&t1, &t2, &sigma));
    }
}

