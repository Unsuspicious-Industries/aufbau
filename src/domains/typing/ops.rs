//! Type Operations — unification, equality, and subtyping on closed types.
//!
//! After the TypeExpr/Type separation (matching §3 of the draft), the unifier
//! operates on closed `Type` values only.  Meta variables, context calls, and
//! `typeof` references are handled in `TypeExpr` evaluation (`eval_type_expr`
//! in `domain.rs`) and never reach this module.
//!
//! The unifier is purely functional (no mutation needed) since no `Meta`
//! variants remain in `Type`.

use crate::domains::typing::Type;

// =============================================================================
// UnifyResult
// =============================================================================

/// Result of a unification attempt
#[derive(Debug, Clone, PartialEq)]
pub enum UnifyResult {
    Ok,
    Indeterminate,
    Fail(String),
}

impl UnifyResult {
    pub fn is_ok(&self) -> bool {
        matches!(self, UnifyResult::Ok)
    }
    pub fn is_fail(&self) -> bool {
        matches!(self, UnifyResult::Fail(_))
    }
    pub fn is_indeterminate(&self) -> bool {
        matches!(self, UnifyResult::Indeterminate)
    }
}

// =============================================================================
// Unifier — purely functional, closed types only
// =============================================================================

/// Purely functional unifier for closed types.
///
/// After compilation, no `Type::Meta` variants remain.  The unifier operates
/// statelessly on closed-type pairs.  It can stack-allocate fresh metas only
/// if needed (currently not needed — all metas are compiled away).
#[derive(Debug, Clone, Default)]
pub struct Unifier;

impl Unifier {
    pub fn new() -> Self {
        Self
    }

    /// Unify two closed types.  Three-valued result:
    /// - Ok: types are structurally equal
    /// - Indeterminate: can't decide (involves Any, Partial)
    /// - Fail: types are definitively incompatible
    pub fn unify(t1: &Type, t2: &Type) -> UnifyResult {
        match (t1, t2) {
            // Identical constructors
            (Type::Raw(a), Type::Raw(b)) => {
                if a == b {
                    UnifyResult::Ok
                } else {
                    UnifyResult::Fail(format!("'{}' ≠ '{}'", a, b))
                }
            }

            (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => match Self::unify(l1, l2) {
                UnifyResult::Ok => Self::unify(r1, r2),
                UnifyResult::Indeterminate => match Self::unify(r1, r2) {
                    UnifyResult::Fail(e) => UnifyResult::Fail(e),
                    _ => UnifyResult::Indeterminate,
                },
                fail => fail,
            },

            (Type::Union(a), Type::Union(b)) => {
                if a.len() != b.len() {
                    return UnifyResult::Fail(format!(
                        "Union arity mismatch: {} vs {}",
                        a.len(),
                        b.len()
                    ));
                }
                let mut saw_indeterminate = false;
                for (l, r) in a.iter().zip(b.iter()) {
                    match Self::unify(l, r) {
                        UnifyResult::Ok => {}
                        UnifyResult::Indeterminate => saw_indeterminate = true,
                        fail => return fail,
                    }
                }
                if saw_indeterminate {
                    UnifyResult::Indeterminate
                } else {
                    UnifyResult::Ok
                }
            }

            (Type::Not(a), Type::Not(b)) => Self::unify(a, b),

            // Any = Any: ok
            (Type::Any, Type::Any) => UnifyResult::Ok,

            // Any vs concrete: indeterminate
            (Type::Any, _) | (_, Type::Any) => UnifyResult::Indeterminate,

            // None = None: ok
            (Type::None, Type::None) => UnifyResult::Ok,

            // None vs non-None: fail
            (Type::None, _) | (_, Type::None) => {
                UnifyResult::Fail("⊥ is not unifiable with non-⊥".to_string())
            }

            // Partial: unwrap and unify inner
            (Type::Partial(t, _), other) | (other, Type::Partial(t, _)) => Self::unify(t, other),

            // Union vs non-union: fail
            (Type::Union(_), _) | (_, Type::Union(_)) => {
                UnifyResult::Fail(format!("Cannot unify {} with {}", t1, t2))
            }

            // Structural mismatch
            _ => UnifyResult::Fail(format!("Cannot unify {} with {}", t1, t2)),
        }
    }
}

// =============================================================================
// Equality: τ₁ = τ₂
// =============================================================================

/// Structural equality on closed types.
/// Returns `None` when equality depends on unknown information (Any).
pub fn equal(t1: &Type, t2: &Type) -> Option<bool> {
    match (t1, t2) {
        (Type::Raw(a), Type::Raw(b)) => Some(a == b),
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => Some(equal(l1, l2)? && equal(r1, r2)?),
        (Type::Union(a), Type::Union(b)) => {
            if a.len() != b.len() {
                return Some(false);
            }
            let mut all = true;
            for (x, y) in a.iter().zip(b.iter()) {
                all = all && equal(x, y)?;
            }
            Some(all)
        }
        (Type::Not(a), Type::Not(b)) => equal(a, b),
        (Type::Any, Type::Any) => Some(true),
        (Type::Any, _) | (_, Type::Any) => None,
        (Type::None, Type::None) => Some(true),
        (Type::None, _) | (_, Type::None) => Some(false),
        (Type::Partial(a, _), other) | (other, Type::Partial(a, _)) => equal(a, other),
        _ => Some(false),
    }
}

// =============================================================================
// Subtyping: τ₁ ⊆ τ₂
// =============================================================================

/// Check if τ₁ is a subtype of τ₂.
///
/// Rules:
/// -  ⊥ ⊆ τ   (None is subtype of everything)
/// -  τ ⊆ ⊤   (Everything is subtype of Any)
/// -  τ ⊆ τ   (Reflexivity)
/// -  τ₁ → τ₂  ⊆  σ₁ → σ₂  iff  σ₁ ⊆ τ₁ ∧ τ₂ ⊆ σ₂  (contravariant domain)
pub fn subtype(t1: &Type, t2: &Type) -> bool {
    if matches!(t1, Type::None) || matches!(t2, Type::Any) {
        return true;
    }
    if let Some(true) = equal(t1, t2) {
        return true;
    }
    match (t1, t2) {
        (Type::Arrow(d1, c1), Type::Arrow(d2, c2)) => subtype(d2, d1) && subtype(c1, c2),
        (Type::Union(parts), other) => parts.iter().all(|p| subtype(p, other)),
        (other, Type::Union(parts)) => parts.iter().any(|p| subtype(other, p)),
        (Type::Partial(a, _), other) => subtype(a, other),
        (other, Type::Partial(a, _)) => subtype(other, a),
        _ => false,
    }
}

// =============================================================================
// Tests
// =============================================================================

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(t: &str) -> Type {
        Type::parse(t).expect("type should parse")
    }

    #[test]
    fn unify_raw_same() {
        assert!(Unifier::unify(&parse("'Int'"), &parse("'Int'")).is_ok());
    }

    #[test]
    fn unify_raw_different() {
        assert!(Unifier::unify(&parse("'Int'"), &parse("'Bool'")).is_fail());
    }

    #[test]
    fn unify_arrow_same() {
        let t = parse("'Int' -> 'Bool'");
        assert!(Unifier::unify(&t, &t).is_ok());
    }

    #[test]
    fn unify_any_indeterminate() {
        assert!(Unifier::unify(&Type::Any, &parse("'Int'")).is_indeterminate());
    }

    #[test]
    fn unify_none_vs_non_none_fails() {
        assert!(Unifier::unify(&Type::None, &parse("'Int'")).is_fail());
    }

    #[test]
    fn unify_partial_unwraps() {
        let partial = Type::Partial(Box::new(parse("'Int'")), "'Int'".into());
        assert!(Unifier::unify(&partial, &parse("'Int'")).is_ok());
    }

    #[test]
    fn unify_union_same_arity() {
        let t = parse("'Int' | 'Bool'");
        assert!(Unifier::unify(&t, &t).is_ok());
    }

    #[test]
    fn unify_union_different_arity_fails() {
        let a = parse("'Int' | 'Bool'");
        let b = parse("'Int' | 'Bool' | 'String'");
        assert!(Unifier::unify(&a, &b).is_fail());
    }

    #[test]
    fn equal_partial_delegates_to_inner() {
        let p = Type::Partial(Box::new(parse("'Int'")), "x".into());
        assert_eq!(equal(&p, &parse("'Int'")), Some(true));
    }

    #[test]
    fn subtype_none_is_bottom() {
        assert!(subtype(&Type::None, &parse("'Int'")));
    }

    #[test]
    fn subtype_everything_is_top() {
        assert!(subtype(&parse("'Int'"), &Type::Any));
    }

    #[test]
    fn subtype_reflexive() {
        let t = parse("'Int'");
        assert!(subtype(&t, &t));
    }

    #[test]
    fn subtype_arrow_contravariant() {
        // (Any → Int) ⊆ (Int → Any)  because Int ⊆ Any (codomain) and Any ⊆ Int is false...
        // Actually: σ₁ ⊆ τ₁ means Any ⊆ Int? No. So this should be false.
        let arrow1 = Type::Arrow(Box::new(Type::Any), Box::new(Type::Raw("Int".into())));
        let arrow2 = Type::Arrow(Box::new(Type::Raw("Int".into())), Box::new(Type::Any));
        // subtype(arrow1, arrow2) iff subtype(Int, Any) && subtype(Any, Int)
        // = subtype(domain2, domain1) && subtype(codomain1, codomain2)
        // domain: subtype(Int, Any) = true
        // codomain: subtype(Int, Any) = true
        // So this IS true
        assert!(subtype(&arrow1, &arrow2));
    }

    #[test]
    fn subtype_member_into_union() {
        let int_t = parse("'Int'");
        let union_t = parse("'Int' | 'Bool'");
        assert!(subtype(&int_t, &union_t));
    }

    #[test]
    fn subtype_union_not_into_single_member() {
        let union_t = parse("'Int' | 'Bool'");
        let int_t = parse("'Int'");
        assert!(!subtype(&union_t, &int_t));
    }
}
