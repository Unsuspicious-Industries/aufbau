//! Type Operations
//!
//! All operations between types: substitution, unification, equality, and subtyping.
//!
//! The Unifier provides proper Hindley-Milner style unification following the
//! formal spec in §1.7, replacing the ad-hoc set_meta/solve_meta system.

use crate::logic::typing::core::Context;
use crate::logic::typing::Type;
use std::collections::HashMap;

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
// Unifier: σ : MetaVar → Type
// =============================================================================

/// Result of a unification attempt
#[derive(Debug, Clone, PartialEq)]
pub enum UnifyResult {
    /// Unification succeeded, substitution updated
    Ok,
    /// Cannot determine yet (unresolved paths, context calls, Any involved)
    Indeterminate,
    /// Unification definitely failed (structural mismatch)
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

/// Proper unification engine following §1.7 of the formal spec.
///
/// Manages a substitution map σ: MetaVar → Type and provides:
/// - `unify(τ₁, τ₂)`: attempt to make τ₁ and τ₂ equal under σ
/// - `apply(τ)`: substitute all bound meta variables in τ
/// - `resolve(name)`: look up a meta variable binding
///
/// Invariants maintained:
/// - Occurs check: ?X ∉ FV(τ) before binding ?X := τ
/// - Idempotent substitution: apply(apply(τ)) = apply(τ)
/// - Any is treated as indeterminate (not unified with concrete types)
#[derive(Debug, Clone, Default)]
pub struct Unifier {
    /// The substitution map: meta variable name → type
    pub substitution: HashMap<String, Type>,
    /// Current typing context for resolving context calls
    pub context: Option<Context>,
    /// Resolved binding names for context calls (binding name -> current text).
    /// This lets Γ(name) resolve to Γ(x) once `name` is bound to `x` in the tree,
    /// without changing any typing rules.
    pub binding_values: HashMap<String, String>,
}

impl Unifier {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create from an existing map (for backward compatibility during migration)
    pub fn from_map(map: HashMap<String, Type>) -> Self {
        Self {
            substitution: map,
            context: None,
            binding_values: HashMap::new(),
        }
    }

    pub fn set_context(&mut self, ctx: &Context) {
        self.context = Some(ctx.clone());
    }

    pub fn clear_context(&mut self) {
        self.context = None;
    }

    /// Update binding name resolution from the current tree.
    /// This keeps context calls like Γ(name) synced to the latest bound name text.
    pub fn set_binding_values(&mut self, values: HashMap<String, String>) {
        self.binding_values = values;
    }

    /// Export the underlying map (for backward compatibility)
    pub fn as_map(&self) -> &HashMap<String, Type> {
        &self.substitution
    }

    /// Export the underlying map mutably (for backward compatibility)
    pub fn as_map_mut(&mut self) -> &mut HashMap<String, Type> {
        &mut self.substitution
    }

    /// Look up a meta variable binding
    pub fn resolve(&self, name: &str) -> Option<&Type> {
        self.substitution.get(name)
    }

    /// Bind a meta variable, with occurs check
    pub fn bind(&mut self, name: &str, ty: &Type) -> UnifyResult {
        // If already bound, unify the existing binding with the new type
        if let Some(existing) = self.substitution.get(name).cloned() {
            return self.unify(&existing, ty);
        }

        // Occurs check: prevent infinite types
        if self.occurs_in(name, ty) {
            return UnifyResult::Fail(format!("Occurs check: ?{} appears in {}", name, ty));
        }

        self.substitution.insert(name.to_string(), ty.clone());
        UnifyResult::Ok
    }

    /// Apply the current substitution to a type, resolving all bound meta variables.
    /// This is the replacement for `solve_meta`.
    pub fn apply(&self, ty: &Type) -> Result<Type, String> {
        let resolved = self.resolve_ctx_call(ty, true);
        match resolved {
            Type::Meta(name) => {
                if let Some(bound) = self.substitution.get(&name) {
                    // Recursively apply to handle chains: ?A -> ?B where ?B is also bound
                    self.apply(bound)
                } else {
                    Err(format!("Unbound meta variable: ?{}", name))
                }
            }
            Type::Arrow(a, b) => {
                let a = self.apply(a.as_ref())?;
                let b = self.apply(b.as_ref())?;
                Ok(Type::Arrow(Box::new(a), Box::new(b)))
            }
            Type::Union(parts) => {
                let mut resolved = Vec::with_capacity(parts.len());
                for p in parts {
                    resolved.push(self.apply(&p)?);
                }
                Ok(Type::Union(resolved))
            }
            Type::Not(a) => {
                let a = self.apply(a.as_ref())?;
                Ok(Type::Not(Box::new(a)))
            }
            Type::Partial(t, s) => Ok(Type::Partial(
                Box::new(self.resolve_ctx_call(t.as_ref(), false)),
                s,
            )),
            Type::PathOf(t, p) => Ok(Type::PathOf(
                Box::new(self.resolve_ctx_call(t.as_ref(), false)),
                p,
            )),
            _ => Ok(resolved),
        }
    }

    /// Check if a type contains any unresolved meta variables
    pub fn has_unresolved_meta(&self, ty: &Type) -> bool {
        match ty {
            Type::Meta(name) => !self.substitution.contains_key(name),
            Type::Arrow(a, b) => self.has_unresolved_meta(a) || self.has_unresolved_meta(b),
            Type::Union(parts) => parts.iter().any(|p| self.has_unresolved_meta(p)),
            Type::Not(a) => self.has_unresolved_meta(a),
            _ => false,
        }
    }

    /// Resolve context calls (including binding-name rebinding) for subtyping/equality.
    /// Partial wrappers remain unresolved to preserve indeterminate behavior on incomplete trees.
    pub fn resolve_for_subtyping(&self, ty: &Type) -> Type {
        self.resolve_ctx_call(ty, true)
    }

    /// Unify two types following §1.7:
    /// UNIFY(τ₁, τ₂, σ) attempts to find a substitution making τ₁ = τ₂
    ///
    /// Three-valued result:
    /// - Ok: types are equal under the (possibly extended) substitution
    /// - Indeterminate: can't decide yet (involves Any, paths, context calls)
    /// - Fail: types are definitively incompatible
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> UnifyResult {
        // Resolve context calls first, then apply current substitution (walk)
        let t1 = self.walk(&self.resolve_ctx_call(t1, true));
        let t2 = self.walk(&self.resolve_ctx_call(t2, true));

        match (&t1, &t2) {
            // Identical types
            (Type::Raw(a), Type::Raw(b)) => {
                if a == b {
                    UnifyResult::Ok
                } else {
                    UnifyResult::Fail(format!("{} ≠ {}", a, b))
                }
            }

            // Meta variable on left: bind
            (Type::Meta(name), _) => self.bind(name, &t2),

            // Meta variable on right: bind
            (_, Type::Meta(name)) => self.bind(name, &t1),

            // Arrow types: unify components
            (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
                let l1 = l1.clone();
                let r1 = r1.clone();
                let l2 = l2.clone();
                let r2 = r2.clone();
                match self.unify(&l1, &l2) {
                    UnifyResult::Ok => self.unify(&r1, &r2),
                    UnifyResult::Indeterminate => {
                        // Try right side too; if it fails, propagate the failure
                        match self.unify(&r1, &r2) {
                            UnifyResult::Fail(e) => UnifyResult::Fail(e),
                            _ => UnifyResult::Indeterminate,
                        }
                    }
                    fail => fail,
                }
            }

            // Negation types: unify inner
            (Type::Not(a), Type::Not(b)) => {
                let a = a.clone();
                let b = b.clone();
                self.unify(&a, &b)
            }

            // Any = Any: ok
            (Type::Any, Type::Any) => UnifyResult::Ok,

            // Any vs concrete: indeterminate (Any is top, not a concrete type)
            (Type::Any, _) | (_, Type::Any) => UnifyResult::Indeterminate,

            // Union types: unify point-wise (same arity/ordering for now)
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
                    match self.unify(l, r) {
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
            (Type::Union(_), _) | (_, Type::Union(_)) => {
                UnifyResult::Fail(format!("Cannot unify {} with {}", t1, t2))
            }

            // None = None: ok
            (Type::None, Type::None) => UnifyResult::Ok,

            // None vs non-None: fail
            (Type::None, _) | (_, Type::None) => {
                UnifyResult::Fail("None is not unifiable with non-None".to_string())
            }

            // Unresolved paths/context calls: indeterminate
            (Type::Path(_), _) | (_, Type::Path(_)) => UnifyResult::Indeterminate,
            (Type::PathOf(_, _), _) | (_, Type::PathOf(_, _)) => UnifyResult::Indeterminate,
            (Type::ContextCall(_, _), _) | (_, Type::ContextCall(_, _)) => {
                UnifyResult::Indeterminate
            }

            // Partial types: unwrap and unify inner
            (Type::Partial(t, _), other) | (other, Type::Partial(t, _)) => {
                let t = t.clone();
                let other = other.clone();
                self.unify(&t, &other)
            }

            // Atom types: these should have been resolved by solve_binding before unification
            (Type::Atom(a), Type::Atom(b)) => {
                if a == b {
                    UnifyResult::Ok
                } else {
                    UnifyResult::Fail(format!("Atom {} ≠ {}", a, b))
                }
            }

            // Structural mismatch: fail
            _ => UnifyResult::Fail(format!("Cannot unify {} with {}", t1, t2)),
        }
    }

    /// Walk a type through the substitution, resolving top-level meta variables.
    /// Does NOT recursively apply — just resolves the outermost meta.
    fn walk(&self, ty: &Type) -> Type {
        match ty {
            Type::Meta(name) => {
                if let Some(bound) = self.substitution.get(name) {
                    self.walk(bound)
                } else {
                    ty.clone()
                }
            }
            _ => ty.clone(),
        }
    }

    fn resolve_ctx_call(&self, ty: &Type, allow_context: bool) -> Type {
        match ty {
            Type::ContextCall(ctx_name, var) => {
                // First, rebind the lookup variable if it refers to a binding name.
                // Example: Γ(name) where `name` is bound to `x` becomes Γ(x).
                let resolved_var = self
                    .binding_values
                    .get(var)
                    .map(|v| v.as_str())
                    .unwrap_or(var.as_str());
                // Then, attempt a context lookup if allowed. For partial types we keep
                // context calls unresolved so they can remain indeterminate.
                if allow_context {
                    if let Some(ctx) = self.context.as_ref() {
                        if let Some(found) = ctx.lookup(resolved_var) {
                            return found.clone();
                        }
                        // Prefix lookups keep partial inputs indeterminate, rather than failing.
                        if ctx.lookup_starts_with(resolved_var).is_some() {
                            return ty.clone();
                        }
                    }
                }
                if resolved_var != var.as_str() {
                    return Type::ContextCall(ctx_name.clone(), resolved_var.to_string());
                }
                ty.clone()
            }
            Type::Arrow(a, b) => Type::Arrow(
                Box::new(self.resolve_ctx_call(a, allow_context)),
                Box::new(self.resolve_ctx_call(b, allow_context)),
            ),
            Type::Union(parts) => Type::Union(
                parts
                    .iter()
                    .map(|p| self.resolve_ctx_call(p, allow_context))
                    .collect(),
            ),
            Type::Not(a) => Type::Not(Box::new(self.resolve_ctx_call(a, allow_context))),
            Type::Partial(t, s) => {
                Type::Partial(Box::new(self.resolve_ctx_call(t, false)), s.clone())
            }
            Type::PathOf(t, p) => {
                Type::PathOf(Box::new(self.resolve_ctx_call(t, false)), p.clone())
            }
            _ => ty.clone(),
        }
    }

    /// Occurs check: does meta variable ?name appear in ty (after walking)?
    fn occurs_in(&self, name: &str, ty: &Type) -> bool {
        let ty = self.walk(ty);
        match &ty {
            Type::Meta(n) => n == name,
            Type::Arrow(a, b) => self.occurs_in(name, a) || self.occurs_in(name, b),
            Type::Union(parts) => parts.iter().any(|p| self.occurs_in(name, p)),
            Type::Not(a) => self.occurs_in(name, a),
            Type::Partial(t, _) | Type::PathOf(t, _) => self.occurs_in(name, t),
            _ => false,
        }
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
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => Some(equal(l1, l2)? && equal(r1, r2)?),
        (Type::Union(a), Type::Union(b)) => {
            if a.len() != b.len() {
                Some(false)
            } else {
                let mut all = true;
                for (x, y) in a.iter().zip(b.iter()) {
                    all = all && equal(x, y)?;
                }
                Some(all)
            }
        }
        (Type::Not(a), Type::Not(b)) => equal(a, b),

        // Any context-dependent equality is handled by the evaluator.
        // Returning None here makes the equality relation partial.
        (Type::ContextCall(_, _), _) | (_, Type::ContextCall(_, _)) => None,

        // Path-based types are placeholders for unresolved bindings and must be
        // resolved before equality can be decided.
        (Type::Path(_), _) | (_, Type::Path(_)) => None,
        (Type::PathOf(_, _), _) | (_, Type::PathOf(_, _)) => None,

        // Any = Any is definitionally true (same type)
        (Type::Any, Type::Any) => Some(true),
        // Any vs non-Any: indeterminate — Any is a supertype, not equal to concrete types.
        // Returning None lets the evaluator treat this as Partial (possibly completable)
        // rather than erroneously accepting or rejecting.
        (Type::Any, _) | (_, Type::Any) => None,

        // None = None is true (both are the empty type)
        (Type::None, Type::None) => Some(true),
        // None vs non-None: definitionally false (empty type is not equal to any inhabited type)
        (Type::None, _) | (_, Type::None) => Some(false),

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
    if let Some(true) = equal(&t1, &t2) {
        return true;
    }

    // Structural subtyping
    match (&t1, &t2) {
        // Arrow: contravariant in domain, covariant in codomain
        (Type::Arrow(d1, c1), Type::Arrow(d2, c2)) => subtype(d2, d1) && subtype(c1, c2),
        // Union on left: every member must be subtype of target
        (Type::Union(parts), other) => parts.iter().all(|p| subtype(p, other)),
        // Union on right: source must be subtype of at least one member
        (other, Type::Union(parts)) => parts.iter().any(|p| subtype(other, p)),

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::typing::Type;

    fn parse(t: &str) -> Type {
        Type::parse(t).expect("type should parse")
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

    #[test]
    fn unify_union_with_meta_member() {
        let mut unifier = Unifier::new();
        let lhs = parse("?A | 'Bool'");
        let rhs = parse("'Int' | 'Bool'");
        assert!(unifier.unify(&lhs, &rhs).is_ok());
        assert!(matches!(unifier.resolve("A"), Some(Type::Raw(name)) if name == "Int"));
    }
}
