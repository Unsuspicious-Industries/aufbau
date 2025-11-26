//! Core typing types and operations
//!
//! Clean, minimal definitions for the typing system.

use crate::logic::typing::Type;
use std::collections::HashMap;

// ============================================================================
// Context: Γ : String → Type
// ============================================================================

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub bindings: HashMap<String, Type>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    /// Γ(x) - lookup
    pub fn lookup(&self, x: &str) -> Option<&Type> {
        self.bindings.get(x)
    }

    /// Γ[x:τ] - functional extension (returns new context)
    pub fn extend(&self, x: String, ty: Type) -> Self {
        let mut new = self.clone();
        new.bindings.insert(x, ty);
        new
    }
    
    /// Mutable add (for imperative contexts)
    pub fn add(&mut self, x: String, ty: Type) {
        self.bindings.insert(x, ty);
    }
}

// ============================================================================
// Substitution: inference variable → Type
// ============================================================================

pub type Substitution = HashMap<String, Type>;

/// Apply substitution to type
pub fn subst(ty: &Type, s: &Substitution) -> Type {
    subst_rec(ty, s, 0)
}

fn subst_rec(ty: &Type, s: &Substitution, depth: usize) -> Type {
    if depth > 20 { return Type::Universe; }
    
    match ty {
        Type::Atom(name) => s.get(name)
            .map(|t| subst_rec(t, s, depth + 1))
            .unwrap_or_else(|| Type::Atom(name.clone())),
        Type::Arrow(l, r) => Type::Arrow(
            Box::new(subst_rec(l, s, depth + 1)),
            Box::new(subst_rec(r, s, depth + 1)),
        ),
        Type::Not(t) => Type::Not(Box::new(subst_rec(t, s, depth + 1))),
        Type::Intersection(l, r) => Type::Intersection(
            Box::new(subst_rec(l, s, depth + 1)),
            Box::new(subst_rec(r, s, depth + 1)),
        ),
        Type::Union(l, r) => Type::Union(
            Box::new(subst_rec(l, s, depth + 1)),
            Box::new(subst_rec(r, s, depth + 1)),
        ),
        other => other.clone(),
    }
}

// ============================================================================
// Unification
// ============================================================================

/// Unify two types, extending substitution
pub fn unify(t1: &Type, t2: &Type, s: &mut Substitution) -> bool {
    let t1 = subst(t1, s);
    let t2 = subst(t2, s);
    
    if t1 == t2 { return true; }
    
    // Raw and Atom with same content should unify
    // This handles cases like Type::Raw("int") unifying with Type::Atom("int")
    match (&t1, &t2) {
        (Type::Raw(n1), Type::Atom(n2)) | (Type::Atom(n1), Type::Raw(n2)) => {
            if n1 == n2 { return true; }
        }
        _ => {}
    }
    
    // Bind inference variables
    if let Type::Atom(name) = &t2 {
        if is_var(name) && !occurs(name, &t1) {
            s.insert(name.clone(), t1);
            return true;
        }
    }
    if let Type::Atom(name) = &t1 {
        if is_var(name) && !occurs(name, &t2) {
            s.insert(name.clone(), t2);
            return true;
        }
    }
    
    // Structural
    match (&t1, &t2) {
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
            unify(l1, l2, s) && unify(r1, r2, s)
        }
        (Type::Universe, _) | (_, Type::Universe) => true,
        _ => false,
    }
}

fn is_var(name: &str) -> bool {
    name.starts_with('?') || name.starts_with('τ')
}

fn occurs(var: &str, ty: &Type) -> bool {
    match ty {
        Type::Atom(n) => n == var,
        Type::Arrow(l, r) | Type::Intersection(l, r) | Type::Union(l, r) => {
            occurs(var, l) || occurs(var, r)
        }
        Type::Not(t) => occurs(var, t),
        _ => false,
    }
}

// ============================================================================
// Tree Status
// ============================================================================

/// Result of checking a tree
#[derive(Clone, Debug)]
pub enum TreeStatus {
    /// Complete and well-typed
    Valid(Type),
    /// Partial but well-formed (at frontier)
    Partial(Type),
    /// Malformed - drop this tree
    Malformed,
}

impl TreeStatus {
    pub fn is_ok(&self) -> bool {
        !matches!(self, TreeStatus::Malformed)
    }
    
    pub fn ty(&self) -> Option<&Type> {
        match self {
            TreeStatus::Valid(t) | TreeStatus::Partial(t) => Some(t),
            TreeStatus::Malformed => None,
        }
    }
}

