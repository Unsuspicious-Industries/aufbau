//! Core typing types - Context and TreeStatus

use crate::logic::typing::Type;
use std::collections::HashMap;

// Re-export from ops for backward compatibility
pub use crate::logic::typing::ops::{Substitution, apply as subst, unify};

// =============================================================================
// Context: Γ : String → Type
// =============================================================================

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub bindings: HashMap<String, Type>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lookup(&self, x: &str) -> Option<&Type> {
        self.bindings.get(x)
    }

    /// Γ[x:τ] - functional extension (immutable)
    pub fn extend(&self, x: String, ty: Type) -> Self {
        let mut new = self.clone();
        new.bindings.insert(x, ty);
        new
    }

    /// Mutable extension
    pub fn add(&mut self, x: String, ty: Type) {
        self.bindings.insert(x, ty);
    }
}

// =============================================================================
// Tree Status
// =============================================================================

#[derive(Clone, Debug)]
pub enum TreeStatus {
    /// Fully well-typed tree
    Valid(Type),
    /// Can be made well-typed by completion
    Partial(Type),
    /// Cannot be well-typed
    Malformed,
    /// Too deep
    TooDeep,
}

impl TreeStatus {
    pub fn is_ok(&self) -> bool {
        !matches!(self, TreeStatus::Malformed)
    }

    pub fn ty(&self) -> Option<&Type> {
        match self {
            TreeStatus::Valid(t) | TreeStatus::Partial(t) => Some(t),
            TreeStatus::Malformed | TreeStatus::TooDeep => None,
        }
    }
}
