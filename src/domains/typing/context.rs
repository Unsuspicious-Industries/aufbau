//! Formal typing environments and tree-level status objects.

use crate::domains::typing::Type;
use std::collections::BTreeMap;

/// Typing environment for a derivation point.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Context {
    pub bindings: BTreeMap<String, Type>,
}

impl Context {
    #[must_use] 
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use] 
    pub fn lookup(&self, x: &str) -> Option<&Type> {
        self.bindings.get(x)
    }

    #[must_use] 
    pub fn lookup_starts_with(&self, prefix: &str) -> Option<&Type> {
        self.bindings
            .iter()
            .find(|(k, _)| k.starts_with(prefix))
            .map(|(_, v)| v)
    }

    /// Add a binding; fails if `x` is already bound.
    pub fn extend(&self, x: String, ty: Type) -> Result<Self, String> {
        if self.bindings.contains_key(&x) {
            return Err(format!("Context already contains binding for '{x}'"));
        }
        let mut new = self.clone();
        new.bindings.insert(x, ty);
        Ok(new)
    }

    /// Add or replace a binding.
    #[must_use] 
    pub fn shadow(&self, x: String, ty: Type) -> Self {
        let mut new = self.clone();
        new.bindings.insert(x, ty);
        new
    }

    pub fn add(&mut self, x: String, ty: Type) {
        self.bindings.insert(x, ty);
    }
}

/// A context morphism between two interned contexts.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ContextTransition {
    pub transforms: Vec<(String, Type)>,
}

impl ContextTransition {
    #[must_use] 
    pub fn identity() -> Self {
        Self {
            transforms: Vec::new(),
        }
    }

    #[must_use] 
    pub fn compose(&self, next: &Self) -> Self {
        let mut new_transforms = self.transforms.clone();
        new_transforms.extend(next.transforms.clone());
        Self {
            transforms: new_transforms,
        }
    }
}

/// Semantic classification of a parsed tree.
#[derive(Clone, Debug)]
pub enum TreeStatus {
    Valid(Type),
    Partial(Type),
    Malformed,
    TooDeep,
}

impl TreeStatus {
    #[must_use] 
    pub fn is_ok(&self) -> bool {
        !matches!(self, TreeStatus::Malformed)
    }

    #[must_use] 
    pub fn ty(&self) -> Option<&Type> {
        match self {
            TreeStatus::Valid(t) | TreeStatus::Partial(t) => Some(t),
            TreeStatus::Malformed | TreeStatus::TooDeep => None,
        }
    }
}
