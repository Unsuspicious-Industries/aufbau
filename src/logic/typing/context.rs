//! Formal typing environments and tree-level status objects.

use crate::logic::path::TreePath;
use crate::logic::typing::{Type, TypeAscription};
use std::collections::BTreeMap;

/// Typing environment for a derivation point.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Context {
    pub bindings: BTreeMap<String, Type>,
    pub unresolved_bindings: BTreeMap<TreePath, Type>,
}

impl Context {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lookup(&self, x: &str) -> Option<&Type> {
        self.bindings.get(x)
    }

    pub fn merge(&self, other: &Self) -> Self {
        let mut new = self.clone();
        for (k, v) in &other.bindings {
            new.bindings.insert(k.clone(), v.clone());
        }
        for (k, v) in &other.unresolved_bindings {
            new.unresolved_bindings.insert(k.clone(), v.clone());
        }
        new
    }

    pub fn lookup_unresolved(&self, path: &TreePath) -> Option<&Type> {
        self.unresolved_bindings.get(path)
    }

    pub fn lookup_starts_with(&self, prefix: &str) -> Option<&Type> {
        self.bindings
            .iter()
            .find(|(k, _)| k.starts_with(prefix))
            .map(|(_, v)| v)
    }

    pub fn extend(&self, x: String, ty: Type) -> Result<Self, String> {
        if self.bindings.contains_key(&x) {
            return Err(format!("Context already contains binding for '{}'", x));
        }

        let mut new = self.clone();
        new.bindings.insert(x, ty);
        Ok(new)
    }

    pub fn shadow(&self, x: String, ty: Type) -> Self {
        let mut new = self.clone();
        new.bindings.insert(x, ty);
        new
    }

    pub fn extend_unresolved(&self, path: TreePath, ty: Type) -> Result<Self, String> {
        if self.unresolved_bindings.contains_key(&path) {
            return Err(format!(
                "Context already contains unresolved binding for path '{:?}'",
                path
            ));
        }

        let mut new = self.clone();
        new.unresolved_bindings.insert(path, ty);
        Ok(new)
    }

    pub fn add(&mut self, x: String, ty: Type) {
        self.bindings.insert(x, ty);
    }
}

/// A context morphism between two interned contexts.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ContextTransition {
    pub transforms: Vec<TypeAscription>,
}

impl ContextTransition {
    pub fn identity() -> Self {
        Self {
            transforms: Vec::new(),
        }
    }

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
