//! Core typing types - Context and TreeStatus

use crate::logic::typing::rule::TypeOperation;
use crate::logic::typing::Type;
use std::collections::BTreeMap;

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

#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct TreePath(Vec<usize>);

impl TreePath {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, index: usize) {
        self.0.push(index);
    }

    pub fn pop(&mut self) -> Option<usize> {
        self.0.pop()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn as_slice(&self) -> &[usize] {
        &self.0
    }

    pub fn iter(&self) -> std::slice::Iter<'_, usize> {
        self.0.iter()
    }

    pub fn into_vec(self) -> Vec<usize> {
        self.0
    }
}

impl From<Vec<usize>> for TreePath {
    fn from(path: Vec<usize>) -> Self {
        Self(path)
    }
}

impl AsRef<[usize]> for TreePath {
    fn as_ref(&self) -> &[usize] {
        self.as_slice()
    }
}

/// Dead code — scaffolding for a future constraint-propagation pass.
#[allow(dead_code)]
pub enum Constraint {
    Op(TypeOperation, TypeOperation),
    Type(TypeOperation, Type),
}
