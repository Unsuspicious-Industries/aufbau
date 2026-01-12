//! Core typing types - Context and TreeStatus

use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use crate::logic::typing::Type;
use crate::logic::typing::rule::TypeOperation;
use std::collections::HashMap;

// =============================================================================
// Context: Γ : String → Type
// =============================================================================

#[derive(Clone, Debug, Default)]
pub struct Context {
    pub bindings: HashMap<String, Type>,
    pub unresolved_bindings: HashMap<TreePath, Type>,
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

    /// Γ[x:τ] - functional extension (immutable)
    pub fn extend(&self, x: String, ty: Type) -> Result<Self, String> {

        // check for coonflicts
        if self.bindings.contains_key(&x) {
            return Err(format!("Context already contains binding for '{}'", x));
        }

        let mut new = self.clone();
        new.bindings.insert(x, ty);
        Ok(new)
    }
    pub fn extend_unresolved(&self, path: TreePath, ty: Type) -> Result<Self, String> {

        // check for conflicts same path => overwrite (i think)
        // !JANKY!
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

pub type TreePath = Vec<usize>;

#[derive(Clone, Debug)]
pub struct TreeRef<'a> {
    root: &'a NonTerminal,
    path: Vec<usize>,
}

impl<'a> TreeRef<'a> {
    pub fn new(root: &'a NonTerminal, path: Vec<usize>) -> Self {
        TreeRef { root, path }
    }

    pub fn path(&self) -> &[usize] {
        &self.path
    }

    pub fn path_vec(&self) -> TreePath {
        self.path.clone()
    }

    pub fn get(&self, index: usize) -> TreeRef<'a> {
        let mut path = self.path.clone();
        path.push(index);
        TreeRef::new(self.root, path)
    }

    pub fn get_path(&self, path: TreePath) -> TreeRef<'a> {
        let mut new_path = self.path.clone();
        new_path.extend(path);
        TreeRef::new(self.root, new_path)
    }

    pub fn exists(&self) -> bool {
        self.root.path_exists(self.path())
    }

    pub fn is_nt(&self) -> bool {
        self.root.is_path_nt(self.path())
    }

    pub fn is_terminal(&self) -> bool {
        !self.is_nt()
    }

    /// Get terminal without cloning NonTerminal (only clones terminal, which is OK)
    pub fn as_terminal(&self) -> Option<Terminal> {
        if !self.is_terminal() {
            return None;
        }
        // Only clone terminal, not the whole node path
        match self.root.get_path(self.path())? {
            Node::Terminal(t) => Some(t),
            Node::NonTerminal(_) => None,
        }
    }

    /// Access NT data via path without cloning typing rule
    pub fn rule(&self) -> Option<&str> {
        self.get_nt().and_then(|n| n.production.rule.as_deref())
    }

    /// Access NT data via path without cloning - get name
    pub fn name(&self) -> Option<&str> {
        self.get_nt().and_then(|n| Some(n.name.as_str()))
    }

    /// Get NT at path by traversing without cloning the full node
    pub fn get_nt(&self) -> Option<&'a NonTerminal> {
        self.root.get_path_as_nt(&self.path)
    }

    /// Check if at frontier using path-based checks
    pub fn is_at_frontier(&self) -> bool {
        // For NT, check via path without cloning
        let Some(nt) = self.get_nt() else {
            return true;
        };
        !nt.is_complete()
    }

    pub fn nt_child_indices(&self) -> Vec<usize> {
        let Some(nt) = self.get_nt() else {
            return vec![];
        };
        let indices: Vec<usize> = nt
            .children
            .iter()
            .enumerate()
            .filter(|(_, c)| matches!(c, Node::NonTerminal(_)))
            .map(|(i, _)| i)
            .collect();
        indices
    }

    /// Get text at relative path without cloning NT
    pub fn node_text_path(&self, rel_path: &TreePath) -> Option<String> {
        if !self.is_nt() {
            return None;
        }
        // Build absolute path
        let mut abs_path = self.path.clone();
        abs_path.extend(rel_path);
        self.root.node_text_path(&abs_path)
    }
}

pub enum Constraint<'a> {
    Op(TreeRef<'a>, TypeOperation, TreeRef<'a>), // two partial nodes are linked by an operation
    Type(TreeRef<'a>, Type),                     // a partial node has a specific type
}
