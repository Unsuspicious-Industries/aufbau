//! Formal typing environments and tree-level status objects.

use crate::typing::Type;
use std::collections::BTreeMap;

/// A resolved context address: which namespace, and the name within it.
///
/// This is a [`Key`](crate::typing::Key) after resolution — the compile-time
/// question "how is this addressed" answered into a runtime "where".
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Slot {
    /// Named by input text: the value of some bound token.
    Binding(String),
    /// Named by the grammar itself, independent of any input.
    Ambient(String),
}

/// Typing environment for a derivation point.
///
/// The two maps are deliberately separate. `bindings` is keyed by *input text*,
/// so its keys are user data — in any real grammar an identifier can be spelled
/// anything. An ambient key is chosen by the grammar author, who cannot know
/// what a program will name its variables, so sharing one namespace would make
/// every ambient entry reachable by a program that happens to name a variable
/// after it. That is not hypothetical: with one namespace, C's `return;`
/// type-checks as an expression statement reading a variable called `return`.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct Context {
    pub bindings: BTreeMap<String, Type>,
    pub ambient: BTreeMap<String, Type>,
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

    /// The entry at a fixed name. No prefix matching: an ambient key is written
    /// in full by a rule, so a partial one is a typo, not a growing token.
    #[must_use]
    pub fn lookup_ambient(&self, k: &str) -> Option<&Type> {
        self.ambient.get(k)
    }

    #[must_use]
    pub fn lookup_starts_with(&self, prefix: &str) -> Option<&Type> {
        self.bindings
            .iter()
            .find(|(k, _)| k.starts_with(prefix))
            .map(|(_, v)| v)
    }

    /// Add or replace a binding.
    #[must_use]
    pub fn shadow(&self, x: String, ty: Type) -> Self {
        let mut new = self.clone();
        new.bindings.insert(x, ty);
        new
    }

    /// Add or replace an entry in whichever namespace `slot` names.
    #[must_use]
    pub fn shadow_at(&self, slot: &Slot, ty: Type) -> Self {
        let mut new = self.clone();
        match slot {
            Slot::Binding(x) => new.bindings.insert(x.clone(), ty),
            Slot::Ambient(k) => new.ambient.insert(k.clone(), ty),
        };
        new
    }

    pub fn add(&mut self, x: String, ty: Type) {
        self.bindings.insert(x, ty);
    }
}

/// A context morphism between two interned contexts.
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq)]
pub struct ContextTransition {
    pub transforms: Vec<(Slot, Type)>,
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
