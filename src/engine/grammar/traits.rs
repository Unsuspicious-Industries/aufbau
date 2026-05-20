//! Trait implementations for SPG (extracted for STYLE.md budget).

use super::SPG;
use crate::semantics::domain::ConstraintDomain;

impl<D: ConstraintDomain> Clone for SPG<D> {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            productions: self.productions.clone(),
            nonterminals: self.nonterminals.clone(),
            nonterminal_rules: self.nonterminal_rules.clone(),
            rules: self.rules.clone(),
            start: self.start.clone(),
            tokenizer: self.tokenizer.clone(),
            bindings: self.bindings.clone(),
        }
    }
}

impl<D: ConstraintDomain> PartialEq for SPG<D> {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}

impl<D: ConstraintDomain> Eq for SPG<D> {}

impl<D: ConstraintDomain> std::hash::Hash for SPG<D> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        let mut keys: Vec<&String> = self.productions.keys().collect();
        keys.sort();
        for k in keys {
            k.hash(state);
            if let Some(prods) = self.productions.get(k) {
                prods.hash(state);
            }
        }
        self.start.hash(state);
    }
}
