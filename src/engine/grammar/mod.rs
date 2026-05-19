
pub mod load;
pub mod production;
pub mod save;
pub mod symbol;
pub mod tokenizer;
pub mod utils;

use crate::engine::binding::{self, BindingMap};
pub use production::Production;
pub use symbol::Symbol;
pub use tokenizer::{DEFAULT_DELIMITERS, Segment, Tokenizer};

#[cfg(test)]
mod tests;

pub type AltId = usize;
pub type NtId = usize;

// nt_idx, alt_idx
pub type ProdId = (NtId, AltId);

use crate::semantics::domain::ConstraintDomain;
use std::collections::{HashMap, HashSet};

/// SPG<D> — Syntax-directed Program Grammar parameterized by constraint domain D.
///
/// Corresponds to `G = (N, T, P, S, Θ, 𝒯, B, A)` from §1.2 `sec:gram-def`.
/// The rule table `Θ` has type `HashMap<String, D::Rule>`.
#[derive(Debug)]
pub struct SPG<D: ConstraintDomain> {
    pub name: String,
    pub productions: HashMap<String, Vec<Production>>,
    pub nonterminals: Vec<String>,
    pub nonterminal_rules: HashMap<String, String>,
    pub bridge_nonterminals: HashSet<String>,
    pub rules: HashMap<String, D::Rule>,

    pub specials: Vec<String>,
    pub delimiters: Vec<char>,
    pub start: Option<String>,
    pub tokenizer: Option<Tokenizer>,
    pub bindings: Option<BindingMap>,
}

// Manual Clone: D need not be Clone; only D::Rule: Clone (guaranteed by ConstraintDomain).
impl<D: ConstraintDomain> Clone for SPG<D> {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            productions: self.productions.clone(),
            nonterminals: self.nonterminals.clone(),
            nonterminal_rules: self.nonterminal_rules.clone(),
            bridge_nonterminals: self.bridge_nonterminals.clone(),
            rules: self.rules.clone(),
            specials: self.specials.clone(),
            delimiters: self.delimiters.clone(),
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
        self.specials.hash(state);
        self.delimiters.hash(state);
        self.start.hash(state);
    }
}

impl<D: ConstraintDomain> SPG<D> {
    pub fn new() -> Self {
        Self {
            name: String::new(),
            productions: HashMap::default(),
            nonterminals: Vec::default(),
            nonterminal_rules: HashMap::default(),
            bridge_nonterminals: HashSet::default(),
            rules: HashMap::default(),
            specials: Vec::default(),
            delimiters: DEFAULT_DELIMITERS.to_vec(),
            start: None,
            bindings: None,
            tokenizer: None,
        }
    }

    pub fn add_special(&mut self, token: String) {
        if !self.specials.contains(&token) {
            self.specials.push(token);
            self.tokenizer = None;
        }
    }

    pub fn add_rule(&mut self, name: String, rule: D::Rule) {
        self.rules.insert(name, rule);
    }

    pub fn set_nt_rule(&mut self, nt: String, rule_name: String) -> Result<(), String> {
        if let Some(existing) = self.nonterminal_rules.get(&nt) {
            if existing != &rule_name {
                return Err(format!(
                    "Conflicting typing rules for nonterminal {}: {} vs {}",
                    nt, existing, rule_name
                ));
            }
        } else {
            self.nonterminal_rules.insert(nt.clone(), rule_name);
        }
        if !self.productions.contains_key(&nt) && !self.nonterminals.contains(&nt) {
            self.nonterminals.push(nt.clone());
        }
        Ok(())
    }

    pub fn nt_rule(&self, nt: &str) -> Option<&String> {
        self.nonterminal_rules.get(nt)
    }

    pub fn rule_for_prod(&self, prod: ProdId) -> Option<&String> {
        self.nt(prod.0).and_then(|n| self.nt_rule(n))
    }

    pub fn rule_of_prod<'a>(&'a self, prod: ProdId) -> Option<&'a D::Rule> {
        self.rule_for_prod(prod)
            .and_then(|name| self.rules.get(name.as_str()))
    }

    pub fn is_transparent_nt(&self, nt: &str) -> bool {
        self.productions.get(nt).is_some_and(|productions| {
            !productions.is_empty()
                && productions.iter().all(|production| {
                    let nonterminal_children = production
                        .rhs
                        .iter()
                        .filter(|symbol| matches!(symbol, Symbol::Nonterminal { .. }))
                        .count();
                    let bound_terminals = production.rhs.iter().any(|symbol| {
                        matches!(
                            symbol,
                            Symbol::Terminal {
                                binding: Some(_),
                                ..
                            }
                        )
                    });
                    nonterminal_children == 1 && !bound_terminals
                })
        })
    }

    pub fn mark_bridge_nt<S: Into<String>>(&mut self, nt: S) {
        self.bridge_nonterminals.insert(nt.into());
    }

    pub fn is_bridge_nt(&self, nt: &str) -> bool {
        self.bridge_nonterminals.contains(nt)
    }

    pub fn add_production(&mut self, nt: String, prod: Production) {
        if !self.productions.contains_key(&nt) {
            self.nonterminals.push(nt.clone());
        }
        self.productions.entry(nt.clone()).or_default().push(prod);
    }

    pub fn set_start<S: Into<String>>(&mut self, start: S) {
        self.start = Some(start.into());
    }

    pub fn start(&self) -> Option<&String> {
        self.start.as_ref()
    }

    pub fn build_bindings(&mut self) {
        self.bindings = Some(binding::build_binding_map(self));
    }

    pub fn build_tokenizer(&mut self) {
        if self.tokenizer.is_none() {
            self.tokenizer = Some(Tokenizer::new(
                self.specials.clone(),
                self.delimiters.clone(),
            ));
        }
    }

    pub fn production_count(&self) -> usize {
        self.nonterminals.len()
    }

    pub fn production(&self, nt: &str) -> Option<&Vec<Production>> {
        self.productions.get(nt)
    }

    pub fn nt(&self, idx: usize) -> Option<&str> {
        self.nonterminals.get(idx).map(|n| n.as_str())
    }

    pub fn nt_index(&self, name: &str) -> Option<usize> {
        self.nonterminals.iter().position(|n| n == name)
    }

    pub fn productions_at(&self, idx: usize) -> Option<&Vec<Production>> {
        self.nt(idx).and_then(|nts| self.productions.get(nts))
    }

    pub fn prod(&self, pid: ProdId) -> Option<Production> {
        self.productions_at(pid.0)?.get(pid.1).cloned()
    }

    pub fn specials(&self) -> &Vec<String> {
        &self.specials
    }

    pub fn rules(&self) -> &HashMap<String, D::Rule> {
        &self.rules
    }

    pub fn productions(&self) -> impl Iterator<Item = (&str, &Vec<Production>)> {
        self.productions.iter().map(|(k, v)| (k.as_str(), v))
    }

    pub fn nu(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::Terminal { .. } => false,
            Symbol::Nonterminal { name: nt, .. } => {
                let nt = self.productions.get(nt);
                nt.map(|prod| prod.iter().all(|s| s.rhs.iter().all(|sym| self.nu(sym))))
                    .unwrap_or(false)
            }
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Result<Vec<Segment>, String> {
        if self.tokenizer.is_none() {
            self.build_tokenizer();
        }
        self.tokenizer.as_ref().unwrap().tokenize(input)
    }
}


