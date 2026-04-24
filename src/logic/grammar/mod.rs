pub mod extend;
pub mod fill;
pub mod load;
pub mod production;
pub mod save;
pub mod symbol;
pub mod tokenizer;
pub mod typing;
pub mod utils;

use crate::logic::binding::{self, BindingMap};
pub use production::Production;
pub use symbol::Symbol;
pub use tokenizer::{DEFAULT_DELIMITERS, Segment, Tokenizer};

#[cfg(test)]
mod tests;

pub type AltId = usize;
pub type NtId = usize;

// nt_idx, alt_idx
pub type ProdId = (NtId, AltId);

use crate::logic::typing::TypingRule;
use std::collections::{HashMap, HashSet};

/// A complete grammar consisting of context-free productions and
/// inference-style typing rules.
#[derive(Debug, Clone)]
pub struct Grammar {
    pub name: String,
    pub productions: HashMap<String, Vec<Production>>,
    pub nonterminals: Vec<String>,
    pub nonterminal_rules: HashMap<String, String>,
    pub bridge_nonterminals: HashSet<String>,
    pub rules: HashMap<String, TypingRule>,

    // tokenization helpers
    pub specials: Vec<String>,
    pub delimiters: Vec<char>,

    pub start: Option<String>,

    /// Cached tokenizer (built lazily from special_tokens and delimiters)
    pub tokenizer: Option<Tokenizer>,
    /// Cached binding map (built lazily from productions and typing rules)
    pub bindings: Option<BindingMap>,
}

// Simple eqaulity
// Can be falsified but we don't care
impl PartialEq for Grammar {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for Grammar {}

// Provide a stable, deterministic Hash implementation that mirrors the
// fields considered by `PartialEq`. We iterate `productions` in sorted
// order to ensure the hash is independent of HashMap iteration order.
impl std::hash::Hash for Grammar {
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

impl Grammar {
    /// Create an empty grammar
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

    // ---------------------
    // Data handling methods
    //----------------------

    /// Add a special token to the grammar if not already present.
    pub fn add_special(&mut self, token: String) {
        if !self.specials.contains(&token) {
            self.specials.push(token);
            self.tokenizer = None; // Invalidate cache
        }
    }

    /// Add a typing rule to the grammar.
    pub fn add_typing_rule(&mut self, rule: TypingRule) {
        self.rules.insert(rule.name.clone(), rule);
    }

    /// Attach a typing rule name to a nonterminal.
    pub fn set_nonterminal_rule(&mut self, nt: String, rule_name: String) -> Result<(), String> {
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

    /// Get the typing rule name associated with a nonterminal.
    pub fn nonterminal_rule(&self, nt: &str) -> Option<&String> {
        self.nonterminal_rules.get(nt)
    }

    /// Get the typing rule name associated with a production.
    pub fn rule_for_prod(&self, prod: ProdId) -> Option<&String> {
        self.nt(prod.0)
            .and_then(|nt| self.nonterminal_rules.get(nt))
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

    /// Add a production rule to the grammar.
    /// kinda complete but whatever
    pub fn add_production(&mut self, nt: String, prod: Production) {
        if !self.productions.contains_key(&nt) {
            self.nonterminals.push(nt.clone());
        }
        self.productions.entry(nt.clone()).or_default().push(prod);
    }

    /// Set the start nonterminal.
    pub fn set_start<S: Into<String>>(&mut self, start: S) {
        self.start = Some(start.into());
    }

    /// Get the start nonterminal if available.
    pub fn start(&self) -> Option<&String> {
        self.start.as_ref()
    }

    // Rebuild the binding map from the current productions and typing rules
    pub fn build_bindings(&mut self) {
        self.bindings = Some(binding::build_binding_map(self));
    }
    /// Build and cache the tokenizer from current special_tokens and delimiters.
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

    pub fn rules(&self) -> &HashMap<String, TypingRule> {
        &self.rules
    }

    pub fn productions(&self) -> impl Iterator<Item = (&str, &Vec<Production>)> {
        self.productions.iter().map(|(k, v)| (k.as_str(), v))
    }

    /// Check if a symbol is nullable (can match zero tokens).
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

    /// Tokenize input using the grammar's special tokens and delimiters.
    pub fn tokenize(&mut self, input: &str) -> Result<Vec<Segment>, String> {
        if self.tokenizer.is_none() {
            self.build_tokenizer();
        }

        self.tokenizer.as_ref().unwrap().tokenize(input)
    }

    pub fn extend_input(&mut self, input: &str, token: &str) -> String {
        extend::extend_input(self, input, token)
    }
}
