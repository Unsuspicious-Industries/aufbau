pub mod load;
pub mod production;
pub mod save;
pub mod symbol;
pub mod tokenizer;
pub mod utils;

use crate::engine::binding::{self, BindingMap};
pub use production::Production;
pub use symbol::Symbol;
pub use tokenizer::{Segment, Tokenizer};

#[cfg(test)]
mod tests;

pub type AltId = usize;
pub type NtId = usize;

pub type ProdId = (NtId, AltId);

use crate::typing::domain::Trees;
use crate::typing::{Normalizer, RewriteRule, Term, TyExpr, TypingRule};
use std::collections::HashMap;

/// Syntax-directed Program Grammar `G = (N, T, P, S, Θ, R, B, A)` — `sec:gram-def`.
///
/// `R` maps each nonterminal to its typing-rule name; `Θ` is the rule table.
#[derive(Debug)]
pub struct SPG {
    /// Human-readable name for the grammar (not part of the formal tuple).
    pub name: String,
    /// `P` — the set of production rules, keyed by nonterminal name.
    pub productions: HashMap<String, Vec<Production>>,
    /// `N` — the ordered set of nonterminal symbols.
    pub nonterminals: Vec<String>,
    /// `R` — maps each nonterminal to its typing rule name.
    pub nonterminal_rules: HashMap<String, String>,
    /// `Θ` — the set of typing rules.
    pub rules: HashMap<String, TypingRule>,
    /// Oriented type-rewrite rules `lhs ⇝ rhs` (the normalization theory), as raw
    /// source-string pairs. The runtime parses each side into a term with the
    /// grammar (`?A` is a metavariable, everything else concrete type syntax),
    /// exactly as it parses the rule type-expressions.
    pub rewrites: Vec<(String, String)>,

    /// `S` — the designated start symbol.
    pub start: Option<String>,
    /// `A` — the tokenizer holding special tokens and delimiters.
    pub tokenizer: Option<Tokenizer>,
    /// `B` — the binding map from (binding, rule) pairs to grammar paths.
    pub bindings: Option<BindingMap>,
}

impl Clone for SPG {
    fn clone(&self) -> Self {
        Self {
            name: self.name.clone(),
            productions: self.productions.clone(),
            nonterminals: self.nonterminals.clone(),
            nonterminal_rules: self.nonterminal_rules.clone(),
            rules: self.rules.clone(),
            rewrites: self.rewrites.clone(),
            start: self.start.clone(),
            tokenizer: self.tokenizer.clone(),
            bindings: self.bindings.clone(),
        }
    }
}

impl PartialEq for SPG {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
    }
}
impl Eq for SPG {}

impl std::hash::Hash for SPG {
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

impl Default for SPG {
    fn default() -> Self {
        Self::new()
    }
}

impl SPG {
    #[must_use]
    pub fn new() -> Self {
        Self {
            name: String::new(),
            productions: HashMap::default(),
            nonterminals: Vec::default(),
            nonterminal_rules: HashMap::default(),
            rules: HashMap::default(),
            rewrites: Vec::default(),
            start: None,
            tokenizer: Some(Tokenizer::new()),
            bindings: None,
        }
    }

    pub fn add_special(&mut self, token: String) {
        self.tokenizer
            .get_or_insert_with(Tokenizer::new)
            .add_special(token);
    }

    pub fn add_rule(&mut self, name: String, rule: TypingRule) {
        self.rules.insert(name, rule);
    }

    pub fn bind_nt_rule(&mut self, nt: String, rule_name: String) -> Result<(), String> {
        if let Some(existing) = self.nonterminal_rules.get(&nt) {
            if existing != &rule_name {
                return Err(format!(
                    "Conflicting typing rules for nonterminal {nt}: {existing} vs {rule_name}"
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

    #[must_use]
    pub fn nt_rule(&self, nt: &str) -> Option<&String> {
        self.nonterminal_rules.get(nt)
    }

    #[must_use]
    pub fn rule_for_prod(&self, prod: ProdId) -> Option<&String> {
        self.nt(prod.0).and_then(|n| self.nt_rule(n))
    }

    #[must_use]
    pub fn rule_of_prod(&self, prod: ProdId) -> Option<&TypingRule> {
        self.rule_for_prod(prod)
            .and_then(|name| self.rules.get(name.as_str()))
    }

    /// The binding names the production(s) carrying `rule` declare (their `[x]`
    /// annotations). In that rule's type-expressions a bare identifier is a
    /// reference iff it names one of these; any other is object-grammar text (a
    /// type keyword like `list`), so `list` stays a keyword while `τ` is a ref.
    #[must_use]
    pub fn rule_bindings(&self, rule: &str) -> std::collections::HashSet<String> {
        let mut out = std::collections::HashSet::new();
        for (nt, label) in &self.nonterminal_rules {
            if label != rule {
                continue;
            }
            let prods = self.productions.get(nt).into_iter().flatten();
            for sym in prods.flat_map(|p| p.rhs.iter()) {
                if let Some(b) = sym.binding() {
                    out.insert(b.clone());
                }
            }
        }
        out
    }

    /// Is this production a *transparent* wrapper: rule-less, exactly one
    /// nonterminal child, and no bound terminal? Such a node carries no
    /// semantics of its own, so it is collapsed to its single child. This is
    /// the one definition of transparency, shared by elaboration (evidence
    /// pass-through) and term construction (tree pass-through); nothing else
    /// should restate it.
    #[must_use]
    pub fn is_transparent(&self, prod: ProdId) -> bool {
        if self.rule_for_prod(prod).is_some() {
            return false;
        }
        let Some(p) = self.prod(prod) else {
            return false;
        };
        let nts = p
            .rhs
            .iter()
            .filter(|s| matches!(s, Symbol::Nonterminal { .. }))
            .count();
        let bound_terminal = p
            .rhs
            .iter()
            .any(|s| matches!(s, Symbol::Terminal { binding: Some(_), .. }));
        nts == 1 && !bound_terminal
    }

    /// The rewrite theory parsed against this grammar. Empty ⇒ the normalizer is
    /// the identity. A side that fails to parse is dropped (no silent mis-rewrite).
    #[must_use]
    pub fn normalizer(&self) -> Normalizer {
        let rules = self
            .rewrites
            .iter()
            .filter_map(|(l, r)| {
                Some(RewriteRule {
                    lhs: Term::parse(self, l).ok()?,
                    rhs: Term::parse(self, r).ok()?,
                })
            })
            .collect();
        Normalizer::from_rules(rules)
    }

    /// Each rule type-expression parsed into its tree once. A pattern that cannot
    /// be built (ambiguous, or no parse) is left out and reads as unresolved.
    #[must_use]
    pub fn type_trees(&self) -> Trees {
        let mut trees = Trees::new();
        for rule in self.rules.values() {
            let bindings = self.rule_bindings(&rule.name);
            for te in rule.type_exprs() {
                if !trees.contains_key(te)
                    && let Ok(ty) = TyExpr::build(self, te, &bindings)
                {
                    trees.insert(te.clone(), ty);
                }
            }
        }
        trees
    }

    pub fn add_production(&mut self, nt: String, prod: Production) {
        if !self.productions.contains_key(&nt) {
            self.nonterminals.push(nt.clone());
        }
        self.productions.entry(nt.clone()).or_default().push(prod);
    }

    pub fn with_start<S: Into<String>>(&mut self, start: S) {
        self.start = Some(start.into());
    }

    #[must_use]
    pub fn start(&self) -> Option<&String> {
        self.start.as_ref()
    }

    pub fn build_bindings(&mut self) {
        self.bindings = Some(binding::build_binding_map(self));
    }

    pub fn build_tokenizer(&mut self) {
        if let Some(t) = &mut self.tokenizer {
            t.build();
        }
    }

    #[must_use]
    pub fn production_count(&self) -> usize {
        self.nonterminals.len()
    }

    #[must_use]
    pub fn production(&self, nt: &str) -> Option<&Vec<Production>> {
        self.productions.get(nt)
    }

    #[must_use]
    pub fn nt(&self, idx: usize) -> Option<&str> {
        self.nonterminals.get(idx).map(std::string::String::as_str)
    }

    #[must_use]
    pub fn nt_index(&self, name: &str) -> Option<usize> {
        self.nonterminals.iter().position(|n| n == name)
    }

    #[must_use]
    pub fn productions_at(&self, idx: usize) -> Option<&Vec<Production>> {
        self.nt(idx).and_then(|nts| self.productions.get(nts))
    }

    #[must_use]
    pub fn prod(&self, pid: ProdId) -> Option<Production> {
        self.productions_at(pid.0)?.get(pid.1).cloned()
    }

    #[must_use]
    pub fn specials(&self) -> Option<&Vec<String>> {
        self.tokenizer.as_ref().map(tokenizer::Tokenizer::specials)
    }

    #[must_use]
    pub fn rules(&self) -> &HashMap<String, TypingRule> {
        &self.rules
    }

    pub fn productions(&self) -> impl Iterator<Item = (&str, &Vec<Production>)> {
        self.productions.iter().map(|(k, v)| (k.as_str(), v))
    }

    #[must_use]
    pub fn nu(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::Terminal { .. } => false,
            Symbol::Nonterminal { name: nt, .. } => {
                let nt = self.productions.get(nt);
                nt.is_some_and(|prod| prod.iter().all(|s| s.rhs.iter().all(|sym| self.nu(sym))))
            }
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Result<Vec<Segment>, String> {
        if self.tokenizer.is_none() {
            self.build_tokenizer();
        }
        let Some(tokenizer) = self.tokenizer.as_mut() else {
            return Err("tokenizer not initialized".to_string());
        };
        tokenizer.tokenize(input)
    }
}
