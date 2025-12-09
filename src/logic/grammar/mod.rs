pub mod binding;
pub mod load;
pub mod save;
pub mod utils;

#[cfg(test)]
mod tests;

use crate::regex::Regex as DerivativeRegex;
use binding::BindingMap;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum Symbol {
    Expression {
        name: String,
        binding: Option<String>,
    },
    Regex {
        regex: DerivativeRegex,
        binding: Option<String>,
    },
}

impl Eq for Symbol {}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Symbol::Expression {
                    name: a,
                    binding: ba,
                },
                Symbol::Expression {
                    name: b,
                    binding: bb,
                },
            ) => a == b && ba == bb,
            (
                Symbol::Regex {
                    regex: a,
                    binding: ba,
                    ..
                },
                Symbol::Regex {
                    regex: b,
                    binding: bb,
                    ..
                },
            ) => a.equiv(b) && ba == bb,
            _ => false,
        }
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Expression { name, binding } => {
                0u8.hash(state);
                name.hash(state);
                binding.hash(state);
            }
            Symbol::Regex { regex, binding, .. } => {
                1u8.hash(state);
                regex.to_pattern().hash(state);
                binding.hash(state);
            }
        }
    }
}

impl Symbol {
    pub fn new(value: String) -> Self {
        debug_trace!("grammar", "Creating symbol from value: {}", value);
        if value.starts_with('\'') && value.ends_with('\'') {
            let literal = value[1..value.len() - 1].to_string();
            Self::Regex {
                regex: DerivativeRegex::literal(&literal),
                binding: None,
            }
        } else if value.starts_with('"') && value.ends_with('"') {
            let literal = value[1..value.len() - 1].to_string();
            Self::Regex {
                regex: DerivativeRegex::literal(&literal),
                binding: None,
            }
        } else if value.starts_with('/') && value.ends_with('/') && value.len() > 2 {
            let pattern = value[1..value.len() - 1].to_string();
            Self::Regex {
                regex: DerivativeRegex::new(&pattern).expect("invalid regex literal"),
                binding: None,
            }
        } else {
            Self::Expression {
                name: value,
                binding: None,
            }
        }
    }

    pub fn with_binding(value: String, binding: String) -> Self {
        Self::new(value).attach_binding(binding)
    }

    pub fn attach_binding(mut self, binding: String) -> Self {
        match &mut self {
            Symbol::Expression { binding: slot, .. } | Symbol::Regex { binding: slot, .. } => {
                *slot = Some(binding);
            }
        }
        self
    }

    pub fn binding(&self) -> Option<&String> {
        match self {
            Symbol::Expression { binding, .. } | Symbol::Regex { binding, .. } => binding.as_ref(),
        }
    }

    pub fn has_binding(&self) -> bool {
        self.binding().is_some()
    }

    pub fn is_regex(&self) -> bool {
        matches!(self, Symbol::Regex { .. })
    }

    pub fn is_nonterminal(&self) -> bool {
        matches!(self, Symbol::Expression { .. })
    }
}

/// Convenience alias for non-terminal symbols.
pub type Nonterminal = String;
/// A single production rule `left ::= right₀ right₁ …`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Production {
    pub rule: Option<String>,
    pub rhs: Vec<Symbol>,
}

use crate::debug_trace;
use crate::logic::typing::TypingRule;

/// A complete grammar consisting of context-free productions and
/// inference-style typing rules.
#[derive(Debug, Clone)]
pub struct Grammar {
    pub productions: HashMap<Nonterminal, Vec<Production>>,
    pub typing_rules: HashMap<String, TypingRule>, // name -> rule
    pub special_tokens: Vec<String>,
    // Optional explicit start nonterminal for parsing
    pub start: Option<Nonterminal>,
    // Preserve declaration order of productions as they appear in the spec
    pub production_order: Vec<Nonterminal>,
    // Regex representing the union of all accepted tokens (special tokens + regex patterns)
    pub accepted_tokens_regex: Option<DerivativeRegex>,
    // Binding map for resolving bindings in typing rules
    pub binding_map: BindingMap,
}

impl PartialEq for Grammar {
    fn eq(&self, other: &Self) -> bool {
        // Compare everything except accepted_tokens_regex and binding_map
        self.productions == other.productions
            && self.typing_rules == other.typing_rules
            && self.special_tokens == other.special_tokens
            && self.start == other.start
            && self.production_order == other.production_order
    }
}

impl Default for Grammar {
    fn default() -> Self {
        Self {
            productions: HashMap::default(),
            typing_rules: HashMap::default(),
            special_tokens: Vec::default(),
            start: None,
            production_order: Vec::default(),
            accepted_tokens_regex: None,
            binding_map: BindingMap::new(),
        }
    }
}

impl Grammar {
    /// Create an empty grammar
    pub fn new() -> Self {
        Self::default()
    }

    /// Rebuild the binding map from the current productions and typing rules
    pub fn rebuild_bindings(&mut self) {
        self.binding_map = binding::build_binding_map(self);
    }

    /// Add a special token to the grammar if not already present.
    pub fn add_special_token(&mut self, token: String) {
        if !self.special_tokens.contains(&token) {
            self.special_tokens.push(token);
        }
    }

    /// Add a typing rule to the grammar.
    pub fn add_typing_rule(&mut self, rule: TypingRule) {
        self.typing_rules.insert(rule.name.clone(), rule);
    }

    /// Set the start nonterminal.
    pub fn set_start<S: Into<Nonterminal>>(&mut self, start: S) {
        self.start = Some(start.into());
    }

    /// Get the start nonterminal if available.
    pub fn start_nonterminal(&self) -> Option<&Nonterminal> {
        self.start.as_ref()
    }

    /// Check if a symbol is nullable (can match zero tokens).
    pub fn symbol_nullable(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::Regex { .. } => false,
            Symbol::Expression { name: nt, .. } => {
                let nt = self.productions.get(nt);
                nt.map(|prod| {
                    prod.iter()
                        .all(|s| s.rhs.iter().all(|sym| self.symbol_nullable(sym)))
                })
                .unwrap_or(false)
            }
        }
    }
}
