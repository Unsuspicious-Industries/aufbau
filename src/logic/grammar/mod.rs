pub mod load;
pub mod save;
pub mod utils;
pub mod desugar;

#[cfg(test)]
mod tests;

use crate::regex::Regex as DerivativeRegex;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

#[derive(Debug, Clone)]
pub enum Symbol {
    Litteral(String),
    Expression(String),
    Regex(DerivativeRegex),
    Single {
        value: Box<Symbol>,
        binding: Option<String>,
        repetition: Option<(usize, Option<usize>)>,
    },
}

impl Eq for Symbol {}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Symbol::Litteral(a), Symbol::Litteral(b)) => a == b,
            (Symbol::Expression(a), Symbol::Expression(b)) => a == b,
            (Symbol::Regex(a), Symbol::Regex(b)) => a.equiv(b),
            (
                Symbol::Single {
                    value: va,
                    binding: ba,
                    repetition: ra,
                },
                Symbol::Single {
                    value: vb,
                    binding: bb,
                    repetition: rb,
                },
            ) => va == vb && ba == bb && ra == rb,
            _ => false,
        }
    }
}

impl Hash for Symbol {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Symbol::Litteral(s) => {
                0u8.hash(state);
                s.hash(state);
            }
            Symbol::Expression(s) => {
                1u8.hash(state);
                s.hash(state);
            }
            Symbol::Regex(r) => {
                2u8.hash(state);
                r.to_pattern().hash(state);
            }
            Symbol::Single {
                value,
                binding,
                repetition,
            } => {
                3u8.hash(state);
                value.hash(state);
                binding.hash(state);
                repetition.hash(state);
            }
        }
    }
}

// Repetition is represented uniformly as a range (min, max), where max=None means unbounded.

impl Symbol {
    pub fn new(value: String) -> Self {
        debug_trace!("grammar", "Creating symbol from value: {}", value);
        if value.starts_with('\'') && value.ends_with('\'') {
            Self::Litteral(value[1..value.len() - 1].to_string())
        } else if value.starts_with('/') && value.ends_with('/') && value.len() > 2 {
            Self::Regex(DerivativeRegex::new(&value[1..value.len() - 1]).unwrap())
        } else {
            Self::Expression(value)
        }
    }
    pub fn with_binding(value: String, binding: String) -> Self {
        Self::Single {
            value: Box::new(Self::new(value)),
            binding: Some(binding),
            repetition: None,
        }
    }
    pub fn with_repetition(value: String, repetition: (usize, Option<usize>)) -> Self {
        Self::Single {
            value: Box::new(Self::new(value)),
            binding: None,
            repetition: Some(repetition),
        }
    }
    pub fn with_binding_and_repetition(
        value: String,
        binding: String,
        repetition: (usize, Option<usize>),
    ) -> Self {
        Self::Single {
            value: Box::new(Self::new(value)),
            binding: Some(binding),
            repetition: Some(repetition),
        }
    }

    pub fn value(&self) -> String {
        match self {
            Symbol::Litteral(value) => value.clone(),
            Symbol::Expression(value) => value.clone(),
            Symbol::Regex(regex) => format!("/{}/", regex.to_pattern()),
            Symbol::Single { value, .. } => value.value(),
        }
    }
    pub fn binding(&self) -> Option<&String> {
        match self {
            Symbol::Single { binding, .. } => binding.as_ref(),
            _ => None,
        }
    }
    pub fn repetition(&self) -> Option<&(usize, Option<usize>)> {
        match self {
            Symbol::Single { repetition, .. } => repetition.as_ref(),
            _ => None,
        }
    }
    pub fn has_binding(&self) -> bool {
        self.binding().is_some()
    }
    pub fn is_litteral(&self) -> bool {
        matches!(self, Symbol::Litteral(_))
    }
    pub fn is_regex(&self) -> bool {
        matches!(self, Symbol::Regex(_))
    }
    pub fn is_terminal(&self) -> bool {
        self.is_litteral() || self.is_regex()
    }
    pub fn is_nonterminal(&self) -> bool {
        !self.is_terminal()
    }
}

/// Convenience alias for non-terminal symbols.
pub type Nonterminal = String;
/// A single production rule `left ::= right₀ right₁ …`.
#[derive(Debug, Clone, PartialEq)]
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
}

impl PartialEq for Grammar {
    fn eq(&self, other: &Self) -> bool {
        // Compare everything except accepted_tokens_regex
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
        }
    }
}

impl Grammar {
    /// Create an empty grammar
    pub fn new() -> Self {
        Self::default()
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
            Symbol::Litteral(_) | Symbol::Regex(_) => false,

            Symbol::Expression(nt) => {
                let nt = self.productions.get(nt);
                nt.map(|prod| {
                    prod.iter()
                        .all(|s| s.rhs.iter().all(|sym| self.symbol_nullable(sym)))
                })
                .unwrap_or(false)
            }

            Symbol::Single { repetition, .. } => repetition.map(|(min, _)| min == 0).unwrap_or(false)
            
        }
    }
}

