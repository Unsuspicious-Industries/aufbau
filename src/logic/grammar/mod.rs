pub mod load;
pub mod save;
pub mod tokenizer;
pub mod utils;

use crate::logic::binding::{self, BindingMap};
pub use tokenizer::{DEFAULT_DELIMITERS, Segment, Tokenizer};

#[cfg(test)]
mod tests;

use crate::regex::Regex as DerivativeRegex;
use std::collections::HashMap;
use std::ffi::os_str::Display;
use std::hash::{Hash, Hasher};

// ANCHOR: Symbol
#[derive(Debug, Clone)]
pub enum Symbol {
    Nonterminal {
        name: String,
        binding: Option<String>,
    },
    Terminal {
        regex: DerivativeRegex,
        binding: Option<String>,
    },
}
// ANCHOR_END: Symbol

impl Eq for Symbol {}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Symbol::Nonterminal {
                    name: a,
                    binding: ba,
                },
                Symbol::Nonterminal {
                    name: b,
                    binding: bb,
                },
            ) => a == b && ba == bb,
            (
                Symbol::Terminal {
                    regex: a,
                    binding: ba,
                    ..
                },
                Symbol::Terminal {
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
            Symbol::Nonterminal { name, binding } => {
                0u8.hash(state);
                name.hash(state);
                binding.hash(state);
            }
            Symbol::Terminal { regex, binding, .. } => {
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
            Symbol::Terminal {
                regex: DerivativeRegex::literal(&literal),
                binding: None,
            }
        } else if value.starts_with('"') && value.ends_with('"') {
            let literal = value[1..value.len() - 1].to_string();
            Symbol::Terminal {
                regex: DerivativeRegex::literal(&literal),
                binding: None,
            }
        } else if value.starts_with('/') && value.ends_with('/') && value.len() > 2 {
            let pattern = value[1..value.len() - 1].to_string();
            Symbol::Terminal {
                regex: DerivativeRegex::new(&pattern).expect("invalid regex literal"),
                binding: None,
            }
        } else {
            Symbol::Nonterminal {
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
            Symbol::Nonterminal { binding: slot, .. } | Symbol::Terminal { binding: slot, .. } => {
                *slot = Some(binding);
            }
        }
        self
    }

    pub fn binding(&self) -> Option<&String> {
        match self {
            Symbol::Nonterminal { binding, .. } | Symbol::Terminal { binding, .. } => {
                binding.as_ref()
            }
        }
    }

    pub fn has_binding(&self) -> bool {
        self.binding().is_some()
    }

    pub fn is_regex(&self) -> bool {
        matches!(self, Symbol::Terminal { .. })
    }

    pub fn is_nonterminal(&self) -> bool {
        matches!(self, Symbol::Nonterminal { .. })
    }
}

/// A single production rule `left ::= right₀ right₁ …`.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Production {
    pub rule: Option<String>,
    pub rhs: Vec<Symbol>,
}

impl std::fmt::Display for Production {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let symbols: Vec<String> = self
            .rhs
            .iter()
            .map(|s| match s {
                Symbol::Nonterminal { name, .. } => name.clone(),
                Symbol::Terminal { regex, .. } => format!("/{}/", regex.to_pattern()),
            })
            .collect();
        write!(f, "{}", symbols.join(" "))
    }
}

use crate::debug_trace;
use crate::logic::typing::TypingRule;

/// A complete grammar consisting of context-free productions and
/// inference-style typing rules.
#[derive(Debug, Clone)]
pub struct Grammar {
    pub productions: HashMap<String, Vec<Production>>,
    pub typing_rules: HashMap<String, TypingRule>,
    pub special_tokens: Vec<String>,
    pub delimiters: Vec<char>,
    pub start: Option<String>,
    pub binding_map: BindingMap,
    /// Cached tokenizer (built lazily from special_tokens and delimiters)
    tokenizer: Option<Tokenizer>,
}

// Note: Typing rules are intentionally excluded from equality comparison
// for performance reasons. This may need revision if type-aware comparison
// becomes necessary.
impl PartialEq for Grammar {
    fn eq(&self, other: &Self) -> bool {
        self.productions == other.productions
            && self.special_tokens == other.special_tokens
            && self.delimiters == other.delimiters
            && self.start == other.start
    }
}

impl Default for Grammar {
    fn default() -> Self {
        Self {
            productions: HashMap::default(),
            typing_rules: HashMap::default(),
            special_tokens: Vec::default(),
            delimiters: DEFAULT_DELIMITERS.to_vec(),
            start: None,
            binding_map: BindingMap::new(),
            tokenizer: None,
        }
    }
}

impl Grammar {
    /// Create an empty grammar
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a grammar with the given productions
    pub fn new_with_productions(productions: HashMap<String, Vec<Production>>) -> Self {
        let mut grammar = Self::default();
        grammar.productions = productions;
        grammar
    }

    /// Rebuild the binding map from the current productions and typing rules
    pub fn rebuild_bindings(&mut self) {
        self.binding_map = binding::build_binding_map(self);
    }

    /// Add a special token to the grammar if not already present.
    pub fn add_special_token(&mut self, token: String) {
        if !self.special_tokens.contains(&token) {
            self.special_tokens.push(token);
            self.tokenizer = None; // Invalidate cache
        }
    }

    /// Add a typing rule to the grammar.
    pub fn add_typing_rule(&mut self, rule: TypingRule) {
        self.typing_rules.insert(rule.name.clone(), rule);
    }

    /// Add a production rule to the grammar.
    pub fn add_production(&mut self, nt: String, prod: Production) {
        self.productions.entry(nt.clone()).or_default().push(prod);
    }

    /// Set the start nonterminal.
    pub fn set_start<S: Into<String>>(&mut self, start: S) {
        self.start = Some(start.into());
    }

    /// Get the start nonterminal if available.
    pub fn start_nonterminal(&self) -> Option<&String> {
        self.start.as_ref()
    }

    /// Check if a symbol is nullable (can match zero tokens).
    pub fn symbol_nullable(&self, symbol: &Symbol) -> bool {
        match symbol {
            Symbol::Terminal { .. } => false,
            Symbol::Nonterminal { name: nt, .. } => {
                let nt = self.productions.get(nt);
                nt.map(|prod| {
                    prod.iter()
                        .all(|s| s.rhs.iter().all(|sym| self.symbol_nullable(sym)))
                })
                .unwrap_or(false)
            }
        }
    }

    /// Build and cache the tokenizer from current special_tokens and delimiters.
    pub fn prepare_tokenizer(&mut self) {
        if self.tokenizer.is_none() {
            self.tokenizer = Some(Tokenizer::new(
                self.special_tokens.clone(),
                self.delimiters.clone(),
            ));
        }
    }

    /// Tokenize input using the grammar's special tokens and delimiters.
    pub fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        let result = match &self.tokenizer {
            Some(tok) => tok.tokenize(input),
            None => {
                // Build tokenizer on-the-fly if not cached
                let tok = Tokenizer::new(self.special_tokens.clone(), self.delimiters.clone());
                tok.tokenize(input)
            }
        };
        match result {
            Ok(segments) => Ok(segments),
            Err(err) => Err(err),
        }
    }

    pub fn nt_regex(&self, nt: &String) -> DerivativeRegex {
        let nt = self.productions.get(nt);
        nt.map(|prod| {
            let mut regexes = Vec::new();
            for prod in prod {
                let mut prod_regexes = Vec::new();
                for sym in &prod.rhs {
                    prod_regexes.push(self.symbol_regex(sym.clone()));
                }
                regexes.push(DerivativeRegex::concat_many(prod_regexes));
            }
            DerivativeRegex::union_many(regexes)
        })
        .unwrap_or(DerivativeRegex::Epsilon)
    }

    pub fn symbol_regex(&self, symbol: Symbol) -> DerivativeRegex {
        match symbol {
            Symbol::Nonterminal { name, .. } => self.nt_regex(&name),
            Symbol::Terminal { regex, .. } => regex.clone(),
        }
    }

    pub fn as_regex(&self) -> DerivativeRegex {
        match self.start_nonterminal() {
            Some(start) => self.nt_regex(start),
            None => DerivativeRegex::Epsilon,
        }
    }
}
