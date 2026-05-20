use crate::debug_trace;
use crate::regex::Regex;
use std::hash::{Hash, Hasher};

// ANCHOR: Symbol
#[derive(Debug, Clone)]
pub enum Symbol {
    Nonterminal {
        name: String,
        binding: Option<String>,
    },
    Terminal {
        regex: Regex,
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
            Symbol::Terminal { regex, binding } => {
                1u8.hash(state);
                regex.to_pattern().hash(state);
                binding.hash(state);
            }
        }
    }
}

impl Symbol {
    pub fn new(value: String) -> Result<Self, String> {
        debug_trace!("grammar", "Creating symbol from value: {}", value);
        if value.len() >= 2
            && ((value.starts_with('\'') && value.ends_with('\''))
                || (value.starts_with('"') && value.ends_with('"')))
        {
            let literal = value[1..value.len() - 1].to_string();
            Ok(Symbol::Terminal {
                regex: Regex::literal(&literal),
                binding: None,
            })
        } else if value.starts_with('/') && value.ends_with('/') && value.len() > 2 {
            let pattern = value[1..value.len() - 1].to_string();
            Ok(Symbol::Terminal {
                regex: Regex::new(&pattern)?,
                binding: None,
            })
        } else {
            Ok(Symbol::Nonterminal {
                name: value,
                binding: None,
            })
        }
    }

    #[must_use] 
    pub fn attach_binding(mut self, binding: String) -> Self {
        match &mut self {
            Symbol::Nonterminal { binding: slot, .. } | Symbol::Terminal { binding: slot, .. } => {
                *slot = Some(binding);
            }
        }
        self
    }

    #[must_use] 
    pub fn binding(&self) -> Option<&String> {
        match self {
            Symbol::Nonterminal { binding, .. } | Symbol::Terminal { binding, .. } => {
                binding.as_ref()
            }
        }
    }

    #[must_use] 
    pub fn has_binding(&self) -> bool {
        self.binding().is_some()
    }
}
