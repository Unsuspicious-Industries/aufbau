use super::*;
use crate::debug_debug;
use crate::debug_trace;
use crate::logic::grammar::{Grammar, Symbol};
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use std::collections::HashSet;

/// The result of computing valid next tokens for a partial parse.
#[derive(Clone, Debug)]
pub struct CompletionSet {
    /// The set of all valid next tokens (deduplicated)
    pub tokens: Vec<DerivativeRegex>,
}

impl CompletionSet {
    fn new(mut tokens: Vec<DerivativeRegex>) -> Self {
        // Deduplicate and sort
        let unique: HashSet<_> = tokens.drain(..).collect();
        let tokens: Vec<_> = unique.into_iter().collect();
        Self { tokens }
    }

    pub fn iter(&self) -> impl Iterator<Item = &DerivativeRegex> {
        self.tokens.iter()
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn get(&self, idx: usize) -> Option<&DerivativeRegex> {
        self.tokens.get(idx)
    }

    pub fn matches(&self, text: &str) -> bool {
        let text = text.as_ref();
        self.tokens.iter().any(|t| match t.prefix_match(text) {
            PrefixStatus::Extensible(_) | PrefixStatus::Complete | PrefixStatus::Prefix(_) => true,
            PrefixStatus::NoMatch => match DerivativeRegex::from_str(text) {
                Ok(parsed) => &parsed == t,
                Err(_) => false,
            },
        })
    }

    pub fn filtered<F>(&self, mut predicate: F) -> Self
    where
        F: FnMut(&DerivativeRegex) -> bool,
    {
        let tokens = self
            .tokens
            .iter()
            .cloned()
            .filter(|t| predicate(t))
            .collect();
        Self::new(tokens).cleanup()
    }

    pub fn empty() -> Self {
        Self { tokens: Vec::new() }
    }

    pub fn cleanup(&self) -> Self {
        // deduplicate and remove epslons
        let unique: HashSet<_> = self.tokens.clone().drain(..).collect();
        let tokens: Vec<_> = unique.into_iter().filter(|t| !t.is_nullable()).collect();
        Self { tokens }
    }
}

// === Implementation ========================================================================== //

impl PartialAST {
    pub fn completions(&self, grammar: &Grammar) -> CompletionSet {
        debug_trace!(
            "partial.completion",
            "PartialAST::completions: input='{}', roots={}",
            self.input,
            self.roots.len()
        );

        let tokens: Vec<_> = self
            .roots
            .iter()
            .flat_map(|root| root.collect_valid_tokens(grammar))
            .collect();

        debug_debug!(
            "partial.completion",
            "PartialAST::completions: input='{}' raw_tokens={:?}",
            self.input,
            tokens.iter().map(|t| t.to_pattern()).collect::<Vec<_>>()
        );

        CompletionSet::new(tokens).cleanup()
    }
}

impl NonTerminal {
    pub fn collect_valid_tokens(&self, grammar: &Grammar) -> Vec<DerivativeRegex> {
        let mut tokens = Vec::new();

        if self.is_complete() {
            // If complete, we can only extend the last token if it is extensible
            if let Some(last) = self.children.last() {
                tokens.extend(last.collect_extensions());
            }
            return tokens;
        }

        // Partial node: find the frontier
        if let Some(last_child) = self.children.last() {
            match last_child {
                Node::Terminal(Terminal::Partial {
                    remainder: Some(rem),
                    value,
                    ..
                }) => {
                    tokens.push(rem.clone());
                    // Only collect extension from second-to-last if the partial terminal is empty
                    // (i.e., we haven't started typing the next token yet)
                    if value.is_empty() && self.children.len() >= 2 {
                        if let Some(prev) = self.children.get(self.children.len() - 2) {
                            tokens.extend(prev.collect_extensions());
                        }
                    }
                }
                Node::NonTerminal(nt) => {
                    if !nt.is_complete() {
                        tokens.extend(nt.collect_valid_tokens(grammar));
                    } else {
                        // Last child is complete. We need the next symbol in the production
                        // AND we should include any extension from the complete child.
                        tokens.extend(last_child.collect_extensions());
                        let next_idx = self.children.len();
                        if let Some(symbol) = self.production.rhs.get(next_idx) {
                            tokens.extend(first_set(symbol, grammar));
                        }
                    }
                }
                Node::Terminal(Terminal::Complete { extension, .. }) => {
                    // Last child is complete terminal. Include extension AND next symbol.
                    if let Some(ext) = extension {
                        tokens.push(ext.clone());
                    }
                    let next_idx = self.children.len();
                    if let Some(symbol) = self.production.rhs.get(next_idx) {
                        tokens.extend(first_set(symbol, grammar));
                    }
                }
                _ => {}
            }
        } else {
            // No children. First symbol.
            if let Some(symbol) = self.production.rhs.first() {
                tokens.extend(first_set(symbol, grammar));
            }
        }

        tokens
    }
}

impl Node {
    fn collect_extensions(&self) -> Vec<DerivativeRegex> {
        match self {
            Node::Terminal(Terminal::Complete {
                extension: Some(ext),
                ..
            }) => vec![ext.clone()],
            Node::NonTerminal(nt) => {
                if let Some(last) = nt.children.last() {
                    last.collect_extensions()
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }
}

/// Get the FIRST set for a symbol (all tokens that can start this symbol).
fn first_set(symbol: &Symbol, grammar: &Grammar) -> Vec<DerivativeRegex> {
    fn first_set_rec(
        symbol: &Symbol,
        grammar: &Grammar,
        visited: &mut HashSet<String>,
    ) -> Vec<DerivativeRegex> {
        match symbol {
            Symbol::Terminal { regex, .. } => vec![regex.clone()],
            Symbol::Nonterminal { name: nt_name, .. } => {
                if visited.contains(nt_name) {
                    return vec![];
                }
                visited.insert(nt_name.clone());

                let res = if let Some(productions) = grammar.productions.get(nt_name) {
                    productions
                        .iter()
                        .flat_map(|prod| {
                            if let Some(first_sym) = prod.rhs.first() {
                                first_set_rec(first_sym, grammar, visited)
                            } else {
                                vec![]
                            }
                        })
                        .collect()
                } else {
                    vec![]
                };

                visited.remove(nt_name);
                res
            }
        }
    }
    first_set_rec(symbol, grammar, &mut HashSet::new())
}
