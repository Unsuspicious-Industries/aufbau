use crate::regex::{PrefixStatus, Regex};
use std::collections::HashSet;

/// The result of computing valid next tokens for a partial parse.
#[derive(Clone, Debug)]
pub struct CompletionSet {
    /// The set of all valid next tokens (deduplicated)
    pub tokens: Vec<Regex>,
}

impl CompletionSet {
    fn new(mut tokens: Vec<Regex>) -> Self {
        let mut seen = HashSet::new();
        let mut ordered = Vec::with_capacity(tokens.len());
        for token in tokens.drain(..) {
            if seen.insert(token.clone()) {
                ordered.push(token);
            }
        }
        Self { tokens: ordered }
    }

    pub fn from_tokens(tokens: Vec<Regex>) -> Self {
        Self::new(tokens).cleanup()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Regex> {
        self.tokens.iter()
    }

    pub fn len(&self) -> usize {
        self.tokens.len()
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }

    pub fn get(&self, idx: usize) -> Option<&Regex> {
        self.tokens.get(idx)
    }

    pub fn matches(&self, text: &str) -> bool {
        self.tokens.iter().any(|t| match t.prefix_match(text) {
            PrefixStatus::Extensible(_) | PrefixStatus::Complete | PrefixStatus::Prefix(_) => true,
            PrefixStatus::NoMatch => match Regex::from_str(text) {
                Ok(parsed) => &parsed == t,
                Err(_) => false,
            },
        })
    }

    pub fn filtered<F>(&self, mut predicate: F) -> Self
    where
        F: FnMut(&Regex) -> bool,
    {
        let tokens = self
            .tokens
            .iter()
            .filter(|&t| predicate(t))
            .cloned()
            .collect();
        Self::new(tokens).cleanup()
    }

    pub fn empty() -> Self {
        Self { tokens: Vec::new() }
    }

    pub fn cleanup(&self) -> Self {
        let tokens: Vec<_> = self
            .tokens
            .iter()
            .filter(|t| !t.is_nullable())
            .cloned()
            .collect();
        Self { tokens }
    }
}
