use crate::debug_debug;
use crate::debug_trace;
use crate::logic::grammar::{Grammar, Symbol};
use crate::logic::typing::tree::{TypedAST, TypedNode};
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use scaffold_macros::allow_copying;
use std::collections::HashSet;

/// The result of computing valid next tokens for a partial parse.
#[derive(Clone, Debug)]
pub struct CompletionSet {
    /// The set of all valid next tokens (deduplicated)
    pub tokens: Vec<DerivativeRegex>,
}

impl CompletionSet {
    fn new(mut tokens: Vec<DerivativeRegex>) -> Self {
        // Deduplicate.
        let unique: HashSet<_> = tokens.drain(..).collect();
        let mut tokens: Vec<_> = unique.into_iter().collect();
        // Deterministic ordering matters for stable, reproducible completion search.
        // Prefer shorter/simpler patterns first, then lexical tie-break.
        tokens.sort_by(|a, b| {
            let pa = a.to_pattern();
            let pb = b.to_pattern();
            pa.len().cmp(&pb.len()).then_with(|| pa.cmp(&pb))
        });
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
        // Remove nullable tokens.
        let tokens: Vec<_> = self
            .tokens
            .iter()
            .filter(|t| !t.is_nullable())
            .cloned()
            .collect();
        Self { tokens }
    }
}

// === Implementation ========================================================================== //

impl TypedAST {
    // careful completions could lead to an unwell-typed tree
    // this is structural compltions froma typed tree.
    pub fn completions(&self, grammar: &Grammar) -> CompletionSet {
        debug_trace!(
            "partial.completion",
            "TypedAST::completions: input='{}', roots={}",
            self.text(),
            self.roots.len()
        );

        let tokens: Vec<_> = self
            .roots
            .iter()
            .flat_map(|root| root.collect_valid_tokens(grammar))
            .collect();

        debug_debug!(
            "partial.completion",
            "TypedAST::completions: input='{}' raw_tokens={:?}",
            self.text(),
            tokens.iter().map(|t| t.to_pattern()).collect::<Vec<_>>()
        );

        CompletionSet::new(tokens).cleanup()
    }
}

impl TypedNode {
    #[allow_copying]
    fn collect_extensions(&self) -> Vec<DerivativeRegex> {
        match self {
            TypedNode::Term {
                extension: Some(ext),
                ..
            } => vec![ext.clone()],
            TypedNode::Expr { children, .. } => {
                if let Some(last) = children.last() {
                    last.collect_extensions()
                } else {
                    vec![]
                }
            }
            _ => vec![],
        }
    }

    pub fn collect_valid_tokens(&self, grammar: &Grammar) -> Vec<DerivativeRegex> {
        let (complete, name, alt_index, children) = match self {
            TypedNode::Expr {
                complete,
                name,
                alt_index,
                children,
                ..
            } => (complete, name, alt_index, children),
            TypedNode::Term { .. } => return vec![],
        };

        // Resolve the production RHS from the grammar (cheap — just two table lookups).
        let rhs = grammar
            .productions
            .get(name)
            .and_then(|alts| alts.get(*alt_index))
            .map(|p| p.rhs.as_slice())
            .unwrap_or(&[]);

        let mut tokens = Vec::new();

        if *complete {
            if let Some(last) = children.last() {
                tokens.extend(last.collect_extensions());
            }
            return tokens;
        }

        if let Some(last_child) = children.last() {
            match last_child {
                TypedNode::Term {
                    remainder: Some(rem),
                    val,
                    ..
                } => {
                    tokens.push(rem.clone());
                    // If we haven't started this token yet, also offer extension of the previous child
                    if val.is_empty() && children.len() >= 2 {
                        if let Some(prev) = children.get(children.len() - 2) {
                            tokens.extend(prev.collect_extensions());
                        }
                    }
                }
                TypedNode::Expr {
                    complete: false, ..
                } => {
                    tokens.extend(last_child.collect_valid_tokens(grammar));
                }
                TypedNode::Expr { complete: true, .. } => {
                    tokens.extend(last_child.collect_extensions());
                    let next_idx = children.len();
                    if let Some(symbol) = rhs.get(next_idx) {
                        tokens.extend(first_set(symbol, grammar));
                    }
                }
                TypedNode::Term {
                    remainder: None,
                    extension,
                    ..
                } => {
                    // Complete terminal
                    if let Some(ext) = extension {
                        tokens.push(ext.clone());
                    }
                    let next_idx = children.len();
                    if let Some(symbol) = rhs.get(next_idx) {
                        tokens.extend(first_set(symbol, grammar));
                    }
                }
            }
        } else {
            // No children yet — offer the first symbol of the production
            if let Some(symbol) = rhs.first() {
                tokens.extend(first_set(symbol, grammar));
            }
        }

        tokens
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
