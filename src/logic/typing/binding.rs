// ============================================================================
// Binding Resolution
// ============================================================================
//
// Resolves variable bindings in partial ASTs for type checking.
// Maps binding names to tree paths for efficient lookup during evaluation.

use crate::debug_trace;
use crate::logic::binding::GrammarPath;
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use crate::logic::typing::core::TreePath;
use std::collections::HashMap;

pub enum BindError {
    AtFrontier,
    Malformed,
}

#[derive(Debug)]
pub struct Bindings {
    full: HashMap<String, TreePath>,
    partial: HashMap<String, TreePath>,
}

pub enum Binding {
    Full(TreePath),
    Partial(TreePath),
    None,
}

impl Bindings {
    pub fn new() -> Self {
        Bindings {
            full: HashMap::new(),
            partial: HashMap::new(),
        }
    }

    pub fn get_full(&self, name: &str) -> Option<&TreePath> {
        self.full.get(name)
    }

    pub fn get_partial(&self, name: &str) -> Option<&TreePath> {
        self.partial.get(name)
    }

    pub fn get(&self, name: &str) -> Binding {
        if self.full.contains_key(name) {
            Binding::Full(self.full[name].clone())
        } else if self.partial.contains_key(name) {
            Binding::Partial(self.partial[name].clone())
        } else {
            Binding::None
        }
    }

    /// Iterate full (resolved) bindings.
    pub fn iter_full(&self) -> impl Iterator<Item = (&String, &TreePath)> {
        self.full.iter()
    }

    /// Iterate partial (frontier) bindings.
    pub fn iter_partial(&self) -> impl Iterator<Item = (&String, &TreePath)> {
        self.partial.iter()
    }
}

pub fn resolve_bindings(
    nt: &NonTerminal,
    rule_name: &str,
    grammar: &Grammar,
) -> Result<Bindings, BindError> {
    let mut bound = Bindings::new();
    debug_trace!("binding", "Resolving bindings for {}", nt);

    for (name, paths) in grammar.binding_map.bindings_for_rule(rule_name) {
        debug_trace!("binding", "building bindings for {} in {:?}", name, paths);
        for path in paths {
            debug_trace!("binding", "got path for {} : {:?}", name, path);
            match validate_path(nt, path) {
                PathValidationResult::Valid => {
                    debug_trace!("binding", "valid path for {} : {:?}", name, path);
                    // Validate path with specific frontier handling
                    if is_extensible_path(nt, &path.idxs()) {
                        debug_trace!(
                            "binding",
                            "setting partial binding for {} : {:?}",
                            name,
                            path
                        );
                        // Keep partial only when no full binding exists.
                        bound
                            .partial
                            .entry(name.to_string())
                            .or_insert_with(|| path.idxs());
                    } else {
                        // Prefer concrete/full bindings over partial ones.
                        let key = name.to_string();
                        if !bound.full.contains_key(&key) {
                            bound.full.insert(key.clone(), path.idxs());
                        }
                        bound.partial.remove(&key);
                    }
                }
                PathValidationResult::Partial => {
                    let key = name.to_string();
                    if !bound.full.contains_key(&key) {
                        bound.partial.entry(key).or_insert_with(|| path.idxs());
                    }
                }
                PathValidationResult::Invalid => {
                    // skip invalid paths
                }
            }
        } // should be okay
    }

    Ok(bound)
}

enum PathValidationResult {
    Valid,
    Invalid,
    Partial,
}

fn validate_path(nt: &NonTerminal, p: &GrammarPath) -> PathValidationResult {
    match p.forward() {
        Some((step, rest)) => {
            debug_trace!(
                "validate_path",
                "Checking {} ?= {} and (children {:?} vs {:?}) with {:?}",
                nt.alternative_index,
                step.a(),
                nt.children.len(),
                step.i,
                rest
            );
            if nt.alternative_index != step.a() {
                return PathValidationResult::Invalid;
            }
            match nt.get(step.i) {
                Ok(Some(child)) => match child {
                    Node::NonTerminal(nt) => validate_path(&nt, &rest),
                    Node::Terminal(_) => match rest.is_empty() {
                        true => PathValidationResult::Valid,
                        false => PathValidationResult::Invalid,
                    },
                },
                // None indicates frontier node requiring special handling
                Ok(None) => PathValidationResult::Partial,
                Err(e) => panic!("Grammar path error: {}", e),
            }
        }
        None => PathValidationResult::Valid,
    }
}

fn node_is_extensible(node: &Node) -> bool {
    match node {
        Node::NonTerminal(nt) => nt.is_extensible(),
        Node::Terminal(Terminal::Complete { extension, .. }) => extension.is_some(),
        Node::Terminal(Terminal::Partial { .. }) => true,
    }
}

pub fn is_extensible_path(nt: &NonTerminal, p: &TreePath) -> bool {
    match p.first() {
        Some(i) => {
            if nt.is_frontier(*i) {
                return true;
            }

            // Extensible bindings must live on the rightmost spine ("last node").
            // This avoids marking early bindings (e.g. lambda parameter/type) as partial
            // just because their regex could still extend.
            if nt.children.is_empty() || *i != nt.children.len() - 1 {
                return false;
            }

            match nt.get(*i) {
                Ok(Some(child)) => match child {
                    Node::NonTerminal(nt_child) => {
                        if p.len() == 1 {
                            node_is_extensible(child)
                        } else {
                            is_extensible_path(nt_child, &p[1..].to_vec())
                        }
                    }
                    Node::Terminal(_) => p.len() == 1 && node_is_extensible(child),
                },

                Ok(None) => false,
                Err(e) => panic!("Grammar path error: {}", e),
            }
        }
        None => false,
    }
}
