// ============================================================================
// Binding Resolution
// ============================================================================
//
// Resolves variable bindings in partial ASTs for type checking.
// Maps binding names to tree paths for efficient lookup during evaluation.

use crate::debug_trace;
use crate::logic::binding::GrammarPath;
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal};
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
                    if is_frontier(nt, &path.idxs()) {
                        debug_trace!(
                            "binding",
                            "setting partial binding for {} : {:?}",
                            name,
                            path
                        );
                        bound.partial.insert(name.to_string(), path.idxs());
                    } else {
                        bound.full.insert(name.to_string(), path.idxs());
                    }
                }
                PathValidationResult::Partial => {
                    bound.partial.insert(name.to_string(), path.idxs());
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

pub fn is_frontier(nt: &NonTerminal, p: &TreePath) -> bool {
    match p.first() {
        Some(i) => {
            debug_trace!(
                "is_frontier",
                "Checking {} ?= {} and (children {:?}) with {:?}",
                i,
                nt.children.len() - 1,
                i,
                p
            );
            if nt.is_frontier(*i) {
                return true;
            }
            match nt.get(*i) {
                Ok(Some(child)) => match child {
                    Node::NonTerminal(nt) => is_frontier(&nt, &p[1..].to_vec()),
                    Node::Terminal(_) => false,
                },

                Ok(None) => false,
                Err(e) => panic!("Grammar path error: {}", e),
            }
        }
        None => false,
    }
}
