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

#[derive(Debug)]
pub enum BindError {
    AtFrontier,
    Malformed,
}

#[derive(Debug)]
pub struct Bindings {
    full: HashMap<String, TreePath>,
    partial: HashMap<String, TreePath>,
    full_grouped: HashMap<String, Vec<TreePath>>,
    partial_grouped: HashMap<String, Vec<TreePath>>,
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
            full_grouped: HashMap::new(),
            partial_grouped: HashMap::new(),
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

    /// All concrete/full paths for a binding name.
    pub fn get_full_group(&self, name: &str) -> Option<&[TreePath]> {
        self.full_grouped.get(name).map(|paths| paths.as_slice())
    }

    /// All frontier/partial paths for a binding name.
    pub fn get_partial_group(&self, name: &str) -> Option<&[TreePath]> {
        self.partial_grouped.get(name).map(|paths| paths.as_slice())
    }

    pub fn iter_full_grouped(&self) -> impl Iterator<Item = (&String, &[TreePath])> {
        self.full_grouped
            .iter()
            .map(|(name, paths)| (name, paths.as_slice()))
    }

    pub fn iter_partial_grouped(&self) -> impl Iterator<Item = (&String, &[TreePath])> {
        self.partial_grouped
            .iter()
            .map(|(name, paths)| (name, paths.as_slice()))
    }

    fn push_full(&mut self, name: &str, path: TreePath) {
        let key = name.to_string();
        let paths = self.full_grouped.entry(key.clone()).or_default();
        if !paths.contains(&path) {
            paths.push(path.clone());
        }
        if !self.full.contains_key(&key) {
            self.full.insert(key.clone(), path);
        }
        self.partial.remove(&key);
        self.partial_grouped.remove(&key);
    }

    fn push_partial(&mut self, name: &str, path: TreePath) {
        let key = name.to_string();
        if self.full.contains_key(&key) {
            return;
        }
        let paths = self.partial_grouped.entry(key.clone()).or_default();
        if !paths.contains(&path) {
            paths.push(path.clone());
        }
        self.partial.entry(key).or_insert(path);
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
                        bound.push_partial(name, path.idxs());
                    } else {
                        // Prefer concrete/full bindings over partial ones.
                        bound.push_full(name, path.idxs());
                    }
                }
                PathValidationResult::Partial => {
                    bound.push_partial(name, path.idxs());
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::partial::Parser;

    #[test]
    fn resolve_bindings_keeps_grouped_matches() {
        let spec = r#"
        Number(num) ::= /[0-9]+/
        Pair(pair) ::= Number[x] ',' Number[x] ';'

        Γ ⊢ x : 'number'
        ----------------- (pair)
        'number'
        "#;

        let grammar = Grammar::load(spec).expect("load pair grammar");
        let mut parser = Parser::new(grammar.clone());
        let ast = parser.parse("1 , 2 ;").expect("parse pair");
        let root = ast.complete().expect("complete pair root");

        let bindings = resolve_bindings(&root, "pair", &grammar).expect("resolve bindings");
        let grouped = bindings
            .get_full_group("x")
            .expect("grouped full bindings for x");

        assert_eq!(grouped.len(), 2);
        assert_eq!(grouped[0], vec![0]);
        assert_eq!(grouped[1], vec![2]);
        assert_eq!(bindings.get_full("x"), Some(&vec![0]));
    }
}
