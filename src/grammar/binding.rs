// Grammar paths implement the β(b, p) construction from the spec with
// acyclic paths only.
// Recursive segments are truncated after MAX_RECURSION_DEPTH.
// PLANNED FIX:
// generalise to regular path expressions (finite automata over grammar steps),
// but this is not yet specced.

// Example: for STLC `Abstraction(abs)` the path `[(3, Some(1)), (0, None)]`
// mirrors the formal β(τ₁, abs) = 3@1·0 constraint.
use std::collections::{HashMap, HashSet};

use crate::engine::grammar::{Production, SPG, Symbol};
pub use crate::engine::path::{GrammarPath, PathStep};

const MAX_RECURSION_DEPTH: usize = 16;

/// Mapping from semantic rule name -> (binding name -> grammar paths).
///
/// `pos(n, a, b)` from `prop:binding-uniqueness` — each binding appears at
/// exactly one child position per alternative, enforced by binding-regularity.
#[derive(Debug, Clone, Default)]
pub struct BindingMap {
    // map:     rule    ->    {   binding :   [path]  }
    map: HashMap<String, HashMap<String, Vec<GrammarPath>>>,
}

impl BindingMap {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Insert a new path for the provided rule/binding pair.
    pub fn insert(&mut self, rule: &str, binding: &str, path: GrammarPath) {
        let rule_entry = self.map.entry(rule.to_string()).or_default();
        let binding_entry = rule_entry.entry(binding.to_string()).or_default();

        if !binding_entry.contains(&path) {
            binding_entry.push(path);
            binding_entry.sort();
        }
    }

    /// Retrieve the grammar paths for a (binding, rule) pair if present.
    #[must_use]
    pub fn get(&self, binding: &str, rule: &str) -> Option<&[GrammarPath]> {
        self.map
            .get(rule)
            .and_then(|bindings| bindings.get(binding))
            .map(std::vec::Vec::as_slice)
    }

    /// Return all bindings known for a given rule name.
    pub fn bindings_for_rule(&self, rule: &str) -> impl Iterator<Item = (&str, &[GrammarPath])> {
        self.map.get(rule).into_iter().flat_map(|bindings| {
            bindings
                .iter()
                .map(|(binding, paths)| (binding.as_str(), paths.as_slice()))
        })
    }
}

/// Build the binding map for an entire grammar by enumerating grammar paths for
/// every semantic-rule-bearing production.
///
/// This currently enumerates acyclic paths only; recursive bindings will be
/// generalised into regular path expressions in future iterations.
#[must_use]
pub fn build_binding_map(grammar: &SPG) -> BindingMap {
    let mut binding_map = BindingMap::new();

    for (nt_name, productions) in &grammar.productions {
        let Some(rule_name) = grammar.nt_rule(nt_name) else {
            continue;
        };

        for (alt_idx, production) in productions.iter().enumerate() {
            let mut path = GrammarPath::new();
            let mut stack = HashSet::new();
            collect_paths(
                grammar,
                nt_name,
                alt_idx,
                production,
                rule_name,
                &mut path,
                &mut stack,
                &mut binding_map,
            );
        }
    }

    binding_map
}

#[allow(clippy::too_many_arguments)]
fn collect_paths(
    grammar: &SPG,
    current_nt: &str,
    current_alt: usize,
    production: &Production,
    rule_name: &str,
    path: &mut GrammarPath,
    recursion_stack: &mut HashSet<(String, usize)>,
    binding_map: &mut BindingMap,
) {
    let frame = (current_nt.to_string(), current_alt);
    if !recursion_stack.insert(frame.clone()) {
        // Stop exploring to avoid infinite recursion. Recursive paths will be
        // generalised to regular expressions in a follow-up iteration.
        return;
    }

    if path.len() >= MAX_RECURSION_DEPTH {
        recursion_stack.remove(&frame);
        return;
    }

    for (child_idx, symbol) in production.rhs.iter().enumerate() {
        match symbol {
            Symbol::Terminal { binding, .. } => {
                if let Some(binding_name) = binding {
                    path.push(PathStep::new(child_idx, current_alt));
                    binding_map.insert(rule_name, binding_name, path.clone());
                    path.pop();
                }
            }
            Symbol::Nonterminal { name, binding, .. } => {
                // Binding attached directly to the child non-terminal.
                if let Some(binding_name) = binding {
                    path.push(PathStep::new(child_idx, current_alt));
                    binding_map.insert(rule_name, binding_name, path.clone());
                    path.pop();
                }

                if let Some(child_productions) = grammar.productions.get(name) {
                    for (child_alt, child_prod) in child_productions.iter().enumerate() {
                        // If the child nonterminal carries its own semantic rule, it's a boundary.
                        // We should not look for bindings for the current `rule_name` inside it.
                        if grammar.nt_rule(name).is_some() {
                            continue;
                        }

                        // PathStep stores the current node's alternative index.
                        // We are still at `current_nt` here.
                        path.push(PathStep::new(child_idx, current_alt));
                        collect_paths(
                            grammar,
                            name,
                            child_alt,
                            child_prod,
                            rule_name,
                            path,
                            recursion_stack,
                            binding_map,
                        );
                        path.pop();
                    }
                }
            }
        }
    }

    recursion_stack.remove(&frame);
}
