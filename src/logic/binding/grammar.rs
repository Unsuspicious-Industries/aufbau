// Grammar paths implement the β(b, p) construction from the spec with
// acyclic regular paths only
// Recursive segments are truncated after MAX_RECURSION_DEPTH
// until the regular-path generalisation lands. Example: for STLC `Abstraction(abs)` the path
// `[(3, Some(1)), (0, None)]` mirrors the formal β(τ₁, abs) = 3@1·0 constraint.
use std::collections::{HashMap, HashSet};

use crate::logic::grammar::{Grammar, Production, Symbol};

const MAX_RECURSION_DEPTH: usize = 16;

/// A single transition in a grammar path. The parser traverses to `i`
/// and optionally constrains the child non-terminal to `a`.
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PathStep {
    pub i: usize,
    pub a: Option<usize>,
}

impl PathStep {
    pub fn new(i: usize, a: Option<usize>) -> Self {
        Self { i, a }
    }
}

/// A concrete grammar path represented as a finite sequence of `PathStep`s.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Default)]
pub struct GrammarPath {
    steps: Vec<PathStep>,
}

impl Ord for GrammarPath {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        match self.steps.len().cmp(&other.steps.len()) {
            std::cmp::Ordering::Equal => self.steps.cmp(&other.steps),
            ord => ord,
        }
    }
}

impl PartialOrd for GrammarPath {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl GrammarPath {
    pub fn new() -> Self {
        Self { steps: Vec::new() }
    }

    pub fn push(&mut self, step: PathStep) {
        self.steps.push(step);
    }

    pub fn pop(&mut self) {
        self.steps.pop();
    }

    pub fn is_empty(&self) -> bool {
        self.steps.is_empty()
    }

    pub fn len(&self) -> usize {
        self.steps.len()
    }

    pub fn steps(&self) -> &[PathStep] {
        &self.steps
    }
}

impl From<Vec<PathStep>> for GrammarPath {
    fn from(steps: Vec<PathStep>) -> Self {
        Self { steps }
    }
}

/// Mapping from typing rule name -> (binding name -> grammar paths).
#[derive(Debug, Clone, Default)]
pub struct BindingMap {
    // map:     rule    ->    {   binding :   [path]  }
    map: HashMap<String, HashMap<String, Vec<GrammarPath>>>,
}

impl BindingMap {
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
    pub fn get(&self, binding: &str, rule: &str) -> Option<&[GrammarPath]> {
        self.map
            .get(rule)
            .and_then(|bindings| bindings.get(binding))
            .map(|paths| paths.as_slice())
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
/// every typing-rule-bearing production.
///
/// This currently enumerates acyclic paths only; recursive bindings will be
/// generalised into regular path expressions in future iterations.
pub fn build_binding_map(grammar: &Grammar) -> BindingMap {
    let mut binding_map = BindingMap::new();

    for (nt_name, productions) in &grammar.productions {
        for (alt_idx, production) in productions.iter().enumerate() {
            let rule_name = match &production.rule {
                Some(rule) => rule.clone(),
                None => continue,
            };

            let mut path = GrammarPath::new();
            let mut stack = HashSet::new();
            collect_paths(
                grammar,
                nt_name,
                alt_idx,
                production,
                &rule_name,
                &mut path,
                &mut stack,
                &mut binding_map,
            );
        }
    }

    binding_map
}

fn collect_paths(
    grammar: &Grammar,
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
                    path.push(PathStep::new(child_idx, None));
                    binding_map.insert(rule_name, binding_name, path.clone());
                    path.pop();
                }
            }
            Symbol::Nonterminal { name, binding } => {
                // Binding attached directly to the child non-terminal.
                if let Some(binding_name) = binding {
                    path.push(PathStep::new(child_idx, None));
                    binding_map.insert(rule_name, binding_name, path.clone());
                    path.pop();
                }

                if let Some(child_productions) = grammar.productions.get(name) {
                    for (child_alt, child_prod) in child_productions.iter().enumerate() {
                        // If the child production defines its own rule, it's a boundary.
                        // We should not look for bindings for the current `rule_name` inside it.
                        if child_prod.rule.is_some() {
                            continue;
                        }

                        path.push(PathStep::new(child_idx, Some(child_alt)));
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
