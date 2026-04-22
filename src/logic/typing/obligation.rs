//! Formal obligation objects and their structural evolution.

use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{ArenaNode, Binding, Lexeme, NodeStatus, ProdId, TypeId};
use crate::logic::parse::NtId;
use crate::logic::path::{GrammarPath, TreePath};
use std::collections::HashSet;

/// Deferred semantic requirement induced by a typing rule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Obligation {
    pub name: String,
    pub paths: Vec<GrammarPath>,
    pub value: Option<Lexeme>,
    pub actual: Option<TypeId>,
}

impl Obligation {
    pub fn to_binding(&self) -> Binding {
        Binding {
            name: self.name.clone(),
            value: self.value.clone(),
            ty: self.actual,
        }
    }

    fn has_matched(&self) -> bool {
        self.value.is_some()
    }

    fn matches(&self, dot: usize, alt: usize) -> bool {
        self.paths.iter().any(|path| {
            let steps = path.steps();
            steps.len() == 1 && steps[0].i == dot && steps[0].a == alt
        })
    }

    fn step(&self, dot: usize, alt: usize) -> Option<Self> {
        let paths: Vec<GrammarPath> = self
            .paths
            .iter()
            .filter_map(|path| {
                let steps = path.steps();
                let first = steps.first()?;
                if first.i == dot && first.a == alt {
                    Some(GrammarPath::from(steps[1..].to_vec()))
                } else {
                    None
                }
            })
            .collect();

        (!paths.is_empty()).then(|| Self {
            name: self.name.clone(),
            paths,
            value: self.value.clone(),
            actual: self.actual,
        })
    }

    fn for_seed(&self, alt: usize) -> Option<Self> {
        let paths: Vec<GrammarPath> = self
            .paths
            .iter()
            .filter(|path| path.steps().first().map_or(true, |step| step.a == alt))
            .cloned()
            .collect();

        (!paths.is_empty()).then(|| Self {
            name: self.name.clone(),
            paths,
            value: self.value.clone(),
            actual: self.actual,
        })
    }

    fn resolve_from_node(&mut self, dot: usize, alt: usize, node: &ArenaNode) {
        if !self.has_matched() && self.matches(dot, alt) {
            self.value = Some(Lexeme::new(
                node.span,
                node.status.complete(),
                node.status.open(),
            ));
            self.actual = Some(node.ty);
            return;
        }

        for binding in &node.bindings {
            if binding.name == self.name {
                self.value = binding.value.clone();
                self.actual = binding.ty;
                break;
            }
        }
    }
}

/// Obligation set rooted at a concrete tree position.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Obligations {
    root: TreePath,
    items: Vec<Obligation>,
}

impl<'a> IntoIterator for &'a Obligations {
    type Item = &'a Obligation;
    type IntoIter = std::slice::Iter<'a, Obligation>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl Obligations {
    /// Create the obligations induced by a production at `root`.
    pub fn create(grammar: &Grammar, prod: ProdId, root: TreePath) -> Self {
        let Some(_) = grammar.prod(prod) else {
            return Self::new(root, Vec::new());
        };
        let Some(rule_name) = grammar.rule_for_prod(prod) else {
            return Self::new(root, Vec::new());
        };
        let Some(binding_map) = &grammar.bindings else {
            return Self::new(root, Vec::new());
        };
        let Some(rule) = grammar.rules().get(rule_name.as_str()) else {
            return Self::new(root, Vec::new());
        };

        let alt = prod.1;
        let items = rule
            .used_bindings()
            .into_iter()
            .filter_map(|name| {
                let paths = binding_map.get(name, rule_name)?;
                let filtered: Vec<GrammarPath> = paths
                    .iter()
                    .filter(|path| path.steps().first().map_or(true, |step| step.a == alt))
                    .cloned()
                    .collect();

                (!filtered.is_empty()).then(|| Obligation {
                    name: name.to_string(),
                    paths: filtered,
                    value: None,
                    actual: None,
                })
            })
            .collect();

        Self::new(root, items)
    }

    pub fn new(root: TreePath, items: Vec<Obligation>) -> Self {
        Self { root, items }
    }

    pub fn empty() -> Self {
        Self::default()
    }

    pub fn root(&self) -> &TreePath {
        &self.root
    }

    pub fn len(&self) -> usize {
        self.items.len()
    }

    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Obligation> {
        self.items.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Obligation> {
        self.items.iter_mut()
    }

    pub fn as_slice(&self) -> &[Obligation] {
        &self.items
    }

    pub fn extend(&mut self, other: Self) {
        debug_assert_eq!(self.root, other.root);
        self.items.extend(other.items);
    }

    /// Advance obligations across one child in the current production.
    pub fn step(&self, dot: usize, alt: usize) -> Self {
        Self::new(
            self.root.clone(),
            self.items
                .iter()
                .filter_map(|obligation| obligation.step(dot, alt))
                .collect(),
        )
    }

    /// Restrict obligations to a concrete production seed.
    pub fn for_seed(&self, alt: usize) -> Self {
        Self::new(
            self.root.clone(),
            self.items
                .iter()
                .filter_map(|obligation| obligation.for_seed(alt))
                .collect(),
        )
    }

    /// Re-root obligations at the designated child path.
    pub fn at_child(&self, dot: usize) -> Self {
        let mut root = self.root.clone();
        root.push(dot);
        Self::new(root, self.items.clone())
    }

    /// Resolve obligations against one parsed child node.
    pub fn resolve_nonterminal(&mut self, dot: usize, alt: usize, node: &ArenaNode) {
        for obligation in &mut self.items {
            obligation.resolve_from_node(dot, alt, node);
        }
    }

    /// Resolve obligations bound directly to a terminal child.
    pub fn resolve_terminal(&mut self, dot: usize, alt: usize, lexeme: &Lexeme) {
        for obligation in &mut self.items {
            if !obligation.has_matched() && obligation.matches(dot, alt) {
                obligation.value = Some(lexeme.clone());
            }
        }
    }

    /// Compute the alternatives permitted by the remaining obligations.
    pub fn prune(&self, nt: NtId, grammar: &Grammar) -> Vec<ProdId> {
        let total = grammar
            .productions_at(nt)
            .map_or(0, |productions| productions.len());
        if total == 0 {
            return Vec::new();
        }

        let mut constrained = HashSet::new();
        let mut unconstrained = false;

        for obligation in &self.items {
            for path in &obligation.paths {
                if let Some(first) = path.steps().first() {
                    let a = first.a;
                    if a < total {
                        constrained.insert(a);
                    }
                }
            }
        }

        if constrained.is_empty() {
            (0..total).map(|alt| (nt, alt)).collect()
        } else {
            constrained.into_iter().map(|alt| (nt, alt)).collect()
        }
    }
}
