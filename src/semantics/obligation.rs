//! Formal obligation objects and their structural evolution — `def:obligation-store`.
//!
//! Obligations are intentionally domain-neutral: they route child evidence and
//! lexemes according to binding positions, but do not interpret the evidence.

use crate::engine::grammar::SPG;
use crate::engine::parse::arena::{ArenaNode, BindingStatus, EvidenceId, Lexeme, ProdId, Span};
use crate::engine::parse::NtId;
use crate::engine::path::{GrammarPath, TreePath};
use crate::semantics::domain::{ConstraintDomain, HasBindings};
use std::collections::HashSet;

/// Deferred semantic requirement induced by a semantic rule.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Obligation {
    pub name: String,
    pub paths: Vec<GrammarPath>,
    pub value: Option<Lexeme>,
    pub evidence: Option<EvidenceId>,
}

impl Obligation {
    /// Convert obligation into the β(ν,b) map entry.
    #[must_use]
    pub fn to_binding_status(&self, position: usize) -> (String, BindingStatus) {
        let status = if let Some(value) = &self.value {
            BindingStatus::Resolved {
                span: value.matched,
                complete: value.complete,
                open: value.open,
                evidence: self.evidence.unwrap_or(0),
            }
        } else {
            BindingStatus::Pending { position }
        };
        (self.name.clone(), status)
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
            value: self.value,
            evidence: self.evidence,
        })
    }

    fn for_seed(&self, alt: usize) -> Option<Self> {
        let paths: Vec<GrammarPath> = self
            .paths
            .iter()
            .filter(|path| path.steps().first().is_none_or(|step| step.a == alt))
            .cloned()
            .collect();

        (!paths.is_empty()).then(|| Self {
            name: self.name.clone(),
            paths,
            value: self.value,
            evidence: self.evidence,
        })
    }

    fn resolve_from_node(&mut self, dot: usize, alt: usize, node: &ArenaNode) {
        if !self.has_matched() && self.matches(dot, alt) {
            self.value = Some(Lexeme::new(
                node.span,
                node.status.complete(),
                node.status.open(),
            ));
            self.evidence = Some(node.evidence);
            return;
        }

        if let Some(BindingStatus::Resolved {
            span,
            complete,
            open,
            evidence,
        }) = node.binding_map.get(&self.name)
        {
            self.value = Some(Lexeme::new(*span, *complete, *open));
            self.evidence = Some(*evidence);
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
    #[must_use]
    pub fn create<D: ConstraintDomain>(grammar: &SPG<D>, prod: ProdId, root: TreePath) -> Self {
        let Some(_) = grammar.prod(prod) else {
            return Self::new(root, Vec::new());
        };
        let Some(rule_name) = grammar.nt(prod.0).and_then(|n| grammar.nt_rule(n)) else {
            return Self::new(root, Vec::new());
        };
        let Some(_binding_map) = &grammar.bindings else {
            return Self::new(root, Vec::new());
        };
        let Some(_rule) = grammar.rules.get(rule_name.as_str()) else {
            return Self::new(root, Vec::new());
        };
        let Some(nt_name) = grammar.nt(prod.0) else {
            eprintln!("DEBUG obl_create: nt missing for {}", prod.0);
            return Self::new(root, Vec::new());
        };
        let Some(rule_name) = grammar.nt_rule(nt_name) else {
            eprintln!("DEBUG obl_create: nt_rule missing for {nt_name}");
            return Self::new(root, Vec::new());
        };
        let Some(binding_map) = &grammar.bindings else {
            eprintln!("DEBUG obl_create: bindings missing for {nt_name} / {rule_name}");
            return Self::new(root, Vec::new());
        };
        let Some(rule) = grammar.rules.get(rule_name.as_str()) else {
            eprintln!("DEBUG obl_create: rule missing for {nt_name} / {rule_name}");
            return Self::new(root, Vec::new());
        };

        let alt = prod.1;
        let items = rule
            .referenced_bindings()
            .filter_map(|name| {
                let paths = binding_map.get(name, rule_name)?;
                let filtered: Vec<GrammarPath> = paths
                    .iter()
                    .filter(|path| path.steps().first().is_none_or(|step| step.a == alt))
                    .cloned()
                    .collect();

                (!filtered.is_empty()).then(|| Obligation {
                    name: name.to_string(),
                    paths: filtered,
                    value: None,
                    evidence: None,
                })
            })
            .collect();

        Self::new(root, items)
    }

    #[must_use]
    pub fn new(root: TreePath, items: Vec<Obligation>) -> Self {
        Self { root, items }
    }

    #[must_use]
    pub fn empty() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn root(&self) -> &TreePath {
        &self.root
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }

    pub fn iter(&self) -> std::slice::Iter<'_, Obligation> {
        self.items.iter()
    }

    pub fn iter_mut(&mut self) -> std::slice::IterMut<'_, Obligation> {
        self.items.iter_mut()
    }

    #[must_use]
    pub fn as_slice(&self) -> &[Obligation] {
        &self.items
    }

    pub fn extend(&mut self, other: Self) {
        debug_assert_eq!(self.root, other.root);
        self.items.extend(other.items);
    }

    /// Advance obligations across one child in the current production.
    #[must_use]
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
    #[must_use]
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
    #[must_use]
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

    /// Resolve obligations at (dot, alt) with explicit span and evidence.
    /// Used by the word generator which builds derivations without arena nodes.
    pub fn resolve_explicit(
        &mut self,
        dot: usize,
        alt: usize,
        span: Span,
        complete: bool,
        open: bool,
        evidence: EvidenceId,
    ) {
        for obligation in &mut self.items {
            if !obligation.has_matched() && obligation.matches(dot, alt) {
                obligation.value = Some(Lexeme::new(span, complete, open));
                obligation.evidence = Some(evidence);
            }
        }
    }

    /// Snapshot obligations at (dot, alt) for backtracking.
    /// Returns (index, `saved_value`, `saved_evidence`) for each matching obligation.
    #[must_use]
    pub fn save_at(
        &self,
        dot: usize,
        alt: usize,
    ) -> Vec<(usize, Option<Lexeme>, Option<EvidenceId>)> {
        self.items
            .iter()
            .enumerate()
            .filter(|(_, ob)| !ob.has_matched() && ob.matches(dot, alt))
            .map(|(i, ob)| (i, ob.value, ob.evidence))
            .collect()
    }

    /// Restore obligations from a `save_at` snapshot.
    pub fn restore_at(&mut self, saved: Vec<(usize, Option<Lexeme>, Option<EvidenceId>)>) {
        for (i, value, evidence) in saved {
            if let Some(ob) = self.items.get_mut(i) {
                ob.value = value;
                ob.evidence = evidence;
            }
        }
    }

    /// Resolve obligations bound directly to a terminal child.
    pub fn resolve_terminal(&mut self, dot: usize, alt: usize, lexeme: &Lexeme) {
        for obligation in &mut self.items {
            if !obligation.has_matched() && obligation.matches(dot, alt) {
                obligation.value = Some(*lexeme);
            }
        }
    }

    /// Compute the alternatives permitted by the remaining obligations.
    #[must_use]
    pub fn prune<D: ConstraintDomain>(&self, nt: NtId, grammar: &SPG<D>) -> Vec<ProdId> {
        let total = grammar
            .productions_at(nt)
            .map_or(0, |productions: &Vec<_>| productions.len());
        if total == 0 {
            return Vec::new();
        }

        let mut constrained = HashSet::new();
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
