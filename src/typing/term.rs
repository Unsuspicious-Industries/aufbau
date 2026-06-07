//! Prototype: first-order unification over type *trees* (§2).
//!
//! The production unifier ([`Pattern`](super::Pattern)) matches a holed pattern
//! against a flat string by leftmost split. Three limits follow from that flat
//! model, and all three are artifacts of it (sec:ambiguity):
//!
//! 1. capture is leftmost-only — `?A→?B` on `a→b→c` cannot reach `A=a→b, B=c`;
//! 2. a multi-hole pattern cannot split a non-literal regular *set* (→ `Open`);
//! 3. two open patterns cannot be aligned (→ `Open`).
//!
//! Lifting a type to a *tree* over the grammar signature and unifying it
//! structurally (Robinson / Martelli-Montanari) removes all three: the parse
//! fixes the split, variables on both sides bind natively, and the leaves meet
//! as regular sets via the existing [`Pattern`]. The type grammar is a
//! sub-grammar of the main grammar, so the same engine produces these trees
//! ([`Term::from_node`]).
//!
//! This is a prototype for investigating the unifier's power; it is not yet
//! wired into the constraint domain.

use super::Pattern;
use super::pattern::Match;
use crate::engine::structure::{FusionChild, FusionNode};
use std::collections::HashMap;
use std::fmt;

/// A type term over the grammar signature.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Term {
    /// A unification variable: a hole `?A`. Binds on both sides natively.
    Var(String),
    /// A constructor node, labelled by its nonterminal, with ordered children.
    /// The structural shape the parser committed to — no separator to re-split.
    Con(String, Vec<Term>),
    /// A regular leaf: a base type as a regular set. Leaves meet via `Pattern`,
    /// so a set-valued leaf is no harder than a singleton (lifts limit 2).
    Leaf(Pattern),
}

/// Variable → bound term. As in miniKanren, bindings are shallow; [`walk`]
/// chases chains and recursion resolves them lazily.
pub type Subst = HashMap<String, Term>;

impl fmt::Display for Term {
    /// Generic, constructor-agnostic: `?A`, a leaf as its regular set, and a
    /// node as `label(k₁, …, kₙ)`. No constructor (arrow, product) is special.
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Var(n) => write!(f, "?{n}"),
            Self::Leaf(p) => write!(f, "{p}"),
            Self::Con(label, kids) => {
                write!(f, "{label}(")?;
                for (i, k) in kids.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{k}")?;
                }
                write!(f, ")")
            }
        }
    }
}

impl Term {
    #[must_use]
    pub fn var(name: impl Into<String>) -> Self {
        Self::Var(name.into())
    }
    /// A base-type leaf from text, taken literally (the production unifier's
    /// `raw`): the leaf is exactly its cleaned string as a regular set.
    #[must_use]
    pub fn leaf(s: &str) -> Self {
        Self::Leaf(Pattern::raw(s))
    }
    /// A leaf term from text. Alias of [`Term::leaf`] for the `Type` surface.
    #[must_use]
    pub fn raw(s: impl AsRef<str>) -> Self {
        Self::Leaf(Pattern::raw(s.as_ref()))
    }
    /// Parse a leaf type. Infallible: any string is a regular leaf.
    pub fn parse_raw(s: &str) -> Result<Self, String> {
        Ok(Self::raw(s))
    }
    #[must_use]
    pub fn con(label: impl Into<String>, kids: Vec<Term>) -> Self {
        Self::Con(label.into(), kids)
    }

    /// `⊤` — the unknown type. A reserved variable: unifies with anything, and
    /// is the evidence-store sentinel for "no constraint yet".
    #[must_use]
    pub fn top() -> Self {
        Self::Var("⊤".into())
    }
    /// `⊥` — the empty leaf, which unifies with no other leaf and no `Con`.
    #[must_use]
    pub fn bottom() -> Self {
        Self::Leaf(Pattern::bottom())
    }
    #[must_use]
    pub fn is_top(&self) -> bool {
        matches!(self, Self::Var(n) if n == "⊤")
    }
    /// Has at least one unification variable (a partial type).
    #[must_use]
    pub fn has_vars(&self) -> bool {
        match self {
            Self::Var(_) => true,
            Self::Con(_, kids) => kids.iter().any(Self::has_vars),
            Self::Leaf(_) => false,
        }
    }
    /// No variables: a fully determined type.
    #[must_use]
    pub fn is_ground(&self) -> bool {
        !self.has_vars()
    }

    /// Build a term from a resolved engine subtree. A node whose text is a
    /// metavariable `?A` is a [`Term::Var`]; a transparent wrapper collapses to
    /// its child; a node with no child nodes is a [`Term::Leaf`]; otherwise a
    /// [`Term::Con`] labelled by its nonterminal, over its child *nodes*
    /// (terminals are the separators the label encodes).
    #[must_use]
    pub fn from_node(node: &FusionNode) -> Self {
        let text = node.text();
        if let Some(name) = text.trim().strip_prefix('?') {
            return Self::var(name.trim());
        }
        let mut child_nodes = node.children().filter_map(|c| match c {
            FusionChild::Node(n) => Some(n),
            FusionChild::Terminal { .. } => None,
        });
        // Transparent wrapper: no construct of its own, collapse to its single
        // child. The test is the grammar's one definition, shared with
        // elaboration ([`crate::engine::grammar::SPG::is_transparent`]).
        if node.is_transparent() && let Some(child) = child_nodes.next() {
            return Self::from_node(&child);
        }
        let kids: Vec<Term> = child_nodes.map(|n| Self::from_node(&n)).collect();
        if kids.is_empty() {
            return Self::leaf(text.trim());
        }
        Self::con(node.nt_name().unwrap_or("?"), kids)
    }
}

/// Resolve a term to its representative: chase variable bindings until a
/// non-variable or an unbound variable. Shallow, as in miniKanren's `walk`.
#[must_use]
pub fn walk(t: &Term, s: &Subst) -> Term {
    let mut t = t.clone();
    while let Term::Var(x) = &t {
        match s.get(x) {
            Some(next) => t = next.clone(),
            None => break,
        }
    }
    t
}

/// Does `x` occur in `t` under `s`? The occurs-check that keeps unification
/// well-founded; dropping it admits rational (recursive) trees (§2).
#[must_use]
fn occurs(x: &str, t: &Term, s: &Subst) -> bool {
    match walk(t, s) {
        Term::Var(y) => x == y,
        Term::Con(_, kids) => kids.iter().any(|k| occurs(x, k, s)),
        Term::Leaf(_) => false,
    }
}

/// Unify `a` and `b`, extending `s`. Returns `false` on clash. With
/// `occurs_check`, rejects `?A = ?A→?B` (no infinite types); without it, that
/// binding stands and `s` denotes a rational tree — a recursive type (§2).
#[must_use]
pub fn unify(a: &Term, b: &Term, s: &mut Subst, occurs_check: bool) -> bool {
    let (a, b) = (walk(a, s), walk(b, s));
    match (&a, &b) {
        (Term::Var(x), Term::Var(y)) if x == y => true,
        (Term::Var(x), t) | (t, Term::Var(x)) => {
            if occurs_check && occurs(x, t, s) {
                return false;
            }
            s.insert(x.clone(), t.clone());
            true
        }
        (Term::Con(f, xs), Term::Con(g, ys)) => {
            f == g
                && xs.len() == ys.len()
                && xs.iter().zip(ys).all(|(x, y)| unify(x, y, s, occurs_check))
        }
        // Leaves meet as regular sets: a non-empty intersection unifies. This is
        // where the lattice lives; a set-valued leaf is handled the same way.
        (Term::Leaf(p), Term::Leaf(q)) => !matches!(p.unify(q), Match::Empty),
        // Con vs Leaf, or anything else: a clash.
        _ => false,
    }
}

/// Apply `s` to `t`, resolving every variable as deeply as it is bound. An
/// unbound variable stays a variable; a leaf is unchanged.
#[must_use]
pub fn apply(t: &Term, s: &Subst) -> Term {
    match walk(t, s) {
        Term::Con(label, kids) => Term::Con(label, kids.iter().map(|k| apply(k, s)).collect()),
        other => other,
    }
}
