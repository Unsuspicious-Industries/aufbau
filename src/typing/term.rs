//! First-order unification over type *trees* (§2) — the typing core.
//!
//! A type is a `Term`: a [`Var`](Term::Var) (a hole `?A`), a [`Con`](Term::Con)
//! (a constructor node labelled by its nonterminal), or a [`Leaf`](Term::Leaf)
//! (a base type as a regular set, via [`Pattern`](super::Pattern)). Unification
//! is Robinson / Martelli-Montanari over that tree: variables bind on either
//! side, constructors match label-and-arity then recurse, and leaves meet as
//! regular sets. The split a string model would have to guess is fixed by the
//! parse — the type grammar is a sub-grammar of the main grammar, so the same
//! engine yields these trees ([`Term::from_node`]). `⊤` and `⊥` are the lattice
//! ends ([`Term::top`], [`Term::bottom`]).

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

    /// `⊤` — the top type: the unknown, the unit of the meet. Carried as the
    /// reserved name `Var("⊤")` so it is a recognisable sentinel (the evidence
    /// store interns it as "no constraint yet", and [`resolve_ref`] reads it as
    /// unresolved), but [`unify`] gives it the top rule, not the variable rule:
    /// `⊤ ⊓ t = t` for every `t`, succeeding and binding nothing. Distinct ⊤s are
    /// therefore never identified.
    ///
    /// [`resolve_ref`]: crate::typing::TypingDomain
    #[must_use]
    pub fn top() -> Self {
        Self::Var("⊤".into())
    }
    /// `⊥` — the bottom type: the empty regular leaf `∅`. The unit of the join
    /// and the absorbing element of the meet: `⊥ ⊓ t = ⊥`, so [`unify`] clashes
    /// `⊥` against any other leaf and any `Con`. It still instantiates a bare
    /// variable (`?A ⊓ ⊥ = ⊥`), the one case where `⊥` unifies.
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

    /// Rename every variable to a globally-fresh one — instantiation (the ∀ of a
    /// polymorphic scheme made concrete at a use site). Repeated variables stay
    /// shared; `next` is bumped to keep names unique within an evaluation.
    #[must_use]
    pub fn freshen(&self, next: &mut u64) -> Self {
        fn go(t: &Term, map: &mut HashMap<String, String>, next: &mut u64) -> Term {
            match t {
                Term::Var(x) => {
                    let fresh = map.entry(x.clone()).or_insert_with(|| {
                        let n = format!("{x}%{next}");
                        *next += 1;
                        n
                    });
                    Term::Var(fresh.clone())
                }
                Term::Con(l, ks) => Term::Con(l.clone(), ks.iter().map(|k| go(k, map, next)).collect()),
                Term::Leaf(p) => Term::Leaf(p.clone()),
            }
        }
        go(self, &mut HashMap::new(), next)
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
        // ⊤ — the top of the lattice — unifies with everything and binds nothing.
        // It is *not* an ordinary variable: a bare `Var("⊤")` would be identified
        // across occurrences (and could bind to a concrete type), which can force a
        // spurious clash. Succeeding without binding keeps each ⊤ independent and
        // adds no constraint, the textbook top rule.
        _ if a.is_top() || b.is_top() => true,
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
