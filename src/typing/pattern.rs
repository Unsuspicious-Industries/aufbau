//! The regular leaf of a type term (§2).
//!
//! A base type is a regular *set* of strings. `Pattern` wraps that set and the
//! meet ([`unify`](Pattern::unify)) is its only operation: two leaves unify iff
//! their sets intersect. There are no holes here — a hole is a [`Term::Var`], so
//! a leaf is closed by construction; the structure a type once re-derived by
//! splitting a string now lives in the [`Term`] tree, and the leaf is just its
//! base case.
//!
//! The two lattice ends meet here too: `⊥ = ∅` is `Regex::Empty`, which
//! intersects nothing (not even itself); `⊤` is not a leaf at all but
//! [`Term::top`], handled one level up by the term unifier.
//!
//! [`Term`]: crate::typing::Term
//! [`Term::Var`]: crate::typing::Term::Var
//! [`Term::top`]: crate::typing::Term::top

use crate::regex::Regex;
use std::fmt;

/// A base type as a regular set. §2 def:pattern.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pattern {
    pub head: Regex,
}

/// The result of meeting two leaves: the intersection is non-empty (`Solved`)
/// or empty (`Empty`, a clash). The verdict (`Satisfied`/`Live`/`Lost`) is read
/// off this upstream, together with node openness (rem:subtype-inclusion).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Match {
    Solved,
    Empty,
}

impl Pattern {
    /// A leaf from an already-built regular set.
    #[must_use]
    pub fn closed(r: Regex) -> Self {
        Self { head: r }
    }
    /// A leaf denoting exactly the literal `s`.
    #[must_use]
    pub fn lit(s: &str) -> Self {
        Self::closed(Regex::literal(s))
    }
    /// A concrete leaf from text, taken literally (quotes and whitespace
    /// stripped). The unifier knows no type constructors: a base type is exactly
    /// its cleaned string as a regular set.
    pub fn raw(s: impl AsRef<str>) -> Self {
        Self::closed(Regex::literal(&clean(s.as_ref())))
    }
    /// Parse a leaf type. Infallible: any string is a regular set.
    pub fn parse_raw(s: &str) -> Result<Self, String> {
        Ok(Self::raw(s))
    }
    /// `⊥` — the empty set, which intersects nothing.
    #[must_use]
    pub fn bottom() -> Self {
        Self::closed(Regex::Empty)
    }
    /// The underlying regular set.
    #[must_use]
    pub fn as_regex(&self) -> &Regex {
        &self.head
    }

    /// Meet two leaves: their sets unify iff they intersect. §2 def:match.
    #[must_use]
    pub fn unify(&self, other: &Self) -> Match {
        if self.head.has_intersection(&other.head) {
            Match::Solved
        } else {
            Match::Empty
        }
    }

    /// `L(self) ⊆ L(other)`: containment of one set in another. The sound basis
    /// for `Satisfied` on a leaf ascription (rem:subtype-inclusion).
    #[must_use]
    pub fn subset(&self, other: &Self) -> bool {
        self.head.subset(&other.head)
    }
}

/// Strip surrounding quotes and insignificant whitespace from a type literal.
/// No type-construct knowledge: separators are whatever the grammar writes.
fn clean(s: &str) -> String {
    let s = s.trim();
    let s = s
        .strip_prefix('\'')
        .and_then(|x| x.strip_suffix('\''))
        .unwrap_or(s);
    s.chars().filter(|c| !c.is_whitespace()).collect()
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.head.literal_value() {
            Some(s) => write!(f, "{s}"),
            None => write!(f, "{}", self.head),
        }
    }
}
