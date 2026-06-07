//! The single typing construct: a pattern over the grammar. §2.
//!
//! A type is a pattern `head ?h₁ sep₁ … ?hₖ sepₖ`: regular separators
//! interleaved with named holes. Closed (no holes) ⟹ just `head`, a regular
//! set. Unification is the meet (§2 def:match); `Satisfied`/`Live` are decided
//! upstream from the meet plus node openness (rem:subtype-inclusion).
//!
//! `⊥ = ∅` is `Regex::Empty` natively. `⊤ = Σ*` is the unconstrained set; it is
//! not materialized here (the DFA has no wildcard) and surfaces at the verdict
//! layer as "no constraint". TODO(pkd): `Regex::All` for a first-class `⊤`.

use crate::regex::Regex;
use std::collections::HashMap;
use std::fmt;

/// A type term. Closed iff `rest` is empty. §2 def:pattern.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pattern {
    pub head: Regex,
    pub rest: Vec<(String, Regex)>,
}

/// A hole's binding. `Lit` is concrete captured text; `Top` and `Bot` are
/// lattice markers from grammar atoms (`Atom::Top`/`Atom::Bot`). A binding is
/// a constraint to add: `Lit(s)` adds a literal constraint, `Bot` adds a bottom
/// constraint, `Top` adds no constraint. §2 def:match.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Binding {
    Lit(String),
    Top,
    Bot,
}

impl fmt::Display for Binding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Lit(s) => write!(f, "{s}"),
            Self::Top => write!(f, "⊤"),
            Self::Bot => write!(f, "⊥"),
        }
    }
}

/// Hole → `Binding`. §2 def:match.
pub type Subst = HashMap<String, Binding>;

/// A resolved atom: literal text, or an open hole.
#[derive(Clone, Debug)]
pub enum Part {
    Lit(Regex),
    Hole(String),
}

/// Meet status of a unification. The lattice verdict is read off this upstream.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Match {
    /// Meet non-empty; holes bound to `Lit`, `⊤`, or `⊥`. Not yet `Satisfied`
    /// (rem:subtype-inclusion).
    Solved(Subst),
    /// Meet `∅` → `Lost`.
    Empty,
    /// Undecidable here: regex separators or non-concrete input (sec:ambiguity).
    Open,
}

impl Pattern {
    #[must_use]
    pub fn closed(r: Regex) -> Self {
        Self {
            head: r,
            rest: Vec::new(),
        }
    }
    #[must_use]
    pub fn lit(s: &str) -> Self {
        Self::closed(Regex::literal(s))
    }
    /// A concrete (closed) type from text, taken literally. The unifier knows no
    /// type constructors: a type is exactly its (cleaned) string plus any holes.
    pub fn raw(s: impl AsRef<str>) -> Self {
        Self::closed(Regex::literal(&clean(s.as_ref())))
    }
    /// Parse a closed type. Infallible: any string is a regular set.
    pub fn parse_raw(s: &str) -> Result<Self, String> {
        Ok(Self::raw(s))
    }
    /// Is this `⊤` (the unconstrained type)?
    #[must_use]
    pub fn is_top(&self) -> bool {
        *self == Self::top()
    }
    /// `⊥` — unifies with nothing.
    #[must_use]
    pub fn bottom() -> Self {
        Self::closed(Regex::Empty)
    }
    /// `⊤` — unifies with everything. An unconstrained hole denotes `Σ*`
    /// without materializing it: it captures any input and never fails. A
    /// hole bound to `⊤` adds no constraint; the consuming type stays open
    /// at that position.
    #[must_use]
    pub fn top() -> Self {
        Self::hole("⊤")
    }
    #[must_use]
    pub fn hole(name: &str) -> Self {
        Self {
            head: Regex::Epsilon,
            rest: vec![(name.into(), Regex::Epsilon)],
        }
    }

    /// Assemble a pattern from resolved parts: literals fold into the
    /// surrounding separator, holes open a new slot. Regexes are canonicalized
    /// so equal types share one structure (evidence interning relies on it).
    #[must_use]
    pub fn build(parts: Vec<Part>) -> Self {
        let mut head = Regex::Epsilon;
        let mut rest: Vec<(String, Regex)> = Vec::new();
        for part in parts {
            match part {
                Part::Lit(r) => match rest.last_mut() {
                    Some((_, sep)) => *sep = append(sep.clone(), r),
                    None => head = append(head.clone(), r),
                },
                Part::Hole(n) => rest.push((n, Regex::Epsilon)),
            }
        }
        head = canon(head);
        for (_, sep) in &mut rest {
            *sep = canon(std::mem::replace(sep, Regex::Epsilon));
        }
        Self { head, rest }
    }

    #[must_use]
    pub fn is_closed(&self) -> bool {
        self.rest.is_empty()
    }
    /// The regular set, when closed.
    #[must_use]
    pub fn as_regex(&self) -> Option<&Regex> {
        self.is_closed().then_some(&self.head)
    }

    /// The lone hole `?h`, when the pattern is exactly that.
    #[must_use]
    pub fn as_single_hole(&self) -> Option<&str> {
        match self.rest.as_slice() {
            [(h, sep)]
                if lit_of(&self.head).is_some_and(|s| s.is_empty())
                    && lit_of(sep).is_some_and(|s| s.is_empty()) =>
            {
                Some(h)
            }
            _ => None,
        }
    }

    /// Unify two types. §2 def:match.
    #[must_use]
    pub fn unify(&self, other: &Self) -> Match {
        match (self.is_closed(), other.is_closed()) {
            (true, true) => meet(&self.head, &other.head),
            (false, true) => self.capture(&other.head),
            (true, false) => other.capture(&self.head),
            (false, false) => Match::Open,
        }
    }

    /// `L(self) ⊆ L(other)` for closed types; `None` if either is open. The
    /// only sound basis for `Satisfied` on a partial type (rem:subtype-inclusion).
    #[must_use]
    pub fn subset(&self, other: &Self) -> Option<bool> {
        match (self.as_regex(), other.as_regex()) {
            (Some(a), Some(b)) => Some(a.subset(b)),
            _ => None,
        }
    }

    /// Match a holed pattern against a concrete string. Leftmost split; regex
    /// separators or non-concrete input defer to `Open` (sec:ambiguity).
    fn capture(&self, u: &Regex) -> Match {
        // A bare hole binds the whole value. `u = ∅` → `Bot`; `u = ε` (empty
        // literal) → `Top` (no constraint); non-empty literal → `Lit`; anything
        // else is non-concrete and defers to `Open`.
        if let Some(h) = self.as_single_hole() {
            let bind = match u {
                Regex::Empty => Binding::Bot,
                _ => match u.literal_value() {
                    Some(s) if !s.is_empty() => Binding::Lit(s),
                    Some(_) => Binding::Top,
                    None => return Match::Open,
                },
            };
            return Match::Solved(Subst::from([(h.to_string(), bind)]));
        }
        let (Some(s), Some(head)) = (lit_of(u), lit_of(&self.head)) else {
            return Match::Open;
        };
        let Some(mut rem) = s.strip_prefix(&head) else {
            return Match::Empty;
        };
        let mut sub = Subst::new();
        let k = self.rest.len();
        for (i, (h, sep)) in self.rest.iter().enumerate() {
            let Some(w) = lit_of(sep) else {
                return Match::Open;
            };
            let (cap, tail) = match seg(rem, &w, i + 1 == k) {
                Seg::Take(c, t) => (c, t),
                Seg::Defer => return Match::Open,
                Seg::Miss => return Match::Empty,
            };
            rem = tail;
            if bind(&mut sub, h, cap).is_some() {
                return Match::Empty;
            }
        }
        if rem.is_empty() {
            Match::Solved(sub)
        } else {
            Match::Empty
        }
    }
}

/// Closed/closed: the meet is non-empty. §2 def:match.
fn meet(a: &Regex, b: &Regex) -> Match {
    if a.has_intersection(b) {
        Match::Solved(Subst::new())
    } else {
        Match::Empty
    }
}

enum Seg<'a> {
    Take(&'a str, &'a str),
    Defer,
    Miss,
}

/// Carve the next capture: up to the first occurrence of `w`, or all of `rem`
/// when `w` is empty and this is the final hole. Empty `w` mid-pattern is two
/// adjacent holes with no delimiter — ambiguous, so deferred.
fn seg<'a>(rem: &'a str, w: &str, last: bool) -> Seg<'a> {
    if w.is_empty() {
        return if last { Seg::Take(rem, "") } else { Seg::Defer };
    }
    match rem.find(w) {
        Some(i) => Seg::Take(&rem[..i], &rem[i + w.len()..]),
        None => Seg::Miss,
    }
}

/// Intersect a hole's capture with any prior binding. Captures are concrete, so
/// a repeated hole is consistent iff the strings are equal; a prior `Top`/`Bot`
/// is incompatible with any new capture. Returns the failure marker on conflict.
fn bind(sub: &mut Subst, h: &str, cap: &str) -> Option<()> {
    let conflict = match sub.get(h) {
        None => false,
        Some(Binding::Lit(p)) => p != cap,
        Some(_) => true,
    };
    if conflict {
        return Some(());
    }
    sub.insert(h.into(), Binding::Lit(cap.into()));
    None
}

/// Concatenate, dropping a leading `ε` so a single literal stays canonical.
fn append(acc: Regex, r: Regex) -> Regex {
    match acc {
        Regex::Epsilon => r,
        _ => Regex::cat(acc, r),
    }
}

/// Canonicalize a literal concatenation to the standard left-nested form, so
/// regexes built by different paths but denoting the same string compare equal.
fn canon(r: Regex) -> Regex {
    match r.literal_value() {
        Some(s) => Regex::literal(&s),
        None => r,
    }
}

/// Strip surrounding quotes and insignificant whitespace from a type literal.
/// No type-construct knowledge: separators are whatever the grammar writes.
pub(crate) fn clean(s: &str) -> String {
    let s = s.trim();
    let s = s
        .strip_prefix('\'')
        .and_then(|x| x.strip_suffix('\''))
        .unwrap_or(s);
    s.chars().filter(|c| !c.is_whitespace()).collect()
}

/// The literal string a separator denotes, treating `ε` as the empty literal.
fn lit_of(r: &Regex) -> Option<String> {
    match r {
        Regex::Epsilon => Some(String::new()),
        _ => r.literal_value(),
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(s) = lit_of(&self.head) {
            write!(f, "{s}")?;
        } else {
            write!(f, "{}", self.head)?;
        }
        for (h, sep) in &self.rest {
            let s = lit_of(sep).unwrap_or_else(|| sep.to_string());
            write!(f, "?{h}{s}")?;
        }
        Ok(())
    }
}
