//! Property tests for the regular-leaf core (§2). A leaf is a regular set; the
//! only operation is the meet, and the only lattice facts are intersection and
//! subset. Tree unification (variables, constructors, the lifted string-capture
//! limits) is property-tested in `term.rs`; `⊤` lives in `Term`, tested there.

use crate::regex::Regex;
use crate::typing::Pattern;
use crate::typing::pattern::Match;
use proptest::prelude::*;

// ── Generators ──────────────────────────────────────────────────────────────

fn leaf() -> impl Strategy<Value = String> {
    "[A-Za-z][A-Za-z0-9]{0,3}"
}

/// A singleton literal type, e.g. `Int`, `a1`.
fn lit_ty() -> impl Strategy<Value = Pattern> {
    leaf().prop_map(Pattern::raw)
}

/// A genuine regular-set leaf: a union of 1..3 literals. Exercises the lattice
/// on sets, not just singletons.
fn set_ty() -> impl Strategy<Value = Pattern> {
    prop::collection::vec(leaf(), 1..4)
        .prop_map(|ps| Pattern::closed(Regex::union_many(ps.iter().map(|p| Regex::literal(p)))))
}

/// A non-`⊤` leaf: literal, set, or `⊥`.
fn concrete_ty() -> impl Strategy<Value = Pattern> {
    prop_oneof![
        6 => lit_ty(),
        5 => set_ty(),
        1 => Just(Pattern::bottom()),
    ]
}

fn solved(m: &Match) -> bool {
    matches!(m, Match::Solved)
}

// ── Meet ────────────────────────────────────────────────────────────────────

proptest! {
    /// A leaf unifies with itself, unless it is `⊥`.
    #[test]
    fn meet_reflexive(t in concrete_ty()) {
        prop_assert_eq!(solved(&t.unify(&t)), t != Pattern::bottom());
    }

    /// The meet is symmetric.
    #[test]
    fn unify_symmetric(a in concrete_ty(), b in concrete_ty()) {
        prop_assert_eq!(a.unify(&b), b.unify(&a));
    }

    /// `Solved` is exactly non-empty regular intersection.
    #[test]
    fn meet_is_intersection(a in concrete_ty(), b in concrete_ty()) {
        prop_assert_eq!(solved(&a.unify(&b)), a.head.has_intersection(&b.head));
    }

    /// The meet never panics and yields exactly one of the two verdicts.
    #[test]
    fn unify_total(a in concrete_ty(), b in concrete_ty()) {
        let m = a.unify(&b);
        prop_assert!(matches!(m, Match::Solved | Match::Empty));
    }
}

// ── Lattice endpoint: `⊥` ────────────────────────────────────────────────────

proptest! {
    /// `⊥` (the empty set) intersects nothing.
    #[test]
    fn bottom_always_empty(t in concrete_ty()) {
        prop_assert_eq!(Pattern::bottom().unify(&t), Match::Empty);
    }
}

// ── Subset lattice ───────────────────────────────────────────────────────────

proptest! {
    #[test]
    fn subset_reflexive(t in concrete_ty()) {
        prop_assert!(t.subset(&t));
    }

    /// `⊥ ⊆ τ` for every τ.
    #[test]
    fn bottom_below_all(t in concrete_ty()) {
        prop_assert!(Pattern::bottom().subset(&t));
    }

    /// Inclusion is transitive (exercises the DFA difference).
    #[test]
    fn subset_transitive(a in set_ty(), b in set_ty(), c in set_ty()) {
        if a.subset(&b) && b.subset(&c) {
            prop_assert!(a.subset(&c));
        }
    }

    /// Mutual inclusion means the meet is non-empty (the sets are equal).
    #[test]
    fn subset_antisymmetric(a in set_ty(), b in set_ty()) {
        if a.subset(&b) && b.subset(&a) {
            prop_assert!(solved(&a.unify(&b)));
        }
    }

    /// `σ ⊆ τ` with σ non-empty forces a non-empty meet.
    #[test]
    fn subset_implies_meet(a in set_ty(), b in set_ty()) {
        if a.subset(&b) {
            prop_assert!(solved(&a.unify(&b)));
        }
    }
}

// ── Canonicalization (evidence interning relies on it) ──────────────────────

proptest! {
    /// `raw(s)` and `lit(cleaned)` agree, so equal types intern to one structure.
    #[test]
    fn raw_is_canonical(s in leaf()) {
        prop_assert_eq!(Pattern::raw(&s), Pattern::lit(&s));
    }
}
