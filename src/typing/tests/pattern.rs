//! Properties of pattern matching (§2). Each maps to a draft lemma.

use crate::regex::Regex;
use crate::typing::pattern::{Binding, Match, Pattern};
use proptest::prelude::*;

/// `?A sep ?B` with a literal separator.
fn binary(sep: &str) -> Pattern {
    Pattern {
        head: Regex::Epsilon,
        rest: vec![
            ("A".into(), Regex::literal(sep)),
            ("B".into(), Regex::Epsilon),
        ],
    }
}

/// `?A sep ?A` — a repeated hole.
fn repeated(sep: &str) -> Pattern {
    Pattern {
        head: Regex::Epsilon,
        rest: vec![
            ("A".into(), Regex::literal(sep)),
            ("A".into(), Regex::Epsilon),
        ],
    }
}

// ── Meet soundness (lem:match-finite core) ──────────────────────────────────

#[test]
fn meet_equal_is_solved() {
    assert!(matches!(
        Pattern::lit("Int").unify(&Pattern::lit("Int")),
        Match::Solved(_)
    ));
}

#[test]
fn meet_distinct_is_empty() {
    assert_eq!(
        Pattern::lit("Int").unify(&Pattern::lit("Bool")),
        Match::Empty
    );
}

#[test]
fn bottom_unifies_with_nothing() {
    assert_eq!(Pattern::bottom().unify(&Pattern::lit("Int")), Match::Empty);
    assert_eq!(Pattern::bottom().unify(&Pattern::bottom()), Match::Empty);
}

#[test]
fn broad_set_unifies_with_member() {
    // ⊤-like over {a,b}: (a|b)* meets any member non-empty.
    let universe = Pattern::closed(Regex::star(Regex::or(Regex::Char('a'), Regex::Char('b'))));
    assert!(matches!(
        universe.unify(&Pattern::lit("abba")),
        Match::Solved(_)
    ));
}

// ── Capture roundtrip (def:match) ───────────────────────────────────────────

#[test]
fn capture_binds_both_sides() {
    let Match::Solved(sub) = binary(" -> ").unify(&Pattern::lit("Int -> Bool")) else {
        panic!("expected solved");
    };
    assert_eq!(sub["A"], Binding::Lit("Int".into()));
    assert_eq!(sub["B"], Binding::Lit("Bool".into()));
}

#[test]
fn capture_missing_separator_is_empty() {
    assert_eq!(binary(" -> ").unify(&Pattern::lit("Int")), Match::Empty);
}

#[test]
fn repeated_hole_consistent_is_solved() {
    assert!(matches!(
        repeated(",").unify(&Pattern::lit("Int,Int")),
        Match::Solved(_)
    ));
}

#[test]
fn repeated_hole_conflict_is_empty() {
    // Shared name forces equality; "Int" ≠ "Bool".
    assert_eq!(repeated(",").unify(&Pattern::lit("Int,Bool")), Match::Empty);
}

proptest! {
    /// `unify(P, u) = Solved(sub)` reconstructs `u`: `subP = u` (def:match).
    #[test]
    fn capture_reassembles(a in "[A-Za-z]{1,6}", b in "[A-Za-z]{1,6}") {
        let u = format!("{a} -> {b}");
        let Match::Solved(sub) = binary(" -> ").unify(&Pattern::lit(&u)) else {
            prop_assert!(false, "no solve for {u}");
            unreachable!()
        };
        prop_assert_eq!(&sub["A"], &Binding::Lit(a.clone()));
        prop_assert_eq!(&sub["B"], &Binding::Lit(b.clone()));
    }
}

// ── Directional safety (rem:subtype-inclusion) ──────────────────────────────

#[test]
fn solved_does_not_imply_containment() {
    // A partial type {Int, Bool} meets `Int` (non-empty) but is not contained
    // in it. So `Solved` must not license `Satisfied`: the verdict is `Live`.
    let partial = Pattern::closed(Regex::or(Regex::literal("Int"), Regex::literal("Bool")));
    let expected = Pattern::lit("Int");
    assert!(matches!(partial.unify(&expected), Match::Solved(_)));
    assert_eq!(partial.subset(&expected), Some(false));
}

#[test]
fn resolved_point_is_contained() {
    // Once resolved to a singleton in the required set, containment holds and
    // `Satisfied` is sound.
    let resolved = Pattern::lit("Int");
    let expected = Pattern::closed(Regex::or(Regex::literal("Int"), Regex::literal("Bool")));
    assert_eq!(resolved.subset(&expected), Some(true));
}

// ── Lattice laws (rem:subtype-inclusion) ────────────────────────────────────

/// `⊤` is never `Empty`: it unifies with everything.
fn top_solves(x: &Pattern) -> bool {
    !matches!(Pattern::top().unify(x), Match::Empty)
        && !matches!(x.unify(&Pattern::top()), Match::Empty)
}

#[test]
fn bottom_annihilates_closed() {
    for s in ["Int", "Int -> Bool", ""] {
        assert_eq!(Pattern::bottom().unify(&Pattern::lit(s)), Match::Empty);
    }
}

#[test]
fn subset_reflexive_on_literals() {
    for s in ["Int", "a -> b -> c"] {
        assert_eq!(Pattern::lit(s).subset(&Pattern::lit(s)), Some(true));
    }
}

proptest! {
    /// `⊤` absorbs: it unifies with every concrete type.
    #[test]
    fn top_unifies_with_everything(s in "[A-Za-z ]{0,12}") {
        prop_assert!(top_solves(&Pattern::lit(&s)));
    }

    /// Meet is commutative on closed types: order does not change the verdict.
    #[test]
    fn meet_commutes(a in "[A-Za-z]{1,5}", b in "[A-Za-z]{1,5}") {
        let (x, y) = (Pattern::lit(&a), Pattern::lit(&b));
        prop_assert_eq!(
            matches!(x.unify(&y), Match::Solved(_)),
            matches!(y.unify(&x), Match::Solved(_)),
        );
    }

    /// Totality: a closed/closed meet is always decided, never `Open`, and is
    /// `Solved` iff the literals are equal.
    #[test]
    fn closed_meet_is_decided(a in "[A-Za-z]{0,6}", b in "[A-Za-z]{0,6}") {
        let m = Pattern::lit(&a).unify(&Pattern::lit(&b));
        prop_assert!(!matches!(m, Match::Open));
        prop_assert_eq!(matches!(m, Match::Solved(_)), a == b);
    }
}

// ── Multi-hole capture ──────────────────────────────────────────────────────

#[test]
fn three_holes_curried_arrow() {
    // ?A -> ?B -> ?C against a -> b -> c, leftmost split.
    let p = Pattern {
        head: Regex::Epsilon,
        rest: vec![
            ("A".into(), Regex::literal(" -> ")),
            ("B".into(), Regex::literal(" -> ")),
            ("C".into(), Regex::Epsilon),
        ],
    };
    let Match::Solved(sub) = p.unify(&Pattern::lit("a -> b -> c")) else {
        panic!("expected solved");
    };
    assert_eq!(sub["A"], Binding::Lit("a".into()));
    assert_eq!(sub["B"], Binding::Lit("b".into()));
    assert_eq!(sub["C"], Binding::Lit("c".into()));
}
