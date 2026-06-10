//! Properties of the regular-leaf meet (§2). Each maps to a draft lemma. Holes,
//! constructors, and `⊤` are the term unifier's job (`term.rs`); a leaf is a
//! closed regular set and its only operation is intersection.

use crate::regex::Regex;
use crate::typing::pattern::{Match, Pattern};
use proptest::prelude::*;

// ── Meet soundness (lem:match-finite core) ──────────────────────────────────

#[test]
fn meet_equal_is_solved() {
    assert_eq!(Pattern::lit("Int").unify(&Pattern::lit("Int")), Match::Solved);
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
    // A set (a|b)* meets any of its members non-empty.
    let universe = Pattern::closed(Regex::star(Regex::or(Regex::Char('a'), Regex::Char('b'))));
    assert_eq!(universe.unify(&Pattern::lit("abba")), Match::Solved);
}

// ── Directional safety (rem:subtype-inclusion) ──────────────────────────────

#[test]
fn solved_does_not_imply_containment() {
    // A partial type {Int, Bool} meets `Int` (non-empty) but is not contained
    // in it. So `Solved` must not license `Satisfied`: the verdict is `Live`.
    let partial = Pattern::closed(Regex::or(Regex::literal("Int"), Regex::literal("Bool")));
    let expected = Pattern::lit("Int");
    assert_eq!(partial.unify(&expected), Match::Solved);
    assert!(!partial.subset(&expected));
}

#[test]
fn resolved_point_is_contained() {
    // Once resolved to a singleton in the required set, containment holds and
    // `Satisfied` is sound.
    let resolved = Pattern::lit("Int");
    let expected = Pattern::closed(Regex::or(Regex::literal("Int"), Regex::literal("Bool")));
    assert!(resolved.subset(&expected));
}

// ── Lattice laws (rem:subtype-inclusion) ────────────────────────────────────

#[test]
fn bottom_annihilates_closed() {
    for s in ["Int", "Int -> Bool", ""] {
        assert_eq!(Pattern::bottom().unify(&Pattern::lit(s)), Match::Empty);
    }
}

#[test]
fn subset_reflexive_on_literals() {
    for s in ["Int", "a -> b -> c"] {
        assert!(Pattern::lit(s).subset(&Pattern::lit(s)));
    }
}

proptest! {
    /// Meet is commutative on leaves: order does not change the verdict.
    #[test]
    fn meet_commutes(a in "[A-Za-z]{1,5}", b in "[A-Za-z]{1,5}") {
        let (x, y) = (Pattern::lit(&a), Pattern::lit(&b));
        prop_assert_eq!(x.unify(&y), y.unify(&x));
    }

    /// A leaf/leaf meet is `Solved` iff the literals are equal.
    #[test]
    fn singleton_meet_is_equality(a in "[A-Za-z]{0,6}", b in "[A-Za-z]{0,6}") {
        let m = Pattern::lit(&a).unify(&Pattern::lit(&b));
        prop_assert_eq!(matches!(m, Match::Solved), a == b);
    }
}
