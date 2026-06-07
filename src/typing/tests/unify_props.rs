//! Property tests mapping the power of the unifier (§2). The unifier knows no
//! type constructors: a type is a literal string (a regular leaf) plus holes.
//! Every law here is checked over generated types and patterns.

use crate::regex::Regex;
use crate::typing::Pattern;
use crate::typing::pattern::{Binding, Match, Part};
use proptest::prelude::*;

// ── Generators ──────────────────────────────────────────────────────────────

fn leaf() -> impl Strategy<Value = String> {
    "[A-Za-z][A-Za-z0-9]{0,3}"
}

/// A singleton literal type, e.g. `Int`, `a1`.
fn lit_ty() -> impl Strategy<Value = Pattern> {
    leaf().prop_map(Pattern::raw)
}

/// A genuine regular-set leaf: a union of 1..3 literals, built directly. Leaves
/// may be any regex; this exercises the lattice without the dropped `|` sugar.
fn set_ty() -> impl Strategy<Value = Pattern> {
    prop::collection::vec(leaf(), 1..4)
        .prop_map(|ps| Pattern::closed(Regex::union_many(ps.iter().map(|p| Regex::literal(p)))))
}

/// A closed (`as_regex`-able) type: literal, set, or `⊥`. Excludes `⊤`, which is
/// an open hole.
fn concrete_ty() -> impl Strategy<Value = Pattern> {
    prop_oneof![
        6 => lit_ty(),
        5 => set_ty(),
        1 => Just(Pattern::bottom()),
    ]
}

/// A holed pattern with adversarial separators drawn from punctuation.
fn holed() -> impl Strategy<Value = Pattern> {
    prop::collection::vec((leaf(), "[,:#>~.|-]{1,2}"), 1..4).prop_map(|hs| {
        let mut parts = Vec::new();
        for (i, (name, sep)) in hs.into_iter().enumerate() {
            if i > 0 {
                parts.push(Part::Lit(Regex::literal(&sep)));
            }
            parts.push(Part::Hole(name));
        }
        Pattern::build(parts)
    })
}

/// Anything: closed type, `⊤`, or a holed pattern.
fn any_pat() -> impl Strategy<Value = Pattern> {
    prop_oneof![
        4 => concrete_ty(),
        1 => Just(Pattern::top()),
        4 => holed(),
    ]
}

fn solved(m: &Match) -> bool {
    matches!(m, Match::Solved(_))
}

// ── Closed/closed meet ──────────────────────────────────────────────────────

proptest! {
    /// A type unifies with itself, unless it is `⊥`.
    #[test]
    fn meet_reflexive(t in concrete_ty()) {
        prop_assert_eq!(solved(&t.unify(&t)), t != Pattern::bottom());
    }

    /// Unification is symmetric in every case (capture, meet, open).
    #[test]
    fn unify_symmetric(a in any_pat(), b in any_pat()) {
        prop_assert_eq!(a.unify(&b), b.unify(&a));
    }

    /// Closed against closed is always decided, never `Open`.
    #[test]
    fn closed_meet_is_decided(a in concrete_ty(), b in concrete_ty()) {
        prop_assert!(!matches!(a.unify(&b), Match::Open));
    }

    /// `Solved` on closed types is exactly non-empty regular intersection.
    #[test]
    fn meet_is_intersection(a in concrete_ty(), b in concrete_ty()) {
        prop_assert_eq!(solved(&a.unify(&b)), a.head.has_intersection(&b.head));
    }
}

// ── Lattice endpoints ───────────────────────────────────────────────────────

proptest! {
    /// `⊤` unifies with everything.
    #[test]
    fn top_never_empty(t in concrete_ty()) {
        prop_assert!(!matches!(Pattern::top().unify(&t), Match::Empty));
        prop_assert!(!matches!(t.unify(&Pattern::top()), Match::Empty));
    }

    /// `⊥` unifies with nothing.
    #[test]
    fn bottom_always_empty(t in concrete_ty()) {
        prop_assert_eq!(Pattern::bottom().unify(&t), Match::Empty);
    }
}

// ── Subsumption / subset lattice ────────────────────────────────────────────

proptest! {
    #[test]
    fn subset_reflexive(t in concrete_ty()) {
        prop_assert_eq!(t.subset(&t), Some(true));
    }

    /// `⊥ ⊆ τ` for every τ.
    #[test]
    fn bottom_below_all(t in concrete_ty()) {
        prop_assert_eq!(Pattern::bottom().subset(&t), Some(true));
    }

    /// Inclusion is transitive (exercises the now-sound DFA difference).
    #[test]
    fn subset_transitive(a in set_ty(), b in set_ty(), c in set_ty()) {
        if a.subset(&b) == Some(true) && b.subset(&c) == Some(true) {
            prop_assert_eq!(a.subset(&c), Some(true));
        }
    }

    /// Mutual inclusion means the meet is non-empty (the sets are equal).
    #[test]
    fn subset_antisymmetric(a in set_ty(), b in set_ty()) {
        if a.subset(&b) == Some(true) && b.subset(&a) == Some(true) {
            prop_assert!(solved(&a.unify(&b)));
        }
    }

    /// `σ ⊆ τ` with σ non-empty forces a non-empty meet (sets here are never empty).
    #[test]
    fn subset_implies_meet(a in set_ty(), b in set_ty()) {
        if a.subset(&b) == Some(true) {
            prop_assert!(solved(&a.unify(&b)));
        }
    }
}

// ── Capture ─────────────────────────────────────────────────────────────────

proptest! {
    /// `unify(P, u) = Solved(θ)` reconstructs `u`: each hole binds its slice.
    #[test]
    fn capture_roundtrip(fills in prop::collection::vec(leaf(), 1..5)) {
        let mut parts = Vec::new();
        for (i, _) in fills.iter().enumerate() {
            if i > 0 {
                parts.push(Part::Lit(Regex::literal("→")));
            }
            parts.push(Part::Hole(format!("h{i}")));
        }
        let pat = Pattern::build(parts);
        let u = fills.join("→");
        let Match::Solved(sub) = pat.unify(&Pattern::raw(&u)) else {
            prop_assert!(false, "no solve for {u}");
            unreachable!()
        };
        for (i, f) in fills.iter().enumerate() {
            prop_assert_eq!(sub.get(&format!("h{i}")), Some(&Binding::Lit(f.clone())));
        }
    }

    /// A bare hole binds concrete text to a `Lit` capture.
    #[test]
    fn single_hole_binds_concrete(t in lit_ty()) {
        let s = t.as_regex().unwrap().literal_value().unwrap();
        let Match::Solved(sub) = Pattern::hole("Z").unify(&t) else {
            prop_assert!(false);
            unreachable!()
        };
        prop_assert_eq!(sub.get("Z"), Some(&Binding::Lit(s)));
    }

    /// A repeated hole forces equality of its captures.
    #[test]
    fn repeated_hole_equality(a in leaf(), b in leaf()) {
        let pat = Pattern::build(vec![
            Part::Hole("X".into()),
            Part::Lit(Regex::literal(",")),
            Part::Hole("X".into()),
        ]);
        let m = pat.unify(&Pattern::raw(format!("{a},{b}")));
        prop_assert_eq!(solved(&m), a == b);
    }

    /// Two open patterns are undecidable here (`Open`), never a false verdict.
    #[test]
    fn open_against_open_is_open(a in holed(), b in holed()) {
        prop_assert_eq!(a.unify(&b), Match::Open);
    }
}

// ── Totality and canonicalization ───────────────────────────────────────────

proptest! {
    /// Unification never panics and yields exactly one of three verdicts.
    #[test]
    fn unify_total(a in any_pat(), b in any_pat()) {
        let m = a.unify(&b);
        prop_assert!(matches!(m, Match::Solved(_) | Match::Empty | Match::Open));
    }

    /// `raw(s)` and a single-literal `build` agree (interning relies on it).
    #[test]
    fn raw_is_canonical(s in leaf()) {
        prop_assert_eq!(Pattern::raw(&s), Pattern::build(vec![Part::Lit(Regex::literal(&s))]));
    }

    /// A concrete type is closed and has a regex; `⊤` is open.
    #[test]
    fn concrete_is_closed(t in concrete_ty()) {
        prop_assert!(t.is_closed());
        prop_assert!(t.as_regex().is_some());
        prop_assert!(!t.is_top());
    }
}

// ── Power envelope: the documented limits of string-capture ─────────────────
// These pin the boundary, which is exactly the spec for the term-unification
// upgrade (draft §2): a parsed Arrow tree would lift each of these.

fn arrow_pat() -> Pattern {
    Pattern::build(vec![
        Part::Hole("A".into()),
        Part::Lit(Regex::literal("→")),
        Part::Hole("B".into()),
    ])
}

#[test]
fn capture_is_leftmost_only() {
    // `?A→?B` against `a→b→c` finds only the leftmost split. The alternative
    // (A=`a→b`, B=`c`) is unreachable: a flat string cannot enumerate splits.
    let Match::Solved(s) = arrow_pat().unify(&Pattern::raw("a→b→c")) else {
        panic!("expected solved")
    };
    assert_eq!(s["A"], Binding::Lit("a".into()));
    assert_eq!(s["B"], Binding::Lit("b→c".into()));
}

#[test]
fn multi_hole_against_a_set_defers() {
    // A multi-hole pattern cannot split a non-literal regular set: `Open`, never
    // a wrong verdict. Structural leaves would need parsing.
    let set = Pattern::closed(Regex::or(Regex::literal("a→b"), Regex::literal("c→d")));
    assert_eq!(arrow_pat().unify(&set), Match::Open);
}

#[test]
fn empty_captures_are_allowed() {
    // `?A→?B` against `→x`: A binds the empty string.
    let Match::Solved(s) = arrow_pat().unify(&Pattern::raw("→x")) else {
        panic!("expected solved")
    };
    assert_eq!(s["A"], Binding::Lit(String::new()));
    assert_eq!(s["B"], Binding::Lit("x".into()));
}

#[test]
fn bottom_absorbed_by_bare_hole_but_not_split() {
    // A bare hole absorbs ⊥; a multi-hole pattern cannot split it → `Open`.
    let Match::Solved(s) = Pattern::hole("A").unify(&Pattern::bottom()) else {
        panic!("expected solved")
    };
    assert_eq!(s["A"], Binding::Bot);
    assert_eq!(arrow_pat().unify(&Pattern::bottom()), Match::Open);
}
