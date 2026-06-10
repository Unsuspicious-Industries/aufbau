//! Tree-unification prototype (§2): the three string-capture limits lifted, the
//! core Robinson laws, and the engine bridge. Compare `unify_props.rs`, whose
//! envelope tests pin exactly the limits this module removes.

use crate::engine::grammar::SPG;
use crate::typing::pattern::Pattern;
use crate::typing::term::{Subst, Term, apply, unify};
use crate::typing::{Context, TypingSynth};

fn arrow(a: Term, b: Term) -> Term {
    Term::con("->", vec![a, b])
}

fn solve(a: &Term, b: &Term) -> Option<Subst> {
    let mut s = Subst::new();
    unify(a, b, &mut s, true).then_some(s)
}

// ── The three lifted limits ─────────────────────────────────────────────────

/// Limit 1 (leftmost-only) is gone: the *parse* fixes the split, not the
/// unifier. `?A→?B` reads each tree's top node, so `a→b→c` parsed
/// right-associatively gives `A=a, B=b→c`, and the same pattern on the
/// left-associative tree gives `A=a→b, B=c`. Both are reachable — the shape
/// chose, once.
#[test]
fn no_leftmost_ambiguity() {
    let pat = arrow(Term::var("A"), Term::var("B"));

    let right = arrow(Term::leaf("a"), arrow(Term::leaf("b"), Term::leaf("c")));
    let s = solve(&pat, &right).expect("unifies");
    assert_eq!(apply(&Term::var("A"), &s), Term::leaf("a"));
    assert_eq!(
        apply(&Term::var("B"), &s),
        arrow(Term::leaf("b"), Term::leaf("c"))
    );

    let left = arrow(arrow(Term::leaf("a"), Term::leaf("b")), Term::leaf("c"));
    let s = solve(&pat, &left).expect("unifies");
    assert_eq!(
        apply(&Term::var("A"), &s),
        arrow(Term::leaf("a"), Term::leaf("b"))
    );
    assert_eq!(apply(&Term::var("B"), &s), Term::leaf("c"));
}

/// Limit 3 (two open patterns → `Open`) is gone: variables on both sides bind
/// natively. `?A→?B` against `?X→?Y` solves `A=X, B=Y`.
#[test]
fn two_open_patterns_align() {
    let p = arrow(Term::var("A"), Term::var("B"));
    let q = arrow(Term::var("X"), Term::var("Y"));
    let s = solve(&p, &q).expect("unifies");
    assert_eq!(apply(&Term::var("A"), &s), Term::var("X"));
    assert_eq!(apply(&Term::var("B"), &s), Term::var("Y"));
}

/// Limit 2 (cannot split a non-literal set → `Open`) is gone: a set lives at a
/// leaf and meets via `Pattern`. `{Int,Bool}` and `{Bool,Str}` share `Bool`,
/// so the leaves unify; inside a constructor they unify pointwise.
#[test]
fn set_valued_leaves_meet() {
    let ab = Term::Leaf(Pattern::closed(crate::regex::Regex::union_many(
        ["Int", "Bool"]
            .into_iter()
            .map(crate::regex::Regex::literal),
    )));
    let bs = Term::Leaf(Pattern::closed(crate::regex::Regex::union_many(
        ["Bool", "Str"]
            .into_iter()
            .map(crate::regex::Regex::literal),
    )));
    assert!(solve(&ab, &bs).is_some());
    // No overlap → clash.
    let cd = Term::leaf("Char");
    assert!(solve(&ab, &cd).is_none());
    // The same set inside a constructor unifies against a hole.
    let s = solve(
        &arrow(ab.clone(), Term::leaf("c")),
        &arrow(Term::var("A"), Term::leaf("c")),
    )
    .expect("unifies");
    assert_eq!(apply(&Term::var("A"), &s), ab);
}

// ── Core Robinson laws ──────────────────────────────────────────────────────

#[test]
fn constructor_clash_and_arity() {
    // Different labels clash.
    assert!(
        solve(
            &arrow(Term::leaf("a"), Term::leaf("b")),
            &Term::con(",", vec![Term::leaf("a"), Term::leaf("b")])
        )
        .is_none()
    );
    // Different arity clashes.
    assert!(
        solve(
            &Term::con("f", vec![Term::leaf("a")]),
            &Term::con("f", vec![Term::leaf("a"), Term::leaf("b")])
        )
        .is_none()
    );
    // Con vs Leaf clashes.
    assert!(solve(&arrow(Term::leaf("a"), Term::leaf("b")), &Term::leaf("a")).is_none());
}

#[test]
fn shared_variable_propagates() {
    // `?A→?A` vs `Int→?B` forces `A=Int` then `B=Int`.
    let p = arrow(Term::var("A"), Term::var("A"));
    let q = arrow(Term::leaf("Int"), Term::var("B"));
    let s = solve(&p, &q).expect("unifies");
    assert_eq!(apply(&Term::var("A"), &s), Term::leaf("Int"));
    assert_eq!(apply(&Term::var("B"), &s), Term::leaf("Int"));
}

#[test]
fn shared_variable_can_clash() {
    // `?A→?A` vs `Int→Bool` is unsatisfiable: A cannot be both.
    let p = arrow(Term::var("A"), Term::var("A"));
    let q = arrow(Term::leaf("Int"), Term::leaf("Bool"));
    assert!(solve(&p, &q).is_none());
}

/// The occurs-check toggle: rejecting an infinite type, or admitting it as a
/// rational tree (a recursive type, §2).
#[test]
fn occurs_check_toggles_recursive_types() {
    let recursive = arrow(Term::var("A"), Term::leaf("B"));
    // Sound syntactic unification rejects `A = A→B`.
    let mut s = Subst::new();
    assert!(!unify(&Term::var("A"), &recursive, &mut s, true));
    // Dropping the check binds it: `s` now denotes the rational tree `μA.A→B`.
    let mut s = Subst::new();
    assert!(unify(&Term::var("A"), &recursive, &mut s, false));
    assert_eq!(s.get("A"), Some(&recursive));
}

// ── Engine bridge: terms from the type sub-grammar ──────────────────────────

/// A concrete type parsed by the main engine becomes a `Term`, and unifies
/// against a holed pattern term — the structure the parser committed to drives
/// the match, with no string splitting.
#[test]
fn from_engine_parsed_type() {
    let grammar = SPG::load(
        r#"
        Atom ::= 'Int' | 'Bool'
        Fun ::= Atom '->' Type
        Type ::= Fun | Atom
        Start ::= Type
        "#,
    )
    .unwrap();
    let mut synth = TypingSynth::new(grammar, "Int->Bool");
    let ast = synth.parse_with(&Context::new()).unwrap();
    let root = ast
        .roots()
        .find(|r| r.is_complete())
        .expect("complete root");

    let term = Term::from_node(&root);
    // Collapsed through the transparent Start/Type wrappers to the Fun node.
    let expected = Term::con("Fun", vec![Term::leaf("Int"), Term::leaf("Bool")]);
    assert_eq!(term, expected);

    // It unifies against `?A → ?B` (labelled by the same nonterminal).
    let pat = Term::con("Fun", vec![Term::var("A"), Term::var("B")]);
    let s = solve(&pat, &term).expect("unifies");
    assert_eq!(apply(&Term::var("A"), &s), Term::leaf("Int"));
    assert_eq!(apply(&Term::var("B"), &s), Term::leaf("Bool"));
}

// ── ⊤ and ⊥ rules (the lattice ends) ────────────────────────────────────────

/// ⊤ unifies with everything — variable, constructor, leaf, and ⊥ — and never
/// records a binding, so distinct ⊤s are independent (no spurious identity).
#[test]
fn top_unifies_with_everything_and_binds_nothing() {
    let cases = [
        Term::var("A"),
        Term::leaf("Int"),
        arrow(Term::leaf("Int"), Term::leaf("Bool")),
        Term::bottom(),
        Term::top(),
    ];
    for t in &cases {
        let s = solve(&Term::top(), t).expect("⊤ unifies");
        assert!(s.is_empty(), "⊤ must not bind, got {s:?} against {t}");
        // Symmetric.
        assert!(solve(t, &Term::top()).is_some());
    }
}

/// Two ⊤s ascribed to one hole leave it unconstrained, and a later concrete
/// type still fits — the bug a shared `Var("⊤")` would cause (false clash).
#[test]
fn two_tops_do_not_constrain_a_shared_hole() {
    let mut s = Subst::new();
    assert!(unify(&Term::var("T"), &Term::top(), &mut s, true));
    assert!(unify(&Term::var("T"), &Term::top(), &mut s, true));
    assert!(unify(&Term::var("T"), &Term::leaf("Int"), &mut s, true));
    assert_eq!(apply(&Term::var("T"), &s), Term::leaf("Int"));
}

/// ⊥ is absorbing: it clashes with any other leaf and any constructor, but it
/// still instantiates a bare variable (`?A ⊓ ⊥ = ⊥`).
#[test]
fn bottom_clashes_except_against_a_variable() {
    assert!(solve(&Term::bottom(), &Term::leaf("Int")).is_none());
    assert!(solve(&Term::bottom(), &arrow(Term::leaf("a"), Term::leaf("b"))).is_none());
    assert!(solve(&Term::bottom(), &Term::bottom()).is_none());
    let s = solve(&Term::var("A"), &Term::bottom()).expect("⊥ instantiates a var");
    assert_eq!(apply(&Term::var("A"), &s), Term::bottom());
}
