//! `TyExpr::build` recovers a rule type-expression's tree by parsing it with the
//! grammar (§2): the rule's structure is the grammar's own.

use crate::engine::grammar::SPG;
use crate::typing::{TyExpr, TypeExpr};

fn grammar() -> SPG {
    SPG::load(
        r#"
        Atom ::= 'Int' | 'Bool'
        Fun  ::= Atom '->' Type
        Type ::= Fun | Atom
        Start ::= Type
        "#,
    )
    .unwrap()
}

fn build(g: &SPG, s: &str) -> TyExpr {
    TyExpr::build(g, &TypeExpr::parse(s).unwrap()).unwrap()
}

fn con(label: &str, kids: Vec<TyExpr>) -> TyExpr {
    TyExpr::Con(label.into(), kids)
}

#[test]
fn lone_hole_is_var() {
    assert_eq!(build(&grammar(), "?B"), TyExpr::Var("B".into()));
}

#[test]
fn lone_ref_and_ctx_and_lattice() {
    let g = grammar();
    assert_eq!(build(&g, "τ"), TyExpr::Ref("τ".into()));
    assert_eq!(build(&g, "Γ(x)"), TyExpr::Ctx("x".into()));
    assert_eq!(build(&g, "⊤"), TyExpr::Top);
    assert_eq!(build(&g, "∅"), TyExpr::Bot);
    assert_eq!(build(&g, "'Int'"), TyExpr::Lit("Int".into()));
}

#[test]
fn arrow_of_holes_is_a_constructor() {
    // `?A -> ?B` parses to the `Fun` production over two hole leaves.
    assert_eq!(
        build(&grammar(), "?A -> ?B"),
        con("Fun", vec![TyExpr::Var("A".into()), TyExpr::Var("B".into())]),
    );
}

#[test]
fn ref_and_hole_mixed() {
    assert_eq!(
        build(&grammar(), "τ -> ?B"),
        con("Fun", vec![TyExpr::Ref("τ".into()), TyExpr::Var("B".into())]),
    );
}

#[test]
fn arrow_is_right_associative_from_the_grammar() {
    // `?A -> ?B -> ?C` = `?A -> (?B -> ?C)` because `Fun ::= Atom '->' Type`.
    assert_eq!(
        build(&grammar(), "?A -> ?B -> ?C"),
        con(
            "Fun",
            vec![
                TyExpr::Var("A".into()),
                con("Fun", vec![TyExpr::Var("B".into()), TyExpr::Var("C".into())]),
            ],
        ),
    );
}

#[test]
fn literal_atom_in_structure() {
    assert_eq!(
        build(&grammar(), "'Int' -> ?B"),
        con("Fun", vec![TyExpr::Lit("Int".into()), TyExpr::Var("B".into())]),
    );
}

#[test]
fn overlap_with_hole_syntax_is_refused() {
    // A grammar whose own tokens can start with `?` cannot use `?` as the meta
    // marker; building a structured pattern must error rather than misparse.
    let g = SPG::load(
        r#"
        Name ::= /\?[a-z]+/
        Pair ::= Name '-' Name
        Start ::= Pair
        "#,
    )
    .unwrap();
    let err = TyExpr::build(&g, &TypeExpr::parse("?A - ?B").unwrap());
    assert!(err.is_err(), "expected overlap refusal, got {err:?}");
}
