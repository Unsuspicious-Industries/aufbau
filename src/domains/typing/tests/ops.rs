use crate::domains::typing::ops::{equal, subtype, Unifier};
use crate::domains::typing::Type;

fn parse(t: &str) -> Type {
    Type::parse(t).expect("type should parse")
}

#[test]
fn unify_raw_same() {
    assert!(Unifier::unify(&parse("'Int'"), &parse("'Int'")).is_ok());
}

#[test]
fn unify_raw_different() {
    assert!(Unifier::unify(&parse("'Int'"), &parse("'Bool'")).is_fail());
}

#[test]
fn unify_arrow_same() {
    let t = parse("'Int' -> 'Bool'");
    assert!(Unifier::unify(&t, &t).is_ok());
}

#[test]
fn unify_any_indeterminate() {
    assert!(Unifier::unify(&Type::Any, &parse("'Int'")).is_indeterminate());
}

#[test]
fn unify_none_vs_non_none_fails() {
    assert!(Unifier::unify(&Type::None, &parse("'Int'")).is_fail());
}

#[test]
fn unify_partial_unwraps() {
    let partial = Type::Partial(Box::new(parse("'Int'")), "'Int'".into());
    assert!(Unifier::unify(&partial, &parse("'Int'")).is_ok());
}

#[test]
fn unify_union_same_arity() {
    let t = parse("'Int' | 'Bool'");
    assert!(Unifier::unify(&t, &t).is_ok());
}

#[test]
fn unify_union_different_arity_fails() {
    let a = parse("'Int' | 'Bool'");
    let b = parse("'Int' | 'Bool' | 'String'");
    assert!(Unifier::unify(&a, &b).is_fail());
}

#[test]
fn equal_partial_delegates_to_inner() {
    let p = Type::Partial(Box::new(parse("'Int'")), "x".into());
    assert_eq!(equal(&p, &parse("'Int'")), Some(true));
}

#[test]
fn subtype_none_is_bottom() {
    assert!(subtype(&Type::None, &parse("'Int'")));
}

#[test]
fn subtype_everything_is_top() {
    assert!(subtype(&parse("'Int'"), &Type::Any));
}

#[test]
fn subtype_reflexive() {
    let t = parse("'Int'");
    assert!(subtype(&t, &t));
}

#[test]
fn subtype_arrow_contravariant() {
    let arrow1 = Type::Arrow(Box::new(Type::Any), Box::new(Type::Raw("Int".into())));
    let arrow2 = Type::Arrow(Box::new(Type::Raw("Int".into())), Box::new(Type::Any));
    assert!(subtype(&arrow1, &arrow2));
}

#[test]
fn subtype_member_into_union() {
    let int_t = parse("'Int'");
    let union_t = parse("'Int' | 'Bool'");
    assert!(subtype(&int_t, &union_t));
}

#[test]
fn subtype_union_not_into_single_member() {
    let union_t = parse("'Int' | 'Bool'");
    let int_t = parse("'Int'");
    assert!(!subtype(&union_t, &int_t));
}
