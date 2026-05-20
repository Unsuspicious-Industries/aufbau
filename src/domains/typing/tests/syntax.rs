use crate::domains::typing::{Type, TypeExpr};

#[test]
fn closed_type_parses_raw_literals() {
    let t = Type::parse("'Int'").unwrap();
    assert_eq!(t, Type::Raw("Int".into()));
}
#[test]
fn closed_type_parses_arrows() {
    let t = Type::parse("'Int' -> 'Bool'").unwrap();
    assert_eq!(
        t,
        Type::Arrow(
            Box::new(Type::Raw("Int".into())),
            Box::new(Type::Raw("Bool".into()))
        )
    );
}
#[test]
fn closed_type_arrow_right_associative() {
    let t = Type::parse("'A' -> 'B' -> 'C'").unwrap();
    match &t {
        Type::Arrow(left, right) => {
            assert_eq!(**left, Type::Raw("A".into()));
            assert!(matches!(**right, Type::Arrow(_, _)));
        }
        _ => panic!(),
    }
}
#[test]
fn closed_type_parses_unions() {
    let t = Type::parse("'Int' | 'Bool'").unwrap();
    match t {
        Type::Union(parts) => {
            assert_eq!(parts.len(), 2);
        }
        other => panic!("{:?}", other),
    }
}
#[test]
fn closed_type_parses_negation() {
    assert!(matches!(Type::parse("¬'Int'").unwrap(), Type::Not(_)));
}
#[test]
fn closed_type_parses_any_and_none() {
    assert_eq!(Type::parse("⊤").unwrap(), Type::Any);
    assert_eq!(Type::parse("∅").unwrap(), Type::None);
}
#[test]
fn closed_type_rejects_meta() {
    assert!(Type::parse("?A").is_err());
}
#[test]
fn closed_type_rejects_typeof() {
    assert!(Type::parse("typeof(x)").is_err());
}

#[test]
fn type_expr_parses_meta() {
    assert_eq!(TypeExpr::parse("?A").unwrap(), TypeExpr::Meta("A".into()));
}
#[test]
fn type_expr_parses_typeof() {
    assert_eq!(
        TypeExpr::parse("typeof(x)").unwrap(),
        TypeExpr::TypeOf("x".into())
    );
}
#[test]
fn type_expr_parses_ctx() {
    assert_eq!(
        TypeExpr::parse("Γ(x)").unwrap(),
        TypeExpr::ContextExt("x".into())
    );
}
#[test]
fn type_expr_parses_arrow_metas() {
    assert_eq!(
        TypeExpr::parse("?A -> ?B").unwrap(),
        TypeExpr::Arrow(
            Box::new(TypeExpr::Meta("A".into())),
            Box::new(TypeExpr::Meta("B".into()))
        )
    );
}
#[test]
fn type_expr_parses_lit() {
    assert_eq!(
        TypeExpr::parse("'Int'").unwrap(),
        TypeExpr::Lit("Int".into())
    );
}

#[test]
fn type_expr_roundtrip_meta() {
    let o = TypeExpr::Meta("A".into());
    assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
}
#[test]
fn type_expr_roundtrip_arrow() {
    let o = TypeExpr::Arrow(
        Box::new(TypeExpr::Meta("A".into())),
        Box::new(TypeExpr::Meta("B".into())),
    );
    assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
}

#[test]
fn type_parse_raw_bare_identifiers() {
    assert_eq!(
        Type::parse_raw("number").unwrap(),
        Type::Raw("number".into())
    );
}
#[test]
fn multi_arg_fn_type_curried() {
    let t = Type::parse_raw("(number, string) => boolean").unwrap();
    assert_eq!(
        t,
        Type::Arrow(
            Box::new(Type::Raw("number".into())),
            Box::new(Type::Arrow(
                Box::new(Type::Raw("string".into())),
                Box::new(Type::Raw("boolean".into()))
            ))
        )
    );
}
#[test]
fn type_expr_roundtrip_typeof() {
    let o = TypeExpr::TypeOf("x".into());
    assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
}
#[test]
fn type_expr_roundtrip_ctx() {
    let o = TypeExpr::ContextExt("x".into());
    assert_eq!(TypeExpr::parse(&o.to_string()).unwrap(), o);
}
