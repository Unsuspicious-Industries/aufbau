use crate::logic::typing::Type;

#[test]
fn test_parse_raw() {
    println!("A -> {:?}", Type::parse("A"));
    println!("A raw -> {:?}", Type::parse_raw("A"));
}

#[test]
fn test_parse_raw_preserves_ts_surface_types() {
    let cases = [
        "number[]",
        "{ name: string; age: number }",
        "(number, string) => boolean",
    ];

    for case in cases {
        match Type::parse_raw(case).expect("raw parse should succeed") {
            Type::Raw(value) => assert_eq!(value, case),
            other => panic!("expected raw type for '{}', got {:?}", case, other),
        }
    }
}

#[test]
fn test_parse_array_types() {
    match Type::parse("number[]").expect("parse array type") {
        Type::Array(inner) => match *inner {
            Type::Atom(name) => assert_eq!(name, "number"),
            other => panic!("expected inner atom, got {:?}", other),
        },
        other => panic!("expected array type, got {:?}", other),
    }

    match Type::parse_raw("number[]").expect("parse raw array type") {
        Type::Array(inner) => match *inner {
            Type::Raw(name) => assert_eq!(name, "number"),
            other => panic!("expected inner raw type, got {:?}", other),
        },
        other => panic!("expected raw array type, got {:?}", other),
    }
}
