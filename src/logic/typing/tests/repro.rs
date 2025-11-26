//! Reproduction tests for specific parsing issues

use crate::logic::typing::Type;

#[test]
fn test_parse_silent_var() {
    let t = Type::parse("?A");
    assert!(t.is_ok(), "Failed to parse ?A: {:?}", t.err());
    if let Ok(Type::Atom(s)) = t {
        assert_eq!(s, "?A");
    } else {
        panic!("Parsed ?A as {:?}", t);
    }
}

#[test]
fn test_parse_silent_var_arrow() {
    let t = Type::parse("?A -> ?B");
    assert!(t.is_ok(), "Failed to parse ?A -> ?B: {:?}", t.err());
}

