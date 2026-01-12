use crate::logic::typing::Type;

fn is_partial_any(t: &Type) -> bool {
    match t {
        Type::Partial(inner, _) => matches!(**inner, Type::Any),
        _ => false,
    }
}

fn is_complete(t: &Type) -> bool {
    !matches!(t, Type::Partial(_, _))
}

#[test]
fn test_empty_input() {
    let res = Type::parse_partial("").unwrap();
    assert!(is_partial_any(&res));
    if let Type::Partial(inner, input) = res {
        assert!(matches!(*inner, Type::Any));
        assert_eq!(input, "");
    } else {
        panic!("Expected partial");
    }
}

#[test]
fn test_complete_atom() {
    let res = Type::parse_partial("int").unwrap();
    assert!(is_complete(&res));
    if let Type::Atom(s) = res {
        assert_eq!(s, "int");
    } else {
        panic!("Expected Atom, got {:?}", res);
    }
}

#[test]
fn test_partial_arrow_start() {
    let res = Type::parse_partial("int ->").unwrap();
    // Should be Arrow(int, Any) - type constraint: any function starting with int
    if let Type::Partial(inner, input) = res {
        if let Type::Arrow(l, r) = *inner {
            if let Type::Atom(s) = *l {
                assert_eq!(s, "int");
            } else {
                panic!("Left should be Atom(int)");
            }
            assert!(matches!(*r, Type::Any));
        } else {
            panic!("Expected Arrow");
        }
        assert_eq!(input, "int ->");
    } else {
        panic!("Expected partial");
    }
}

#[test]
fn test_partial_arrow_space() {
    let res = Type::parse_partial("int -> ").unwrap();
    // Should be Arrow(int, Any) - original input preserves trailing space
    if let Type::Partial(inner, input) = res {
        if let Type::Arrow(_l, r) = *inner {
            assert!(matches!(*r, Type::Any));
        } else {
            panic!("Expected Arrow");
        }
        assert_eq!(input, "int -> ");
    } else {
        panic!("Expected partial");
    }
}

#[test]
fn test_partial_parens_open() {
    let res = Type::parse_partial("(").unwrap();
    // Should be Partial type preserving original input

    if let Type::Partial(_inner, input) = res {
        assert_eq!(input, "(");
    } else {
        panic!("Expected partial");
    }
}

#[test]
fn test_partial_inner_parens() {
    let res = Type::parse_partial("(int").unwrap();
    // Inner parses as complete "int", but unbalanced parens make it partial
    // Return Type::Partial(int, original input)

    if let Type::Partial(inner, input) = res {
        if let Type::Atom(s) = *inner {
            assert_eq!(s, "int");
        } else {
            panic!("Expected Atom(int)");
        }
        assert_eq!(input, "(int");
    } else {
        panic!("Expected partial");
    }
}

#[test]
fn test_partial_nested_arrow() {
    let res = Type::parse_partial("A -> B ->").unwrap();
    // Arrow(A, Arrow(B, Any)) - type constraint for curried function
    if let Type::Partial(inner, input) = res {
        if let Type::Arrow(_l1, r1) = *inner {
            // l1 = A
            if let Type::Arrow(_l2, r2) = *r1 {
                // l2 = B
                assert!(matches!(*r2, Type::Any));
            } else {
                panic!("Expected nested Arrow");
            }
        } else {
            panic!("Expected Arrow");
        }
        assert_eq!(input, "A -> B ->");
    } else {
        panic!("Expected partial");
    }
}

#[test]
fn test_partial_identifier() {
    // "in" is a complete valid type (Type::Atom("in")).
    // parse_partial("in") returns complete Type::Atom("in").

    // What if we have unclosed raw?
    let res = Type::parse_partial("'int").unwrap();
    if let Type::Partial(inner, input) = res {
        if let Type::Raw(s) = *inner {
            assert_eq!(s, "int");
        } else {
            panic!("Expected Raw");
        }
        assert_eq!(input, "'int");
    } else {
        panic!("Expected partial");
    }
}

#[test]
fn test_partial_unicode_arrow_prefix_does_not_panic() {
   
    let res = Type::parse_partial("A-").unwrap();
    // "A-" should be partial because "-" is a prefix of "->"
    assert!(matches!(res, Type::Partial(_, _)), "A- should be partial");
    if let Type::Partial(inner, input) = res {
        if let Type::Arrow(l, r) = *inner {
            assert!(matches!(*l, Type::Atom(_)));
            assert!(matches!(*r, Type::Any));
        } else {
            panic!("Expected Arrow");
        }
        assert_eq!(input, "A-");
    }

    // Also ensure a complete unicode arrow works.
    let res2 = Type::parse_partial("A →").unwrap();
    assert!(matches!(res2, Type::Partial(_, _)), "A → should be partial (no right side)");
    if let Type::Partial(inner, input) = res2 {
        if let Type::Arrow(l, r) = *inner {
            assert!(matches!(*l, Type::Atom(_)));
            assert!(matches!(*r, Type::Any));
        } else {
            panic!("Expected Arrow");
        }
        assert_eq!(input, "A →");
    }

    // Test without space as well
    let res3 = Type::parse_partial("A→").unwrap();
    assert!(matches!(res3, Type::Partial(_, _)), "A→ should be partial (no right side)");
}

#[test]
fn test_partial_ascii_arrow_no_space() {
    // "A->" should be partial (no right side)
    let res = Type::parse_partial("A->").unwrap();
    assert!(matches!(res, Type::Partial(_, _)), "A-> should be partial");
    if let Type::Partial(inner, input) = res {
        if let Type::Arrow(l, r) = *inner {
            assert!(matches!(*l, Type::Atom(_)));
            assert!(matches!(*r, Type::Any));
        } else {
            panic!("Expected Arrow");
        }
        assert_eq!(input, "A->");
    }
}

#[test]
fn test_partial_arrow_with_trailing_space() {
    // "A -> " (with trailing space) should be partial
    let res = Type::parse_partial("A -> ").unwrap();
    assert!(matches!(res, Type::Partial(_, _)), "A ->  should be partial");
    if let Type::Partial(inner, input) = res {
        if let Type::Arrow(_l, r) = *inner {
            assert!(matches!(*r, Type::Any));
        } else {
            panic!("Expected Arrow");
        }
        assert_eq!(input, "A -> ");
    }
}

#[test]
fn test_complete_arrow_with_right_side() {
    // "A -> B" should be complete (not partial)
    let res = Type::parse_partial("A -> B").unwrap();
    assert!(matches!(res, Type::Arrow(_, _)), "A -> B should be complete");
    if let Type::Arrow(l, r) = res {
        assert!(matches!(*l, Type::Atom(_)));
        assert!(matches!(*r, Type::Atom(_)));
    }
}


#[test]
fn test_partial_negation_hole_is_any() {
    // Holes should be Any in partial types.
    let res = Type::parse_partial("¬").unwrap();
    if let Type::Partial(inner, input) = res {
        assert!(matches!(*inner, Type::Not(t) if matches!(*t, Type::Any)));
        assert_eq!(input, "¬");
    } else {
        panic!("Expected partial");
    }
}
