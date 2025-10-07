use super::*;
use crate::logic::grammar::Production;
use crate::logic::partial::{PartialASTNode, PartialNonTerminal, PartialProduction, PartialTerminal};
use crate::logic::typing::{Conclusion, Premise, Type, TypingJudgment, TypingRule};
use crate::logic::typing::rule::ConclusionKind;

/// Helper to create a simple PartialNonTerminal for testing
fn mk_partial_nt(value: &str, binding: Option<&str>, children: Vec<PartialASTNode>) -> PartialNonTerminal {
    PartialNonTerminal {
        production: PartialProduction::from_progress(
            Production {
                rule: None,
                rhs: vec![],
            },
            0,
            0,
        ),
        children,
        value: value.to_string(),
        span: None,
        binding: binding.map(|s| s.to_string()),
        bound_typing_rule: None,
    }
}

/// Helper to create a PartialTerminal
fn mk_terminal(value: &str, binding: Option<&str>) -> PartialASTNode {
    PartialASTNode::Terminal(PartialTerminal {
        value: value.to_string(),
        span: None,
        binding: binding.map(|s| s.to_string()),
    })
}

/// Helper to create a PartialNonTerminal node from alternatives
fn mk_nt_node(alts: Vec<PartialNonTerminal>) -> PartialASTNode {
    PartialASTNode::NonTerminal(alts)
}

#[test]
fn test_bind_type_partial_atom() {
    // Create a simple partial AST: Expr(x=int)
    let int_term = mk_terminal("int", None);
    let x_nt = mk_partial_nt("Type", Some("x"), vec![int_term]);
    let root = mk_partial_nt("Expr", None, vec![mk_nt_node(vec![x_nt])]);

    // Bind the type variable 'x' -> should resolve to BoundType::Atom("int")
    let bound = bind_type_partial(&root, Type::Atom("x".to_string()));
    assert_eq!(bound, Some(BoundType::Atom("int".to_string())));
}

#[test]
fn test_bind_type_partial_raw() {
    let root = mk_partial_nt("Expr", None, vec![]);
    
    // Raw types should bind directly
    let bound = bind_type_partial(&root, Type::Raw("int".to_string()));
    assert_eq!(bound, Some(BoundType::Atom("int".to_string())));
}

#[test]
fn test_bind_type_partial_arrow() {
    // Create: Lambda(x=int, y=bool)
    let int_term = mk_terminal("int", None);
    let bool_term = mk_terminal("bool", None);
    let x_nt = mk_partial_nt("Type", Some("x"), vec![int_term]);
    let y_nt = mk_partial_nt("Type", Some("y"), vec![bool_term]);
    let root = mk_partial_nt("Lambda", None, vec![mk_nt_node(vec![x_nt]), mk_nt_node(vec![y_nt])]);

    // Bind x -> y
    let arrow_ty = Type::Arrow(
        Box::new(Type::Atom("x".to_string())),
        Box::new(Type::Atom("y".to_string())),
    );
    let bound = bind_type_partial(&root, arrow_ty);
    assert_eq!(
        bound,
        Some(BoundType::Arrow(
            Box::new(BoundType::Atom("int".to_string())),
            Box::new(BoundType::Atom("bool".to_string())),
        ))
    );
}

#[test]
fn test_bind_type_partial_tuple() {
    // Create: Tuple with multiple 'elem' bindings at the same level
    // Root(elem=int, elem=bool, elem=str)
    let elem1 = mk_partial_nt("Type", Some("elem"), vec![mk_terminal("int", None)]);
    let elem2 = mk_partial_nt("Type", Some("elem"), vec![mk_terminal("bool", None)]);
    let elem3 = mk_partial_nt("Type", Some("elem"), vec![mk_terminal("str", None)]);
    let root = mk_partial_nt(
        "Tuple",
        None,
        vec![
            mk_nt_node(vec![elem1]),
            mk_nt_node(vec![elem2]),
            mk_nt_node(vec![elem3]),
        ],
    );

    // Bind Tuple(elem)
    let tuple_ty = Type::Tuple("elem".to_string());
    let bound = bind_type_partial(&root, tuple_ty);
    assert_eq!(
        bound,
        Some(BoundType::Tuple(vec![
            BoundType::Atom("int".to_string()),
            BoundType::Atom("bool".to_string()),
            BoundType::Atom("str".to_string()),
        ]))
    );
}

#[test]
fn test_get_nt_binding_partial() {
    // Create: Expr(x=Var(foo))
    let var_term = mk_terminal("foo", None);
    let x_nt = mk_partial_nt("Var", Some("x"), vec![var_term]);
    let root = mk_partial_nt("Expr", None, vec![mk_nt_node(vec![x_nt.clone()])]);

    // Should find binding 'x'
    let bound = get_nt_binding_partial(&root, "x".to_string());
    assert!(bound.is_some());
    assert_eq!(bound.unwrap().value, "Var");
}

#[test]
fn test_get_var_binding_partial() {
    // Create: Expr(x=Var(myvar))
    let var_term = mk_terminal("myvar", None);
    let x_nt = mk_partial_nt("Var", Some("x"), vec![var_term]);
    let root = mk_partial_nt("Expr", None, vec![mk_nt_node(vec![x_nt])]);

    // Should extract terminal value "myvar"
    let bound = get_var_binding_partial(&root, "x".to_string());
    assert_eq!(bound, Ok(Some("myvar".to_string())));
}

#[test]
fn test_collect_nt_bindings_same_level_partial() {
    // Create: Expr(arg=A, arg=B, arg=C) - three args at same level
    let arg1 = mk_partial_nt("Arg", Some("arg"), vec![mk_terminal("a", None)]);
    let arg2 = mk_partial_nt("Arg", Some("arg"), vec![mk_terminal("b", None)]);
    let arg3 = mk_partial_nt("Arg", Some("arg"), vec![mk_terminal("c", None)]);
    let root = mk_partial_nt(
        "Expr",
        None,
        vec![
            mk_nt_node(vec![arg1]),
            mk_nt_node(vec![arg2]),
            mk_nt_node(vec![arg3]),
        ],
    );

    // Should collect all three 'arg' bindings
    let collected = collect_nt_bindings_same_level_partial(&root, "arg");
    assert_eq!(collected.len(), 3);
}

#[test]
fn test_resolve_simple_typing_rule() {
    // Create a simple AST: Expr(x=int)
    let int_term = mk_terminal("int", None);
    let x_nt = mk_partial_nt("Type", Some("x"), vec![int_term]);
    let root = mk_partial_nt("Expr", None, vec![mk_nt_node(vec![x_nt])]);

    // Create a simple typing rule: |- x
    let rule = TypingRule {
        name: "TypeVar".to_string(),
        premises: vec![],
        conclusion: Conclusion {
            context: Default::default(),
            kind: ConclusionKind::Type(Type::Atom("x".to_string())),
        },
    };

    // Resolve the rule
    let resolver = DefaultBindingResolver;
    let bound_rule = resolver.resolve_rule(&rule, &root);
    assert!(bound_rule.is_ok());
    
    let bound = bound_rule.unwrap();
    assert_eq!(bound.name, "TypeVar");
    match &bound.conclusion.kind {
        BoundConclusionKind::Type(ty) => {
            assert_eq!(*ty, BoundType::Atom("int".to_string()));
        }
        _ => panic!("Expected Type conclusion"),
    }
}

#[test]
fn test_resolve_rule_with_premise() {
    // Create AST: App(f=Func, arg=Value)
    let f_term = mk_terminal("myFunc", None);
    let arg_term = mk_terminal("myArg", None);
    let f_nt = mk_partial_nt("Func", Some("f"), vec![f_term]);
    let arg_nt = mk_partial_nt("Value", Some("arg"), vec![arg_term]);
    let root = mk_partial_nt("App", None, vec![mk_nt_node(vec![f_nt]), mk_nt_node(vec![arg_nt])]);

    // Create rule: f:'int' |- f
    let rule = TypingRule {
        name: "AppRule".to_string(),
        premises: vec![Premise {
            setting: None,
            judgment: Some(TypingJudgment::Ascription((
                "f".to_string(),
                Type::Raw("int".to_string()),
            ))),
        }],
        conclusion: Conclusion {
            context: Default::default(),
            kind: ConclusionKind::Type(Type::Atom("f".to_string())),
        },
    };

    let resolver = DefaultBindingResolver;
    let bound_rule = resolver.resolve_rule(&rule, &root);
    assert!(bound_rule.is_ok());
    
    let bound = bound_rule.unwrap();
    assert_eq!(bound.premises.len(), 1);
}

#[test]
fn test_bound_type_is_subtype() {
    let int_ty = BoundType::Atom("int".to_string());
    let bool_ty = BoundType::Atom("bool".to_string());
    
    // Reflexivity
    assert!(int_ty.is_subtype_of(&int_ty));
    
    // Different atoms are not subtypes
    assert!(!int_ty.is_subtype_of(&bool_ty));
    
    // Everything is subtype of Universe
    assert!(int_ty.is_subtype_of(&BoundType::Universe));
    
    // Empty is subtype of everything
    assert!(BoundType::Empty.is_subtype_of(&int_ty));
}

#[test]
fn test_bound_type_union() {
    let int_ty = BoundType::Atom("int".to_string());
    let bool_ty = BoundType::Atom("bool".to_string());
    
    let union = int_ty.clone().union_with(bool_ty.clone());
    
    // Union should be created
    match union {
        BoundType::Union(left, right) => {
            assert_eq!(*left, int_ty);
            assert_eq!(*right, bool_ty);
        }
        _ => panic!("Expected Union type"),
    }
}

#[test]
fn test_bound_type_overlaps() {
    let int_ty = BoundType::Atom("int".to_string());
    let bool_ty = BoundType::Atom("bool".to_string());
    
    // Same types overlap
    assert!(int_ty.overlaps_with(&int_ty));
    
    // Different atoms don't overlap
    assert!(!int_ty.overlaps_with(&bool_ty));
    
    // Universe overlaps with everything
    assert!(int_ty.overlaps_with(&BoundType::Universe));
    assert!(BoundType::Universe.overlaps_with(&int_ty));
    
    // Empty overlaps with nothing
    assert!(!int_ty.overlaps_with(&BoundType::Empty));
    assert!(!BoundType::Empty.overlaps_with(&int_ty));
}
