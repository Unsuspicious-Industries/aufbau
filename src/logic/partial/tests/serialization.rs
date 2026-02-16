use crate::logic::grammar::Grammar;
use crate::logic::partial::serialize::{
    ast_to_sexpr, nt_to_sexpr, parse_sexpr, sexpr_to_ast, sexpr_to_nt, sexpr_to_string,
    sexpr_to_terminal, terminal_to_sexpr,
};
use crate::logic::partial::structure::{Node, NonTerminal, PartialAST, Terminal};
use crate::regex::Regex as DerivativeRegex;

#[test]
fn test_roundtrip_simple() {
    let grammar_str = r#"
    start ::= "hello"
    "#;

    let _grammar = Grammar::load(grammar_str).unwrap();

    // Create a simple complete terminal
    let terminal = Terminal::Complete {
        value: "hello".to_string(),
        binding: None,
        extension: None,
    };

    let sexpr = terminal_to_sexpr(&terminal);
    let roundtrip = sexpr_to_terminal(&sexpr, false).unwrap();

    assert_eq!(terminal, roundtrip);
}

#[test]
fn test_nt_serialization() {
    let grammar_str = r#"
    start ::= "hello" "world"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();
    let production = grammar.productions.get("start").unwrap()[0].clone();

    let nt = NonTerminal::new(
        "start".to_string(),
        production,
        0,
        vec![
            Node::Terminal(Terminal::Complete {
                value: "hello".to_string(),
                binding: None,
                extension: None,
            }),
            Node::Terminal(Terminal::Complete {
                value: "world".to_string(),
                binding: None,
                extension: None,
            }),
        ],
        None,
        2,
    );

    let sexpr = nt_to_sexpr(&nt);
    let roundtrip = sexpr_to_nt(&sexpr, &grammar).unwrap();

    assert_eq!(nt.name, roundtrip.name);
    assert_eq!(nt.alternative_index, roundtrip.alternative_index);
    assert_eq!(nt.children.len(), roundtrip.children.len());
}

#[test]
fn test_partial_ast_serialization() {
    let grammar_str = r#"
    start ::= "hello"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();
    let production = grammar.productions.get("start").unwrap()[0].clone();

    let root = NonTerminal::new(
        "start".to_string(),
        production,
        0,
        vec![Node::Terminal(Terminal::Partial {
            value: "hel".to_string(),
            binding: None,
            remainder: None,
        })],
        None,
        1,
    );

    let ast = PartialAST::new(vec![root], "hel".to_string());
    let sexpr = ast_to_sexpr(&ast);
    let s = sexpr_to_string(&sexpr);

    println!("Serialized:\n{}", s);

    // Parse it back
    let parsed_sexpr = parse_sexpr(&s).unwrap();
    let roundtrip = sexpr_to_ast(&parsed_sexpr, &grammar, "hel".to_string()).unwrap();

    assert_eq!(ast.roots.len(), roundtrip.roots.len());
}

#[test]
fn test_load_complete_terminal() {
    let grammar_str = r#"
    start ::= "hello"
    "#;

    let _grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"(T "hello")"#;
    let sexpr = parse_sexpr(s).unwrap();
    let terminal = sexpr_to_terminal(&sexpr, false).unwrap();

    match terminal {
        Terminal::Complete {
            value,
            binding,
            extension,
        } => {
            assert_eq!(value, "hello");
            assert_eq!(binding, None);
            assert_eq!(extension, None);
        }
        _ => panic!("Expected complete terminal"),
    }
}

#[test]
fn test_load_partial_terminal() {
    let grammar_str = r#"
    start ::= "hello"
    "#;

    let _grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"(T~ "hel")"#;
    let sexpr = parse_sexpr(s).unwrap();
    let terminal = sexpr_to_terminal(&sexpr, true).unwrap();

    match terminal {
        Terminal::Partial {
            value,
            binding,
            remainder,
        } => {
            assert_eq!(value, "hel");
            assert_eq!(binding, None);
            assert_eq!(remainder, None);
        }
        _ => panic!("Expected partial terminal"),
    }
}

#[test]
fn test_load_terminal_with_binding() {
    let grammar_str = r#"
    start ::= "hello"
    "#;

    let _grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"(T "hello" $greeting)"#;
    let sexpr = parse_sexpr(s).unwrap();
    let terminal = sexpr_to_terminal(&sexpr, false).unwrap();

    match terminal {
        Terminal::Complete { value, binding, .. } => {
            assert_eq!(value, "hello");
            assert_eq!(binding, Some("greeting".to_string()));
        }
        _ => panic!("Expected complete terminal"),
    }
}

#[test]
fn test_load_nonterminal() {
    let grammar_str = r#"
    start ::= "hello" "world"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"(start @0 (T "hello") (T "world"))"#;
    let sexpr = parse_sexpr(s).unwrap();
    let nt = sexpr_to_nt(&sexpr, &grammar).unwrap();

    assert_eq!(nt.name, "start");
    assert_eq!(nt.alternative_index, 0);
    assert_eq!(nt.children.len(), 2);
    assert!(nt.is_complete());
}

#[test]
fn test_load_partial_nonterminal() {
    let grammar_str = r#"
    start ::= "hello" "world"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"(start @0 (T "hello") (T~ "wor"))"#;
    let sexpr = parse_sexpr(s).unwrap();
    let nt = sexpr_to_nt(&sexpr, &grammar).unwrap();

    assert_eq!(nt.name, "start");
    assert_eq!(nt.alternative_index, 0);
    assert_eq!(nt.children.len(), 2);
    assert!(!nt.is_complete());
}

#[test]
fn test_load_nested_tree() {
    let grammar_str = r#"
    start ::= expr
    expr ::= "x" "+" "y"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"
    (start @0
      (expr @0
        (T "x")
        (T "+")
        (T "y")))
    "#;

    let sexpr = parse_sexpr(s).unwrap();
    let nt = sexpr_to_nt(&sexpr, &grammar).unwrap();

    assert_eq!(nt.name, "start");
    assert_eq!(nt.children.len(), 1);
    assert!(nt.is_complete());

    // Check nested expression
    if let Node::NonTerminal(expr) = &nt.children[0] {
        assert_eq!(expr.name, "expr");
        assert_eq!(expr.children.len(), 3);
    } else {
        panic!("Expected nonterminal child");
    }
}

#[test]
fn test_load_partial_ast_single_root() {
    let grammar_str = r#"
    start ::= "hello"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"(start @0 (T~ "hel"))"#;
    let sexpr = parse_sexpr(s).unwrap();
    let ast = sexpr_to_ast(&sexpr, &grammar, "hel".to_string()).unwrap();

    assert_eq!(ast.roots.len(), 1);
    assert!(!ast.is_complete());
    assert_eq!(ast.input(), "hel");
}

#[test]
fn test_load_partial_ast_multiple_roots() {
    let grammar_str = r#"
    start ::= "hello"
    other ::= "world"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    let s = r#"
    ((start @0 (T "hello"))
     (other @0 (T "world")))
    "#;

    let sexpr = parse_sexpr(s).unwrap();
    let ast = sexpr_to_ast(&sexpr, &grammar, "input".to_string()).unwrap();

    assert_eq!(ast.roots.len(), 2);
    assert_eq!(ast.roots[0].name, "start");
    assert_eq!(ast.roots[1].name, "other");
}

#[test]
fn test_load_with_alternative_index() {
    let grammar_str = r#"
    start ::= "hello" | "hi" | "hey"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    // Test alternative 0
    let s0 = r#"(start @0 (T "hello"))"#;
    let sexpr0 = parse_sexpr(s0).unwrap();
    let nt0 = sexpr_to_nt(&sexpr0, &grammar).unwrap();
    assert_eq!(nt0.alternative_index, 0);

    // Test alternative 1
    let s1 = r#"(start @1 (T "hi"))"#;
    let sexpr1 = parse_sexpr(s1).unwrap();
    let nt1 = sexpr_to_nt(&sexpr1, &grammar).unwrap();
    assert_eq!(nt1.alternative_index, 1);

    // Test alternative 2
    let s2 = r#"(start @2 (T "hey"))"#;
    let sexpr2 = parse_sexpr(s2).unwrap();
    let nt2 = sexpr_to_nt(&sexpr2, &grammar).unwrap();
    assert_eq!(nt2.alternative_index, 2);
}

#[test]
fn test_roundtrip_complex_tree() {
    let grammar_str = r#"
    start ::= expr
    expr ::= term "+" expr | term
    term ::= "x" | "y" | "(" expr ")"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    // Create a complex tree: (x + y)
    let production_start = grammar.productions.get("start").unwrap()[0].clone();
    let production_expr = grammar.productions.get("expr").unwrap()[0].clone();
    let production_term_x = grammar.productions.get("term").unwrap()[0].clone();
    let production_term_y = grammar.productions.get("term").unwrap()[1].clone();

    let term_x = NonTerminal::new(
        "term".to_string(),
        production_term_x,
        0,
        vec![Node::Terminal(Terminal::Complete {
            value: "x".to_string(),
            binding: None,
            extension: None,
        })],
        None,
        1,
    );

    let term_y = NonTerminal::new(
        "term".to_string(),
        production_term_y,
        1,
        vec![Node::Terminal(Terminal::Complete {
            value: "y".to_string(),
            binding: None,
            extension: None,
        })],
        None,
        1,
    );

    let expr_inner = NonTerminal::new(
        "expr".to_string(),
        production_expr.clone(),
        0,
        vec![
            Node::NonTerminal(term_x),
            Node::Terminal(Terminal::Complete {
                value: "+".to_string(),
                binding: None,
                extension: None,
            }),
            Node::NonTerminal(term_y),
        ],
        None,
        3,
    );

    let start = NonTerminal::new(
        "start".to_string(),
        production_start,
        0,
        vec![Node::NonTerminal(expr_inner)],
        None,
        1,
    );

    let ast = PartialAST::new(vec![start], "x+y".to_string());

    // Serialize and deserialize
    let sexpr = ast_to_sexpr(&ast);
    let s = sexpr_to_string(&sexpr);

    println!("Complex tree serialized:\n{}", s);

    let parsed_sexpr = parse_sexpr(&s).unwrap();
    let roundtrip = sexpr_to_ast(&parsed_sexpr, &grammar, "x+y".to_string()).unwrap();

    assert_eq!(ast.roots.len(), roundtrip.roots.len());
    assert_eq!(ast.roots[0].name, roundtrip.roots[0].name);
    assert!(roundtrip.is_complete());
}

#[test]
fn test_consumed_segments_preserved() {
    let grammar_str = r#"
    start ::= "a" "b" "c"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();
    let production = grammar.productions.get("start").unwrap()[0].clone();

    // Create a node with consumed_segments = 5 (different from children.len())
    let nt = NonTerminal::new(
        "start".to_string(),
        production,
        0,
        vec![
            Node::Terminal(Terminal::Complete {
                value: "a".to_string(),
                binding: None,
                extension: None,
            }),
            Node::Terminal(Terminal::Complete {
                value: "b".to_string(),
                binding: None,
                extension: None,
            }),
        ],
        None,
        5, // Different from children.len() which is 2
    );

    // Serialize
    let sexpr = nt_to_sexpr(&nt);
    let s = sexpr_to_string(&sexpr);

    println!("Serialized with consumed_segments=5:\n{}", s);

    // Deserialize
    let parsed_sexpr = parse_sexpr(&s).unwrap();
    let roundtrip = sexpr_to_nt(&parsed_sexpr, &grammar).unwrap();

    // Verify consumed_segments is preserved
    assert_eq!(nt.consumed_segments, roundtrip.consumed_segments);
    assert_eq!(roundtrip.consumed_segments, 5);
    assert_ne!(roundtrip.consumed_segments, roundtrip.children.len());
}

#[test]
fn test_all_terminal_fields_preserved() {
    let _grammar_str = r#"
    start ::= "hello"
    "#;

    // Complete terminal with all fields
    let terminal = Terminal::Complete {
        value: "hello".to_string(),
        binding: Some("greeting".to_string()),
        extension: Some(DerivativeRegex::literal("world")),
    };

    let sexpr = terminal_to_sexpr(&terminal);
    let s = sexpr_to_string(&sexpr);
    let parsed_sexpr = parse_sexpr(&s).unwrap();
    let roundtrip = sexpr_to_terminal(&parsed_sexpr, false).unwrap();

    assert_eq!(terminal, roundtrip);

    // Partial terminal with all fields
    let partial = Terminal::Partial {
        value: "hel".to_string(),
        binding: Some("partial_greeting".to_string()),
        remainder: Some(DerivativeRegex::literal("lo")),
    };

    let sexpr2 = terminal_to_sexpr(&partial);
    let s2 = sexpr_to_string(&sexpr2);
    let parsed_sexpr2 = parse_sexpr(&s2).unwrap();
    let roundtrip2 = sexpr_to_terminal(&parsed_sexpr2, true).unwrap();

    assert_eq!(partial, roundtrip2);
}

#[test]
fn test_complete_roundtrip_no_information_loss() {
    // This test verifies that EVERY field is preserved through serialization
    let grammar_str = r#"
    start ::= expr
    expr ::= term "+" expr | term
    term ::= "x" | "y"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    let production_start = grammar.productions.get("start").unwrap()[0].clone();
    let production_expr = grammar.productions.get("expr").unwrap()[0].clone();
    let production_term = grammar.productions.get("term").unwrap()[0].clone();

    // Build a tree with all possible field variations
    let term = NonTerminal::new(
        "term".to_string(),
        production_term,
        0,
        vec![Node::Terminal(Terminal::Complete {
            value: "x".to_string(),
            binding: Some("var".to_string()),
            extension: Some(DerivativeRegex::literal("_suffix")),
        })],
        Some("bound_term".to_string()),
        3, // Intentionally different from children.len()
    );

    let partial_term = NonTerminal::new(
        "term".to_string(),
        grammar.productions.get("term").unwrap()[1].clone(),
        1,
        vec![Node::Terminal(Terminal::Partial {
            value: "y".to_string(),
            binding: Some("partial_var".to_string()),
            remainder: Some(DerivativeRegex::literal("_rest")),
        })],
        None,
        2,
    );

    let expr = NonTerminal::new(
        "expr".to_string(),
        production_expr,
        0,
        vec![
            Node::NonTerminal(term),
            Node::Terminal(Terminal::Complete {
                value: "+".to_string(),
                binding: None,
                extension: None,
            }),
            Node::NonTerminal(partial_term),
        ],
        Some("bound_expr".to_string()),
        7,
    );

    let start = NonTerminal::new(
        "start".to_string(),
        production_start,
        0,
        vec![Node::NonTerminal(expr)],
        None,
        10,
    );

    let ast = PartialAST::new(vec![start.clone()], "x+y".to_string());

    // Serialize and deserialize
    let sexpr = ast_to_sexpr(&ast);
    let s = sexpr_to_string(&sexpr);

    println!("\n=== Full serialization ===\n{}\n", s);

    let parsed_sexpr = parse_sexpr(&s).unwrap();
    let roundtrip = sexpr_to_ast(&parsed_sexpr, &grammar, "x+y".to_string()).unwrap();

    // Verify ALL fields match
    assert_eq!(ast.roots.len(), roundtrip.roots.len());
    assert_eq!(ast.input(), roundtrip.input());

    // Check root
    let original_root = &ast.roots[0];
    let roundtrip_root = &roundtrip.roots[0];
    assert_eq!(original_root.name, roundtrip_root.name);
    assert_eq!(
        original_root.alternative_index,
        roundtrip_root.alternative_index
    );
    assert_eq!(original_root.binding, roundtrip_root.binding);
    assert_eq!(
        original_root.consumed_segments,
        roundtrip_root.consumed_segments
    );
    assert_eq!(original_root.children.len(), roundtrip_root.children.len());

    // Check nested expr
    if let (Node::NonTerminal(orig_expr), Node::NonTerminal(rt_expr)) =
        (&original_root.children[0], &roundtrip_root.children[0])
    {
        assert_eq!(orig_expr.name, rt_expr.name);
        assert_eq!(orig_expr.alternative_index, rt_expr.alternative_index);
        assert_eq!(orig_expr.binding, rt_expr.binding);
        assert_eq!(orig_expr.consumed_segments, rt_expr.consumed_segments);

        // Check term with binding and extension
        if let (Node::NonTerminal(orig_term), Node::NonTerminal(rt_term)) =
            (&orig_expr.children[0], &rt_expr.children[0])
        {
            assert_eq!(orig_term.consumed_segments, rt_term.consumed_segments);
            assert_eq!(orig_term.binding, rt_term.binding);

            if let (Node::Terminal(orig_t), Node::Terminal(rt_t)) =
                (&orig_term.children[0], &rt_term.children[0])
            {
                assert_eq!(orig_t, rt_t);
            }
        }

        // Check partial term
        if let (Node::NonTerminal(orig_pt), Node::NonTerminal(rt_pt)) =
            (&orig_expr.children[2], &rt_expr.children[2])
        {
            assert_eq!(orig_pt.consumed_segments, rt_pt.consumed_segments);
            assert_eq!(orig_pt.alternative_index, rt_pt.alternative_index);

            if let (Node::Terminal(orig_t), Node::Terminal(rt_t)) =
                (&orig_pt.children[0], &rt_pt.children[0])
            {
                assert_eq!(orig_t, rt_t);
            }
        }
    }

    println!("✓ All fields preserved through serialization!");
}

#[test]
fn test_partial_ast_serialize_method() {
    let grammar_str = r#"
    start ::= "hello" "world"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();
    let production = grammar.productions.get("start").unwrap()[0].clone();

    let root = NonTerminal::new(
        "start".to_string(),
        production,
        0,
        vec![
            Node::Terminal(Terminal::Complete {
                value: "hello".to_string(),
                binding: None,
                extension: None,
            }),
            Node::Terminal(Terminal::Partial {
                value: "wor".to_string(),
                binding: None,
                remainder: None,
            }),
        ],
        None,
        2,
    );

    let ast = PartialAST::new(vec![root], "hellowor".to_string());

    // Use the convenient serialize method
    let serialized = ast.serialize();
    println!("Serialized using .serialize():\n{}", serialized);

    // Deserialize using the convenient method
    let deserialized =
        PartialAST::deserialize(&serialized, &grammar, "hellowor".to_string()).unwrap();

    assert_eq!(ast.roots.len(), deserialized.roots.len());
    assert_eq!(ast.input(), deserialized.input());
}

#[test]
fn test_nonterminal_serialize_method() {
    let grammar_str = r#"
    expr ::= "x" "+" "y"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();
    let production = grammar.productions.get("expr").unwrap()[0].clone();

    let nt = NonTerminal::new(
        "expr".to_string(),
        production,
        0,
        vec![
            Node::Terminal(Terminal::Complete {
                value: "x".to_string(),
                binding: Some("left".to_string()),
                extension: None,
            }),
            Node::Terminal(Terminal::Complete {
                value: "+".to_string(),
                binding: None,
                extension: None,
            }),
            Node::Terminal(Terminal::Complete {
                value: "y".to_string(),
                binding: Some("right".to_string()),
                extension: None,
            }),
        ],
        Some("expr_binding".to_string()),
        3,
    );

    // Use the convenient serialize method
    let serialized = nt.serialize();
    println!("NonTerminal serialized:\n{}", serialized);

    // Deserialize
    let deserialized = NonTerminal::deserialize(&serialized, &grammar).unwrap();

    assert_eq!(nt.name, deserialized.name);
    assert_eq!(nt.binding, deserialized.binding);
    assert_eq!(nt.consumed_segments, deserialized.consumed_segments);
    assert_eq!(nt.children.len(), deserialized.children.len());
}

#[test]
fn test_terminal_serialize_method() {
    // Complete terminal
    let complete = Terminal::Complete {
        value: "test".to_string(),
        binding: Some("var".to_string()),
        extension: Some(DerivativeRegex::literal("_ext")),
    };

    let serialized = complete.serialize();
    println!("Complete terminal:\n{}", serialized);

    let deserialized = Terminal::deserialize(&serialized).unwrap();
    assert_eq!(complete, deserialized);

    // Partial terminal
    let partial = Terminal::Partial {
        value: "tes".to_string(),
        binding: Some("partial_var".to_string()),
        remainder: Some(DerivativeRegex::literal("t")),
    };

    let serialized2 = partial.serialize();
    println!("Partial terminal:\n{}", serialized2);

    let deserialized2 = Terminal::deserialize(&serialized2).unwrap();
    assert_eq!(partial, deserialized2);
}

#[test]
fn test_node_serialize_method() {
    let grammar_str = r#"
    term ::= "value"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    // Terminal node
    let term_node = Node::Terminal(Terminal::Complete {
        value: "value".to_string(),
        binding: None,
        extension: None,
    });

    let serialized = term_node.serialize();
    let deserialized = Node::deserialize(&serialized, &grammar).unwrap();

    match (&term_node, &deserialized) {
        (Node::Terminal(t1), Node::Terminal(t2)) => assert_eq!(t1, t2),
        _ => panic!("Expected terminal nodes"),
    }

    // NonTerminal node
    let production = grammar.productions.get("term").unwrap()[0].clone();
    let nt_node = Node::NonTerminal(NonTerminal::new(
        "term".to_string(),
        production,
        0,
        vec![Node::Terminal(Terminal::Complete {
            value: "value".to_string(),
            binding: None,
            extension: None,
        })],
        None,
        1,
    ));

    let serialized2 = nt_node.serialize();
    let deserialized2 = Node::deserialize(&serialized2, &grammar).unwrap();

    match (&nt_node, &deserialized2) {
        (Node::NonTerminal(nt1), Node::NonTerminal(nt2)) => {
            assert_eq!(nt1.name, nt2.name);
            assert_eq!(nt1.consumed_segments, nt2.consumed_segments);
        }
        _ => panic!("Expected nonterminal nodes"),
    }
}

#[test]
fn test_serialize_with_comments() {
    let grammar_str = r#"
    start ::= "hello"
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    // Test that strip_headers works with comments
    let s_with_comments = r#"
; This is a comment
; Another comment

(start @0 #1
  (T "hello"))
    "#;

    let ast = PartialAST::deserialize(s_with_comments, &grammar, "hello".to_string()).unwrap();
    assert_eq!(ast.roots.len(), 1);
    assert_eq!(ast.roots[0].name, "start");
}

#[test]
fn test_new_symbol_format() {
    let grammar_str = r#"
    start ::= name | value
    name ::= /[a-z]+/
    value ::= /[0-9]+/
    "#;

    let grammar = Grammar::load(grammar_str).unwrap();

    // Test with all symbol features:
    // @ = alternative index
    // $ = binding
    // # = consumed segments
    // + = extension (for complete terminals)
    // ~ = remainder (for partial terminals)

    let s = r#"(start @1 $result #2
  (T "123" $num +[0-9]+)
  (T~ "45" $partial ~[0-9]+))"#;

    let parsed = parse_sexpr(s).unwrap();
    let ast = sexpr_to_ast(&parsed, &grammar, "12345".to_string()).unwrap();

    assert_eq!(ast.roots.len(), 1);
    let root = &ast.roots[0];
    assert_eq!(root.name, "start");
    assert_eq!(root.alternative_index, 1);
    assert_eq!(root.binding, Some("result".to_string()));
    assert_eq!(root.consumed_segments, 2);

    // Check first child (complete terminal)
    match &root.children[0] {
        Node::Terminal(Terminal::Complete {
            value,
            binding,
            extension,
        }) => {
            assert_eq!(value, "123");
            assert_eq!(binding, &Some("num".to_string()));
            assert!(extension.is_some());
        }
        _ => panic!("Expected complete terminal"),
    }

    // Check second child (partial terminal)
    match &root.children[1] {
        Node::Terminal(Terminal::Partial {
            value,
            binding,
            remainder,
        }) => {
            assert_eq!(value, "45");
            assert_eq!(binding, &Some("partial".to_string()));
            assert!(remainder.is_some());
        }
        _ => panic!("Expected partial terminal"),
    }
}
