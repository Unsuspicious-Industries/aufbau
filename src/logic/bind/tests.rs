#[cfg(test)]
mod tests {
    use std::fmt::format;

    use crate::logic::ast::{ASTNode, NonTerminal, SourceSpan, Terminal};
    use crate::logic::typing::{Type, TypingJudgment, Premise};
    use crate::logic::bind::{extract_terminal_value, get_nt_binding, bind_type, get_var_binding, BindingResolver, BoundConclusion, BoundConclusionKind, BoundConclusionContext, BoundTypingJudgment, BoundTypingRule, DefaultBindingResolver};
    use crate::{debug_info, set_debug_input, set_debug_level};
    use crate::logic::bind::typing::BoundType;

    /// Helper function to create a test NonTerminal node
    fn create_test_nonterminal(value: &str, binding: Option<String>) -> NonTerminal {
        NonTerminal {
            value: value.to_string(),
            children: vec![],
            span: Some(SourceSpan { start: 0, end: 0 }),
            binding,
            bound_typing_rule: None,
        }
    }

    /// Helper function to create a terminal node
    fn create_test_terminal(value: &str, binding: Option<String>) -> ASTNode {
        ASTNode::Terminal(Terminal {
            value: value.to_string(),
            span: Some(SourceSpan { start: 0, end: 0 }),
            binding,
        })
    }

    /// Create a test AST with nested structure and bindings
    fn create_test_ast() -> NonTerminal {
        let var_terminal = create_test_terminal("x", Some("var".to_string()));
        let type_terminal = create_test_terminal("Int", Some("ty".to_string()));
        
        let var_node = NonTerminal {
            value: "Variable".to_string(),
            children: vec![var_terminal],
            span: Some(SourceSpan { start: 0, end: 0 }),
            binding: Some("var".to_string()),
            bound_typing_rule: None,
        };
        
        let type_node = NonTerminal {
            value: "Type".to_string(),
            children: vec![type_terminal],
            span: Some(SourceSpan { start: 0, end: 0 }),
            binding: Some("ty".to_string()),
            bound_typing_rule: None,
        };

        NonTerminal {
            value: "Expression".to_string(),
            children: vec![var_node.as_node(), type_node.as_node()],
            span: Some(SourceSpan { start: 0, end: 0 }),
            binding: None,
            bound_typing_rule: None,
        }
    }

    #[test]
    fn test_get_type_binding_atom() {
        let test_node = create_test_ast();
        
        // Test resolving a type variable that has a binding
        let type_var = Type::Atom("ty".to_string());
        let result = bind_type(&test_node, type_var);
        
        assert!(result.is_some());
        if let Some(BoundType::Atom(resolved)) = result {
            assert_eq!(resolved, "Int");
        } else {
            panic!("Expected resolved atom type");
        }
    }

    #[test]
    fn test_get_type_binding_unbound_variable() {
        let test_node = create_test_ast();
        
        // Test resolving a type variable that has no binding
        let type_var = Type::Atom("unbound".to_string());
        let result = bind_type(&test_node, type_var);
        
        assert!(result.is_none());
    }

    #[test]
    fn test_get_nt_binding_success() {
        let test_node = create_test_ast();
        
        // Test finding a binding that exists
        let result = get_nt_binding(&test_node, "var".to_string());
        
        assert!(result.is_some());
        let bound_node = result.unwrap();
        assert_eq!(bound_node.value, "Variable");
    }

    #[test]
    fn test_get_nt_binding_not_found() {
        let test_node = create_test_ast();
        
        // Test finding a binding that doesn't exist
        let result = get_nt_binding(&test_node, "nonexistent".to_string());
        
        assert!(result.is_none());
    }

    #[test]
    fn test_get_var_binding_success() {
        let test_node = create_test_ast();
        
        // Test extracting variable binding value
        let result = get_var_binding(&test_node, "var".to_string());
        
        assert!(result.is_ok());
        let binding_value = result.unwrap();
        assert!(binding_value.is_some());
        assert_eq!(binding_value.unwrap(), "x");
    }

    #[test]
    fn test_extract_terminal_value_direct() {
        let terminal = create_test_terminal("test_value", None);
        let result = extract_terminal_value(&terminal);
        
        assert!(result.is_some());
        assert_eq!(result.unwrap(), "test_value");
    }

    #[test]
    fn test_extract_terminal_value_nested() {
        let terminal = create_test_terminal("nested_value", None);
        let wrapper = NonTerminal {
            value: "Wrapper".to_string(),
            children: vec![terminal],
            span: Some(SourceSpan { start: 0, end: 0 }),
            binding: None,
            bound_typing_rule: None,
        };
        
        let result = extract_terminal_value(&wrapper.as_node());
        
        assert!(result.is_some());
        assert_eq!(result.unwrap(), "nested_value");
    }

    #[test]
    fn test_nonterminal_bind_self() {
        let test_node = create_test_nonterminal("TestNode", Some("self_binding".to_string()));
        
        // Test binding to itself
        let result = test_node.bind("self_binding".to_string());
        
        assert!(result.is_some());
        let bound = result.unwrap();
        assert_eq!(bound.value, "TestNode");
    }

    #[test]
    fn test_nonterminal_bind_child() {
        let child = create_test_nonterminal("Child", Some("child_binding".to_string()));
        let parent = NonTerminal {
            value: "Parent".to_string(),
            children: vec![child.as_node()],
            span: Some(SourceSpan { start: 0, end: 0 }),
            binding: None,
            bound_typing_rule: None,
        };
        
        // Test binding to child
        let result = parent.bind("child_binding".to_string());
        
        assert!(result.is_some());
        let bound = result.unwrap();
        assert_eq!(bound.value, "Child");
    }

    #[test]
    fn test_binding_resolver_resolve_premise_ascription() {
        let test_node = create_test_ast();
        let premise = Premise {
            setting: None,
            judgment: Some(TypingJudgment::Ascription(("var".to_string(), Type::Atom("ty".to_string())))),
        };
        let resolver = DefaultBindingResolver;
        
        let result = resolver.resolve_premise(&premise, &test_node);
        
        assert!(result.is_ok());
        let bound_premise = result.unwrap();
        
        match bound_premise.judgment {
            Some(BoundTypingJudgment::Ascription(ascr)) => {
                assert_eq!(ascr.node.value, "Variable");
                assert_eq!(ascr.ty, BoundType::Atom("Int".to_string()));
            }
            _ => panic!("Expected ascription judgment"),
        }
    }

    #[test]
    fn test_bound_typing_rule_display() {
        let bound_rule = BoundTypingRule {
            name: "display_test".to_string(),
            premises: vec![],
            conclusion: BoundConclusion { context: BoundConclusionContext::default(), kind: BoundConclusionKind::Type(BoundType::Atom("Result".to_string())) },
        };
        
        // Display should print only the logical content (no BOUND: prefix)
        let display_string = format!("{}", bound_rule);
        assert!(display_string.contains("display_test ⇒ Result"));

        // Debug provides a prefixed, verbose form with premise count for diagnostics
        let debug_string = format!("{:?}", bound_rule);
        assert!(debug_string.contains("BOUND:display_test"));
        assert!(debug_string.contains("0 premises"));
    }

    #[test]
    fn test_bound_typing_rule_is_well_formed_empty() {
        let bound_rule = BoundTypingRule {
            name: "empty_rule".to_string(),
            premises: vec![],
            conclusion: BoundConclusion { context: BoundConclusionContext::default(), kind: BoundConclusionKind::Type(BoundType::Atom("Result".to_string())) },
        };
        
        assert!(bound_rule.is_well_formed());
    }

    #[test]
    fn complete_test() {
        let expr = "(λy:a->a.y)((λx:a->a.x)z)";

        // Enable debug output for this test
        set_debug_level(crate::DebugLevel::Info);
        set_debug_input(Some(expr.to_string()));

        let mut parser = crate::logic::parser::Parser::new(
            crate::logic::grammar::Grammar::load(
                crate::logic::grammar::tests::STLC_SPEC)
                    .unwrap()
        );  

        let term = parser.parse(expr).unwrap();
        debug_info!("test", "AST: {}", term.pretty());

        // assert is has one child and that child is a NonTerminal
        assert_eq!(term.nonterminal_children().len(), 1);
        let nt = &term.nonterminal_children()[0];

        let r = if let Some(r) = nt.bound_typing_rule.as_ref() {
            r
        } else {
            panic!("Expected NonTerminal to have a bound typing rule");
        };
        
        let typing_rule_str = format!("{}",r.as_ref());
        println!("{}",typing_rule_str);

        assert_eq!(typing_rule_str,"Γ ⊢ (λy:a->a.y) : a → a, Γ ⊢ ((λx:a->a.x)z) : a ⇒ a")


    }

    #[test]
    fn test_bound_type_display() {
        // Test basic atom type
        let atom = BoundType::Atom("Int".to_string());
        assert_eq!(format!("{}", atom), "Int");

        // Test arrow type
        let arrow = BoundType::Arrow(
            Box::new(BoundType::Atom("Int".to_string())),
            Box::new(BoundType::Atom("Bool".to_string()))
        );
        assert_eq!(format!("{}", arrow), "Int → Bool");

        // Test tuple type
        let tuple = BoundType::Tuple(vec![
            BoundType::Atom("Int".to_string()),
            BoundType::Atom("String".to_string()),
            BoundType::Atom("Bool".to_string())
        ]);
        assert_eq!(format!("{}", tuple), "(Int, String, Bool)");

        // Test pointer type
        let pointer = BoundType::Pointer(Box::new(BoundType::Atom("Int".to_string())));
        assert_eq!(format!("{}", pointer), "*Int");

        // Test array type
        let array = BoundType::Array(Box::new(BoundType::Atom("Int".to_string())), 10);
        assert_eq!(format!("{}", array), "Int[10]");

        // Test negation type
        let negation = BoundType::Not(Box::new(BoundType::Atom("Int".to_string())));
        assert_eq!(format!("{}", negation), "¬Int");

        // Test intersection type
        let intersection = BoundType::Intersection(
            Box::new(BoundType::Atom("Readable".to_string())),
            Box::new(BoundType::Atom("Writable".to_string()))
        );
        assert_eq!(format!("{}", intersection), "Readable ∧ Writable");

        // Test union type
        let union = BoundType::Union(
            Box::new(BoundType::Atom("Int".to_string())),
            Box::new(BoundType::Atom("String".to_string()))
        );
        assert_eq!(format!("{}", union), "Int ∨ String");

        // Test universe type
        let universe = BoundType::Universe;
        assert_eq!(format!("{}", universe), "⊤");

        // Test empty type
        let empty = BoundType::Empty;
        assert_eq!(format!("{}", empty), "∅");

        // Test complex nested type
        let complex = BoundType::Arrow(
            Box::new(BoundType::Tuple(vec![
                BoundType::Atom("Int".to_string()),
                BoundType::Pointer(Box::new(BoundType::Atom("Char".to_string())))
            ])),
            Box::new(BoundType::Union(
                Box::new(BoundType::Atom("Result".to_string())),
                Box::new(BoundType::Empty)
            ))
        );
        assert_eq!(format!("{}", complex), "(Int, *Char) → Result ∨ ∅");
    }
}
