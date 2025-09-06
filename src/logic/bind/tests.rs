#[cfg(test)]
mod tests {
    use crate::logic::ast::{ASTNode, NonTerminal, SourceSpan, Terminal};
    use crate::logic::typing::{Type, TypingJudgment, Premise};
    use crate::logic::bind::{extract_terminal_value, get_nt_binding, get_type_binding, get_var_binding, BindingResolver, BoundConclusion, BoundTypingJudgment, BoundTypingRule, DefaultBindingResolver};

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
        let result = get_type_binding(&test_node, type_var);
        
        assert!(result.is_some());
        if let Some(Type::Atom(resolved)) = result {
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
        let result = get_type_binding(&test_node, type_var);
        
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
            judgment: TypingJudgment::Ascription(("var".to_string(), Type::Atom("ty".to_string()))),
        };
        let resolver = DefaultBindingResolver;
        
        let result = resolver.resolve_premise(&premise, &test_node);
        
        assert!(result.is_ok());
        let bound_premise = result.unwrap();
        
        match bound_premise.judgment {
            BoundTypingJudgment::Ascription(ascr) => {
                assert_eq!(ascr.node.value, "Variable");
                assert_eq!(ascr.ty, Type::Atom("Int".to_string()));
            }
            _ => panic!("Expected ascription judgment"),
        }
    }

    #[test]
    fn test_bound_typing_rule_display() {
        let bound_rule = BoundTypingRule {
            name: "display_test".to_string(),
            premises: vec![],
            conclusion: BoundConclusion::Type(Type::Atom("Result".to_string())),
        };
        
        let display_string = format!("{}", bound_rule);
        assert!(display_string.contains("BOUND:display_test"));
        assert!(display_string.contains("0 premises"));
    }

    #[test]
    fn test_bound_typing_rule_is_well_formed_empty() {
        let bound_rule = BoundTypingRule {
            name: "empty_rule".to_string(),
            premises: vec![],
            conclusion: BoundConclusion::Type(Type::Atom("Result".to_string())),
        };
        
        assert!(bound_rule.is_well_formed());
    }
}
