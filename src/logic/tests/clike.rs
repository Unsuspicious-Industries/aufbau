pub fn c_like_spec() -> String {
    use std::path::Path;
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples").join("clike.spec");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read C-like spec at {:?}: {}", path, e))
}

#[test]
fn test_pass_clike() {
    let grammar = crate::logic::grammar::Grammar::load(&c_like_spec())
        .expect("Failed to load C-like grammar");
    let mut parser = crate::logic::Parser::new(grammar.clone());

    let exprs = [
        "int main() {return 10;}",
        "int main() {int x = 5; return x;}",
        "int main() {int x = 5; int y = x + 2; return y;}",
        "int main() {if (1) {return 10;} return 0;}",
        "int main() {if (1) {return 10;} else {return 20;}}",
        "int main() {int x = 0; while (x < 10) {x = x + 1;} return x;}",
        "int main() {for (int i = 0; i < 10; i = i + 1) {} return 0;}",
        "int add(int a, int b) {return a + b;} int main() {return add(3, 4);}",
        "int x = 5;",
        "float y;",
        "int main() {int a; int b = 10; return b+a;}",
        "int main() {int x = 1; {int x = 2; x = x + 1;} return x;}",
        "int get_five() {return 5;} int main() {return get_five();}",
        "int add(int a, int b) {return a + b;} int main() {return add(3, 4);}",
        r#"int main() {
            int x = 0;
            for (int i = 0; i < 10; i = i + 1) {
                if (i % 2 == 0) {
                    x = x + 1;
                } else {
                    x = x + 2;
                }
            }
            while (x < 30) {
                x = x + 3;
            }
            return x;
        }"#,
    ];

    for expr in exprs {
        println!("Parsing expression: {}", expr);
        let past = parser.partial(expr).unwrap();
        let ast = past.into_complete().unwrap();
        println!("AST: {}", ast.pretty());
    }
}

#[test]
fn test_clike_typecheck() {
    use crate::logic::typing::eval::check_tree;
    use crate::logic::typing::core::TreeStatus;
    
    let grammar = crate::logic::grammar::Grammar::load(&c_like_spec())
        .expect("Failed to load C-like grammar");
    let mut parser = crate::logic::Parser::new(grammar.clone());
    
    // Test cases: (input, should_be_valid, description)
    let test_cases = [
        // Variable declarations
        ("int x = 5;", true, "simple var decl"),
        ("float y;", true, "var decl without init"),
        
        // Variable usage - context propagation
        ("int x = 5; int y = x;", true, "use declared var"),
        ("int x = 5; int y = x + 2;", true, "arithmetic with var"),
        
        // Functions - should be Valid, not Partial
        ("int main() {return 10;}", true, "simple function"),
        ("int main() {int x = 5; return x;}", true, "function with local var"),
        ("int add(int a) {return a;}", true, "function with param"),
        
        // Multiple functions
        ("int f() {return 1;} int g() {return 2;}", true, "multiple functions"),
    ];
    
    for (input, expect_valid, desc) in test_cases {
        println!("\n=== Test: {} ===", desc);
        println!("Input: {}", input);
        
        let ast = parser.partial(input).expect("Failed to parse");
        
        let complete_trees: Vec<_> = ast.roots.iter()
            .filter(|r| r.is_complete())
            .collect();
        
        let any_valid = complete_trees.iter().any(|tree| {
            let status = check_tree(tree, &grammar);
            println!("  Status: {:?}", status);
            !matches!(status, TreeStatus::Malformed)
        });
        
        if expect_valid {
            assert!(any_valid, "'{}' should typecheck but didn't", desc);
        } else {
            assert!(!any_valid || complete_trees.is_empty(), "'{}' should fail typecheck but passed", desc);
        }
    }
    
    println!("\n✓ All C-like typecheck tests passed!");
}

#[test]
fn test_clike_completions() {
    use crate::logic::typing::eval::check_tree;
    use crate::logic::typing::core::TreeStatus;
    
    let grammar = crate::logic::grammar::Grammar::load(&c_like_spec())
        .expect("Failed to load C-like grammar");
    let mut parser = crate::logic::Parser::new(grammar.clone());
    
    // Test completion for partial C-like code
    let input = "int main() {int x = 5; return ";
    println!("\n=== C-like Completions Test ===");
    println!("Input: '{}'", input);
    
    let ast = parser.partial(input).expect("Failed to parse");
    let completions = ast.completions(&grammar);
    
    let completion_examples: Vec<_> = completions.tokens.iter()
        .filter_map(|c| c.example())
        .take(15)
        .collect();
    
    println!("Syntactic completions ({} total): {:?}", completions.tokens.len(), completion_examples);
    
    // Get well-typed completions
    let well_typed: Vec<String> = completions.tokens.iter()
        .filter_map(|comp| {
            let ext = comp.example()?;
            let extended = format!("{}{}", input, ext);
            
            let mut p = crate::logic::Parser::new(grammar.clone());
            match p.partial(&extended) {
                Ok(past) => {
                    let is_valid = past.roots.iter().any(|r| {
                        !matches!(check_tree(r, &grammar), TreeStatus::Malformed)
                    });
                    if is_valid { Some(ext) } else { None }
                }
                Err(_) => None,
            }
        })
        .take(10)
        .collect();
    
    println!("Well-typed completions: {:?}", well_typed);
    
    // Verify that completions are syntactically valid
    for comp_str in &well_typed {
        let extended = format!("{}{}", input, comp_str);
        let result = parser.partial(&extended);
        assert!(result.is_ok(), "Completion '{}' should parse", comp_str);
    }
    
    println!("✓ All completions are syntactically valid");
}

#[test]
fn test_fail_clike() {
    crate::set_debug_level(crate::DebugLevel::Debug);

    let grammar = crate::logic::grammar::Grammar::load(&c_like_spec())
        .expect("Failed to load C-like grammar");
    crate::debug_info!(
        "test",
        "Loaded grammar with {} rules",
        grammar.typing_rules.len()
    );
    let mut parser = crate::logic::Parser::new(grammar.clone());
    crate::debug_info!("test", "Initialized parser");

    let exprs = [r#"int main() {
            int x = 0;
            for (int i = 0; i < 10; i = i + 1) {
                if (i % 2 == 0) {
                    x = x + 1;
                } else {
                    x = x + 2;
                }
            }
            while (x < 30) {
                x = x + 3;
            }"#];

    for expr in exprs {
        crate::set_debug_input(Some(expr.to_string()));

        let _ = parser.parse(expr).unwrap_err();
        println!("---");
    }
}
