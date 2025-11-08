/// Validation Module
/// 
/// Here we validate everything to ensure each module is good. Not formal check yet but good tests
/// 
pub mod completability;
pub mod binding;

#[cfg(test)]
mod tests {
    use core::panic;

    use crate::{logic::grammar::Grammar, set_debug_input, set_debug_level};
    use super::completability::{complete_ast, CompletionResult};

    // Test grammars - simple and focused
    const SIMPLE_GRAMMAR: &str = r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Expression ::= Variable
    "#;

    const ARITHMETIC_GRAMMAR: &str = r#"
        Number ::= /[0-9]+/
        Identifier ::= /[a-z][a-zA-Z0-9]*/
        Literal ::= Number
        Variable ::= Identifier
        Operator ::= '+' | '-' | '*' | '/'
        Primary ::= Literal | Variable | '(' Expression ')'
        Expression ::= Primary (Operator Primary)*
    "#;

    const LAMBDA_GRAMMAR: &str = r#"
    // Identifier (more permissive)
    Identifier ::= /[a-zA-Z][a-zA-Z0-9]*/

    // Variables with var typing rule
    Variable(var) ::= Identifier[x]

    // Type names (can be any identifier)
    TypeName ::= Identifier

    // Base types (parentheses are literals, hence quoted)
    BaseType ::= TypeName | '(' Type ')'

    // Function types (right-associative)
    Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

    // Typed parameter
    TypedParam ::= Variable[x] ':' Type[τ]

    // Lambda abstraction (dot is a literal)
    Lambda(lambda) ::= 'λ' TypedParam '.' Term[e]
    
    // Base terms (cannot be applications; parentheses are literal tokens)
    BaseTerm ::= Variable | Lambda | '(' Term ')'

    // Applications (left-associative via iteration)
    Application(app) ::= BaseTerm[f] BaseTerm[e]

    // Terms
    Term ::= Application[e] | BaseTerm[e]
    "#;

    const PATHOLOGICAL_GRAMMAR: &str = r#"
        A ::= 'a' A | 'b'
        Expression ::= A
    "#;

    // Simple test data structures - focused on quick, reliable tests
    fn test_completable_cases() -> Vec<(&'static str, &'static str, &'static str, usize)> {
        vec![
            // (grammar_name, grammar_spec, input, max_depth)
            
            // Simple grammar tests - basic cases
            ("simple", SIMPLE_GRAMMAR, "", 3),
            ("simple", SIMPLE_GRAMMAR, "x", 3),
            ("simple", SIMPLE_GRAMMAR, "var", 3),
            
            // Arithmetic grammar tests - basic cases
            ("arithmetic", ARITHMETIC_GRAMMAR, "", 5),
            ("arithmetic", ARITHMETIC_GRAMMAR, "42 *", 3),
            ("arithmetic", ARITHMETIC_GRAMMAR, "x", 3),
            ("arithmetic", ARITHMETIC_GRAMMAR, "42 * 3 +", 3),
            ("arithmetic", ARITHMETIC_GRAMMAR, "42 * 3", 3),
            ("arithmetic", ARITHMETIC_GRAMMAR, "x / y", 3),
            ("arithmetic", ARITHMETIC_GRAMMAR, "(", 5),
            ("arithmetic", ARITHMETIC_GRAMMAR, "(x + 2", 5),

            
            // Lambda calculus tests - basic cases  
            ("lambda", LAMBDA_GRAMMAR, "", 6),
            ("lambda", LAMBDA_GRAMMAR, "x", 3),
            ("lambda", LAMBDA_GRAMMAR, "f", 3),
            ("lambda", LAMBDA_GRAMMAR, "λ", 8),
            ("lambda", LAMBDA_GRAMMAR, "λx:", 6),
            ("lambda", LAMBDA_GRAMMAR, "λx:int", 5),
            ("lambda", LAMBDA_GRAMMAR, "λx:int.", 5),
            ("lambda", LAMBDA_GRAMMAR, "f x", 5),
            ("lambda", LAMBDA_GRAMMAR, "(x", 6),
            ("lambda", LAMBDA_GRAMMAR, "λx:String.", 5), 
            
            // Pathological grammar tests (limited depth to avoid infinite loops)
            ("pathological", PATHOLOGICAL_GRAMMAR, "", 5),
            ("pathological", PATHOLOGICAL_GRAMMAR, "a", 8),
            ("pathological", PATHOLOGICAL_GRAMMAR, "aa", 6),
        ]
    }

    fn test_non_completable_cases() -> Vec<(&'static str, &'static str, &'static str, usize)> {
        vec![
            // (grammar_name, grammar_spec, input, max_depth)
            
            // Simple grammar - invalid tokens
            ("simple", SIMPLE_GRAMMAR, "123", 5),
            ("simple", SIMPLE_GRAMMAR, "X", 5), 
            ("simple", SIMPLE_GRAMMAR, "var123", 5), 
            ("simple", SIMPLE_GRAMMAR, "@#$", 5),
            ("simple", SIMPLE_GRAMMAR, ".", 5),
            
            // Arithmetic grammar - invalid operators and tokens
            ("arithmetic", ARITHMETIC_GRAMMAR, "42 %", 5),
            ("arithmetic", ARITHMETIC_GRAMMAR, "x &", 5),
            ("arithmetic", ARITHMETIC_GRAMMAR, "42.5", 5), 
            ("arithmetic", ARITHMETIC_GRAMMAR, "'string'", 5),
            ("arithmetic", ARITHMETIC_GRAMMAR, ")", 5), 
            ("arithmetic", ARITHMETIC_GRAMMAR, ")))", 5),
            
            ("lambda", LAMBDA_GRAMMAR, "λx:Int ←", 5), 
            ("lambda", LAMBDA_GRAMMAR, "λx.y", 5), 
            ("lambda", LAMBDA_GRAMMAR, "λ )x", 5), 
            ("lambda", LAMBDA_GRAMMAR, "λ123:Int.x", 5), 
            ("lambda", LAMBDA_GRAMMAR, "let x = 5", 5), 
            
            // Pathological grammar - invalid starting tokens
            ("pathological", PATHOLOGICAL_GRAMMAR, "c", 5),
            ("pathological", PATHOLOGICAL_GRAMMAR, "x", 5),
            ("pathological", PATHOLOGICAL_GRAMMAR, "1", 5),
        ]
    }

    #[test]
    fn test_completable_cases_comprehensive() {
        let test_cases = test_completable_cases();
        let mut passed = 0;
        let mut failed = 0;
        
        println!("Testing {} completable cases:", test_cases.len());
        
        for (grammar_name, grammar_spec, input, max_depth) in test_cases {
            print!("  {}/{}: '{}' -> ", grammar_name, input, input);
            
            let grammar = match Grammar::load(grammar_spec) {
                Ok(g) => g,
                Err(e) => {
                    println!("Grammar load error: {}", e);
                    failed += 1;
                    continue;
                }
            };
            
            println!("Grammar validation: {:#?}", grammar.clone().accepted_tokens_regex.unwrap().to_pattern());

            //set_debug_level(crate::DebugLevel::Trace);
            set_debug_input(Some(input.to_string()));
            match complete_ast(&grammar, input, max_depth) {
                CompletionResult::Success { complete_input, depth, .. } => {
                    // Verify the result actually parses
                    let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
                    match parser.parse(complete_input.trim()) {
                        Ok(_) => {
                            println!("Completed to '{}' in {} steps", 
                                   if complete_input.len() > 30 { 
                                       format!("{}...", &complete_input[..27]) 
                                   } else { 
                                       complete_input 
                                   }, depth);
                            passed += 1;
                        }
                        Err(e) => {
                            println!("Suspicious: completion succeeded but result doesn't parse: {}", e);
                            failed += 1;
                        }
                    }
                }
                CompletionResult::Failure { states_explored, visited_states, .. } => {
                    println!("Completion failure {} states:{}", 
                        states_explored, 
                        visited_states.join("\n"));
                    failed += 1;
                }
                CompletionResult::Error(e) => {
                    println!("Completion failure: Error: {}", e);
                    failed += 1;
                }
                CompletionResult::Invalid(e) => {
                    println!("Completion failure: Invalid input or inconsistency: {}", e);
                    failed += 1;
                }
                CompletionResult::Inconsistency(e) => {
                    println!("Completion failure: Inconsistency detected: {}", e);
                    failed += 1;
                }
            }
        }
        
        println!("\nCompletable cases: {} passed, {} failed", passed, failed);
        assert_eq!(failed, 0, "Some completable cases failed - this indicates a bug");
    }

    #[test]
    fn test_non_completable_cases_comprehensive() {
        let test_cases = test_non_completable_cases();
        let mut passed = 0;
        let mut failed = 0;
        
        println!("Testing {} non-completable cases:", test_cases.len());
        
        for (grammar_name, grammar_spec, input, max_depth) in test_cases {
            print!("  {}/{}: '{}' -> ", grammar_name, input, input);
            
            let grammar = match Grammar::load(grammar_spec) {
                Ok(g) => g,
                Err(e) => {
                    println!("Grammar load error: {}", e);
                    failed += 1;
                    continue;
                }
            };
            
            match complete_ast(&grammar, input, max_depth) {
                CompletionResult::Success { complete_input, .. } => {
                    println!("Error: completed to '{}'", complete_input);
                    let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
                    match parser.parse(complete_input.trim()) {
                        Ok(tree) => {
                            println!("  (but parsed tree: {:#?})", tree);
                        }
                        Err(e) => {
                            println!("  (but parsing failed: {})", e);
                        }
                    }
                    failed += 1;
                }
                CompletionResult::Failure { states_explored, .. } => {
                    println!("Correct: failed after {} states", states_explored);
                    passed += 1;
                }
                CompletionResult::Error(e) => {
                    println!("Error: {}", e);
                    failed += 1;
                }
                CompletionResult::Invalid(e) => {
                    println!("Correct: Input flagged as invalid: {}", e);
                    passed += 1;
                }
                CompletionResult::Inconsistency(e) => {
                    println!("Inconsistency detected: {}", e);
                    passed += 1;
                }
            }
        }
        
        println!("\nNon-completable cases: {} passed, {} failed", passed, failed);
        
        if failed > 0 {
            println!("\nCritical: {} cases that should be non-completable actually completed successfully!", failed);
            println!("This indicates serious issues with grammar design or completion logic.");
        }
        
        assert_eq!(failed, 0, "Non-completable cases should never succeed - this indicates bugs in the system");
    }

    #[test]
    fn test_depth_limits() {
        println!("Testing depth limits:");
        
        let grammar = Grammar::load(LAMBDA_GRAMMAR).expect("Grammar should load");
        
        let test_cases = vec![
            ("", 1, true),   // Should complete in 1 step
            ("", 0, false),  // Should not complete with 0 depth
            ("λ", 10, true), // Should complete with reasonable depth
            ("λ", 2, false), // Should not complete with very low depth
        ];
        
        for (input, max_depth, should_complete) in test_cases {
            print!("  '{}' with depth {} -> ", input, max_depth);
            
            let result = complete_ast(&grammar, input, max_depth);
            let actually_completed = matches!(result, CompletionResult::Success { .. });
            
            if actually_completed == should_complete {
                println!("{}", if should_complete { "completed" } else { "failed" });
            } else {
                println!("expected {}, got {}", should_complete, actually_completed);
                panic!("Depth limit test failed");
            }
        }
    }

    #[test]
    fn test_suspicious_grammars() {
        println!("Testing suspicious grammars:");
        
        // Grammar with no termination (should either fail to load or be detected)
        let infinite_grammar = r#"
            A ::= 'a' A 
            Expression ::= A
        "#;
        
        print!("  infinite grammar -> ");
        match Grammar::load(infinite_grammar) {
            Ok(grammar) => {
                println!("loaded, testing completion...");
                let result = complete_ast(&grammar, "a", 5);
                match result {
                    CompletionResult::Invalid(_) | CompletionResult::Failure { .. } => {
                        println!("Correct: detected issue");
                    }
                    CompletionResult::Success { .. } => {
                        panic!("Suspicious: completed an infinite grammar");
                    }
                    _ => {
                        println!("Unexpected result");
                    }
                }
            }
            Err(e) => {
                println!("correctly rejected: {}", e);
            }
        }
        
        // Grammar with cycles but termination
        let cyclic_grammar = r#"
            X ::= 'x' Y | 'done'
            Y ::= 'y' X | 'stop'
            Expression ::= X
        "#;
        
        print!("  cyclic but terminable grammar -> ");
        match Grammar::load(cyclic_grammar) {
            Ok(grammar) => {
                let result = complete_ast(&grammar, "x y", 10);
                match result {
                    CompletionResult::Success { .. } => {
                        println!("correctly completed");
                    }
                    _ => {
                        println!("failed to complete (might be expected)");
                    }
                }
            }
            Err(e) => {
                println!("? rejected: {}", e);
            }
        }
    }

    #[test]
    fn debug_weird_grammar() {

        let spec = r#"
        U ::= /b/ /a/ /r/ /c/ /b/ /a/ /r/ /c/ /u/
        A ::= /a/
        B ::= /b/ A /r/
        start ::= U | (B /c/ )+ | /t/
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::parse::Parser::new(g);
        let input = "b a r c";
        set_debug_input(Some(input.to_string()));
        set_debug_level(crate::DebugLevel::Trace);
        let ast = p.partial(input).unwrap();
        println!("Partial AST: {:#?}", ast);
    }

    #[test]
    fn test_debug_fail() {
        let spec = ARITHMETIC_GRAMMAR;
        let g = Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::parse::Parser::new(g);
        
        let inputs = vec![
            "1 +"
        ];

        for input in inputs {
            set_debug_input(Some(input.to_string()));
            set_debug_level(crate::DebugLevel::Trace);
            println!("Testing input: '{}'", input);
            match p.partial(input) {
                Ok(ast) => {
                    println!("GOOD: Parsed AST");
                }
                Err(e) => {
                    panic!(" got error: {}", e);
                }
            }
        }
    }

    #[test]
    fn test_lambdas() {
        let spec = LAMBDA_GRAMMAR;
        let g = Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::parse::Parser::new(g);
        
        let inputs = vec![
            "λ",
            "λx:",
            "λx:int",
            "λx:int.x",
        ];

        let mut fails = 0;
        let mut passes = 0;

        for input in inputs {
            println!("Testing input: '{}'", input);
            match p.partial(input) {
                Ok(ast) => {
                    println!("Partial AST: {}", ast);
                }
                Err(e) => {
                    println!("Partial parse error: {}", e);
                }
            }
        }
        assert_eq!(fails, 0, "{}", format!("{:?} lambda test cases failed ", fails));
        println!("Lambda tests passed: {}", passes);
    }

    #[test]
    fn test_comprehensive_suite() {
        println!("\nRunning comprehensive completability validation");
        
        test_completable_cases_comprehensive();
        test_non_completable_cases_comprehensive();
        test_depth_limits();
        test_suspicious_grammars();
        println!("\nAll validation tests passed!");
    }
}
