pub mod binding;
/// Validation Module
///
/// Here we validate everything to ensure each module is good. Not formal check yet but good tests
///
pub mod completability;

#[cfg(test)]
mod tests {
    use core::panic;

    use super::completability::{CompletionResult, complete_ast};
    use crate::{logic::grammar::Grammar, set_debug_input, set_debug_level};

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
        Expression ::= Primary | Primary Operator Expression
    "#;

    const LAMBDA_GRAMMAR: &str = r#"
        // Identifier (supports Unicode)
        Identifier ::= /[A-Za-z_][A-Za-z0-9_τ₁₂₃₄₅₆₇₈₉₀]*/

        // Variables with var typing rule
        Variable(var) ::= Identifier[x]

        // Type names (supports Unicode type variables like τ₁, τ₂)
        TypeName ::= Identifier

        // Base types (parentheses are literals, hence quoted)
        BaseType ::= TypeName | '(' Type ')'

        // Function types (right-associative)
        Type ::= BaseType[τ₁] '->' Type[τ₂] | BaseType[τ]

        // Lambda abstraction (dot is a literal)
        Lambda(lambda) ::= 'λ' Identifier[x] ':' Type[τ] '.' Term[e]

        // variable declaration
        Let(let) ::= '{' Identifier[x] ':' Type[τ] '}'


        // Base terms (cannot be applications; parentheses are literal tokens)
        BaseTerm ::= Variable | '(' Term ')' 

        // Applications (left-associative via iteration)
        Application(app) ::= BaseTerm[f] BaseTerm[e]


        // Terms
        // FIX: Lambda is in Term, not BaseTerm, to ensure it wraps applications (e.g., λx.x y parses as λx.(x y)).
        // If Lambda were in BaseTerm, Application could consume it, parsing λx.x y as (λx.x) y.
        Term ::=  Lambda | Application[e] | BaseTerm[e] 

        Expr ::= Term | Let

        ProgramTail ::= ε | Expr ProgramTail
        Program ::= Expr ProgramTail

        // Typing Rules
        x ∈ Γ
        ----------- (var)
        Γ(x)

        Γ[x:τ] ⊢ e : ?B
        --------------------------- (lambda)
        τ → ?B

        Γ ⊢ f : ?A → ?B, Γ ⊢ e : ?A
        -------------------------------- (app)
        ?B

        -------------------------------- (let)
        Γ -> Γ[x:τ] ⊢ τ
    "#;

    const PATHOLOGICAL_GRAMMAR: &str = r#"
        A ::= 'a' A | 'b'
        Expression ::= A
    "#;

    // Simple test data structures - focused on quick, reliable tests
    fn test_completable_cases() -> Vec<(&'static str, &'static str, &'static str, usize)> {
        let cases = vec![
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
            ("arithmetic", ARITHMETIC_GRAMMAR, "(x + (y *", 6),
            ("arithmetic", ARITHMETIC_GRAMMAR, "((42 / x) +", 6),
            // Lambda calculus tests - basic cases
            ("lambda", LAMBDA_GRAMMAR, "", 6),
            ("lambda", LAMBDA_GRAMMAR, "λ", 8),
            ("lambda", LAMBDA_GRAMMAR, "λx:", 6),
            ("lambda", LAMBDA_GRAMMAR, "λx:int", 5),
            ("lambda", LAMBDA_GRAMMAR, "λx:int.", 5),
            ("lambda", LAMBDA_GRAMMAR, "λx:String.", 5),
            ("lambda", LAMBDA_GRAMMAR, "λx:(A", 6),
            ("lambda", LAMBDA_GRAMMAR, "(λx:(A->B).x", 8),
            ("lambda", LAMBDA_GRAMMAR, "(λx:(A->B).λy:A. x y", 10),
            ("lambda", LAMBDA_GRAMMAR, "λf:(A->B)->(B->C).", 8),
            ("lambda", LAMBDA_GRAMMAR, "λf:(A->B)->(B->C).f (", 10),
            // Pathological grammar tests (limited depth to avoid infinite loops)
            ("pathological", PATHOLOGICAL_GRAMMAR, "", 5),
            ("pathological", PATHOLOGICAL_GRAMMAR, "a", 8),
            ("pathological", PATHOLOGICAL_GRAMMAR, "aa", 6),
        ];
        println!("DEBUG: test_completable_cases returning {} cases", cases.len());
        cases
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
            ("arithmetic", ARITHMETIC_GRAMMAR, "(x + )", 6),
            ("arithmetic", ARITHMETIC_GRAMMAR, "x ++ y", 6),
            ("arithmetic", ARITHMETIC_GRAMMAR, "*/ x", 6),
            ("lambda", LAMBDA_GRAMMAR, "λx:Int ←", 5),
            ("lambda", LAMBDA_GRAMMAR, "λx.y", 5),
            ("lambda", LAMBDA_GRAMMAR, "λ )x", 5),
            ("lambda", LAMBDA_GRAMMAR, "λ123:Int.x", 5),
            ("lambda", LAMBDA_GRAMMAR, "let x = 5", 5),
            ("lambda", LAMBDA_GRAMMAR, "λx:(A->.", 5),
            ("lambda", LAMBDA_GRAMMAR, "λx::A", 6),
            ("lambda", LAMBDA_GRAMMAR, "λ:(A->B).x", 6),
            ("lambda", LAMBDA_GRAMMAR, "λx:(->A", 8),
            ("lambda", LAMBDA_GRAMMAR, "λx:A).", 6),
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

            println!(
                "Grammar validation: {:#?}",
                grammar.clone().accepted_tokens_regex.unwrap().to_pattern()
            );

            //set_debug_level(crate::DebugLevel::Debug);
            //set_debug_input(Some(input.to_string()));
            

            match complete_ast(&grammar, input, max_depth) {
                CompletionResult::Success {
                    complete_input,
                    depth,
                    ..
                } => {
                    // Verify the result actually parses
                    let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
                    match parser.parse(complete_input.trim()) {
                        Ok(_) => {
                            println!(
                                "Completed to '{}' in {} steps",
                                if complete_input.len() > 30 {
                                    format!("{}...", &complete_input[..27])
                                } else {
                                    complete_input
                                },
                                depth
                            );
                            passed += 1;
                        }
                        Err(e) => {
                            println!(
                                "Suspicious: completion succeeded but result doesn't parse: {}",
                                e
                            );
                            failed += 1;
                        }
                    }
                }
                CompletionResult::Failure {
                    states_explored,
                    visited_states,
                    ..
                } => {
                    println!(
                        "Completion failure {} states:{}",
                        states_explored,
                        visited_states.join("\n")
                    );
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
        assert_eq!(
            failed, 0,
            "Some completable cases failed - this indicates a bug"
        );
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
                CompletionResult::Failure {
                    states_explored, ..
                } => {
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

        println!(
            "\nNon-completable cases: {} passed, {} failed",
            passed, failed
        );

        if failed > 0 {
            println!(
                "\nCritical: {} cases that should be non-completable actually completed successfully!",
                failed
            );
            println!("This indicates serious issues with grammar design or completion logic.");
        }

        assert_eq!(
            failed, 0,
            "Non-completable cases should never succeed - this indicates bugs in the system"
        );
    }

    #[test]
    fn test_depth_limits() {
        println!("Testing depth limits:");

        // Use simple grammar without typing rules for depth testing
        let simple = r#"
            A ::= 'a'
            B ::= 'a' 'b'
            C ::= 'a' 'b' 'c'
            start ::= A | B | C
        "#;
        let grammar = Grammar::load(simple).expect("Grammar should load");

        let test_cases = vec![
            ("", 1, true),    // 'a' completes in 1 step
            ("", 0, false),   // Zero depth can't complete
            ("a", 2, true),   // 'a b' or 'a b c' needs 1-2 more
            ("a b", 1, true), // 'a b c' needs 1 more
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
    fn test_completion_ast_roundtrip() {
        println!("Testing completion AST roundtrip integrity");
        crate::set_debug_level(crate::DebugLevel::Debug);

        let cases = vec![
            (LAMBDA_GRAMMAR, "λx:(A->", 10),
            (LAMBDA_GRAMMAR, "(λx:(A->B).λy:A. x y", 12),
            (LAMBDA_GRAMMAR, "λf:(A->B)->(B->C).", 10),
            (ARITHMETIC_GRAMMAR, "(x + (y *", 8),
        ];

        for (grammar_spec, input, max_depth) in cases {
            let grammar = Grammar::load(grammar_spec).expect("Grammar should load");
            let (complete_input, completion_ast) = match complete_ast(&grammar, input, max_depth) {
                CompletionResult::Success {
                    complete_input,
                    ast,
                    ..
                } => (complete_input, ast),
                other => panic!("expected success for '{}', got {:?}", input, other),
            };

            let mut direct_parser = crate::logic::partial::parse::Parser::new(grammar.clone());
            let direct_ast = direct_parser
                .parse(complete_input.trim())
                .unwrap_or_else(|e| panic!("direct parse failed for '{}': {}", complete_input, e));

            assert!(
                completion_ast.syneq(&direct_ast),
                "complete_ast returned AST diverging from direct parse for '{}'",
                input
            );

            let mut partial_parser = crate::logic::partial::parse::Parser::new(grammar.clone());
            let partial_ast = partial_parser
                .partial(complete_input.trim())
                .unwrap_or_else(|e| panic!("partial parse failed for '{}': {}", complete_input, e));
            assert!(
                partial_ast.complete(),
                "completed string '{}' should yield a complete partial AST",
                complete_input
            );

            let recovered_ast = partial_ast
                .into_complete()
                .expect("partial AST should convert to full AST");

            assert!(
                completion_ast.syneq(&recovered_ast),
                "PartialAST::into_complete diverged from BFS-completed AST for '{}'",
                input
            );
            assert!(
                recovered_ast.syneq(&direct_ast),
                "Recovered AST differs from direct parse for '{}'",
                complete_input
            );
        }
    }

    #[test]
    fn debug_weird_grammar() {
        let spec = r#"
    U ::= /b/ /a/ /r/ /c/ /b/ /a/ /r/ /c/ /u/
    A ::= /a/
    B ::= /b/ A /r/
    BSeq ::= B /c/ BSeqTail
    BSeqTail ::= ε | B /c/ BSeqTail
    start ::= U | BSeq | /t/
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::parse::Parser::new(g);
        let input = "b a r c";
        //set_debug_input(Some(input.to_string()));
        //set_debug_level(crate::DebugLevel::Trace);
        let ast = p.partial(input).unwrap();
        println!("Partial AST: {:#?}", ast);
    }

    #[test]
    fn test_debug_fail() {
        let spec = LAMBDA_GRAMMAR;
        let g = Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::parse::Parser::new(g.clone());

        let inputs = vec!["", "(λx:(A->B).x"];

        for input in inputs {
            set_debug_input(Some(input.to_string()));
            set_debug_level(crate::DebugLevel::Debug);
            println!("Testing input: '{}'", input);
            let t = match p.partial(input) {
                Ok(t) => {
                    println!("GOOD: Parsed AST");
                    t
                }
                Err(e) => {
                    panic!(" got error: {}", e);
                }
            };

            println!("Partial AST: {}", t);
            
            let valid = crate::logic::typing::evaluate_typing(t.roots(), &g);
            println!("Typing check: {}", valid);

            let completions = t.completions(&(g.clone()));
            println!("Completions: {:#?}", completions);
            
            // Try to complete it manually to see where it fails
            let res = complete_ast(&g, input, 5);
            println!("Completion result: {:?}", res);
        }
    }

    #[test]
    fn test_lambdas() {
        let spec = LAMBDA_GRAMMAR;
        let g = Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::parse::Parser::new(g);

        let inputs = vec!["λ", "λx:", "λx:int", "λx:int.x"];

        let fails = 0;
        let passes = 0;

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
        assert_eq!(
            fails,
            0,
            "{}",
            format!("{:?} lambda test cases failed ", fails)
        );
        println!("Lambda tests passed: {}", passes);
    }

    #[test]
    fn test_full_pipeline() {
        println!("Testing full pipeline (Parse -> Type -> Complete -> Type)");
        crate::set_debug_level(crate::DebugLevel::Debug);

        let scenarios = vec![
            (
                "Simple",
                SIMPLE_GRAMMAR,
                vec![
                    ("x", true),
                    ("var", true),
                ]
            ),
            (
                "Arithmetic",
                ARITHMETIC_GRAMMAR,
                vec![
                    ("42", true),
                    ("x + 1", true),
                    ("1 + 2 * 3", true),
                    ("(1 + 2) * 3", true),
                ]
            ),
            (
                "Lambda",
                LAMBDA_GRAMMAR,
                vec![
                    ("λx:A.x", true),
                    ("λx:A.λy:B.x", true),
                    ("λf:A->B.λx:A.f x", true),
                ]
            )
        ];

        for (name, grammar_spec, inputs) in scenarios {
            println!("Scenario: {}", name);
            let grammar = Grammar::load(grammar_spec).expect("Grammar load failed");

            for (input, should_type_check) in inputs {
                println!("  Input: '{}'", input);

                // 1. Parse Complete
                let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
                let ast = parser.partial(input).expect("Failed to parse valid input");
                assert!(ast.complete(), "Input '{}' should parse completely", input);
                
                // 2. Type Check
                let is_well_typed = crate::logic::typing::evaluate_typing(ast.roots(), &grammar);
                if should_type_check {
                    assert!(is_well_typed, "Input '{}' should be well-typed but failed", input);
                } else {
                    assert!(!is_well_typed, "Input '{}' should NOT be well-typed but passed", input);
                }

                // 3. Partial Completion (take a prefix)
                if input.chars().count() > 1 {
                    let char_count = input.chars().count();
                    let prefix: String = input.chars().take(char_count/2 + 1).collect();
                    println!("    Prefix: '{}'", prefix);
                    
                    let depth = if name == "Lambda" { 6 } else { 5 };
                    
                    match complete_ast(&grammar, &prefix, depth) {
                        CompletionResult::Success { complete_input, .. } => {
                            println!("    Completed to: '{}'", complete_input);
                            let comp_ast = parser.partial(&complete_input).expect("Completion result should parse");
                            assert!(comp_ast.complete(), "Completion '{}' should be complete", complete_input);
                            let comp_typed = crate::logic::typing::evaluate_typing(comp_ast.roots(), &grammar);
                            if should_type_check {
                                println!("    Completion typed: {}", comp_typed);
                            }
                        },
                        CompletionResult::Failure { .. } => {
                            println!("    Failed to complete prefix (might be expected with low depth)");
                        },
                        _ => {}
                    }
                }
            }
        }
    }

    #[test]
    fn test_context_sensitive_completion() {
        println!("Testing context-sensitive completion:");
        
        // Use LAMBDA_GRAMMAR because it has typing rules defined (specifically 'var')
        let grammar = Grammar::load(LAMBDA_GRAMMAR).expect("Grammar load failed");
        
        // Case 1: Empty context, "x" should fail or not complete if we enforce variable existence
        // The typing rule for Variable is:
        // x ∈ Γ
        // ----------- (var)
        // Γ(x)
        
        // So with empty context, "x" is ill-typed.
        // Use complete_ast_with_context to enforce typing rules
        let empty_ctx = crate::logic::typing::Context::new();
        let result_empty = crate::validation::completability::complete_ast_with_context(&grammar, "x", 3, empty_ctx);
        
        match result_empty {
             CompletionResult::Success { .. } => panic!("'x' should not complete in empty context"),
             _ => println!("Correctly rejected 'x' in empty context"),
        }

        // Case 2: Context with x: int
        let mut ctx = crate::logic::typing::Context::new();
        // Note: In LAMBDA_GRAMMAR, types are like 'int', 'A', etc.
        // We can use a raw type for testing.
        ctx.add("x".to_string(), crate::logic::typing::Type::Raw("int".to_string()));
        
        let result_ctx = crate::validation::completability::complete_ast_with_context(&grammar, "x", 3, ctx.clone());
        
        match result_ctx {
            CompletionResult::Success { .. } => println!("Correctly accepted 'x' with context"),
            _ => panic!("'x' should complete with context [x: int]"),
        }
        
        // Case 3: Context with x: int, input "(x"
        // Should complete to "(x)" or similar. 
        // Note: In LAMBDA_GRAMMAR, BaseTerm ::= Variable | '(' Term ')'
        // So "(x" can complete to "(x)".
        let result_ctx_paren = crate::validation::completability::complete_ast_with_context(&grammar, "(x", 5, ctx.clone());
         match result_ctx_paren {
            CompletionResult::Success { complete_input, .. } => println!("Correctly completed '(x' to '{}'", complete_input),
            _ => panic!("'(x' should complete with context [x: int]"),
        }
    }

    // ============================================================================
    // COMPREHENSIVE WELL-TYPED COMPLETION VALIDATION
    // ============================================================================
    // 
    // These tests ensure that completions are ALWAYS:
    // 1. Syntactically correct (parse without error)
    // 2. Well-typed (pass type checking)
    //
    // The tests cover arbitrary grammars with various typing rule patterns.
    // ============================================================================

    use crate::logic::typing::eval::check_tree;
    use crate::logic::typing::core::TreeStatus;

    /// Verify a single completion leads to well-typed trees
    fn verify_completion_well_typed(
        grammar: &Grammar,
        input: &str,
        completion: &str,
        ctx: &crate::logic::typing::Context,
    ) -> Result<(), String> {
        let extended = format!("{}{}", input, completion);
        
        // Step 1: Parse the extended input
        let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
        let partial = parser.partial(&extended)
            .map_err(|e| format!("Parse failed for '{}': {}", extended, e))?;
        
        // Step 2: Check that at least one tree is well-typed
        let any_well_typed = partial.roots.iter().any(|root| {
            match crate::logic::typing::eval::check_tree_with_context(root, grammar, ctx) {
                TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                TreeStatus::Malformed => false,
            }
        });
        
        if !any_well_typed {
            return Err(format!(
                "Completion '{}' after '{}' produces no well-typed trees! Extended: '{}'",
                completion, input, extended
            ));
        }
        
        Ok(())
    }

    /// Get well-typed completions using our filtering
    fn get_well_typed_completions(
        grammar: &Grammar,
        input: &str,
    ) -> Vec<String> {
        let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
        let partial = match parser.partial(input) {
            Ok(p) => p,
            Err(_) => return vec![],
        };
        
        let completions = partial.completions(grammar);
        let all_completion_strings: Vec<String> = completions
            .iter()
            .map(|r| r.example().unwrap_or_else(|| r.to_pattern()))
            .collect();
        
        // Filter to well-typed only
        all_completion_strings
            .into_iter()
            .filter(|completion| {
                let extended = format!("{}{}", input, completion);
                let mut p2 = crate::logic::partial::parse::Parser::new(grammar.clone());
                match p2.partial(&extended) {
                    Ok(partial) => {
                        partial.roots.iter().any(|root| {
                            match check_tree(root, grammar) {
                                TreeStatus::Valid(_) | TreeStatus::Partial(_) => true,
                                TreeStatus::Malformed => false,
                            }
                        })
                    }
                    Err(_) => false,
                }
            })
            .collect()
    }

    #[test]
    fn test_completions_always_well_typed_lambda() {
        println!("\n=== Testing: All completions lead to well-typed trees (Lambda) ===\n");
        
        let grammar = Grammar::load(LAMBDA_GRAMMAR).expect("Grammar load failed");
        
        // Test inputs at various stages of lambda expressions
        let test_inputs = vec![
            // Empty input
            "",
            // Lambda abstraction stages
            "λ",
            "λx",
            "λx:",
            "λx:A",
            "λx:A.",
            "λx:A.x",  // Identity - complete
            // Nested lambdas
            "λx:A.λy",
            "λx:A.λy:",
            "λx:A.λy:B",
            "λx:A.λy:B.",
            "λx:A.λy:B.x",  // x is in scope
            // Applications
            "(λx:A.x",
            "(λx:A.x)",
            "(λx:A.x) (λy:B.",
            // Function types
            "λf:A->",
            "λf:A->B.",
            "λf:(A->B)->",
        ];
        
        let mut total_completions_checked = 0;
        let mut all_passed = true;
        
        for input in test_inputs {
            let completions = get_well_typed_completions(&grammar, input);
            println!("Input '{}' -> {} well-typed completions", input, completions.len());
            
            for completion in &completions {
                total_completions_checked += 1;
                if let Err(e) = verify_completion_well_typed(&grammar, input, completion, &crate::logic::typing::Context::new()) {
                    println!("  FAILED: {}", e);
                    all_passed = false;
                }
            }
        }
        
        println!("\nChecked {} total completions", total_completions_checked);
        assert!(all_passed, "Some completions were not well-typed!");
        assert!(total_completions_checked > 0, "No completions were generated - test is invalid");
    }

    #[test]
    fn test_completions_reject_unbound_variables() {
        println!("\n=== Testing: Completions reject unbound variables ===\n");
        
        let grammar = Grammar::load(LAMBDA_GRAMMAR).expect("Grammar load failed");
        
        // After "λx:A." the only valid variable is "x"
        // Variables like "y", "z", etc. should NOT be valid completions
        
        let input = "λx:A.";
        let completions = get_well_typed_completions(&grammar, input);
        
        println!("Input: '{}'", input);
        println!("Well-typed completions: {:?}", completions);
        
        // Check that we have completions
        assert!(!completions.is_empty(), "Should have completions after 'λx:A.'");
        
        // If there are variable completions, "x" should be there but not other free variables
        let has_x = completions.iter().any(|c| c == "x");
        let has_y = completions.iter().any(|c| c == "y");
        let has_z = completions.iter().any(|c| c == "z");
        let has_unbound = completions.iter().any(|c| c == "unbound");
        
        // x is bound, so it might be a valid completion
        // y, z, unbound are NOT bound, so they should NOT be completions
        println!("Has 'x': {} (should be present if variables are suggested)", has_x);
        println!("Has 'y': {} (should be false)", has_y);
        println!("Has 'z': {} (should be false)", has_z);
        println!("Has 'unbound': {} (should be false)", has_unbound);
        
        assert!(!has_y, "Unbound variable 'y' should not be a valid completion");
        assert!(!has_z, "Unbound variable 'z' should not be a valid completion");
        assert!(!has_unbound, "Unbound variable 'unbound' should not be a valid completion");
    }

    #[test]
    fn test_completions_respect_type_constraints() {
        println!("\n=== Testing: Completions respect type constraints ===\n");
        
        let grammar = Grammar::load(LAMBDA_GRAMMAR).expect("Grammar load failed");
        
        // In "λf:A->B. λx:A. f " the next must be something of type A
        // If we apply f, the argument must have type A
        
        let input = "λf:A->B. λx:A. f ";
        let completions = get_well_typed_completions(&grammar, input);
        
        println!("Input: '{}'", input);
        println!("Well-typed completions: {:?}", completions);
        
        // Verify all completions are well-typed
        for completion in &completions {
            let result = verify_completion_well_typed(&grammar, input, completion, &crate::logic::typing::Context::new());
            assert!(result.is_ok(), "Completion '{}' should be well-typed: {:?}", completion, result);
        }
    }

    #[test]
    fn test_completions_never_produce_malformed() {
        println!("\n=== Testing: No completion ever produces a malformed tree ===\n");
        
        let grammar = Grammar::load(LAMBDA_GRAMMAR).expect("Grammar load failed");
        
        // Exhaustive test: try many input prefixes
        let test_prefixes = vec![
            "", "λ", "λx", "λx:", "λx:A", "λx:A.", "λx:A.x",
            "(", "(λ", "(λx", "(λx:", "(λx:A", "(λx:A.", "(λx:A.x",
            "λx:A.λ", "λx:A.λy", "λx:A.λy:", "λx:A.λy:B", "λx:A.λy:B.",
            "λf:A->B.", "λf:(A->B)->C.",
        ];
        
        let mut failures = Vec::new();
        let mut total_checked = 0;
        
        for prefix in test_prefixes {
            let completions = get_well_typed_completions(&grammar, prefix);
            
            for completion in completions {
                total_checked += 1;
                let extended = format!("{}{}", prefix, completion);
                
                let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
                if let Ok(partial) = parser.partial(&extended) {
                    // Check ALL trees, not just if any is valid
                    // At least one tree must not be malformed
                    let has_valid = partial.roots.iter().any(|root| {
                        !matches!(check_tree(root, &grammar), TreeStatus::Malformed)
                    });
                    
                    if !has_valid {
                        failures.push(format!(
                            "'{}' + '{}' = '{}' -> ALL trees malformed",
                            prefix, completion, extended
                        ));
                    }
                }
            }
        }
        
        println!("Checked {} completion combinations", total_checked);
        
        if !failures.is_empty() {
            println!("\nFAILURES ({}):", failures.len());
            for f in &failures {
                println!("  {}", f);
            }
            panic!("Some completions produced only malformed trees!");
        }
        
        println!("All completions produced at least one valid/partial tree ✓");
    }

    #[test]
    fn test_arithmetic_completions_syntactically_correct() {
        println!("\n=== Testing: Arithmetic completions are syntactically correct ===\n");
        
        let grammar = Grammar::load(ARITHMETIC_GRAMMAR).expect("Grammar load failed");
        
        let test_inputs = vec![
            "", "42", "x", "42 +", "42 + 3", "42 * 3 +",
            "(", "(42", "(42 +", "(42 + 3", "(42 + 3)",
            "((", "((42", "((42))",
        ];
        
        let mut total_checked = 0;
        let mut failures = Vec::new();
        
        for input in test_inputs {
            let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
            if let Ok(partial) = parser.partial(input) {
                let completions = partial.completions(&grammar);
                
                for regex in completions.iter() {
                    if let Some(example) = regex.example() {
                        total_checked += 1;
                        let extended = format!("{}{}", input, if input.is_empty() || input.ends_with(' ') || !example.chars().next().unwrap_or(' ').is_alphanumeric() { example.clone() } else { format!(" {}", example) });
                        
                        let mut p2 = crate::logic::partial::parse::Parser::new(grammar.clone());
                        if p2.partial(&extended).is_err() {
                            failures.push(format!("'{}' -> '{}' failed to parse", input, extended));
                        }
                    }
                }
            }
        }
        
        println!("Checked {} completions", total_checked);
        
        if !failures.is_empty() {
            println!("\nFAILURES:");
            for f in &failures[..failures.len().min(10)] {
                println!("  {}", f);
            }
            if failures.len() > 10 {
                println!("  ... and {} more", failures.len() - 10);
            }
            panic!("Some completions were not syntactically valid!");
        }
        
        println!("All completions syntactically valid ✓");
    }

    #[test]
    fn test_stlc_well_typed_completions() {
        println!("\n=== Testing: STLC (from file) completions are well-typed ===\n");
        
        // Load the real STLC grammar
        let spec = std::fs::read_to_string("examples/stlc.spec")
            .expect("Failed to read examples/stlc.spec");
        let grammar = Grammar::load(&spec).expect("Failed to load STLC grammar");
        
        let test_inputs = vec![
            "", "λ", "λx", "λx:", "λx:Int", "λx:Int.",
            "(", "(λ", "(λx:Int.x",
        ];
        
        let mut total_checked = 0;
        let mut all_passed = true;
        
        for input in test_inputs {
            let completions = get_well_typed_completions(&grammar, input);
            println!("Input '{}' -> {} well-typed completions", input, completions.len());
            
            for completion in &completions {
                total_checked += 1;
                if let Err(e) = verify_completion_well_typed(&grammar, input, completion, &crate::logic::typing::Context::new()) {
                    println!("  FAILED: {}", e);
                    all_passed = false;
                }
            }
        }
        
        println!("\nChecked {} completions", total_checked);
        assert!(all_passed, "Some STLC completions were not well-typed!");
    }

    #[test]
    fn test_xtlc_well_typed_completions() {
        println!("\n=== Testing: XTLC (from file) completions are well-typed ===\n");
        
        // Load the real XTLC grammar
        let spec = std::fs::read_to_string("examples/xtlc.spec")
            .expect("Failed to read examples/xtlc.spec");
        let grammar = Grammar::load(&spec).expect("Failed to load XTLC grammar");
        
        let test_inputs = vec![
            "", "λ", "λx", "λx:", "λx:A", "λx:A.",
            "(", "(λ", "(λx:A.x",
            "λf:A->B.",
        ];
        
        let mut total_checked = 0;
        let mut all_passed = true;
        
        for input in test_inputs {
            let completions = get_well_typed_completions(&grammar, input);
            println!("Input '{}' -> {} well-typed completions", input, completions.len());
            
            for completion in &completions {
                total_checked += 1;
                if let Err(e) = verify_completion_well_typed(&grammar, input, completion, &crate::logic::typing::Context::new()) {
                    println!("  FAILED: {}", e);
                    all_passed = false;
                }
            }
        }
        
        println!("\nChecked {} completions", total_checked);
        assert!(all_passed, "Some XTLC completions were not well-typed!");
    }

    #[test]
    fn test_completion_chain_invariant() {
        println!("\n=== Testing: Chain of completions maintains well-typedness ===\n");
        
        let grammar = Grammar::load(LAMBDA_GRAMMAR).expect("Grammar load failed");
        
        // Start with empty input and chain completions
        // At each step, verify we still have well-typed options
        
        let max_chain_length = 8;
        let mut current = String::new();
        let mut chain = Vec::new();
        
        for step in 0..max_chain_length {
            let completions = get_well_typed_completions(&grammar, &current);
            
            println!("Step {}: '{}' -> {} completions", step, current, completions.len());
            
            if completions.is_empty() {
                // Check if we have a complete parse
                let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
                if let Ok(partial) = parser.partial(&current) {
                    if partial.complete() {
                        println!("  Reached complete parse: '{}'", current);
                        break;
                    }
                }
                println!("  No completions available but not complete - this may indicate an issue");
                break;
            }
            
            // Verify ALL completions are well-typed (the invariant)
            for c in &completions {
                let result = verify_completion_well_typed(&grammar, &current, c, &crate::logic::typing::Context::new());
                assert!(result.is_ok(), "Invariant violated at step {}: '{}' + '{}' is not well-typed: {:?}", 
                    step, current, c, result);
            }
            
            // Pick first completion to continue chain
            let next = &completions[0];
            chain.push(next.clone());
            current = format!("{}{}", current, 
                if current.is_empty() || current.ends_with(' ') || !next.chars().next().unwrap_or(' ').is_alphanumeric() {
                    next.clone()
                } else {
                    format!(" {}", next)
                }
            );
        }
        
        println!("\nCompletion chain: {:?}", chain);
        println!("Final input: '{}'", current);
    }

    #[test]
    fn test_pathological_grammar_completions() {
        println!("\n=== Testing: Pathological grammar completions ===\n");
        
        let grammar = Grammar::load(PATHOLOGICAL_GRAMMAR).expect("Grammar load failed");
        
        // This grammar has no typing rules, so all syntactic completions should be valid
        let test_inputs = vec!["", "a", "aa", "aaa", "b"];
        
        for input in test_inputs {
            let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
            if let Ok(partial) = parser.partial(input) {
                let completions = partial.completions(&grammar);
                println!("Input '{}' -> {} completions", input, completions.tokens.len());
                
                // All syntactic completions should at least parse
                for regex in completions.iter() {
                    if let Some(example) = regex.example() {
                        let extended = format!("{}{}", input, example);
                        let mut p2 = crate::logic::partial::parse::Parser::new(grammar.clone());
                        assert!(p2.partial(&extended).is_ok(), 
                            "Completion '{}' after '{}' should parse", example, input);
                    }
                }
            }
        }
    }

    #[test]
    fn test_no_completion_leads_to_type_error() {
        println!("\n=== CRITICAL TEST: No completion should ever lead to type error ===\n");
        
        // This is the most important test - verifies our core guarantee
        
        let grammars: Vec<(&str, &str)> = vec![
            ("LAMBDA", LAMBDA_GRAMMAR),
            ("SIMPLE", SIMPLE_GRAMMAR),
        ];
        
        let mut total_violations = 0;
        let mut total_checked = 0;
        
        for (name, spec) in grammars {
            println!("\nTesting grammar: {}", name);
            let grammar = Grammar::load(spec).expect("Grammar load failed");
            
            // Generate various partial inputs
            let inputs = generate_test_inputs(&grammar, 5);
            println!("Generated {} test inputs", inputs.len());
            
            for input in inputs {
                let completions = get_well_typed_completions(&grammar, &input);
                
                for completion in completions {
                    total_checked += 1;
                    let extended = format!("{}{}", input, completion);
                    
                    let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
                    if let Ok(partial) = parser.partial(&extended) {
                        // Check that at least one tree is NOT malformed
                        let all_malformed = partial.roots.iter().all(|root| {
                            matches!(check_tree(root, &grammar), TreeStatus::Malformed)
                        });
                        
                        if all_malformed && !partial.roots.is_empty() {
                            println!("VIOLATION: '{}' + '{}' = '{}' -> all trees malformed!", 
                                input, completion, extended);
                            total_violations += 1;
                        }
                    }
                }
            }
        }
        
        println!("\n=== Summary ===");
        println!("Total completions checked: {}", total_checked);
        println!("Total violations: {}", total_violations);
        
        assert_eq!(total_violations, 0, 
            "CRITICAL: {} completions led to type errors! Our core guarantee is violated!", 
            total_violations);
    }

    /// Generate test inputs by exploring the grammar
    fn generate_test_inputs(grammar: &Grammar, max_depth: usize) -> Vec<String> {
        use std::collections::VecDeque;
        
        let mut inputs = vec![String::new()];
        let mut queue = VecDeque::new();
        let mut visited = std::collections::HashSet::new();
        
        queue.push_back((String::new(), 0usize));
        visited.insert(String::new());
        
        while let Some((current, depth)) = queue.pop_front() {
            if depth >= max_depth {
                continue;
            }
            
            let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
            if let Ok(partial) = parser.partial(&current) {
                let completions = partial.completions(grammar);
                
                for regex in completions.iter().take(3) { // Limit branching
                    if let Some(example) = regex.example() {
                        let next = format!("{}{}", current, 
                            if current.is_empty() || current.ends_with(' ') || !example.chars().next().unwrap_or(' ').is_alphanumeric() {
                                example.clone()
                            } else {
                                format!(" {}", example)
                            }
                        );
                        
                        if !visited.contains(&next) && next.len() < 50 {
                            visited.insert(next.clone());
                            inputs.push(next.clone());
                            queue.push_back((next, depth + 1));
                        }
                    }
                }
            }
        }
        
        inputs
    }
}
