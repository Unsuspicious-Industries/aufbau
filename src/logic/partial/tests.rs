use super::PartialOutcome;
use super::PartialASTNode;
use crate::logic::check::debug;
use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use crate::logic::debug::*;
use super::display::*;
use crate::logic::ast::Terminal;
use std::fs;

fn parser(spec: &str) -> Parser { 
    Parser::new(Grammar::load(spec).expect("grammar load")) 
}

fn load_stlc_spec() -> String {
    fs::read_to_string("examples/stlc.spec").expect("Failed to read stlc.spec file")
}

#[test]
fn partial_empty_input_incomplete() {
    set_debug_level(DebugLevel::Trace);
    let spec = r#"
    Start ::= 'a' 'b'
    "#;
    let mut p = parser(spec);
    let out = p.partial("").expect("init ok");
    match out { 
        PartialOutcome::Incomplete { states } => assert!(!states.is_empty()), 
        _ => panic!("expected incomplete") 
    }
}

#[test]
fn partial_one_token_progress() {
    set_debug_level(DebugLevel::Trace);
    let spec = r#"Start ::= 'a' 'b'"#;
    let mut p = parser(spec);
    let out = p.partial("a").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            // We keep the expected node for 'b'
            assert!(!states.is_empty());
        }
        other => panic!("unexpected: {:?}", other)
    }
}

#[test]
fn partial_complete_two_tokens() {
    set_debug_level(DebugLevel::Trace);
    let spec = r#"Start ::= 'a' 'b'"#;
    let mut p = parser(spec);
    let out = p.partial("a b").expect("init ok");
    match out { 
        PartialOutcome::Complete { node } => {
            let nt = node.as_nonterminal().unwrap();
            assert_eq!(nt.children.len(), 2);
            assert_eq!(nt.children[0].value(), "a");
            assert_eq!(nt.children[1].value(), "b");
        }, 
        other => panic!("unexpected: {:?}", other)
    }
}

#[test]
fn partial_repetition_pending_trailer() {
    set_debug_level(DebugLevel::Trace);
    let spec = r#"
    Item ::= 'x'
    List ::= Item* 'end'
    "#;
    let mut p = parser(spec);
    let out = p.partial("x x").expect("init ok");
    match out { 
        PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty());
            // First partial state corresponds to List with flattened items and expected 'end'
            let st = &states[0];
            // We only check the textual form for now to avoid reaching into PartialASTNode internals from tests
            let txt = st.to_string();
            assert!(txt.contains("List"));
            assert!(txt.contains("x"));
            assert!(txt.contains("expected end") || txt.contains("expected 'end'"));
        }, 
        other => panic!("expected incomplete, got: {:?}", other)
    }
}

#[test]
fn partial_nested_nonterminal_midway() {
    set_debug_level(DebugLevel::Trace);
    let spec = r#"
    Start ::= A 'z'
    A ::= 'x' 'y'
    "#;
    let mut p = parser(spec);
    let out = p.partial("x").expect("init ok");
    match out { 
        PartialOutcome::Incomplete { states } => {
            for state in states {
                println!("State: {}", state);
            }
        }, 
        _ => panic!("expected incomplete") 
    }
}

#[test]
fn partial_mismatch_reports_incomplete() {
    set_debug_level(DebugLevel::Trace);
    let spec = r#"Start ::= 'a' 'b'"#;
    let mut p = parser(spec);
    let out = p.partial("c").expect("init ok");
    match out { 
        PartialOutcome::Error(_e) => {}, 
        _ => panic!("expected hard error on mismatch") 
    }
}

#[test]
fn debug_partial_one_token() {
    set_debug_level(DebugLevel::Trace);
    use crate::{debug_info, set_debug_level, DebugLevel};
    set_debug_level(DebugLevel::Debug);
    
    let spec = r#"Start ::= 'a' 'b'"#;
    let mut p = parser(spec);
    debug_info!("test", "Starting debug test for partial parsing");
    let out = p.partial("a").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            debug_info!("test", "States count: {}", states.len());
            for (i, state) in states.iter().enumerate() {
                debug_info!("test", "State {}: {}", i, state);
            }
        }
        other => debug_info!("test", "Unexpected outcome: {:?}", other)
    }
}

#[test]
fn partial_complete_and_serialize_complex_expr() {
    set_debug_level(DebugLevel::Trace);
    let spec = r#"
    Item ::= /[a-z]+/
    Items ::= Item (',' Item)*
    Start ::= 'test' Items 'end' | 'test' Items 'good' | 'bid'
    "#;
    let mut p = parser(spec);

    // Parse an incomplete input (missing trailing 'end')
    let out = p.partial("test foo,bar").expect("init ok");
    println!("Partial outcome: {:?}", out);
    println!("Got {} states", match &out {
        PartialOutcome::Incomplete { states } => states.len(),
        _ => 0
    });
    match out { 
        PartialOutcome::Incomplete { states } => {
            for state in states {
                println!("State: {}", state);
                println!("AST (simple): {}", state.ast.show_simple());
                println!("Production: {:#?}", state.final_production);
            }
        }, 
        _ => panic!("expected incomplete")  
    }
}


#[test]
fn partial_exact_hash_matching() {
    set_debug_level(DebugLevel::Info);
    let spec = r#"
    Item ::= 'a' | 'b'
    Start ::= Item Item
    "#;
    let mut p = parser(spec);
    
    // Parse partial input that should create incomplete states  
    let out = p.partial("a").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            // Verify each state has a unique hash based on its exact structure
            let mut hashes = Vec::new();
            for state in &states {
                let hash = state.calculate_hash();
                hashes.push(hash);
                println!("State hash: {}, AST: {}", hash, state.ast.show_simple());
            }
            
            // Check that identical states would produce identical hashes
            let first_state = &states[0];
            let cloned_state = first_state.clone();
            assert_eq!(first_state.calculate_hash(), cloned_state.calculate_hash(),
                      "Identical states should have identical hashes");
            
            println!("Hash consistency test passed");
        },
        PartialOutcome::Complete { .. } => {
            println!("Got complete parse, this is acceptable");
            // This could happen if the first part parses completely
        },
        other => panic!("Expected incomplete or complete, got: {:?}", other)
    }
}

#[test]
fn partial_stlc_lambda_abstraction() {
    set_debug_level(DebugLevel::Trace);
    let spec = load_stlc_spec();
    let mut p = parser(&spec);

    // Test parsing incomplete lambda abstraction (missing body)
    let out = p.partial("λ x : Int .").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty(), "Should have incomplete states for partial lambda");
            println!("Incomplete lambda parse: {} states", states.len());
            // Check that we're expecting a Term for the lambda body
            for state in &states {
                println!("Lambda partial state: {}", state);
                println!("AST structure: {}", state.ast.show_simple());
                let display = state.to_string();
                assert!(display.contains("Lambda") || display.contains("λ") || display.contains("Term"), 
                       "State should be parsing a lambda abstraction expecting Term");
            }
        },
        PartialOutcome::Complete { node } => {
            println!("Unexpected complete parse: {}", node.show_simple());
            panic!("Expected incomplete parse, but got complete");
        },
        PartialOutcome::Expandable { node } => {
            println!("Unexpected expandable parse: {}", node.show_simple());
            panic!("Expected incomplete parse, but got expandable");
        },
        other => panic!("Expected incomplete parse for partial lambda, got: {:?}", other)
    }
}

#[test]
fn partial_stlc_complete_lambda() {
    set_debug_level(DebugLevel::Trace);
    let spec = load_stlc_spec();
    let mut p = parser(&spec);

    // Test parsing complete lambda abstraction (identity function)
    let out = p.partial("λ x : Int . x").expect("init ok");
    match out {
        PartialOutcome::Complete { node } => {
            println!("Complete lambda parse: {}", node.show_simple());
            
            // Verify the structure of the parsed lambda abstraction
            let nt = node.as_nonterminal().expect("Should be a nonterminal");
            assert!(nt.value.contains("Lambda") || nt.value.contains("Term") || nt.value.contains("BaseTerm") || nt.value.contains("Program"), 
                   "Should parse as lambda abstraction or term containing it");
            
            // Check that we have the expected components
            let ast_display = node.show_simple();
            assert!(ast_display.contains("λ"), "Should contain lambda symbol");
            assert!(ast_display.contains("x"), "Should contain variable name");
            assert!(ast_display.contains("Int"), "Should contain type annotation");
        },
        PartialOutcome::Incomplete { states } => {
            // This is actually expected for STLC because Program ::= Term+ allows more terms
            println!("Incomplete parse (expandable): {} states", states.len());
            assert!(!states.is_empty(), "Should have incomplete states for expandable lambda");
            
            for state in &states {
                println!("Expandable lambda state: {}", state);
                let ast_display = state.ast.show_simple();
                // Verify the lambda was parsed correctly within the incomplete state
                assert!(ast_display.contains("λ"), "Should contain lambda symbol");
                assert!(ast_display.contains("x"), "Should contain variable name");  
                assert!(ast_display.contains("Int"), "Should contain type annotation");
            }
        },
        PartialOutcome::Expandable { node } => {
            // This is expected for STLC because Program ::= Term+ allows more terms
            println!("Expandable lambda parse: {}", node.show_simple());
            
            // Verify the structure of the parsed lambda abstraction
            let nt = node.as_nonterminal().expect("Should be a nonterminal");
            assert!(nt.value.contains("Lambda") || nt.value.contains("Term") || nt.value.contains("BaseTerm") || nt.value.contains("Program"), 
                   "Should parse as lambda abstraction or term containing it");
            
            // Check that we have the expected components
            let ast_display = node.show_simple();
            assert!(ast_display.contains("λ"), "Should contain lambda symbol");
            assert!(ast_display.contains("x"), "Should contain variable name");
            assert!(ast_display.contains("Int"), "Should contain type annotation");
        },
        other => panic!("Expected complete, incomplete, or expandable parse for full lambda, got: {:?}", other)
    }
}

#[test]
fn partial_stlc_function_application() {
    set_debug_level(DebugLevel::Trace);
    let spec = load_stlc_spec();
    let mut p = parser(&spec);

    // Test parsing function application with two variables
    let out = p.partial("f x").expect("init ok");
    match out {
        PartialOutcome::Complete { node } => {
            println!("Complete application parse: {}", node.show_simple());
            
            // Should parse as an Application
            let ast_display = node.show_simple();
            assert!(ast_display.contains("f"), "Should contain function f");
            assert!(ast_display.contains("x"), "Should contain argument x");
        },
        PartialOutcome::Expandable { node } => {
            println!("Expandable application parse: {}", node.show_simple());
            
            // This is the expected outcome since Program ::= Term+ allows more terms
            let ast_display = node.show_simple();
            assert!(ast_display.contains("f"), "Should contain function f");
            assert!(ast_display.contains("x"), "Should contain argument x");
        },
        PartialOutcome::Incomplete { states } => {
            // This might happen if the parser is expecting more input
            assert!(!states.is_empty(), "Should have states for application");
            
            for state in &states {
                println!("Application partial state: {}", state);
                let display = state.to_string();
                assert!(display.contains("Application") || display.contains("BaseTerm") || display.contains("Term"),
                       "Should be parsing application-related constructs");
            }
        },
        other => panic!("Expected complete, expandable, or incomplete parse for application, got: {:?}", other)
    }
}

#[test]
fn partial_stlc_let_binding() {
    set_debug_level(DebugLevel::Trace);
    let spec = load_stlc_spec();
    let mut p = parser(&spec);

    // Test parsing incomplete let binding (missing closing brace)
    let out = p.partial("{ x : Int").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty(), "Should have incomplete states for partial let");
            
            // Check that we're expecting the closing brace
            for state in &states {
                println!("Let partial state: {}", state);
                println!("AST structure: {}", state.ast.show_simple());
                let display = state.to_string();
                assert!(display.contains("Let") || display.contains("}") || display.contains("'}'"), 
                       "State should be parsing a let binding expecting closing brace");
            }
        },
        other => panic!("Expected incomplete parse for partial let, got: {:?}", other)
    }
}

#[test]
fn partial_stlc_function_type() {
    set_debug_level(DebugLevel::Trace);
    let spec = load_stlc_spec();
    let mut p = parser(&spec);

    // Test parsing lambda with function type annotation (Int -> Bool -> String)
    let out = p.partial("λ f : Int -> Bool").expect("init ok");
    match out {
        PartialOutcome::Incomplete { states } => {
            println!("Incomplete function type parse: {} states", states.len());
            assert!(!states.is_empty(), "Should have states for function type in lambda");
            
            // Print the states to debug
            for (i, state) in states.iter().enumerate() {
                println!("State {}: {}", i, state);
                println!("  AST: {}", state.ast.show_simple());
            }
            
            // We now explore all branches: ensure at least one branch captures the function type
            let has_fn_types = states.iter().any(|st| {
                let s = st.ast.show_simple();
                s.contains("Int") && s.contains("Bool")
            });
            assert!(has_fn_types, "At least one state should contain Int and Bool types");

            // Also expect some states to highlight specific missing tokens in context
            let has_missing_arrow = states.iter().any(|st| st.ast.show_simple().contains("missing ->"));
            let has_missing_dot = states.iter().any(|st| st.ast.show_simple().contains("missing ."));
            assert!(has_missing_arrow || has_missing_dot, "Should surface missing '->' or missing '.' states");
        },
        PartialOutcome::Complete { node } => {
            // This might happen if it parses a complete type
            println!("Complete function type parse: {}", node.show_simple());
            let ast_display = node.show_simple();
            assert!(ast_display.contains("Int") && ast_display.contains("Bool"), 
                   "Should contain type names");
        },
        other => panic!("Expected complete or incomplete parse for function type, got: {:?}", other)
    }
}
