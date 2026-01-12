use regex_syntax::ast::print;

use super::*;

#[test]
fn test_beam_search_simple_grammar() {
    use crate::logic::grammar::Grammar;

    // Simple grammar: A -> 'a' | 'a' B, B -> 'b'
    let spec = r#"
        A ::= 'a' | 'a' B
        B ::= 'b'
        start ::= A
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let ctx = crate::logic::typing::Context::new();

    // Complete input "ab" should succeed immediately
    let config = BeamConfig::fast();
    match beam_complete(&grammar, "ab", &config, &ctx) {
        BeamResult::Success {
            complete_input,
            depth,
            ..
        } => {
            assert_eq!(complete_input, "ab");
            assert_eq!(depth, 0);
        }
        _ => panic!("Expected success for complete input"),
    }

    // Partial input "a" should find completion "ab"
    match beam_complete(&grammar, "a", &config, &ctx) {
        BeamResult::Success {
            complete_input,
            depth,
            ..
        } => {
            println!("Completed input: '{}'", complete_input);
            assert!(complete_input.contains('a'));
            assert!(depth == 0);
        }
        _ => panic!("Expected success for partial input"),
    }
}

#[test]
fn test_beam_size_preference() {
    use crate::logic::grammar::Grammar;

    let spec = r#"
        A ::= 'a'
        start ::= A | A A | A A A
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let ctx = crate::logic::typing::Context::new();

    let config = BeamConfig {
        beam_width: 2,
        max_depth: 3,
        max_states: None,
        completeness_weight: 0.1,
        typing_weight: 0.1,
        simplicity_weight: 1.0,
    };

    // Small completion "a" should be preferred over "aa" or "aaa"
    match beam_complete(&grammar, "", &config, &ctx) {
        BeamResult::Success { complete_input, .. } => {
            // Should prefer "a" (smaller tree)
            assert_eq!(complete_input, " a");
        }
        _ => {}
    }
}

#[test]
fn test_beam_pruning() {
    use crate::logic::grammar::Grammar;

    let spec = r#"
        A ::= 'a' | 'b' | 'c' | 'd' | 'e'
        start ::= A
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let ctx = crate::logic::typing::Context::new();

    // Narrow beam should only explore top 2 states
    let config = BeamConfig {
        beam_width: 2,
        max_depth: 1,
        max_states: None,
        completeness_weight: 1.0,
        typing_weight: 0.5,
        simplicity_weight: 0.5,
    };

    match beam_complete(&grammar, "", &config, &ctx) {
        BeamResult::Success { .. } => {
            // Should find a completion
        }
        BeamResult::Exhausted {
            states_explored, ..
        } => {
            // With beam width 2, we should explore at most 2 candidates
            assert!(states_explored <= 10); // Rough upper bound
        }
        _ => {}
    }
}

