// Completability Validation Tests
//
// These tests exercise the end-to-end guarantee that a partially parsed AST can
// always be completed to a full AST by repeatedly appending one of the valid
// completion tokens suggested by the partial-parsing engine.
//
// The algorithm implements a breadth-first search that explores all possible
// completion paths in parallel to ensure at least one path leads to a complete AST.
// This avoids infinite loops on paths that don't terminate while still finding
// successful completion sequences.

use std::collections::{HashMap, HashSet, VecDeque};
use crate::logic::ast::ASTNode;
use crate::logic::grammar::Grammar;
use crate::logic::partial::completion::CompletionSet;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::PartialAST;
use crate::{debug_debug, debug_trace, regex};
use crate::regex::Regex as DerivativeRegex;

/// Represents a search state in the completion exploration
#[derive(Clone, Debug)]
struct CompletionState {
    /// Current input string being explored
    input: String,
    /// Depth of exploration (number of tokens added)
    depth: usize,
    /// Path taken to reach this state (for debugging)
    path: Vec<DerivativeRegex>,
}

/// Result of completion exploration
#[derive(Debug)]
pub enum CompletionResult {
    /// Successfully found a complete AST
    Success {
        /// The complete input string that parses to a full AST
        complete_input: String,
        /// The resulting AST
        ast: ASTNode,
        /// The sequence of completion tokens used
        completion_path: Vec<DerivativeRegex>,
        /// Depth required to reach completion
        depth: usize,
    },
    /// No completion found within the search bounds
    Failure {
        /// Maximum depth explored
        max_depth_reached: usize,
        /// Number of unique states explored
        states_explored: usize,
        /// Sample of states that were explored but didn't lead to completion
        visited_states: Vec<String>,
    },

    // the input is invalid, not even partial
    Invalid(String),
    Inconsistency(String),
    /// Error during exploration
    Error(String),
}

/// Explore all possible completion paths using breadth-first search
/// until we find at least one path that leads to a complete AST.
pub fn complete_ast(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
) -> CompletionResult {
    // Initialize BFS queue with the starting state
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    let mut states_explored = 0;
    
    queue.push_back(CompletionState {
        input: input.to_string(),
        depth: 0,
        path: Vec::new(),
    });

    // fist do a partial parse check. If it parses partially its completeable. 
    // if it fails to parse partially, its invalid input.
    let mut parser = Parser::new(grammar.clone());
    match parser.partial(input) {
        Ok(_) => { /* valid partial input, continue */ }
        Err(e) => {
            return CompletionResult::Invalid(format!("Input is not even partially valid: {}", e))
        }
    };
    
    while let Some(current_state) = queue.pop_front() {
        states_explored += 1;
        
        // Avoid revisiting the same input string
        if visited.contains(&current_state.input) {
            continue;
        }
        visited.insert(current_state.input.clone());
        
        // Try to parse the current input
        let mut parser = Parser::new(grammar.clone());
        let partial_ast = match parser.partial(&current_state.input) {
            Ok(ast) => ast,
            Err(e) => {
                return CompletionResult::Invalid(e)
            }
        };
        
        // Check if we can get a complete AST from this state
        if let Ok(complete_ast) = try_complete_parse(grammar, &current_state.input) {
            return CompletionResult::Success {
                complete_input: current_state.input,
                ast: complete_ast,
                completion_path: current_state.path,
                depth: current_state.depth,
            };
        }
        
        // Get completions
        let completions = partial_ast.completions(grammar);
        if completions.tokens.is_empty() {
            // CRITICAL BUG: We followed completion engine suggestions but ended up with no completions
            // The input was at least partially parsed and succeded, so its an inconsistency.
            return CompletionResult::Inconsistency("No completions available from partial AST.".to_string());
        }
        debug_debug!("complete_ast",
            "At depth {}: input='{}' -> completions: {:?}", 
            current_state.depth, 
            current_state.input, 
            completions.tokens);
        
        // If we haven't reached max depth, explore next completion tokens
        if current_state.depth < max_depth {
            for token in &completions.tokens {
                let next_input = match extend_input(&current_state.input, token) {
                    Ok(input) => input,
                    Err(e) => return CompletionResult::Inconsistency(e),
                };
                
                // Skip if we've already explored this input
                // this is tree convergence pruning
                if visited.contains(&next_input) {
                    continue;
                }
                
                let mut next_path = current_state.path.clone();
                next_path.push(token.clone());
                
                queue.push_back(CompletionState {
                    input: next_input,
                    depth: current_state.depth + 1,
                    path: next_path,
                });
            }
        }
    }
    
    CompletionResult::Failure {
        max_depth_reached: max_depth,
        states_explored,
        visited_states: visited.into_iter().collect(),
    }
}

/// Try to parse the input as a complete AST (not partial)
fn try_complete_parse(grammar: &Grammar, input: &str) -> Result<ASTNode, String> {
    let mut parser = Parser::new(grammar.clone());
    
    // Try to parse as a complete expression
    match parser.parse(input.trim()) {
        Ok(ast) => Ok(ast),
        Err(e) => Err(format!("Complete parse failed: {}", e)),
    }
}

/// Extend input string with a completion token
fn extend_input(input: &str, token: &DerivativeRegex) -> Result<String, String> {
    match token.example() {
        Some(e) => Ok(format!("{}{}", input, e)),
        None => Err("Empty or corrupted regex pattern.".to_string())
    }

}



/// Test helper: Check if a grammar and input combination is completable
pub fn is_completable(grammar: &Grammar, input: &str, max_depth: usize) -> bool {
    matches!(complete_ast(grammar, input, max_depth), CompletionResult::Success { .. })
}

/// Test helper: Get completion statistics
pub fn completion_stats(grammar: &Grammar, input: &str, max_depth: usize) -> (bool, usize, usize) {
    match complete_ast(grammar, input, max_depth) {
        CompletionResult::Success { depth, .. } => (true, depth, 1),
        CompletionResult::Failure { states_explored, .. } => (false, max_depth, states_explored),
        CompletionResult::Error(_) => (false, 0, 0),
        CompletionResult::Invalid(_) => (false, 0, 0),
        CompletionResult::Inconsistency(_) => (false, 0, 0),
    }
}
