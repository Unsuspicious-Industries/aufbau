// Completability Validation Tests
//
// Implements the formal framework for completability as defined in docs/challenges.md.
//
// A string s is completable in L if there exists s' such that ss' in L.
// We use a partial parser and typing core to check for completability.
//
// The algorithm explores the partial parse forest and uses the typing core
// to filter out invalid branches based on defined type rules.
use crate::logic::PartialAST;
use crate::logic::ast::ASTNode;
use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::{Node, Terminal};
use crate::logic::typing::Context;
use crate::regex::Regex as DerivativeRegex;
use std::collections::{HashSet, VecDeque};

/// Extract all identifier tokens from the AST by traversing terminal nodes
/// This preserves the order in which tokens appear in the parse tree
fn extract_tokens_from_ast(ast: &PartialAST) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut seen = HashSet::new();
    
    for root in ast.roots() {
        collect_tokens_from_node(&Node::NonTerminal(root.clone()), &mut tokens, &mut seen);
    }
    
    tokens
}

fn collect_tokens_from_node(node: &Node, tokens: &mut Vec<String>, seen: &mut HashSet<String>) {
    match node {
        Node::Terminal(Terminal::Complete { value, .. }) => {
            if !value.is_empty() && !value.chars().all(|c| c.is_whitespace()) && seen.insert(value.clone()) {
                tokens.push(value.clone());
            }
        }
        Node::Terminal(Terminal::Partial { value, .. }) => {
            if !value.is_empty() && !value.chars().all(|c| c.is_whitespace()) && seen.insert(value.clone()) {
                tokens.push(value.clone());
            }
        }
        Node::NonTerminal(nt) => {
            for child in &nt.children {
                collect_tokens_from_node(child, tokens, seen);
            }
        }
    }
}



/// Represents a search state in the completion exploration
#[derive(Clone, Debug)]
struct CompletionState {
    /// Current tree being explored
    tree: PartialAST,
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

    // overflowing max states
    StateOverflow(usize),

    // the input is invalid, not even partial
    Invalid(String),
    Inconsistency(String),
    /// Error during exploration
    Error(String),
}


/// Result of prefix soundness check with detailed information
#[derive(Debug)]
pub struct PrefixSoundnessResult {
    /// Whether all prefixes are completable
    pub is_sound: bool,
    /// The first failing prefix (if any)
    pub failing_prefix: Option<String>,
    /// Number of prefixes checked
    pub prefixes_checked: usize,
    /// Details about each prefix check
    pub prefix_details: Vec<(String, bool)>,
}

/// Check sound completion for a given string
// Check for all the possible prefixes of the string
// if they are completable
// Simple implementation
pub fn check_sound_completion(
    grammar: &Grammar, 
    input: &str, 
    max_depth: usize,
    opt_ctx: Option<Context>
) -> PrefixSoundnessResult {
    let chars: Vec<char> = input.chars().collect();
    let mut prefix_details = Vec::new();
    let mut failing_prefix = None;

    // handle completion without context
    let ctx = match opt_ctx {
        Some(c) => c,
        None => Context::new(),
    };
    
    
    for len in 0..=chars.len() {
        let prefix: String = chars[..len].iter().collect();
        
        // Skip whitespace-only prefixes after the first
        if len > 0 && prefix.trim().is_empty() {
            continue;
        }
        
        let is_completable = matches!(
            complete(
                grammar, 
                &prefix, 
                max_depth + (chars.len() - len), 
                Some(ctx.clone()),
                None
            ),
            CompletionResult::Success { .. }
        );
        
        prefix_details.push((prefix.clone(), is_completable));
        
        if !is_completable && failing_prefix.is_none() {
            failing_prefix = Some(prefix);
        }
    }
    
    PrefixSoundnessResult {
        is_sound: failing_prefix.is_none(),
        failing_prefix,
        prefixes_checked: prefix_details.len(),
        prefix_details,
    }
}


/// Complete with typing context - always enforces type rules
pub fn complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
) -> CompletionResult {

    let ctx = match opt_ctx {
        Some(c) => c,
        None => Context::new(),
    };

    // Initialize BFS queue with the starting state
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    let mut states_explored = 0;

    // fist do a partial parse check. If it parses partially its completeable.
    // if it fails to parse partially, its invalid input.
    let mut parser = Parser::new(grammar.clone());
    let base_tree = match parser.partial_typed(input) {
        Ok(ast) => ast,
        Err(e) => {
            return CompletionResult::Invalid(format!("Input is not even partially valid: {}", e));
        }
    };

    queue.push_back(CompletionState {
        tree: base_tree,
        depth: 0,
        path: Vec::new(),
    });

    while let Some(current_state) = queue.pop_front() {
        states_explored += 1;
        
        // Avoid revisiting the same input string
        if visited.contains(&current_state.tree) {
            continue;
        } else if max_states.is_some_and(|max|states_explored > max) {
            return CompletionResult::Error(format!(
                "Exceeded maximum states explored limit: {}",
                max_states.unwrap()
            ));
        }
        visited.insert(current_state.tree.clone());
        let ast = current_state.tree.clone();

        // Check if we have a complete AND well-typed AST
        if ast.typed_complete_ctx(grammar, &ctx).is_ok() {
            if let Ok(complete_ast) = ast.clone().into_complete() {
                return CompletionResult::Success {
                    complete_input: current_state.tree.input.clone(),
                    ast: complete_ast,
                    completion_path: current_state.path.clone(),
                    depth: current_state.depth,
                };
            }
        }
        
        // If we haven't reached max depth, explore next completion tokens
        // here we add all new states
        if current_state.depth >= max_depth {
            continue; // Reached max depth, do not explore further
        }

        // Gather symbols for THIS specific state by extracting tokens from the AST
        // This is cached per unique input automatically by the BFS deduplication
        let state_symbols = extract_tokens_from_ast(&ast);

        // heuristics stupid but yea dont have choice
        let max_completion_search = 10;

        let completions = ast.completions(grammar);
        if completions.tokens.is_empty() {
            continue; // No completions available from this state
        }

        for token in completions.tokens.iter() {
            let next_tree = match extend_tree(
                &current_state.tree, 
                &token, grammar, 
                &state_symbols, 
                max_completion_search
            ) {
                Some(extended) => extended,
                None => continue, // Could not extend with this token
            };

            // Skip if we've already explored this input
            // this is tree convergence pruning
            if visited.contains(&next_tree) {
                continue;
            }

            let mut next_path = current_state.path.clone();
            next_path.push(token.clone());

            queue.push_back(CompletionState {
                tree: next_tree,
                depth: current_state.depth + 1,
                path: next_path,
            });
        }
    }

    CompletionResult::Failure {
        max_depth_reached: max_depth,
        states_explored,
        visited_states: visited.into_iter().map(|ast| ast.input.clone()).collect(),
    }
}



fn try_extend(
    tree: &PartialAST,
    grammar: &Grammar,
    token: String,
) -> Result<PartialAST, String> {
    // add a space as delimiter
    // TODO improve this weird behavior
    let new_input = format!("{} {}", tree.input(), token);

    let mut parser = Parser::new(grammar.clone());
    match parser.partial_typed(&new_input) {
        Ok(new_tree) => Ok(new_tree),
        Err(e) => Err(format!("Failed to extend tree with token {:?}: {}", token, e)),
    }
}

fn extend_tree(
    tree: &PartialAST,
    token: &DerivativeRegex,
    grammar: &Grammar,
    symbols: &[String],
    n_search: usize,
) -> Option<PartialAST> {

    // Phase 1: Try context symbols first (symbols that match the token regex)
    // Symbols are already shuffled for better exploration
    let filtered_sybols: Vec<String> = symbols
        .iter()
        .filter(|sym| token.matches(sym))
        .cloned()
        .collect();

    // First priority: candidates that are in the context symbols
    for candidate in &filtered_sybols {
        if let Ok(extended) = try_extend(tree, grammar, candidate.clone()) {
            return Some(extended);
        }

    }
    let examples = token.examples(n_search);
    for candidate in &examples {
        if let Ok(extended) = try_extend(tree, grammar, candidate.clone()) {
            return Some(extended);
        }
    }

    None
}

/// Test helper: Check if a grammar and input combination is completable
pub fn is_completable(grammar: &Grammar, input: &str, max_depth: usize) -> bool {
    matches!(
        complete(grammar, input, max_depth, None, None),
        CompletionResult::Success { .. }
    )
}




