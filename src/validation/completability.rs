// Completability Validation Tests
//
// Implements a formal framework for completability as defined in docs/challenges.md.
//
// A string s is completable in L if there exists s' such that ss' in L.
// We use a partial parser and typing core to check for completability.
//
// The algorithm explores the partial parse forest and uses the typing core
// to filter out invalid branches based on defined type rules.
use crate::logic::PartialAST;
use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::{Node, NonTerminal, Terminal};
use crate::logic::typing::Context;
use crate::logic::{BeamConfig, BeamResult, beam_complete};
use crate::regex::Regex as DerivativeRegex;
use rayon::prelude::*;
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
            if !value.is_empty()
                && !value.chars().all(|c| c.is_whitespace())
                && seen.insert(value.clone())
            {
                tokens.push(value.clone());
            }
        }
        Node::Terminal(Terminal::Partial { value, .. }) => {
            if !value.is_empty()
                && !value.chars().all(|c| c.is_whitespace())
                && seen.insert(value.clone())
            {
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
        ast: NonTerminal,
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
    /// The complete input string that was checked
    pub complete_string: Option<String>,
    /// Sample of visited states during failed completion
    pub failing_prefix_visited_states: Option<Vec<String>>,
}

/// Check sound completion for a given string
// Check for all the possible prefixes of the string
// if they are completable
// Parallelized implementation using rayon (toggle with PARALLELIZE=1)
pub fn sound_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
) -> PrefixSoundnessResult {
    let chars: Vec<char> = input.chars().collect();
    
    // handle completion without context
    let ctx = match opt_ctx {
        Some(c) => c,
        None => Context::new(),
    };

    // Check if parallelization is enabled via env var (default: false)
    let use_parallel = std::env::var("PARALLELIZE").is_ok_and(|v| v == "1");

    // Collect prefixes to check (keep order stable)
    let mut prefixes: Vec<(usize, String)> = Vec::with_capacity(chars.len() + 1);
    for len in 0..=chars.len() {
        let prefix: String = chars[..len].iter().collect();
        // Skip whitespace-only prefixes after the first
        if len > 0 && prefix.trim().is_empty() {
            continue;
        }
        prefixes.push((len, prefix));
    }

    // Compute whether each prefix is completable.
    // Keep this light: don't store full CompletionResult for every prefix.
    let flags: Vec<bool> = if use_parallel {
        prefixes
            .par_iter()
            .map(|(len, prefix)| {
                let result = complete(
                    grammar,
                    prefix,
                    max_depth + (chars.len() - *len),
                    Some(ctx.clone()),
                    max_states,
                );
                matches!(result, CompletionResult::Success { .. })
            })
            .collect()
    } else {
        prefixes
            .iter()
            .map(|(len, prefix)| {
                let result = complete(
                    grammar,
                    prefix,
                    max_depth + (chars.len() - *len),
                    Some(ctx.clone()),
                    max_states,
                );
                matches!(result, CompletionResult::Success { .. })
            })
            .collect()
    };

    // Process results sequentially to find first failure and extract complete_string
    let mut prefix_details = Vec::new();
    let mut failing_prefix = None;
    let mut failing_prefix_visited_states = None;
    let mut complete_string = String::new();

    for ((len, prefix), is_completable) in prefixes.into_iter().zip(flags.into_iter()) {
        prefix_details.push((prefix.clone(), is_completable));

        // Track a representative completed string (best-effort).
        // We only compute it once (last successful prefix wins).
        if is_completable {
            if let CompletionResult::Success { complete_input, .. } = complete(
                grammar,
                &prefix,
                max_depth + (chars.len() - len),
                Some(ctx.clone()),
                max_states,
            ) {
                complete_string = complete_input;
            }
        }

        // Record first failing prefix and gather detailed failure info once.
        if !is_completable && failing_prefix.is_none() {
            failing_prefix = Some(prefix.clone());

            let result = complete(
                grammar,
                &prefix,
                max_depth + (chars.len() - len),
                Some(ctx.clone()),
                max_states,
            );

            match result {
                CompletionResult::Failure { ref visited_states, .. } => {
                    if !visited_states.is_empty() {
                        failing_prefix_visited_states = Some(visited_states.clone());
                    }
                }
                _ => {}
            }
        }
    }

    PrefixSoundnessResult {
        is_sound: failing_prefix.is_none(),
        failing_prefix,
        prefixes_checked: prefix_details.len(),
        prefix_details,
        complete_string: if complete_string.is_empty() {
            None
        } else {
            Some(complete_string)
        },
        failing_prefix_visited_states,
    }
}

/// Complete with typing context using beam search (faster than BFS)
pub fn beam_complete_with_context(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
    beam_width: usize,
) -> CompletionResult {
    let ctx = match opt_ctx {
        Some(c) => c,
        None => Context::new(),
    };

    // Use beam search with given configuration
    let beam_config = BeamConfig {
        beam_width,
        max_depth,
        max_states,
        ..Default::default()
    };

    match beam_complete(grammar, input, &beam_config, &ctx) {
        BeamResult::Success {
            complete_input,
            ast,
            completion_path,
            score: _,
            depth,
        } => CompletionResult::Success {
            complete_input,
            ast,
            completion_path,
            depth,
        },
        BeamResult::Exhausted {
            max_depth: _,
            states_explored,
            best_state: _,
        } => CompletionResult::Failure {
            max_depth_reached: max_depth,
            states_explored,
            visited_states: vec![],
        },
        BeamResult::StateOverflow {
            limit,
            states_explored: _,
        } => CompletionResult::StateOverflow(limit),
        BeamResult::Invalid { message } => CompletionResult::Invalid(message),
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
            let error_msg = format!(
                "Input is not even partially valid: {}\n  Input: '{}'\n  Error: {}",
                e, input, e
            );
            return CompletionResult::Invalid(error_msg);
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
        } else if max_states.is_some_and(|max| states_explored > max) {
            return CompletionResult::Error(format!(
                "Exceeded maximum states explored limit: {}",
                max_states.unwrap()
            ));
        }
        visited.insert(current_state.tree.clone());
        let ast = current_state.tree.clone();

        // Check if we have a complete AND well-typed AST
        if ast.typed_complete_ctx(grammar, &ctx).is_ok() {
            if let Some(complete_ast) = ast.clone().complete() {
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
                &token,
                grammar,
                &state_symbols,
                max_completion_search,
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

fn try_extend(tree: &PartialAST, grammar: &Grammar, token: String) -> Result<PartialAST, String> {
    // Try appending the token directly first
    let new_input = format!("{}{}", tree.input(), token);

    let mut parser = Parser::new(grammar.clone());
    match parser.partial_typed(&new_input) {
        Ok(new_tree) => Ok(new_tree),
        Err(e) => {
            // If direct append fails, try with a space delimiter
            let new_input_with_space = format!("{} {}", tree.input(), token);
            match parser.partial_typed(&new_input_with_space) {
                Ok(new_tree) => Ok(new_tree),
                Err(e2) => Err(format!(
                    "Failed to extend tree with token {:?}: {} (with space: {})",
                    token, e, e2
                )),
            }
        }
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
