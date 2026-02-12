//! Beam Search Algorithm
//!
//! Core beam search implementation for efficient completion exploration.
//!
//! ## Algorithm Overview
//!
//! 1. Parse initial input to get initial state
//! 2. For each depth level:
//!    a. Check if any state is complete and well-typed
//!    b. Generate candidates by extending each state with type-checked tokens
//!    c. Score all candidates
//!    d. Keep only top K candidates (beam pruning)
//!
//! ## Key Optimizations
//!
//! - **Beam pruning**: At each depth, only keep top K states
//! - **State deduplication**: Don't explore same input twice
//! - **Early termination**: Stop when we find a complete, well-typed state
//! - **Type-aware extension**: Only accept extensions that remain well-typed
//! - **Smart scoring**: Prefer small, well-typed, simple completions

use crate::logic::PartialAST;
use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::{Node, NonTerminal, Terminal};
use crate::logic::typing::Context;
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree_with_context;
use crate::regex::Regex as DerivativeRegex;
use std::collections::{BinaryHeap, HashSet};

use super::config::BeamConfig;
use super::scoring::calculate_score;
use super::state::{BeamResult, BeamState};

/// Run beam search to find a completion for given input
pub fn beam_complete(
    grammar: &Grammar,
    input: &str,
    config: &BeamConfig,
    ctx: &Context,
) -> BeamResult {
    // Parse initial input
    let mut parser = Parser::new(grammar.clone());
    let base_tree = match parser.partial(input) {
        Ok(ast) => ast,
        Err(e) => {
            return BeamResult::Invalid {
                message: format!("Input is not partially valid: {}", e),
            };
        }
    };

    // Early reject: if no root is even partially well-typed, the input is type-broken
    if !base_tree.roots.is_empty() && !has_well_typed_root(&base_tree, grammar, ctx) {
        return BeamResult::Invalid {
            message: format!("No well-typed parse trees for input: '{}'", input),
        };
    }

    // Initialize beam with starting state
    let initial_score = calculate_score(&base_tree, 0, config);
    let mut current_beam = vec![BeamState::new(base_tree.clone(), 0, initial_score.overall)];

    // Track visited states to avoid re-exploring same input
    let mut visited: HashSet<String> = HashSet::new();
    visited.insert(base_tree.input().to_string());

    let mut states_explored = 0;

    // Beam search: explore depth by depth
    for depth in 0..config.max_depth {
        // Check if any state in current beam is complete and well-typed
        for state in &current_beam {
            if let Some(complete_ast) = find_valid_completion(&state.tree, grammar, ctx) {
                return BeamResult::Success {
                    complete_input: state.tree.input().to_string(),
                    ast: complete_ast,
                    completion_path: state.path.clone(),
                    score: state.score,
                    depth: state.depth,
                };
            }
        }

        // Generate next beam by extending each state
        let mut next_candidates: BinaryHeap<ScoredCandidate> = BinaryHeap::new();

        for state in &current_beam {
            states_explored += 1;

            // Check state limit
            if let Some(max) = config.max_states {
                if states_explored > max {
                    return BeamResult::StateOverflow {
                        limit: max,
                        states_explored,
                    };
                }
            }

            // Extract symbols from the current AST for candidate generation
            let state_symbols = extract_tokens_from_ast(&state.tree);

            // Get valid completions for this state (type-filtered)
            let completions = state.tree.completions_in_ctx(grammar, ctx);

            for token in completions.tokens.iter() {
                // Try to extend with this token (type-checked)
                if let Some(extended_tree) =
                    extend_tree_typed(&state.tree, token, grammar, &state_symbols, 10, ctx)
                {
                    // Skip if we've already explored this input
                    if visited.contains(extended_tree.input()) {
                        continue;
                    }

                    // Build extended path
                    let mut next_path = state.path.clone();
                    next_path.push(token.clone());

                    // Calculate score for extended state
                    let score = calculate_score(&extended_tree, depth + 1, config);

                    next_candidates.push(ScoredCandidate {
                        state: BeamState::with_path(
                            extended_tree,
                            depth + 1,
                            score.overall,
                            next_path,
                        ),
                        score: score.overall,
                    });
                }
            }
        }

        // Prune to beam width: keep only top K states
        if next_candidates.is_empty() {
            break;
        }

        current_beam = select_top_k(next_candidates, config.beam_width);

        // Mark all new inputs as visited
        for state in &current_beam {
            visited.insert(state.tree.input().to_string());
        }
    }

    // Check one last time after the final depth
    for state in &current_beam {
        if let Some(complete_ast) = find_valid_completion(&state.tree, grammar, ctx) {
            return BeamResult::Success {
                complete_input: state.tree.input().to_string(),
                ast: complete_ast,
                completion_path: state.path.clone(),
                score: state.score,
                depth: state.depth,
            };
        }
    }

    // Beam exhausted without finding completion
    let best_state = current_beam.into_iter().max_by(|a, b| {
        a.score
            .partial_cmp(&b.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    BeamResult::Exhausted {
        max_depth: config.max_depth,
        states_explored,
        best_state,
    }
}

// ============================================================================
// Type-aware helpers (shared logic with completability.rs)
// ============================================================================

/// Check if a partial AST has at least one potentially well-typed root.
fn has_well_typed_root(ast: &PartialAST, grammar: &Grammar, ctx: &Context) -> bool {
    ast.roots.iter().any(|root| {
        matches!(
            check_tree_with_context(root, grammar, ctx),
            TreeStatus::Valid(_) | TreeStatus::Partial(_)
        )
    })
}

/// Find a root that is complete, well-typed, AND matches the start symbol.
fn find_valid_completion(
    ast: &PartialAST,
    grammar: &Grammar,
    ctx: &Context,
) -> Option<NonTerminal> {
    let start_nt = grammar.start_nonterminal();
    ast.roots.iter().find(|root| {
        root.is_complete()
            && start_nt.map_or(true, |s| &root.name == s)
            && matches!(
                check_tree_with_context(root, grammar, ctx),
                TreeStatus::Valid(_)
            )
    }).cloned()
}

/// Extract identifier tokens from the AST for candidate generation.
fn extract_tokens_from_ast(ast: &PartialAST) -> Vec<String> {
    let mut tokens = Vec::new();
    let mut seen = HashSet::new();
    for root in ast.roots() {
        collect_tokens(&Node::NonTerminal(root.clone()), &mut tokens, &mut seen);
    }
    tokens
}

fn collect_tokens(node: &Node, tokens: &mut Vec<String>, seen: &mut HashSet<String>) {
    match node {
        Node::Terminal(Terminal::Complete { value, .. })
        | Node::Terminal(Terminal::Partial { value, .. }) => {
            if !value.is_empty()
                && !value.chars().all(|c| c.is_whitespace())
                && seen.insert(value.clone())
            {
                tokens.push(value.clone());
            }
        }
        Node::NonTerminal(nt) => {
            for child in &nt.children {
                collect_tokens(child, tokens, seen);
            }
        }
    }
}

/// Try appending a concrete token to the input, returning parsed tree if valid.
fn try_extend(tree: &PartialAST, grammar: &Grammar, token: &str) -> Option<PartialAST> {
    let input = tree.input();
    let mut parser = Parser::new(grammar.clone());

    // Try without space first
    let direct = format!("{}{}", input, token);
    if let Ok(t) = parser.partial(&direct) {
        return Some(t);
    }

    // Try with space
    let spaced = format!("{} {}", input, token);
    if let Ok(t) = parser.partial(&spaced) {
        return Some(t);
    }

    None
}

/// Extend tree with a token regex, choosing the first candidate that
/// produces a well-typed partial tree.
fn extend_tree_typed(
    tree: &PartialAST,
    token: &DerivativeRegex,
    grammar: &Grammar,
    symbols: &[String],
    n_search: usize,
    ctx: &Context,
) -> Option<PartialAST> {
    // Phase 1: Try AST symbols that match the token regex
    for sym in symbols.iter().filter(|s| token.matches(s)) {
        if let Some(ext) = try_extend(tree, grammar, sym) {
            if has_well_typed_root(&ext, grammar, ctx) {
                return Some(ext);
            }
        }
    }

    // Phase 2: Try generated examples from the regex
    for candidate in token.examples(n_search) {
        if let Some(ext) = try_extend(tree, grammar, &candidate) {
            if has_well_typed_root(&ext, grammar, ctx) {
                return Some(ext);
            }
        }
    }

    None
}

/// Select top K candidates from a max-heap
fn select_top_k(mut heap: BinaryHeap<ScoredCandidate>, k: usize) -> Vec<BeamState> {
    let mut result = Vec::with_capacity(k);
    for _ in 0..k {
        match heap.pop() {
            Some(c) => result.push(c.state),
            None => break,
        }
    }
    result
}

// ============================================================================
// Internal types
// ============================================================================

#[derive(Debug, Clone)]
struct ScoredCandidate {
    state: BeamState,
    score: f64,
}

impl PartialEq for ScoredCandidate {
    fn eq(&self, other: &Self) -> bool {
        self.score.to_bits() == other.score.to_bits()
    }
}

impl Eq for ScoredCandidate {}

impl PartialOrd for ScoredCandidate {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // Higher score = higher priority
        self.score.partial_cmp(&other.score)
    }
}

impl Ord for ScoredCandidate {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score
            .partial_cmp(&other.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    }
}
