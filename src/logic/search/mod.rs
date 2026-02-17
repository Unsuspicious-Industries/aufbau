//! Priority-guided DFS search for completions.

pub mod scoring;

use crate::debug_info;
use crate::logic::grammar::Grammar;
use crate::logic::partial::{PartialAST, Synthesizer};
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree_with_context;
use crate::logic::typing::Context;
use crate::regex::Regex as DerivativeRegex;
use std::collections::{BinaryHeap, HashSet, VecDeque};

#[derive(Debug, Clone, Copy)]
pub struct SearchConfig {
    pub max_depth: usize,
    pub max_token_examples: usize,
}

impl Default for SearchConfig {
    fn default() -> Self {
        Self {
            max_depth: 10,
            max_token_examples: 10,
        }
    }
}

#[derive(Debug)]
pub enum SearchResult {
    Success {
        complete_input: String,
        ast: crate::logic::partial::NonTerminal,
        completion_path: Vec<DerivativeRegex>,
        depth: usize,
    },
    Exhausted {
        max_depth: usize,
        states_explored: usize,
        visited_states: Vec<String>,
    },
    Invalid {
        message: String,
    },
}

#[derive(Clone)]
struct SearchState {
    tree: PartialAST,
    depth: usize,
    path: Vec<DerivativeRegex>,
}

#[derive(Clone)]
struct ScoredState {
    score: f64,
    state: SearchState,
}

impl PartialEq for ScoredState {
    fn eq(&self, other: &Self) -> bool {
        self.score.to_bits() == other.score.to_bits()
    }
}

impl Eq for ScoredState {}

impl PartialOrd for ScoredState {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.score.partial_cmp(&other.score)
    }
}

impl Ord for ScoredState {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.score
            .partial_cmp(&other.score)
            .unwrap_or(std::cmp::Ordering::Equal)
    }
}

/// DFS with priority ordering: always expand the best-scoring child first.
///
/// Uses a shared Synthesizer for cached incremental parsing.
pub fn search_complete(
    grammar: &Grammar,
    input: &str,
    config: &SearchConfig,
    ctx: &Context,
) -> SearchResult {
    let mut synth = Synthesizer::new(grammar.clone(), input);
    let base_tree = match synth.partial() {
        Ok(ast) => ast,
        Err(e) => {
            return SearchResult::Invalid {
                message: format!("Input is not partially valid: {}", e),
            }
        }
    };

    if !base_tree.roots.is_empty() && !has_well_typed_root(&base_tree, grammar, ctx) {
        return SearchResult::Invalid {
            message: format!("No well-typed parse trees for input: '{}'", input),
        };
    }

    let mut visited: HashSet<String> = HashSet::new();
    visited.insert(base_tree.input().to_string());
    let mut visited_states: VecDeque<String> = VecDeque::new();
    visited_states.push_back(base_tree.input().to_string());

    let mut states_explored = 0usize;

    let initial_state = SearchState {
        tree: base_tree,
        depth: 0,
        path: Vec::new(),
    };

    let mut frontier: BinaryHeap<ScoredState> = BinaryHeap::new();
    let initial_score = scoring::calculate_score(&initial_state.tree, 0, config.max_depth).overall;
    frontier.push(ScoredState {
        score: initial_score,
        state: initial_state,
    });

    while let Some(ScoredState { state, .. }) = frontier.pop() {
        if let Some(complete_ast) = find_valid_completion(&state.tree, grammar, ctx) {
            if let Some(reconstructed) = complete_ast.text() {
                debug_info!(
                    "search",
                    "Completion found: depth={} input='{}'",
                    state.depth,
                    reconstructed
                );
                return SearchResult::Success {
                    complete_input: reconstructed,
                    ast: complete_ast,
                    completion_path: state.path.clone(),
                    depth: state.depth,
                };
            } else {
                return SearchResult::Invalid {
                    message: format!(
                        "Failed to reconstruct input from completed AST for prefix='{}'",
                        state.tree.input()
                    ),
                };
            }
        }

        if state.depth >= config.max_depth {
            continue;
        }

        // max_depth controls search size.
        let children = build_children(
            &mut synth,
            &state,
            ctx,
            config.max_depth,
            config.max_token_examples,
            &mut visited,
            &mut visited_states,
            &mut states_explored,
        );

        for (child, score) in children {
            frontier.push(ScoredState {
                score,
                state: child,
            });
        }
    }

    SearchResult::Exhausted {
        max_depth: config.max_depth,
        states_explored,
        visited_states: visited_states.into_iter().collect(),
    }
}

fn build_children(
    synth: &mut Synthesizer,
    state: &SearchState,
    ctx: &Context,
    max_depth: usize,
    max_token_examples: usize,
    visited: &mut HashSet<String>,
    visited_states: &mut VecDeque<String>,
    states_explored: &mut usize,
) -> Vec<(SearchState, f64)> {
    synth.set_input(state.tree.input().to_string());
    let tokens = synth.typed_completions(ctx);
    let mut children = Vec::new();

    for token in tokens.iter() {
        if let Some(child) = extend_state_with_token(synth, state, token, ctx, max_token_examples) {
            if visited.insert(child.tree.input().to_string()) {
                visited_states.push_back(child.tree.input().to_string());
                *states_explored += 1;
                let score = scoring::calculate_score(&child.tree, child.depth, max_depth).overall;
                children.push((child, score));
            }
        }
    }

    children.sort_by(|(_, score_a), (_, score_b)| {
        score_b
            .partial_cmp(score_a)
            .unwrap_or(std::cmp::Ordering::Equal)
    });

    children
}

fn extend_state_with_token(
    synth: &mut Synthesizer,
    state: &SearchState,
    token: &DerivativeRegex,
    ctx: &Context,
    max_token_examples: usize,
) -> Option<SearchState> {
    synth.set_input(state.tree.input().to_string());
    if let Some((ext, _extended)) = synth.extend_with_regex(token, ctx, max_token_examples) {
        if has_well_typed_root(&ext, synth.grammar(), ctx) {
            let mut path = state.path.clone();
            path.push(token.clone());
            return Some(SearchState {
                tree: ext,
                depth: state.depth + 1,
                path,
            });
        }
    }

    None
}

fn has_well_typed_root(ast: &PartialAST, grammar: &Grammar, ctx: &Context) -> bool {
    ast.roots.iter().any(|root| {
        if !root.is_complete() {
            true
        } else {
            matches!(
                check_tree_with_context(root, grammar, ctx),
                TreeStatus::Valid(_)
            )
        }
    })
}

fn find_valid_completion(
    ast: &PartialAST,
    grammar: &Grammar,
    ctx: &Context,
) -> Option<crate::logic::partial::NonTerminal> {
    let start = grammar.start_nonterminal();
    ast.roots.iter().find_map(|root| {
        if !root.is_complete() {
            return None;
        }
        if let Some(start) = start {
            if &root.name != start {
                return None;
            }
        }
        match check_tree_with_context(root, grammar, ctx) {
            TreeStatus::Valid(_) => Some(root.clone()),
            _ => None,
        }
    })
}
