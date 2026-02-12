// Completability Validation
//
// Implements a formal framework for completability as defined in docs/challenges.md.
//
// A string s is completable in L if there exists s' such that ss' in L.
// We use a partial parser and typing core to check for completability.
//
// Strategy:
//   1. Beam search first (fast, O(k*d) instead of O(b^d))
//   2. BFS fallback if beam misses (complete, explores all reachable states)
//
// All extensions are type-checked: only well-typed partial trees are kept.

use crate::logic::PartialAST;
use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::{Node, NonTerminal, Terminal};
use crate::logic::typing::Context;
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree_with_context;
use crate::logic::{BeamConfig, BeamResult, beam_complete};
use crate::regex::Regex as DerivativeRegex;
use std::collections::{HashSet, VecDeque};

// ============================================================================
// Core type-aware helpers
// ============================================================================

/// Find a root that is complete, well-typed, AND matches the start symbol.
/// Returns None if no such root exists.
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

/// Filter AST to keep only well-typed roots (Valid or Partial status).
fn filter_well_typed_roots(ast: PartialAST, grammar: &Grammar, ctx: &Context) -> PartialAST {
    let well_typed_roots = ast
        .roots
        .into_iter()
        .filter(|root| {
            matches!(
                check_tree_with_context(root, grammar, ctx),
                TreeStatus::Valid(_) | TreeStatus::Partial(_)
            )
        })
        .collect();
    PartialAST {
        roots: well_typed_roots,
        input: ast.input,
    }
}

/// Extract all identifier tokens from the AST (deduped, order-preserving).
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
                collect_tokens_from_node(child, tokens, seen);
            }
        }
    }
}

/// Try appending a concrete token to the input (with and without space).
fn try_extend(tree: &PartialAST, grammar: &Grammar, token: &str) -> Option<PartialAST> {
    let input = tree.input();
    let mut parser = Parser::new(grammar.clone());

    let direct = format!("{}{}", input, token);
    if let Ok(t) = parser.partial(&direct) {
        return Some(t);
    }

    let spaced = format!("{} {}", input, token);
    if let Ok(t) = parser.partial(&spaced) {
        return Some(t);
    }

    None
}

/// Extend tree with a token regex, returning an AST with only well-typed roots.
/// This optimizes by filtering malformed roots early, avoiding redundant checks downstream.
fn extend_tree(
    tree: &PartialAST,
    token: &DerivativeRegex,
    grammar: &Grammar,
    symbols: &[String],
    n_search: usize,
    ctx: &Context,
) -> Option<PartialAST> {
    // Phase 1: Try AST symbols matching the token regex
    for sym in symbols.iter().filter(|s| token.matches(s)) {
        if let Some(ext) = try_extend(tree, grammar, sym) {
            let filtered = filter_well_typed_roots(ext, grammar, ctx);
            if !filtered.roots.is_empty() {
                return Some(filtered);
            }
        }
    }

    // Phase 2: Try generated examples from the regex
    for candidate in token.examples(n_search) {
        if let Some(ext) = try_extend(tree, grammar, &candidate) {
            let filtered = filter_well_typed_roots(ext, grammar, ctx);
            if !filtered.roots.is_empty() {
                return Some(filtered);
            }
        }
    }

    None
}

// ============================================================================
// Completion State
// ============================================================================

#[derive(Clone, Debug)]
struct CompletionState {
    tree: PartialAST,
    depth: usize,
    path: Vec<DerivativeRegex>,
}

// ============================================================================
// Public Result Types
// ============================================================================

/// Result of completion exploration
#[derive(Debug)]
pub enum CompletionResult {
    /// Successfully found a complete AST
    Success {
        complete_input: String,
        ast: NonTerminal,
        completion_path: Vec<DerivativeRegex>,
        depth: usize,
        /// Whether beam search was attempted and we had to fall back to BFS
        beam_fallback: bool,
    },
    /// No completion found within the search bounds
    Failure {
        max_depth_reached: usize,
        states_explored: usize,
        visited_states: Vec<String>,
        /// Whether we attempted beam first and fell back to BFS
        beam_fallback: bool,
    },
    StateOverflow(usize),
    Invalid(String),
    Inconsistency(String),
    Error(String),
}

/// Per-prefix detailed measurements useful for profiling and analytics
#[derive(Debug, Clone)]
pub struct PrefixDetail {
    /// The prefix string (empty string for length 0)
    pub prefix: String,
    /// Whether this prefix was completable
    pub ok: bool,
    /// Time taken to check this prefix in microseconds
    pub time_us: u128,
    /// Number of states explored (if available)
    pub states_explored: Option<usize>,
    /// Count of visited states (if available)
    pub visited_count: Option<usize>,
    /// Sample of visited states (truncated)
    pub visited_sample: Vec<String>,
    /// Whether beam search was attempted and (if so) whether BFS fallback occurred
    pub beam_fallback: Option<bool>,
}

/// Result of prefix soundness check with detailed information
#[derive(Debug)]
pub struct PrefixSoundnessResult {
    pub is_sound: bool,
    pub failing_prefix: Option<String>,
    pub prefixes_checked: usize,
    pub prefix_details: Vec<(String, bool)>,
    pub complete_string: Option<String>,
    pub failing_prefix_visited_states: Option<Vec<String>>,
    /// Rich per-prefix metadata (timings, visited counts, samples, beam info)
    pub prefix_meta: Vec<PrefixDetail>,
}

// ============================================================================
// Unified completion: beam-first, BFS fallback
// ============================================================================

/// Complete with typing context.
///
/// Strategy: try beam search first (fast), fall back to BFS (thorough).
/// All extensions are type-checked so only well-typed trees are explored.
pub fn complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
) -> CompletionResult {
    let ctx = opt_ctx.unwrap_or_else(Context::new);

    // Phase 1: Beam search (fast path)
    let beam_config = BeamConfig {
        beam_width: 15,
        max_depth,
        max_states,
        ..Default::default()
    };

    let beam_res = beam_complete(grammar, input, &beam_config, &ctx);
    match beam_res {
        BeamResult::Success {
            complete_input,
            ast,
            completion_path,
            depth,
            ..
        } => {
            return CompletionResult::Success {
                complete_input,
                ast,
                completion_path,
                depth,
                beam_fallback: false,
            };
        }
        BeamResult::Invalid { message } => {
            return CompletionResult::Invalid(message);
        }
        other => {
            crate::debug_trace!("completability", "beam search did not succeed (result={:?}); falling back to BFS", other);
        }
    }

    // Phase 2: BFS fallback (thorough) — record that beam was attempted
    bfs_complete(grammar, input, max_depth, &ctx, max_states, true)
}

/// BFS-based completion (exhaustive, used as fallback).
fn bfs_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    ctx: &Context,
    max_states: Option<usize>,
    beam_fallback: bool,
) -> CompletionResult {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    let mut states_explored = 0;

    let mut parser = Parser::new(grammar.clone());
    let parsed = match parser.partial(input) {
        Ok(ast) => ast,
        Err(e) => {
            return CompletionResult::Invalid(format!(
                "Input is not even partially valid: {}\n  Input: '{}'\n  Error: {}",
                e, input, e
            ));
        }
    };

    // Filter out malformed roots early to avoid redundant checks later.
    let filtered_base = filter_well_typed_roots(parsed.clone(), grammar, ctx);

    // Early reject: input had parse roots but none are well-typed
    if !parsed.roots.is_empty() && filtered_base.roots.is_empty() {
        return CompletionResult::Invalid(format!(
            "No well-typed parse trees for input: '{}'",
            input
        ));
    }

    queue.push_back(CompletionState {
        tree: filtered_base,
        depth: 0,
        path: Vec::new(),
    });

    while let Some(current) = queue.pop_front() {
        states_explored += 1;

        if visited.contains(&current.tree) {
            continue;
        }
        if max_states.is_some_and(|max| states_explored > max) {
            return CompletionResult::StateOverflow(max_states.unwrap());
        }

        visited.insert(current.tree.clone());
        let ast = &current.tree;

        // Check: root must be complete + well-typed + match start symbol
        if let Some(complete_ast) = find_valid_completion(ast, grammar, ctx) {
            return CompletionResult::Success {
                complete_input: current.tree.input.clone(),
                ast: complete_ast,
                completion_path: current.path.clone(),
                depth: current.depth,
                beam_fallback,
            };
        }

        if current.depth >= max_depth {
            continue;
        }

        let state_symbols = extract_tokens_from_ast(ast);
        let completions = ast.completions_in_ctx(grammar, ctx);
        if completions.tokens.is_empty() {
            continue;
        }

        for token in completions.tokens.iter() {
            let next_tree = match extend_tree(
                &current.tree,
                token,
                grammar,
                &state_symbols,
                10,
                ctx,
            ) {
                Some(ext) => ext,
                None => continue,
            };

            if visited.contains(&next_tree) {
                continue;
            }

            let mut next_path = current.path.clone();
            next_path.push(token.clone());

            queue.push_back(CompletionState {
                tree: next_tree,
                depth: current.depth + 1,
                path: next_path,
            });
        }
    }

    CompletionResult::Failure {
        max_depth_reached: max_depth,
        states_explored,
        visited_states: visited.into_iter().map(|ast| ast.input.clone()).collect(),
        beam_fallback,
    }
}

// ============================================================================
// Beam-only completion (public API for callers that only want beam)
// ============================================================================

/// Complete using beam search only (no BFS fallback).
pub fn beam_complete_with_context(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
    beam_width: usize,
) -> CompletionResult {
    let ctx = opt_ctx.unwrap_or_else(Context::new);

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
            depth,
            ..
        } => CompletionResult::Success {
            complete_input,
            ast,
            completion_path,
            depth,
            beam_fallback: false,
        },
        BeamResult::Exhausted {
            states_explored, ..
        } => CompletionResult::Failure {
            max_depth_reached: max_depth,
            states_explored,
            visited_states: vec![],
            beam_fallback: false,
        },
        BeamResult::StateOverflow { limit, .. } => CompletionResult::StateOverflow(limit),
        BeamResult::Invalid { message } => CompletionResult::Invalid(message),
    }
}

// ============================================================================
// Prefix Soundness (parallelized)
// ============================================================================

/// Check sound completion: every prefix of the input must be completable.
///
/// Check each prefix sequentially. Each prefix is checked independently.
pub fn sound_complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
    max_states: Option<usize>,
) -> PrefixSoundnessResult {
    let chars: Vec<char> = input.chars().collect();
    let ctx = opt_ctx.unwrap_or_else(Context::new);

    // Collect prefixes to check
    let prefixes: Vec<(usize, String)> = (0..=chars.len())
        .map(|len| (len, chars[..len].iter().collect::<String>()))
        .filter(|(len, prefix)| *len == 0 || !prefix.trim().is_empty())
        .collect();

    // Check all prefixes in parallel — collect rich metadata for each prefix
    // (is_ok, opt_complete, opt_visited, time_us, states_explored, visited_count, beam_fallback)
    let results: Vec<(bool, Option<String>, Option<Vec<String>>, u128, Option<usize>, Option<usize>, Option<bool>)> = prefixes
        .iter()
        .map(|(len, prefix)| {
            let depth_budget = max_depth + (chars.len() - len);
            let start = std::time::Instant::now();
            let result = complete(
                grammar,
                prefix,
                depth_budget,
                Some(ctx.clone()),
                max_states,
            );
            let dur_us = start.elapsed().as_micros();

            match result {
                CompletionResult::Success { complete_input, beam_fallback, .. } => {
                    (true, Some(complete_input), None, dur_us, None, None, Some(beam_fallback))
                }
                CompletionResult::Failure { visited_states, states_explored, beam_fallback, .. } => {
                    let visited_count = visited_states.len();
                    (false, None, Some(visited_states.clone()), dur_us, Some(states_explored), Some(visited_count), Some(beam_fallback))
                }
                CompletionResult::StateOverflow(limit) => {
                    (false, None, None, dur_us, Some(limit), None, None)
                }
                CompletionResult::Invalid(_) | CompletionResult::Error(_) | CompletionResult::Inconsistency(_) => {
                    (false, None, None, dur_us, None, None, None)
                }
            }
        })
        .collect();

    // Assemble results sequentially and build prefix metadata
    let mut prefix_details = Vec::with_capacity(prefixes.len());
    let mut prefix_meta: Vec<PrefixDetail> = Vec::with_capacity(prefixes.len());
    let mut failing_prefix = None;
    let mut failing_prefix_visited_states = None;
    let mut complete_string = None;

    for ((_, prefix), (is_ok, opt_complete, opt_visited, time_us, opt_states, opt_visited_count, opt_bf)) in
        prefixes.into_iter().zip(results.into_iter())
    {
        prefix_details.push((prefix.clone(), is_ok));

        // collect overall complete string (first observed)
        if is_ok {
            if let Some(s) = opt_complete {
                if complete_string.is_none() {
                    complete_string = Some(s);
                }
            }
        }

        // first failing prefix report
        if !is_ok && failing_prefix.is_none() {
            failing_prefix = Some(prefix.clone());
            failing_prefix_visited_states = opt_visited.clone();
        }

        // sample visited states (truncate to 20)
        let visited_sample = opt_visited.clone().map_or(vec![], |v| v.into_iter().take(20).collect());

        prefix_meta.push(PrefixDetail {
            prefix: prefix.clone(),
            ok: is_ok,
            time_us: time_us as u128,
            states_explored: opt_states,
            visited_count: opt_visited_count,
            visited_sample,
            beam_fallback: opt_bf,
        });
    }

    PrefixSoundnessResult {
        is_sound: failing_prefix.is_none(),
        failing_prefix,
        prefixes_checked: prefix_details.len(),
        prefix_details,
        complete_string,
        failing_prefix_visited_states,
        prefix_meta,
    }
}

// ============================================================================
// Test helper
// ============================================================================

/// Test helper: Check if a grammar and input combination is completable
pub fn is_completable(grammar: &Grammar, input: &str, max_depth: usize) -> bool {
    matches!(
        complete(grammar, input, max_depth, None, None),
        CompletionResult::Success { .. }
    )
}
