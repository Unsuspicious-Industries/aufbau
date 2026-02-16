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

use crate::logic::grammar::Grammar;
use crate::logic::partial::NonTerminal;
use crate::logic::typing::Context;
use crate::logic::{beam_complete, BeamConfig, BeamResult};
use crate::regex::Regex as DerivativeRegex;

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
        completion_depth: usize,
    },
    /// No completion found within the search bounds
    Failure {
        max_depth_reached: usize,
        states_explored: usize,
        visited_states: Vec<String>,
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
                completion_depth: depth,
            };
        }
        BeamResult::Invalid { message } => {
            return CompletionResult::Invalid(message);
        }
        other => {
            // TODO: imrpvoe error hanlding
            return CompletionResult::Error(format!("Beam search failed: {:?}", other));
        }
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

    // Check all prefixes in parallel
    // Returns: (PrefixDetail, completion string if success, full visited_states if failure)
    let results: Vec<(usize, PrefixDetail, Option<String>, Option<Vec<String>>)> = prefixes
        .iter()
        .map(|(len, prefix)| {
            let depth_budget = max_depth + (chars.len() - len);
            let start = std::time::Instant::now();
            let result = complete(grammar, prefix, depth_budget, Some(ctx.clone()), max_states);
            let elapsed_us = start.elapsed().as_micros();

            match result {
                CompletionResult::Success { complete_input, .. } => {
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: true,
                        time_us: elapsed_us,
                        states_explored: None,
                        visited_count: None,
                        visited_sample: vec![],
                    };
                    (*len, detail, Some(complete_input), None)
                }
                CompletionResult::Failure {
                    visited_states,
                    states_explored,
                    ..
                } => {
                    let visited_sample = visited_states.iter().take(20).cloned().collect();
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: false,
                        time_us: elapsed_us,
                        states_explored: Some(states_explored),
                        visited_count: Some(visited_states.len()),
                        visited_sample,
                    };
                    (*len, detail, None, Some(visited_states))
                }
                CompletionResult::StateOverflow(limit) => {
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: false,
                        time_us: elapsed_us,
                        states_explored: Some(limit),
                        visited_count: None,
                        visited_sample: vec![],
                    };
                    (*len, detail, None, None)
                }
                CompletionResult::Invalid(_)
                | CompletionResult::Error(_)
                | CompletionResult::Inconsistency(_) => {
                    let detail = PrefixDetail {
                        prefix: prefix.clone(),
                        ok: false,
                        time_us: elapsed_us,
                        states_explored: None,
                        visited_count: None,
                        visited_sample: vec![],
                    };
                    (*len, detail, None, None)
                }
            }
        })
        .collect();

    // Process results in a single loop
    let mut prefix_details = Vec::with_capacity(results.len());
    let mut prefix_meta = Vec::with_capacity(results.len());
    let mut failing_prefix = None;
    let mut failing_prefix_visited_states = None;
    let mut complete_string = None;

    let mut full_completion = None;
    for (len, detail, completion, visited_states) in results {
        prefix_details.push((detail.prefix.clone(), detail.ok));

        if len == chars.len() && completion.is_some() {
            full_completion = completion.clone();
        }

        if detail.ok && complete_string.is_none() {
            complete_string = completion;
        }

        if !detail.ok && failing_prefix.is_none() {
            failing_prefix = Some(detail.prefix.clone());
            failing_prefix_visited_states = visited_states;
        }

        prefix_meta.push(detail);
    }

    let complete_string = full_completion.or(complete_string);

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
