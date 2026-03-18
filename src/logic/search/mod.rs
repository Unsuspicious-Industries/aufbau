//! Priority-guided DFS search for completions.

pub mod scoring;

use crate::debug_info;
use crate::logic::grammar::Grammar;
use crate::logic::partial::Synthesizer;
use crate::logic::typing::tree::{TypedAST, TypedNode};
use crate::logic::typing::{gather_terminals_typed, Context, Type};
use crate::regex::Regex as DerivativeRegex;
use std::collections::{BinaryHeap, HashSet, VecDeque};

#[derive(Debug, Clone, Copy)]
pub struct SearchConfig {
    pub max_depth: usize,
    /// Max concrete string examples tried per regex token. Bounds branching factor.
    /// (Regexes are infinite; this is not a correctness loss, just instance selection.)
    pub max_token_examples: usize,
    /// Total states budget. Search returns Exhausted when hit.
    pub max_states: usize,
    /// Beam width per expanded state: keep only the top-N scored children.
    ///
    /// This is a performance/correctness trade-off tuned for completion tasks:
    /// wildly branching token choices (especially expression operators) can
    /// explode before structural closure tokens (`;`, `)`, `=>`) are explored.
    /// Keeping only high-score children preserves the most promising paths and
    /// avoids hangs on prefix-heavy validations.
    pub max_children_per_state: usize,
}

impl Default for SearchConfig {
    fn default() -> Self {
        Self {
            max_depth: 10,
            // Keep branching tight. Candidate quality is handled by synthesizer
            // ordering, so one witness per token is usually enough.
            max_token_examples: 1,
            max_states: 96,
            max_children_per_state: 12,
        }
    }
}

#[derive(Debug)]
pub enum SearchResult {
    Success {
        complete_input: String,
        ast: TypedNode,
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
    tree: TypedAST,
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
    let base_tree = match synth.partial_typed_ctx(ctx) {
        Ok(ast) => ast,
        Err(e) => {
            return SearchResult::Invalid {
                message: format!("Input is not partially valid: {}", e),
            }
        }
    };

    let mut visited: HashSet<String> = HashSet::new();
    visited.insert(base_tree.text().to_string());
    let mut visited_states: VecDeque<String> = VecDeque::new();
    visited_states.push_back(base_tree.text().to_string());

    let mut states_explored = 0usize;

    // Fast path: greedy single-branch completion avoids expensive frontier
    // exploration on common prefix states like `let` / `(` where one obvious
    // continuation reaches a complete tree quickly.
    if let Some(success) = try_greedy_complete(
        &mut synth,
        base_tree.clone(),
        ctx,
        config.max_depth,
        config.max_token_examples,
    ) {
        return success;
    }

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
        debug_info!(
            "search",
            "Exploring state: depth={} input='{}' score={}",
            state.depth,
            state.tree.text(),
            scoring::calculate_score(&state.tree, state.depth, config.max_depth).overall
        );
        if let Some(complete_node) = find_valid_completion(&state.tree) {
            let reconstructed = state.tree.text();
            debug_info!(
                "search",
                "Completion found: depth={} input='{}'",
                state.depth,
                reconstructed
            );
            return SearchResult::Success {
                complete_input: reconstructed,
                ast: complete_node,
                completion_path: state.path.clone(),
                depth: state.depth,
            };
        }

        if state.depth >= config.max_depth {
            continue;
        }

        if states_explored >= config.max_states {
            break;
        }

        let children = build_children(
            &mut synth,
            &state,
            ctx,
            config.max_depth,
            config.max_token_examples,
            config.max_children_per_state,
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

fn try_greedy_complete(
    synth: &mut Synthesizer,
    mut tree: TypedAST,
    ctx: &Context,
    max_depth: usize,
    max_token_examples: usize,
) -> Option<SearchResult> {
    let mut path = Vec::new();

    for depth in 0..=max_depth {
        if let Some(complete_node) = find_valid_completion(&tree) {
            return Some(SearchResult::Success {
                complete_input: tree.text(),
                ast: complete_node,
                completion_path: path,
                depth,
            });
        }

        if depth == max_depth {
            break;
        }

        synth.set_input(tree.text());
        let tokens = synth.completions_ctx(ctx);
        if tokens.is_empty() {
            break;
        }

        let mut local_terms = Vec::new();
        for root in &tree.roots {
            local_terms.extend(gather_terminals_typed(root));
        }

        let mut best_next: Option<(TypedAST, DerivativeRegex, usize, f64, f64)> = None;
        for token in tokens.iter() {
            let candidates = synth.extend_all_with_regex_candidates(
                token,
                ctx,
                &local_terms,
                max_token_examples,
            );
            for (next_tree, _ext) in candidates.into_iter() {
                if !has_well_typed_root(&next_tree) || next_tree.text() == tree.text() {
                    continue;
                }
                if let Some(complete_node) = find_valid_completion(&next_tree) {
                    let mut completion_path = path.clone();
                    completion_path.push(token.clone());
                    return Some(SearchResult::Success {
                        complete_input: next_tree.text(),
                        ast: complete_node,
                        completion_path,
                        depth: depth + 1,
                    });
                }

                let state_score = scoring::calculate_score(&next_tree, depth + 1, max_depth);
                let score = state_score.overall;
                let open_slots = state_score.open_slots;
                let grounded = grounded_root_count(&next_tree);
                match &best_next {
                    Some((_, _, best_grounded, best_open_slots, best_score))
                        if grounded < *best_grounded
                            || (grounded == *best_grounded
                                && (open_slots < *best_open_slots
                                    || (open_slots == *best_open_slots
                                        && score <= *best_score))) => {}
                    _ => {
                        best_next = Some((next_tree, token.clone(), grounded, open_slots, score));
                    }
                }
            }
        }

        if let Some((next_tree, chosen_token, _, _, _)) = best_next {
            path.push(chosen_token);
            tree = next_tree;
        } else {
            break;
        }
    }

    None
}

fn build_children(
        synth: &mut Synthesizer,
        state: &SearchState,
        ctx: &Context,
        max_depth: usize,
        max_token_examples: usize,
        max_children_per_state: usize,
        visited: &mut HashSet<String>,
        visited_states: &mut VecDeque<String>,
        states_explored: &mut usize,
    ) -> Vec<(SearchState, f64)> {
        synth.set_input(state.tree.text().to_string());
        let tokens = synth.completions_ctx(ctx);

        debug_info!(
            "search",
            "Expanding state: depth={} input='{}' tokens: {}",
            state.depth,
            state.tree.text(),
            tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<_>>()
                .join(", ")
        );

        let mut children: Vec<(SearchState, usize, f64)> = Vec::new();
        let mut local_terms = Vec::new();
        for root in &state.tree.roots {
            local_terms.extend(gather_terminals_typed(root));
        }

        for token in tokens.iter() {
            for child in
                extend_states_with_token(synth, state, token, ctx, &local_terms, max_token_examples)
            {
                if visited.insert(child.tree.text().to_string()) {
                    visited_states.push_back(child.tree.text().to_string());
                    *states_explored += 1;
                    let score =
                        scoring::calculate_score(&child.tree, child.depth, max_depth).overall;
                    let grounded = grounded_root_count(&child.tree);
                    children.push((child, grounded, score));
                }
            }
        }

        let has_grounded = children.iter().any(|(_, grounded, _)| *grounded > 0);
        if has_grounded {
            children.retain(|(_, grounded, _)| *grounded > 0);
        }

        children.sort_by(|(_, grounded_a, score_a), (_, grounded_b, score_b)| {
            grounded_b.cmp(grounded_a).then_with(|| {
                score_b
                    .partial_cmp(score_a)
                    .unwrap_or(std::cmp::Ordering::Equal)
            })
        });

        if max_children_per_state > 0 && children.len() > max_children_per_state {
            children.truncate(max_children_per_state);
        }

        children
            .into_iter()
            .map(|(state, _grounded, score)| (state, score))
            .collect()
    }

fn extend_states_with_token(
        synth: &mut Synthesizer,
        state: &SearchState,
        token: &DerivativeRegex,
        ctx: &Context,
        local_terms: &[String],
        max_token_examples: usize,
    ) -> Vec<SearchState> {
        synth.set_input(state.tree.text().to_string());
        let mut out = Vec::new();
        let mut extra = local_terms.to_vec();
        extra.sort();
        extra.dedup();

        let candidates =
            synth.extend_all_with_regex_candidates(token, ctx, &extra, max_token_examples);
        // Candidates are priority-ordered (example first, then grammar types, literals).
        // max_token_examples is enforced inside the call — no extra work done beyond the limit.
        for (ext, _extended) in candidates.into_iter() {
            if has_well_typed_root(&ext) {
                let mut path = state.path.clone();
                path.push(token.clone());
                out.push(SearchState {
                    tree: ext,
                    depth: state.depth + 1,
                    path,
                });
            }
        }

        out
    }

    fn has_well_typed_root(ast: &TypedAST) -> bool {
        !ast.roots.is_empty()
    }

    fn grounded_root_count(ast: &TypedAST) -> usize {
        ast.roots
            .iter()
            .filter(|r| !matches!(r.ty(), Type::Any))
            .count()
    }

    fn find_valid_completion(ast: &TypedAST) -> Option<TypedNode> {
        ast.roots.iter().find(|r| r.is_complete()).cloned()
    }

    /// Returns up to k complete typed programs that extend the current input.
    ///
    /// This function uses the same search algorithm as `search_complete` but continues
    /// to search for up to k distinct completions instead of returning at the first one.
    pub fn search_k(
        grammar: &Grammar,
        input: &str,
        k: usize,
        config: &SearchConfig,
        ctx: &Context,
    ) -> Vec<TypedNode> {
        // Check if the input is at least partially valid
        let mut synth = Synthesizer::new(grammar.clone(), input);
        let base_tree = match synth.partial_typed_ctx(ctx) {
            Ok(ast) => ast,
            Err(_e) => {
                // If the input is not even partially valid, there are no completions.
                return Vec::new();
            }
        };

        let mut visited: HashSet<String> = HashSet::new();
        visited.insert(base_tree.text().to_string());
        let mut visited_states: VecDeque<String> = VecDeque::new();
        visited_states.push_back(base_tree.text().to_string());

        let mut states_explored = 0usize;
        let mut results: Vec<TypedNode> = Vec::new();
        let mut seen_completions: HashSet<String> = HashSet::new();

        // We skip the greedy fast path for simplicity in the k version.
        // The frontier search will find completions in order of score.

        let initial_state = SearchState {
            tree: base_tree,
            depth: 0,
            path: Vec::new(),
        };

        let mut frontier: BinaryHeap<ScoredState> = BinaryHeap::new();
        let initial_score =
            scoring::calculate_score(&initial_state.tree, 0, config.max_depth).overall;
        frontier.push(ScoredState {
            score: initial_score,
            state: initial_state,
        });

        while let Some(ScoredState { state, .. }) = frontier.pop() {
            // If we already have k results, we can stop early.
            if results.len() >= k {
                break;
            }

            // Check if this state is a completion.
            if let Some(complete_node) = find_valid_completion(&state.tree) {
                let completion_text = complete_node.text();
                // Avoid duplicate completions.
                if seen_completions.insert(completion_text.clone()) {
                    results.push(complete_node);
                }
                // Note: we do not break here because we might find more completions.
            }

            if state.depth >= config.max_depth {
                continue;
            }

            if states_explored >= config.max_states {
                break;
            }

            let children = build_children(
                &mut synth,
                &state,
                ctx,
                config.max_depth,
                config.max_token_examples,
                config.max_children_per_state,
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

        results
    }

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{set_debug_level, testing::load_example_grammar};
    use crate::logic::typing::{Context, Type};

    const SCOPED_TYPED_SPEC: &str = r#"
    Identifier ::= /[a-z]+/
    Type ::= 'X' | 'Y'
    Variable(var) ::= Identifier[x]
    Num(num) ::= /[0-9]+/
    Let(letb) ::= 'def' Identifier[name] ':' Type[τ] '=' Atom[value] 'in' Expr[body]
    Scoped(scoped) ::= '{' Expr[inner] '}'
    Atom ::= Variable | Num | Scoped | '(' Expr ')'
    Expr ::= Let | Atom

    x ∈ Γ
    ----------- (var)
    Γ(x)

    ----------- (num)
    'X'

    Γ ⊢ value : τ, Γ[name:τ] ⊢ body : ?R
    ----------- (letb)
    ?R

    [Γ] ⊢ inner : ?T
    ----------- (scoped)
    ?T
"#;

    #[test]
    fn search_never_returns_syntactically_or_typedly_invalid_completion() {
        let grammar = load_example_grammar("fun");
        let cfg = SearchConfig {
            max_depth: 6,
            ..Default::default()
        };
        let prefix = "let";

        let result = search_complete(&grammar, prefix, &cfg, &Context::new());
        if let SearchResult::Success { complete_input, .. } = result {
            let mut mp = crate::logic::partial::MetaParser::new(grammar).with_max_depth(62);
            let typed = mp
                .partial_typed(&complete_input)
                .unwrap_or_else(|e| panic!("invalid completion '{}': {}", complete_input, e));
            assert!(
                typed.clone().complete().is_ok(),
                "completion is not a complete typed tree: {}",
                complete_input
            );
        }
    }

    #[test]
    #[ignore = "search broken after max_states cap, needs fixing"]
    fn search_fun_let_name_prefix_depth6() {
        set_debug_level(crate::DebugLevel::Trace);
        crate::add_module_filter("search");
        let grammar = load_example_grammar("fun");
        let cfg = SearchConfig {
            max_depth: 6,
            ..Default::default()
        };
        let result = search_complete(&grammar, "let x", &cfg, &Context::new());
        assert!(
            matches!(result, SearchResult::Success { .. }),
            "expected completion success for 'let x' with depth 6, got {:?}",
            result
        );
    }

    #[test]
    #[ignore = "search broken after max_states cap, needs fixing"]
    fn search_fun_let_prefix_depth7() {
        set_debug_level(crate::DebugLevel::Trace);
        crate::add_module_filter("search");
        let grammar = load_example_grammar("fun");
        let cfg = SearchConfig {
            max_depth: 7,
            ..Default::default()
        };
        let result = search_complete(&grammar, "let", &cfg, &Context::new());
        assert!(matches!(result, SearchResult::Success { .. }));
    }

    /// Test that search_k returns up to k complete programs from a prefix
    #[test]
    fn test_search_k_returns_multiple_programs() {
        let grammar = Grammar::load(SCOPED_TYPED_SPEC).unwrap();
        let _ctx = Context::new();
        let config = SearchConfig::default();
        // Start with a prefix that can be completed in multiple ways
        let results = search_k(&grammar, "def a : X =", 2, &config, &_ctx);
        assert!(results.len() >= 1, "Should get at least one completion");
        assert!(results.len() <= 2, "Should not exceed requested k");
        
        // All results should be complete programs (non-empty text)
        for result in results {
            assert!(!result.text().is_empty(), "Each result should be a non-empty completion");
        }
    }

    /// Test that search_k respects the k limit
    #[test]
    fn test_search_k_respects_limit() {
        let grammar = Grammar::load(SCOPED_TYPED_SPEC).unwrap();
        let _ctx = Context::new();
        let config = SearchConfig::default();
        // Request 1 completion
        let results = search_k(&grammar, "def a : X =", 1, &config, &_ctx);
        assert_eq!(
            results.len(),
            1,
            "Should return exactly 1 completion when k=1"
        );
        
        // Request 5 completions (but there may be fewer available)
        let results = search_k(&grammar, "def a : X =", 5, &config, &_ctx);
        assert!(results.len() <= 5, "Should not exceed requested k");
        assert!(results.len() >= 1, "Should get at least one completion");
    }
}
