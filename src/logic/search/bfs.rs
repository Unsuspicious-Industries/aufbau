//! Simple BFS-based search that explores prefixes from shortest to longest.
//!
//! This approach naturally prefers shorter completions without complex scoring.

use std::collections::{HashSet, VecDeque};
use std::rc::Rc;

use crate::logic::completion::CompletionSet;
use crate::logic::fusion::{RuleRuntime, TypedParser, TypedPrefixError};
use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;

use super::candidate::{collect_seeds, gather, Ctx};
use super::score::score;
use super::{extend, CompletionResult, State};

/// BFS-based searcher that explores states level by level (by path length).
/// This guarantees shorter completions are found before longer ones.
pub struct BfsSearcher {
    pub grammar: Grammar,
    pub runtime: RuleRuntime,
    parser: TypedParser<RuleRuntime>,
    max_depth: usize,
    seeds: Vec<String>,
}

impl BfsSearcher {
    pub fn new(grammar: Grammar, max_depth: usize) -> Self {
        let runtime = RuleRuntime::new(grammar.clone());
        let parser = TypedParser::new(grammar.clone(), runtime.clone());
        Self {
            grammar: grammar.clone(),
            runtime,
            parser,
            max_depth: max_depth.max(1),
            seeds: collect_seeds(&grammar),
        }
    }

    pub fn complete(&mut self, input: &str, opt_ctx: Option<Context>) -> CompletionResult {
        let ctx = opt_ctx.unwrap_or_default();
        let ctx_id = self.runtime.intern_context(ctx.clone());

        match self.parse(input, ctx_id) {
            Ok(st) if st.view().is_complete() => st.verified_success().unwrap_or_else(|| {
                CompletionResult::Invalid(
                    "incremental completion check produced incomplete output".into(),
                )
            }),
            Ok(st) => {
                let mut visited = HashSet::from([(*st.input).clone()]);
                let mut explored = 0;

                if let Some(r) = self.bfs_search(&ctx, st, &mut visited, &mut explored) {
                    return r;
                }

                let mut states: Vec<_> = visited.into_iter().collect();
                states.sort();
                CompletionResult::Failure {
                    max_depth_reached: self.max_depth,
                    states_explored: explored,
                    visited_states: states,
                }
            }
            Err(err) if err.depth.hit_depth_limit => CompletionResult::Failure {
                max_depth_reached: err.depth.searched_depth as usize,
                states_explored: 0,
                visited_states: vec![],
            },
            Err(err) => CompletionResult::Invalid(err.to_string()),
        }
    }

    pub(crate) fn parse(
        &self,
        input: &str,
        ctx_id: crate::logic::fusion::CtxId,
    ) -> Result<State, TypedPrefixError> {
        use crate::logic::fusion::ast::FusionForest;

        const MIN_PARSE_DEPTH: u16 = 12;
        let parse_depth_cap = (self.max_depth as u16).max(MIN_PARSE_DEPTH);
        let mut depth = 4u16;

        loop {
            let mut parser = self.parser.fork().with_max_depth(depth);
            match parser.parse(input, ctx_id) {
                Ok(prefix) => {
                    let segments = self.grammar.tokenize(input).unwrap_or_default();
                    let s = score(
                        &FusionForest::new(parser.arena(), &segments, &prefix.roots, input),
                        &self.grammar,
                    );

                    // The parser found a valid prefix at `depth`, but future extensions
                    // will need more depth. Give the parser its full budget back.
                    let parser = parser.with_max_depth(parse_depth_cap);

                    return Ok(State {
                        base: s,
                        total: s,
                        input: Rc::new(input.to_string()),
                        path: Rc::new(vec![]),
                        ctx_id,
                        parser,
                        prefix,
                        segments: Rc::new(segments),
                    });
                }
                Err(err) => {
                    if depth >= parse_depth_cap {
                        return Err(err);
                    }
                    let mut next = ((depth as f64) * 1.5).ceil() as u16;
                    if next <= depth {
                        next = depth + 1;
                    }
                    depth = next.min(parse_depth_cap);
                }
            }
        }
    }

    /// BFS search: explore states level by level (by path length).
    fn bfs_search(
        &self,
        ctx: &Context,
        init: State,
        visited: &mut HashSet<String>,
        explored: &mut usize,
    ) -> Option<CompletionResult> {
        const MAX_EXPLORED: usize = 200;

        let mut queue: VecDeque<State> = VecDeque::new();
        queue.push_back(init);

        // Check if initial state is already complete
        if let Some(st) = queue.front() {
            if st.view().is_complete() {
                if let Some(ok) = st.verified_success() {
                    return Some(ok);
                }
            }
        }

        while let Some(st) = queue.pop_front() {
            if *explored >= MAX_EXPLORED {
                return None;
            }
            *explored += 1;

            println!("BFS exploring: '{}' path_len={}", *st.input, st.path.len());

            // Check completion
            if st.view().is_complete() {
                if let Some(ok) = st.verified_success() {
                    return Some(ok);
                }
            }

            // Don't expand beyond max depth
            if st.path.len() >= self.max_depth {
                println!(
                    "  -> skipping, path_len {} >= max_depth {}",
                    st.path.len(),
                    self.max_depth
                );
                continue;
            }

            // Gather and try all candidates
            let view = st.view();
            let tokens = CompletionSet::from_tokens(view.completions(&self.grammar));
            println!("  tokens: {:?}", tokens.iter().collect::<Vec<_>>());

            for tok in tokens.iter() {
                let candidates = gather(
                    tok,
                    &Ctx {
                        grammar: &self.grammar,
                        ast: &view,
                        ctx,
                        seeds: &self.seeds,
                    },
                );
                println!("  tok {:?} -> candidates: {:?}", tok, candidates);

                for cand in candidates {
                    let next_input = self.grammar.extend_input(&st.input, &cand);
                    println!("  -> trying candidate '{}' -> '{}'", cand, next_input);

                    // Skip if already visited
                    if visited.contains(&next_input) {
                        println!("     already visited");
                        continue;
                    }

                    // TODO(soundness): extend() failing on a candidate generated by gather()
                    // is a fundamental soundness violation. gather() guarantees syntactic validity,
                    // so if extend() fails, either the parser is broken/out-of-resources, or
                    // gather() is unsound. We shouldn't just ignore it.
                    // See SOUNDNESS_ISSUES.md for details.
                    let next_result = extend(&st, next_input.clone(), &self.grammar);
                    let Ok(next) = next_result else {
                        println!("     extend failed: {:?}", next_result.err());
                        continue;
                    };

                    // If complete, return immediately (BFS guarantees shortest)
                    if next.view().is_complete() {
                        println!("     COMPLETE!");
                        let next = next.extend_path(tok.clone());
                        if let Some(ok) = next.verified_success() {
                            return Some(ok);
                        }
                        println!("     but verified_success failed");
                        continue;
                    }

                    // Mark visited and enqueue
                    println!("     enqueueing");
                    visited.insert(next_input);
                    queue.push_back(next.extend_path(tok.clone()));
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn load(spec: &str) -> Grammar {
        Grammar::load(spec).unwrap()
    }

    #[test]
    fn bfs_finds_shortest_completion() {
        let grammar = load(
            r#"
            Name ::= /[a-z]+/
            Expr ::= Name | Name Name | Name Name Name
            Start ::= Expr
            "#,
        );

        let mut searcher = BfsSearcher::new(grammar, 4);
        let result = searcher.complete("", None);

        match result {
            CompletionResult::Success { complete_input, .. } => {
                // BFS should find shortest - just one name
                let tokens: Vec<_> = complete_input.split_whitespace().collect();
                assert_eq!(
                    tokens.len(),
                    1,
                    "BFS should find single-token completion first"
                );
            }
            other => panic!("expected success, got {:?}", other),
        }
    }

    #[test]
    fn bfs_right_recursive() {
        let grammar = load(
            r#"
            A ::= 'a' A | 'b'
            Start ::= A
            "#,
        );

        let mut searcher = BfsSearcher::new(grammar.clone(), 8);

        // Debug: check what completions are available from "a a a a"
        let ctx = Context::new();
        let ctx_id = searcher.runtime.intern_context(ctx.clone());
        let st = searcher.parse("a a a a", ctx_id).unwrap();
        let view = st.view();

        let tokens = view.completions(&grammar);
        println!("Available tokens from 'a a a a': {:?}", tokens);
        println!("Is complete: {:?}", view.is_complete());
        println!("Path len: {:?}", st.path.len());

        // From "a a a a", should find "a a a a b"
        let result = searcher.complete("a a a a", None);
        match result {
            CompletionResult::Success { complete_input, .. } => {
                assert_eq!(complete_input, "a a a a b");
            }
            other => panic!("expected success, got {:?}", other),
        }
    }

    #[test]
    fn bfs_prefers_shorter() {
        let grammar = load(
            r#"
            A ::= 'a' A | 'b'
            Start ::= A
            "#,
        );

        let mut searcher = BfsSearcher::new(grammar, 8);

        // From empty, should find "b" (shortest) not "a b" or "a a b"
        let result = searcher.complete("", None);
        match result {
            CompletionResult::Success { complete_input, .. } => {
                assert_eq!(
                    complete_input, "b",
                    "BFS should find 'b' as shortest completion"
                );
            }
            other => panic!("expected success, got {:?}", other),
        }
    }
}
