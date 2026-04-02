use crate::logic::fusion::ast::FusionForest;
use crate::logic::fusion::{
    FusionAST, RuleRuntime, TypedParser, TypedPrefixError, TypedPrefixState,
};
use crate::logic::grammar::{Grammar, Segment};
use crate::logic::typing::Context;
use crate::regex::Regex;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::time::Instant;

mod candidate;
mod distance;
mod score;

#[cfg(test)]
mod tests;

use candidate::{Composite, Ctx, Strategy, collect_seeds};
use score::{Total, rerank, score};

#[derive(Debug)]
pub enum CompletionResult {
    Success {
        complete_input: String,
        ast: FusionAST,
        completion_path: Vec<Regex>,
        completion_depth: usize,
    },
    Failure {
        max_depth_reached: usize,
        states_explored: usize,
        visited_states: Vec<String>,
    },
    Invalid(String),
    Inconsistency(String),
    Error(String),
}

pub(crate) struct State {
    base: Total,
    total: Total,
    input: Rc<String>,
    path: Rc<Vec<Regex>>,
    ctx_id: crate::logic::fusion::CtxId,
    parser: TypedParser<RuleRuntime>,
    prefix: TypedPrefixState,
    segments: Rc<Vec<Segment>>,
}

impl Clone for State {
    fn clone(&self) -> Self {
        Self {
            base: self.base,
            total: self.total,
            input: Rc::clone(&self.input),
            path: Rc::clone(&self.path),
            ctx_id: self.ctx_id,
            parser: self.parser.fork(),
            prefix: self.prefix.clone(),
            segments: Rc::clone(&self.segments),
        }
    }
}

impl State {
    fn view(&self) -> FusionForest<'_> {
        FusionForest::new(
            self.parser.arena(),
            &self.segments,
            &self.prefix.roots,
            &self.input,
        )
    }

    fn materialize(&self) -> FusionAST {
        self.parser.materialize(
            &self.prefix.roots,
            (*self.segments).clone(),
            (*self.input).clone(),
        )
    }

    fn extend_path(&self, token: Regex) -> Self {
        let mut p = (*self.path).clone();
        p.push(token);
        Self {
            path: Rc::new(p),
            ..self.clone()
        }
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        *self.input == *other.input
    }
}

impl Eq for State {}

pub struct Searcher {
    pub grammar: Grammar,
    pub runtime: RuleRuntime,
    parser: TypedParser<RuleRuntime>,
    start_depth: u16,
    max_depth: u16,
    depth_factor: f64,
    seeds: Vec<String>,
}

impl Searcher {
    pub fn new(grammar: Grammar, max_depth: usize) -> Self {
        let runtime = RuleRuntime::new(grammar.clone());
        let parser = TypedParser::new(grammar.clone(), runtime.clone());
        Self {
            grammar: grammar.clone(),
            runtime,
            parser,
            start_depth: 4,
            max_depth: max_depth.max(1) as u16,
            depth_factor: 1.5,
            seeds: collect_seeds(&grammar),
        }
    }

    pub fn complete(&mut self, input: &str, opt_ctx: Option<Context>) -> CompletionResult {
        let ctx = opt_ctx.unwrap_or_default();
        let ctx_id = self.runtime.intern_context(ctx.clone());

        match self.parse(input, ctx_id) {
            Ok(st) if st.view().is_complete() => CompletionResult::Success {
                complete_input: input.to_string(),
                ast: st.materialize(),
                completion_path: vec![],
                completion_depth: 0,
            },
            Ok(st) => {
                let mut visited = HashSet::from([(*st.input).clone()]);
                let mut explored = 0;

                if let Some(r) = self.search(&ctx, st, &mut visited, &mut explored) {
                    return r;
                }

                let mut states: Vec<_> = visited.into_iter().collect();
                states.sort();
                CompletionResult::Failure {
                    max_depth_reached: 0,
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
        // `max_depth` is a completion-search budget (how many tokens we may append),
        // not a strict bound on parsing the already-present prefix.
        // Keep a structural parse baseline so complete/near-complete inputs remain
        // parseable even when completion budget is small (e.g. depth=1).
        const MIN_PARSE_DEPTH: u16 = 12;
        let parse_depth_cap = self.max_depth.max(MIN_PARSE_DEPTH);
        let mut depth = self.start_depth;

        loop {
            let mut parser = self.parser.fork().with_max_depth(depth);
            match parser.parse(input, ctx_id) {
                Ok(prefix) => {
                    let segments = self.grammar.tokenize(input).unwrap_or_default();
                    let s = score(
                        &FusionForest::new(parser.arena(), &segments, &prefix.roots, input),
                        &self.grammar,
                    );
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
                    let mut next = ((depth as f64) * self.depth_factor).ceil() as u16;
                    if next <= depth {
                        next = depth + 1;
                    }
                    depth = next.min(parse_depth_cap);
                }
            }
        }
    }

    fn search(
        &self,
        ctx: &Context,
        init: State,
        visited: &mut HashSet<String>,
        explored: &mut usize,
    ) -> Option<CompletionResult> {
        const MAX: usize = 200;
        let mut pq = Vec::new();
        let mut popped = HashSet::new();
        let mut canon = HashMap::<(bool, usize, usize, usize, usize, usize, u64), usize>::new();
        let strategy = Composite::default();

        pq.push(init);

        // Accept a syntactically complete starting state immediately.
        if let Some(st) = pq.last()
            && st.view().is_complete()
        {
            return Some(CompletionResult::Success {
                complete_input: (*st.input).clone(),
                ast: st.materialize(),
                completion_depth: st.path.len(),
                completion_path: (*st.path).clone(),
            });
        }

        while let Some(st) = best(&mut pq, &popped) {
            if !popped.insert((*st.input).clone()) {
                continue;
            }
            if *explored >= MAX {
                return None;
            }
            *explored += 1;

            if st.view().is_complete() {
                return Some(CompletionResult::Success {
                    complete_input: (*st.input).clone(),
                    ast: st.materialize(),
                    completion_depth: st.path.len(),
                    completion_path: (*st.path).clone(),
                });
            }

            if st.path.len() >= self.max_depth as usize {
                continue;
            }

            let view = st.view();
            let tokens = view.completions(&self.grammar);
            let mut local = HashSet::new();

            let cctx = Ctx {
                grammar: &self.grammar,
                ast: &view,
                ctx,
                seeds: &self.seeds,
            };

            for tok in tokens.iter() {
                for cand in strategy.gather(tok, &cctx) {
                    if st.path.len() + 1 > self.max_depth as usize {
                        continue;
                    }
                    let next_input = self.grammar.extend_input(&st.input, &cand);
                    let Ok(next) = extend(&st, next_input, &self.grammar) else {
                        continue;
                    };

                    if next.view().is_complete() {
                        return Some(CompletionResult::Success {
                            complete_input: (*next.input).clone(),
                            ast: next.materialize(),
                            completion_depth: next.path.len() + 1,
                            completion_path: (*next.extend_path(tok.clone()).path).clone(),
                        });
                    }

                    let next_str = (*next.input).clone();
                    if !local.insert(next_str.clone()) || visited.contains(&next_str) {
                        continue;
                    }

                    let sig = signature(&next, &self.grammar);
                    let len = next_str.len();
                    if canon.get(&sig).is_some_and(|best| *best >= len) {
                        continue;
                    }
                    canon.insert(sig, len);
                    visited.insert(next_str);

                    pq.push(next.extend_path(tok.clone()));
                }
            }
        }

        None
    }
}

pub fn complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
) -> CompletionResult {
    let mut s = Searcher::new(grammar.clone(), max_depth);
    let mut r = s.complete(input, opt_ctx);
    if let CompletionResult::Failure {
        max_depth_reached, ..
    } = &mut r
    {
        *max_depth_reached = max_depth;
    }
    r
}

fn best(pq: &mut Vec<State>, popped: &HashSet<String>) -> Option<State> {
    if pq.is_empty() {
        return None;
    }

    pq.iter_mut()
        .for_each(|st| st.total = rerank(st.base, &st.input, popped));

    let idx = pq
        .iter()
        .enumerate()
        .max_by(|(_, l), (_, r)| l.total.cmp(&r.total))
        .map(|(i, _)| i)?;

    Some(pq.swap_remove(idx))
}

fn signature(st: &State, grammar: &Grammar) -> (bool, usize, usize, usize, usize, usize, u64) {
    let v = st.view();
    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    for token in v.completions(grammar).into_iter().take(8) {
        token.to_pattern().hash(&mut hasher);
        token.example().hash(&mut hasher);
    }
    (
        v.is_complete(),
        v.min_open_slots(grammar),
        v.min_tree_depth(),
        v.len(),
        v.leaf_terminal_count(),
        st.path.len(),
        hasher.finish(),
    )
}

pub(crate) fn extend(
    st: &State,
    next_input: String,
    grammar: &Grammar,
) -> Result<State, TypedPrefixError> {
    let mut parser = st.parser.fork();
    let start = Instant::now();
    let prefix = match parser.advance(&st.prefix, &next_input, st.ctx_id) {
        Ok(prefix) => prefix,
        Err(inc_err) => {
            let mut fresh = st.parser.fork();
            match fresh.parse(&next_input, st.ctx_id) {
                Ok(prefix) => {
                    parser = fresh;
                    prefix
                }
                Err(_) => return Err(inc_err),
            }
        }
    };
    crate::debug_debug!(
        "completion_perf",
        "search_extend: input='{}' next='{}' roots={} nodes={} elapsed_us={}",
        *st.input,
        next_input,
        prefix.roots.len(),
        parser.arena().node_count(),
        start.elapsed().as_micros()
    );
    let segments = grammar.tokenize(&next_input).unwrap_or_default();
    let s = score(
        &FusionForest::new(parser.arena(), &segments, &prefix.roots, &next_input),
        grammar,
    );
    Ok(State {
        base: s,
        total: s,
        input: Rc::new(next_input),
        path: Rc::clone(&st.path),
        ctx_id: st.ctx_id,
        parser,
        prefix,
        segments: Rc::new(segments),
    })
}
