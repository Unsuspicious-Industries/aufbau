use crate::logic::completion::CompletionSet;
use crate::logic::error::PrefixError;
use crate::logic::grammar::Grammar;
use crate::logic::parse::{CtxId, TypedParser};
use crate::logic::structure::ast::FusionForest;
use crate::logic::synth::atoms::gather_candidates;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::Context;
use crate::logic::typing::runtime::RuleRuntime;
use crate::regex::Regex;
use std::collections::{HashSet, VecDeque};
use std::rc::Rc;

#[derive(Debug)]
pub enum SearchResult {
    Success {
        complete_input: String,
        ast: crate::logic::structure::FusionAST,
        completion_path: Vec<Regex>,
        completion_depth: usize,
    },
    SuccessMultiple {
        completions: Vec<String>,
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

struct SearchState {
    input: Rc<String>,
    path: Rc<Vec<Regex>>,
    ctx_id: CtxId,
    parser: TypedParser<RuleRuntime>,
    ast: crate::logic::structure::ast::FusionAST,
    segments: Rc<Vec<crate::logic::grammar::Segment>>,
}

impl Clone for SearchState {
    fn clone(&self) -> Self {
        Self {
            input: Rc::clone(&self.input),
            path: Rc::clone(&self.path),
            ctx_id: self.ctx_id,
            parser: self.parser.fork(),
            ast: self.ast.clone(),
            segments: Rc::clone(&self.segments),
        }
    }
}

impl SearchState {
    fn view(&self) -> FusionForest<'_> {
        self.ast.view()
    }

    fn extend_path(&self, token: Regex) -> Self {
        let mut p = (*self.path).clone();
        p.push(token);
        Self {
            path: Rc::new(p),
            ..self.clone()
        }
    }

    fn verified_success(&self) -> Option<SearchResult> {
        let reparsed = {
            let mut parser = self.parser.fork();
            parser.parse(&self.input, self.ctx_id).ok()?
        };
        if !reparsed.is_complete() {
            return None;
        }
        Some(SearchResult::Success {
            complete_input: (*self.input).clone(),
            ast: reparsed,
            completion_depth: self.path.len(),
            completion_path: (*self.path).clone(),
        })
    }
}

fn parse_state(
    grammar: &mut Grammar,
    runtime: &RuleRuntime,
    input: &str,
    ctx_id: CtxId,
    max_depth: usize,
) -> Result<SearchState, PrefixError> {
    const MIN_PARSE_DEPTH: u16 = 12;
    let parse_depth_cap = (max_depth as u16).max(MIN_PARSE_DEPTH);
    let parser = TypedParser::new(grammar.clone(), runtime.clone());
    let mut depth = 4u16;

    loop {
        let mut p = parser.fork();
        match p.parse(input, ctx_id) {
            Ok(ast) => {
                let segments = grammar.tokenize(input).unwrap_or_default();
                return Ok(SearchState {
                    input: Rc::new(input.to_string()),
                    path: Rc::new(vec![]),
                    ctx_id,
                    parser: p,
                    ast,
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

fn extend_state(
    st: &SearchState,
    next_input: String,
    grammar: &mut Grammar,
    _max_depth: usize,
) -> Result<SearchState, PrefixError> {
    let mut parser = st.parser.fork();
    let ast = parser.parse(&next_input, st.ctx_id)?;
    let segments = grammar.tokenize(&next_input).unwrap_or_default();
    Ok(SearchState {
        input: Rc::new(next_input),
        path: Rc::clone(&st.path),
        ctx_id: st.ctx_id,
        parser,
        ast,
        segments: Rc::new(segments),
    })
}

fn candidate_inputs(grammar: &mut Grammar, st: &SearchState, candidate: &str) -> Vec<String> {
    let mut out = Vec::new();

    if let Some(last) = st.segments.last()
        && last.end == st.input.len()
        && candidate.starts_with(last.as_str())
        && candidate != last.as_str()
    {
        out.push(format!("{}{}", &st.input[..last.start], candidate));
    }

    let appended = crate::logic::grammar::extend::extend_input(grammar, &st.input, candidate);
    if !out.iter().any(|existing| existing == &appended) {
        out.push(appended);
    }

    out
}

fn max_explored_multi(max_depth: usize) -> usize {
    200usize.saturating_add(250usize.saturating_mul(max_depth.max(1)))
}

fn child_rank(st: &SearchState, grammar: &Grammar) -> (usize, usize, usize, usize, String) {
    let view = st.view();
    (
        usize::from(!view.is_complete()),
        view.min_open_slots(grammar),
        view.min_tree_depth(),
        st.path.len(),
        (*st.input).clone(),
    )
}

fn bfs_multi(
    grammar: &mut Grammar,
    init: SearchState,
    visited: &mut HashSet<String>,
    explored: &mut usize,
    max_depth: usize,
    k: usize,
    ctx: &Context,
    atoms: &[String],
) -> Option<SearchResult> {
    const MAX_AFTER_FIRST: usize = 48;
    let max_explored = max_explored_multi(max_depth);

    let mut queue: VecDeque<SearchState> = VecDeque::new();
    let mut completions = Vec::new();
    let mut seen_results = HashSet::new();

    queue.push_back(init);

    if let Some(st) = queue.front() {
        if let Some(SearchResult::Success { complete_input, .. }) = st.verified_success() {
            if seen_results.insert(complete_input.clone()) {
                completions.push(complete_input);
                if completions.len() >= k {
                    return Some(SearchResult::SuccessMultiple { completions });
                }
            }
        }
    }

    while let Some(st) = queue.pop_front() {
        if *explored >= max_explored {
            break;
        }
        *explored += 1;

        if !completions.is_empty() && *explored >= MAX_AFTER_FIRST {
            break;
        }

        if st.view().is_complete() {
            if let Some(SearchResult::Success { complete_input, .. }) = st.verified_success() {
                if seen_results.insert(complete_input.clone()) {
                    completions.push(complete_input);
                    if completions.len() >= k {
                        return Some(if k == 1 {
                            st.verified_success()?
                        } else {
                            SearchResult::SuccessMultiple { completions }
                        });
                    }
                }
            }
        }

        if st.path.len() >= max_depth {
            continue;
        }

        let view = st.view();
        let tokens = CompletionSet::from_tokens(view.completions(grammar));

        let mut next_states = Vec::new();

        for tok in tokens.iter() {
            let candidates = gather_candidates(
                tok,
                grammar,
                view.bound_texts(view.segs()),
                view.completions(grammar),
                ctx,
                atoms,
            );

            for cand in candidates {
                for next_input in candidate_inputs(grammar, &st, &cand) {
                    if visited.contains(&next_input) {
                        continue;
                    }

                    let next = match extend_state(&st, next_input.clone(), grammar, max_depth) {
                        Ok(n) => n,
                        Err(_) => continue,
                    };

                    if next.view().is_complete() {
                        let next = next.extend_path(tok.clone());
                        if let Some(SearchResult::Success { complete_input, .. }) =
                            next.verified_success()
                        {
                            if seen_results.insert(complete_input.clone()) {
                                completions.push(complete_input);
                                if completions.len() >= k {
                                    return Some(if k == 1 {
                                        next.verified_success()?
                                    } else {
                                        SearchResult::SuccessMultiple { completions }
                                    });
                                }
                            }
                        }
                        continue;
                    }

                    visited.insert(next_input);
                    next_states.push(next.extend_path(tok.clone()));
                }
            }
        }

        next_states.sort_by_key(|next| child_rank(next, grammar));
        queue.extend(next_states);
    }

    if !completions.is_empty() {
        Some(SearchResult::SuccessMultiple { completions })
    } else {
        None
    }
}

pub fn search(
    synth: &mut Synthesizer,
    input: &str,
    ctx: &Context,
    max_depth: usize,
) -> SearchResult {
    search_k(synth, input, ctx, max_depth, 1)
}

pub fn search_k(
    synth: &mut Synthesizer,
    input: &str,
    ctx: &Context,
    max_depth: usize,
    k: usize,
) -> SearchResult {
    let mut grammar = synth.grammar().clone();
    let runtime = synth.runtime().clone();
    let atoms = crate::logic::synth::collect_atoms(&grammar);
    let ctx_id = runtime.intern_context(ctx.clone());

    let init = match parse_state(&mut grammar, &runtime, input, ctx_id, max_depth) {
        Ok(st) => st,
        Err(err) => return SearchResult::Invalid(err.to_string()),
    };

    if init.view().is_complete() {
        if k == 1 {
            return init.verified_success().unwrap_or_else(|| {
                SearchResult::Invalid(
                    "incremental completion check produced incomplete output".into(),
                )
            });
        } else {
            let mut visited = HashSet::from([(*init.input).clone()]);
            let mut explored = 0;
            let mut completions = Vec::new();

            if let Some(SearchResult::Success { complete_input, .. }) = init.verified_success() {
                completions.push(complete_input);
            }

            if let Some(results) = bfs_multi(
                &mut grammar.clone(),
                init,
                &mut visited,
                &mut explored,
                max_depth,
                k,
                ctx,
                &atoms,
            ) {
                return results;
            }

            if !completions.is_empty() {
                return SearchResult::SuccessMultiple { completions };
            }

            let mut states: Vec<_> = visited.into_iter().collect();
            states.sort();
            return SearchResult::Failure {
                max_depth_reached: max_depth,
                states_explored: explored,
                visited_states: states,
            };
        }
    }

    let mut grammar_mut = grammar;
    let mut visited = HashSet::from([(*init.input).clone()]);
    let mut explored = 0;

    if let Some(r) = bfs_multi(
        &mut grammar_mut,
        init,
        &mut visited,
        &mut explored,
        max_depth,
        k,
        ctx,
        &atoms,
    ) {
        return r;
    }

    let mut states: Vec<_> = visited.into_iter().collect();
    states.sort();
    SearchResult::Failure {
        max_depth_reached: max_depth,
        states_explored: explored,
        visited_states: states,
    }
}
