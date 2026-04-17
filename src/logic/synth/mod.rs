use crate::debug_debug;
use crate::debug_trace;
use crate::logic::completion::CompletionSet;
use crate::logic::grammar::Grammar;
use crate::logic::parse::{CtxId, TypedParser};
use crate::logic::structure::ast::FusionAST;
use crate::logic::typing::Context;
use crate::regex::Regex;
use std::time::Instant;

use crate::logic::typing::runtime::RuleRuntime;

pub mod atoms;
pub mod search;
#[cfg(test)]
mod tests;

pub use atoms::collect_atoms;
use atoms::gather_candidates;
pub use search::{search, search_k, SearchResult};

#[cfg(test)]
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) struct SynthStats {
    pub full_parses: usize,
    pub incremental_advances: usize,
}

pub struct Synthesizer {
    grammar: Grammar,
    runtime: RuleRuntime,
    parser: TypedParser<RuleRuntime>,

    input: String,
    tree: Option<FusionAST>,

    #[cfg(test)]
    stats: SynthStats,
}

impl Synthesizer {
    pub fn new(grammar: Grammar, input: impl Into<String>) -> Self {
        let input = input.into();
        debug_trace!("synth", "new: input='{}'", input);
        let runtime = RuleRuntime::new(grammar.clone());
        let parser = TypedParser::new(grammar.clone(), runtime.clone());

        Self {
            grammar,
            runtime,
            parser,
            input,
            tree: None,
            #[cfg(test)]
            stats: SynthStats::default(),
        }
    }

    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    pub fn runtime(&self) -> &RuleRuntime {
        &self.runtime
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn ast(&self) -> Option<&FusionAST> {
        self.tree.as_ref()
    }

    pub fn parse_with(&mut self, ctx: &Context) -> Result<FusionAST, String> {
        debug_trace!("synth", "parse_with: input='{}'", self.input);
        let input = self.input.clone();
        let ctx_id = ctx_id(ctx, &self.runtime);
        let parsed = self.parse(input.clone(), ctx_id)?;
        self.input = input;
        self.tree = Some(parsed.clone());
        Ok(parsed)
    }

    pub fn completions(&mut self) -> CompletionSet {
        self.completions_with(&Context::new())
    }

    pub fn completions_with(&mut self, ctx: &Context) -> CompletionSet {
        debug_trace!("synth", "completions_with: input='{}'", self.input);
        match self.parse_with(ctx) {
            Ok(typed) => {
                debug_trace!(
                    "synth",
                    "completions_with: parsed successfully input='{}' ast={}",
                    self.input,
                    typed
                );
                let out_tokens = typed.completions(&self.grammar);
                debug_debug!(
                    "completion",
                    "completions: input='{}' tokens={}",
                    self.input,
                    out_tokens.len()
                );
                for token in out_tokens.iter().take(16) {
                    debug_debug!(
                        "completion",
                        "completions: token='{}' example={:?}",
                        token.to_pattern(),
                        token.example()
                    );
                }
                CompletionSet::from_tokens(out_tokens)
            }
            Err(e) => {
                self.tree = None;
                debug_debug!(
                    "completion",
                    "completions: failed input='{}' err='{}'",
                    self.input,
                    e
                );
                CompletionSet::empty()
            }
        }
    }

    pub fn feed(&mut self, token: &str, ctx: &Context) -> Result<FusionAST, String> {
        debug_trace!("synth", "feed: input='{}' token='{}'", self.input, token);
        let extended =
            crate::logic::grammar::extend::extend_input(&mut self.grammar, &self.input, token);
        let ctx_id = ctx_id(ctx, &self.runtime);

        let parsed = self.parse(extended.clone(), ctx_id)?;
        self.input = extended;
        self.tree = Some(parsed.clone());
        Ok(parsed)
    }

    pub fn extend_with_completion(
        &mut self,
        token: &Regex,
        ctx: &Context,
    ) -> Option<(FusionAST, String)> {
        for candidate in self.completions_candidates(token, ctx) {
            if let Ok(ast) = self.feed(&candidate, ctx) {
                return Some((ast, self.input.clone()));
            }
        }
        None
    }

    pub fn completions_candidates(&self, token: &Regex, ctx: &Context) -> Vec<String> {
        let bound_texts = self.ast().map(|t| t.bound_texts()).unwrap_or_default();
        let other_completions = self
            .ast()
            .map(|t| t.completions(&self.grammar))
            .unwrap_or_default();
        let atoms = collect_atoms(&self.grammar);
        gather_candidates(
            token,
            &self.grammar,
            bound_texts,
            other_completions,
            ctx,
            &atoms,
        )
    }

    fn parse(
        &mut self,
        input: String,
        ctx_id: CtxId,
    ) -> Result<FusionAST, String> {
        let start = Instant::now();
        match self.parser.parse(&input, ctx_id) {
            Ok(ast) => {
                debug_debug!(
                    "completion_perf",
                    "synth_full_parse: input='{}' roots={} nodes={} elapsed_us={}",
                    input,
                    ast.len(),
                    self.parser.arena().node_count(),
                    start.elapsed().as_micros()
                );
                #[cfg(test)]
                {
                    self.stats.full_parses += 1;
                }
                Ok(ast)
            }
            Err(err) => {
                debug_debug!(
                    "completion_perf",
                    "synth_full_parse: input='{}' elapsed_us={} err='{}'",
                    input,
                    start.elapsed().as_micros(),
                    err
                );
                Err(err.to_string())
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn stats(&self) -> SynthStats {
        self.stats
    }
}

/// Complete a prefix into one typed expression using the synth search engine.
pub fn complete(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    opt_ctx: Option<Context>,
) -> SearchResult {
    let ctx = opt_ctx.unwrap_or_default();
    let mut synth = Synthesizer::new(grammar.clone(), input);
    search(&mut synth, input, &ctx, max_depth)
}

/// Return up to `count` completed strings from synth search.
pub fn complete_k(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
    count: usize,
    opt_ctx: Option<Context>,
) -> Vec<String> {
    let ctx = opt_ctx.unwrap_or_default();
    let mut synth = Synthesizer::new(grammar.clone(), input);
    match search_k(&mut synth, input, &ctx, max_depth, count.max(1)) {
        SearchResult::Success { complete_input, .. } => vec![complete_input],
        SearchResult::SuccessMultiple { completions } => completions,
        SearchResult::Failure { .. }
        | SearchResult::Invalid(_)
        | SearchResult::Inconsistency(_)
        | SearchResult::Error(_) => Vec::new(),
    }
}

fn ctx_id(ctx: &Context, runtime: &RuleRuntime) -> CtxId {
    runtime.intern_context(ctx.clone())
}
