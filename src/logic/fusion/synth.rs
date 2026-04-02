use crate::debug_debug;
use crate::logic::grammar::Grammar;
use crate::logic::partial::completion::CompletionSet;
use crate::logic::typing::{Context, gather_raw_types};
use crate::regex::Regex as DerivativeRegex;
use std::collections::{HashMap, HashSet};
use std::time::Instant;

use super::ast::FusionAST;
use super::runtime::RuleRuntime;
use super::{TypedParser, TypedPrefixState};

struct ParsedState {
    parser: TypedParser<RuleRuntime>,
    prefix: TypedPrefixState,
    segments: Vec<crate::logic::grammar::Segment>,
    input: String,
    ctx_id: crate::logic::fusion::CtxId,
}

impl Clone for ParsedState {
    fn clone(&self) -> Self {
        Self {
            parser: self.parser.fork(),
            prefix: self.prefix.clone(),
            segments: self.segments.clone(),
            input: self.input.clone(),
            ctx_id: self.ctx_id,
        }
    }
}

impl ParsedState {
    fn ast(&self) -> FusionAST {
        self.parser.materialize(
            &self.prefix.roots,
            self.segments.clone(),
            self.input.clone(),
        )
    }
}

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
    start_depth: u16,
    max_depth: u16,
    depth_factor: f64,
    input: String,
    tree: Option<FusionAST>,
    regex_seed_candidates: Vec<String>,
    parse_cache: HashMap<(String, Context), Result<ParsedState, String>>,
    #[cfg(test)]
    stats: SynthStats,
}

impl Synthesizer {
    pub fn new(grammar: Grammar, input: impl Into<String>) -> Self {
        let runtime = RuleRuntime::new(grammar.clone());
        let parser = TypedParser::new(grammar.clone(), runtime.clone());
        let input = input.into();
        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);

        Self {
            grammar,
            runtime,
            parser,
            start_depth: 4,
            max_depth: 128,
            depth_factor: 1.5,
            input,
            tree: None,
            regex_seed_candidates,
            parse_cache: HashMap::new(),
            #[cfg(test)]
            stats: SynthStats::default(),
        }
    }

    pub fn new_with_max_depth(
        grammar: Grammar,
        input: impl Into<String>,
        max_depth: usize,
    ) -> Self {
        let runtime = RuleRuntime::new(grammar.clone());
        let parser = TypedParser::new(grammar.clone(), runtime.clone());
        let input = input.into();
        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);
        let max_depth = max_depth.max(1) as u16;

        Self {
            grammar,
            runtime,
            parser,
            start_depth: 4,
            max_depth,
            depth_factor: 1.5,
            input,
            tree: None,
            regex_seed_candidates,
            parse_cache: HashMap::new(),
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
        let input = self.input.clone();
        self.extended_typed_ctx(&input, ctx)
    }

    pub fn tokens(&mut self) -> CompletionSet {
        self.tokens_with(&Context::new())
    }

    pub fn tokens_with(&mut self, ctx: &Context) -> CompletionSet {
        let input = self.input.clone();
        match self.extended_typed_ctx(&input, ctx) {
            Ok(typed) => {
                self.tree = Some(typed.clone());
                // NOTE:
                // These completions are a *performance hint* and may include ill-typed tokens.
                // Any consumer that needs soundness must verify by actually appending via
                // `feed`.
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
        let extended = self.grammar.extend_input(&self.input, token);
        let current_input = self.input.clone();
        let current_key = (current_input.clone(), ctx.clone());
        let next_key = (extended.clone(), ctx.clone());

        if let Some(Ok(parsed)) = self.parse_cache.get(&next_key).cloned() {
            let typed = parsed.ast();
            self.input = extended;
            self.tree = Some(typed.clone());
            return Ok(typed);
        }

        let current = match self.parse_cache.remove(&current_key) {
            Some(Ok(parsed)) => Ok(parsed),
            Some(Err(err)) => Err(err),
            None => self.extended_state_ctx(&current_input, ctx),
        }?;

        let parsed = match self.advance_owned_state(current.clone(), &extended) {
            Ok(parsed) => parsed,
            Err(_inc_err) => {
                // Keep candidate constraints from completion generation,
                // but recover from incremental parser misses via a full parse.
                self.parse_fresh(&extended, ctx)?
            }
        };

        self.parse_cache.insert(next_key, Ok(parsed.clone()));
        let typed = parsed.ast();
        self.input = extended;
        self.tree = Some(typed.clone());
        Ok(typed)
    }

    pub fn extend_with_regex(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
    ) -> Option<(FusionAST, String)> {
        for candidate in self.regex_gather_candidates(token, ctx) {
            if let Ok(ast) = self.feed(&candidate, ctx) {
                return Some((ast, self.input.clone()));
            }
        }
        None
    }

    pub fn regex_gather_candidates(&self, token: &DerivativeRegex, ctx: &Context) -> Vec<String> {
        let mut candidates = Vec::new();
        let mut seen = HashSet::new();
        let token_example = token.example();

        let is_allowed = |candidate: &str| {
            !self.grammar.special_tokens.iter().any(|t| t == candidate)
                || token_example.as_deref() == Some(candidate)
        };

        let mut push_candidate = |candidate: String| {
            if is_allowed(&candidate) && token.matches(&candidate) && seen.insert(candidate.clone())
            {
                candidates.push(candidate);
            }
        };

        if let Some(example) = token.example() {
            push_candidate(example);
        }
        for name in ctx.bindings.keys() {
            push_candidate(name.clone());
        }
        if let Some(t) = self.ast() {
            for text in t.bound_texts() {
                push_candidate(text);
            }
            for terminal in t.completions(&self.grammar) {
                if let Some(text) = terminal.example() {
                    push_candidate(text);
                }
            }
        }

        for candidate in &self.regex_seed_candidates {
            push_candidate(candidate.clone());
        }

        candidates
    }

    fn extended_typed_ctx(&mut self, input: &str, ctx: &Context) -> Result<FusionAST, String> {
        self.extended_state_ctx(input, ctx)
            .map(|parsed| parsed.ast())
    }

    fn extended_state_ctx(&mut self, input: &str, ctx: &Context) -> Result<ParsedState, String> {
        let key = (input.to_string(), ctx.clone());
        if let Some(cached) = self.parse_cache.get(&key) {
            return cached.clone();
        }

        let parsed = self
            .parse_incrementally(input, ctx)
            .or_else(|_| self.parse_fresh(input, ctx));

        self.parse_cache.insert(key, parsed.clone());
        parsed
    }

    fn parse_incrementally(&mut self, input: &str, ctx: &Context) -> Result<ParsedState, String> {
        if input == self.input {
            return Err("incremental parse requires extended input".into());
        }

        let current_key = (self.input.clone(), ctx.clone());
        let Some(Ok(current)) = self.parse_cache.get(&current_key).cloned() else {
            return Err("missing cached prefix state".into());
        };

        self.advance_owned_state(current, input)
    }

    fn advance_owned_state(
        &mut self,
        mut current: ParsedState,
        input: &str,
    ) -> Result<ParsedState, String> {
        let start = Instant::now();
        let prefix = current
            .parser
            .advance(&current.prefix, input, current.ctx_id)
            .map_err(|err| err.to_string())?;
        debug_debug!(
            "completion_perf",
            "synth_incremental: from='{}' to='{}' roots={} nodes={} elapsed_us={}",
            current.input,
            input,
            prefix.roots.len(),
            current.parser.arena().node_count(),
            start.elapsed().as_micros()
        );
        #[cfg(test)]
        {
            self.stats.incremental_advances += 1;
        }
        Ok(ParsedState {
            parser: current.parser,
            prefix,
            segments: self.grammar.tokenize(input).unwrap_or_default(),
            input: input.to_string(),
            ctx_id: current.ctx_id,
        })
    }

    fn parse_fresh(&mut self, input: &str, ctx: &Context) -> Result<ParsedState, String> {
        let ctx_id = ctx_id(ctx, &self.runtime);
        let mut depth = self.start_depth.min(self.max_depth);

        loop {
            let mut parser = self.parser.fork().with_max_depth(depth);
            let start = Instant::now();
            match parser.parse(input, ctx_id) {
                Ok(prefix) => {
                    debug_debug!(
                        "completion_perf",
                        "synth_full_parse: input='{}' depth={} roots={} nodes={} elapsed_us={}",
                        input,
                        depth,
                        prefix.roots.len(),
                        parser.arena().node_count(),
                        start.elapsed().as_micros()
                    );
                    #[cfg(test)]
                    {
                        self.stats.full_parses += 1;
                    }
                    return Ok(ParsedState {
                        parser,
                        prefix,
                        segments: self.grammar.tokenize(input).unwrap_or_default(),
                        input: input.to_string(),
                        ctx_id,
                    });
                }
                Err(err) => {
                    debug_debug!(
                        "completion_perf",
                        "synth_full_parse_retry: input='{}' depth={} elapsed_us={} err='{}'",
                        input,
                        depth,
                        start.elapsed().as_micros(),
                        err
                    );
                    if depth >= self.max_depth {
                        return Err(err.to_string());
                    }
                    let mut next = ((depth as f64) * self.depth_factor).ceil() as u16;
                    if next <= depth {
                        next = depth + 1;
                    }
                    depth = next.min(self.max_depth);
                }
            }
        }
    }

    #[cfg(test)]
    pub(crate) fn stats(&self) -> SynthStats {
        self.stats
    }
}

fn ctx_id(ctx: &Context, runtime: &RuleRuntime) -> crate::logic::fusion::CtxId {
    runtime.intern_context(ctx.clone())
}

fn collect_regex_seed_candidates(grammar: &Grammar) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();

    for raw in gather_raw_types(grammar) {
        if seen.insert(raw.clone()) {
            out.push(raw);
        }
    }

    for seed in ["a", "x", "0", "1"] {
        if seen.insert(seed.to_string()) {
            out.push(seed.to_string());
        }
    }

    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn completion_fingerprint(ast: &FusionAST, grammar: &Grammar) -> Vec<(String, Option<String>)> {
        ast.completions(grammar)
            .into_iter()
            .map(|token| (token.to_pattern(), token.example()))
            .collect()
    }

    #[test]
    fn feed_uses_incremental_advance_after_prefix_parse() {
        let grammar = Grammar::load("start ::= 'x' 'y'").unwrap();
        let ctx = Context::new();
        let mut synth = Synthesizer::new_with_max_depth(grammar, "x", 8);

        let prefix = synth.parse_with(&ctx).unwrap();
        assert!(!prefix.is_complete());
        assert_eq!(synth.stats().full_parses, 1);
        assert_eq!(synth.stats().incremental_advances, 0);

        let next = synth.feed("y", &ctx).unwrap();
        assert_eq!(synth.input(), "x y");
        assert!(next.is_complete());
        assert_eq!(synth.stats().full_parses, 1);
        assert_eq!(synth.stats().incremental_advances, 1);
    }

    #[test]
    fn feed_caches_incremental_result_for_reuse() {
        let grammar = Grammar::load("start ::= 'x' 'y' 'z'").unwrap();
        let ctx = Context::new();
        let mut synth = Synthesizer::new_with_max_depth(grammar, "x", 8);

        let _ = synth.parse_with(&ctx).unwrap();
        let _ = synth.feed("y", &ctx).unwrap();
        assert_eq!(synth.stats().incremental_advances, 1);

        let extended = synth.extended_typed_ctx("x y", &ctx).unwrap();
        assert!(!extended.is_complete());
        assert_eq!(synth.stats().full_parses, 1);
        assert_eq!(synth.stats().incremental_advances, 1);
    }

    #[test]
    fn incremental_feed_matches_full_parse_shape() {
        let grammar = Grammar::load(
            r#"
            Name ::= /[a-z]+/
            Start ::= 'let' Name ':' 't' '=' Name
            "#,
        )
        .unwrap();
        let ctx = Context::new();
        let mut incremental = Synthesizer::new_with_max_depth(grammar.clone(), "let x", 12);

        let _ = incremental.parse_with(&ctx).unwrap();
        let incremental_ast = incremental.feed(":", &ctx).unwrap();
        let next_input = incremental.input().to_string();
        let mut full = Synthesizer::new_with_max_depth(grammar.clone(), next_input.clone(), 12);
        let full_ast = full.parse_with(&ctx).unwrap();

        assert_eq!(incremental_ast.text(), full_ast.text());
        assert_eq!(incremental_ast.is_complete(), full_ast.is_complete());
        assert_eq!(incremental_ast.len(), full_ast.len());
        assert_eq!(
            incremental_ast.min_open_slots(&grammar),
            full_ast.min_open_slots(&grammar)
        );
        assert_eq!(incremental_ast.min_tree_depth(), full_ast.min_tree_depth());
        assert_eq!(incremental_ast.bound_texts(), full_ast.bound_texts());
        assert_eq!(
            completion_fingerprint(&incremental_ast, &grammar),
            completion_fingerprint(&full_ast, &grammar)
        );
    }
}
