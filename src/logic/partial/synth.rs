use crate::debug_debug;
use crate::logic::grammar::Grammar;
use crate::logic::partial::completion::CompletionSet;
use crate::logic::partial::{MetaParser, PartialAST};
use crate::logic::typing::symbols::gather_terminals;
use crate::logic::typing::Context;
use crate::regex::Regex as DerivativeRegex;
use std::collections::HashSet;

pub struct Synthesizer {
    grammar: Grammar,
    meta: MetaParser,
    input: String,
}

impl Synthesizer {
    pub fn new(grammar: Grammar, input: impl Into<String>) -> Self {
        let meta = MetaParser::new(grammar.clone());
        Self {
            grammar,
            meta,
            input: input.into(),
        }
    }

    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn set_input(&mut self, input: impl Into<String>) {
        self.input = input.into();
    }

    pub fn partial(&mut self) -> Result<PartialAST, String> {
        self.meta.partial(&self.input).map_err(|e| e.to_string())
    }

    pub fn completions(&mut self) -> CompletionSet {
        match self.partial() {
            Ok(partial) => partial.completions(&self.grammar),
            Err(_) => CompletionSet::empty(),
        }
    }

    // returns completions from typed trees
    pub fn typed_completions(&mut self, ctx: &Context) -> CompletionSet {
        match self.meta.partial_with_depth(&self.input) {
            Ok((mut partial, used_depth)) => {
                let typed = match partial.filter_typed_ctx(&self.grammar, ctx) {
                    Ok(ast) => ast,
                    Err(e) => {
                        debug_debug!(
                            "completion",
                            "typed_completions: filter_typed_ctx failed input='{}' err='{}'",
                            self.input,
                            e
                        );
                        // try reparsing with more depth to get better completions, but don't fail if it doesn't work
                        // VERY HACKING FIXING
                        let base_depth = self.meta.cached_best_depth().unwrap_or(used_depth);
                        let retry_depth = base_depth.saturating_mul(2);
                        let try2 = match MetaParser::new(self.grammar().clone())
                            .with_start_depth(retry_depth)
                            .partial(&self.input)
                        {
                            Ok(ast) => ast,
                            Err(e) => {
                                debug_debug!(
                                    "completion",
                                    "typed_completions: retry failed input='{}' depth={} err='{}'",
                                    self.input,
                                    retry_depth,
                                    e
                                );
                                return CompletionSet::empty();
                            }
                        };
                        match try2.filter_typed_ctx(&self.grammar, ctx) {
                            Ok(ast) => ast,
                            Err(e) => {
                                debug_debug!(
                                    "completion",
                                    "typed_completions: retry filter_typed_ctx failed input='{}' depth={} err='{}'",
                                    self.input,
                                    retry_depth,
                                    e
                                );
                                return CompletionSet::empty();
                            }
                        }
                        // END OF HACK
                    }
                };
                let tokens = typed.completions(&self.grammar);
                debug_debug!(
                    "completion",
                    "typed_completions: input='{}' tokens={}",
                    self.input,
                    tokens.len()
                );
                for token in tokens.iter() {
                    debug_debug!(
                        "completion",
                        "typed_completions: token='{}' example={:?}",
                        token.to_pattern(),
                        token.example()
                    );
                }
                tokens
            }
            Err(_) => CompletionSet::empty(),
        }
    }

    pub fn try_extend(
        &mut self,
        token: &str,
        ctx: &Context,
    ) -> Result<(PartialAST, String), String> {
        let (partial, extended) = match self.parse_extended(token) {
            Ok(parsed) => parsed,
            Err(e) => {
                debug_debug!(
                    "completion",
                    "try_extend: parse failed input='{}' token='{}' err='{}'",
                    self.input,
                    token,
                    e
                );
                return Err(e);
            }
        };
        let typed = match partial.filter_typed_ctx(&self.grammar, ctx) {
            Ok(ast) => ast,
            Err(e) => {
                debug_debug!(
                    "completion",
                    "try_extend: filter_typed_ctx failed input='{}' token='{}' err='{}'",
                    self.input,
                    token,
                    e
                );
                return Err(e);
            }
        };
        Ok((typed, extended))
    }

    pub fn extend(&mut self, token: &str, ctx: &Context) -> Result<PartialAST, String> {
        let (partial, extended) = self.try_extend(token, ctx)?;
        self.input = extended;
        Ok(partial)
    }

    pub fn extend_with_regex(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
        max_examples: usize,
    ) -> Option<(PartialAST, String)> {
        let mut candidates = Vec::new();
        let mut seen = HashSet::new();

        if let Ok(partial) = self.partial() {
            for root in &partial.roots {
                for term in gather_terminals(root) {
                    if token.matches(&term) && seen.insert(term.clone()) {
                        candidates.push(term);
                    }
                }
            }
        }

        if !candidates.is_empty() {
            debug_debug!(
                "completion",
                "extend_with_regex: input='{}' token='{}' gathered_terminals={:?}",
                self.input,
                token.to_pattern(),
                candidates
            );
        }

        if let Some(example) = token.example() {
            if seen.insert(example.clone()) {
                candidates.push(example);
            }
        }

        for example in token.examples(max_examples) {
            if seen.insert(example.clone()) {
                candidates.push(example);
            }
        }

        if !candidates.is_empty() {
            debug_debug!(
                "completion",
                "extend_with_regex: input='{}' token='{}' candidates={:?}",
                self.input,
                token.to_pattern(),
                candidates
            );
        }

        for candidate in candidates {
            if let Ok((partial, extended)) = self.try_extend(&candidate, ctx) {
                return Some((partial, extended));
            }
        }

        None
    }

    pub fn complete(&mut self) -> Option<crate::logic::partial::NonTerminal> {
        self.partial().ok().and_then(|ast| ast.complete())
    }

    fn parse_extended(&mut self, token: &str) -> Result<(PartialAST, String), String> {
        let direct = format!("{}{}", self.input, token);
        if let Ok(partial) = self.meta.partial(&direct) {
            return Ok((partial, direct));
        }

        let spaced = format!("{} {}", self.input, token);
        if let Ok(partial) = self.meta.partial(&spaced) {
            return Ok((partial, spaced));
        }

        Err(format!(
            "Parse failed for input='{}' token='{}'",
            self.input, token
        ))
    }
}
