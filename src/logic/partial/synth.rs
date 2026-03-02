use crate::debug_debug;
use crate::logic::grammar::Grammar;
use crate::logic::grammar::Symbol;
use crate::logic::partial::completion::CompletionSet;
use crate::logic::partial::{MetaParser, PartialAST};
use crate::logic::typing::symbols::gather_raw_types;
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
        let input = input.into();
        Self {
            grammar,
            meta,
            input,
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
        self.meta
            .partial_with_depth(&self.input)
            .map(|(ast, _)| ast)
    }

    pub fn completions(&mut self) -> CompletionSet {
        match self.partial() {
            Ok(partial) => partial.completions(&self.grammar),
            Err(_) => CompletionSet::empty(),
        }
    }

    // returns completions from typed trees
    pub fn typed_completions(&mut self, ctx: &Context) -> CompletionSet {
        let input = self.input.clone();
        match self.meta.partial_with_depth(&input) {
            Ok((partial, _used_depth)) => {
                let typed = match partial.filter_typed_ctx(&self.grammar, ctx) {
                    Ok(ast) => ast,
                    Err(e) => {
                        debug_debug!(
                            "completion",
                            "typed_completions: filter_typed_ctx failed input='{}' err='{}'",
                            self.input,
                            e
                        );
                        return CompletionSet::empty();
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
        self.extend_all_with_regex(token, ctx, max_examples)
            .into_iter()
            .next()
    }

    pub fn extend_all_with_regex(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
        max_examples: usize,
    ) -> Vec<(PartialAST, String)> {
        self.extend_all_with_regex_candidates(token, ctx, &[], max_examples)
    }

    pub fn extend_all_with_regex_candidates(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
        extra_candidates: &[String],
        max_examples: usize,
    ) -> Vec<(PartialAST, String)> {
        let mut candidates = Vec::new();
        let mut seen = HashSet::new();

        if let Some(example) = token.example() {
            if seen.insert(example.clone()) {
                candidates.push(example);
            }
        }

        for raw in gather_raw_types(&self.grammar) {
            if token.matches(&raw) && seen.insert(raw.clone()) {
                candidates.push(raw);
            }
        }

        // Fallback: try concrete terminal symbols from the grammar that
        // satisfy the expected completion regex. This keeps completion
        // grammar-agnostic while broadening search coverage.
        for lit in &self.grammar.special_tokens {
            if token.matches(lit) && seen.insert(lit.clone()) {
                candidates.push(lit.clone());
            }
        }
        for prods in self.grammar.productions.values() {
            for prod in prods {
                for sym in &prod.rhs {
                    if let Symbol::Terminal { regex, .. } = sym {
                        if let Some(example) = regex.example() {
                            if token.matches(&example) && seen.insert(example.clone()) {
                                candidates.push(example);
                            }
                        }
                    }
                }
            }
        }

        for candidate in extra_candidates {
            if token.matches(candidate) && seen.insert(candidate.clone()) {
                candidates.push(candidate.clone());
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

        let mut out = Vec::new();
        let mut seen_extended = HashSet::new();
        for candidate in candidates {
            if max_examples > 0 && out.len() >= max_examples {
                break;
            }
            if let Ok((partial, extended)) = self.try_extend(&candidate, ctx) {
                if seen_extended.insert(extended.clone()) {
                    out.push((partial, extended));
                }
            }
        }

        out
    }

    pub fn complete(&mut self) -> Option<crate::logic::partial::NonTerminal> {
        self.partial().ok().and_then(|ast| ast.complete())
    }

    fn parse_extended(&mut self, token: &str) -> Result<(PartialAST, String), String> {
        // hacky and hardcoded
        // bad
        let needs_sep = self
            .input
            .chars()
            .last()
            .map(|c| c.is_ascii_alphanumeric() || c == '_')
            .unwrap_or(false)
            && token
                .chars()
                .next()
                .map(|c| c.is_ascii_alphanumeric() || c == '_')
                .unwrap_or(false);

        let spaced = format!("{} {}", self.input, token);
        let direct = format!("{}{}", self.input, token);

        if needs_sep {
            if let Ok((partial, _)) = self.meta.partial_with_depth(&spaced) {
                return Ok((partial, spaced));
            }
            if let Ok((partial, _)) = self.meta.partial_with_depth(&direct) {
                return Ok((partial, direct));
            }
        } else {
            if let Ok((partial, _)) = self.meta.partial_with_depth(&direct) {
                return Ok((partial, direct));
            }
            if let Ok((partial, _)) = self.meta.partial_with_depth(&spaced) {
                return Ok((partial, spaced));
            }
        }

        Err(format!(
            "Parse failed for input='{}' token='{}'",
            self.input, token
        ))
    }
}
