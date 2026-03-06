use crate::debug_debug;
use crate::logic::grammar::Grammar;
use crate::logic::partial::completion::CompletionSet;
use crate::logic::partial::{MetaParser, PartialAST};
use crate::logic::typing::gather_terminals_typed;
use crate::logic::typing::tree::TypedNode;
use crate::logic::typing::{gather_raw_types, Context, TypedAST};
use crate::regex::Regex as DerivativeRegex;
use std::collections::HashSet;

pub struct Synthesizer {
    grammar: Grammar,
    meta: MetaParser,
    input: String,
    tree: Option<TypedAST>,
    regex_seed_candidates: Vec<String>,
}

impl Synthesizer {
    pub fn new(grammar: Grammar, input: impl Into<String>) -> Self {
        let mut meta = MetaParser::new(grammar.clone());
        let input = input.into();
        let tree = meta
            .partial(&input)
            .ok()
            .and_then(|t| t.typed(&grammar).ok());

        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);

        Self {
            grammar,
            meta,
            input,
            tree,
            regex_seed_candidates,
        }
    }

    pub fn new_with_max_depth(
        grammar: Grammar,
        input: impl Into<String>,
        max_depth: usize,
    ) -> Self {
        let mut meta = MetaParser::new(grammar.clone()).with_max_depth(max_depth);
        let input = input.into();
        let tree = meta
            .partial(&input)
            .ok()
            .and_then(|t| t.typed(&grammar).ok());

        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);

        Self {
            grammar,
            meta,
            input,
            tree,
            regex_seed_candidates,
        }
    }

    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn tree(&self) -> Result<TypedAST, String> {
        self.tree
            .clone()
            .ok_or_else(|| "No typed parse tree available".to_string())
    }

    pub fn update_tree(&mut self) {
        self.tree = self.partial_typed().ok();
    }

    pub fn set_input(&mut self, input: impl Into<String>) {
        self.input = input.into();
        self.update_tree();
    }

    pub fn partial(&mut self) -> Result<PartialAST, String> {
        self.meta
            .partial_with_depth(&self.input)
            .map(|(ast, _)| ast)
    }

    pub fn partial_typed(&mut self) -> Result<TypedAST, String> {
        self.meta.partial_typed(&self.input)
    }

    pub fn partial_typed_ctx(&mut self, ctx: &Context) -> Result<TypedAST, String> {
        self.meta.partial_typed_ctx(&self.input, ctx)
    }

    pub fn completions(&mut self) -> CompletionSet {
        self.completions_ctx(&Context::new())
    }

    pub fn completions_ctx(&mut self, ctx: &Context) -> CompletionSet {
        let input = self.input.clone();
        match self.meta.partial_typed_ctx(&input, ctx) {
            Ok(typed) => {
                let tokens = typed.completions(&self.grammar);
                debug_debug!(
                    "completion",
                    "completions: input='{}' tokens={}",
                    self.input,
                    tokens.len()
                );
                for token in tokens.iter() {
                    debug_debug!(
                        "completion",
                        "completions: token='{}' example={:?}",
                        token.to_pattern(),
                        token.example()
                    );
                }
                tokens
            }
            Err(e) => {
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

    pub fn try_extend(&mut self, token: &str, ctx: &Context) -> Result<(TypedAST, String), String> {
        let (typed, extended) = match self.parse_extended_ctx(token, ctx) {
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

        Ok((typed, extended))
    }

    pub fn extend(&mut self, token: &str, ctx: &Context) -> Result<TypedAST, String> {
        let (typed, extended) = self.try_extend(token, ctx)?;
        self.input = extended;
        Ok(typed)
    }

    pub fn extend_with_regex(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
    ) -> Option<(TypedAST, String)> {
        self.extend_greedy_with_regex(token, ctx).into_iter().next()
    }

    pub fn extend_all_with_regex(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
        max_examples: usize,
    ) -> Vec<(TypedAST, String)> {
        self.extend_all_with_regex_candidates(token, ctx, &[], max_examples)
    }

    pub fn regex_gather_candidates(&self, token: &DerivativeRegex) -> Vec<String> {
        let mut candidates = Vec::new();
        let mut seen = HashSet::new();

        // Seed from grammar-derived literals/types first so candidate choice is
        // generic and language-aware rather than hardcoded.
        for candidate in &self.regex_seed_candidates {
            // Mirror parser-side keyword reservation: broad regex terminals should
            // not consume reserved literal tokens unless they match the exact literal.
            if self.grammar.special_tokens.iter().any(|t| t == candidate)
                && !token.equiv(&DerivativeRegex::literal(candidate))
            {
                continue;
            }
            if token.matches(&candidate) && seen.insert(candidate.clone()) {
                candidates.push(candidate.clone());
            }
        }

        if let Some(example) = token.example() {
            if seen.insert(example.clone()) {
                candidates.push(example);
            }
        }
        if let Ok(t) = self.tree() {
            for root in t.roots.iter() {
                let terminals = gather_terminals_typed(root);
                for terminal in terminals {
                    if token.matches(&terminal) && seen.insert(terminal.clone()) {
                        candidates.push(terminal);
                    }
                }
            }
        }

        candidates
    }

    pub fn extend_all_with_regex_candidates(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
        extra_candidates: &[String],
        max_examples: usize,
    ) -> Vec<(TypedAST, String)> {
        let mut candidates = self.regex_gather_candidates(token);
        let mut seen = candidates.iter().cloned().collect::<HashSet<_>>();

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

    fn extend_greedy_with_regex(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
    ) -> Option<(TypedAST, String)> {
        let candidates = self.regex_gather_candidates(token);
        for candidate in candidates {
            if let Ok((partial, extended)) = self.try_extend(&candidate, ctx) {
                return Some((partial, extended));
            }
        }
        None
    }

    pub fn complete(&mut self) -> Option<TypedNode> {
        self.tree.as_ref().and_then(|t| t.clone().complete().ok())
    }

    fn parse_extended_ctx(
        &mut self,
        token: &str,
        ctx: &Context,
    ) -> Result<(TypedAST, String), String> {
        let spaced = format!("{} {}", self.input, token);
        if let Ok((partial, _)) = self.meta.partial_typed_ctx_with_depth(&spaced, ctx) {
            return Ok((partial, spaced));
        }
        let direct = format!("{}{}", self.input, token);
        if let Ok((partial, _)) = self.meta.partial_typed_ctx_with_depth(&direct, ctx) {
            return Ok((partial, direct));
        }

        Err(format!(
            "Parse failed for input='{}' token='{}'",
            self.input, token
        ))
    }
}

fn collect_regex_seed_candidates(grammar: &Grammar) -> Vec<String> {
    let mut out = Vec::new();
    let mut seen = HashSet::new();

    for lit in &grammar.special_tokens {
        // Regex seeding only needs word-like literals. Punctuation/operators are
        // handled by direct literal tokens and just add overhead here.
        if lit.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') && seen.insert(lit.clone()) {
            out.push(lit.clone());
        }
    }

    for raw in gather_raw_types(grammar) {
        if seen.insert(raw.clone()) {
            out.push(raw);
        }
    }

    out
}
