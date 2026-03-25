use crate::debug_debug;
use crate::logic::grammar::Grammar;
use crate::logic::partial::completion::CompletionSet;
use crate::logic::partial::{MetaParser, SppfForest};
use crate::logic::typing::gather_terminals_typed;
use crate::logic::typing::tree::TypedNode;
use crate::logic::typing::{gather_raw_types, Context, TypedAST};
use crate::regex::Regex as DerivativeRegex;
use std::cell::Ref;
use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

type MemoMap = HashMap<String, Result<Arc<SppfForest>, String>>;

/// Per-entry stats for the parse memo, computed cheaply from interned SPPF nodes.
/// Caller must consume the Ref before releasing it.
pub type MemoRef<'a> = Ref<'a, MemoMap>;

pub struct Synthesizer {
    grammar: Grammar,
    meta: MetaParser,
    input: String,
    tree: Option<TypedAST>,
    regex_seed_candidates: Vec<String>,
    // Note: synthesizer no longer keeps persistent caches for partial/typed/
    // completion results. The parser still uses its within-call memo table to
    // avoid exponential parsing work. This struct only keeps lightweight
    // helpers and the meta-parser.
    /// Cross-parse memo for partial parse results (input -> SppfForest).
    /// Stored as interior-mutable RefCell to avoid copying and allow cheap
    /// Arc clones for shared ownership across callers.
    parse_memo: RefCell<HashMap<String, Result<Arc<SppfForest>, String>>>,
}

impl Synthesizer {
    pub fn new(grammar: Grammar, input: impl Into<String>) -> Self {
        let meta = MetaParser::new(grammar.clone());
        let input = input.into();

        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);

        Self {
            grammar,
            meta,
            input,
            tree: None,
            regex_seed_candidates,
            parse_memo: RefCell::new(HashMap::new()),
        }
    }

    pub fn new_with_max_depth(
        grammar: Grammar,
        input: impl Into<String>,
        max_depth: usize,
    ) -> Self {
        let meta = MetaParser::new(grammar.clone()).with_max_depth(max_depth);
        let input = input.into();

        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);

        Self {
            grammar,
            meta,
            input,
            tree: None,
            regex_seed_candidates,
            parse_memo: RefCell::new(HashMap::new()),
        }
    }

    pub fn clear_memo(&self) {
        self.parse_memo.borrow_mut().clear();
    }

    pub fn memo_entry_count(&self) -> usize {
        self.parse_memo.borrow().len()
    }

    /// Borrow the memo for iteration. Returns a Ref that must be consumed
    /// before the borrow is released. This avoids copying — callers iterate
    /// directly over the interned data.
    pub fn iter_memo(&self) -> MemoRef<'_> {
        self.parse_memo.borrow()
    }

    pub fn cache_stats(&self) -> (usize, usize, usize) {
        // Return (partial_cached_inputs, typed_node_count, approx_size_bytes)
        let partial_cached = self.parse_memo.borrow().len();

        let mut typed_node_count = 0usize;
        let mut approx_size = 0usize;
        if let Some(t) = &self.tree {
            fn count_node(n: &crate::logic::typing::tree::TypedNode) -> usize {
                match n {
                    crate::logic::typing::tree::TypedNode::Term { .. } => 1,
                    crate::logic::typing::tree::TypedNode::Expr { children, .. } => {
                        1 + children.iter().map(|c| count_node(c)).sum::<usize>()
                    }
                }
            }
            typed_node_count = t.roots.iter().map(|r| count_node(r)).sum();
            // include cached partials in approx size
            approx_size = self
                .parse_memo
                .borrow()
                .values()
                .filter_map(|res| res.as_ref().ok())
                .map(|p| p.node_count() * 64)
                .sum::<usize>();
            // add typed tree size
            approx_size += typed_node_count * 64;
        }

        (partial_cached, typed_node_count, approx_size)
    }

    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn tree(&self) -> Option<TypedAST> {
        self.tree.clone()
    }

    pub fn update_tree(&mut self) {
        self.tree = self.partial_typed().ok();
    }

    pub fn set_input(&mut self, input: impl Into<String>) {
        self.input = input.into();
        self.update_tree();
    }

    /// Feed a new input snapshot and return typed completions for it.
    /// This is the hot path for interactive synthesis and is cache-backed.
    pub fn feed(&mut self, input: impl Into<String>, ctx: &Context) -> CompletionSet {
        self.input = input.into();
        self.completions_ctx(ctx)
    }

    pub fn partial(&mut self) -> Result<SppfForest, String> {
        let input = self.input.clone();
        self.cached_partial_ref(&input)
            .map(|ast| ast.as_ref().clone())
    }

    pub fn partial_typed(&mut self) -> Result<TypedAST, String> {
        let input = self.input.clone();
        self.cached_typed_ctx_ref(&input, &Context::new())
            .map(|typed| typed.as_ref().clone())
    }

    pub fn partial_typed_ctx(&mut self, ctx: &Context) -> Result<TypedAST, String> {
        let input = self.input.clone();
        self.cached_typed_ctx_ref(&input, ctx)
            .map(|typed| typed.as_ref().clone())
    }

    pub fn completions(&mut self) -> CompletionSet {
        self.completions_ctx(&Context::new())
    }

    pub fn completions_ctx(&mut self, ctx: &Context) -> CompletionSet {
        let input = self.input.clone();
        let _ctx_key = context_cache_key(ctx);
        match self.cached_typed_ctx_ref(&input, ctx) {
            Ok(typed) => {
                self.tree = Some(typed.as_ref().clone());
                let tokens = typed.as_ref().completions(&self.grammar);
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
        self.tree = Some(typed.clone());
        Ok(typed)
    }

    pub fn extend_with_regex(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
    ) -> Option<(TypedAST, String)> {
        let (typed, extended) = self.extend_greedy_with_regex(token, ctx)?;
        self.input = extended.clone();
        self.tree = Some(typed.clone());
        Some((typed, extended))
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
        if let Some(t) = self.tree() {
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
        if let Ok(partial) = self.cached_typed_ctx_ref(&spaced, ctx) {
            return Ok((partial.as_ref().clone(), spaced));
        }
        let direct = format!("{}{}", self.input, token);
        if let Ok(partial) = self.cached_typed_ctx_ref(&direct, ctx) {
            return Ok((partial.as_ref().clone(), direct));
        }

        Err(format!(
            "Parse failed for input='{}' token='{}'",
            self.input, token
        ))
    }

    // copying a string
    fn cached_partial_ref(&mut self, input: &str) -> Result<Arc<SppfForest>, String> {
        // First check cross-parse memo to avoid re-parsing identical inputs.
        if let Some(cached) = self.parse_memo.borrow().get(input) {
            return cached.clone();
        }

        let parsed = self
            .meta
            .partial_with_depth(input)
            .map(|(ast, _)| Arc::new(ast));

        // Store in parse_memo for reuse across synth calls.
        self.parse_memo
            .borrow_mut()
            .insert(input.to_string(), parsed.clone());

        parsed
    }

    fn cached_typed_ctx_ref(
        &mut self,
        input: &str,
        ctx: &Context,
    ) -> Result<Arc<TypedAST>, String> {
        // First check partial memo to reuse parsed forests, then type the result.
        // This chains into cached_partial_ref so repeated inputs hit the memo.
        self.cached_partial_ref(input)?
            .typed_ctx(&self.grammar, ctx)
            .map(Arc::new)
    }

    // Previously the synthesizer kept several LRU caches here. Those have
    // been removed to simplify behavior and avoid stale cross-request state.
}

fn context_cache_key(ctx: &Context) -> String {
    let mut bindings: Vec<(String, String)> = ctx
        .bindings
        .iter()
        .map(|(k, v)| (k.clone(), v.to_string()))
        .collect();
    bindings.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

    let mut unresolved: Vec<(String, String)> = ctx
        .unresolved_bindings
        .iter()
        .map(|(p, t)| {
            let path = p
                .iter()
                .map(|idx| idx.to_string())
                .collect::<Vec<_>>()
                .join(".");
            (path, t.to_string())
        })
        .collect();
    unresolved.sort_by(|a, b| a.0.cmp(&b.0).then(a.1.cmp(&b.1)));

    let b = bindings
        .into_iter()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect::<Vec<_>>()
        .join("|");
    let u = unresolved
        .into_iter()
        .map(|(k, v)| format!("{}={}", k, v))
        .collect::<Vec<_>>()
        .join("|");
    format!("b:{};u:{}", b, u)
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
