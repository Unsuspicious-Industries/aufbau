use crate::debug_debug;
use crate::logic::grammar::Grammar;
use crate::logic::partial::completion::CompletionSet;
use crate::logic::partial::{MetaParser, Parser, PrefixState, SppfForest};
use crate::logic::typing::gather_terminals_typed;
use crate::logic::typing::tree::TypedNode;
use crate::logic::typing::{gather_raw_types, Context, TypedAST};
use crate::regex::Regex as DerivativeRegex;
use std::cell::{Cell, Ref, RefCell};
use std::collections::HashMap;
use std::collections::HashSet;
use std::sync::Arc;

type MemoMap = HashMap<String, Result<Arc<SppfForest>, String>>;
type TypedMemoMap = HashMap<String, Result<Arc<TypedAST>, String>>;

/// Per-entry stats for the parse memo, computed cheaply from interned SPPF nodes.
/// Caller must consume the Ref before releasing it.
pub type MemoRef<'a> = Ref<'a, MemoMap>;

pub struct Synthesizer {
    grammar: Grammar,
    meta: MetaParser,
    input: String,
    tree: Option<TypedAST>,
    prefix_state: Option<PrefixState>,
    regex_seed_candidates: Vec<String>,
    // Note: synthesizer no longer keeps persistent caches for partial/typed/
    // completion results. The parser still uses its within-call memo table to
    // avoid exponential parsing work. This struct only keeps lightweight
    // helpers and the meta-parser.
    /// Cross-parse memo for partial parse results (input -> SppfForest).
    /// Stored as interior-mutable RefCell to avoid copying and allow cheap
    /// Arc clones for shared ownership across callers.
    parse_memo: RefCell<HashMap<String, Result<Arc<SppfForest>, String>>>,
    typed_memo: RefCell<TypedMemoMap>,
    parse_memo_hits: Cell<usize>,
    parse_memo_misses: Cell<usize>,
    typed_memo_hits: Cell<usize>,
    typed_memo_misses: Cell<usize>,
}

impl Synthesizer {
    pub fn new(grammar: Grammar, input: impl Into<String>) -> Self {
        let meta = MetaParser::new(grammar.clone()).with_preserve_cache_across_parses(true);
        let input = input.into();

        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);

        Self {
            grammar,
            meta,
            input,
            tree: None,
            prefix_state: None,
            regex_seed_candidates,
            parse_memo: RefCell::new(HashMap::new()),
            typed_memo: RefCell::new(HashMap::new()),
            parse_memo_hits: Cell::new(0),
            parse_memo_misses: Cell::new(0),
            typed_memo_hits: Cell::new(0),
            typed_memo_misses: Cell::new(0),
        }
    }

    pub fn new_with_max_depth(
        grammar: Grammar,
        input: impl Into<String>,
        max_depth: usize,
    ) -> Self {
        let meta = MetaParser::new(grammar.clone())
            .with_max_depth(max_depth)
            .with_preserve_cache_across_parses(true);
        let input = input.into();

        let regex_seed_candidates = collect_regex_seed_candidates(&grammar);

        Self {
            grammar,
            meta,
            input,
            tree: None,
            prefix_state: None,
            regex_seed_candidates,
            parse_memo: RefCell::new(HashMap::new()),
            typed_memo: RefCell::new(HashMap::new()),
            parse_memo_hits: Cell::new(0),
            parse_memo_misses: Cell::new(0),
            typed_memo_hits: Cell::new(0),
            typed_memo_misses: Cell::new(0),
        }
    }

    pub fn clear_memo(&mut self) {
        self.parse_memo.borrow_mut().clear();
        self.typed_memo.borrow_mut().clear();
        self.prefix_state = None;
        self.parse_memo_hits.set(0);
        self.parse_memo_misses.set(0);
        self.typed_memo_hits.set(0);
        self.typed_memo_misses.set(0);
    }

    pub fn memo_entry_count(&self) -> usize {
        self.parse_memo.borrow().len() + self.typed_memo.borrow().len()
    }

    pub fn memo_stats(&self) -> (usize, usize, usize, usize) {
        (
            self.parse_memo_hits.get(),
            self.parse_memo_misses.get(),
            self.typed_memo_hits.get(),
            self.typed_memo_misses.get(),
        )
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
        let next = input.into();
        if next == self.input {
            return;
        }

        self.input = next;
        self.prefix_state = None;
        self.parse_memo.borrow_mut().clear();
        self.typed_memo.borrow_mut().clear();
        self.update_tree();
    }

    /// Feed a new input snapshot and return typed completions for it.
    /// This is the hot path for interactive synthesis and is cache-backed.
    pub fn feed(&mut self, input: impl Into<String>, ctx: &Context) -> CompletionSet {
        let next_input = input.into();

        if self.tree.is_some()
            && let Some(tokens) = self.append_only_tokens_for(&next_input)
            && let Some(tokens) = self.try_append_only_incremental(tokens, &next_input, ctx)
        {
            return tokens;
        }

        self.input = next_input;
        self.completions_ctx(ctx)
    }

    fn append_only_tokens_for(&self, next_input: &str) -> Option<Vec<String>> {
        let suffix = next_input.strip_prefix(&self.input)?;
        if suffix.is_empty() {
            return Some(Vec::new());
        }

        if suffix.trim().is_empty() {
            return Some(Vec::new());
        }

        self.grammar
            .tokenize(suffix)
            .ok()
            .map(|segments| segments.into_iter().map(|segment| segment.text()).collect())
    }

    fn try_append_only_incremental(
        &mut self,
        tokens: Vec<String>,
        next_input: &str,
        ctx: &Context,
    ) -> Option<CompletionSet> {
        let original_input = self.input.clone();
        let original_tree = self.tree.clone();

        for token in tokens {
            if self.extend(&token, ctx).is_err() {
                self.input = original_input;
                self.tree = original_tree;
                return None;
            }
        }

        if !same_tokenization(&self.grammar, &self.input, next_input) {
            self.input = original_input;
            self.tree = original_tree;
            return None;
        }

        let typed = self.tree.clone()?;
        let key = typed_cache_key(next_input, ctx);
        self.typed_memo.borrow_mut().insert(key, Ok(Arc::new(typed.clone())));
        self.input = next_input.to_string();
        Some(typed.completions(&self.grammar))
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
                let local_terms = typed
                    .roots
                    .iter()
                    .flat_map(gather_terminals_typed)
                    .collect::<Vec<_>>();
                let tokens = self.refine_tokens_for_typed_extensions(
                    typed.as_ref().completions(&self.grammar),
                    ctx,
                    &local_terms,
                );
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
            self.parse_memo_hits.set(self.parse_memo_hits.get() + 1);
            return cached.clone();
        }

        self.parse_memo_misses.set(self.parse_memo_misses.get() + 1);

        let parsed = match self.prefix_state.take() {
            Some(prev) if input.starts_with(prev.input()) => self
                .meta
                .advance_owned_with_depth(prev, input)
                .map(|(prefix, _)| prefix),
            Some(prev) => {
                self.prefix_state = Some(prev);
                self.meta.prefix_with_depth(input).map(|(prefix, _)| prefix)
            }
            None => self.meta.prefix_with_depth(input).map(|(prefix, _)| prefix),
        }
        .map(|prefix| {
            let forest = prefix.forest().clone();
            self.prefix_state = Some(prefix);
            Arc::new(forest)
        });

        // Store in parse_memo for reuse across synth calls.
        let mut parse_memo = self.parse_memo.borrow_mut();
        parse_memo.clear();
        parse_memo.insert(input.to_string(), parsed.clone());

        parsed
    }

    fn cached_typed_ctx_ref(
        &mut self,
        input: &str,
        ctx: &Context,
    ) -> Result<Arc<TypedAST>, String> {
        let key = typed_cache_key(input, ctx);

        if let Some(cached) = self.typed_memo.borrow().get(&key) {
            self.typed_memo_hits.set(self.typed_memo_hits.get() + 1);
            return cached.clone();
        }

        self.typed_memo_misses.set(self.typed_memo_misses.get() + 1);

        let typed = self
            .cached_partial_ref(input)?
            .typed_ctx(&self.grammar, ctx)
            .or_else(|err| {
                if err != "No well-typed trees" {
                    return Err(err);
                }

                let mut parser = Parser::new(self.grammar.clone()).with_max_recursion(
                    self.meta
                        .last_used_depth()
                        .unwrap_or(self.meta.parser().max_recursion()),
                );
                let ast = parser
                    .partial(input)
                    .into_result()
                    .map_err(|e| e.to_string())?;
                ast.typed_ctx(&self.grammar, ctx)
            })
            .map(Arc::new);

        let mut typed_memo = self.typed_memo.borrow_mut();
        typed_memo.clear();
        typed_memo.insert(key, typed.clone());
        typed
    }

    fn refine_tokens_for_typed_extensions(
        &mut self,
        tokens: CompletionSet,
        ctx: &Context,
        local_terms: &[String],
    ) -> CompletionSet {
        let refined = tokens
            .iter()
            .filter_map(|token| self.refine_token_for_typed_extension(token, ctx, local_terms))
            .collect::<Vec<_>>();
        CompletionSet::from_tokens(refined)
    }

    fn refine_token_for_typed_extension(
        &mut self,
        token: &DerivativeRegex,
        ctx: &Context,
        local_terms: &[String],
    ) -> Option<DerivativeRegex> {
        if let Some(example) = token.example() {
            if self.try_extend(&example, ctx).is_ok() {
                return Some(token.clone());
            }
        }

        // Not a heuristic: if a completion regex is standing in for an identifier
        // continuation, the current context gives an exact set of admissible
        // suffixes for that partial identifier.
        if let Some(fragment) = trailing_identifier_fragment(&self.input).map(str::to_string) {
            for name in ctx.bindings.keys() {
                if let Some(suffix) = name.strip_prefix(&fragment) {
                    if !suffix.is_empty()
                        && token.matches(suffix)
                        && self.try_extend(suffix, ctx).is_ok()
                    {
                        return Some(DerivativeRegex::literal(suffix));
                    }
                }
            }
        }

        self.regex_gather_candidates(token)
            .into_iter()
            .chain(local_terms.iter().cloned())
            .find(|candidate| self.try_extend(candidate, ctx).is_ok())
            .map(|candidate| DerivativeRegex::literal(&candidate))
    }

    // Previously the synthesizer kept several LRU caches here. Those have
    // been removed to simplify behavior and avoid stale cross-request state.
}

fn typed_cache_key(input: &str, ctx: &Context) -> String {
    format!("{}::{}", input, context_cache_key(ctx))
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

fn trailing_identifier_fragment(input: &str) -> Option<&str> {
    let end = input.len();
    let start = input
        .char_indices()
        .rev()
        .take_while(|(_, ch)| ch.is_ascii_alphanumeric() || *ch == '_')
        .last()
        .map(|(idx, _)| idx)?;
    Some(&input[start..end])
}

fn same_tokenization(grammar: &Grammar, left: &str, right: &str) -> bool {
    match (grammar.tokenize(left), grammar.tokenize(right)) {
        (Ok(left_segments), Ok(right_segments)) => left_segments
            .iter()
            .map(|segment| segment.as_str())
            .eq(right_segments.iter().map(|segment| segment.as_str())),
        _ => false,
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
