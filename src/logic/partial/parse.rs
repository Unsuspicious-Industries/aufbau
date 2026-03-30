use std::collections::{HashMap, HashSet};

use crate::debug_trace;
use crate::logic::grammar::{Grammar, Production, Segment, Symbol};
use crate::logic::partial::memo::{
    clear_shared_memo, MemoEntry, MemoTable, ParseMemoKey, ParsedNt,
};
use crate::logic::partial::state::{ParseState, PrefixState, SeedMemo};
use crate::logic::partial::structure::{
    grammar_store_key, register_grammar, register_node, PackedAlternative, SppfChild, SppfForest,
    SppfNode, SppfNodeId, Terminal,
};
use crate::logic::segment::SegmentRange;
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use serde::Serialize;
use std::sync::Arc;

/// Shifts indices [0..n) by `level` positions, wrapping around.
/// Different recursion depths try productions in different orders to distribute search effort.
///
/// Example with n=4, level=1: [1, 2, 3, 0]
/// Example with n=4, level=2: [2, 3, 0, 1]
///
/// ## Time Complexity
/// O(n) where n is the number of elements
fn prng_shuffle(n: usize, level: usize) -> Vec<usize> {
    if n == 0 {
        return Vec::new();
    }
    (0..n).map(|i| (i + level) % n).collect()
}

/*
 * Parser for context-free grammars with left-recursion support.
 *
 * ## Time Complexity Analysis
 *
 * For a grammar G and input string of length n:
 * - Tokenization: O(n) (linear scan)
 * - Parsing: O(n * |G| * d)
 *   + n the input length
 *   + |G| the size of the grammar
 *   + d the maximum recursion depth
 *  This means basically linear ?
 *  TODO: work on depth correctness
 * - With memoization: O(n * |G| * d) but with reduced constant factors
 *
 * ## Termination
 *
 * The parser terminates because:
 *  - we are supposed to consume tokens
 *  - memoization stores exeisting states
 *
 * For left-recursive grammars like STLC's Application rule:
 * Term -> Application | BaseTerm
 * Application -> Term BaseTerm
 *
 * Without memoization, this would cause infinite recursion:
 * Term -> Application -> Term -> Application -> ...
 *
 * With memoization, the first Term->Application expansion is cached,
 * so subsequent attempts reuse the cached result.
 *
 * ## Within-Call Memoization
 *
 * Each call to `partial()` creates a fresh `ParseState` with its own memo
 * table. The memo is keyed by `(nt_name, abs_pos, segments_len)` and prevents
 * exponential recomputation of the same subproblem within a single parse.
 *
 * By default the memo does NOT persist across separate `partial()` calls.
 * It can optionally be preserved across parses for experiments.
 */

/// Default maximum recursion depth for left-recursive grammars.
/// Limits depth to prevent exponential blowup on ambiguous grammars.
/// MetaParser enables adaptive depth finding. Override with `with_max_recursion`.
const DEFAULT_MAX_RECURSION_DEPTH: usize = 15;
const DEFAULT_PERSIST_WINDOW: usize = 32;
const DEFAULT_MAX_PERSISTED_MEMO_ENTRIES: usize = 2048;

/// Outcome of a partial parse operation with detailed metadata.
///
/// This provides rich information about the parse result, distinguishing between
/// depth-limited parses (which might improve with higher recursion limits) and
/// grammar mismatches (which will never succeed regardless of depth).
#[derive(Debug, Clone)]
pub enum PartialParseOutcome {
    Success { ast: SppfForest },
    Failure(ParseError),
}

/// Error types for parse failures.
#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    /// Tokenization failed - input contains characters/tokens not recognized by grammar
    Tokenization(String),
    /// Grammar has no start symbol defined
    NoStartSymbol,
    /// No valid parse alternatives found for this input
    NoValidParse,
    /// Hit recursion depth limit - may succeed with higher max_recursion
    DepthLimit,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Tokenization(e) => write!(f, "Tokenization error: {}", e),
            ParseError::NoStartSymbol => write!(f, "Grammar has no start symbol"),
            ParseError::NoValidParse => write!(f, "No valid parse alternatives found"),
            ParseError::DepthLimit => write!(f, "Recursion depth limit reached"),
        }
    }
}

impl std::error::Error for ParseError {}

impl PartialParseOutcome {
    pub fn is_complete(&self) -> bool {
        match self {
            Self::Success { ast } => ast.is_complete(),
            _ => false,
        }
    }

    pub fn is_success(&self) -> bool {
        match self {
            Self::Success { .. } => true,
            _ => false,
        }
    }

    pub fn into_result(self) -> Result<SppfForest, ParseError> {
        match self {
            Self::Success { ast } if !ast.is_empty() => Ok(ast),
            Self::Success { .. } => Err(ParseError::NoValidParse),
            Self::Failure(e) => Err(e),
        }
    }

    pub fn ast(&self) -> Option<&SppfForest> {
        match self {
            Self::Success { ast } => Some(ast),
            _ => None,
        }
    }

    pub fn unwrap(self) -> SppfForest {
        match self {
            Self::Success { ast } if !ast.is_empty() => ast,
            Self::Success { .. } => panic!("Called unwrap on Success with 0 roots"),
            Self::Failure(e) => panic!("Called unwrap on Failure: {}", e),
        }
    }

    pub fn expect(self, msg: &str) -> SppfForest {
        match self {
            Self::Success { ast } if !ast.is_empty() => ast,
            _ => panic!("{}", msg),
        }
    }

    pub fn is_ok(&self) -> bool {
        self.is_success()
    }

    pub fn unwrap_err(self) -> ParseError {
        match self {
            Self::Failure(e) => e,
            Self::Success { .. } => panic!("Called unwrap_err on Success"),
        }
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct ParserStats {
    pub nt_cache_hits: usize,
    pub nt_cache_misses: usize,
    pub nt_cache_stores: usize,
    pub suffix_cache_hits: usize,
    pub suffix_cache_misses: usize,
    pub cycle_cuts: usize,
    pub clone_events: usize,
}

#[derive(Debug, Clone)]
struct ParsedChild {
    child: SppfChild,
    consumed: usize,
    complete: bool,
}

impl ParsedChild {
    fn from_nt(nt: ParsedNt) -> Self {
        Self {
            child: SppfChild::Node(nt.node_id),
            consumed: nt.consumed,
            complete: nt.complete,
        }
    }
}

impl Segment {
    /// Get the segment range (just its own index)
    ///
    pub fn seg_range(&self) -> SegmentRange {
        SegmentRange::single(self.index)
    }
}

pub struct Parser {
    pub(crate) grammar: Grammar,
    pub forest: SppfForest,
    /// Precomputed reserved literal tokens (keywords/operators) for O(1) checks
    /// during regex terminal matching.
    reserved_tokens: HashSet<String>,
    /// Maximum recursion depth for left-recursive patterns like `Expr Expr`
    max_recursion: usize,
    persist_window: usize,
    max_persisted_memo_entries: usize,
    /// Whether the last parse hit the depth limit
    last_hit_depth_limit: bool,
    preserve_cache_across_parses: bool,
    parse_cache: MemoTable,
    cached_input: Option<String>,
    cached_recursion: Option<usize>,
    last_stats: ParserStats,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        register_grammar(grammar_store_key(&grammar), grammar.clone());
        let reserved_tokens: HashSet<String> = grammar.special_tokens.iter().cloned().collect();
        let mut forest = SppfForest::new();
        forest.set_grammar(grammar.clone());
        Self {
            grammar,
            forest,
            reserved_tokens,
            max_recursion: DEFAULT_MAX_RECURSION_DEPTH,
            persist_window: DEFAULT_PERSIST_WINDOW,
            max_persisted_memo_entries: DEFAULT_MAX_PERSISTED_MEMO_ENTRIES,
            last_hit_depth_limit: false,
            preserve_cache_across_parses: true,
            parse_cache: MemoTable::new(),
            cached_input: None,
            cached_recursion: None,
            last_stats: ParserStats::default(),
        }
    }

    /// Set the maximum recursion depth for left-recursive grammars (builder pattern)
    pub fn with_max_recursion(mut self, depth: usize) -> Self {
        self.max_recursion = depth;
        self
    }

    /// Set the maximum recursion depth for left-recursive grammars
    pub fn set_max_recursion(&mut self, depth: usize) {
        self.max_recursion = depth;
    }

    pub fn max_recursion(&self) -> usize {
        self.max_recursion
    }

    pub fn with_persist_window(mut self, window: usize) -> Self {
        self.persist_window = window;
        self
    }

    pub fn set_persist_window(&mut self, window: usize) {
        self.persist_window = window;
    }

    pub fn with_max_persisted_memo_entries(mut self, max_entries: usize) -> Self {
        self.max_persisted_memo_entries = max_entries.max(1);
        self
    }

    pub fn set_max_persisted_memo_entries(&mut self, max_entries: usize) {
        self.max_persisted_memo_entries = max_entries.max(1);
    }

    pub fn with_preserve_cache_across_parses(mut self, preserve: bool) -> Self {
        self.preserve_cache_across_parses = preserve;
        self
    }

    pub fn set_preserve_cache_across_parses(&mut self, preserve: bool) {
        self.preserve_cache_across_parses = preserve;
    }

    pub fn preserve_cache_across_parses(&self) -> bool {
        self.preserve_cache_across_parses
    }

    /// Returns whether the last `partial()` call hit the recursion depth limit.
    pub fn last_hit_depth_limit(&self) -> bool {
        self.last_hit_depth_limit
    }

    pub fn clear_cache(&mut self) {
        self.parse_cache.clear();
        self.cached_input = None;
        self.cached_recursion = None;
        clear_shared_memo();
        self.last_stats = ParserStats::default();
    }

    pub fn last_stats(&self) -> &ParserStats {
        &self.last_stats
    }

    pub fn cache_entry_count(&self) -> usize {
        self.parse_cache.len()
    }

    pub fn prefix(&mut self, input: &str) -> Result<PrefixState, ParseError> {
        self.prefix_with_seed(input, SeedMemo::empty())
    }

    pub fn advance(&mut self, prev: &PrefixState, input: &str) -> Result<PrefixState, ParseError> {
        if !input.starts_with(prev.input()) || prev.max_recursion() != self.max_recursion {
            return self.prefix(input);
        }

        let state = self.prefix_with_seed(input, prev.seed_memo())?;
        Ok(state)
    }

    pub fn advance_owned(
        &mut self,
        prev: PrefixState,
        input: &str,
    ) -> Result<PrefixState, ParseError> {
        if !input.starts_with(prev.input()) || prev.max_recursion() != self.max_recursion {
            return self.prefix(input);
        }

        let state = self.prefix_with_seed(input, prev.into_seed_memo())?;
        Ok(state)
    }

    /// Parse input and return a complete AST (simple interface).
    ///
    /// This is a convenience wrapper around `partial()` that:
    /// 1. Returns only complete parses
    /// 2. Returns a simple Result type for backward compatibility
    ///
    /// For more control (partial parses, depth info), use `partial()` directly.
    pub fn parse(&mut self, input: &str) -> Result<SppfForest, String> {
        match self.partial(input) {
            PartialParseOutcome::Success { ast, .. } => {
                // Re-tokenize to determine how many segments the full input contributes.
                let segments = self.tokenize(input).map_err(|e| e.to_string())?;
                let total_segments = segments.len();

                let complete_root = ast.root_ids().iter().any(|root_id| {
                    ast.consumed_segments(*root_id) == total_segments
                        && ast.node_is_complete(*root_id)
                });

                if complete_root {
                    Ok(ast)
                } else {
                    Err(format!(
                        "Parse error: no complete parse found consuming all {} tokens",
                        total_segments
                    ))
                }
            }
            PartialParseOutcome::Failure(e) => Err(e.to_string()),
        }
    }

    pub fn partial(&mut self, input: &str) -> PartialParseOutcome {
        let seed = self.take_seed_memo_for(input);
        match self.prefix_with_seed(input, seed) {
            Ok(prefix) => PartialParseOutcome::Success {
                ast: {
                    if self.preserve_cache_across_parses && !prefix.hit_depth_limit() {
                        self.parse_cache = prefix.seed_memo().memo.as_ref().clone();
                        self.cached_input = Some(input.to_string());
                        self.cached_recursion = Some(self.max_recursion);
                    }
                    prefix.into_forest()
                },
            },
            Err(err) => PartialParseOutcome::Failure(err),
        }
    }

    fn prefix_with_seed(&mut self, input: &str, seed: SeedMemo) -> Result<PrefixState, ParseError> {
        self.last_hit_depth_limit = false;
        self.last_stats = ParserStats::default();

        debug_trace!("parser2      ", "Starting parse of input: '{}'", input);

        let segments = self.tokenize(input).map_err(ParseError::Tokenization)?;
        debug_trace!("parser2      ", "Tokenized into {:?}", segments);

        let start_nt = self
            .grammar
            .start_nonterminal()
            .map(|s| s.to_string())
            .ok_or(ParseError::NoStartSymbol)?;

        debug_trace!("parser2      ", "Start nonterminal: {}", start_nt);

        let mut parse_state = ParseState::with_seed(seed);
        let mut forest = std::mem::take(&mut self.forest);
        let roots = self
            .parse_nonterminal(
                &segments,
                &start_nt,
                None,
                0,
                0,
                &mut parse_state,
                &mut forest,
            )
            .map_err(ParseError::Tokenization)?;

        let total_segments = segments.len();
        let depth_limited = parse_state.hit_depth_limit;
        self.last_hit_depth_limit = depth_limited;

        let mut seen_roots = HashSet::new();
        let valid_roots: Vec<SppfNodeId> = roots
            .into_iter()
            .filter(|r| r.consumed == total_segments)
            .map(|r| r.node_id)
            .filter(|node_id| seen_roots.insert(*node_id))
            .collect();

        if valid_roots.is_empty() {
            return Err(if depth_limited {
                ParseError::DepthLimit
            } else {
                ParseError::NoValidParse
            });
        }

        forest.set_grammar(self.grammar.clone());
        forest.set_roots(valid_roots);
        forest.set_input(input.to_string());

        let persisted_memo = self.persistable_memo(&parse_state.memo, total_segments);
        let frontier = self.persistable_frontier(&parse_state.frontier, total_segments);
        Ok(PrefixState::new(
            input.to_string(),
            segments,
            forest,
            persisted_memo,
            frontier,
            depth_limited,
            self.max_recursion,
        ))
    }

    fn take_seed_memo_for(&mut self, input: &str) -> SeedMemo {
        if !self.preserve_cache_across_parses {
            return SeedMemo::empty();
        }

        match (&self.cached_input, self.cached_recursion) {
            (Some(cached), Some(depth)) if input == cached && depth == self.max_recursion => {
                SeedMemo {
                    memo: Arc::new(std::mem::take(&mut self.parse_cache)),
                    frontier: Vec::new(),
                    total_segments: 0,
                }
            }
            _ => SeedMemo::empty(),
        }
    }

    fn persistable_memo(&self, table: &MemoTable, total_segments: usize) -> MemoTable {
        let cutoff = total_segments.saturating_sub(self.persist_window);
        let mut entries = table
            .iter()
            .filter_map(|(key, entry)| {
                let stable = entry.stable_only();
                (!stable.is_empty()).then(|| {
                    let class = if key.abs_pos == 0 {
                        0usize
                    } else if key.abs_pos >= cutoff {
                        1
                    } else {
                        2
                    };
                    let distance = total_segments.saturating_sub(key.abs_pos);
                    ((class, distance), key.clone(), stable)
                })
            })
            .collect::<Vec<_>>();

        entries.sort_by(|left, right| left.0.cmp(&right.0));
        entries
            .into_iter()
            .take(self.max_persisted_memo_entries)
            .map(|(_, key, entry)| (key, entry))
            .collect()
    }

    fn persistable_frontier(
        &self,
        frontier: &HashSet<ParseMemoKey>,
        total_segments: usize,
    ) -> Vec<ParseMemoKey> {
        let cutoff = total_segments.saturating_sub(self.persist_window);
        let mut keys = frontier
            .iter()
            .filter(|key| key.abs_pos >= cutoff || key.abs_pos == 0)
            .cloned()
            .collect::<Vec<_>>();
        keys.sort_by_key(|key| {
            (
                usize::from(key.abs_pos != 0),
                total_segments.saturating_sub(key.abs_pos),
            )
        });
        keys.truncate(self.max_persisted_memo_entries.min(keys.len()));
        keys
    }

    /// Tokenize input into segments using the grammar's tokenizer
    fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        self.grammar.tokenize(input)
    }

    fn memo_key(
        &self,
        nt_name: &str,
        binding: &Option<String>,
        abs_pos: usize,
        level: usize,
    ) -> ParseMemoKey {
        ParseMemoKey {
            nt_name: nt_name.to_string(),
            binding: binding.clone(),
            abs_pos,
            level,
        }
    }

    fn memo_lookup(
        &mut self,
        parse_state: &mut ParseState,
        key: &ParseMemoKey,
    ) -> Option<Vec<ParsedNt>> {
        if let Some(cached) = parse_state.memoized(key) {
            self.last_stats.nt_cache_hits += 1;
            return Some(cached);
        }

        if parse_state.seed_entry_is_exact(key) {
            self.last_stats.nt_cache_hits += 1;
            return Some(parse_state.seed_outcomes(key));
        }

        self.last_stats.nt_cache_misses += 1;
        None
    }

    fn memo_store(
        &mut self,
        parse_state: &mut ParseState,
        key: ParseMemoKey,
        value: Vec<ParsedNt>,
    ) -> Vec<ParsedNt> {
        self.last_stats.nt_cache_stores += 1;
        let entry = MemoEntry::from_outcomes(value.clone());
        if entry.has_partial() {
            parse_state.frontier.insert(key.clone());
        }
        parse_state.memo.insert(key, entry);
        value
    }

    fn parse_nonterminal(
        &mut self,
        segments: &[Segment],
        nt_name: &str,
        binding: Option<String>,
        abs_pos: usize,
        level: usize,
        parse_state: &mut ParseState,
        forest: &mut SppfForest,
    ) -> Result<Vec<ParsedNt>, String> {
        let indent = "  ".repeat(level);
        debug_trace!(
            "parser2      ",
            "{}[L{}] Parsing nonterminal '{}' at abs_pos {}",
            indent,
            level,
            nt_name,
            abs_pos
        );

        // Global depth limit based on max_recursion
        // This prevents exponential blowup from highly ambiguous grammars
        // like Expr ::= Expr Expr | Expr '+' Expr | ...
        // Users can adjust max_recursion via with_max_recursion() or MetaParser
        // to allow deeper parses for non-ambiguous grammars
        if level > self.max_recursion {
            debug_trace!(
                "parser2      ",
                "{}[L{}] Termination: Global depth limit exceeded (> {})",
                indent,
                level,
                self.max_recursion
            );
            // Mark that we hit the depth limit - results from this parse
            // should not be cached as they may be incomplete
            parse_state.hit_depth_limit = true;
            return Ok(Vec::new());
        }

        let key = self.memo_key(nt_name, &binding, abs_pos, level);

        if let Some(cached) = self.memo_lookup(parse_state, &key) {
            return Ok(cached);
        }

        if !parse_state.active.insert(key.clone()) {
            self.last_stats.cycle_cuts += 1;
            return Ok(Vec::new());
        }

        let productions = self
            .grammar
            .productions
            .get(nt_name)
            .cloned()
            .ok_or_else(|| format!("No productions for nonterminal '{}'", nt_name))?;

        // Shuffle productions using a PRNG seeded by level to avoid bias
        // This helps explore different parse alternatives at different depths,
        // preventing systematic bias toward earlier productions
        let shuffled_indices = prng_shuffle(productions.len(), level);

        let mut outcomes = parse_state.seed_outcomes(&key);
        let mut packed_nodes = outcomes
            .iter()
            .map(|parsed| (parsed.consumed, parsed.node_id))
            .collect::<HashMap<_, _>>();
        let mut seen = outcomes
            .iter()
            .map(|parsed| {
                let children = forest
                    .node(parsed.node_id)
                    .and_then(|node| node.alternatives.first().cloned())
                    .map(|alt| {
                        (
                            alt.alternative_index,
                            alt.children,
                            parsed.consumed,
                            parsed.complete,
                        )
                    })
                    .unwrap_or((usize::MAX, Vec::new(), parsed.consumed, parsed.complete));
                children
            })
            .collect::<HashSet<_>>();
        let grammar_name = forest.grammar_name().to_string();

        for &alt_idx in &shuffled_indices {
            let prod = productions.get(alt_idx).cloned().ok_or_else(|| {
                format!(
                    "No production index {} for nonterminal '{}'",
                    alt_idx, nt_name
                )
            })?;
            debug_trace!(
                "parser2      ",
                "{}[L{}] Trying production {}@{}: {} on {}",
                indent,
                level,
                nt_name,
                alt_idx,
                prod,
                segments
                    .iter()
                    .map(|s| s.text())
                    .collect::<Vec<String>>()
                    .join(" ")
            );

            match self.parse_production(segments, &prod, abs_pos, level, parse_state, forest) {
                Ok(prod_outcomes) => {
                    if prod_outcomes.is_empty() {
                        debug_trace!(
                            "parser2      ",
                            "{}[L{}] Production {}@{} produced no results",
                            indent,
                            level,
                            nt_name,
                            alt_idx
                        );
                        continue;
                    } else {
                        debug_trace!(
                            "parser2      ",
                            "{}[L{}] Production {}@{} succeeded with {} parse sequences",
                            indent,
                            level,
                            nt_name,
                            alt_idx,
                            prod_outcomes.len()
                        );
                        for children in prod_outcomes {
                            let (consumed, complete) =
                                self.children_summary(&children, prod.rhs.len());
                            let alt_children = children
                                .into_iter()
                                .map(|child| child.child)
                                .collect::<Vec<_>>();

                            if !seen.insert((alt_idx, alt_children.clone(), consumed, complete)) {
                                continue;
                            }

                            let node_id = *packed_nodes.entry(consumed).or_insert_with(|| {
                                register_node(
                                    &grammar_name,
                                    SppfNode {
                                        name: nt_name.to_string(),
                                        grammar: grammar_name.clone(),
                                        binding: binding.clone(),
                                        abs_pos,
                                        consumed_segments: consumed,
                                        alternatives: vec![],
                                        ty: None,
                                    },
                                )
                            });
                            forest.add_alternative(
                                node_id,
                                PackedAlternative {
                                    alternative_index: alt_idx,
                                    children: alt_children,
                                },
                            );
                            outcomes.push(ParsedNt {
                                node_id,
                                consumed,
                                complete,
                            });
                        }
                    }
                }
                Err(e) => {
                    debug_trace!(
                        "parser2      ",
                        "{}[L{}] Production {}@{} failed: {}",
                        indent,
                        level,
                        nt_name,
                        alt_idx,
                        e
                    );
                }
            }
        }

        parse_state.active.remove(&key);

        debug_trace!(
            "parser2      ",
            "{}[L{}] Finished parsing nonterminal '{}': {} trees",
            indent,
            level,
            nt_name,
            outcomes.len()
        );

        Ok(self.memo_store(parse_state, key, outcomes))
    }

    /// Parse a production (sequence of symbols)
    ///
    /// ## Algorithm
    /// Recursively parse each symbol in order, building up the parse tree
    /// This is the core of the recursive descent parser
    ///
    /// ## Time Complexity
    /// O(s) where s = number of symbols in production
    /// Each symbol parse is O(1) with memoization
    fn parse_production(
        &mut self,
        segments: &[Segment],
        prod: &Production,
        abs_pos: usize,
        level: usize,
        parse_state: &mut ParseState,
        forest: &mut SppfForest,
    ) -> Result<Vec<Vec<ParsedChild>>, String> {
        let indent = "  ".repeat(level);
        debug_trace!(
            "parser2.prod ",
            "{}[L{}] Parsing production: {:?}",
            indent,
            level,
            prod
        );

        // Epsilon production - matches empty input
        if prod.rhs.is_empty() {
            debug_trace!(
                "parser2.prod ",
                "{}[L{}] Epsilon production matched",
                indent,
                level
            );
            return Ok(vec![vec![]]);
        }

        self.parse_symbols(segments, &prod.rhs, abs_pos, level, parse_state, forest)
    }

    /// Parse a sequence of symbols
    ///
    /// ## Algorithm
    /// 1. Parse first symbol
    /// 2. For each successful parse, recursively parse remaining symbols
    /// 3. Combine results to form complete parse trees
    ///
    /// ## Time Complexity
    /// O(s * p)
    /// - s the number of symbols
    /// - p the number of average parses per symbol
    /// This is the main source of complexity in the parser
    fn parse_symbols(
        &mut self,
        segments: &[Segment],
        symbols: &[Symbol],
        abs_pos: usize,
        level: usize,
        parse_state: &mut ParseState,
        forest: &mut SppfForest,
    ) -> Result<Vec<Vec<ParsedChild>>, String> {
        // Base case: empty symbol list
        if symbols.is_empty() {
            return Ok(vec![vec![]]);
        }

        let first_sym = &symbols[0];
        let rest_syms = &symbols[1..];

        let first_parses =
            self.parse_symbol(segments, first_sym, abs_pos, level, parse_state, forest)?;

        // If no parses for first symbol, this production fails
        // ensure early exit
        if first_parses.is_empty() {
            return Ok(Vec::new());
        }

        let mut outcomes = Vec::with_capacity(first_parses.len());
        let mut rest_cache: HashMap<usize, Vec<Vec<ParsedChild>>> = HashMap::new();

        for node in first_parses {
            // Calculate remaining input after consuming this node
            let consumed: usize = node.consumed;

            // Optimization:
            //  - if node is partial, we can't continue this production
            // This prevents wasted work on incomplete prefixes
            if !node.complete {
                // store result on full consumption only
                if consumed == segments.len() {
                    outcomes.push(vec![node]);
                }
                continue;
            }

            let remaining_segments = segments.get(consumed..).unwrap_or(&[]);

            // Recursively parse remaining symbols with updated absolute position
            let new_abs_pos = abs_pos + consumed;
            let rest_parses = if let Some(cached) = rest_cache.get(&consumed) {
                self.last_stats.suffix_cache_hits += 1;
                cached
            } else {
                self.last_stats.suffix_cache_misses += 1;
                let parsed = self.parse_symbols(
                    remaining_segments,
                    rest_syms,
                    new_abs_pos,
                    level,
                    parse_state,
                    forest,
                )?;
                rest_cache.insert(consumed, parsed);
                rest_cache.get(&consumed).expect("rest cache inserted")
            };

            // Combine results
            for rest_nodes in rest_parses.iter() {
                let mut full_parse = Vec::with_capacity(1 + rest_nodes.len());
                full_parse.push(node.clone());
                full_parse.extend(rest_nodes.iter().cloned());
                self.last_stats.clone_events += 1 + rest_nodes.len();
                outcomes.push(full_parse);
            }
        }

        Ok(outcomes)
    }

    /// Count how many segments a node consumes
    ///
    /// ## Purpose
    /// Tracks parsing progress to ensure termination
    /// Each complete node must consume at least one segment
    ///
    /// ## Time Complexity
    /// idk but could be costly
    fn children_summary(&self, nodes: &[ParsedChild], rhs_len: usize) -> (usize, bool) {
        nodes
            .iter()
            .fold((0, nodes.len() == rhs_len), |(consumed, complete), node| {
                (consumed + node.consumed, complete && node.complete)
            })
    }

    /// Parse a symbol (expression or regex)
    /// Parse a single symbol (terminal or nonterminal)
    ///
    /// ## Algorithm
    /// Dispatch to appropriate parser based on symbol type:
    /// - Terminal: regex matching
    /// - Nonterminal: recursive parse
    ///
    /// ## Time Complexity
    /// O(1) dispatch + cost of specific parser
    /// Terminal: O(1) regex prefix match
    /// Nonterminal: O(p) where p is the number of productions
    fn parse_symbol(
        &mut self,
        segments: &[Segment],
        symbol: &Symbol,
        abs_pos: usize,
        level: usize,
        parse_state: &mut ParseState,
        forest: &mut SppfForest,
    ) -> Result<Vec<ParsedChild>, String> {
        let res = match symbol {
            Symbol::Terminal { regex, binding } => {
                self.parse_regex(segments, regex, binding.clone(), level)
            }
            Symbol::Nonterminal { name, binding } => {
                let nts = self.parse_nonterminal(
                    segments,
                    name,
                    binding.clone(),
                    abs_pos,
                    level + 1,
                    parse_state,
                    forest,
                )?;
                Ok(nts.into_iter().map(ParsedChild::from_nt).collect())
            }
        };
        res
    }

    /// Parse regex terminal
    ///
    /// ## Algorithm
    /// Uses regex derivatives for efficient prefix matching:
    /// 1. If at end of input, return partial match
    /// 2. Try to match regex against current segment
    /// 3. Handle four cases: Complete, Prefix, Extensible, NoMatch
    ///
    /// ## Time Complexity
    /// O(1) for prefix matching using regex derivatives
    /// Could be more because regex derivative computation is kinda bad
    fn parse_regex(
        &self,
        segments: &[Segment],
        re: &DerivativeRegex,
        binding: Option<String>,
        level: usize,
    ) -> Result<Vec<ParsedChild>, String> {
        // /!\ Important design choice
        // produce remaining prods on end
        // This means easier completion generation
        if segments.is_empty() {
            debug_trace!(
                "parser2.regex",
                "{}[L{}] At end of input, returning partial terminal",
                "  ".repeat(level),
                level
            );
            let node = ParsedChild {
                child: SppfChild::Terminal(Terminal::Partial {
                    value: String::new(),
                    binding: binding.clone(),
                    remainder: Some(re.clone()),
                }),
                consumed: 0,
                complete: false,
            };
            return Ok(vec![node]);
        }

        let seg = &segments[0];
        let seg_text = seg.as_str();
        if self.reserved_tokens.contains(seg_text) && !re.equiv(&DerivativeRegex::literal(seg_text))
        {
            return Ok(vec![]);
        }
        let indent = "  ".repeat(level);
        debug_trace!(
            "parser2.regex",
            "{}[L{}] Trying regex '{}' against segment '{}'",
            indent,
            level,
            re.to_pattern(),
            seg_text
        );

        let node = match re.prefix_match(seg_text) {
            PrefixStatus::Complete => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex FULL match for segment '{}'",
                    indent,
                    level,
                    seg_text
                );
                Some(ParsedChild {
                    child: SppfChild::Terminal(Terminal::Complete {
                        value: seg_text.to_string(),
                        binding: binding.clone(),
                        extension: None,
                    }),
                    consumed: 1,
                    complete: true,
                })
            }
            PrefixStatus::Prefix(derivative) => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex PARTIAL match for segment '{}'",
                    indent,
                    level,
                    seg_text
                );
                Some(ParsedChild {
                    child: SppfChild::Terminal(Terminal::Partial {
                        value: seg_text.to_string(),
                        binding: binding.clone(),
                        remainder: Some(derivative.clone()),
                    }),
                    consumed: 1,
                    complete: false,
                })
            }
            PrefixStatus::Extensible(derivative) => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex EXTENSIBLE match for segment '{}'",
                    indent,
                    level,
                    seg_text
                );
                Some(ParsedChild {
                    child: SppfChild::Terminal(Terminal::Complete {
                        value: seg_text.to_string(),
                        binding: binding.clone(),
                        extension: Some(derivative.clone()),
                    }),
                    consumed: 1,
                    complete: true,
                })
            }
            PrefixStatus::NoMatch => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex NO match for segment '{}'",
                    indent,
                    level,
                    seg_text
                );
                None
            }
        };

        Ok(node.into_iter().collect())
    }
}
