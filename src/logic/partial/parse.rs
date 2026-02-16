use std::collections::HashMap;
use std::time::Instant;

use super::cache::{is_prefix, SegmentKey};
use super::monitor::{CacheMonitor, CacheStatsSnapshot, CacheTimingSnapshot};
use super::SpanCache;
use crate::logic::grammar::{Grammar, Production, Segment, Symbol};
use crate::logic::partial::{Node, NonTerminal, PartialAST, Terminal};
use crate::logic::segment::SegmentRange;
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use crate::{debug_debug, debug_trace};

/// Rotate production indices by level (Caesar cipher style).
///
/// Shifts indices [0..n) by `level` positions, wrapping around.
/// This ensures different recursion depths try productions in different orders.
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
 * ## Cache Reuse Across Parses
 *
 * The parser maintains a persistent memoization cache that survives across
 * multiple `partial()` calls on the same `Parser` instance. This enables
 * incremental parsing scenarios where you parse progressively longer prefixes:
 *
 * ```text
 * parser.partial("x")        // Parses and caches results for "x"
 * parser.partial("x t")      // Reuses cache for "x", only parses " t"
 * parser.partial("x t +")    // Reuses cache for "x" and "x t", only parses " +"
 * ```
 *
 * This optimization is crucial for completion generation where we iteratively
 * extend partial inputs. The cache is span-keyed by
 * (nonterminal, start_segment_index, span_len, depth), and tracks how far a
 * given (nonterminal, start) has been fully computed for a prefix.
 *
 * To reset the cache (e.g., when switching to a completely different input),
 * use `clear_cache()` or create a new `Parser` instance.
 */

/// Default maximum recursion depth for left-recursive grammars.
/// Set to 20 which handles most real-world cases while preventing
/// exponential blowup on highly ambiguous grammars.
/// Use MetaParser for adaptive depth finding, or with_max_recursion() to override.
const DEFAULT_MAX_RECURSION_DEPTH: usize = 15;

/// Outcome of a partial parse operation with detailed metadata.
///
/// This provides rich information about the parse result, distinguishing between
/// depth-limited parses (which might improve with higher recursion limits) and
/// grammar mismatches (which will never succeed regardless of depth).
#[derive(Debug, Clone)]
pub enum PartialParseOutcome {
    /// Parse succeeded with at least one valid tree.
    Success { ast: PartialAST },
    /// Parse failed - input doesn't match the grammar.
    /// Unlike depth-limited results, this will NOT improve with higher recursion limits.
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
    /// Returns true if the parse produced at least one complete tree.
    pub fn is_complete(&self) -> bool {
        match self {
            Self::Success { ast } => ast.roots.iter().any(|r| r.is_complete()),
            _ => false,
        }
    }

    /// Returns true if parse succeeded (even if partial).
    pub fn is_success(&self) -> bool {
        match self {
            Self::Success { .. } => true,
            _ => false,
        }
    }

    /// Convert to simple Result for backward compatibility.
    ///
    /// Note: If the outcome has 0 roots (e.g., due to depth limit), this returns
    /// an error because an AST with no roots is not useful for most purposes.
    /// Use pattern matching on PartialParseOutcome directly if you need to handle
    /// depth-limited empty results specially.
    pub fn into_result(self) -> Result<PartialAST, ParseError> {
        match self {
            Self::Success { ast } if !ast.roots.is_empty() => Ok(ast),
            Self::Success { .. } => Err(ParseError::NoValidParse),
            Self::Failure(e) => Err(e),
        }
    }

    /// Get reference to AST if success.
    pub fn ast(&self) -> Option<&PartialAST> {
        match self {
            Self::Success { ast } => Some(ast),
            _ => None,
        }
    }

    /// Unwrap the AST, panicking if this was a failure or if roots are empty.
    pub fn unwrap(self) -> PartialAST {
        match self {
            Self::Success { ast } if !ast.roots.is_empty() => ast,
            Self::Success { .. } => panic!("Called unwrap on ParseSuccess with 0 roots"),
            Self::Failure(e) => panic!("Called unwrap on ParseFailure: {}", e),
        }
    }

    /// Unwrap with custom error message, panicking if this was a failure or if roots are empty.
    pub fn expect(self, msg: &str) -> PartialAST {
        match self {
            Self::Success { ast } if !ast.roots.is_empty() => ast,
            _ => panic!("{}", msg),
        }
    }

    /// Returns true for success (backward compatibility with Result).
    pub fn is_ok(&self) -> bool {
        self.is_success()
    }

    /// Unwrap the error, panicking if this was a success.
    pub fn unwrap_err(self) -> ParseError {
        match self {
            Self::Failure(e) => e,
            Self::Success { .. } => panic!("Called unwrap_err on successful parse"),
        }
    }
}

/// Tracks parsing state for a single parse operation
///
/// This struct contains per-parse state that should NOT be shared across
/// multiple parse calls. It tracks recursion to detect cycles during parsing.
struct ParseState {
    /// Tracks recursion depth for (non-terminal, absolute_position)
    /// Used to detect potential infinite loops within a single parse
    visited: HashMap<(String, usize), usize>,
    /// Set to true when we hit the depth limit during this parse.
    /// When true, we should not cache results because they may be incomplete
    /// due to early termination, and higher depths could find more results.
    hit_depth_limit: bool,
}

impl ParseState {
    fn new() -> Self {
        Self {
            visited: HashMap::new(),
            hit_depth_limit: false,
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
    /// Maximum recursion depth for left-recursive patterns like `Expr Expr`
    max_recursion: usize,
    /// Span-keyed memoization cache for parsing results.
    span_cache: SpanCache,
    /// Tracks the previous normalized input segments for cache invalidation.
    last_input_segments: Option<Vec<SegmentKey>>,
    /// Whether the last parse hit the depth limit
    last_hit_depth_limit: bool,

    cache_monitor: CacheMonitor,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let mut specials = grammar.special_tokens.clone();
        // Ensure longest-match for multi-char literals (e.g. "<=", ">=", "==")
        // by checking longer specials before their prefixes ("<", ">", "=").
        specials.sort_by(|a, b| b.len().cmp(&a.len()).then_with(|| a.cmp(b)));
        Self {
            grammar,
            max_recursion: DEFAULT_MAX_RECURSION_DEPTH,
            span_cache: SpanCache::default(),
            last_input_segments: None,
            last_hit_depth_limit: false,
            cache_monitor: CacheMonitor::new(),
        }
    }

    pub fn enable_cache_monitoring(&mut self, enabled: bool) {
        self.cache_monitor.set_enabled(enabled);
    }

    pub fn reset_cache_monitoring(&mut self) {
        self.cache_monitor.reset();
    }

    pub fn cache_stats(&self) -> CacheStatsSnapshot {
        self.cache_monitor.stats_snapshot()
    }

    pub fn cache_timing(&self) -> CacheTimingSnapshot {
        self.cache_monitor.timing_snapshot()
    }

    pub fn cache_report(&self, max_nonterminals: usize, max_entries_per_nt: usize) -> String {
        let counts = self.span_cache.counts();
        let stats = self.cache_stats();
        let timing = self.cache_timing();

        let mut out = String::new();
        out.push_str(&format!(
            "cache: enabled={} nts={} span_buckets={} trees={}\n",
            stats.enabled, counts.nt_count, counts.span_buckets, counts.stored_trees
        ));
        out.push_str(&format!(
            "lookups={} exact={} prefix={} miss={} scanned={}\n",
            stats.lookups,
            stats.lookup_hits_exact,
            stats.lookup_hits_prefix,
            stats.lookup_misses,
            stats.lookup_scanned_entries
        ));
        out.push_str(&format!(
            "stores={} inserts={} updates={} invalidations={} clears={} depth_limited={}\n",
            stats.stores,
            stats.store_inserts,
            stats.store_updates,
            stats.cache_invalidations,
            stats.cache_clears,
            stats.depth_limited_parses
        ));
        out.push_str(&format!(
            "time: partial_total={:?} lookup_total={:?} store_total={:?} partial_last={:?}\n",
            timing.partial_total, timing.lookup_total, timing.store_total, timing.partial_last
        ));

        out.push_str(&format!(
            "note: span cache only; max_nts={}, max_entries_per_nt={}\n",
            max_nonterminals, max_entries_per_nt
        ));

        out
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

    /// Returns whether the last `partial()` call hit the recursion depth limit.
    pub fn last_hit_depth_limit(&self) -> bool {
        self.last_hit_depth_limit
    }

    /// Clear the memoization cache.
    ///
    /// Use this when switching to a completely different input that doesn't
    /// share a common prefix with previous inputs. This frees memory and
    /// ensures stale cached results don't affect parsing.
    ///
    /// For incremental parsing of progressively longer inputs (e.g., "x" -> "x t" -> "x t +"),
    /// do NOT clear the cache as it provides the performance benefit.
    pub fn clear_cache(&mut self) {
        self.cache_monitor.record_cache_clear();
        self.span_cache.clear();
    }

    /// Parse input and return a complete AST (simple interface).
    ///
    /// This is a convenience wrapper around `partial()` that:
    /// 1. Returns only complete parses
    /// 2. Returns a simple Result type for backward compatibility
    ///
    /// For more control (partial parses, depth info), use `partial()` directly.
    pub fn parse(&mut self, input: &str) -> Result<PartialAST, String> {
        match self.partial(input) {
            PartialParseOutcome::Success { ast, .. } => {
                // Re-tokenize to determine how many segments the full input contributes.
                let segments = self.tokenize(input).map_err(|e| e.to_string())?;
                let total_segments = segments.len();

                // Find a complete tree that consumed all segments
                let complete_root = ast
                    .roots
                    .iter()
                    .find(|r| r.is_complete() && r.consumed_segments == total_segments);

                if complete_root.is_some() {
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

    /// Main entry point: parse input and return rich outcome with depth metadata.
    ///
    /// ## Cache Reuse
    ///
    /// The parser maintains a memoization cache across calls. Cache is reused when
    /// the new input starts with the previous input (incremental parsing scenario).
    /// Otherwise, the cache is cleared automatically.
    ///
    /// When extending input (e.g., "x" -> "x + y"), cache entries at the previous
    /// input boundary are invalidated since they may have different parses with
    /// more input available.
    ///
    /// Example of cache reuse:
    /// ```text
    /// parser.partial("x")      // Fresh parse, caches results
    /// parser.partial("x + y")  // Different input, cache cleared
    /// parser.partial("x + y")  // Same input, cache reused
    /// ```
    ///
    /// ## Return Value
    ///
    /// Returns `PartialParseOutcome`.
    /// Use `Parser::last_hit_depth_limit()` to check whether the last `partial()`
    /// invocation hit the recursion depth limit.
    pub fn partial(&mut self, input: &str) -> PartialParseOutcome {
        let start_time = Instant::now();
        self.last_hit_depth_limit = false;

        debug_trace!("parser2      ", "Starting parse of input: '{}'", input);

        // Tokenize
        let outcome = (|| {
            let segments = match self.tokenize(input) {
                Ok(s) => s,
                Err(e) => return PartialParseOutcome::Failure(ParseError::Tokenization(e)),
            };
            debug_debug!("parser2      ", "Tokenized into {:?}", segments);

            let normalized_segments = self.normalize_segments(&segments);
            if let Some(prev) = &self.last_input_segments {
                if !is_prefix(prev, &normalized_segments) {
                    debug_debug!("parser2      ", "Cache invalidated (input prefix mismatch)");
                    self.span_cache.clear();
                    self.cache_monitor.record_cache_invalidation();
                }
            }
            self.last_input_segments = Some(normalized_segments);

            // Get start nonterminal (clone to avoid borrow conflict)
            let start_nt = match self.grammar.start_nonterminal() {
                Some(s) => s.to_string(),
                None => return PartialParseOutcome::Failure(ParseError::NoStartSymbol),
            };

            debug_debug!("parser2      ", "Start nonterminal: {}", start_nt);

            // Parse from start with absolute position 0
            let mut parse_state = ParseState::new();
            let roots =
                match self.parse_nonterminal(&segments, &start_nt, None, 0, 0, &mut parse_state) {
                    Ok(r) => r,
                    Err(e) => {
                        // Internal errors from parse_nonterminal are typically grammar issues
                        return PartialParseOutcome::Failure(ParseError::Tokenization(e));
                    }
                };

            let total_segments = segments.len();
            let depth_limited = parse_state.hit_depth_limit;

            // record for callers: whether this parse hit the depth limit
            self.last_hit_depth_limit = depth_limited;
            if depth_limited {
                self.cache_monitor.record_depth_limited_parse();
            }

            // Filter roots that consumed all input
            let valid_roots: Vec<NonTerminal> = roots
                .into_iter()
                .filter(|r| r.consumed_segments == total_segments)
                .collect();

            if valid_roots.is_empty() {
                debug_debug!(
                    "parser2      ",
                    "No alternatives consuming {} segments for start symbol '{}'",
                    total_segments,
                    start_nt
                );
                // If we hit the depth limit, report that specifically so MetaParser can retry
                if depth_limited {
                    return PartialParseOutcome::Failure(ParseError::DepthLimit);
                }
                return PartialParseOutcome::Failure(ParseError::NoValidParse);
            }

            let ast = PartialAST::new(valid_roots, input.to_string());

            PartialParseOutcome::Success { ast }
        })();

        self.cache_monitor.record_partial(start_time.elapsed());
        outcome
    }

    /// Tokenize input into segments using the grammar's tokenizer
    fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        self.grammar.tokenize(input)
    }

    /// Parse a nonterminal: try all productions, return all valid trees
    ///
    /// ## Algorithm
    /// - Check memoization cache first (O(1) lookup)
    /// - Check for recursion cycles
    /// - Try each production in order
    /// - Cache successful complete results
    ///
    /// ## Time Complexity
    /// O(p * s) where p = number of productions, s = number of symbols in RHS
    /// Memoizations makes it O(1)
    ///
    /// ## Parameters
    /// - `segments`: The remaining input segments to parse
    /// - `nt_name`: The nonterminal to parse
    /// - `binding`: Optional binding name for the nonterminal
    /// - `abs_pos`: Absolute position in the original input (for cache keys)
    /// - `level`: Recursion depth (for debugging)
    /// - `parse_state`: Per-parse state for cycle detection
    fn parse_nonterminal(
        &mut self,
        segments: &[Segment],
        nt_name: &str,
        binding: Option<String>,
        abs_pos: usize,
        level: usize,
        parse_state: &mut ParseState,
    ) -> Result<Vec<NonTerminal>, String> {
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
            debug_debug!(
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

        // Check span cache first (only complete subtrees)
        if self.allow_cache_lookup(segments) {
            let max_len = segments.len();
            let t0 = if self.cache_monitor.enabled() {
                Some(Instant::now())
            } else {
                None
            };

            if self
                .span_cache
                .can_answer(self.max_recursion, nt_name, abs_pos, max_len)
            {
                let (cached_result, scanned) =
                    self.span_cache
                        .collect(self.max_recursion, nt_name, abs_pos, max_len);
                self.cache_monitor.record_lookup(scanned, true, false);
                if let Some(t0) = t0 {
                    self.cache_monitor.record_lookup_time(t0.elapsed());
                }
                debug_trace!(
                    "parser2      ",
                    "{}[L{}] Cache HIT for '{}' at span {}+{}",
                    indent,
                    level,
                    nt_name,
                    abs_pos,
                    max_len
                );
                return Ok(cached_result);
            }

            let computed_len = self
                .span_cache
                .computed_len(self.max_recursion, nt_name, abs_pos);
            if computed_len > 0 {
                self.cache_monitor.record_lookup(0, false, true);
            } else {
                self.cache_monitor.record_lookup(0, false, false);
            }
            if let Some(t0) = t0 {
                self.cache_monitor.record_lookup_time(t0.elapsed());
            }
        }

        // Check for recursion on same input position for cycle detection
        // Uses absolute position to correctly detect cycles
        let key = (nt_name.to_string(), abs_pos);
        if let Some(count) = parse_state.visited.get(&key) {
            let local_limit = self.max_recursion.min(segments.len().saturating_add(2));
            debug_trace!(
                "parser2      ",
                "{}[L{}] Recursion detected for '{}' at abs_pos {} depth {}",
                indent,
                level,
                nt_name,
                abs_pos,
                count
            );

            // Termination fallback
            // computer crashed too many times before
            if *count >= local_limit {
                debug_debug!(
                    "parser2      ",
                    "{}[L{}] Termination: Too much recursion (>= {}, remaining segments: {})",
                    indent,
                    level,
                    local_limit,
                    segments.len()
                );
                parse_state.hit_depth_limit = true;
                return Ok(Vec::new());
            } else {
                debug_trace!(
                    "parser2      ",
                    "{}[L{}] Continuing recursion (count: {})",
                    indent,
                    level,
                    count + 1
                );
                parse_state.visited.insert(key.clone(), count + 1);
            }
        } else {
            debug_trace!(
                "parser2      ",
                "{}[L{}] First parse attempt for '{}' at abs_pos {}",
                indent,
                level,
                nt_name,
                abs_pos
            );
            parse_state.visited.insert(key.clone(), 1);
        }

        // Clone productions to avoid borrow conflict with span cache
        let productions = self
            .grammar
            .productions
            .get(nt_name)
            .ok_or_else(|| format!("No productions for nonterminal '{}'", nt_name))?
            .clone();

        // Shuffle productions using a PRNG seeded by level to avoid bias
        // This helps explore different parse alternatives at different depths,
        // preventing systematic bias toward earlier productions
        let shuffled_indices = prng_shuffle(productions.len(), level);

        let mut results = Vec::new();

        for &alt_idx in &shuffled_indices {
            let prod = &productions[alt_idx];
            debug_debug!(
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

            match self.parse_production(segments, prod, abs_pos, level, parse_state) {
                Ok(prod_results) => {
                    if prod_results.is_empty() {
                        debug_debug!(
                            "parser2      ",
                            "{}[L{}] Production {}@{} produced no results",
                            indent,
                            level,
                            nt_name,
                            alt_idx
                        );
                        continue;
                    } else {
                        debug_debug!(
                            "parser2      ",
                            "{}[L{}] Production {}@{} succeeded with {} parse sequences",
                            indent,
                            level,
                            nt_name,
                            alt_idx,
                            prod_results.len()
                        );
                        for children in prod_results {
                            let consumed = self.count_consumed_segments(&children);
                            let nt = NonTerminal::new(
                                nt_name.to_string(),
                                prod.clone(),
                                alt_idx,
                                children,
                                binding.clone(),
                                consumed,
                            );
                            results.push(nt);
                        }
                    }
                }
                Err(e) => {
                    debug_debug!(
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

        parse_state.visited.remove(&key);

        debug_trace!(
            "parser2      ",
            "{}[L{}] Finished parsing nonterminal '{}': {} trees",
            indent,
            level,
            nt_name,
            results.len()
        );

        // Cache optimization: only store complete results that made progress
        // AND only when we didn't hit the depth limit during this parse.
        // This ensures:
        // 1. We don't cache failures (waste of space)
        // 2. We don't cache partial results (could lead to infinite loops)
        // 3. We only cache when we've actually done useful work
        // 4. We don't cache depth-limited results (higher depths might find more)
        if !results.is_empty() && !parse_state.hit_depth_limit && self.allow_cache_store(segments) {
            let mut by_len: HashMap<usize, Vec<NonTerminal>> = HashMap::new();
            for nt in results.iter() {
                if nt.is_complete() && nt.consumed_segments > 0 {
                    by_len
                        .entry(nt.consumed_segments)
                        .or_default()
                        .push(nt.clone());
                }
            }

            if !by_len.is_empty() {
                debug_trace!(
                    "parser2      ",
                    "{}[L{}] Caching complete results for '{}'",
                    indent,
                    level,
                    nt_name
                );

                let t0 = if self.cache_monitor.enabled() {
                    Some(Instant::now())
                } else {
                    None
                };

                let mut updates: u64 = 0;
                let mut inserts: u64 = 0;
                for (len, trees) in by_len {
                    let existed = self.span_cache.store_span(
                        self.max_recursion,
                        nt_name,
                        abs_pos,
                        len,
                        trees,
                    );
                    if existed {
                        updates += 1;
                    } else {
                        inserts += 1;
                    }
                }

                self.span_cache
                    .mark_computed(self.max_recursion, nt_name, abs_pos, segments.len());

                let total_entries_after = self.span_cache.counts().span_buckets;
                self.cache_monitor
                    .record_store(updates, inserts, total_entries_after);
                if let Some(t0) = t0 {
                    self.cache_monitor.record_store_time(t0.elapsed());
                }
            }
        }

        Ok(results)
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
    ) -> Result<Vec<Vec<Node>>, String> {
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

        self.parse_symbols(segments, &prod.rhs, abs_pos, level, parse_state)
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
    ) -> Result<Vec<Vec<Node>>, String> {
        // Base case: empty symbol list
        if symbols.is_empty() {
            return Ok(vec![vec![]]);
        }

        let first_sym = &symbols[0];
        let rest_syms = &symbols[1..];

        let first_parses = self.parse_symbol(segments, first_sym, abs_pos, level, parse_state)?;

        // If no parses for first symbol, this production fails
        // ensure early exit
        if first_parses.is_empty() {
            return Ok(Vec::new());
        }

        let mut results = Vec::with_capacity(first_parses.len());
        let mut rest_cache: HashMap<usize, Vec<Vec<Node>>> = HashMap::new();

        for node in first_parses {
            // Calculate remaining input after consuming this node
            let consumed: usize = self.node_consumed(&node);

            // Optimization:
            //  - if node is partial, we can't continue this production
            // This prevents wasted work on incomplete prefixes
            if !node.is_complete() {
                // store result on full consumption only
                if consumed == segments.len() {
                    results.push(vec![node]);
                }
                continue;
            }

            let remaining_segments = segments.get(consumed..).unwrap_or(&[]);

            // Recursively parse remaining symbols with updated absolute position
            let new_abs_pos = abs_pos + consumed;
            let rest_parses = if let Some(cached) = rest_cache.get(&consumed) {
                cached.clone()
            } else {
                let parsed = self.parse_symbols(
                    remaining_segments,
                    rest_syms,
                    new_abs_pos,
                    level,
                    parse_state,
                )?;
                rest_cache.insert(consumed, parsed.clone());
                parsed
            };

            // Combine results
            for mut rest_nodes in rest_parses {
                let mut full_parse = vec![node.clone()];
                full_parse.append(&mut rest_nodes);
                results.push(full_parse);
            }
        }

        Ok(results)
    }

    /// Count how many segments a node consumes
    ///
    /// ## Purpose
    /// Tracks parsing progress to ensure termination
    /// Each complete node must consume at least one segment
    ///
    /// ## Time Complexity
    /// idk but could be costly
    fn node_consumed(&self, node: &Node) -> usize {
        match node {
            Node::Terminal(Terminal::Complete { .. }) => 1,
            Node::Terminal(Terminal::Partial { value, .. }) => {
                if !value.is_empty() {
                    1
                } else {
                    0
                }
            }
            Node::NonTerminal(nt) => nt.consumed_segments,
        }
    }

    fn count_consumed_segments(&self, nodes: &[Node]) -> usize {
        nodes.iter().map(|n| self.node_consumed(n)).sum()
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
    ) -> Result<Vec<Node>, String> {
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
                )?;
                Ok(nts.into_iter().map(Node::NonTerminal).collect())
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
    ) -> Result<Vec<Node>, String> {
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
            let node = Node::Terminal(Terminal::Partial {
                value: String::new(),
                binding: binding.clone(),
                remainder: Some(re.clone()),
            });
            return Ok(vec![node]);
        }

        let seg = &segments[0];
        let seg_text = seg.as_str();
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
                Some(Node::Terminal(Terminal::Complete {
                    value: seg_text.to_string(),
                    binding: binding.clone(),
                    extension: None,
                }))
            }
            PrefixStatus::Prefix(derivative) => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex PARTIAL match for segment '{}'",
                    indent,
                    level,
                    seg_text
                );
                Some(Node::Terminal(Terminal::Partial {
                    value: seg_text.to_string(),
                    binding: binding.clone(),
                    remainder: Some(derivative.clone()),
                }))
            }
            PrefixStatus::Extensible(derivative) => {
                debug_trace!(
                    "parser2.regex",
                    "{}[L{}] Regex EXTENSIBLE match for segment '{}'",
                    indent,
                    level,
                    seg_text
                );
                Some(Node::Terminal(Terminal::Complete {
                    value: seg_text.to_string(),
                    binding: binding.clone(),
                    extension: Some(derivative.clone()),
                }))
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

    fn normalize_segments(&self, segments: &[Segment]) -> Vec<SegmentKey> {
        segments
            .iter()
            .map(|s| SegmentKey {
                text: s.text(),
                is_partial_special: s.is_partial_special,
            })
            .collect()
    }

    fn allow_cache_lookup(&self, segments: &[Segment]) -> bool {
        if segments.is_empty() {
            return false;
        }
        !segments.last().map_or(false, |s| s.is_partial_special)
    }

    fn allow_cache_store(&self, segments: &[Segment]) -> bool {
        if segments.is_empty() {
            return false;
        }
        !segments.last().map_or(false, |s| s.is_partial_special)
    }
}
