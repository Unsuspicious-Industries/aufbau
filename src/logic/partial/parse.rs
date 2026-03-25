use std::collections::{HashMap, HashSet};
use std::sync::Arc;

use crate::debug_trace;
use crate::logic::grammar::{Grammar, Production, Segment, Symbol};
use crate::logic::partial::structure::{
    PackedAlternative, SppfChild, SppfForest, SppfNode, SppfNodeId, Terminal,
};
use crate::logic::segment::SegmentRange;
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};

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
 * The memo does NOT persist across separate `partial()` calls — every
 * invocation starts from scratch. `clear_cache()` is a no-op kept for API
 * compatibility.
 */

/// Default maximum recursion depth for left-recursive grammars.
/// Limits depth to prevent exponential blowup on ambiguous grammars.
/// MetaParser enables adaptive depth finding. Override with `with_max_recursion`.
const DEFAULT_MAX_RECURSION_DEPTH: usize = 15;

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

/// Tracks parsing state for a single parse operation
///
/// This struct contains per-parse state that should NOT be shared across
/// multiple parse calls. It tracks recursion to detect cycles during parsing,
/// and memoizes completed sub-parses within a single invocation so the parser
/// runs in polynomial rather than exponential time.
struct ParseState {
    /// Tracks recursion depth for (non-terminal, absolute_position)
    /// Used to detect potential infinite loops within a single parse
    visited: HashMap<(String, usize), usize>,
    /// Set to true when we hit the depth limit during this parse.
    hit_depth_limit: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct ParsedNt {
    node_id: SppfNodeId,
    consumed: usize,
    complete: bool,
}

#[derive(Debug, Clone)]
struct ParsedChild {
    child: SppfChild,
    consumed: usize,
    complete: bool,
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
    pub forest: SppfForest,
    /// Precomputed reserved literal tokens (keywords/operators) for O(1) checks
    /// during regex terminal matching.
    reserved_tokens: HashSet<String>,
    /// Maximum recursion depth for left-recursive patterns like `Expr Expr`
    max_recursion: usize,
    /// Whether the last parse hit the depth limit
    last_hit_depth_limit: bool,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let reserved_tokens: HashSet<String> = grammar.special_tokens.iter().cloned().collect();
        let mut forest = SppfForest::new();
        forest.set_grammar_name(grammar.name.clone());
        Self {
            grammar,
            forest,
            reserved_tokens,
            max_recursion: DEFAULT_MAX_RECURSION_DEPTH,
            last_hit_depth_limit: false,
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

    /// Returns whether the last `partial()` call hit the recursion depth limit.
    pub fn last_hit_depth_limit(&self) -> bool {
        self.last_hit_depth_limit
    }

    /// No-op. The memoization cache is local to each `partial()` call and
    /// is discarded automatically when the call returns. Kept for API
    /// compatibility.
    pub fn clear_cache(&mut self) {}

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
        self.last_hit_depth_limit = false;

        debug_trace!("parser2      ", "Starting parse of input: '{}'", input);

        let outcome = (|| {
            let segments = match self.tokenize(input) {
                Ok(s) => s,
                Err(e) => return PartialParseOutcome::Failure(ParseError::Tokenization(e)),
            };
            debug_trace!("parser2      ", "Tokenized into {:?}", segments);

            let start_nt = match self.grammar.start_nonterminal() {
                Some(s) => s.to_string(),
                None => return PartialParseOutcome::Failure(ParseError::NoStartSymbol),
            };

            debug_trace!("parser2      ", "Start nonterminal: {}", start_nt);

            let mut parse_state = ParseState::new();
            let mut forest = std::mem::take(&mut self.forest);
            let roots = match self.parse_nonterminal(
                &segments,
                &start_nt,
                None,
                0,
                0,
                &mut parse_state,
                &mut forest,
            ) {
                Ok(r) => r,
                Err(e) => {
                    return PartialParseOutcome::Failure(ParseError::Tokenization(e));
                }
            };

            let total_segments = segments.len();
            let depth_limited = parse_state.hit_depth_limit;
            self.last_hit_depth_limit = depth_limited;

            let valid_roots: Vec<SppfNodeId> = roots
                .into_iter()
                .filter(|r| r.consumed == total_segments)
                .map(|r| r.node_id)
                .collect();

            if valid_roots.is_empty() {
                if depth_limited {
                    return PartialParseOutcome::Failure(ParseError::DepthLimit);
                }
                return PartialParseOutcome::Failure(ParseError::NoValidParse);
            }

            PartialParseOutcome::Success {
                ast: {
                    forest.set_roots(valid_roots);
                    forest.set_input(input.to_string());
                    forest
                },
            }
        })();
        outcome
    }

    /// Tokenize input into segments using the grammar's tokenizer
    fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        self.grammar.tokenize(input)
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

        // Check for recursion on same input position for cycle detection
        // Uses absolute position to correctly detect cycles
        let key = (nt_name.to_string(), abs_pos);

        // Clone productions to avoid borrow conflict with span cache
        let productions_len = self
            .grammar
            .productions
            .get(nt_name)
            .ok_or_else(|| format!("No productions for nonterminal '{}'", nt_name))?
            .len();

        // Shuffle productions using a PRNG seeded by level to avoid bias
        // This helps explore different parse alternatives at different depths,
        // preventing systematic bias toward earlier productions
        let shuffled_indices = prng_shuffle(productions_len, level);

        let mut outcomes = Vec::new();
        let mut seen = HashSet::new();

        for &alt_idx in &shuffled_indices {
            let prod = self
                .grammar
                .productions
                .get(nt_name)
                .and_then(|ps| ps.get(alt_idx))
                .ok_or_else(|| {
                    format!(
                        "No production index {} for nonterminal '{}'",
                        alt_idx, nt_name
                    )
                })?
                .clone();
            let prod_ref = Arc::new(prod);
            debug_trace!(
                "parser2      ",
                "{}[L{}] Trying production {}@{}: {} on {}",
                indent,
                level,
                nt_name,
                alt_idx,
                prod_ref,
                segments
                    .iter()
                    .map(|s| s.text())
                    .collect::<Vec<String>>()
                    .join(" ")
            );

            match self.parse_production(segments, &prod_ref, abs_pos, level, parse_state, forest) {
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
                            let consumed = self.count_consumed_segments(&children);
                            let complete =
                                self.children_are_complete(&children, prod_ref.rhs.len());
                            let node_id = forest.intern_node(SppfNode {
                                name: nt_name.to_string(),
                                grammar: forest.grammar_name().to_string(),
                                binding: binding.clone(),
                                abs_pos,
                                consumed_segments: 0,
                                alternatives: vec![],
                                ty: None,
                            });
                            forest.add_alternative(
                                node_id,
                                PackedAlternative {
                                    alternative_index: alt_idx,
                                    children: children.into_iter().map(|c| c.child).collect(),
                                },
                            );
                            let out = ParsedNt {
                                node_id,
                                consumed,
                                complete,
                            };
                            if seen.insert(out.clone()) {
                                outcomes.push(out);
                            }
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

        parse_state.visited.remove(&key);

        debug_trace!(
            "parser2      ",
            "{}[L{}] Finished parsing nonterminal '{}': {} trees",
            indent,
            level,
            nt_name,
            outcomes.len()
        );

        Ok(outcomes)
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
                cached.clone()
            } else {
                let parsed = self.parse_symbols(
                    remaining_segments,
                    rest_syms,
                    new_abs_pos,
                    level,
                    parse_state,
                    forest,
                )?;
                rest_cache.insert(consumed, parsed.clone());
                parsed
            };

            // Combine results
            for mut rest_nodes in rest_parses {
                let mut full_parse = vec![node.clone()];
                full_parse.append(&mut rest_nodes);
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
    fn count_consumed_segments(&self, nodes: &[ParsedChild]) -> usize {
        nodes.iter().map(|n| n.consumed).sum()
    }

    fn children_are_complete(&self, nodes: &[ParsedChild], rhs_len: usize) -> bool {
        nodes.len() == rhs_len && nodes.iter().all(|n| n.complete)
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
                Ok(nts
                    .into_iter()
                    .map(|nt| ParsedChild {
                        child: SppfChild::Node(nt.node_id),
                        consumed: nt.consumed,
                        complete: nt.complete,
                    })
                    .collect())
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
