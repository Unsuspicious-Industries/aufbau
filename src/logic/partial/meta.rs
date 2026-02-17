//! MetaParser - Adaptive recursion depth parser
//!
//! The MetaParser wraps a Parser and automatically finds the minimum recursion
//! depth needed to parse an input. It starts with a low depth and incrementally
//! increases until parsing succeeds.
//!
//! ## Why Start Low
//!
//! Starting at depth 1 handles highly ambiguous grammars efficiently:
//! - Ambiguous grammars like `Expr ::= Expr Expr | Expr '+' Expr` explode at high depths
//! - Starting low finds the minimum depth needed, avoiding exponential blowup
//! - For simple inputs, we find a parse quickly at low depth
//! - For complex inputs, we gradually increase until we find one
//!
//! ## Example
//!
//! ```text
//! // For input "x + x" with grammar Expr ::= Expr '+' Expr | 'x'
//! // Depth 1: Fails
//! // Depth 2: Fails  
//! // Depth 3: Succeeds! Returns parse tree
//! ```

use crate::debug_info;
use crate::logic::grammar::Grammar;
use crate::logic::partial::{ParseError, Parser, PartialAST, PartialParseOutcome};
use std::collections::HashMap;

/// Default starting recursion depth - start low to handle ambiguous grammars
const DEFAULT_START_DEPTH: usize = 5;

/// Default maximum recursion depth to try before giving up
const DEFAULT_MAX_DEPTH: usize = 100000;

/// Default multiplicative factor for recursion depth growth (1.5x)
const DEFAULT_DEPTH_FACTOR: f64 = 1.5;

/// MetaParser that automatically finds the right recursion depth
pub struct MetaParser {
    parser: Parser,
    /// Starting depth for search
    start_depth: usize,
    /// Maximum depth before giving up
    max_depth: usize,
    /// Multiplicative factor to grow depth each iteration (d <- ceil(d * factor))
    depth_factor: f64,
    /// Grammar start depth cache: for each grammar store the best (lowest)
    /// start depth found so far. Helps avoid re-scanning small depths for
    /// grammars that are known to require deeper recursion.
    gscache: HashMap<Grammar, usize>,
    /// Last input seen (for incremental parsing heuristics)
    last_input: Option<String>,
    /// Last successful depth (for incremental parsing heuristics)
    last_success_depth: Option<usize>,
}

impl MetaParser {
    /// Create a new MetaParser from a grammar
    pub fn new(grammar: Grammar) -> Self {
        Self {
            parser: Parser::new(grammar),
            start_depth: DEFAULT_START_DEPTH,
            max_depth: DEFAULT_MAX_DEPTH,
            depth_factor: DEFAULT_DEPTH_FACTOR,
            gscache: HashMap::new(),
            last_input: None,
            last_success_depth: None,
        }
    }

    /// Create from an existing parser (takes ownership)
    pub fn from_parser(parser: Parser) -> Self {
        Self {
            parser,
            start_depth: DEFAULT_START_DEPTH,
            max_depth: DEFAULT_MAX_DEPTH,
            depth_factor: DEFAULT_DEPTH_FACTOR,
            gscache: HashMap::new(),
            last_input: None,
            last_success_depth: None,
        }
    }

    /// Set the starting depth
    pub fn with_start_depth(mut self, depth: usize) -> Self {
        self.start_depth = depth;
        self
    }

    /// Set the maximum depth
    pub fn with_max_depth(mut self, depth: usize) -> Self {
        self.max_depth = depth;
        self
    }

    /// Set the multiplicative depth growth factor (must be > 1.0). Defaults to 1.5.
    /// Example: factor=1.5 will grow depths like 5 -> 8 -> 12 -> ... (ceil applied).
    pub fn with_depth_factor(mut self, factor: f64) -> Self {
        // Enforce a factor > 1.0 so depth makes progress; clamp conservatively.
        self.depth_factor = factor.max(1.01);
        self
    }
    //parse but discard depth
    pub fn parse(&mut self, input: &str) -> Result<PartialAST, String> {
        let ast = self.partial(input)?;
        Ok(PartialAST {
            roots: ast.completes(),
            input: ast.input,
        })
    }

    /// Parse for partial results (may be incomplete)
    ///
    /// Uses rich parse outcomes to intelligently determine when to stop:
    /// - If parse fails due to grammar mismatch, stops immediately (won't improve with depth)
    /// - If parse succeeds but was depth-limited, continues to higher depth
    /// - If parse succeeds and wasn't depth-limited, returns results (won't improve with depth)
    ///
    /// Returns the best partial AST found
    pub fn partial(&mut self, input: &str) -> Result<PartialAST, String> {
        // For incremental inputs, start from the last successful depth to avoid
        // repeated ascent through low depths on every prefix extension.
        // Otherwise fall back to grammar-level cache and configured start depth.
        let incremental = self
            .last_input
            .as_ref()
            .map_or(false, |prev| input.starts_with(prev));

        let mut current_depth = if incremental {
            self.last_success_depth
                .unwrap_or(self.start_depth)
                .max(self.start_depth)
        } else {
            match self.gscache.get(&self.parser.grammar) {
                Some(&best) => best.max(self.start_depth),
                None => self.start_depth,
            }
        };
        debug_info!(
            "meta",
            "MetaParser: Starting partial with initial depth {} (cached best for grammar: {})",
            current_depth,
            self.gscache.get(&self.parser.grammar).unwrap_or(&0)
        );

        while current_depth <= self.max_depth {
            self.parser.set_max_recursion(current_depth);

            match self.parser.partial(input) {
                PartialParseOutcome::Success { ast } => {
                    // Update gscache with the (potentially) better depth
                    let key = self.parser.grammar.clone();
                    self.gscache
                        .entry(key)
                        .and_modify(|e| {
                            if current_depth < *e {
                                *e = current_depth
                            }
                        })
                        .or_insert(current_depth);
                    debug_info!(
                        "meta",
                        "MetaParser: Success at depth {} (cached best for grammar: {})",
                        current_depth,
                        self.gscache[&self.parser.grammar]
                    );
                    self.last_input = Some(input.to_string());
                    self.last_success_depth = Some(current_depth);
                    // return result
                    return Ok(ast);
                }
                PartialParseOutcome::Failure(ParseError::DepthLimit) => {
                    // Compute multiplicative growth (ceil), but ensure we always increase by at least 1
                    let mut next_depth =
                        ((current_depth as f64) * self.depth_factor).ceil() as usize;
                    if next_depth <= current_depth {
                        next_depth = current_depth + 1;
                    }
                    debug_info!(
                        "meta",
                        "MetaParser: Incrementing at depth {} -> {} (factor={})",
                        current_depth,
                        next_depth,
                        self.depth_factor
                    );
                    current_depth = next_depth;
                }
                PartialParseOutcome::Failure(ParseError::NoValidParse) => {
                    if incremental {
                        let mut next_depth =
                            ((current_depth as f64) * self.depth_factor).ceil() as usize;
                        if next_depth <= current_depth {
                            next_depth = current_depth + 1;
                        }
                        debug_info!(
                            "meta",
                            "MetaParser: No valid parse at depth {} (incremental) -> {}",
                            current_depth,
                            next_depth
                        );
                        current_depth = next_depth;
                    } else {
                        // Grammar mismatch - won't improve with higher depth
                        // Stop immediately to avoid wasted work
                        debug_info!(
                            "meta",
                            "MetaParser: No valid parse at depth {} - stopping search",
                            current_depth
                        );
                        break;
                    }
                }
                PartialParseOutcome::Failure(_) => {
                    // Other errors (tokenization, no start symbol) - also won't improve
                    debug_info!(
                        "meta",
                        "MetaParser: Parse error at depth {} - stopping search",
                        current_depth
                    );
                    break;
                }
            }
        }

        self.last_input = Some(input.to_string());

        Err(format!(
            "No parse results after trying depths {} to {}",
            self.start_depth, self.max_depth
        ))
    }

    pub fn partial_typed(&mut self, input: &str) -> Result<PartialAST, String> {
        let ast = self.partial(input)?;
        ast.filter_typed(&self.parser.grammar)
    }

    /// Parse for partial results and return depth used (debugging/metrics)
    pub fn partial_with_depth(&mut self, input: &str) -> Result<(PartialAST, usize), String> {
        let ast = self.partial(input)?;
        Ok((ast, self.last_success_depth.unwrap_or(self.start_depth)))
    }

    /// Clear the parser's cache (useful when switching to completely different input)
    pub fn clear_cache(&mut self) {
        self.parser.clear_cache();
        self.last_input = None;
        self.last_success_depth = None;
    }

    /// Get a reference to the underlying parser
    pub fn parser(&self) -> &Parser {
        &self.parser
    }

    /// Get the cached best depth for the current grammar, if any
    pub fn cached_best_depth(&self) -> Option<usize> {
        self.gscache.get(&self.parser.grammar).copied()
    }

    /// Get a mutable reference to the underlying parser
    pub fn parser_mut(&mut self) -> &mut Parser {
        &mut self.parser
    }
}
