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

use crate::logic::grammar::Grammar;
use crate::logic::partial::{Parser, PartialAST};

/// Default starting recursion depth - start low to handle ambiguous grammars
const DEFAULT_START_DEPTH: usize = 5;

/// Default maximum recursion depth to try before giving up
const DEFAULT_MAX_DEPTH: usize = 100;

/// Default increment for recursion depth
const DEFAULT_DEPTH_INCREMENT: usize = 2;

/// MetaParser that automatically finds the right recursion depth
pub struct MetaParser {
    parser: Parser,
    /// Starting depth for search
    start_depth: usize,
    /// Maximum depth before giving up
    max_depth: usize,
    /// How much to increment depth each iteration
    depth_increment: usize,
}

impl MetaParser {
    /// Create a new MetaParser from a grammar
    pub fn new(grammar: Grammar) -> Self {
        Self {
            parser: Parser::new(grammar),
            start_depth: DEFAULT_START_DEPTH,
            max_depth: DEFAULT_MAX_DEPTH,
            depth_increment: DEFAULT_DEPTH_INCREMENT,
        }
    }

    /// Create from an existing parser (takes ownership)
    pub fn from_parser(parser: Parser) -> Self {
        Self {
            parser,
            start_depth: DEFAULT_START_DEPTH,
            max_depth: DEFAULT_MAX_DEPTH,
            depth_increment: DEFAULT_DEPTH_INCREMENT,
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

    /// Set the depth increment
    pub fn with_depth_increment(mut self, increment: usize) -> Self {
        self.depth_increment = increment.max(1); // At least 1
        self
    }

    /// Parse input, automatically finding the right recursion depth
    ///
    /// Returns the AST and the depth that succeeded
    pub fn meta_parse(&mut self, input: &str) -> Result<(PartialAST, usize), String> {
        let mut current_depth = self.start_depth;

        while current_depth <= self.max_depth {
            // Update parser's max recursion depth
            self.parser.set_max_recursion(current_depth);

            // Try parsing - cache from previous attempts is still valid!
            match self.parser.parse(input) {
                Ok(ast) => {
                    return Ok((ast, current_depth));
                }
                Err(_) => {
                    // Increase depth and try again
                    // Cache is preserved, so we only explore new paths
                    current_depth += self.depth_increment;
                }
            }
        }

        Err(format!(
            "Failed to parse after trying depths {} to {}",
            self.start_depth, self.max_depth
        ))
    }

    //parse but discard depth
    pub fn parse(&mut self, input: &str) -> Result<PartialAST, String> {
        let (ast, _depth) = self.meta_parse(input)?;
        Ok(ast)
    }

    /// Parse for partial results (may be incomplete)
    ///
    /// Returns the best partial AST found and the depth used
    pub fn meta_partial(&mut self, input: &str) -> Result<(PartialAST, usize), String> {
        let mut current_depth = self.start_depth;
        let mut best_result: Option<(PartialAST, usize)> = None;

        while current_depth <= self.max_depth {
            self.parser.set_max_recursion(current_depth);

            match self.parser.partial(input) {
                Ok(ast) => {
                    // Check if we have a complete parse
                    if ast.roots.iter().any(|r| r.is_complete()) {
                        return Ok((ast, current_depth));
                    }

                    // Keep track of best partial result
                    // (could add heuristics here for "best" - most segments consumed, etc.)
                    best_result = Some((ast, current_depth));
                    current_depth += self.depth_increment;
                }
                Err(_) => {
                    current_depth += self.depth_increment;
                }
            }
        }

        // Return best partial result if we have one
        best_result.ok_or_else(|| {
            format!(
                "No parse results after trying depths {} to {}",
                self.start_depth, self.max_depth
            )
        })
    }

    // partial but discard depth
    pub fn partial(&mut self, input: &str) -> Result<PartialAST, String> {
        let (ast, _depth) = self.meta_partial(input)?;
        Ok(ast)
    }

    /// Clear the parser's cache (useful when switching to completely different input)
    pub fn clear_cache(&mut self) {
        self.parser.clear_cache();
    }

    /// Get a reference to the underlying parser
    pub fn parser(&self) -> &Parser {
        &self.parser
    }

    /// Get a mutable reference to the underlying parser
    pub fn parser_mut(&mut self) -> &mut Parser {
        &mut self.parser
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::set_debug_level;

    #[test]
    fn test_meta_parser_simple() {
        let spec = r#"
        Expr ::= Expr '+' 'n' | 'n'
        start ::= Expr
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut mp = MetaParser::new(g);

        let (ast, depth) = mp.meta_parse("n + n + n").unwrap();
        assert!(ast.is_complete());
        println!("Parsed at depth {}", depth);
    }

    #[test]
    fn test_meta_parser_deep_nesting() {
        // Non-ambiguous deep nesting - just parentheses
        let spec = r#"
        Expr ::= '(' Expr ')' | 'f' Expr | 'x'
        start ::= Expr
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut mp = MetaParser::new(g);

        // This needs higher depth due to nesting
        let (ast, depth) = mp.meta_parse("f ( f ( x ) )").unwrap();
        assert!(ast.is_complete());
        println!("Parsed 'f ( f ( x ) )' at depth {}", depth);
    }

    #[test]
    fn test_meta_parser_highly_ambiguous() {
        set_debug_level(crate::DebugLevel::None);
        let spec = r#"
        Expr ::= Expr Expr | Expr '+' Expr | '(' Expr ')' | 'x'
        start ::= Expr
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut mp = MetaParser::new(g);

        let (ast, depth) = mp.meta_parse("x + x + x").unwrap();
        assert!(ast.is_complete());
        println!("Parsed highly ambiguous at depth {}", depth);
    }

    #[test]
    fn test_meta_parser_incremental_cache() {
        // Test that cache is preserved across depth increments
        let spec = r#"
        Expr ::= '(' Expr ')' | 'x'
        start ::= Expr
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut mp = MetaParser::new(g);

        // Deeply nested parens - needs increasing depth
        let (ast, depth) = mp.meta_parse("( ( ( x ) ) )").unwrap();
        assert!(ast.is_complete());
        println!("Parsed nested parens at depth {}", depth);
    }
}

