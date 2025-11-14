
use std::fmt;

 

use crate::{debug_debug, debug_trace};
use crate::logic::ast::{ASTNode, SegmentRange};
use crate::logic::grammar::{Grammar, Production, Symbol};
use crate::logic::tokenizer::{Tokenizer, Segment};
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use crate::logic::partial::{
    Alt, Node, NonTerminal, PartialAST, Slot, Terminal
};

impl Segment {
    /// Get the segment range (just its own index)
    pub fn seg_range(&self) -> SegmentRange {
        SegmentRange::single(self.index)
    }
}

#[derive(Clone, Debug)]
pub enum ParseResult {
    /// Symbol failed to match (mismatch)
    Fail,
    /// Symbol matched but only with partial branches (no complete parse)
    Partial(Vec<Node>),
    /// Symbol matched with at least one (and at most one) complete branch
    Complete(Vec<Node>),
}

impl ParseResult {
    /// Create a ParseResult from nodes, determining if they represent a complete or partial parse
    #[allow(dead_code)]
    fn from_nodes(nodes: Vec<Node>) -> Self {
        if nodes.is_empty() {
            return ParseResult::Fail;
        }
        
        // Check if any node is complete
        let has_complete = nodes.iter().any(|node| match node {
            Node::Terminal(Terminal::Complete { extension, .. }) => extension.is_none(),
            Node::Terminal(Terminal::Partial { .. }) => false,
            Node::NonTerminal(nt) => nt.is_complete(),
        });
        
        if has_complete {
            ParseResult::Complete(nodes)
        } else {
            ParseResult::Partial(nodes)
        }
    }

    /// Map the underlying nodes for Partial / Complete variants, preserving the outer variant
    fn map_nodes<F>(self, f: F) -> Self
    where
        F: FnOnce(Vec<Node>) -> Vec<Node>,
    {
        match self {
            ParseResult::Fail => ParseResult::Fail,
            ParseResult::Partial(nodes) => ParseResult::Partial(f(nodes)),
            ParseResult::Complete(nodes) => ParseResult::Complete(f(nodes)),
        }
    }
}

impl fmt::Display for ParseResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseResult::Fail => write!(f, "Fail"),
            ParseResult::Partial(nodes) => {
                write!(f, "Partial(nodes={:?})", nodes)
            }
            ParseResult::Complete(nodes) => {
                write!(f, "Complete(nodes={:?})", nodes)
            }
        }
    }
}

pub struct Parser {
    grammar: Grammar,
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let specials = grammar.special_tokens.clone();
        let validation_regex = grammar.accepted_tokens_regex.clone();
        Self {
            grammar,
            // hardcoded special delimiters for now
            tokenizer: Tokenizer::new(
                specials,
                vec![' ', '\n', '\t'],
                validation_regex,
            ),
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<ASTNode, String> {
        let past = self.partial(input)
            .map_err(|e| format!("Parse error: {}", e))?;
        let cast = past.into_complete()
            .map_err(|e| format!("Incomplete parse: {}", e))?;
        Ok(cast)
    }

    /// Main entry point: parse input and return new PartialAST
    pub fn partial(&mut self, input: &str) -> Result<PartialAST, String> {
        debug_trace!("parser2      ", "Starting parse of input: '{}'", input);
        
        // Tokenize
        let segments = self.tokenize(input)?;
        debug_debug!("parser2      ", "Tokenized into {:?}", segments);
        
        // Get start nonterminal
        let start_nt = self.grammar.start_nonterminal()
            .ok_or_else(|| "No start nonterminal in grammar".to_string())?;
        
        debug_debug!("parser2      ", "Start nonterminal: {}", start_nt);
        
        // Parse from start (allow partial consumption for partial parsing)
        let root = self.parse_nonterminal(&segments, start_nt,  None,0)?;
        if root.alts.is_empty() {
            debug_debug!("parser2      ", "No alternatives found for start symbol '{}'", start_nt);
            return Err("No valid parse alternatives found".to_string());
        }

        let ast = PartialAST::new(root, input.to_string());
        
        // do type filtering here
        Ok(ast)
    }

    /// Tokenize input into segments
    fn tokenize(&self, input: &str) -> Result<Vec<Segment>, String> {
        self.tokenizer
            .tokenize(input)
            .map_err(|e| format!("Tokenization failed: {:?}", e))
    }

    /// Parse a nonterminal: try all productions, keep valid and partial alternatives
    fn parse_nonterminal(
        &self,
        segments: &[Segment],
        nt_name: &str,
        binding: Option<String>,
        level: usize,
    ) -> Result<NonTerminal, String> {
        let indent = "  ".repeat(level);
        debug_trace!("parser2      ", "{}[L{}] Parsing nonterminal '{}'", 
            indent, level, nt_name);
        
        let productions = self.grammar.productions.get(nt_name)
            .ok_or_else(|| format!("No productions for nonterminal '{}'", nt_name))?;
        
        let mut alts = Vec::new();
        
        for prod in productions {
            debug_trace!("parser2      ", "{}[L{}] Trying production: {:?}", indent, level, prod.rhs);   
            match self.parse_production(segments, &prod,  level) {
                Ok(Some(alt)) => {
                    debug_trace!("parser2      ", "{}[L{}] Production succeeded: complete={}", indent, level, alt.is_complete(segments));
                    alts.push(alt);
                }
                Ok(None) => {
                    debug_trace!("parser2      ", "{}[L{}] Production failed (mismatch)", indent, level);
                }
                Err(e) => {
                    debug_trace!("parser2      ", "{}[L{}] Production error: {}", indent, level, e);
                }
            }
        }

        debug_trace!("parser2      ", "{}[L{}] Finished parsing nonterminal '{}': {} alternatives",
            indent, level, nt_name, alts.len());

        Ok(NonTerminal {
            name: nt_name.to_string(),
            alts,
            binding,
        })
    }

    fn parse_production(
        &self,
        segments: &[Segment],
        prod: &Production,
        level: usize
    ) -> Result<Option<Alt>, String> {
        let indent = "  ".repeat(level);
        debug_trace!("parser2.prod ", "{}[L{}] Parsing production: {:?}", indent, level, prod);
        
        let mut slots: std::collections::HashMap<usize, Slot> = std::collections::HashMap::new();

        let symbol_results = match self.parse_symbols_with_info(segments, &prod.rhs, level)? {
            Some(n) => n,
            None => {
                debug_trace!("parser2.prod ", "{}[L{}] Production failed to match any symbols", indent, level);
                return Ok(None);
            }
        };        

        // symbol_results contains (nodes, repetition) for each symbol
        for (i, (nodes, repetition)) in symbol_results.into_iter().enumerate() {
            slots.insert(i, Slot {
                nodes,
                repetition,
            });
        }
    
        debug_trace!("parser2.prod ", "{}[L{}] Production fully matched", indent, level);
        Ok(Some(Alt {
            production: prod.clone(),
            slots
        }))
    }

    /// Parse symbols and return nodes along with repetition info for each symbol
    fn parse_symbols_with_info(
        &self,
        segments: &[Segment],
        symbols: &[Symbol],
        level: usize,
    ) -> Result<Option<Vec<(Vec<Node>, (usize, Option<usize>))>>, String> {
        
        if symbols.is_empty() { return Ok(None); }
        
        // Get repetition info from the symbol
        let repetition = match &symbols[0] {
            Symbol::Single { repetition, .. } => {
                repetition.unwrap_or((1, Some(1)))
            }
            _ => (1, Some(1)), // Non-repetition symbols are exactly once
        };
        
        match self.parse_symbol(segments, &symbols[0], level)? {
            ParseResult::Complete(nodes) | ParseResult::Partial(nodes) => {
                // Determine consumption based on all nodes
                let consumed = self.count_consumed_segments(&nodes, segments, 0);

                // Continue with rest of symbols if any
                let mut result = vec![(nodes, repetition)];
                if symbols.len() > 1 {
                    if consumed < segments.len() {
                        if let Some(mut rest_symbols) = self.parse_symbols_with_info(&segments[consumed..], &symbols[1..], level)? {
                            result.append(&mut rest_symbols);
                        }
                    } else if consumed == segments.len() {
                        // Exactly consumed all segments, continue with empty segments for remaining symbols
                        if let Some(mut rest_symbols) = self.parse_symbols_with_info(&[], &symbols[1..], level)? {
                            result.append(&mut rest_symbols);
                        }
                    }
                }
                
                Ok(Some(result))
            }
            ParseResult::Fail => return Ok(None),
        }
    }

    /// Count how many segments are consumed by a list of nodes
    /// offset is the starting position in the original segments array
    fn count_consumed_segments(&self, nodes: &[Node], all_segments: &[Segment], offset: usize) -> usize {
        let mut total = 0;
        for node in nodes {
            match node {
                Node::Terminal(_) => total += 1,
                Node::NonTerminal(nt) => {
                    // Pass the segments from the current offset + what we've consumed so far
                    if let Some(range) = nt.floor_best_len(&all_segments[offset + total..]) {
                        let consumed = range.end - range.start + 1;
                        total += consumed;
                    } else {
                        // Partial - assume consumes rest
                        return all_segments.len() - offset;
                    }
                }
            }
        }
        total
    }

    /// Parse a symbol
    fn parse_symbol(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        level: usize,
    ) -> Result<ParseResult, String> {
        let res = match symbol {
            Symbol::Litteral(lit) => self.parse_literal(segments, lit, level),
            Symbol::Regex(re) => self.parse_regex(segments, re, level),
            Symbol::Expression(nt) => {
                self.parse_expression(segments, nt, level)
            },
            Symbol::Single { value, binding, repetition } => {
                self.parse_single(segments, value.as_ref(), repetition.clone(), binding.clone(), level)
            }
        };
        debug_trace!("parser2.sym ", "{}[L{}] Symbol parse result: {:?}", "  ".repeat(level), level, &res);
        res
    }

    /// Parse literal terminal
    fn parse_literal(
        &self,
        segments: &[Segment],
        lit: &str,
        level: usize,
    ) -> Result<ParseResult, String> {
        self.parse_regex(segments, &DerivativeRegex::literal(lit), level)
    }

    /// Parse regex terminal
    fn parse_regex(
        &self,
        segments: &[Segment],
        re: &DerivativeRegex,
        level: usize,
    ) -> Result<ParseResult, String> {
        if segments.is_empty() {
            // At end of input - partial match with remainder
            debug_trace!("parser2.regex", "{}[L{}] At end of input, returning partial terminal", "  ".repeat(level), level);
            return Ok(ParseResult::Partial(vec![Node::Terminal(Terminal::Partial {
                value: String::new(),
                binding: None,
                remainder: Some(re.clone()),
            })]));
        }

        let seg = &segments[0];
        let indent = "  ".repeat(level);
        debug_trace!("parser2.regex", "{}[L{}] Trying regex '{}' against segment '{}'", indent, level, re.to_pattern(), seg.text());
        
        match re.prefix_match(&seg.text()) {
            PrefixStatus::Complete => {
                debug_trace!("parser2.regex", "{}[L{}] Regex FULL match for segment '{}'", indent, level, seg.text());
                Ok(ParseResult::Complete(vec![Node::Terminal(Terminal::Complete {
                    value: seg.text().to_string(),
                    binding: None,
                    extension: None,
                })]))
            }
            PrefixStatus::Prefix(derivative) => {
                debug_trace!("parser2.regex", "{}[L{}] Regex PARTIAL match for segment '{}'", indent, level, seg.text());
                Ok(ParseResult::Partial(vec![Node::Terminal(Terminal::Partial {
                    value: seg.text().to_string(),
                    binding: None,
                    remainder: Some(derivative.clone()),
                })]))
            }
            PrefixStatus::Extensible(derivative) => {
                debug_trace!("parser2.regex", "{}[L{}] Regex EXTENSIBLE match for segment '{}'", indent, level, seg.text());
                Ok(ParseResult::Complete(vec![Node::Terminal(Terminal::Complete {
                    value: seg.text().to_string(),
                    binding: None,
                    extension: Some(derivative.clone()),
                })]))
            }
            PrefixStatus::NoMatch => {
                debug_trace!("parser2.regex", "{}[L{}] Regex NO match for segment '{}'", indent, level, seg.text());
                Ok(ParseResult::Fail)
            }
        }
    }

    /// Parse expression (nonterminal reference)
    fn parse_expression(
        &self,
        segments: &[Segment],
        nt: &str,
        level: usize,
    ) -> Result<ParseResult, String> {
        let nt_result = self.parse_nonterminal(segments, nt, None, level + 1)?;
        
        debug_trace!("parser2.expr ", "{}[L{}] Nonterminal '{}'",
            "  ".repeat(level), level, nt);

        if nt_result.alts.is_empty() {
            Ok(ParseResult::Fail)
        } else if nt_result.is_complete() {
            Ok(ParseResult::Complete(vec![Node::NonTerminal(nt_result)]))
        } else {
            // Partial - still return what we have
            Ok(ParseResult::Partial(vec![Node::NonTerminal(nt_result)]))
        }
    }

    /// Parse single symbol with optional repetition
    fn parse_single(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        repetition: Option<(usize, Option<usize>)>,
        binding: Option<String>,
        level: usize,
    ) -> Result<ParseResult, String> {
        let result = match repetition {
            None => {
                // No repetition - just parse the symbol once
                self.parse_symbol(segments, symbol, level)?
            },
            Some((min, max)) => self.parse_repetition(segments, symbol, min, max, level)?,
        };
        
        // Apply binding to all nodes if present
        if let Some(bind_name) = binding {
            let bind_name_cloned = bind_name.clone();
            let mapped = result.map_nodes(|mut nodes| {
                for node in &mut nodes {
                    match node {
                        Node::NonTerminal(nt) => {
                            nt.binding = Some(bind_name_cloned.clone());
                        }
                        Node::Terminal(Terminal::Complete { binding: b, .. }) => {
                            *b = Some(bind_name_cloned.clone());
                        }
                        Node::Terminal(Terminal::Partial { binding: b, .. }) => {
                            *b = Some(bind_name_cloned.clone());
                        }
                    }
                }
                nodes
            });
            return Ok(mapped);
        }

        Ok(result)
    }


    /// Parse repetition of a single symbol
    /// Returns a ParseResult with collected repetitions
    /// If the repetition is extensible (can match more), this is indicated by having
    /// a complete last node with an extension, or by having consumed fewer segments than available
    fn parse_repetition(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        min: usize,
        max: Option<usize>,
        level: usize,
    ) -> Result<ParseResult, String> {
        let mut nodes = Vec::new();
        let mut seg_offset = 0;
        let mut count = 0;
        let mut has_partial = false;
        
        loop {
            // Check if we've hit max
            if let Some(max_count) = max {
                if count >= max_count {
                    break;
                }
            }
            
            // Try to parse one more occurrence
            let remaining_segments = &segments[seg_offset..];
            if remaining_segments.is_empty() {
                // No more input - don't try to create partial continuations
                // (partials are only for when there ARE segments that could partially match)
                break;
            }
            
            let result = self.parse_symbol(remaining_segments, symbol, level)?;
            let is_partial_result = matches!(result, ParseResult::Partial(_));
            
            match result {
                ParseResult::Complete(mut parsed_nodes) | ParseResult::Partial(mut parsed_nodes) => {
                    // parsed_nodes is now Vec<Node>
                    if parsed_nodes.is_empty() {
                        break;
                    }
                    
                    // Check if this is a complete or partial match
                    let (consumed, is_complete) = match &parsed_nodes[0] {
                        Node::Terminal(Terminal::Complete { extension, .. }) => {
                            (1, extension.is_none())
                        }
                        Node::Terminal(Terminal::Partial { .. }) => {
                            // Partial terminal - include it if we've met minimum and it consumes rest of input
                            if count >= min && remaining_segments.len() == 1 {
                                nodes.append(&mut parsed_nodes);
                                return Ok(ParseResult::Partial(nodes));
                            } else {
                                // Haven't met minimum or doesn't consume all, stop
                                break;
                            }
                        },
                        Node::NonTerminal(nt) => {
                            // Check if nonterminal is complete
                            if let Some(range) = nt.complete_len(&segments[seg_offset..]) {
                                let consumed = range.end - range.start + 1;
                                (consumed, true)
                            } else {
                                // Partial nonterminal - include if we've met minimum and it consumes rest
                                if count >= min {
                                    let nt_consumes = self.count_consumed_segments(&parsed_nodes, &segments[seg_offset..], 0);
                                    if seg_offset + nt_consumes >= segments.len() {
                                        nodes.append(&mut parsed_nodes);
                                        return Ok(ParseResult::Partial(nodes));
                                    }
                                }
                                break;
                            }
                        }
                    };
                    
                    // Only add complete matches to the list
                    if is_complete {
                        nodes.append(&mut parsed_nodes);
                        seg_offset += consumed;
                        count += 1;
                    } else {
                        // Not complete, stop the loop
                        has_partial = is_partial_result;
                        break;
                    }
                }
                ParseResult::Fail => {
                    // Can't match anymore
                    break;
                }
            }
        }
        
        // Check if we met minimum requirement
        if count >= min {
            if has_partial {
                Ok(ParseResult::Partial(nodes))
            } else {
                Ok(ParseResult::Complete(nodes))
            }
        } else {
            Ok(ParseResult::Fail)
        }
    }
    

}

#[cfg(test)]
mod tests {
    use crate::set_debug_level;

    use super::*;

    #[test]
    fn test_simple_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("hello").unwrap();
        assert!(ast.root().is_complete());
        println!("AST: {}", ast);
    }

    #[test]
    fn test_partial_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        set_debug_level(crate::DebugLevel::Debug);
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("hel").unwrap();
        assert!(!ast.root().is_complete());
        // Partial parsing - we have some alternatives but none are complete
    }

    #[test]
    fn test_repetition_star() {
        let spec = r#"
        A ::= 'a'
        B ::= 'b'
        start ::= A* B
        "#;

        set_debug_level(crate::DebugLevel::Trace);

        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a a a b").unwrap();
        println!("AST: {}", ast);
        assert!(ast.root().is_complete());
        
        let ast2 = p.partial("b").unwrap();
        println!("AST2: {}", ast2);
        assert!(ast2.root().is_complete()); // Zero matches is valid
    }

    #[test]
    fn test_repetition_plus() {
        let spec = r#"
        A ::= 'a'
        start ::= A+
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a a").unwrap();
        assert!(ast.root().is_complete());
        
        let ast2 = p.partial("").unwrap();
        assert!(!ast2.root().is_complete()); // Zero matches fails
    }

    #[test]
    fn test_alternatives() {
        let spec = r#"
        A ::= 'a'
        B ::= 'b'
        start ::= A | B
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a").unwrap();
        std::println!("AST: {}", ast);
        assert!(ast.root().is_complete());
        // Only the 'A' alternative should succeed (matched 'a')
        // The 'B' alternative fails with mismatch and should NOT be in alts
        assert_eq!(ast.root().alts.len(), 1);
    }

    #[test]
    fn test_partial_alternatives() {
        let spec = r#"
        A ::= 'a'
        B ::= 'a' 'b'
        start ::= A | B
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a").unwrap();
        std::println!("AST: {}", ast);
        assert!(ast.root().is_complete());
        // A: complete (matched 'a')
        // B: partial (matched 'a', missing 'b' at end of input)
        // Both should be kept!
        assert_eq!(ast.root().alts.len(), 2);
    }

    #[test]
    fn test_partial_at_end() {
        let spec = r#"
        start ::= 'hello' 'world'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("hello wor").unwrap();
        assert!(!ast.root().is_complete());
        // Partial parsing in progress
    }

    #[test]
    fn test_mismatch_rejection() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let _ast = p.partial("goodbye").unwrap_err();
    }

    #[test]
    fn test_complex_grammar() {
        let spec = r#"
        Number ::= /[0-9]+/
        Op ::= '+' | '-'
        Expr ::= Number (Op Number)*
        start ::= Expr
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("1 + 2 - 3").unwrap();
        assert!(ast.root().is_complete());
    }

    #[test]
    fn test_binding_preservation() {
        let spec = r#"
        Number ::= /[0-9]+/
        start ::= Number[x]
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("42").unwrap();
        println!("AST: {:#?}", ast);
        assert!(ast.root().is_complete());
        
        // Check that binding is preserved
        let root = ast.root();
        assert_eq!(root.alts.len(), 1);
        let alt = &root.alts[0];
        
        // The slot should have a node with the Number nonterminal
        if let Some(slot) = alt.slots.get(&0) {
            assert_eq!(slot.nodes().len(), 1);
            if let Node::NonTerminal(nt) = &slot.nodes()[0] {
                assert_eq!(nt.binding, Some("x".to_string()));
            } else {
                panic!("Expected NonTerminal node");
            }
        } else {
            panic!("Expected slot 0");
        }
    }

    #[test]
    fn test_binding_in_repetition() {
        let spec = r#"
        Number ::= /[0-9]+/
        start ::= Number[n]+
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("1 2 3").unwrap();
        println!("AST: {:#?}", ast);
        assert!(ast.root().is_complete());
        
        // Check that binding is applied to each occurrence
        // Note: Number[n]+ is desugared into Number[n] Number[n]*
        // So we have slot 0 with 1 node and slot 1 with 2 nodes
        let root = ast.root();
        assert_eq!(root.alts.len(), 1);
        let alt = &root.alts[0];
        
        // Collect all Number nodes from both slots
        let mut all_number_nodes = Vec::new();
        
        // Slot 0: the first Number[n]
        if let Some(slot) = alt.slots.get(&0) {
            all_number_nodes.extend(slot.nodes().iter().cloned().collect::<Vec<_>>());
        } else {
            panic!("Expected slot 0");
        }
        
        // Slot 1: the Number[n]* (zero or more repetitions)
        if let Some(slot) = alt.slots.get(&1) {
            all_number_nodes.extend(slot.nodes().iter().cloned().collect::<Vec<_>>());
        } else {
            panic!("Expected slot 1");
        }
        
        // Should have 3 Number nodes total, each with the binding "n"
        assert_eq!(all_number_nodes.len(), 3);
        for node in all_number_nodes {
            if let Node::NonTerminal(nt) = node {
                assert_eq!(nt.name, "Number");
                assert_eq!(nt.binding, Some("n".to_string()));
            } else {
                panic!("Expected NonTerminal node");
            }
        }
    }
}
