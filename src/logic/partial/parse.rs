
use std::fmt;

 

use crate::{debug_debug, debug_trace};
use crate::logic::ast::{ASTNode, SegmentRange};
use crate::logic::grammar::{Grammar, Production, Symbol};
use crate::logic::tokenizer::{Tokenizer, Segment};
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use crate::logic::partial::{
    Alt, 
    Node, 
    PartialAST, 
    NonTerminal,
    Terminal
};

impl Segment {
    /// Get the segment range (just its own index)
    pub fn seg_range(&self) -> SegmentRange {
        SegmentRange::single(self.index)
    }
}

#[derive(Clone, Debug)]
pub enum ParseResult {
    /// Symbol fully or partially matched, advanced position
    Good(Node),
    /// Symbol failed to match (mismatch)
    Fail,
}

impl fmt::Display for ParseResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseResult::Good(node) => {
                write!(f, "Good(node={})", node)
            }
            ParseResult::Fail => write!(f, "Fail"),
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
            match self.parse_production(segments, &prod.rhs,  level) {
                Ok(Some((alt, end_pos))) => {
                    debug_trace!("parser2      ", "{}[L{}] Production succeeded: complete={}, end_pos={}", indent, level, alt.is_complete(), end_pos);
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
        symbols: &[Symbol],
        level: usize,
    ) -> Result<Option<Vec<Node>>, String> {
        
        if symbols.is_empty() { return Ok(None); }
        match self.parse_symbol(segments, &symbols[0],  level)? {
            ParseResult::Good(node) => {
                match node {
                    Node::Terminal(terminal) => {
                        return Ok(Some(vec![Node::Terminal(terminal)]));
                    }
                    Node::NonTerminal(nonterminal) => {
                        for alt in &nonterminal.alts {

                        }
                    }
                }
            }
            ParseResult::Fail => return Ok(None),
        }
        Ok(None)
    }

    /// Parse a symbol with an explicit binding override
    fn parse_symbol(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        level: usize,
    ) -> Result<ParseResult, String> {
        let res = match symbol {
            Symbol::Litteral(lit) => self.parse_literal(segments, lit,  level),
            Symbol::Regex(re) => self.parse_regex(segments, re, level),
            Symbol::Expression(nt) => {
                self.parse_expression(segments, nt, level)
            },
            Symbol::Single { value, binding, repetition } => {
                self.parse_single(segments, value.as_ref(), Some(binding.clone()), repetition.clone(), level)
            }
        };
        debug_trace!("parser2.sym ", "{}[L{}] Symbol parse result: {}", "  ".repeat(level), level, res.clone()?);
        res
    }

    /// Parse literal terminal
    fn parse_literal(
        &self,
        segments: &[Segment],
        lit: &str,
        sym_idx: usize,
        pos: usize,
        binding: Option<String>,
        level: usize,
    ) -> Result<ParseResult, String> {
        self.parse_regex(segments, &DerivativeRegex::literal(lit), sym_idx, pos, binding, level)
    }

    /// Parse regex terminal
    fn parse_regex(
        &self,
        segments: &[Segment],
        re: &DerivativeRegex,
        sym_idx: usize,
        pos: usize,
        binding: Option<String>,
        level: usize,
    ) -> Result<ParseResult, String> {

        let seg = if let Some(s) = segments.get(pos) {s} else {
            // At end of input, create a partial symbol at the last segment position
            let anchor_seg = segments.last().map(|s| s.index).unwrap_or(0);
            return Ok(ParseResult::Partial {
                node: None,
                partial_symbol: PartialSymbol::Terminal{
                    symbol_index: sym_idx,
                    span: SegmentRange::single(anchor_seg),
                    re: re.clone(),
                    derivative: re.clone(),
                },
                pos,
            });
        };

        let indent = "  ".repeat(level);
        debug_trace!("parser2.regex", "{}[L{}] Trying regex '{}' against segment '{}' at pos {}", indent, level, re.to_pattern(), seg.text(), pos);
        match re.prefix_match(&seg.text()) {
            PrefixStatus::Complete => {
                debug_trace!("parser2.regex", "{}[L{}] Regex FULL match for segment '{}'", indent, level, seg.text());
                Ok(ParseResult::Match {
                    nodes: vec![ParsedNode::Terminal(Terminal {
                        value: seg.text().to_string(),
                        span: Some(seg.seg_range()),
                        binding: binding.clone(),
                        extension: None,
                    })],
                    next_pos: pos + 1,
                    extensible: false,
                })
            }
            PrefixStatus::Prefix(derivative) => {
                debug_trace!("parser2.regex", "{}[L{}] Regex PARTIAL match for segment '{}'", indent, level, seg.text());
                Ok(ParseResult::Partial {
                    node: None,
                    partial_symbol: PartialSymbol::Terminal{
                        symbol_index: sym_idx,
                        span: seg.seg_range(),
                        re: re.clone(),
                        derivative: derivative.clone(),
                    },
                    pos: pos + 1,
                })
            }
            PrefixStatus::Extensible(derivative) => {
                debug_trace!("parser2.regex", "{}[L{}] Regex EXTENSIBLE match for segment '{}'", indent, level, seg.text());
                Ok(ParseResult::Match {
                    nodes: vec![ParsedNode::Terminal(Terminal {
                        value: seg.text().to_string(),
                        span: Some(seg.seg_range()),
                        binding: binding.clone(),
                        extension: Some(derivative.clone()),
                    })],
                    next_pos: pos + 1,
                    extensible: false,
                })
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
        sym_idx: usize,
        pos: usize,
        binding: Option<String>,
        level: usize,
    ) -> Result<ParseResult, String> {
        // Nested parse, so don't require full consumption
        let nt_result = self.parse_nonterminal(segments, nt, pos, binding, level + 1)?;
        
        debug_trace!("parser2.expr ", "{}[L{}] Nonterminal '{}'",
            "  ".repeat(level), level, nt);

        if nt_result.alts.is_empty() {
            // No alternatives succeeded or progressed
            Ok(ParseResult::Fail)
        } else if nt_result.is_complete() {
            debug_trace!("parser2.expr ", "{}[L{}] Nonterminal '{}' complete at pos {}", "  ".repeat(level), level, nt, pos);
            // At least one complete alternative
            let max_seg = nt_result.alts.iter()
                .filter(|alt| alt.is_complete())
                .filter_map(|alt| alt.span.as_ref().map(|s| s.end))
                .max()
                .unwrap_or(pos);
            
            // Find how many segments were consumed (next position after max_seg)
            let next_pos = max_seg + 1;
            
            Ok(ParseResult::Match {
                nodes: vec![ParsedNode::NonTerminal(nt_result)],
                next_pos,
                extensible: false,
            })
        } else {
            if nt_result.is_progressing() {
                // Find the furthest position reached
                let max_seg = nt_result.alts.iter()
                    .filter_map(|alt| alt.span.as_ref().map(|s| s.end))
                     .max()
                    .unwrap_or(segments.len().saturating_sub(1));
                
                let next_pos = max_seg + 1;
                debug_trace!("parser2.expr ", "{}[L{}] Nonterminal '{}' partially matched up to pos {}", "  ".repeat(level), level, nt, next_pos);

                // Return as partial with the nonterminal node included
                Ok(ParseResult::Partial {
                    node: Some(ParsedNode::NonTerminal(nt_result)),
                    partial_symbol: PartialSymbol::NonTerminal {
                        symbol_index: sym_idx,
                        nt: nt.to_string(),
                    },
                    pos: next_pos,
                })
            } else {
                // No progress at all
                Ok(ParseResult::Fail)
            }
        }
    }

    /// Parse single symbol with optional repetition
    fn parse_single(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        sym_idx: usize,
        pos: usize,
        binding: Option<String>,
        repetition: Option<(usize, Option<usize>)>,
        level: usize,
    ) -> Result<ParseResult, String> {
        match repetition {
            None => self.parse_symbol_with_binding(segments, symbol, sym_idx, pos, binding, level),
            Some((min, max)) => self.parse_repetition(segments, symbol, sym_idx, pos, binding, min, max, level),
        }
    }


    /// Parse repetition of a single symbol
    fn parse_repetition(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        sym_idx: usize,
        pos: usize,
        binding: Option<String>,
        min: usize,
        max: Option<usize>,
        level: usize,
    ) -> Result<ParseResult, String> {
    let mut nodes = Vec::new();
    let mut current_pos = pos;
    let mut count = 0;
    let mut child_extensible_seen = false;
        
        loop {
            // Check if we've hit max
            if let Some(max_count) = max {
                if count >= max_count {
                    break;
                }
            }
            
            // Try to parse one more occurrence
            // Apply binding to each occurrence in the repetition
            let result = self.parse_symbol_with_binding(segments, symbol, sym_idx, current_pos, binding.clone(), level)?;
            match result {
                ParseResult::Match { nodes: mut new_nodes, next_pos, extensible: child_extensible } => {
                    if next_pos == current_pos {
                        // No progress quit
                        break;
                    }
                    nodes.append(&mut new_nodes);
                    current_pos = next_pos;
                    count += 1;
                    // Remember if any child remains extensible so completions can flow through
                    child_extensible_seen |= child_extensible;
                }
                ParseResult::Partial { node, partial_symbol, pos: partial_pos } => {
                    // Partial match in repetition
                    if count >= min {
                        // We already satisfied the minimum count.
                        // Decide if we propagate the partial further or treat the
                        // repetition as complete based on whether any progress was
                        // made by the inner parser.
                        let advanced = partial_pos > current_pos && segments.get(current_pos).is_some();
                        if advanced {
                            // Inner parser consumed at least one segment –
                            // propagate partial to indicate more input is expected.
                            return Ok(ParseResult::Partial {
                                node,
                                partial_symbol,
                                pos: partial_pos,
                            });
                        } else {
                            // No progress by inner parser. Treat current repetition
                            // as finished and return what we have as a match.
                            let repetition_extensible = match max {
                                Some(max_count) => count < max_count,
                                None => true,
                            };
                            return Ok(ParseResult::Match {
                                nodes,
                                next_pos: current_pos,
                                extensible: repetition_extensible || child_extensible_seen,
                            });
                        }
                    } else {
                        // Minimum not yet met – propagate partial unchanged
                        return Ok(ParseResult::Partial {
                            node,
                            partial_symbol,
                            pos: partial_pos,
                        });
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
            let repetition_extensible = match max {
                Some(max_count) => count < max_count,
                None => true,
            };
            Ok(ParseResult::Match {
                nodes,
                next_pos: current_pos,
                extensible: repetition_extensible || child_extensible_seen,
            })
        }  else {
            // No matches and minimum required
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
        assert!(ast.root().is_progressing());
    }

    #[test]
    fn test_repetition_star() {
        let spec = r#"
        A ::= 'a'
        start ::= A*
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.partial("a a a").unwrap();
        assert!(ast.root().is_complete());
        
        let ast2 = p.partial("").unwrap();
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
        assert!(ast.root().is_progressing());
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
        
        // The slot should have a filled node with the Number nonterminal
        if let Some(slot) = alt.slots.get(&0) {
            match slot {
                crate::logic::partial::Slot::Filled { nodes, .. } => {
                    assert_eq!(nodes.len(), 1);
                    if let ParsedNode::NonTerminal(nt) = &nodes[0] {
                        assert_eq!(nt.binding, Some("x".to_string()));
                    } else {
                        panic!("Expected NonTerminal node");
                    }
                }
                _ => panic!("Expected Filled slot"),
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
            match slot {
                crate::logic::partial::Slot::Filled { nodes, .. } => {
                    all_number_nodes.extend(nodes.clone());
                }
                _ => panic!("Expected Filled slot 0"),
            }
        } else {
            panic!("Expected slot 0");
        }
        
        // Slot 1: the Number[n]* (zero or more repetitions)
        if let Some(slot) = alt.slots.get(&1) {
            match slot {
                crate::logic::partial::Slot::Filled { nodes, .. } => {
                    all_number_nodes.extend(nodes.clone());
                }
                _ => panic!("Expected Filled slot 1"),
            }
        } else {
            panic!("Expected slot 1");
        }
        
        // Should have 3 Number nodes total, each with the binding "n"
        assert_eq!(all_number_nodes.len(), 3);
        for node in all_number_nodes {
            if let ParsedNode::NonTerminal(nt) = node {
                assert_eq!(nt.name, "Number");
                assert_eq!(nt.binding, Some("n".to_string()));
            } else {
                panic!("Expected NonTerminal node");
            }
        }
    }
}
