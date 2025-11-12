
use std::fmt;

use serde::de;
use tracing_subscriber::field::debug;

use crate::{debug_debug, debug_trace};
use crate::logic::ast::{ASTNode, SegmentRange};
use crate::logic::grammar::{Grammar, Production, RepetitionKind, Symbol};
use crate::logic::tokenizer::{Tokenizer, Segment};
use crate::logic::partial::production::PartialSymbol;
use crate::regex::{PrefixStatus, Regex as DerivativeRegex};
use crate::logic::partial::{
    Alt, 
    ParsedNode, 
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

/// Parse result for a symbol
#[derive(Clone, Debug)]
pub enum ParseResult {
    /// Symbol fully matched, advanced position
    Match {
        nodes: Vec<ParsedNode>,
        next_pos: usize,
    },
    /// Symbol partially matched (prefix at end of input)
    Partial {
        /// The partially parsed node (e.g., a NonTerminal with some children)
        node: Option<ParsedNode>,
        partial_symbol: PartialSymbol,
        pos: usize,
    },
    /// Symbol failed to match (mismatch)
    Fail,
}

impl fmt::Display for ParseResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParseResult::Match { nodes, next_pos } => {
                write!(f, "Match(next_pos={}, nodes=[", next_pos)?;
                for (i, node) in nodes.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", node)?;
                }
                write!(f, "])")
            }
            ParseResult::Partial { node, partial_symbol, pos } => {
                write!(f, "Partial(pos={}, partial_symbol={}", pos, partial_symbol)?;
                if let Some(n) = node {
                    write!(f, ", node={}", n)?;
                }
                write!(f, ")")
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
        
        // Parse from start (allow partial consumption for partial parsing)
        let root = self.parse_nonterminal(&segments, start_nt, 0, None, true, 0)?;
        if root.alts.is_empty() {
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
        pos: usize,
        binding: Option<String>,
        require_full_consumption: bool,
        level: usize,
    ) -> Result<NonTerminal, String> {
        let indent = "  ".repeat(level);
        debug_trace!("parser2      ", "{}[L{}] Parsing nonterminal '{}' at pos {} (require_full={})", 
            indent, level, nt_name, pos, require_full_consumption);
        
        let productions = self.grammar.productions.get(nt_name)
            .ok_or_else(|| format!("No productions for nonterminal '{}'", nt_name))?;
        
        let mut alts = Vec::new();
        let mut max_pos = pos;
        
        for prod in productions {
            debug_trace!("parser2      ", "{}[L{}] Trying production: {:?}", indent, level, prod.rhs);
            
            match self.parse_production(segments, prod, pos, require_full_consumption, level) {
                Ok(Some((alt, end_pos))) => {
                    debug_trace!("parser2      ", "{}[L{}] Production succeeded: complete={}, end_pos={}", indent, level, alt.is_complete(), end_pos);
                    if end_pos > max_pos {
                        max_pos = end_pos;
                    }
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
        
        // Compute span from start position to the furthest position reached by any alternative
        let span = if max_pos > pos {
            if let (Some(start_seg), Some(end_seg)) = (segments.get(pos), segments.get(max_pos.saturating_sub(1))) {
                Some(SegmentRange::new(start_seg.index, end_seg.index))
            } else {
                Some(SegmentRange::new(pos, max_pos.saturating_sub(1)))
            }
        } else {
            segments.get(pos).map(|seg| seg.seg_range())
        };

        debug_trace!("parser2      ", "{}[L{}] Finished parsing nonterminal '{}': {} alternatives found with max span {:?}",
            indent, level, nt_name, alts.len(), span);

        Ok(NonTerminal {
            name: nt_name.to_string(),
            alts,
            binding,
            span,
        })
    }

    /// Parse a production as sequence of symbols
    /// Returns Some((alt, end_pos)) on success where end_pos is the position after last consumed segment
    fn parse_production(
        &self,
        segments: &[Segment],
        prod: &Production,
        start_pos: usize,
        require_full_consumption: bool,
        level: usize,
    ) -> Result<Option<(Alt, usize)>, String> {
        let mut alt = Alt::new(prod.clone());
        let mut pos = start_pos;
        let mut had_progress = false;
        let mut stopped_due_to_partial = false;
        
        for (sym_idx, symbol) in prod.rhs.iter().enumerate() {
            match self.parse_symbol(segments, symbol, sym_idx, pos, level)? {
                ParseResult::Match { nodes, next_pos } => {
                    debug_trace!("parser2.prod ", "{}[L{}] Full Symbol matched, advancing to pos {}", "  ".repeat(level), level, next_pos);
                    // Symbol matched, add to filled slot
                    for node in nodes {
                        alt.add_filled(sym_idx, node);
                        had_progress = true;
                    }
                    pos = next_pos;
                }
                ParseResult::Partial { node, partial_symbol, pos: partial_pos } => {
                    debug_trace!("partial","Partial match found: {:?} until {:?}", partial_symbol, partial_pos);
                    // Symbol partially matched at end of input
                    // The node is already parsed and included in ParseResult
                    alt.set_partial(sym_idx, node, partial_symbol);
                    pos = partial_pos;
                    had_progress = true;
                    stopped_due_to_partial = true;
                    // Can't continue past partial symbol
                    break;
                }
                ParseResult::Fail => {
                    // Mismatch: this production fails completely if we haven't made progress
                    if !had_progress {
                        return Ok(None);
                    }
                    // If we had progress, we stop here with partial
                    break;
                }
            }
        }
        
        // Compute span
        if pos > start_pos {
            if let (Some(start_seg), Some(end_seg)) = (segments.get(start_pos), segments.get(pos.saturating_sub(1))) {
                alt.span = Some(SegmentRange::new(start_seg.index, end_seg.index));
            }
        }
        
        let alt_complete = alt.is_complete();

        let indent = "  ".repeat(level);
        debug_trace!("parser2.prod ", "{}[L{}] Production result: pos={}, segments.len={}, stopped_due_to_partial={}, alt={}, had_progress={}, require_full={} '{}'",
            indent, level, pos, segments.len(), stopped_due_to_partial, alt, had_progress, require_full_consumption,segments.iter().map(|s| s.text()).collect::<Vec<_>>().join(" "));


        // At the top level, reject alternatives that leave unconsumed tokens.
        // This ensures we only accept parses that consume the entire input.
        // For nested parses, we allow partial consumption since the parent will continue.
        // also reject unconsumed if we stopped due to partial match
        if (require_full_consumption || stopped_due_to_partial) && pos < segments.len() {
            debug_trace!("parser2.prod ", "{}[L{}] Rejecting: unconsumed tokens at top or partial (pos={} < len={})", indent, level, pos, segments.len());
            return Ok(None);
        }

        // Return the alternative if we made any progress 
        if alt_complete {
            debug_trace!("parser2.prod ", "{}[L{}] Accepting complete alternative !", indent, level);
            Ok(Some((alt, pos)))
        } else if had_progress  {
            debug_trace!("parser2.prod ", "{}[L{}] Accepting partial alternative", indent, level);
            Ok(Some((alt, pos)))
        } else {
            debug_trace!("parser2.prod ", "{}[L{}] Rejecting: no progress", indent, level);
            Ok(None)
        }
    }

    /// Parse a symbol (with repetition handling)
    fn parse_symbol(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        sym_idx: usize,
        pos: usize,
        level: usize,
    ) -> Result<ParseResult, String> {
        self.parse_symbol_with_binding(segments, symbol, sym_idx, pos, None, level)
    }

    /// Parse a symbol with an explicit binding override
    fn parse_symbol_with_binding(
        &self,
        segments: &[Segment],
        symbol: &Symbol,
        sym_idx: usize,
        pos: usize,
        binding_override: Option<String>,
        level: usize,
    ) -> Result<ParseResult, String> {
        let res = match symbol {
            Symbol::Litteral(lit) => self.parse_literal(segments, lit, sym_idx, pos, binding_override, level),
            Symbol::Regex(re) => self.parse_regex(segments, re, sym_idx, pos, binding_override, level),
            Symbol::Expression(nt) => {
                let binding = binding_override.or_else(|| None);
                self.parse_expression(segments, nt, sym_idx, pos, binding, level)
            },
            Symbol::Single { value, binding, repetition } => {
                let effective_binding = binding_override.or_else(|| binding.clone());
                self.parse_single(segments, value.as_ref(), sym_idx, pos, effective_binding, repetition.clone(), level)
            }
            Symbol::Group { symbols, repetition } => {
                self.parse_group(segments, symbols, sym_idx, pos, binding_override, repetition.clone(), level)
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
        let nt_result = self.parse_nonterminal(segments, nt, pos, binding, false, level + 1)?;
        
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
        repetition: Option<RepetitionKind>,
        level: usize,
    ) -> Result<ParseResult, String> {
        match repetition {
            None => {
                self.parse_symbol_with_binding(segments, symbol, sym_idx, pos, binding, level)
            }
            Some(RepetitionKind::ZeroOrMore) => {
                self.parse_repetition(segments, symbol, sym_idx, pos, binding, 0, None, level)
            }
            Some(RepetitionKind::OneOrMore) => {
                self.parse_repetition(segments, symbol, sym_idx, pos, binding, 1, None, level)
            }
            Some(RepetitionKind::ZeroOrOne) => {
                self.parse_repetition(segments, symbol, sym_idx, pos, binding, 0, Some(1), level)
            }
        }
    }

    /// Parse group with optional repetition
    fn parse_group(
        &self,
        segments: &[Segment],
        symbols: &[Symbol],
        sym_idx: usize,
        pos: usize,
        binding: Option<String>,
        repetition: Option<RepetitionKind>,
        level: usize,
    ) -> Result<ParseResult, String> {
        let res = match repetition {
            None => {
                // Parse group once as a sequence
                self.parse_sequence(segments, symbols, pos, binding.clone(), level)
            }
            Some(RepetitionKind::ZeroOrMore) => {
                self.parse_group_repetition(segments, symbols, sym_idx, pos, binding, 0, None, level)
            }
            Some(RepetitionKind::OneOrMore) => {
                self.parse_group_repetition(segments, symbols, sym_idx, pos, binding, 1, None, level)
            }
            Some(RepetitionKind::ZeroOrOne) => {
                self.parse_group_repetition(segments, symbols, sym_idx, pos, binding, 0, Some(1), level)
            }
        };
        debug_trace!("parser2.grp ", "{}[L{}] Group parse result: {}", "  ".repeat(level), level, res.clone()?);
        res
    }

    /// Parse a sequence of symbols (for groups)
    fn parse_sequence(
        &self,
        segments: &[Segment],
        symbols: &[Symbol],
        start_pos: usize,
        binding: Option<String>,
        level: usize,
    ) -> Result<ParseResult, String> {
        let mut nodes = Vec::new();
        let mut pos = start_pos;
        
        for (idx, symbol) in symbols.iter().enumerate() {
            // Apply binding only to the first symbol in a sequence if binding is present
            let symbol_binding = if idx == 0 { binding.clone() } else { None };
            match self.parse_symbol_with_binding(segments, symbol, idx, pos, symbol_binding, level)? {
                ParseResult::Match { nodes: mut new_nodes, next_pos } => {
                    nodes.append(&mut new_nodes);
                    pos = next_pos;
                }
                ParseResult::Partial { node, partial_symbol, pos: partial_pos } => {
                    debug_trace!("parser2.seq ", "{}[L{}] Partial symbol in sequence at pos {}", "  ".repeat(level), level, partial_pos);
                    // Partial in sequence means whole sequence is partial
                    return Ok(ParseResult::Partial {
                        node,
                        partial_symbol,
                        pos: partial_pos,
                    });
                }
                ParseResult::Fail => {
                    return Ok(ParseResult::Fail);
                }
            }
        }
        debug_trace!("parser2.seq ", "{}[L{}] Sequence matched up to pos {}", "  ".repeat(level), level, pos);
        Ok(ParseResult::Match { nodes, next_pos: pos })
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
                ParseResult::Match { nodes: mut new_nodes, next_pos } => {
                    if next_pos == current_pos {
                        // No progress quit
                        break;
                    }
                    nodes.append(&mut new_nodes);
                    current_pos = next_pos;
                    count += 1;
                }
                ParseResult::Partial { node, partial_symbol, pos: partial_pos } => {
                    // Partial match in repetition
                    if count >= min {
                        // We have enough matches so partial is optional continuation
                        // Return what we have as match
                        return Ok(ParseResult::Match {
                            nodes,
                            next_pos: current_pos,
                        });
                    } else {
                        // We need more matches - propagate partial
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
            Ok(ParseResult::Match {
                nodes,
                next_pos: current_pos,
            })
        }  else {
            // No matches and minimum required
            Ok(ParseResult::Fail)
        }
    }

    /// Parse group repetition
    fn parse_group_repetition(
        &self,
        segments: &[Segment],
        symbols: &[Symbol],
        _sym_idx: usize,
        pos: usize,
        binding: Option<String>,
        min: usize,
        max: Option<usize>,
        level: usize,
    ) -> Result<ParseResult, String> {
        let mut nodes = Vec::new();
        let mut current_pos = pos;
        let mut count = 0;
        
        loop {
            if let Some(max_count) = max {
                if count >= max_count {
                    break;
                }
            }
            
            // Apply binding to each occurrence in the repetition
            match self.parse_sequence(segments, symbols, current_pos, binding.clone(), level)? {
                ParseResult::Match { nodes: mut new_nodes, next_pos } => {
                    if next_pos == current_pos {
                        break;
                    }
                    nodes.append(&mut new_nodes);
                    current_pos = next_pos;
                    count += 1;
                }
                ParseResult::Partial { node, partial_symbol, pos: partial_pos } => {
                    // Partial match in repetition
                    debug_trace!("parser2.rep ", "{}[L{}] Partial symbol in repetition at pos {}", "  ".repeat(level), level, partial_pos);
                    if count >= min {
                        return Ok(ParseResult::Match {
                            nodes,
                            next_pos: partial_pos, // FIXED: it was returning current_pos here, which is incorrect
                        });
                    } else {
                        return Ok(ParseResult::Partial {
                            node,
                            partial_symbol,
                            pos: partial_pos,
                        });
                    }
                }
                ParseResult::Fail => {
                    break;
                }
            }
        }
        
        if count >= min {
            Ok(ParseResult::Match {
                nodes,
                next_pos: current_pos,
            })
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
        let root = ast.root();
        assert_eq!(root.alts.len(), 1);
        let alt = &root.alts[0];
        
        if let Some(slot) = alt.slots.get(&0) {
            match slot {
                crate::logic::partial::Slot::Filled { nodes, .. } => {
                    // Should have 3 Number nodes, each with the binding "n"
                    assert_eq!(nodes.len(), 3);
                    for node in nodes {
                        if let ParsedNode::NonTerminal(nt) = node {
                            assert_eq!(nt.name, "Number");
                            assert_eq!(nt.binding, Some("n".to_string()));
                        } else {
                            panic!("Expected NonTerminal node");
                        }
                    }
                }
                _ => panic!("Expected Filled slot"),
            }
        } else {
            panic!("Expected slot 0");
        }
    }
}
