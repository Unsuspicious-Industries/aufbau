use crate::debug_trace;
use crate::logic::ast::SourceSpan;
use crate::logic::grammar::{Grammar, Production, RepetitionKind, Symbol};
use crate::logic::tokenizer::Tokenizer;
use crate::logic::partial::production::PartialSymbol;
use crate::logic::partial::{NewAlt, NewNonTerminal, NewParsedNode, NewPartialAST, NewSlot, PartialTerminal};
use std::collections::HashMap;

/// Segment represents a tokenized piece of input
#[derive(Clone, Debug)]
pub struct Segment {
    pub text: String,
    pub start: usize,
    pub end: usize,
}

impl Segment {
    pub fn span(&self) -> SourceSpan {
        SourceSpan {
            start: self.start,
            end: self.end,
        }
    }
}

/// Parse result for a symbol
#[derive(Clone, Debug)]
pub enum ParseResult {
    /// Symbol fully matched, advanced position
    Match {
        nodes: Vec<NewParsedNode>,
        next_pos: usize,
    },
    /// Symbol partially matched (prefix at end of input)
    Partial {
        partial_symbol: PartialSymbol,
        pos: usize,
    },
    /// Symbol failed to match (mismatch)
    Fail,
}

pub struct Parser {
    grammar: Grammar,
    tokenizer: Tokenizer,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        let specials = grammar.special_tokens.clone();
        Self {
            grammar,
            tokenizer: Tokenizer::new(specials, vec![' ', '\n', '\t']),
        }
    }

    /// Main entry point: parse input and return new PartialAST
    pub fn parse(&mut self, input: &str) -> Result<NewPartialAST, String> {
        debug_trace!("parser2", "Starting parse of input: '{}'", input);
        
        // Tokenize
        let segments = self.tokenize(input)?;
        debug_trace!("parser2", "Tokenized into {} segments", segments.len());
        
        // Get start nonterminal
        let start_nt = self.grammar.start_nonterminal()
            .ok_or_else(|| "No start nonterminal in grammar".to_string())?;
        
        // Parse from start
        let root = self.parse_nonterminal(&segments, start_nt, 0, None)?;
        
        Ok(NewPartialAST::new(root, input.to_string()))
    }

    /// Tokenize input into segments
    fn tokenize(&mut self, input: &str) -> Result<Vec<Segment>, String> {
        let token_spans = self.tokenizer
            .tokenize_with_spans(input)
            .map_err(|e| format!("Tokenization failed: {:?}", e))?;
        
        let chars: Vec<char> = input.chars().collect();
        let segments = token_spans
            .into_iter()
            .map(|(_, char_start, char_end)| {
                let byte_start = chars.iter().take(char_start).map(|c| c.len_utf8()).sum::<usize>();
                let byte_end = chars.iter().take(char_end).map(|c| c.len_utf8()).sum::<usize>();
                let text: String = chars[char_start..char_end].iter().collect();
                Segment {
                    text,
                    start: byte_start,
                    end: byte_end,
                }
            })
            .collect();
        
        Ok(segments)
    }

    /// Parse a nonterminal: try all productions, keep valid and partial alternatives
    fn parse_nonterminal(
        &self,
        segments: &[Segment],
        nt_name: &str,
        pos: usize,
        binding: Option<String>,
    ) -> Result<NewNonTerminal, String> {
        debug_trace!("parser2", "Parsing nonterminal '{}' at pos {}", nt_name, pos);
        
        let productions = self.grammar.productions.get(nt_name)
            .ok_or_else(|| format!("No productions for nonterminal '{}'", nt_name))?;
        
        let mut alts = Vec::new();
        
        for prod in productions {
            debug_trace!("parser2", "Trying production: {:?}", prod.rhs);
            
            match self.parse_production(segments, prod, pos)? {
                Some(alt) => {
                    debug_trace!("parser2", "Production succeeded: complete={}", alt.is_complete());
                    alts.push(alt);
                }
                None => {
                    debug_trace!("parser2", "Production failed (mismatch)");
                }
            }
        }
        
        // Compute span from all alternatives
        let span = alts.iter()
            .filter_map(|alt| alt.span.clone())
            .max_by_key(|s| s.end)
            .or_else(|| segments.get(pos).map(|seg| seg.span()));
        
        Ok(NewNonTerminal {
            name: nt_name.to_string(),
            alts,
            binding,
            span,
        })
    }

    /// Parse a production: sequence of symbols
    /// Returns Some(alt) if valid/partial, None if mismatch
    fn parse_production(
        &self,
        segments: &[Segment],
        prod: &Production,
        start_pos: usize,
    ) -> Result<Option<NewAlt>, String> {
        let mut alt = NewAlt::new(prod.clone());
        let mut pos = start_pos;
        let mut had_progress = false;
        
        for (sym_idx, symbol) in prod.rhs.iter().enumerate() {
            match self.parse_symbol(segments, symbol, sym_idx, pos)? {
                ParseResult::Match { nodes, next_pos } => {
                    // Symbol matched, add to filled slot
                    for node in nodes {
                        alt.add_filled(sym_idx, node);
                        had_progress = true;
                    }
                    pos = next_pos;
                }
                ParseResult::Partial { partial_symbol, pos: partial_pos } => {
                    // Symbol partially matched at end of input
                    // For nonterminals, we might have parsed children - we need to include them!
                    let partial_node = if let PartialSymbol::Expression { nt, .. } = &partial_symbol {
                        // Re-parse to get the nonterminal with partial children
                        let nt_result = self.parse_nonterminal(segments, nt, pos, None)?;
                        if !nt_result.alts.is_empty() {
                            Some(NewParsedNode::NonTerminal(nt_result))
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                    
                    alt.set_partial(sym_idx, partial_node, partial_symbol);
                    pos = partial_pos;
                    had_progress = true;
                    // Can't continue past partial symbol
                    break;
                }
                ParseResult::Fail => {
                    // Mismatch - this production fails completely if we haven't made progress
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
                alt.span = Some(SourceSpan {
                    start: start_seg.start,
                    end: end_seg.end,
                });
            }
        }
        
        // Return the alternative if we made any progress
        if had_progress || alt.is_complete() {
            Ok(Some(alt))
        } else {
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
    ) -> Result<ParseResult, String> {
        match symbol {
            Symbol::Litteral(lit) => self.parse_literal(segments, lit, sym_idx, pos),
            Symbol::Regex(re) => self.parse_regex(segments, re, sym_idx, pos),
            Symbol::Expression(nt) => self.parse_expression(segments, nt, sym_idx, pos, None),
            Symbol::Single { value, binding, repetition } => {
                self.parse_single(segments, value.as_ref(), sym_idx, pos, binding.clone(), repetition.clone())
            }
            Symbol::Group { symbols, repetition } => {
                self.parse_group(segments, symbols, sym_idx, pos, repetition.clone())
            }
        }
    }

    /// Parse literal terminal
    fn parse_literal(
        &self,
        segments: &[Segment],
        lit: &str,
        sym_idx: usize,
        pos: usize,
    ) -> Result<ParseResult, String> {
        let Some(seg) = segments.get(pos) else {
            // End of input - could be waiting for this literal
            return Ok(ParseResult::Partial {
                partial_symbol: PartialSymbol::Litteral {
                    symbol_index: sym_idx,
                    byte_cursor: 0,
                    span: SourceSpan { start: segments.last().map(|s| s.end).unwrap_or(0), end: segments.last().map(|s| s.end).unwrap_or(0) },
                    expected: lit.to_string(),
                },
                pos,
            });
        };
        
        if &seg.text == lit {
            // Exact match
            Ok(ParseResult::Match {
                nodes: vec![NewParsedNode::Terminal(PartialTerminal {
                    value: seg.text.clone(),
                    span: Some(seg.span()),
                    binding: None,
                })],
                next_pos: pos + 1,
            })
        } else if pos + 1 == segments.len() && lit.starts_with(&seg.text) && !seg.text.is_empty() {
            // Prefix match at end of input - partial
            Ok(ParseResult::Partial {
                partial_symbol: PartialSymbol::Litteral {
                    symbol_index: sym_idx,
                    byte_cursor: seg.text.len(),
                    span: seg.span(),
                    expected: lit.to_string(),
                },
                pos,
            })
        } else {
            // Mismatch
            Ok(ParseResult::Fail)
        }
    }

    /// Parse regex terminal
    fn parse_regex(
        &self,
        segments: &[Segment],
        re: &regex::Regex,
        sym_idx: usize,
        pos: usize,
    ) -> Result<ParseResult, String> {
        let Some(seg) = segments.get(pos) else {
            // End of input
            return Ok(ParseResult::Partial {
                partial_symbol: PartialSymbol::Regex {
                    symbol_index: sym_idx,
                    byte_cursor: 0,
                    span: SourceSpan { start: segments.last().map(|s| s.end).unwrap_or(0), end: segments.last().map(|s| s.end).unwrap_or(0) },
                    re: re.clone(),
                },
                pos,
            });
        };
        
        let full_match = re.find(&seg.text)
            .map(|m| m.start() == 0 && m.end() == seg.text.len())
            .unwrap_or(false);
        
        if full_match {
            Ok(ParseResult::Match {
                nodes: vec![NewParsedNode::Terminal(PartialTerminal {
                    value: seg.text.clone(),
                    span: Some(seg.span()),
                    binding: None,
                })],
                next_pos: pos + 1,
            })
        } else if pos + 1 == segments.len() {
            // Try prefix match
            let synthetic = format!("{}x", seg.text);
            let could_extend = re.find(&synthetic).map(|m| m.start() == 0).unwrap_or(false);
            if could_extend && !seg.text.is_empty() {
                Ok(ParseResult::Partial {
                    partial_symbol: PartialSymbol::Regex {
                        symbol_index: sym_idx,
                        byte_cursor: seg.text.len(),
                        span: seg.span(),
                        re: re.clone(),
                    },
                    pos,
                })
            } else {
                Ok(ParseResult::Fail)
            }
        } else {
            Ok(ParseResult::Fail)
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
    ) -> Result<ParseResult, String> {
        let nt_result = self.parse_nonterminal(segments, nt, pos, binding)?;
        
        if nt_result.alts.is_empty() {
            // No alternatives succeeded or progressed
            Ok(ParseResult::Fail)
        } else if nt_result.is_complete() {
            // At least one complete alternative
            let max_pos = nt_result.alts.iter()
                .filter(|alt| alt.is_complete())
                .filter_map(|alt| alt.span.as_ref().map(|s| s.end))
                .max()
                .unwrap_or(pos);
            
            // Find how many segments were consumed
            let next_pos = segments.iter()
                .position(|seg| seg.start >= max_pos)
                .unwrap_or(segments.len());
            
            Ok(ParseResult::Match {
                nodes: vec![NewParsedNode::NonTerminal(nt_result)],
                next_pos,
            })
        } else {
            // Only partial alternatives - but we still have the nonterminal with partial children
            // We should return the nonterminal as a Match but mark that parsing should stop
            // Actually, if the nonterminal is progressing (has some parsed content), we should
            // include it in the slots and mark the symbol as partial
            if nt_result.is_progressing() {
                // Has some content - include as partial match
                // Find the furthest position reached
                let max_pos = nt_result.alts.iter()
                    .filter_map(|alt| alt.span.as_ref().map(|s| s.end))
                    .max()
                    .unwrap_or(pos);
                
                let next_pos = segments.iter()
                    .position(|seg| seg.start >= max_pos)
                    .unwrap_or(segments.len());
                
                // Return as partial with the nonterminal node included
                Ok(ParseResult::Partial {
                    partial_symbol: PartialSymbol::Expression {
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
    ) -> Result<ParseResult, String> {
        match repetition {
            None => {
                // No repetition - parse once with binding
                match symbol {
                    Symbol::Expression(nt) => self.parse_expression(segments, nt, sym_idx, pos, binding),
                    _ => self.parse_symbol(segments, symbol, sym_idx, pos),
                }
            }
            Some(RepetitionKind::ZeroOrMore) => {
                self.parse_repetition(segments, symbol, sym_idx, pos, binding, 0, None)
            }
            Some(RepetitionKind::OneOrMore) => {
                self.parse_repetition(segments, symbol, sym_idx, pos, binding, 1, None)
            }
            Some(RepetitionKind::ZeroOrOne) => {
                self.parse_repetition(segments, symbol, sym_idx, pos, binding, 0, Some(1))
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
        repetition: Option<RepetitionKind>,
    ) -> Result<ParseResult, String> {
        match repetition {
            None => {
                // Parse group once as a sequence
                self.parse_sequence(segments, symbols, pos)
            }
            Some(RepetitionKind::ZeroOrMore) => {
                self.parse_group_repetition(segments, symbols, sym_idx, pos, 0, None)
            }
            Some(RepetitionKind::OneOrMore) => {
                self.parse_group_repetition(segments, symbols, sym_idx, pos, 1, None)
            }
            Some(RepetitionKind::ZeroOrOne) => {
                self.parse_group_repetition(segments, symbols, sym_idx, pos, 0, Some(1))
            }
        }
    }

    /// Parse a sequence of symbols (for groups)
    fn parse_sequence(
        &self,
        segments: &[Segment],
        symbols: &[Symbol],
        start_pos: usize,
    ) -> Result<ParseResult, String> {
        let mut nodes = Vec::new();
        let mut pos = start_pos;
        
        for (idx, symbol) in symbols.iter().enumerate() {
            match self.parse_symbol(segments, symbol, idx, pos)? {
                ParseResult::Match { nodes: mut new_nodes, next_pos } => {
                    nodes.append(&mut new_nodes);
                    pos = next_pos;
                }
                ParseResult::Partial { partial_symbol, pos: partial_pos } => {
                    // Partial in sequence means whole sequence is partial
                    return Ok(ParseResult::Partial {
                        partial_symbol,
                        pos: partial_pos,
                    });
                }
                ParseResult::Fail => {
                    return Ok(ParseResult::Fail);
                }
            }
        }
        
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
            let result = match symbol {
                Symbol::Expression(nt) => self.parse_expression(segments, nt, sym_idx, current_pos, binding.clone())?,
                _ => self.parse_symbol(segments, symbol, sym_idx, current_pos)?,
            };
            
            match result {
                ParseResult::Match { nodes: mut new_nodes, next_pos } => {
                    if next_pos == current_pos {
                        // No progress - avoid infinite loop
                        break;
                    }
                    nodes.append(&mut new_nodes);
                    current_pos = next_pos;
                    count += 1;
                }
                ParseResult::Partial { partial_symbol, pos: partial_pos } => {
                    // Partial match in repetition
                    if count >= min {
                        // We have enough matches - partial is optional continuation
                        // Return what we have as match
                        return Ok(ParseResult::Match {
                            nodes,
                            next_pos: current_pos,
                        });
                    } else {
                        // We need more matches - propagate partial
                        return Ok(ParseResult::Partial {
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
        } else if count > 0 {
            // Some matches but not enough - this is partial
            Ok(ParseResult::Partial {
                partial_symbol: PartialSymbol::Other { symbol_index: sym_idx },
                pos: current_pos,
            })
        } else {
            // No matches and minimum required
            Ok(ParseResult::Fail)
        }
    }

    /// Parse group repetition
    fn parse_group_repetition(
        &self,
        segments: &[Segment],
        symbols: &[Symbol],
        sym_idx: usize,
        pos: usize,
        min: usize,
        max: Option<usize>,
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
            
            match self.parse_sequence(segments, symbols, current_pos)? {
                ParseResult::Match { nodes: mut new_nodes, next_pos } => {
                    if next_pos == current_pos {
                        break;
                    }
                    nodes.append(&mut new_nodes);
                    current_pos = next_pos;
                    count += 1;
                }
                ParseResult::Partial { partial_symbol, pos: partial_pos } => {
                    if count >= min {
                        return Ok(ParseResult::Match {
                            nodes,
                            next_pos: current_pos,
                        });
                    } else {
                        return Ok(ParseResult::Partial {
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
        } else if count > 0 {
            Ok(ParseResult::Partial {
                partial_symbol: PartialSymbol::Group {
                    symbol_index: sym_idx,
                    inner_index: None,
                    repetition: Some(RepetitionKind::OneOrMore),
                    binding: None,
                },
                pos: current_pos,
            })
        } else {
            Ok(ParseResult::Fail)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.parse("hello").unwrap();
        assert!(ast.root().is_complete());
    }

    #[test]
    fn test_partial_literal() {
        let spec = r#"
        start ::= 'hello'
        "#;
        let g = Grammar::load(spec).unwrap();
        let mut p = Parser::new(g);
        
        let ast = p.parse("hel").unwrap();
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
        
        let ast = p.parse("a a a").unwrap();
        assert!(ast.root().is_complete());
        
        let ast2 = p.parse("").unwrap();
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
        
        let ast = p.parse("a a").unwrap();
        assert!(ast.root().is_complete());
        
        let ast2 = p.parse("").unwrap();
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
        
        let ast = p.parse("a").unwrap();
        println!("AST: {:#?}", ast);
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
        
        let ast = p.parse("a").unwrap();
        println!("AST: {:#?}", ast);
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
        
        let ast = p.parse("hello wor").unwrap();
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
        
        let ast = p.parse("goodbye").unwrap();
        assert_eq!(ast.root().alts.len(), 0);
        assert!(!ast.root().is_complete());
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
        
        let ast = p.parse("1 + 2 - 3").unwrap();
        assert!(ast.root().is_complete());
    }
}
