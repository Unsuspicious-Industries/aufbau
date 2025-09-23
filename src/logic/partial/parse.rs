use crate::debug_trace;
use crate::logic::grammar::Symbol;
use super::*;
use crate::logic::grammar::Grammar;
use crate::logic::tokenizer::Tokenizer;
use crate::logic::ast::{ASTNode as CompleteAST, SourceSpan};
use crate::logic::grammar::RepetitionKind;

#[derive(Clone, Debug)]
pub struct Segment {
    pub text: String,
    pub start: usize,
    pub end: usize,
}

impl Segment {
    pub fn new(text: String, start: usize, end: usize) -> Self {
        Self { text, start, end }
    }
    pub fn span(&self) -> SourceSpan {
        SourceSpan { start: self.start, end: self.end }
    }
}

#[derive(Clone, Debug)]
pub enum ParseStep {
    Continue { nodes: Vec<PartialAST>, next_pos: usize, symbols_parsed: usize },
    Stop { nodes: Vec<PartialAST>, next_pos: usize, symbols_parsed: usize }, // stop current parallel branch
}

pub struct PartialParser {
    grammar: Grammar,
    tokenizer: Tokenizer,
}

impl PartialParser {
    
    pub fn new(grammar: Grammar) -> Self {
        let specials = grammar.special_tokens.clone();
        Self {
            grammar,
            tokenizer: Tokenizer::new(specials, vec![' ', '\n', '\t']),
        }
    }
    
    pub fn parse(&mut self, input: &str) -> Result<CompleteAST, String> {
        debug_trace!("parser", "Starting complete parse of input: '{}'", input);
        let partial_ast = self.partial(input)?;
        debug_trace!("parser", "Partial AST generated: {:#?}", partial_ast);
        debug_trace!("parser", "Converting partial AST to complete AST");
        let complete_ast = partial_ast.into_complete()?;
        debug_trace!("parser", "Successfully generated complete AST");
        Ok(complete_ast)
    }

    pub fn partial(&mut self, input: &str) -> Result<PartialAST, String> {
        debug_trace!("parser", "Starting partial parse of input: '{}'", input);
        // Build segments first - this is the key change!
        let segments = self.build_segments(input)?;
        debug_trace!("parser", "Built {} segments from input", segments.len());
        
        // Determine start nonterminal from grammar
        let start_nt = self
            .grammar
            .start_nonterminal()
            .cloned()
            .ok_or_else(|| "No start nonterminal defined in grammar".to_string())?;
        
        debug_trace!("parser", "Start nonterminal: {}", start_nt);
        let step = self.partial_nt(&segments, &start_nt, 0, None)?;
        match step {
            ParseStep::Continue { nodes, next_pos, symbols_parsed: _ } => {
                debug_trace!("parser", "Start nonterminal parsed successfully, final position: {}", next_pos);
                // Extract the first (and should be only) node which should be a NonTerminal
                if let Some(PartialAST::NonTerminal(children)) = nodes.into_iter().next() {
                    debug_trace!("parser", "Parsed {} parallel branches for start nonterminal", children.len());
                    Ok(PartialAST::NonTerminal(children))
                } else {
                    Err("Expected NonTerminal AST from start nonterminal".to_string())
                }
            }
            ParseStep::Stop { nodes: _, next_pos, symbols_parsed: _ } => {
                debug_trace!("parser", "Start nonterminal failed to parse, stopped at position: {}", next_pos);
                Err(format!("Failed to parse start nonterminal '{}'", start_nt))
            }
        }
    }

    /// Build segments from input using the tokenizer
    fn build_segments(&mut self, input: &str) -> Result<Vec<Segment>, String> {
        debug_trace!("parser", "Tokenizing input of length {}", input.len());
        let token_spans = self
            .tokenizer
            .tokenize_with_spans(input)
            .map_err(|e| format!("tokenization failed: {:?}", e))?;
        
        // Convert character indices to byte indices for safe string slicing
        let chars: Vec<char> = input.chars().collect();
        let segments: Vec<Segment> = token_spans
            .into_iter()
            .map(|(_, char_start, char_end)| {
                // Convert character indices to byte indices
                let byte_start = chars.iter().take(char_start).map(|c| c.len_utf8()).sum::<usize>();
                let byte_end = chars.iter().take(char_end).map(|c| c.len_utf8()).sum::<usize>();
                
                // Extract text using character-based slicing to be safe
                let text: String = chars[char_start..char_end].iter().collect();
                
                Segment::new(text, byte_start, byte_end)
            })
            .collect();
        
        debug_trace!("parser", "Created {} segments: {:?}", segments.len(), segments.iter().map(|s| &s.text).collect::<Vec<_>>());
        Ok(segments)
    }

    pub fn partial_nt(&mut self, segments: &[Segment], nt: &str, seg_pos: usize, binding: Option<String>) -> Result<ParseStep, String> {
        debug_trace!("parser", "Parsing nonterminal '{}' at segment position {} with binding {:?}", nt, seg_pos, binding);
        let productions = self
            .grammar
            .productions
            .get(nt)
            .cloned()
            .ok_or_else(|| format!("No productions found for nonterminal '{}'", nt))?;

        debug_trace!("parser", "Found {} productions for nonterminal '{}'", productions.len(), nt);

        let mut parallels: Vec<PartialNonTerminal> = Vec::new();
        let mut max_pos = seg_pos; // Track the maximum position consumed

        for (prod_idx, prod) in productions.iter().enumerate() {
            debug_trace!("parser", "Parsing production {}/{} for '{}': {:?} at segment position {}", prod_idx + 1, productions.len(), nt, prod.rhs, seg_pos);
            // Parse the entire RHS as one sequence; Stop will end this branch early, Continue means full sequence processed
            let step = self.parse_symbols_once(segments, &prod.rhs, seg_pos, None)?;
            let (nt_children, current_seg_pos, production_succeeded, symbols_parsed) = match step {
                ParseStep::Continue { nodes, next_pos, symbols_parsed } => {
                    debug_trace!("parser", "Production {}/{} completed successfully, consumed {} segments", prod_idx + 1, productions.len(), next_pos - seg_pos);
                    (nodes, next_pos, true, symbols_parsed)
                }
                ParseStep::Stop { nodes, next_pos, symbols_parsed } => {
                    debug_trace!("parser", "Production {}/{} stopped/failed, consumed {} segments", prod_idx + 1, productions.len(), next_pos - seg_pos);
                    (nodes, next_pos, false, symbols_parsed)
                }
            };
            
            // Track the maximum position reached by any production
            max_pos = max_pos.max(current_seg_pos);
            
            // Create a partial nonterminal for both successful and partially successful productions
            if production_succeeded || symbols_parsed > 0 {
                // Build a partial nonterminal for this production
                let mut partial_prod = PartialProduction::new(prod.clone());
                // Set cursor to the actual number of symbols parsed
                partial_prod.set_cursor(symbols_parsed);
                
                debug_trace!("parser", "Created partial nonterminal with {} children, cursor at {}/{} symbols", nt_children.len(), symbols_parsed, prod.rhs.len());
                
                parallels.push(PartialNonTerminal {
                    production: partial_prod,
                    children: nt_children,
                    value: nt.to_string(),
                    span: if current_seg_pos > seg_pos {
                        segments
                            .get(seg_pos)
                            .and_then(|start_seg| segments.get(current_seg_pos - 1)
                            .map(|end_seg| SourceSpan { start: start_seg.start, end: end_seg.end }))
                    } else { None },
                    binding: binding.clone(),
                    bound_typing_rule: None,
                });
            } else {
                debug_trace!("parser", "Skipping failed production {}/{} for '{}' (no symbols parsed)", prod_idx + 1, productions.len(), nt);
            }
        }

        debug_trace!("parser", "Nonterminal '{}' produced {} successful parallel branches out of {} total productions, max position: {}", nt, parallels.len(), productions.len(), max_pos);
        
        if parallels.is_empty() {
            // No successful productions, return Stop
            Ok(ParseStep::Stop { nodes: Vec::new(), next_pos: max_pos, symbols_parsed: 0 })
        } else {
            // At least one successful production, return Continue with NonTerminal containing all parallels
            Ok(ParseStep::Continue { 
                nodes: vec![PartialAST::NonTerminal(parallels)], 
                next_pos: max_pos,
                symbols_parsed: 1  // We successfully parsed one nonterminal
            })
        }
    }

    /// Parse a single grammar symbol from segments and return control flow
    fn parse_symbol(&mut self, segments: &[Segment], symbol: &Symbol, seg_pos: usize, parent_binding: Option<String>) -> Result<ParseStep, String> {
        debug_trace!("parser", "Parsing symbol {:?} at position {} with binding {:?}", symbol, seg_pos, parent_binding);
        // If no next segment stop parsing
        if segments.get(seg_pos).is_none() {
            debug_trace!("parser", "No segment at position {}, stopping", seg_pos);
             return Ok(ParseStep::Stop { nodes: Vec::new(), next_pos: seg_pos, symbols_parsed: 0 });
        }
        let segment = segments[seg_pos].clone();
        debug_trace!("parser", "Current segment: '{}'", segment.text);
        match symbol {
            Symbol::Litteral(value) => {
                // Must match the NEXT segment exactly; do not scan ahead.
                // - If next segment exists but doesn't match, just stop without adding any node.
                debug_trace!("parser", "Trying to match literal '{}' with segment '{}' at position {}", value, segment.text, seg_pos);
                if &segment.text == value {
                    debug_trace!("parser", "Literal match successful");
                    let ast_node = PartialAST::Terminal(PartialTerminal {
                        value: segment.text.clone(),
                        span: Some(segment.span()),
                        binding: parent_binding,
                    });
                    Ok(ParseStep::Continue { nodes: vec![ast_node], next_pos: seg_pos + 1, symbols_parsed: 1 })
                } else {
                    debug_trace!("parser", "Literal match failed: '{}' != '{}'", segment.text, value);
                    Ok(ParseStep::Stop { nodes: Vec::new(), next_pos: seg_pos, symbols_parsed: 0 })
                }
            } 
            Symbol::Expression(value) => {
                debug_trace!("parser", "Parsing nonterminal expression '{}'", value);
                // Parse the nonterminal and return its ParseStep directly
                let step = self.partial_nt(segments, value, seg_pos, parent_binding)?;
                match step {
                    ParseStep::Continue { nodes, next_pos, symbols_parsed } => {
                        debug_trace!("parser", "Nonterminal expression '{}' parsed successfully, consumed {} segments", value, next_pos - seg_pos);
                        Ok(ParseStep::Continue { nodes, next_pos, symbols_parsed })
                    }
                    ParseStep::Stop { nodes, next_pos, symbols_parsed } => {
                        debug_trace!("parser", "Nonterminal expression '{}' failed to parse, consumed {} segments", value, next_pos - seg_pos);
                        Ok(ParseStep::Stop { nodes, next_pos, symbols_parsed })
                    }
                }
            }   
            Symbol::Regex(re) => {
                debug_trace!("parser", "Trying to match regex pattern against segment '{}'", segment.text);
                let full_match = re
                    .find(&segment.text)
                    .map(|m| m.start() == 0 && m.end() == segment.text.len())
                    .unwrap_or(false);
                if full_match {
                    debug_trace!("parser", "Regex match successful");
                    let ast_node = PartialAST::Terminal(PartialTerminal {
                        value: segment.text.clone(),
                        span: Some(segment.span()),
                        binding: parent_binding,
                    });
                    Ok(ParseStep::Continue { nodes: vec![ast_node], next_pos: seg_pos + 1, symbols_parsed: 1 })
                } else {
                    debug_trace!("parser", "Regex match failed");
                    Ok(ParseStep::Stop { nodes: Vec::new(), next_pos: seg_pos, symbols_parsed: 0 })
                }
            }
            Symbol::Single { value, binding, repetition } => {
                debug_trace!("parser", "Parsing single symbol with binding {:?} and repetition {:?}", binding, repetition);
                // Use the binding from this symbol, or fall back to parent binding
                let effective_binding = binding.clone().or(parent_binding);
                
                match repetition {
                    None => {
                        debug_trace!("parser", "No repetition, parsing once");
                        // No repetition, parse exactly once and propagate control flow
                        self.parse_symbol(segments, value.as_ref(), seg_pos, effective_binding)
                    }
                    Some(rep_kind) => {
                        debug_trace!("parser", "Handling repetition: {:?}", rep_kind);
                        // Handle repetition for a single symbol
                        let single_symbol_vec = vec![value.as_ref().clone()];
                        self.parse_symbols_with_repetition(segments, &single_symbol_vec, seg_pos, effective_binding, rep_kind)
                    }
                }
            }
            Symbol::Group { symbols, repetition } => {
                debug_trace!("parser", "Parsing group of {} symbols with repetition {:?}", symbols.len(), repetition);
                match repetition {
                    None => {
                        debug_trace!("parser", "No repetition, parsing group once");
                        // No repetition, parse the group exactly once using the unified method
                        let step = self.parse_symbols_once(segments, symbols, seg_pos, parent_binding)?;
                        Ok(step)
                    }
                    Some(rep_kind) => {
                        debug_trace!("parser", "Handling group repetition: {:?}", rep_kind);
                        // Handle repetition for a group of symbols
                        self.parse_symbols_with_repetition(segments, symbols, seg_pos, parent_binding, rep_kind)
                    }
                }
            }
        }
    }
    
    /// Parse symbols with repetition (generalized for both single symbols and groups)
    fn parse_symbols_with_repetition(&mut self, segments: &[Segment], symbols: &[Symbol], seg_pos: usize, binding: Option<String>, rep_kind: &RepetitionKind) -> Result<ParseStep, String> {
        debug_trace!("parser", "Parsing symbols with repetition {:?} starting at position {}", rep_kind, seg_pos);
        let mut acc: Vec<PartialAST> = Vec::new();
        let mut current_pos = seg_pos;
        
        match rep_kind {
            RepetitionKind::ZeroOrMore => {
                debug_trace!("parser", "ZeroOrMore: parsing as many times as possible");
                // Parse as many times as possible; Stop just means we're done, not failure
                loop {
                    let step = self.parse_symbols_once(segments, symbols, current_pos, binding.clone())?;
                    match step {
                        ParseStep::Continue { mut nodes, next_pos, symbols_parsed: _ } => {
                            if next_pos == current_pos && nodes.is_empty() { // safety: avoid infinite loop
                                debug_trace!("parser", "ZeroOrMore: no progress made, breaking loop");
                                break;
                            }
                            debug_trace!("parser", "ZeroOrMore: iteration successful, got {} nodes", nodes.len());
                            acc.append(&mut nodes);
                            current_pos = next_pos;
                        }
                        ParseStep::Stop { nodes, next_pos: _, symbols_parsed: _ } => {
                            debug_trace!("parser", "ZeroOrMore: iteration stopped naturally (not failure), got {} nodes", nodes.len());
                            // For ZeroOrMore, Stop just means we're done - don't include partial nodes from failed attempt
                            // and don't update position since this attempt didn't succeed
                            break;
                        }
                    }
                }
                debug_trace!("parser", "ZeroOrMore: completed successfully with {} total nodes", acc.len());
                let symbols_parsed = if acc.is_empty() { 0 } else { 1 };
                Ok(ParseStep::Continue { nodes: acc, next_pos: current_pos, symbols_parsed })
            }
            RepetitionKind::OneOrMore => {
                // Must parse at least once; if first attempt Stops, propagate Stop
                let step = self.parse_symbols_once(segments, symbols, current_pos, binding.clone())?;
                match step {
                    ParseStep::Continue { mut nodes, next_pos, symbols_parsed: _ } => {
                        acc.append(&mut nodes);
                        current_pos = next_pos;
                    }
                    ParseStep::Stop { mut nodes, next_pos, symbols_parsed } => {
                        acc.append(&mut nodes);
                        current_pos = next_pos;
                        return Ok(ParseStep::Stop { nodes: acc, next_pos: current_pos, symbols_parsed });
                    }
                }

                // Then try to parse as many more as possible
                loop {
                    let step = self.parse_symbols_once(segments, symbols, current_pos, binding.clone())?;
                    match step {
                        ParseStep::Continue { mut nodes, next_pos, symbols_parsed: _ } => {
                            if next_pos == current_pos && nodes.is_empty() { break; }
                            acc.append(&mut nodes);
                            current_pos = next_pos;
                        }
                        ParseStep::Stop { mut nodes, next_pos, symbols_parsed } => {
                            acc.append(&mut nodes);
                            current_pos = next_pos;
                            return Ok(ParseStep::Stop { nodes: acc, next_pos: current_pos, symbols_parsed });
                        }
                    }
                }
                Ok(ParseStep::Continue { nodes: acc, next_pos: current_pos, symbols_parsed: 1 })
            }
            RepetitionKind::ZeroOrOne => {
                // Try to parse once; Stop just means we get zero items, which is fine for ZeroOrOne
                let step = self.parse_symbols_once(segments, symbols, current_pos, binding.clone())?;
                match step {
                    ParseStep::Continue { mut nodes, next_pos, symbols_parsed: _ } => {
                        debug_trace!("parser", "ZeroOrOne: successfully parsed one iteration");
                        acc.append(&mut nodes);
                        current_pos = next_pos;
                        Ok(ParseStep::Continue { nodes: acc, next_pos: current_pos, symbols_parsed: 1 })
                    }
                    ParseStep::Stop { nodes: _, next_pos: _, symbols_parsed: _ } => {
                        debug_trace!("parser", "ZeroOrOne: parsing stopped, accepting zero items");
                        // For ZeroOrOne, Stop means we successfully parsed zero items
                        Ok(ParseStep::Continue { nodes: acc, next_pos: current_pos, symbols_parsed: 0 })
                    }
                }
            }
        }
    }

    /// Parse symbols once (unified for both single symbols and groups)
    /// Returns ParseStep with nodes and the number of symbols successfully parsed
    fn parse_symbols_once(&mut self, segments: &[Segment], symbols: &[Symbol], seg_pos: usize, binding: Option<String>) -> Result<ParseStep, String> {
        let mut acc: Vec<PartialAST> = Vec::new();
        let mut current_pos = seg_pos;
        let mut symbols_parsed = 0;
        
        for s in symbols {
            let step = self.parse_symbol(segments, s, current_pos, binding.clone())?;
            match step {
                ParseStep::Continue { mut nodes, next_pos, symbols_parsed: step_symbols_parsed } => {
                    acc.append(&mut nodes);
                    let old_pos = current_pos;
                    current_pos = next_pos;
                    // Only count as parsed if we actually parsed something meaningful
                    if step_symbols_parsed > 0 || next_pos > old_pos || !nodes.is_empty() {
                        symbols_parsed += 1;
                    }
                }
                ParseStep::Stop { mut nodes, next_pos, symbols_parsed: step_symbols_parsed } => {
                    acc.append(&mut nodes);
                    current_pos = next_pos;
                    // Count partially parsed symbols
                    if step_symbols_parsed > 0 {
                        symbols_parsed += 1;
                    }
                    return Ok(ParseStep::Stop { nodes: acc, next_pos: current_pos, symbols_parsed });
                }
            }
        }
        Ok(ParseStep::Continue { nodes: acc, next_pos: current_pos, symbols_parsed })
    }
}