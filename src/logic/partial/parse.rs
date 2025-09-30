use super::*;
use crate::debug_trace;
use crate::logic::ast::{ASTNode as CompleteAST, SourceSpan};
use crate::logic::bind::partial::select_branches_by_type;
use crate::logic::grammar::Grammar;
use crate::logic::grammar::RepetitionKind;
use crate::logic::grammar::Symbol;
use crate::logic::tokenizer::Tokenizer;
use crate::logic::typing::{Type, TypingRule};
use std::collections::HashMap;

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
        SourceSpan {
            start: self.start,
            end: self.end,
        }
    }
}

#[derive(Clone, Debug)]
pub enum ParseOutcome {
    Full {
        nodes: Vec<PartialASTNode>,
        next_pos: usize,
        symbols_parsed: usize,
    },
    Partial {
        full_nodes: Vec<PartialASTNode>,
        partial_nodes: Vec<PartialASTNode>,
        next_pos: usize,
        symbols_parsed: usize,
    },
    Error {
        next_pos: usize,
        symbols_parsed: usize,
    },
}

pub struct PartialParser {
    grammar: Grammar,
    tokenizer: Tokenizer,
    /// Optional per-nonterminal type guidance: (rules, expected type)
    type_guidance: HashMap<String, (Vec<TypingRule>, Type)>,
}

impl PartialParser {
    pub fn new(grammar: Grammar) -> Self {
        let specials = grammar.special_tokens.clone();
        Self {
            grammar,
            tokenizer: Tokenizer::new(specials, vec![' ', '\n', '\t']),
            type_guidance: HashMap::new(),
        }
    }

    /// Configure expected type and rules for a given nonterminal name.
    /// During parsing that nonterminal, only branches compatible with the expected type
    /// (under any of the provided rules) will be kept.
    pub fn set_type_guidance(&mut self, nonterminal: &str, rules: Vec<TypingRule>, expected: Type) {
        self.type_guidance
            .insert(nonterminal.to_string(), (rules, expected));
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
        println!(
            "segments: {:?}",
            segments.iter().map(|s| s.text.as_str()).collect::<Vec<_>>()
        );
        debug_trace!("parser", "Built {} segments from input", segments.len());

        // Determine start nonterminal from grammar
        let start_nt = self
            .grammar
            .start_nonterminal()
            .cloned()
            .ok_or_else(|| "No start nonterminal defined in grammar".to_string())?;

        debug_trace!("parser", "Start nonterminal: {}", start_nt);
        let outcome = self.partial_nt(&segments, &start_nt, 0, None)?;
        let total_segments = segments.len();
        match outcome {
            ParseOutcome::Full {
                nodes,
                next_pos,
                symbols_parsed: _,
            } => {
                debug_trace!(
                    "parser",
                    "Start nonterminal parsed successfully, final position: {}",
                    next_pos
                );
                // Enforce full-input consumption: if segments remain, downgrade to Partial
                if next_pos < total_segments {
                    debug_trace!(
                        "parser",
                        "Remaining input after FULL ({} < {}), downgrade to PARTIAL",
                        next_pos,
                        total_segments
                    );
                    let mut parallels: Vec<PartialNonTerminal> = Vec::new();
                    for n in nodes {
                        if let PartialASTNode::NonTerminal(mut ps) = n {
                            parallels.append(&mut ps);
                        }
                    }
                    return Ok(PartialAST::new(
                        PartialASTNode::NonTerminal(parallels),
                        input.to_string(),
                    ));
                }
                let mut parallels: Vec<PartialNonTerminal> = Vec::new();
                for n in nodes {
                    if let PartialASTNode::NonTerminal(mut ps) = n {
                        parallels.append(&mut ps);
                    }
                }
                Ok(PartialAST::new(
                    PartialASTNode::NonTerminal(parallels),
                    input.to_string(),
                ))
            }
            ParseOutcome::Partial {
                mut full_nodes,
                mut partial_nodes,
                next_pos,
                symbols_parsed: _,
                ..
            } => {
                debug_trace!(
                    "parser",
                    "Start nonterminal PARTIAL, last_full_pos: {}",
                    next_pos
                );
                // Merge all parallel productions (from both complete and partial sets) into a single NonTerminal node
                let mut all_parallels: Vec<PartialNonTerminal> = Vec::new();
                for n in full_nodes.drain(..).chain(partial_nodes.drain(..)) {
                    if let PartialASTNode::NonTerminal(mut ps) = n {
                        all_parallels.append(&mut ps);
                    }
                }
                Ok(PartialAST::new(
                    PartialASTNode::NonTerminal(all_parallels),
                    input.to_string(),
                ))
            }
            ParseOutcome::Error {
                next_pos,
                symbols_parsed: _,
            } => {
                debug_trace!(
                    "parser",
                    "Start nonterminal failed to parse, stopped at position: {}",
                    next_pos
                );
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
                let byte_start = chars
                    .iter()
                    .take(char_start)
                    .map(|c| c.len_utf8())
                    .sum::<usize>();
                let byte_end = chars
                    .iter()
                    .take(char_end)
                    .map(|c| c.len_utf8())
                    .sum::<usize>();
                // Extract text using character-based slicing to be safe
                let text: String = chars[char_start..char_end].iter().collect();
                Segment::new(text, byte_start, byte_end)
            })
            .collect();

        debug_trace!(
            "parser",
            "Created {} segments: {:?}",
            segments.len(),
            segments.iter().map(|s| &s.text).collect::<Vec<_>>()
        );
        Ok(segments)
    }

    pub fn partial_nt(
        &mut self,
        segments: &[Segment],
        nt: &str,
        seg_pos: usize,
        binding: Option<String>,
    ) -> Result<ParseOutcome, String> {
        debug_trace!(
            "parser",
            "Parsing nonterminal '{}' at segment position {} with binding {:?}",
            nt,
            seg_pos,
            binding
        );
        let productions = self
            .grammar
            .productions
            .get(nt)
            .cloned()
            .ok_or_else(|| format!("No productions found for nonterminal '{}'", nt))?;

        debug_trace!(
            "parser",
            "Found {} productions for nonterminal '{}'",
            productions.len(),
            nt
        );

        let mut complete_parallels: Vec<PartialNonTerminal> = Vec::new();
        let mut partial_parallels: Vec<PartialNonTerminal> = Vec::new();
        let mut max_complete_pos = seg_pos; // Track last fully parsed position across complete productions

        for (prod_idx, prod) in productions.iter().enumerate() {
            debug_trace!(
                "parser",
                "Parsing production {}/{} for '{}': {:?} at segment position {}",
                prod_idx + 1,
                productions.len(),
                nt,
                prod.rhs,
                seg_pos
            );
            // Parse the entire RHS as one sequence; Stop will end this branch early, Continue means full sequence processed
            let step = self.parse_symbols_once(segments, &prod.rhs, seg_pos, None)?;
            let (
                nt_children,
                current_seg_pos,
                is_complete,
                fully_symbols_parsed,
                partially_symbols_parsed,
                partial_symbol,
            ) = match step {
                ParseOutcome::Full {
                    nodes,
                    next_pos,
                    symbols_parsed,
                } => {
                    debug_trace!(
                        "parser",
                        "PROD {}/{} FULL: seg {} -> {}, symbols_parsed={} (nt={})",
                        prod_idx + 1,
                        productions.len(),
                        seg_pos,
                        next_pos,
                        symbols_parsed,
                        nt
                    );
                    (nodes, next_pos, true, symbols_parsed, 0, None)
                }
                ParseOutcome::Partial {
                    full_nodes,
                    partial_nodes,
                    next_pos,
                    symbols_parsed,
                } => {
                    debug_trace!(
                        "parser",
                        "PROD {}/{} PARTIAL: last_full_p²os={}, full_nodes={}, partial_nodes={}, symbols_parsed={} (nt={})",
                        prod_idx + 1,
                        productions.len(),
                        next_pos,
                        full_nodes.len(),
                        partial_nodes.len(),
                        symbols_parsed,
                        nt
                    );
                    let mut children = full_nodes;
                    if !partial_nodes.is_empty() {
                        children.extend(partial_nodes.clone());
                    }
                    // Mark an in-progress symbol when a production is Partial
                    // Infer which symbol index is in-progress and whether it's a literal or regex
                    let rhs_index = symbols_parsed;
                    let partial_meta = if rhs_index < prod.rhs.len() {
                        // Determine terminal kind from the symbol at rhs_index (resolve through Single)
                        let mut sym = &prod.rhs[rhs_index];
                        // Capture wrapper metadata if present
                        let (wrapper_rep, wrapper_binding) = match sym {
                            Symbol::Single {
                                repetition,
                                binding,
                                ..
                            } => (repetition.clone(), binding.clone()),
                            Symbol::Group { repetition, .. } => (repetition.clone(), None),
                            _ => (None, None),
                        };
                        // Unwrap Single to inspect inner symbol
                        if let Symbol::Single { value, .. } = sym {
                            sym = value.as_ref();
                        }
                        match sym {
                            Symbol::Litteral(s) => {
                                segments.get(next_pos).map(|seg| PartialSymbol::Litteral {
                                    symbol_index: rhs_index,
                                    byte_cursor: seg.end - seg.start,
                                    span: seg.span(),
                                    expected: s.clone(),
                                })
                            }
                            Symbol::Regex(re) => {
                                segments.get(next_pos).map(|seg| PartialSymbol::Regex {
                                    symbol_index: rhs_index,
                                    byte_cursor: seg.end - seg.start,
                                    span: seg.span(),
                                    re: re.clone(),
                                })
                            }
                            Symbol::Expression(nt) => Some(PartialSymbol::Expression {
                                symbol_index: rhs_index,
                                nt: nt.clone(),
                            }),
                            Symbol::Group { .. } => Some(PartialSymbol::Group {
                                symbol_index: rhs_index,
                                inner_index: None,
                                repetition: wrapper_rep.clone(),
                                binding: wrapper_binding.clone(),
                            }),
                            Symbol::Single { .. } => Some(PartialSymbol::Single {
                                symbol_index: rhs_index,
                                repetition: wrapper_rep.clone(),
                                binding: wrapper_binding.clone(),
                            }),
                        }
                    } else {
                        None
                    };
                    println!(
                        "partial_meta for {} idx {} -> {:?}",
                        nt, rhs_index, partial_meta
                    );
                    (children, next_pos, false, symbols_parsed, 1, partial_meta)
                }
                ParseOutcome::Error {
                    next_pos,
                    symbols_parsed,
                } => {
                    debug_trace!(
                        "parser",
                        "PROD {}/{} ERROR: seg {} -> {}, symbols_parsed={} (nt={})",
                        prod_idx + 1,
                        productions.len(),
                        seg_pos,
                        next_pos,
                        symbols_parsed,
                        nt
                    );
                    (Vec::new(), next_pos, false, symbols_parsed, 0, None)
                }
            };

            // Track the maximum fully parsed position from complete productions
            if is_complete {
                max_complete_pos = max_complete_pos.max(current_seg_pos);
            }

            // Create nonterminals for complete and partial productions whenever there was progress,
            // even if children are currently empty. This allows visualizing incomplete endings as
            // partial trees instead of raising a hard error at the top level.
            if is_complete || fully_symbols_parsed > 0 || partially_symbols_parsed > 0 {
                let partial_prod = if partial_symbol.is_some() {
                    PartialProduction::from_progress_with_partial_symbol(
                        prod.clone(),
                        fully_symbols_parsed,
                        partially_symbols_parsed,
                        partial_symbol,
                    )
                } else {
                    PartialProduction::from_progress(
                        prod.clone(),
                        fully_symbols_parsed,
                        partially_symbols_parsed,
                    )
                };
                let nt_node = PartialNonTerminal {
                    production: partial_prod,
                    children: nt_children,
                    value: nt.to_string(),
                    span: if current_seg_pos > seg_pos {
                        segments.get(seg_pos).and_then(|start_seg| {
                            segments.get(current_seg_pos - 1).map(|end_seg| SourceSpan {
                                start: start_seg.start,
                                end: end_seg.end,
                            })
                        })
                    } else {
                        None
                    },
                    binding: binding.clone(),
                    bound_typing_rule: None,
                };
                if is_complete {
                    println!(
                        "nt {} complete children={} (prod idx {})",
                        nt,
                        nt_node.children.len(),
                        prod_idx
                    );
                    complete_parallels.push(nt_node);
                } else {
                    println!(
                        "nt {} partial children={} (prod idx {})",
                        nt,
                        nt_node.children.len(),
                        prod_idx
                    );
                    partial_parallels.push(nt_node);
                }
            }
        }

        debug_trace!(
            "parser",
            "NT '{}' result: complete={}, partial={}, last_full_pos={}",
            nt,
            complete_parallels.len(),
            partial_parallels.len(),
            max_complete_pos
        );
        // Apply optional type-based pruning if guidance is configured for this nonterminal
        if let Some((rules, expected)) = self.type_guidance.get(nt) {
            if !complete_parallels.is_empty() {
                let selected = select_branches_by_type(&complete_parallels, rules, expected);
                complete_parallels = selected.into_iter().cloned().collect();
            }
            if !partial_parallels.is_empty() {
                let selected = select_branches_by_type(&partial_parallels, rules, expected);
                partial_parallels = selected.into_iter().cloned().collect();
            }
        }

        // If there is at least one complete alternative, return FULL, but include
        // both complete and partial alternatives so the caller can visualize the full tree.
        if !complete_parallels.is_empty() {
            let mut all = complete_parallels;
            if !partial_parallels.is_empty() {
                all.extend(partial_parallels);
            }
            Ok(ParseOutcome::Full {
                nodes: vec![PartialASTNode::NonTerminal(all)],
                next_pos: max_complete_pos,
                symbols_parsed: 1,
            })
        } else if !partial_parallels.is_empty() {
            Ok(ParseOutcome::Partial {
                full_nodes: Vec::new(),
                partial_nodes: vec![PartialASTNode::NonTerminal(partial_parallels)],
                next_pos: max_complete_pos,
                symbols_parsed: 0,
            })
        } else {
            Ok(ParseOutcome::Error {
                next_pos: seg_pos,
                symbols_parsed: 0,
            })
        }
    }

    /// Parse a single grammar symbol from segments and return control flow
    fn parse_symbol(
        &mut self,
        segments: &[Segment],
        symbol: &Symbol,
        seg_pos: usize,
        parent_binding: Option<String>,
    ) -> Result<ParseOutcome, String> {
        debug_trace!(
            "parser",
            "SYMBOL {:?} @{} binding {:?}",
            symbol,
            seg_pos,
            parent_binding
        );
        // If no next segment stop parsing: this is an incomplete input -> PARTIAL, not a hard error
        if segments.get(seg_pos).is_none() {
            debug_trace!(
                "parser",
                "No segment at position {}, input exhausted -> PARTIAL",
                seg_pos
            );
            return Ok(ParseOutcome::Partial {
                full_nodes: Vec::new(),
                partial_nodes: Vec::new(),
                next_pos: seg_pos,
                symbols_parsed: 0,
            });
        }
        let segment = segments[seg_pos].clone();
        debug_trace!("parser", "Current segment: '{}'", segment.text);
        match symbol {
            Symbol::Litteral(value) => {
                // Must match the NEXT segment exactly; do not scan ahead.
                // - If next segment exists but doesn't match, just stop without adding any node.
                debug_trace!(
                    "parser",
                    "LITERAL '{}' vs '{}' @{}",
                    value,
                    segment.text,
                    seg_pos
                );
                if &segment.text == value {
                    debug_trace!("parser", "Literal match successful");
                    let ast_node = PartialASTNode::Terminal(PartialTerminal {
                        value: segment.text.clone(),
                        span: Some(segment.span()),
                        binding: parent_binding,
                    });
                    Ok(ParseOutcome::Full {
                        nodes: vec![ast_node],
                        next_pos: seg_pos + 1,
                        symbols_parsed: 1,
                    })
                } else {
                    debug_trace!(
                        "parser",
                        "Literal match failed: '{}' != '{}'",
                        segment.text,
                        value
                    );
                    // If we're at the last available segment and it is a prefix of the expected literal,
                    // treat this as PARTIAL so the input can be completed to the right.
                    let is_last_segment = seg_pos + 1 >= segments.len();
                    if is_last_segment
                        && value.starts_with(&segment.text)
                        && !segment.text.is_empty()
                    {
                        debug_trace!(
                            "parser",
                            "Literal prefix detected at end-of-input -> PARTIAL"
                        );
                        Ok(ParseOutcome::Partial {
                            full_nodes: Vec::new(),
                            partial_nodes: Vec::new(),
                            next_pos: seg_pos,
                            symbols_parsed: 0,
                        })
                    } else {
                        // True mismatch: there exists a segment but it doesn't match the expected literal
                        Ok(ParseOutcome::Error {
                            next_pos: seg_pos,
                            symbols_parsed: 0,
                        })
                    }
                }
            }
            Symbol::Expression(value) => {
                debug_trace!("parser", "EXPR '{}' @{}", value, seg_pos);
                // Parse the nonterminal and return its ParseStep directly
                let step = self.partial_nt(segments, value, seg_pos, parent_binding)?;
                match step {
                    ParseOutcome::Full {
                        nodes,
                        next_pos,
                        symbols_parsed,
                    } => {
                        debug_trace!(
                            "parser",
                            "EXPR '{}' -> FULL: {} -> {} (symbols={})",
                            value,
                            seg_pos,
                            next_pos,
                            symbols_parsed
                        );
                        Ok(ParseOutcome::Full {
                            nodes,
                            next_pos,
                            symbols_parsed,
                        })
                    }
                    ParseOutcome::Partial {
                        full_nodes: nodes,
                        partial_nodes,
                        next_pos,
                        symbols_parsed,
                    } => {
                        debug_trace!(
                            "parser",
                            "EXPR '{}' -> PARTIAL: last_full_pos={} (full_nodes={})",
                            value,
                            next_pos,
                            nodes.len()
                        );
                        Ok(ParseOutcome::Partial {
                            full_nodes: nodes,
                            partial_nodes,
                            next_pos,
                            symbols_parsed,
                        })
                    }
                    ParseOutcome::Error {
                        next_pos,
                        symbols_parsed,
                    } => {
                        debug_trace!(
                            "parser",
                            "EXPR '{}' -> ERROR: {} -> {} (symbols={})",
                            value,
                            seg_pos,
                            next_pos,
                            symbols_parsed
                        );
                        Ok(ParseOutcome::Error {
                            next_pos,
                            symbols_parsed,
                        })
                    }
                }
            }
            Symbol::Regex(re) => {
                debug_trace!("parser", "REGEX vs '{}' @{}", segment.text, seg_pos);
                let full_match = re
                    .find(&segment.text)
                    .map(|m| m.start() == 0 && m.end() == segment.text.len())
                    .unwrap_or(false);
                if full_match {
                    debug_trace!("parser", "Regex match successful");
                    let ast_node = PartialASTNode::Terminal(PartialTerminal {
                        value: segment.text.clone(),
                        span: Some(segment.span()),
                        binding: parent_binding,
                    });
                    Ok(ParseOutcome::Full {
                        nodes: vec![ast_node],
                        next_pos: seg_pos + 1,
                        symbols_parsed: 1,
                    })
                } else {
                    debug_trace!("parser", "Regex match failed");
                    // If we're at the last available segment and the regex can match a string that starts with current segment,
                    // we treat as PARTIAL to allow completion to the right.
                    let is_last_segment = seg_pos + 1 >= segments.len();
                    if is_last_segment {
                        // Conservative prefix check: if the regex matches any string with this prefix.
                        // We approximate by testing regex on a synthetic longer string (prefix + "x") and ensure it can match from start.
                        let synthetic = format!("{}x", segment.text);
                        let could_extend =
                            re.find(&synthetic).map(|m| m.start() == 0).unwrap_or(false);
                        if could_extend && !segment.text.is_empty() {
                            debug_trace!(
                                "parser",
                                "Regex prefix detected at end-of-input -> PARTIAL"
                            );
                            return Ok(ParseOutcome::Partial {
                                full_nodes: Vec::new(),
                                partial_nodes: Vec::new(),
                                next_pos: seg_pos,
                                symbols_parsed: 0,
                            });
                        }
                    }
                    Ok(ParseOutcome::Error {
                        next_pos: seg_pos,
                        symbols_parsed: 0,
                    })
                }
            }
            Symbol::Single {
                value,
                binding,
                repetition,
            } => {
                debug_trace!(
                    "parser",
                    "Parsing single symbol with binding {:?} and repetition {:?}",
                    binding,
                    repetition
                );
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
                        self.parse_symbols_repetition(
                            segments,
                            &single_symbol_vec,
                            seg_pos,
                            effective_binding,
                            rep_kind,
                        )
                    }
                }
            }
            Symbol::Group {
                symbols,
                repetition,
            } => {
                debug_trace!(
                    "parser",
                    "Parsing group of {} symbols with repetition {:?}",
                    symbols.len(),
                    repetition
                );
                match repetition {
                    None => {
                        debug_trace!("parser", "No repetition, parsing group once");
                        // No repetition, parse the group exactly once using the unified method
                        let step =
                            self.parse_symbols_once(segments, symbols, seg_pos, parent_binding)?;
                        Ok(step)
                    }
                    Some(rep_kind) => {
                        debug_trace!("parser", "Handling group repetition: {:?}", rep_kind);
                        // Handle repetition for a group of symbols
                        self.parse_symbols_repetition(
                            segments,
                            symbols,
                            seg_pos,
                            parent_binding,
                            rep_kind,
                        )
                    }
                }
            }
        }
    }

    /// Parse symbols with repetition (generalized for both single symbols and groups)
    fn parse_symbols_repetition(
        &mut self,
        segments: &[Segment],
        symbols: &[Symbol],
        seg_pos: usize,
        binding: Option<String>,
        rep_kind: &RepetitionKind,
    ) -> Result<ParseOutcome, String> {
        debug_trace!("parser", "REPEAT {:?} start_pos {}", rep_kind, seg_pos);
        let mut acc: Vec<PartialASTNode> = Vec::new();
        let _partial_acc: Vec<PartialASTNode> = Vec::new(); // kept for future use in more granular partial propagation
        let mut current_pos = seg_pos;
        let mut last_before_pos = seg_pos;

        match rep_kind {
            RepetitionKind::ZeroOrMore => {
                debug_trace!("parser", "REPEAT ZeroOrMore: loop start @{}", current_pos);
                // Parse as many times as possible; Stop just means we're done, not failure
                loop {
                    let step =
                        self.parse_symbols_once(segments, symbols, current_pos, binding.clone())?;
                    match step {
                        ParseOutcome::Full {
                            mut nodes,
                            next_pos,
                            symbols_parsed: _,
                        } => {
                            if next_pos == current_pos && nodes.is_empty() {
                                // safety: avoid infinite loop
                                debug_trace!("parser", "REPEAT ZeroOrMore: no progress, break");
                                break;
                            }
                            debug_trace!(
                                "parser",
                                "REPEAT ZeroOrMore: FULL iteration -> pos {} -> {} (+{} nodes)",
                                current_pos,
                                next_pos,
                                nodes.len()
                            );
                            acc.append(&mut nodes);
                            // last fully parsed position is the end of this successful iteration
                            last_before_pos = next_pos;
                            current_pos = next_pos;
                        }
                        ParseOutcome::Error {
                            next_pos: _,
                            symbols_parsed: _,
                        } => {
                            debug_trace!("parser", "REPEAT ZeroOrMore: ERROR (natural stop)");
                            // For ZeroOrMore, Stop just means we're done - don't include partial nodes from failed attempt
                            // and don't update position since this attempt didn't succeed
                            break;
                        }
                        ParseOutcome::Partial {
                            mut full_nodes,
                            mut partial_nodes,
                            next_pos: _,
                            symbols_parsed: _,
                            ..
                        } => {
                            // Inner produced a right-completable partial. Provide two branches:
                            // - full_nodes: the zero-or-more SKIP path (acc as-is)
                            // - partial_nodes: the in-progress repetition attempt (combine inner full + partial)
                            debug_trace!(
                                "parser",
                                "REPEAT ZeroOrMore: PARTIAL (accum {} nodes) -> parallel SKIP and PARTIAL",
                                full_nodes.len()
                            );
                            let mut combined_partial = Vec::new();
                            combined_partial.append(&mut full_nodes);
                            combined_partial.append(&mut partial_nodes);
                            return Ok(ParseOutcome::Partial {
                                full_nodes: acc,
                                partial_nodes: combined_partial,
                                next_pos: current_pos,
                                symbols_parsed: 1,
                            });
                        }
                    }
                }
                debug_trace!(
                    "parser",
                    "REPEAT ZeroOrMore: done. last_full_pos={}, nodes_total={}",
                    last_before_pos,
                    acc.len()
                );
                Ok(ParseOutcome::Full {
                    nodes: acc,
                    next_pos: last_before_pos,
                    symbols_parsed: 1,
                })
            }
            RepetitionKind::OneOrMore => {
                debug_trace!(
                    "parser",
                    "REPEAT OneOrMore: must parse at least once @{}",
                    current_pos
                );
                // Must parse at least once; if first attempt Stops, propagate Stop
                let step =
                    self.parse_symbols_once(segments, symbols, current_pos, binding.clone())?;
                match step {
                    ParseOutcome::Full {
                        mut nodes,
                        next_pos,
                        symbols_parsed: _,
                    } => {
                        debug_trace!(
                            "parser",
                            "REPEAT OneOrMore: first FULL -> {} -> {} (+{} nodes)",
                            current_pos,
                            next_pos,
                            nodes.len()
                        );
                        acc.append(&mut nodes);
                        current_pos = next_pos;
                    }
                    ParseOutcome::Error {
                        next_pos,
                        symbols_parsed,
                    } => {
                        debug_trace!(
                            "parser",
                            "REPEAT OneOrMore: first ERROR -> {} (symbols={}), treat as PARTIAL zero matches",
                            next_pos,
                            symbols_parsed
                        );
                        return Ok(ParseOutcome::Partial {
                            full_nodes: acc,
                            partial_nodes: Vec::new(),
                            next_pos: seg_pos,
                            symbols_parsed,
                        });
                    }
                    ParseOutcome::Partial {
                        full_nodes,
                        partial_nodes,
                        next_pos,
                        symbols_parsed: _,
                        ..
                    } => {
                        debug_trace!(
                            "parser",
                            "REPEAT OneOrMore: first PARTIAL -> propagate PARTIAL"
                        );
                        acc.extend(full_nodes);
                        return Ok(ParseOutcome::Partial {
                            full_nodes: acc,
                            partial_nodes,
                            next_pos,
                            symbols_parsed: 1,
                        });
                    }
                }

                // Continue parsing additional repetitions from where we left off
                let step = self.parse_symbols_repetition(
                    segments,
                    symbols,
                    current_pos,
                    binding,
                    &RepetitionKind::ZeroOrMore,
                )?;
                match step {
                    ParseOutcome::Full {
                        mut nodes,
                        next_pos,
                        symbols_parsed: _,
                    } => {
                        debug_trace!(
                            "parser",
                            "REPEAT OneOrMore: subsequent FULL (+{} nodes) -> pos {}",
                            nodes.len(),
                            next_pos
                        );
                        acc.append(&mut nodes);
                        current_pos = next_pos;
                    }
                    ParseOutcome::Error {
                        next_pos: _,
                        symbols_parsed: _,
                    } => {
                        debug_trace!("parser", "REPEAT OneOrMore: subsequent ERROR, end");
                        // For OneOrMore, Stop just means we're done - don't include partial nodes from failed attempt
                        // and don't update position since this attempt didn't succeed
                    }
                    ParseOutcome::Partial {
                        full_nodes,
                        partial_nodes,
                        next_pos,
                        symbols_parsed: _,
                        ..
                    } => {
                        debug_trace!(
                            "parser",
                            "REPEAT OneOrMore: subsequent PARTIAL -> propagate PARTIAL"
                        );
                        acc.extend(full_nodes);
                        return Ok(ParseOutcome::Partial {
                            full_nodes: acc,
                            partial_nodes,
                            next_pos,
                            symbols_parsed: 1,
                        });
                    }
                }
                debug_trace!(
                    "parser",
                    "REPEAT OneOrMore: done at pos {}, nodes_total={}",
                    current_pos,
                    acc.len()
                );
                Ok(ParseOutcome::Full {
                    nodes: acc,
                    next_pos: current_pos,
                    symbols_parsed: 1,
                })
            }
            RepetitionKind::ZeroOrOne => {
                // Try to parse once; failure means we accept zero items and do not advance
                let step =
                    self.parse_symbols_once(segments, symbols, current_pos, binding.clone())?;
                match step {
                    ParseOutcome::Full {
                        mut nodes,
                        next_pos,
                        symbols_parsed,
                    } => {
                        debug_trace!(
                            "parser",
                            "REPEAT ZeroOrOne: FULL (+{} nodes) advance to {}",
                            nodes.len(),
                            next_pos
                        );
                        acc.append(&mut nodes);
                        current_pos = next_pos;
                        // Successful optional match: advance like a normal symbol
                        Ok(ParseOutcome::Full {
                            nodes: acc,
                            next_pos: current_pos,
                            symbols_parsed: if symbols_parsed == 0 {
                                1
                            } else {
                                symbols_parsed
                            },
                        })
                    }
                    ParseOutcome::Error { .. } => {
                        debug_trace!(
                            "parser",
                            "REPEAT ZeroOrOne: not present, stay at {}",
                            seg_pos
                        );
                        Ok(ParseOutcome::Full {
                            nodes: Vec::new(),
                            next_pos: seg_pos,
                            symbols_parsed: 1,
                        })
                    }
                    ParseOutcome::Partial {
                        mut full_nodes,
                        partial_nodes,
                        next_pos,
                        symbols_parsed: _,
                        ..
                    } => {
                        debug_trace!(
                            "parser",
                            "REPEAT ZeroOrOne: inner PARTIAL -> propagate PARTIAL"
                        );
                        acc.append(&mut full_nodes);
                        Ok(ParseOutcome::Partial {
                            full_nodes: acc,
                            partial_nodes,
                            next_pos,
                            symbols_parsed: 1,
                        })
                    }
                }
            }
        }
    }

    /// Parse symbols once (unified for both single symbols and groups)
    /// Returns ParseStep with nodes and the number of symbols successfully parsed
    fn parse_symbols_once(
        &mut self,
        segments: &[Segment],
        symbols: &[Symbol],
        seg_pos: usize,
        binding: Option<String>,
    ) -> Result<ParseOutcome, String> {
        let mut acc: Vec<PartialASTNode> = Vec::new();
        let mut partial_acc: Vec<PartialASTNode> = Vec::new();
        let mut encountered_partial = false;
        let mut current_pos = seg_pos;
        let mut symbols_parsed = 0;

        for s in symbols.iter() {
            let step = self.parse_symbol(segments, s, current_pos, binding.clone())?;
            match step {
                ParseOutcome::Full {
                    mut nodes,
                    next_pos,
                    symbols_parsed: step_symbols_parsed,
                } => {
                    println!(
                        "full in parse_symbols_once for symbol {:?}: nodes={} next_pos={} -> {}",
                        s,
                        nodes.len(),
                        current_pos,
                        next_pos
                    );
                    acc.append(&mut nodes);
                    let old_pos = current_pos;
                    current_pos = next_pos;
                    // Only count as parsed if we actually parsed something meaningful
                    if step_symbols_parsed > 0 || next_pos > old_pos || !nodes.is_empty() {
                        symbols_parsed += 1;
                    }
                }
                ParseOutcome::Partial {
                    mut full_nodes,
                    mut partial_nodes,
                    next_pos,
                    symbols_parsed: _,
                    ..
                } => {
                    println!(
                        "partial in parse_symbols_once for symbol {:?}: full_nodes={}, partial_nodes={}",
                        s,
                        full_nodes.len(),
                        partial_nodes.len()
                    );
                    // Partial at this symbol: keep both branches and continue parsing subsequent symbols
                    // from the provided rewind position (typically same as current_pos for repetitions).
                    // This allows cases like ParamDecl* to be skipped and still match following ')'.
                    debug_trace!(
                        "parser",
                        "SYMBOLS_ONCE PARTIAL: +{} nodes for {:?}; continue from {}",
                        full_nodes.len(),
                        s,
                        next_pos
                    );
                    acc.append(&mut full_nodes);
                    current_pos = next_pos; // do not advance beyond the partial point
                    if !full_nodes.is_empty() {
                        symbols_parsed += 1;
                    }
                    partial_acc.append(&mut partial_nodes);
                    encountered_partial = true;
                }
                ParseOutcome::Error {
                    next_pos,
                    symbols_parsed: step_symbols_parsed,
                } => {
                    debug_trace!(
                        "parser",
                        "SYMBOLS_ONCE ERROR: pos {} -> {} (symbols={})",
                        current_pos,
                        next_pos,
                        step_symbols_parsed
                    );
                    current_pos = next_pos;
                    // Count partially parsed symbols
                    symbols_parsed += step_symbols_parsed;
                    // Propagate error upward (end-of-input becomes Partial earlier)
                    return Ok(ParseOutcome::Error {
                        next_pos: current_pos,
                        symbols_parsed,
                    });
                }
            }
        }
        if encountered_partial {
            println!(
                "returning PARTIAL from parse_symbols_once: full_nodes={}, partial_nodes={}, range {}->{}",
                acc.len(),
                partial_acc.len(),
                seg_pos,
                current_pos
            );
            debug_trace!(
                "parser",
                "SYMBOLS_ONCE PARTIAL: last_full_pos={} (symbols={})",
                current_pos,
                symbols_parsed
            );
            Ok(ParseOutcome::Partial {
                full_nodes: acc,
                partial_nodes: partial_acc,
                next_pos: current_pos,
                symbols_parsed,
            })
        } else {
            println!(
                "returning FULL from parse_symbols_once: nodes={} range {}->{}, symbols={}",
                acc.len(),
                seg_pos,
                current_pos,
                symbols_parsed
            );
            debug_trace!(
                "parser",
                "SYMBOLS_ONCE FULL: {} -> {} (symbols={})",
                seg_pos,
                current_pos,
                symbols_parsed
            );
            Ok(ParseOutcome::Full {
                nodes: acc,
                next_pos: current_pos,
                symbols_parsed,
            })
        }
    }
}
