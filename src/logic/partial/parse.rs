use crate::logic::parser::Parser;
use crate::{debug_info, debug_debug};
use crate::logic::grammar::{RepetitionKind};

use super::{PartialOutcome, PartialProduction, PartialState, ParseResult, State, PartialASTNode, PartialNonTerminal};

impl Parser {
    /// Entry point: perform a partial parse. Always returns a `PartialOutcome` wrapped in Result
    /// (Result only reflects tokenizer/init failures). Grammar exploration result is encoded in `PartialOutcome`.
    pub fn partial(&mut self, input: &str) -> Result<PartialOutcome, String> {
        debug_info!("partial", "Starting partial parse of: '{}'", input);
        self.init(input)?;
        let start_nt = self
            .grammar
            .start_nonterminal()
            .ok_or("No start nonterminal defined in grammar")?
            .clone();
        debug_info!("partial", "Start nonterminal: {}", start_nt);

        let (states, _final_pos) = match self.partial_recursive_at_pos(start_nt.as_str(), None, self.position()) {
            Ok(res) => res,
            Err(e) => return Ok(PartialOutcome::Error(e)),
        };

        let mut partials: Vec<PartialState> = Vec::new();
        let mut completes: Vec<PartialASTNode> = Vec::new();
        let mut expandables: Vec<PartialASTNode> = Vec::new();
        for s in states.into_iter() {
            match s {
                State::Partial(p) => partials.push(p),
                State::Complete(n) => completes.push(n),
                State::Expandable(n) => expandables.push(n),
            }
        }

        // Prefer returning Expandable/Complete parses when available
        if let Some(node) = expandables.into_iter().next() {
            match node.clone().into_ast() {
                Ok(ast) => return Ok(PartialOutcome::Expandable { node: ast }),
                Err(e) => return Ok(PartialOutcome::Error(e)),
            }
        }
        if let Some(node) = completes.into_iter().next() {
            match node.clone().into_ast() {
                Ok(ast) => return Ok(PartialOutcome::Complete { node: ast }),
                Err(e) => return Ok(PartialOutcome::Error(e)),
            }
        }

        // Otherwise, return all partial states if any exist
        if !partials.is_empty() {
            let mut filtered: Vec<PartialState> = Vec::new();
            let mut seen_semantic_hashes: std::collections::HashSet<u64> = std::collections::HashSet::new();
            for st in partials.into_iter() {
                let semantic_hash = st.calculate_semantic_hash();
                if seen_semantic_hashes.insert(semantic_hash) {
                    filtered.push(st);
                } else {
                    debug_debug!("partial", "Filtered duplicate semantic state with hash: {}", semantic_hash);
                }
            }
            return Ok(PartialOutcome::Incomplete { states: filtered });
        }

        Ok(PartialOutcome::Error(format!(
            "Syntax error at start symbol '{}'",
            start_nt
        )))
    }

    /// Recursively attempt to (partially) parse a nonterminal.
    /// Returns all possible states (Complete or Partial) for the given nonterminal.
    pub fn partial_recursive(&mut self, nonterminal: &str, binding: Option<String>) -> ParseResult {
        Ok(match self.partial_recursive_at_pos(nonterminal, binding, self.position()) {
            Ok((states, _final_pos)) => states,
            Err(e) => return Err(e),
        })
    }

    /// Recursively attempt to (partially) parse a nonterminal starting at a specific position.
    /// This allows for scoped position management instead of global position tracking.
    pub(super) fn partial_recursive_at_pos(&mut self, symbol: &str, binding: Option<String>, pos: usize) -> Result<(Vec<State>, usize), String> {
        let nonterminal = symbol;
        let start_pos = pos;
        
        debug_debug!(
            "partial",
            "Entering partial_recursive for '{}' at pos {}",
            nonterminal,
            start_pos
        );
        let productions = match self.grammar.productions.get(nonterminal).cloned() { Some(p)=>p, None=> return Err(format!("No productions found for symbol '{}'", nonterminal)) };

        let mut states: Vec<State> = Vec::new();
        let mut last_err: Option<String> = None;
        let mut max_pos_reached = start_pos; // Track the maximum position reached

        for production in productions.iter() {
            debug_debug!("partial", "Trying production: {:?}", production);
            let mut children: Vec<PartialASTNode> = Vec::new();
            let mut current_pos = start_pos; // Track position for this production attempt
            let mut failed = false;
            for (idx, symbol) in production.rhs.iter().enumerate() {
                debug_debug!("partial", "Matching symbol {} (idx {}): {:?}", symbol.value(), idx, symbol);
                
                let outcomes = match self.partial_match_symbol_at_pos(symbol, current_pos) { 
                    Ok((o, new_pos)) => {
                        debug_debug!("partial", "Symbol {} consumed position {} -> {}", symbol.value(), current_pos, new_pos);
                        current_pos = new_pos; // Update position based on consumption
                        max_pos_reached = max_pos_reached.max(new_pos); // Track maximum position
                        o
                    },
                    Err(e) => { 
                        last_err = Some(e); 
                        failed = true; 
                        break; 
                    } 
                };

                // Separate complete, expandable, and partial outcomes
                let mut complete_outcomes = Vec::new();
                let mut expandable_outcomes = Vec::new();
                let mut partial_outcomes = Vec::new();
                for st in outcomes {
                    match st {
                        State::Complete(node) => complete_outcomes.push(node),
                        State::Expandable(node) => expandable_outcomes.push(node),
                        State::Partial(partial) => partial_outcomes.push(partial),
                    }
                }

                // Check if we have meaningful partial outcomes before processing them
                let has_meaningful_partials = !partial_outcomes.is_empty();

                // Create partial states for each partial outcome
                for child_partial in partial_outcomes {
                    let mut tmp_children = children.clone();
                    tmp_children.push(child_partial.ast.clone());
                    let partial_ast = self.build_partial_ast(nonterminal, binding.clone(), &tmp_children, start_pos, current_pos);
                    let mut partial_production = PartialProduction::from_index(production.clone(), idx);
                    // Mark symbols as parsed up to current index
                    for i in 0..idx {
                        if let Some(sym) = production.rhs.get(i) {
                            partial_production.advance_parsed(sym.clone());
                        }
                    }
                    // Mark current symbol as partial since it didn't complete
                    if let Some(symbol) = production.rhs.get(idx) {
                        partial_production.set_partial(symbol.clone());
                    }
                    let pstate = PartialState { ast: partial_ast, final_production: partial_production };
                    states.push(State::Partial(pstate));
                }

                // Create partial states for each expandable outcome (they're expandable, so mark as partial)
                for expandable_node in &expandable_outcomes {
                    let mut tmp_children = children.clone();
                    tmp_children.push(expandable_node.clone());
                    let partial_ast = self.build_partial_ast(nonterminal, binding.clone(), &tmp_children, start_pos, current_pos);
                    let mut partial_production = PartialProduction::from_index(production.clone(), idx);
                    // Mark symbols as parsed up to current index
                    for i in 0..idx {
                        if let Some(sym) = production.rhs.get(i) {
                            partial_production.advance_parsed(sym.clone());
                        }
                    }
                    // Mark current symbol as partial since it's expandable
                    if let Some(symbol) = production.rhs.get(idx) {
                        partial_production.set_partial(symbol.clone());
                    }
                    let pstate = PartialState { ast: partial_ast, final_production: partial_production };
                    states.push(State::Partial(pstate));
                }

                // Create partial states for complete outcomes only at EOF if there are remaining symbols
                if current_pos >= self.tokens.len() && idx + 1 < production.rhs.len() {
                    for complete_node in &complete_outcomes {
                        let mut tmp_children = children.clone();
                        tmp_children.push(complete_node.clone());
                        // Create state showing expected next symbol without redundant Missing nodes
                        let partial_ast = self.build_partial_ast(nonterminal, binding.clone(), &tmp_children, start_pos, current_pos);
                        let mut partial_production = PartialProduction::from_index(production.clone(), idx + 1);
                        for i in 0..=idx {
                            if let Some(sym) = production.rhs.get(i) {
                                partial_production.advance_parsed(sym.clone());
                            }
                        }
                        let pstate = PartialState { ast: partial_ast, final_production: partial_production };
                        states.push(State::Partial(pstate));
                    }
                }

                // Use the first complete or expandable outcome to advance this production (if any)
                let advance_node = complete_outcomes.into_iter().next()
                    .or_else(|| expandable_outcomes.into_iter().next());
                
                if let Some(advance_node) = advance_node {
                    debug_debug!("partial", "Symbol {} completed: {}", symbol.value(), advance_node.value());
                    children.push(advance_node);
                    continue;
                } else {
                    // Create a shallow partial state if we haven't already created meaningful partial states
                    // and we're at EOF or have made some progress, but only if we would add meaningful Missing information
                    let at_eof = current_pos >= self.tokens.len();
                    
                    if !has_meaningful_partials && (at_eof || !children.is_empty()) {
                        let partial_ast = self.build_partial_ast(nonterminal, binding.clone(), &children, start_pos, current_pos);
                        let mut partial_production = PartialProduction::from_index(production.clone(), idx);
                        for i in 0..idx {
                            if let Some(sym) = production.rhs.get(i) {
                                partial_production.advance_parsed(sym.clone());
                            }
                        }
                        let pstate = PartialState { ast: partial_ast, final_production: partial_production };
                        states.push(State::Partial(pstate));
                    }
                    
                    // Also create specific missing token states when at EOF and we know what's expected
                    if at_eof && idx < production.rhs.len() {
                        if let Some(expected_symbol) = production.rhs.get(idx) {
                            let mut tmp_children = children.clone();
                            tmp_children.push(PartialASTNode::Missing(expected_symbol.value().to_string()));
                            let partial_ast = self.build_partial_ast(nonterminal, binding.clone(), &tmp_children, start_pos, current_pos);
                            let mut partial_production = PartialProduction::from_index(production.clone(), idx + 1);
                            for i in 0..idx {
                                if let Some(sym) = production.rhs.get(i) {
                                    partial_production.advance_parsed(sym.clone());
                                }
                            }
                            // Mark the missing symbol
                            partial_production.advance_parsed(expected_symbol.clone());
                            let pstate = PartialState { ast: partial_ast, final_production: partial_production };
                            states.push(State::Partial(pstate));
                        }
                    }
                    
                    failed = true; 
                    break;
                }
            }
            if failed { continue; }

            // If we consumed all symbols successfully, we have a complete node.
            if children.len() == production.rhs.len() {
                debug_info!("partial", "Production complete for '{}' with {} children", nonterminal, children.len());
                let span = self.children_span_partial(&children);
                let node = PartialASTNode::Nonterminal(PartialNonTerminal { value: nonterminal.to_string(), span, children: children.clone(), binding: binding.clone() });
                let has_trailing_repetition = production.rhs.iter().rev().any(|symbol| {
                    match symbol.repetition() {
                        Some(RepetitionKind::ZeroOrMore) | Some(RepetitionKind::OneOrMore) => true,
                        _ => false,
                    }
                });
                if has_trailing_repetition {
                    states.push(State::Expandable(node));
                } else {
                    states.push(State::Complete(node));
                }
            }
        }

        // Return the collected states - prefer expandable over complete over partial
        if !states.is_empty() {
            debug_info!("partial", "Returning {} states for '{}'", states.len(), nonterminal);
            Ok((states, max_pos_reached))
        } else {
            debug_info!("partial", "No viable states for '{}'", nonterminal);
            Err(last_err.unwrap_or_else(|| format!("Unable to partially parse '{}'", nonterminal)))
        }
    }
}
