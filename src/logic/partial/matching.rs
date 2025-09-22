use crate::logic::ast::{Terminal};
use crate::logic::grammar::{Production, Symbol, RepetitionKind};
use crate::logic::parser::Parser;
use crate::debug_debug;

use super::{PartialProduction, PartialState, ParseResult, State, PartialASTNode};

impl Parser {
    /// Attempt to match a single symbol (respecting repetition). Returns a list of states or a hard error.
    pub(super) fn partial_match_symbol(&mut self, symbol: &Symbol) -> ParseResult {
        if let Some(rep) = symbol.repetition() {
            match rep {
                RepetitionKind::ZeroOrMore => {
                    let mut items: Vec<PartialASTNode> = Vec::new();
                    let mut states: Vec<State> = Vec::new();
                    
                    loop {
                        let before = self.position();
                        let outcomes = match self.partial_match_symbol_no_rep(symbol) {
                            Ok(o) => o,
                            Err(_) => { break; }
                        };
                        if let Some(node) = outcomes.iter().find_map(|s| {
                            match s {
                                State::Complete(n) | State::Expandable(n) => Some(n.clone()),
                                _ => None
                            }
                        }) {
                            if self.position() == before { break; }
                            items.push(node);
                        } else if outcomes.iter().any(|s| matches!(s, State::Partial(_))) {
                            // Create a partial state representing current repetition content plus expected more
                            let item = if items.is_empty() {
                                PartialASTNode::Group { children: vec![], span: None }
                            } else if items.len() == 1 {
                                items[0].clone()
                            } else {
                                PartialASTNode::Group { children: items.clone(), span: None }
                            };
                            let rep = PartialASTNode::Repetition {
                                symbol: symbol.value().to_string(),
                                kind: RepetitionKind::ZeroOrMore,
                                item: Box::new(item),
                                span: None,
                                binding: symbol.binding().cloned(),
                            };
                            states.push(State::Partial(PartialState {
                                ast: rep,
                                final_production: PartialProduction::new(Production { rule: None, rhs: vec![symbol.clone()] }),
                            }));
                            break;
                        } else { break; }
                        if self.position() == before { break; }
                    }
                    
                    // Always include a completed repetition with current items (ZeroOrMore can always be complete)
                    let item = if items.is_empty() {
                        PartialASTNode::Group { children: vec![], span: None }
                    } else if items.len() == 1 {
                        items.into_iter().next().unwrap()
                    } else {
                        PartialASTNode::Group { children: items, span: None }
                    };
                    // ZeroOrMore repetitions are expandable - they could accept more items
                    states.push(State::Expandable(PartialASTNode::Repetition {
                        symbol: symbol.value().to_string(),
                        kind: RepetitionKind::ZeroOrMore,
                        item: Box::new(item),
                        span: None,
                        binding: symbol.binding().cloned(),
                    }));
                    
                    return Ok(states);
                }
                RepetitionKind::OneOrMore => {
                    let mut items: Vec<PartialASTNode> = Vec::new();
                    match self.partial_match_symbol_no_rep(symbol) {
                        Ok(outcomes) => {
                            if let Some(node) = outcomes.iter().find_map(|s| {
                                if let State::Complete(n) = s { Some(n.clone()) } else { None }
                            }) { items.push(node); }
                            else {
                                // Collect all partials and return them all
                                let partials: Vec<PartialState> = outcomes.into_iter().filter_map(|s| {
                                    if let State::Partial(p) = s { Some(p) } else { None }
                                }).collect();
                                if !partials.is_empty() {
                                    return Ok(partials.into_iter().map(State::Partial).collect());
                                } else {
                                    return Err(format!("Expected at least one '{}'", symbol.value()));
                                }
                            }
                        }
                        Err(e) => return Err(e),
                    }
                    loop {
                        let before = self.position();
                        let outcomes = match self.partial_match_symbol_no_rep(symbol) { Ok(o)=>o, Err(_)=>break };
                        if let Some(node) = outcomes.iter().find_map(|s| { 
                            match s {
                                State::Complete(n) | State::Expandable(n) => Some(n.clone()),
                                _ => None
                            }
                        }) {
                            if self.position() == before { break; }
                            items.push(node);
                        } else if outcomes.iter().any(|s| matches!(s, State::Partial(_))) {
                            let item = if items.len() == 1 { items.into_iter().next().unwrap() } else { PartialASTNode::Group { children: items, span: None } };
                            let rep = PartialASTNode::Repetition { symbol: symbol.value().to_string(), kind: RepetitionKind::OneOrMore, item: Box::new(item), span: None, binding: symbol.binding().cloned() };
                            return Ok(vec![State::Partial(PartialState { ast: rep, final_production: PartialProduction::new(Production { rule: None, rhs: vec![symbol.clone()] }) })]);
                        } else { break; }
                        if self.position() == before { break; }
                    }
                    let item = if items.len() == 1 { items.into_iter().next().unwrap() } else { PartialASTNode::Group { children: items, span: None } };
                    // OneOrMore repetitions are also expandable - they could accept more items
                    return Ok(vec![State::Expandable(PartialASTNode::Repetition {
                        symbol: symbol.value().to_string(),
                        kind: RepetitionKind::OneOrMore,
                        item: Box::new(item),
                        span: None,
                        binding: symbol.binding().cloned(),
                    })]);
                }
                RepetitionKind::ZeroOrOne => {
                    let outcomes = match self.partial_match_symbol_no_rep(symbol) { Ok(o)=>o, Err(_)=>{
                        return Ok(vec![State::Complete(PartialASTNode::Repetition {
                            symbol: symbol.value().to_string(),
                            kind: RepetitionKind::ZeroOrOne,
                            item: Box::new(PartialASTNode::Group { children: vec![], span: None }),
                            span: None,
                            binding: symbol.binding().cloned(),
                        })]); } };
                    if let Some(node) = outcomes.iter().find_map(|s| { 
                        match s {
                            State::Complete(n) | State::Expandable(n) => Some(n.clone()),
                            _ => None
                        }
                    }) {
                        let span = node.span().cloned();
                        return Ok(vec![State::Complete(PartialASTNode::Repetition {
                            symbol: symbol.value().to_string(),
                            kind: RepetitionKind::ZeroOrOne,
                            item: Box::new(node),
                            span,
                            binding: symbol.binding().cloned(),
                        })]);
                    }
                    let partials: Vec<PartialState> = outcomes.into_iter().filter_map(|s| if let State::Partial(p)=s { Some(p) } else { None }).collect();
                    if !partials.is_empty() { return Ok(partials.into_iter().map(State::Partial).collect()); }
                    return Ok(vec![State::Complete(PartialASTNode::Repetition {
                        symbol: symbol.value().to_string(),
                        kind: RepetitionKind::ZeroOrOne,
                        item: Box::new(PartialASTNode::Group { children: vec![], span: None }),
                        span: None,
                        binding: symbol.binding().cloned(),
                    })]);
                }
            }
        }
        self.partial_match_symbol_no_rep(symbol)
    }

    /// Match a symbol ignoring its repetition wrapper (handled by caller). Returns a list of states or a hard error.
    pub(super) fn partial_match_symbol_no_rep(&mut self, symbol: &Symbol) -> ParseResult {
        // Group symbol: parse its internal sequence inline
        if symbol.is_group() {
            let inner = symbol.group_symbols().unwrap();
            let mut children: Vec<PartialASTNode> = Vec::new();
            for (idx, inner_sym) in inner.iter().enumerate() {
                let outcomes = self.partial_match_symbol(inner_sym)?;
                if let Some(n) = outcomes.iter().find_map(|s| { 
                    match s {
                        State::Complete(n) | State::Expandable(n) => Some(n.clone()),
                        _ => None
                    }
                }) {
                    children.push(n);
                } else if let Some(p) = outcomes.into_iter().find_map(|s| { if let State::Partial(p)=s { Some(p) } else { None }}) {
                    children.push(p.ast.clone());
                    let group_node = PartialASTNode::Group { children: children.clone(), span: None };
                    let mut partial_production = PartialProduction::new(Production { rule: None, rhs: inner.to_vec() });
                    // Mark symbols up to idx as parsed, current as partial
                    for i in 0..idx {
                        if let Some(sym) = inner.get(i) {
                            partial_production.advance_parsed(sym.clone());
                        }
                    }
                    if let Some(sym) = inner.get(idx) {
                        partial_production.set_partial(sym.clone());
                    }
                    let pstate = PartialState {
                        ast: group_node,
                        final_production: partial_production,
                    };
                    return Ok(vec![State::Partial(pstate)]);
                } else {
                    let group_node = PartialASTNode::Group { children: children.clone(), span: None };
                    let mut partial_production = PartialProduction::new(Production { rule: None, rhs: inner.to_vec() });
                    // Mark symbols up to idx as parsed
                    for i in 0..idx {
                        if let Some(sym) = inner.get(i) {
                            partial_production.advance_parsed(sym.clone());
                        }
                    }
                    let pstate = PartialState { ast: group_node, final_production: partial_production };
                    return Ok(vec![State::Partial(pstate)]);
                }
            }
            return Ok(vec![State::Complete(PartialASTNode::Group { children, span: None })]);
        }

        let sym_val = symbol.value();

        // EOF => incomplete (need more tokens)
        if self.position() >= self.tokens.len() {
            return Ok(vec![State::Partial(PartialState {
                ast: PartialASTNode::Missing(sym_val.to_string()),
                final_production: PartialProduction::new(Production { rule: None, rhs: vec![symbol.clone()] }),
            })]);
        }

        let token = &self.tokens[self.position()];
        debug_debug!("partial", "Matching terminal '{}' against token '{}'", sym_val, token);
        let is_nt = self.grammar.productions.contains_key(&sym_val);
        if is_nt {
            // Recurse: collect states and bubble them up
            let outcomes = self.partial_recursive(&sym_val, symbol.binding().cloned())?;
            Ok(outcomes)
        } else {
            // Terminal matching
            let matches = self.match_terminal(&sym_val, token);
            if matches {
                let span = self.token_span(self.position());
                let node = PartialASTNode::Terminal(Terminal { value: token.clone(), span, binding: symbol.binding().cloned() });
                self.advance_position(1);
                Ok(vec![State::Complete(node)])
            } else {
                // Non-EOF mismatch: do not emit a state here; let caller decide branching.
                Err(format!("Unexpected token '{}' while matching '{}'", token, sym_val))
            }
        }
    }

    /// Position-aware version of partial_match_symbol that doesn't modify global parser state
    pub(super) fn partial_match_symbol_at_pos(&mut self, symbol: &Symbol, pos: usize) -> Result<(Vec<State>, usize), String> {
        if let Some(_rep) = symbol.repetition() {
            // For now, handle repetitions by temporarily setting position
            let original_pos = self.position();
            self.set_position(pos);
            let result = self.partial_match_symbol(symbol);
            let new_pos = self.position();
            self.set_position(original_pos);
            match result {
                Ok(states) => Ok((states, new_pos)),
                Err(e) => Err(e),
            }
        } else {
            self.partial_match_symbol_no_rep_at_pos(symbol, pos)
        }
    }

    /// Position-aware version of partial_match_symbol_no_rep
    fn partial_match_symbol_no_rep_at_pos(&mut self, symbol: &Symbol, pos: usize) -> Result<(Vec<State>, usize), String> {
        if let Some(_group) = symbol.group_symbols() {
            // Handle grouped symbols by temporarily setting position
            let original_pos = self.position();
            self.set_position(pos);
            let result = self.partial_match_symbol_no_rep(symbol);
            let new_pos = self.position();
            self.set_position(original_pos);
            match result {
                Ok(states) => Ok((states, new_pos)),
                Err(e) => Err(e),
            }
        } else {
            // Handle terminal/nonterminal matching
            let sym_val = symbol.value();
            
            // EOF => incomplete (need more tokens)
            if pos >= self.tokens.len() {
                return Ok((vec![State::Partial(PartialState {
                    ast: PartialASTNode::Missing(sym_val.to_string()),
                    final_production: PartialProduction::new(Production { rule: None, rhs: vec![symbol.clone()] }),
                })], pos));
            }

            let token = &self.tokens[pos];
            debug_debug!("partial", "Matching terminal '{}' against token '{}'", sym_val, token);
            let is_nt = self.grammar.productions.contains_key(&sym_val);
            if is_nt {
                // Recurse: collect states and bubble them up using position-aware parsing
                let (outcomes, new_pos) = self.partial_recursive_at_pos(&sym_val, symbol.binding().cloned(), pos)?;
                Ok((outcomes, new_pos))
            } else {
                // Terminal matching
                let matches = self.match_terminal(&sym_val, token);
                if matches {
                    let span = self.token_span(pos);
                    let node = PartialASTNode::Terminal(Terminal { value: token.clone(), span, binding: symbol.binding().cloned() });
                    Ok((vec![State::Complete(node)], pos + 1))
                } else {
                    // Non-EOF mismatch: propagate error; upstream will decide whether to emit partial
                    Err(format!("Unexpected token '{}' while matching '{}'", token, sym_val))
                }
            }
        }
    }
}
