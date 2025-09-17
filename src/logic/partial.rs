use crate::logic::ast::{ASTNode, NonTerminal, Terminal, SourceSpan};
use crate::logic::grammar::{Production, Symbol, RepetitionKind};
use crate::logic::parser::Parser;
use crate::{debug_info, debug_debug};

/// Tracks progress within a production (the next symbol index expected)
#[derive(Debug, Clone)]
pub struct PartialProduction {
    pub production: Production,
    pub current_index: usize, // index of the next symbol to process
}

/// Captures a partial parse state of a nonterminal node.
/// The `ast` contains the children parsed so far; the production indicates
/// which production was chosen and `current_index` where we stopped.
#[derive(Debug, Clone)]
pub struct PartialState {
    pub ast: ASTNode, // partial AST (children parsed so far)
    pub final_production: PartialProduction,
}

/// Internal state returned while incrementally parsing a symbol
#[derive(Debug)]
pub enum State {
    Partial(PartialState),
    Complete(ASTNode),
}

/// Result of attempting a (partial) parse
#[derive(Debug)]
pub enum PartialOutcome {
    Complete { node: ASTNode },
    Incomplete { states: Vec<PartialState> },
    Error(String),
}

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
        Ok(self.partial_recursive(start_nt.as_str(), None))
    }

    /// Recursively attempt to (partially) parse a nonterminal.
    /// Returns:
    /// - Complete{node} when an entire production matched.
    /// - Incomplete{states} when input ended (or a child was incomplete) leaving parse progress.
    /// - Error when all productions failed without any recoverable partial state.
    fn partial_recursive(&mut self, nonterminal: &str, binding: Option<String>) -> PartialOutcome {
        debug_debug!("partial", "Entering partial_recursive for '{}' at pos {}", nonterminal, self.position());
        let productions = match self.grammar.productions.get(nonterminal).cloned() {
            Some(p) => p,
            None => return PartialOutcome::Error(format!("No productions found for symbol '{}'", nonterminal)),
        };

        let original_pos = self.position();
        let mut incomplete_states: Vec<PartialState> = Vec::new();
        let mut last_err: Option<String> = None;

        for production in productions.iter() {
            debug_debug!("partial", "Trying production: {:?}", production);
            let mut children: Vec<ASTNode> = Vec::new();
            let start_pos = self.position(); // position where this production attempt starts
            let mut failed = false;
            for (idx, symbol) in production.rhs.iter().enumerate() {
                debug_debug!("partial", "Matching symbol {} (idx {}): {:?}", symbol.value(), idx, symbol);
                match self.partial_match_symbol(symbol) {
                    State::Complete(node) => {
                        debug_debug!("partial", "Symbol {} completed: {}", symbol.value(), node.value());
                        children.push(node);
                    }
                    State::Partial(_child_partial) => {
                        debug_debug!("partial", "Symbol {} partial, creating incomplete state", symbol.value());
                        // DO NOT add the partial child to the AST - that would pollute it with incomplete data
                        // The partial AST should only contain successfully parsed children
                        let partial_ast = self.build_partial_ast(nonterminal, binding.clone(), &children, start_pos, self.position());
                        let pstate = PartialState {
                            ast: partial_ast,
                            final_production: PartialProduction { production: production.clone(), current_index: idx },
                        };
                        incomplete_states.push(pstate);
                        failed = true; // Stop exploring this production further
                        break;
                    }
                }
            }
            if failed { // Move to next production, but restore position for independence
                self.set_position(original_pos); // Do not consume tokens globally for incomplete alternatives
                continue;
            }

            // If we consumed all symbols successfully, we have a complete node.
            if children.len() == production.rhs.len() {
                debug_info!("partial", "Production complete for '{}' with {} children", nonterminal, children.len());
                // Compute span from first to last child (if available)
                let span = self.children_span(&children);
                let mut node = ASTNode::Nonterminal(NonTerminal {
                    value: nonterminal.to_string(),
                    span,
                    children,
                    binding: binding.clone(),
                    bound_typing_rule: None, // Only attach when fully bound with rule resolution (optional for partial)
                });
                // If production has a typing rule, attempt to resolve (only if full parse of this nonterminal)
                if let Some(rule_name) = &production.rule {
                    if let Some(rule) = self.grammar.typing_rules.get(rule_name) {
                        if let Ok(Some(bound)) = self.resolve_and_attach_bound_rule(&node, rule_name, rule) {
                            if let ASTNode::Nonterminal(ref mut nt) = node {
                                nt.bound_typing_rule = Some(Box::new(bound));
                            }
                        }
                    }
                }
                return PartialOutcome::Complete { node };
            } else {
                // Should not happen; defensive: treat as error
                last_err = Some(format!("Internal mismatch: children count {} < rhs {}", children.len(), production.rhs.len()));
                self.set_position(original_pos);
            }
        }

        if !incomplete_states.is_empty() {
            debug_info!("partial", "Returning {} incomplete states for '{}'", incomplete_states.len(), nonterminal);
            // Deduplicate by (production pointer + index + child count) to avoid noise
            // (Simple O(n^2) filter acceptable for small numbers of alternatives.)
            let mut filtered: Vec<PartialState> = Vec::new();
            for st in incomplete_states.into_iter() {
                let sig = (st.final_production.current_index, st.final_production.production.rhs.len(), st.ast.children().map(|c| c.len()).unwrap_or(0));
                if !filtered.iter().any(|x| (x.final_production.current_index, x.final_production.production.rhs.len(), x.ast.children().map(|c| c.len()).unwrap_or(0)) == sig) {
                    filtered.push(st);
                }
            }
            return PartialOutcome::Incomplete { states: filtered };
        }

        debug_info!("partial", "Error parsing '{}'", nonterminal);
        PartialOutcome::Error(last_err.unwrap_or_else(|| format!("Unable to partially parse '{}'", nonterminal)))
    }

    /// Attempt to match a single symbol (respecting repetition). Returns either a complete AST node,
    /// or a Partial state if more tokens are required inside the symbol.
    fn partial_match_symbol(&mut self, symbol: &Symbol) -> State {
        if let Some(rep) = symbol.repetition() {
            match rep {
                RepetitionKind::ZeroOrMore => {
                    let mut group_children = Vec::new();
                    loop {
                        let before = self.position();
                        match self.partial_match_symbol_no_rep(symbol) {
                            State::Complete(node) => {
                                if self.position() == before { break; }
                                group_children.push(node);
                            }
                            State::Partial(_) => { // Incomplete inside optional repetition does not block higher parse
                                break; // treat as finished (cannot decide yet)
                            }
                        }
                        if self.position() == before { break; }
                    }
                    // Represent the repetition as a synthetic group nonterminal for uniformity
                    return State::Complete(ASTNode::Nonterminal(NonTerminal {
                        value: format!("{}*", symbol.value()),
                        span: self.children_span(&group_children),
                        children: group_children,
                        binding: symbol.binding().cloned(),
                        bound_typing_rule: None,
                    }));
                }
                RepetitionKind::OneOrMore => {
                    let mut group_children = Vec::new();
                    // Need at least one
                    match self.partial_match_symbol_no_rep(symbol) {
                        State::Complete(node) => group_children.push(node),
                        State::Partial(p) => return State::Partial(p),
                    }
                    loop {
                        let before = self.position();
                        match self.partial_match_symbol_no_rep(symbol) {
                            State::Complete(node) => {
                                if self.position() == before { break; }
                                group_children.push(node);
                            }
                            State::Partial(_) => { break; }
                        }
                        if self.position() == before { break; }
                    }
                    return State::Complete(ASTNode::Nonterminal(NonTerminal {
                        value: format!("{}+", symbol.value()),
                        span: self.children_span(&group_children),
                        children: group_children,
                        binding: symbol.binding().cloned(),
                        bound_typing_rule: None,
                    }));
                }
                RepetitionKind::ZeroOrOne => {
                    let before = self.position();
                    match self.partial_match_symbol_no_rep(symbol) {
                        State::Complete(node) => {
                            if self.position() == before { // No progress -> treat as absent
                                return State::Complete(ASTNode::Nonterminal(NonTerminal { value: format!("{}?", symbol.value()), span: None, children: vec![], binding: symbol.binding().cloned(), bound_typing_rule: None }));
                            }
                            return State::Complete(ASTNode::Nonterminal(NonTerminal {
                                value: format!("{}?", symbol.value()),
                                span: node.span().cloned(),
                                children: vec![node],
                                binding: symbol.binding().cloned(),
                                bound_typing_rule: None,
                            }));
                        }
                        State::Partial(_p) => {
                            // Optional: incomplete sub-symbol means we simply omit it (cannot decide yet)
                            return State::Complete(ASTNode::Nonterminal(NonTerminal { value: format!("{}?", symbol.value()), span: None, children: vec![], binding: symbol.binding().cloned(), bound_typing_rule: None }));
                        }
                    }
                }
            }
        }
        self.partial_match_symbol_no_rep(symbol)
    }

    /// Match a symbol ignoring its repetition wrapper (handled by caller).
    fn partial_match_symbol_no_rep(&mut self, symbol: &Symbol) -> State {
        // Group symbol: parse its internal sequence inline
        if symbol.is_group() {
            let inner = symbol.group_symbols().unwrap();
            let start = self.position();
            let mut children: Vec<ASTNode> = Vec::new();
            for (idx, inner_sym) in inner.iter().enumerate() {
                match self.partial_match_symbol(inner_sym) {
                    State::Complete(n) => children.push(n),
                    State::Partial(_p) => {
                        // Build partial group node
                        let group_node = ASTNode::Nonterminal(NonTerminal { value: "<group>".into(), span: self.children_span(&children), children: children.clone(), binding: None, bound_typing_rule: None });
                        let pstate = PartialState { ast: group_node, final_production: PartialProduction { production: Production { rule: None, rhs: inner.to_vec() }, current_index: idx } };
                        return State::Partial(pstate);
                    }
                }
            }
            let span = self.span_from(start, self.position());
            return State::Complete(ASTNode::Nonterminal(NonTerminal { value: "<group>".into(), span, children, binding: None, bound_typing_rule: None }));
        }

        // EOF => incomplete (need more tokens)
        if self.position() >= self.tokens.len() {
            debug_debug!("partial", "EOF reached while parsing symbol: {}", symbol.value());
            let pstate = self.empty_partial_state("<token>");
            return State::Partial(pstate);
        }

        let token = &self.tokens[self.position()];
        let sym_val = symbol.value();
        debug_debug!("partial", "Matching terminal '{}' against token '{}'", sym_val, token);
        let is_nt = self.grammar.productions.contains_key(sym_val);
        if is_nt {
            // Recurse
            let outcome = self.partial_recursive(sym_val, symbol.binding().cloned());
            match outcome {
                PartialOutcome::Complete { node } => State::Complete(node),
                PartialOutcome::Incomplete { states } => {
                    // Use first state as representative
                    if let Some(st) = states.into_iter().next() {
                        State::Partial(st)
                    } else {
                        State::Partial(self.empty_partial_state(sym_val))
                    }
                }
                PartialOutcome::Error(_) => {
                    // Treat as mismatch -> propagate by creating partial state? Better: mismatch => return Partial with no progress
                    State::Partial(self.empty_partial_state(sym_val))
                }
            }
        } else {
            // Terminal matching (mirror core parser logic, simplified)
            let matches = self.match_terminal(sym_val, token);
            if matches {
                debug_debug!("partial", "Terminal '{}' matched token '{}'", sym_val, token);
                let span = self.token_span(self.position());
                let node = ASTNode::Terminal(Terminal { value: token.clone(), span, binding: symbol.binding().cloned() });
                self.advance_position(1);
                State::Complete(node)
            } else {
                debug_debug!("partial", "Terminal '{}' did not match token '{}'", sym_val, token);
                // Mismatch -> produce partial state representing expectation failure (NOT incomplete due to EOF)
                State::Partial(self.empty_partial_state(sym_val))
            }
        }
    }

    // --- helpers ---

    fn build_partial_ast(&self, value: &str, binding: Option<String>, children: &[ASTNode], start_pos: usize, end_pos: usize) -> ASTNode {
        let span = if start_pos < end_pos { self.span_from(start_pos, end_pos) } else { None };
        ASTNode::Nonterminal(NonTerminal {
            value: value.to_string(),
            span,
            children: children.to_vec(),
            binding,
            bound_typing_rule: None,
        })
    }

    fn empty_partial_state(&self, expected: &str) -> PartialState {
        let ast = ASTNode::Nonterminal(NonTerminal { value: expected.to_string(), span: None, children: vec![], binding: None, bound_typing_rule: None });
        PartialState { ast, final_production: PartialProduction { production: Production { rule: None, rhs: vec![] }, current_index: 0 } }
    }

    fn match_terminal(&self, sym_val: &str, token: &str) -> bool {
        if sym_val.starts_with('"') && sym_val.ends_with('"') { // support quoted? (fallback)
            sym_val.trim_matches('"') == token
        } else if sym_val.starts_with('\'') && sym_val.ends_with('\'') {
            sym_val.trim_matches('\'') == token
        } else if sym_val.starts_with('/') && sym_val.ends_with('/') {
            if self.grammar.special_tokens.contains(&token.to_string()) { false } else { regex::Regex::new(sym_val.trim_matches('/')).map(|re| re.is_match(token)).unwrap_or(false) }
        } else {
            sym_val == token
        }
    }

    fn token_span(&self, idx: usize) -> Option<SourceSpan> {
        self.token_spans.get(idx).map(|(s,e)| SourceSpan { start: *s, end: *e })
    }

    fn span_from(&self, start_token: usize, end_token: usize) -> Option<SourceSpan> {
        if start_token >= self.token_spans.len() || end_token == 0 { return None; }
        let start = self.token_spans.get(start_token).map(|p| p.0)?;
        let end = if end_token - 1 < self.token_spans.len() { self.token_spans[end_token - 1].1 } else { start };
        Some(SourceSpan { start, end })
    }

    fn children_span(&self, children: &[ASTNode]) -> Option<SourceSpan> {
        if children.is_empty() { return None; }
        let first = children.first()?.span()?.start;
        let last = children.last()?.span()?.end;
        Some(SourceSpan { start: first, end: last })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::logic::grammar::Grammar;
    use crate::logic::parser::Parser;

    fn parser(spec: &str) -> Parser { Parser::new(Grammar::load(spec).expect("grammar load")) }

    #[test]
    fn partial_empty_input_incomplete() {
        let spec = r#"
        Start ::= 'a' 'b'
        "#;
        let mut p = parser(spec);
        let out = p.partial("").expect("init ok");
        match out { PartialOutcome::Incomplete { states } => assert!(!states.is_empty()), _ => panic!("expected incomplete") }
    }

    #[test]
    fn partial_one_token_progress() {
        let spec = r#"Start ::= 'a' 'b'"#;
        let mut p = parser(spec);
        let out = p.partial("a").expect("init ok");
        match out {
            PartialOutcome::Incomplete { states } => {
                let nt = states[0].ast.as_nonterminal().expect("nt");
                assert_eq!(nt.value, "Start");
                assert_eq!(nt.children.len(), 1);
                assert_eq!(nt.children[0].value(), "a");
            }
            other => panic!("unexpected: {:?}", other)
        }
    }

    #[test]
    fn partial_complete_two_tokens() {
        let spec = r#"Start ::= 'a' 'b'"#;
        let mut p = parser(spec);
        let out = p.partial("a b").expect("init ok");
        match out { PartialOutcome::Complete { node } => {
            let nt = node.as_nonterminal().unwrap();
            assert_eq!(nt.children.len(), 2);
            assert_eq!(nt.children[0].value(), "a");
            assert_eq!(nt.children[1].value(), "b");
        }, other => panic!("unexpected: {:?}", other)}
    }

    #[test]
    fn partial_repetition_pending_trailer() {
        let spec = r#"
        Item ::= 'x'
        List ::= Item* 'end'
        "#;
        let mut p = parser(spec);
        let out = p.partial("x x").expect("init ok");
        match out { PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty());
            let nt = states[0].ast.as_nonterminal().unwrap();
            assert_eq!(nt.value, "List");
            // The structure should be List -> Item* -> Item nodes with "x" terminals
            // Check that we have the Item* repetition node containing parsed items
            assert_eq!(nt.children.len(), 1);
            let items_node = nt.children[0].as_nonterminal().unwrap();
            assert_eq!(items_node.value, "Item*");
            assert!(items_node.children.len() > 0); // Should have parsed some items
            // Check that the items contain "x" values
            assert!(items_node.children.iter().any(|item| {
                if let Some(item_nt) = item.as_nonterminal() {
                    item_nt.children.iter().any(|child| child.value() == "x")
                } else { false }
            }));
        }, 
            other => panic!("expected incomplete, got: {:?}", other)
        }
    }

    #[test]
    fn partial_nested_nonterminal_midway() {
        let spec = r#"
        Start ::= A 'z'
        A ::= 'x' 'y'
        "#;
        let mut p = parser(spec);
        let out = p.partial("x").expect("init ok");
        match out { PartialOutcome::Incomplete { states } => {
            assert!(!states.is_empty());
            assert!(states.iter().any(|s| {
                if let Some(nt)=s.ast.as_nonterminal(){ nt.value=="Start" || nt.value=="A" } else { false }
            }));
        }, _ => panic!("expected incomplete") }
    }

    #[test]
    fn partial_mismatch_reports_incomplete() {
        let spec = r#"Start ::= 'a' 'b'"#;
        let mut p = parser(spec);
        let out = p.partial("c").expect("init ok");
        match out { PartialOutcome::Incomplete { states } => assert!(!states.is_empty()), _ => panic!("expected incomplete on mismatch") }
    }

    #[test]
    fn debug_partial_one_token() {
        use crate::{debug_info, set_debug_level, DebugLevel};
        set_debug_level(DebugLevel::Debug);
        
        let spec = r#"Start ::= 'a' 'b'"#;
        let mut p = parser(spec);
        debug_info!("test", "Starting debug test for partial parsing");
        let out = p.partial("a").expect("init ok");
        match out {
            PartialOutcome::Incomplete { states } => {
                debug_info!("test", "States count: {}", states.len());
                for (i, state) in states.iter().enumerate() {
                    debug_info!("test", "State {}: {:?}", i, state.ast);
                    if let Some(nt) = state.ast.as_nonterminal() {
                        debug_info!("test", "  NT value: {}", nt.value);
                        debug_info!("test", "  Children count: {}", nt.children.len());
                        for (j, child) in nt.children.iter().enumerate() {
                            debug_info!("test", "    Child {}: {:?}", j, child.value());
                        }
                    }
                }
            }
            other => debug_info!("test", "Unexpected outcome: {:?}", other)
        }
    }
}

