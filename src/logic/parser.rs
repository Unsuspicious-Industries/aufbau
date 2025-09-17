use crate::debug_debug;
use crate::logic::grammar::{Grammar, Nonterminal, Production, Symbol, RepetitionKind};
use crate::logic::ast::{ASTNode, SourceSpan, Terminal, NonTerminal};
use crate::logic::tokenizer::Tokenizer;
use crate::logic::recursion::RecursionTracker;
use regex;
use crate::logic::bind::{BindingResolver, DefaultBindingResolver, BoundTypingRule};
use crate::logic::typing::TypingRule;

/// A recursive-descent parser that uses a grammar to build an AST.
pub struct Parser {
    pub grammar: Grammar,
    pub tokenizer: Tokenizer,
    pub tokens: Vec<String>,
    pub pos: usize,
    pub recursion_tracker: RecursionTracker,
    pub token_spans: Vec<(usize, usize)>,
}

impl Parser {
    pub fn new(grammar: Grammar) -> Self {
        // Create tokenizer with special tokens from grammar and common delimiters
        let tokenizer = Tokenizer::new(
            grammar.special_tokens.clone(),
            vec![' ', '\t', '\n', '\r'] // Common whitespace delimiters
        );
        
        Parser {
            grammar,
            tokenizer,
            tokens: vec![],
            pos: 0,
            recursion_tracker: RecursionTracker::new(),
            token_spans: vec![],
        }
    }

    /// Initialize the parser with input, performing tokenization and setting up internal state
    pub fn init(&mut self, input: &str) -> Result<(), String> {
        crate::debug_info!("parser", "Initializing parser with input: '{}'", input);
        
        // Add a simple tokenization fallback to detect issues
        crate::debug_info!("parser", "About to tokenize with {} special tokens", self.grammar.special_tokens.len());
        for token in &self.grammar.special_tokens {
            crate::debug_info!("parser", "Special token: '{}'", token);
        }
        
        // Use tokenizer with spans
        let token_occ = match self.tokenizer.tokenize_with_spans(input) {
            Ok(v) => v,
            Err(_) => return Err("Tokenization failed".to_string()),
        };
        
        // Convert token IDs back to strings for the parser and collect spans
        let mut tokens = Vec::new();
        let mut spans = Vec::new();
        for (id, s, e) in token_occ {
            if let Some(token_str) = self.tokenizer.str(id) {
                tokens.push(token_str);
                spans.push((s, e));
            } else {
                return Err(format!("Invalid token ID: {}", id));
            }
        }
        
        self.tokens = tokens;
        self.token_spans = spans;
        self.pos = 0;
        self.recursion_tracker.reset();
        
        crate::debug_info!("parser", "Proper tokenization resulted in {} tokens: {:?}", self.tokens.len(), self.tokens);
    
        
        Ok(())
    }

    pub fn parse(&mut self, input: &str) -> Result<ASTNode, String> {
        self.init(input)?;
        self.parse_with_tokens()
    }


    fn parse_with_tokens(&mut self) -> Result<ASTNode, String> {
        
        // Handle empty input
        if self.tokens.is_empty() {
            return Err("Empty input".to_string());
        }
        
        // Determine start nonterminal from grammar (must be provided by loader/spec)
        let start_nt = self
            .grammar
            .start_nonterminal()
            .cloned()
            .ok_or_else(|| "No start nonterminal defined in grammar".to_string())?;
        
        crate::debug_info!("parser", "Start nonterminal: {}", start_nt);
        
        // Try all productions for the start nonterminal
        if let Some(productions) = self.grammar.productions.get(&start_nt).cloned() {
            crate::debug_info!("parser", "Found {} productions for start nonterminal", productions.len());
            for (i, production) in productions.iter().enumerate() {
                crate::debug_info!("parser", "Trying production {}: {:?}", i, production);
                self.pos = 0; // Reset position for each attempt
                match self.try_production(&production) {
                    Ok(children) => {
                        crate::debug_info!("parser", "Production {} succeeded, checking if all tokens consumed", i);
                        // Check if all tokens were consumed
                        if self.pos >= self.tokens.len() {
                            crate::debug_info!("parser", "All tokens consumed, building result AST");
                            // Span from first to last token
                            let span = if !self.token_spans.is_empty() { 
                                SourceSpan { start: self.token_spans.first().unwrap().0, end: self.token_spans.last().unwrap().1 }
                            } else { SourceSpan { start: 0, end: self.pos } };
                            
                            // debug output gated
                            crate::debug_debug!("parser", "Parsed production: {:?}", production);

                            // Build node first
                            let mut node = ASTNode::Nonterminal(NonTerminal {
                                value: start_nt.clone(),
                                span: Some(span),
                                children,
                                binding: None,
                                bound_typing_rule: None,
                            });

                            // Attach bound typing rule if production has a rule
                            if let Some(rule_name) = &production.rule {
                                if let Some(rule) = self.grammar.typing_rules.get(rule_name) {
                                    if let Some(bound) = self.resolve_and_attach_bound_rule(&node, rule_name, rule)? {
                                        // Replace with bound rule
                                        if let ASTNode::Nonterminal(ref mut nt) = node {
                                            nt.bound_typing_rule = Some(Box::new(bound));
                                        }
                                    }
                                }
                            }
                            return Ok(node);
                        } else {
                            crate::debug_info!("parser", "Production {} succeeded but not all tokens consumed. pos: {}, tokens.len: {}", i, self.pos, self.tokens.len());
                        }
                    }
                    Err(e) => {
                        crate::debug_info!("parser", "Production {} failed: {}", i, e);
                        // Try next production
                        continue;
                    }
                }
            }
        } else {
            crate::debug_info!("parser", "No productions found for start nonterminal: {}", start_nt);
        }

        crate::debug_info!("parser", "All productions failed");
        Err(format!("Unable to parse input completely {:?} - {}", self.tokens, self.pos))
    }

    fn parse_nonterminal(&mut self, nt: &Nonterminal) -> Result<ASTNode, String> {
        // Check recursion limits using the tracker
        if self.recursion_tracker.exceeds_depth_limit() {
            return Err(format!("Recursion limit exceeded while parsing '{}'", nt));
        }

        // Add debug tracing to see the recursion pattern
        crate::debug_debug!("parser", "parse_nonterminal: {} at pos {} (depth: {})", 
                          nt, self.pos, self.recursion_tracker.depth());
        
        // Print current call stack for debugging
        let stack_trace = self.recursion_tracker.call_stack_trace();
        if !stack_trace.is_empty() {
            crate::debug_debug!("parser", "Current call stack: {}", stack_trace);
        }

        // Check for left-recursion cycle using the tracker
        if self.recursion_tracker.would_create_cycle(nt, self.pos) {
            crate::debug_debug!("parser", "RECURSION DETECTED: {} at pos {} already in call stack", nt, self.pos);
            return Err(format!("Left recursion detected while parsing '{}' at pos {}", nt, self.pos));
        }
        
        // Enter the recursion tracker
        self.recursion_tracker.enter(nt, self.pos)?;

        let result = if let Some(productions) = self.grammar.productions.get(nt).cloned() {
            let mut last_err: Option<String> = None;
            let mut ok_node: Option<ASTNode> = None;
            for production in productions {
                let initial_pos = self.pos;
                match self.try_production(&production) {
                    Ok(children) => {
                        // Map token indices to character spans
                        let span = if initial_pos < self.token_spans.len() {
                            let start_char = self.token_spans[initial_pos].0;
                            let end_char = if self.pos > 0 && self.pos - 1 < self.token_spans.len() { self.token_spans[self.pos - 1].1 } else { start_char };
                            SourceSpan { start: start_char, end: end_char }
                        } else {
                            SourceSpan { start: initial_pos, end: self.pos }
                        };
                        
                        debug_debug!("parser", "Matched production for {}: {:?}", nt, production);

                        // Build node first (binding on RHS symbols propagated elsewhere)
                        let mut node = ASTNode::Nonterminal(NonTerminal {
                            value: nt.clone(),
                            span: Some(span),
                            children,
                            binding: None, // Bindings are now on RHS symbols
                            bound_typing_rule: None,
                        });

                        // Attach bound typing rule if production has a rule
                        if let Some(rule_name) = &production.rule {
                            if let Some(rule) = self.grammar.typing_rules.get(rule_name) {
                                if let Some(bound) = self.resolve_and_attach_bound_rule(&node, rule_name, rule)? {
                                    if let ASTNode::Nonterminal(ref mut nt) = node {
                                        nt.bound_typing_rule = Some(Box::new(bound));
                                    }
                                }
                            }
                        }
                        ok_node = Some(node);
                        break;
                    }
                    Err(e) => {
                        // Backtrack and try next production
                        self.pos = initial_pos;
                        self.recursion_tracker.record_backtrack().ok(); // Ignore errors for now
                        last_err = Some(e);
                        continue;
                    }
                }
            }
            
            ok_node.ok_or_else(|| last_err.unwrap_or_else(|| format!("Unable to parse nonterminal: {}", nt)))
        } else {
            Err(format!("Unable to parse nonterminal: {}", nt))
        };

        // Exit the recursion tracker before returning
        self.recursion_tracker.exit();
        result
    }

    fn try_production(&mut self, production: &Production) -> Result<Vec<ASTNode>, String> {
        crate::debug_trace!("parser", "Trying production: {:?}", production);
        self.parse_sequence(&production.rhs)
    }

    /// Parse a sequence of symbols (supports repetitions). Used for productions and group bodies.
    fn parse_sequence(&mut self, symbols: &[Symbol]) -> Result<Vec<ASTNode>, String> {
        let mut children = Vec::new();
        for symbol in symbols {
            if let Some(rep) = symbol.repetition() {
                match rep {
                    RepetitionKind::ZeroOrMore => {
                        loop {
                            let before = self.pos;
                            match self.parse_symbol_no_repetition(symbol) {
                                Ok(child) => {
                                    if self.pos == before { break; }
                                    children.push(child);
                                }
                                Err(_) => { self.pos = before; break; }
                            }
                        }
                    }
                    RepetitionKind::OneOrMore => {
                        let mut found = false;
                        loop {
                            let before = self.pos;
                            match self.parse_symbol_no_repetition(symbol) {
                                Ok(child) => {
                                    if self.pos == before { break; }
                                    children.push(child);
                                    found = true;
                                }
                                Err(_) => { self.pos = before; break; }
                            }
                        }
                        if !found { return Err(format!("Expected at least one occurrence of '{}'", symbol.value())); }
                    }
                    RepetitionKind::ZeroOrOne => {
                        let before = self.pos;
                        if let Ok(child) = self.parse_symbol_no_repetition(symbol) {
                            if self.pos != before { children.push(child); }
                        } else { self.pos = before; }
                    }
                }
            } else {
                let before = self.pos;
                let child = self.parse_symbol_no_repetition(symbol)?;
                if self.pos == before { return Err(format!("Parser made no progress on '{}'", symbol.value())); }
                children.push(child);
            }
        }
        Ok(children)
    }

    fn parse_symbol(&mut self, symbol: &Symbol) -> Result<ASTNode, String> {
        // Delegate to the non-repetition version since repetition is handled in parse_sequence / try_production
        self.parse_symbol_no_repetition(symbol)
    }

    fn parse_symbol_no_repetition(&mut self, symbol: &Symbol) -> Result<ASTNode, String> {
        // Groups first (single required occurrence of the grouped body; repetition handled externally)
        if symbol.is_group() {
            let start_pos = self.pos;
            let inner_syms = symbol.group_symbols().unwrap();
            let group_children = self.parse_sequence(inner_syms)?; // handles repetitions inside group
            let span = if start_pos < self.token_spans.len() { let start_c = self.token_spans[start_pos].0; let end_c = if self.pos>0 && self.pos-1 < self.token_spans.len(){ self.token_spans[self.pos-1].1 } else { start_c }; SourceSpan{start:start_c,end:end_c} } else { SourceSpan{start:start_pos,end:self.pos} };
            return Ok(ASTNode::Nonterminal(NonTerminal { value: "<group>".into(), span: Some(span), children: group_children, binding: None, bound_typing_rule: None }));
        }
        if self.pos >= self.tokens.len() { return Err("Unexpected end of input".into()); }
        let token = &self.tokens[self.pos];
        let val = symbol.value();
        let is_nonterminal = self.grammar.productions.contains_key(val);
        if is_nonterminal {
            let mut node = self.parse_nonterminal(&val.to_string())?;
            if let Some(b) = symbol.binding() { if let ASTNode::Nonterminal(ref mut nt)=node { nt.binding = Some(b.clone()); } }
            return Ok(node);
        }
        // Terminal matching
        let matches = if val.starts_with('\'') && val.ends_with('\'') {
            val.trim_matches('\'') == token
        } else if val.starts_with('/') && val.ends_with('/') {
            if self.grammar.special_tokens.contains(token) { false } else { regex::Regex::new(val.trim_matches('/')).map(|re| re.is_match(token)).unwrap_or(false) }
        } else { val == token };
        if matches {
            let (s,e)=self.token_spans[self.pos];
            let node = ASTNode::Terminal(Terminal { span: Some(SourceSpan{start:s,end:e}), value: token.clone(), binding: symbol.binding().cloned() });
            self.pos += 1; Ok(node)
        } else { Err(format!("Expected '{}', found '{}'", val, token)) }
    }

    /// Resolve and create a bound rule for a node, if possible
    pub fn resolve_and_attach_bound_rule(&self, node: &ASTNode, rule_name: &str, rule: &TypingRule) -> Result<Option<BoundTypingRule>, String> {
        // Only for nonterminals
        let nt = if let Some(nt) = node.as_nonterminal() { nt } else { return Ok(None) };

        // Construct a temporary NonTerminal (already have it) and resolve
        let resolver = DefaultBindingResolver;
        match resolver.resolve_rule(rule, &nt) {
            Ok(bound) => {
                if !bound.is_well_formed() {
                    return Err(format!("Resolved bound rule '{}' is not well-formed for node {}", rule_name, nt.value));
                }
                Ok(Some(bound))
            }
            Err(e) => Err(format!("Failed to resolve bound typing rule '{}': {}", rule_name, e))
        }
    }

    /// Expose internal grammar immutably for sibling modules (partial parsing etc.).
    pub fn grammar(&self) -> &Grammar { &self.grammar }
    /// Expose internal grammar mutably if future partial parsing wants to augment analysis state.
    pub fn grammar_mut(&mut self) -> &mut Grammar { &mut self.grammar }

    /// Accessor for the parsed tokens (public for partial parser implementation)
    pub fn tokens(&self) -> &[String] { &self.tokens }
    /// Accessor for the token spans (public for partial parser implementation)
    pub fn token_spans(&self) -> &[(usize, usize)] { &self.token_spans }
    /// Current token position (for partial parsing utilities)
    pub fn position(&self) -> usize { self.pos }
    /// Set current token position (used by partial parsing to backtrack independently)
    pub fn set_position(&mut self, new_pos: usize) { self.pos = new_pos; }
    /// Advance by n tokens (safe wrapper)
    pub fn advance_position(&mut self, n: usize) { self.pos += n; }
}