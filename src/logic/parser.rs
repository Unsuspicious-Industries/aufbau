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
    grammar: Grammar,
    pub tokenizer: Tokenizer,
    tokens: Vec<String>,
    pos: usize,
    recursion_tracker: RecursionTracker,
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
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<ASTNode, String> {
        crate::debug_info!("parser", "Starting parse of input: '{}'", input);
        
        // Add a simple tokenization fallback to detect issues
        crate::debug_info!("parser", "About to tokenize with {} special tokens", self.grammar.special_tokens.len());
        for token in &self.grammar.special_tokens {
            crate::debug_info!("parser", "Special token: '{}'", token);
        }
        
        // Use proper tokenizer with STLC special tokens
        let token_ids = match self.tokenizer.tokenize(input.to_string()) {
            Ok(ids) => ids,
            Err(_) => return Err("Tokenization failed".to_string()),
        };
        
        // Convert token IDs back to strings for the parser
        let mut tokens = Vec::new();
        for id in token_ids {
            if let Some(token_str) = self.tokenizer.str(id) {
                tokens.push(token_str);
            } else {
                return Err(format!("Invalid token ID: {}", id));
            }
        }
        
        self.tokens = tokens;
        self.pos = 0;
        self.recursion_tracker.reset();
        
        crate::debug_info!("parser", "Proper tokenization resulted in {} tokens: {:?}", self.tokens.len(), self.tokens);
        
        if self.tokens.is_empty() {
            return Err("Empty input after tokenization".to_string());
        }
        
        return self.parse_with_tokens();
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
                            let span = SourceSpan { start: 0, end: self.pos };
                            
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
                        let span = SourceSpan { start: initial_pos, end: self.pos };
                        
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
        // debug output gated to avoid flooding tests
        crate::debug_trace!("parser", "Trying production: {:?}", production);

        let mut children = Vec::new();
        
        for symbol in &production.rhs {
            if let Some(ref repetition) = symbol.repetition {
                // Handle repetition
                match repetition {
                    RepetitionKind::ZeroOrMore => {
                        // Parse zero or more occurrences
                        loop {
                            let before = self.pos;
                            match self.parse_symbol_no_repetition(symbol) {
                                Ok(child) => {
                                    // Prevent infinite loops on zero-length matches
                                    if self.pos == before {
                                        // Do not push zero-length child; break to avoid infinite loop
                                        break;
                                    }
                                    children.push(child);
                                }
                                Err(_) => {
                                    // Backtrack and break
                                    self.pos = before;
                                    break;
                                }
                            }
                        }
                    }
                    RepetitionKind::OneOrMore => {
                        // Parse one or more occurrences
                        let mut found_at_least_one = false;
                        loop {
                            let before = self.pos;
                            match self.parse_symbol_no_repetition(symbol) {
                                Ok(child) => {
                                    // Prevent infinite loops on zero-length matches
                                    if self.pos == before {
                                        // Stop if no progress is made
                                        break;
                                    }
                                    children.push(child);
                                    found_at_least_one = true;
                                }
                                Err(_) => {
                                    // Backtrack and break
                                    self.pos = before;
                                    break;
                                }
                            }
                        }
                        if !found_at_least_one {
                            return Err(format!("Expected at least one occurrence of '{}'", symbol.value));
                        }
                    }
                    RepetitionKind::ZeroOrOne => {
                        // Parse zero or one occurrence
                        let before = self.pos;
                        match self.parse_symbol_no_repetition(symbol) {
                            Ok(child) => {
                                // Only push if there was progress
                                if self.pos != before {
                                    children.push(child);
                                }
                            }
                            Err(_) => {
                                // Backtrack - zero occurrences is okay
                                self.pos = before;
                            }
                        }
                    }
                }
            } else {
                // No repetition, parse normally
                let before = self.pos;
                match self.parse_symbol(symbol) {
                    Ok(child) => {
                        // If no progress was made, fail to avoid infinite loops on epsilon-like paths
                        if self.pos == before {
                            return Err(format!(
                                "Parser made no progress while matching symbol '{}' in production {:?}",
                                symbol.value, production
                            ));
                        }
                        children.push(child)
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        
        Ok(children)
    }

    fn parse_symbol(&mut self, symbol: &Symbol) -> Result<ASTNode, String> {
        // Delegate to the non-repetition version since repetition is handled in try_production
        self.parse_symbol_no_repetition(symbol)
    }

    fn parse_symbol_no_repetition(&mut self, symbol: &Symbol) -> Result<ASTNode, String> {
        if self.pos >= self.tokens.len() {
            return Err("Unexpected end of input".to_string());
        }

        let token = &self.tokens[self.pos];
        
        // Check if this symbol is a nonterminal (exists in productions)
        let is_nonterminal = self.grammar.productions.contains_key(&symbol.value);
        
        if is_nonterminal {
            // It's a nonterminal, parse recursively and propagate binding from RHS symbol
            let mut node = self.parse_nonterminal(&symbol.value)?;
            // Propagate binding from the symbol reference to the produced nonterminal node
            if symbol.binding.is_some() {
                if let ASTNode::Nonterminal(ref mut nt) = node {
                    nt.binding = symbol.binding.clone();
                }
            }
            Ok(node)
        } else {
            // It's a terminal, check if it matches
            let matches = if symbol.value.starts_with('\'') && symbol.value.ends_with('\'') {
                // Quoted terminal like 'λ' or '+'
                let expected = symbol.value.trim_matches('\'');
                expected == token
            } else if symbol.value.starts_with('/') && symbol.value.ends_with('/') {
                // Regex pattern like /[0-9]+/ - but first check if token is a special token
                if self.grammar.special_tokens.contains(token) {
                    // Token is a special token, so regex shouldn't match it
                    false
                } else {
                    let pattern = symbol.value.trim_matches('/');
                    match regex::Regex::new(pattern) {
                        Ok(re) => re.is_match(token),
                        Err(_) => false,
                    }
                }
            } else {
                // Direct match
                &symbol.value == token
            };
            
            if matches {
                let node = ASTNode::Terminal(Terminal { span: Some(SourceSpan { start: self.pos, end: self.pos + 1 }), value: token.clone(), binding: symbol.binding.clone() });
                self.pos += 1;
                Ok(node)
            } else {
                Err(format!("Expected '{}', found '{}'", symbol.value, token))
            }
        }
    }

    /// Resolve and create a bound rule for a node, if possible
    fn resolve_and_attach_bound_rule(&self, node: &ASTNode, rule_name: &str, rule: &TypingRule) -> Result<Option<BoundTypingRule>, String> {
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
}