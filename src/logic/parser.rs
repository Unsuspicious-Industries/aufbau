use crate::logic::grammar::{Grammar, Nonterminal, Production, Symbol, RepetitionKind};
use crate::logic::ast::{ASTNode, SourceSpan, Terminal, NonTerminal};
use crate::logic::tokenizer::Tokenizer;
use regex;
use crate::logic::bind::{BindingResolver, DefaultBindingResolver, BoundTypingRule};
use crate::logic::typing::TypingRule;

/// A recursive-descent parser that uses a grammar to build an AST.
pub struct Parser {
    grammar: Grammar,
    pub tokenizer: Tokenizer,
    tokens: Vec<String>,
    pos: usize,
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
        }
    }

    pub fn parse(&mut self, input: &str) -> Result<ASTNode, String> {
        // Use proper tokenizer instead of simple whitespace splitting
        let token_ids = self.tokenizer.tokenize(input.to_string())
            .map_err(|_| "Tokenization failed".to_string())?;
        
        // Convert token IDs back to strings
        self.tokens = token_ids.iter()
            .filter_map(|&id| self.tokenizer.str(id))
            .collect();
        
        self.pos = 0;
        
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
        
        // Try all productions for the start nonterminal
        if let Some(productions) = self.grammar.productions.get(&start_nt).cloned() {
            for production in productions {
                self.pos = 0; // Reset position for each attempt
                match self.try_production(&production) {
                    Ok(children) => {
                        // Check if all tokens were consumed
                        if self.pos >= self.tokens.len() {
                            let span = SourceSpan { start: 0, end: self.pos };
                            
                            println!("Parsed production : {:?}", production);

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
                        }
                    }
                    Err(_) => {
                        // Try next production
                        continue;
                    }
                }
            }
        }

        Err(format!("Unable to parse input completely {:?} - {}", self.tokens, self.pos))
    }

    fn parse_nonterminal(&mut self, nt: &Nonterminal) -> Result<ASTNode, String> {
        if let Some(productions) = self.grammar.productions.get(nt).cloned() {
            for production in productions {
                let initial_pos = self.pos;
                match self.try_production(&production) {
                    Ok(children) => {
                        let span = SourceSpan { start: initial_pos, end: self.pos };

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
                        return Ok(node);
                    }
                    Err(_) => {
                        // Backtrack and try next production
                        self.pos = initial_pos;
                        continue;
                    }
                }
            }
        }
        Err(format!("Unable to parse nonterminal: {}", nt))
    }

    fn try_production(&mut self, production: &Production) -> Result<Vec<ASTNode>, String> {

        println!("Trying production: {:?}", production);

        let mut children = Vec::new();
        for symbol in &production.rhs {
            if let Some(ref repetition) = symbol.repetition {
                // Handle repetition
                match repetition {
                    RepetitionKind::ZeroOrMore => {
                        // Parse zero or more occurrences
                        loop {
                            let initial_pos = self.pos;
                            match self.parse_symbol_no_repetition(symbol) {
                                Ok(child) => children.push(child),
                                Err(_) => {
                                    // Backtrack and break
                                    self.pos = initial_pos;
                                    break;
                                }
                            }
                        }
                    }
                    RepetitionKind::OneOrMore => {
                        // Parse one or more occurrences
                        let mut found_at_least_one = false;
                        loop {
                            let initial_pos = self.pos;
                            match self.parse_symbol_no_repetition(symbol) {
                                Ok(child) => {
                                    children.push(child);
                                    found_at_least_one = true;
                                }
                                Err(_) => {
                                    // Backtrack and break
                                    self.pos = initial_pos;
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
                        let initial_pos = self.pos;
                        match self.parse_symbol_no_repetition(symbol) {
                            Ok(child) => children.push(child),
                            Err(_) => {
                                // Backtrack - zero occurrences is okay
                                self.pos = initial_pos;
                            }
                        }
                    }
                }
            } else {
                // No repetition, parse normally
                match self.parse_symbol(symbol) {
                    Ok(child) => children.push(child),
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