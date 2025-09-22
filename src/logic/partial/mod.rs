//! Partial parsing module for incremental parsing support.
//! 
//! This module provides functionality for parsing incomplete input and
//! returning partial parse states that can be used for autocompletion,
//! syntax highlighting, and error recovery.

pub mod parse;
pub mod matching;
pub mod utils;
pub mod display;

#[cfg(test)]
pub mod tests;

use crate::logic::ast::{ASTNode, Terminal, SourceSpan};
use crate::logic::grammar::{Production, RepetitionKind, Symbol};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

/// Tracks progress within a production using a structured approach.
/// Instead of a simple index, this provides detailed parsing state.
#[derive(Debug, Clone)]
pub struct PartialProduction {
    pub production: Production,
    /// Symbols that have been fully parsed and completed
    pub parsed: Vec<Symbol>,
    /// Symbols that are partially parsed (contain incomplete children)
    pub partial: Vec<Symbol>,
    /// Symbols that haven't been started yet
    pub remaining: Vec<Symbol>,
}

impl PartialProduction {
    /// Create a new PartialProduction from a production and current index (for backward compatibility)
    pub fn from_index(production: Production, current_index: usize) -> Self {
        let total_symbols = production.rhs.clone();
        let parsed = if current_index > 0 { total_symbols[0..current_index.min(total_symbols.len())].to_vec() } else { vec![] };
        let remaining = if current_index < total_symbols.len() { total_symbols[current_index..].to_vec() } else { vec![] };
        
        Self {
            production,
            parsed,
            partial: vec![],
            remaining,
        }
    }

    /// Create a new PartialProduction at the beginning
    pub fn new(production: Production) -> Self {
        let remaining = production.rhs.clone();
        Self {
            production,
            parsed: vec![],
            partial: vec![],
            remaining,
        }
    }

    /// Get the equivalent of current_index for backward compatibility
    pub fn current_index(&self) -> usize {
        self.parsed.len()
    }

    /// Get the next symbol to process (if any)
    pub fn next_symbol(&self) -> Option<&Symbol> {
        self.remaining.first()
    }

    /// Mark a symbol as fully parsed and move to next
    pub fn advance_parsed(&mut self, symbol: Symbol) {
        if let Some(pos) = self.remaining.iter().position(|s| s == &symbol) {
            if pos == 0 {
                self.remaining.remove(0);
                self.parsed.push(symbol);
            }
        }
    }

    /// Mark a symbol as partial
    pub fn set_partial(&mut self, symbol: Symbol) {
        if let Some(pos) = self.remaining.iter().position(|s| s == &symbol) {
            if pos == 0 {
                self.remaining.remove(0);
                self.partial.push(symbol);
            }
        }
    }

    /// Get total number of symbols in production
    pub fn total_symbols(&self) -> usize {
        self.production.rhs.len()
    }

    /// Check if production is complete
    pub fn is_complete(&self) -> bool {
        self.remaining.is_empty() && self.partial.is_empty()
    }

    /// Hash all significant components of the production state
    pub fn hash(&self, hasher: &mut DefaultHasher) {
        // Hash the production rule itself
        if let Some(ref rule) = self.production.rule {
            rule.hash(hasher);
        }
        
        // Hash the RHS symbols
        for symbol in &self.production.rhs {
            symbol.value().hash(hasher);
            // Include symbol type and detailed structure for precise matching
            match symbol {
                Symbol::Simple { value, binding, repetition } => {
                    "SIMPLE".hash(hasher);
                    value.hash(hasher);
                    binding.hash(hasher);
                    repetition.hash(hasher);
                },
                Symbol::Group { symbols, repetition } => {
                    "GROUP".hash(hasher);
                    symbols.len().hash(hasher);
                    for s in symbols {
                        s.value().hash(hasher);
                    }
                    repetition.hash(hasher);
                }
            }
        }
        
        // Hash the parsed symbols
        self.parsed.len().hash(hasher);
        for symbol in &self.parsed {
            symbol.value().hash(hasher);
        }
        
        // Hash the partial symbols
        self.partial.len().hash(hasher);
        for symbol in &self.partial {
            symbol.value().hash(hasher);
        }
        
        // Hash the remaining symbols
        self.remaining.len().hash(hasher);
        for symbol in &self.remaining {
            symbol.value().hash(hasher);
        }
    }
}

// ---- Partial AST types ----

/// A partial nonterminal node with children that may themselves be partial.
#[derive(Debug, Clone)]
pub struct PartialNonTerminal {
    pub value: String,
    pub span: Option<SourceSpan>,
    pub children: Vec<PartialASTNode>,
    pub binding: Option<String>,
}

/// Represents an AST-like node during partial parsing, with explicit support for
/// repetitions and placeholders. This avoids polluting the final AST with grammar
/// artifacts like "Item*" while preserving precise progress information.
#[derive(Debug, Clone)]
pub enum PartialASTNode {
    Terminal(Terminal),
    Nonterminal(PartialNonTerminal),
    /// A repetition wrapper collecting zero-or-more or one-or-more items of a symbol.
    /// This will be flattened into the parent when converted to a concrete AST.
    Repetition {
        symbol: String,
        kind: RepetitionKind,
        item: Box<PartialASTNode>,
        span: Option<SourceSpan>,
        binding: Option<String>,
    },
    /// A parsed group "( ... )". Present only in partial trees; can be flattened if desired.
    Group { children: Vec<PartialASTNode>, span: Option<SourceSpan> },
    /// Represents a missing symbol that was expected but not found (typically due to EOF).
    /// The string indicates what symbol/token was expected at this position.
    Missing(String),
}

impl PartialASTNode {
    /// Human-friendly value for debugging (non-unique)
    pub fn value(&self) -> String {
        match self {
            PartialASTNode::Terminal(t) => t.value.clone(),
            PartialASTNode::Nonterminal(nt) => nt.value.clone(),
            PartialASTNode::Repetition { symbol, kind, .. } => match kind {
                RepetitionKind::ZeroOrMore => symbol.clone(),
                RepetitionKind::OneOrMore => symbol.clone(),
                RepetitionKind::ZeroOrOne => symbol.clone(), // represented by optional node
            },
            PartialASTNode::Group { children, .. } => {
                let ch = children.iter().map(|c| c.value()).collect::<Vec<_>>().join(",");
                format!("({})", ch)
            },
            PartialASTNode::Missing(expected) => format!("<missing {}>", expected),
        }
    }

    pub fn children(&self) -> Option<&[PartialASTNode]> {
        match self {
            PartialASTNode::Terminal(_) => None,
            PartialASTNode::Nonterminal(nt) => Some(&nt.children),
            PartialASTNode::Repetition { item, .. } => {
                // If the item is a Group, return its children, otherwise return a slice with the single item
                match item.as_ref() {
                    PartialASTNode::Group { children, .. } => Some(children),
                    _ => None, // Single item repetitions don't expose children through this method
                }
            }
            PartialASTNode::Group { children, .. } => Some(children),
            PartialASTNode::Missing(_) => None,
        }
    }

    pub fn as_nonterminal(&self) -> Option<&PartialNonTerminal> {
        match self { PartialASTNode::Nonterminal(nt) => Some(nt), _ => None }
    }
    pub fn as_terminal(&self) -> Option<&Terminal> {
        match self { PartialASTNode::Terminal(t) => Some(t), _ => None }
    }

    pub fn span(&self) -> Option<&SourceSpan> {
        match self {
            PartialASTNode::Terminal(t) => t.span.as_ref(),
            PartialASTNode::Nonterminal(nt) => nt.span.as_ref(),
            PartialASTNode::Repetition { span, .. } => span.as_ref(),
            PartialASTNode::Group { span, .. } => span.as_ref(),
            PartialASTNode::Missing(_) => None,
        }
    }

    /// True when there are no Missing placeholders in the subtree.
    pub fn is_complete(&self) -> bool {
        match self {
            PartialASTNode::Terminal(_) => true,
            PartialASTNode::Nonterminal(nt) => nt.children.iter().all(|c| c.is_complete()),
            PartialASTNode::Repetition { item, .. } => item.is_complete(),
            PartialASTNode::Group { children, .. } => children.iter().all(|c| c.is_complete()),
            PartialASTNode::Missing(_) => false,
        }
    }

    /// Convert into a concrete ASTNode. Fails if incomplete or if a repetition wrapper
    /// is encountered at the top-level (repetitions should be flattened by the parent).
    pub fn into_ast(self) -> Result<ASTNode, String> {
        match self {
            PartialASTNode::Terminal(t) => Ok(ASTNode::Terminal(t)),
            PartialASTNode::Nonterminal(nt) => {
                let mut concretized = Vec::new();
                for child in nt.children.into_iter() {
                    Self::flatten_into_vec(child, &mut concretized)?;
                }
                Ok(ASTNode::Nonterminal(crate::logic::ast::NonTerminal {
                    value: nt.value,
                    span: nt.span,
                    children: concretized,
                    binding: nt.binding,
                    bound_typing_rule: None,
                }))
            }
            PartialASTNode::Repetition { .. } => Err("Top-level repetition must be flattened by parent".into()),
            PartialASTNode::Group { .. } => Err("Top-level group must be flattened by parent".into()),
            PartialASTNode::Missing(expected) => Err(format!("Cannot convert missing node to AST: expected {}", expected)),
        }
    }

    /// Helper function to recursively flatten nodes into a vector of concrete AST nodes
    fn flatten_into_vec(node: PartialASTNode, output: &mut Vec<ASTNode>) -> Result<(), String> {
        match node {
            PartialASTNode::Repetition { item, .. } => {
                Self::flatten_into_vec(*item, output)?;
            }
            PartialASTNode::Group { children, .. } => {
                for child in children.into_iter() {
                    Self::flatten_into_vec(child, output)?;
                }
            }
            other => {
                output.push(other.into_ast()?);
            }
        }
        Ok(())
    }

    /// Compact textual form for debugging.
    pub fn show_simple(&self) -> String {
        match self {
            PartialASTNode::Terminal(t) => format!("{}", t.value),
            PartialASTNode::Nonterminal(nt) => {
                let ch = nt.children.iter().map(|c| c.show_simple()).collect::<Vec<_>>().join(" ");
                format!("({} {})", nt.value, ch)
            }
            PartialASTNode::Repetition { symbol, kind, item, .. } => {
                let op = match kind { RepetitionKind::ZeroOrMore => "*", RepetitionKind::OneOrMore => "+", RepetitionKind::ZeroOrOne => "?" };
                let ch = match item.as_ref() {
                    PartialASTNode::Group { children, .. } => {
                        children.iter().map(|c| c.show_simple()).collect::<Vec<_>>().join(" ")
                    }
                    single_item => single_item.show_simple(),
                };
                format!("[{}{} {}]", symbol, op, ch)
            }
            PartialASTNode::Group { children, .. } => {
                let ch = children.iter().map(|c| c.show_simple()).collect::<Vec<_>>().join(",");
                format!("({})", ch)
            },
            PartialASTNode::Missing(expected) => format!("<missing {}>", expected),
        }
    }

    /// Hash all significant components of the AST node for exact deduplication.
    /// This includes node type, value, children structure, bindings, and spans.
    pub fn hash(&self, hasher: &mut DefaultHasher) {
        match self {
            PartialASTNode::Terminal(t) => {
                "TERMINAL".hash(hasher);
                t.value.hash(hasher);
                // Include span for precise location matching
                if let Some(ref span) = t.span {
                    span.start.hash(hasher);
                    span.end.hash(hasher);
                }
            },
            PartialASTNode::Nonterminal(nt) => {
                "NONTERMINAL".hash(hasher);
                nt.value.hash(hasher);
                nt.binding.hash(hasher);
                nt.children.len().hash(hasher);
                for child in &nt.children {
                    child.hash(hasher);
                }
                if let Some(ref span) = nt.span {
                    span.start.hash(hasher);
                    span.end.hash(hasher);
                }
            },
            PartialASTNode::Repetition { symbol, kind, item, span, binding } => {
                "REPETITION".hash(hasher);
                symbol.hash(hasher);
                format!("{:?}", kind).hash(hasher);
                binding.hash(hasher);
                item.hash(hasher);
                if let Some(span) = span {
                    span.start.hash(hasher);
                    span.end.hash(hasher);
                }
            },
            PartialASTNode::Group { children, span } => {
                "GROUP".hash(hasher);
                children.len().hash(hasher);
                for child in children {
                    child.hash(hasher);
                }
                if let Some(span) = span {
                    span.start.hash(hasher);
                    span.end.hash(hasher);
                }
            },
            PartialASTNode::Missing(expected) => {
                "MISSING".hash(hasher);
                expected.hash(hasher);
            },
        }
    }
}

/// Captures a partial parse state of a nonterminal node.
/// The `ast` contains the children parsed so far; the production indicates
/// which production was chosen and `current_index` where we stopped.
#[derive(Debug, Clone)]
pub struct PartialState {
    pub ast: PartialASTNode, // partial AST (children parsed so far)
    pub final_production: PartialProduction,
}

impl PartialState {
    /// Calculate a precise hash based on all significant components of the state.
    /// This ensures exact deduplication - only states with identical structure,
    /// production state, and AST content will be considered duplicates.
    pub fn calculate_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        
        // Hash the AST structure and content
        self.ast.hash(&mut hasher);
        
        // Hash the production state
        self.final_production.hash(&mut hasher);
        
        hasher.finish()
    }

    /// Calculate a semantic hash that focuses on parsing state rather than AST structure.
    /// States that represent the same parsing progress should have the same semantic hash.
    pub fn calculate_semantic_hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        
        // Hash the production rule and progress
        if let Some(ref rule) = self.final_production.production.rule {
            rule.hash(&mut hasher);
        }
        
        // Hash the production RHS symbols for identification
        for symbol in &self.final_production.production.rhs {
            symbol.value().hash(&mut hasher);
        }
        
        // Hash the parsing progress (how many symbols parsed and what's expected)
        self.final_production.parsed.len().hash(&mut hasher);
        
        // Hash what symbols are still expected (remaining + partial) - keep order for different branches
        let mut expected_symbols = Vec::new();
        for symbol in &self.final_production.partial {
            expected_symbols.push(format!("partial:{}", symbol.value()));
        }
        for symbol in &self.final_production.remaining {
            expected_symbols.push(format!("remaining:{}", symbol.value()));
        }
        // Don't sort here - order matters for distinguishing different parsing branches
        expected_symbols.hash(&mut hasher);
        
        // Hash terminal values that have been parsed (include Missing nodes to distinguish incomplete states)
        let (terminals, missing) = self.collect_parsed_and_missing();
        terminals.hash(&mut hasher);
        missing.hash(&mut hasher); // Critical: include missing tokens to distinguish incomplete states
        
        // Hash the immediate next expected symbol to distinguish between different branches
        if let Some(next_symbol) = self.final_production.remaining.first() {
            next_symbol.value().hash(&mut hasher);
        } else if let Some(partial_symbol) = self.final_production.partial.first() {
            partial_symbol.value().hash(&mut hasher);
        }
        
        hasher.finish()
    }

    /// Extract the sequence of terminal values that have been successfully parsed.

    /// Extract both terminal values and missing tokens to distinguish incomplete states.
    fn collect_parsed_and_missing(&self) -> (Vec<String>, Vec<String>) {
        let mut terminals = Vec::new();
        let mut missing = Vec::new();
        self.collect_content_recursive(&self.ast, &mut terminals, &mut missing);
        (terminals, missing)
    }

    fn collect_terminals_recursive(&self, node: &PartialASTNode, terminals: &mut Vec<String>) {
        match node {
            PartialASTNode::Terminal(t) => terminals.push(t.value.clone()),
            PartialASTNode::Nonterminal(nt) => {
                for child in &nt.children {
                    self.collect_terminals_recursive(child, terminals);
                }
            },
            PartialASTNode::Repetition { item, .. } => {
                self.collect_terminals_recursive(item, terminals);
            },
            PartialASTNode::Group { children, .. } => {
                for child in children {
                    self.collect_terminals_recursive(child, terminals);
                }
            },
            PartialASTNode::Missing(_) => {
                // Ignore missing nodes for simple terminal collection
            },
        }
    }

    fn collect_content_recursive(&self, node: &PartialASTNode, terminals: &mut Vec<String>, missing: &mut Vec<String>) {
        match node {
            PartialASTNode::Terminal(t) => terminals.push(t.value.clone()),
            PartialASTNode::Nonterminal(nt) => {
                for child in &nt.children {
                    self.collect_content_recursive(child, terminals, missing);
                }
            },
            PartialASTNode::Repetition { item, .. } => {
                self.collect_content_recursive(item, terminals, missing);
            },
            PartialASTNode::Group { children, .. } => {
                for child in children {
                    self.collect_content_recursive(child, terminals, missing);
                }
            },
            PartialASTNode::Missing(expected) => {
                missing.push(expected.clone()); // Include missing tokens for state differentiation
            },
        }
    }
}

/// Internal state returned while incrementally parsing a symbol
#[derive(Debug, Clone)]
pub enum State {
    Partial(PartialState),
    Complete(PartialASTNode),
    /// A node that is syntactically complete but could potentially accept more input
    /// due to trailing repetitions (e.g., zero-or-more, one-or-more patterns)
    Expandable(PartialASTNode),
}

/// Result of attempting a (partial) parse
#[derive(Debug,Clone)]
pub enum PartialOutcome {
    Complete { node: ASTNode },
    Incomplete { states: Vec<PartialState> },
    /// A node that is syntactically complete but could potentially accept more input
    /// due to trailing repetitions (e.g., zero-or-more, one-or-more patterns)
    Expandable { node: ASTNode },
    Error(String),
}

/// Specific result type for parse steps (avoid empty vec for failures)
pub type ParseResult = Result<Vec<State>, String>;
