use crate::debug_trace;
use crate::logic::bind::BoundTypingRule;
use crate::logic::grammar::{Production, Symbol};

#[derive(Clone, Debug)]
pub enum PartialASTNode {
    Terminal(PartialTerminal),
    NonTerminal(Vec<PartialNonTerminal>),
}

/// Wrapper for PartialNonTerminal that includes input string and metadata for completeness checking
#[derive(Clone, Debug)]
pub struct PartialAST {
    pub root: PartialASTNode,
    pub input: String,
}

#[derive(Clone, Debug)]
pub struct PartialTerminal {
    pub value: String,
    pub span: Option<SourceSpan>,
    pub binding: Option<String>,
}

#[derive(Clone, Debug)]
pub struct PartialNonTerminal {
    pub production: PartialProduction,
    pub children: Vec<PartialASTNode>,
    pub value: String,
    pub span: Option<SourceSpan>,
    pub binding: Option<String>,
    pub bound_typing_rule: Option<Box<BoundTypingRule>>,
}



#[derive(Clone, Debug)]
pub struct PartialProduction {
    production: Production,
    fully_parsed_symbols: usize,
    partially_parsed_symbols: usize, // 0 or 1 to indicate an in-progress symbol
}

impl PartialProduction {
    pub fn new(production: Production) -> Self {
        Self {
            production,
            fully_parsed_symbols: 0,
            partially_parsed_symbols: 0,
        }
    }

    fn is_finished(&self) -> bool {
        self.fully_parsed_symbols >= self.production.rhs.len()
    }

    /// A production is complete if it has parsed all its symbols AND
    /// has children that correspond to meaningful content
    fn complete(&self, children_count: usize) -> bool {
        self.is_finished() && children_count > 0
    }

    pub fn set_cursor(&mut self, cursor: usize) {
        // Backwards compatible setter: treat cursor as fully parsed count
        self.fully_parsed_symbols = cursor;
        self.partially_parsed_symbols = 0;
    }

    pub fn set_progress(&mut self, fully_parsed: usize, partially_parsed: usize) {
        self.fully_parsed_symbols = fully_parsed;
        self.partially_parsed_symbols = if partially_parsed > 0 { 1 } else { 0 };
    }

    pub fn fully_parsed_symbols_count(&self) -> usize {
        self.fully_parsed_symbols
    }

    pub fn has_partial_in_progress(&self) -> bool {
        self.partially_parsed_symbols > 0
    }

    // --- Public introspection for visualization ---
    pub fn rhs_len(&self) -> usize { self.production.rhs.len() }
    pub fn cursor_value(&self) -> usize { self.fully_parsed_symbols + self.partially_parsed_symbols }
    pub fn is_complete_state(&self, children_count: usize) -> bool { self.complete(children_count) }
    pub fn rhs_symbols(&self) -> &Vec<Symbol> { &self.production.rhs }
}

impl PartialNonTerminal {
    pub fn production(&self) -> &PartialProduction { &self.production }
    pub fn children(&self) -> &Vec<PartialASTNode> { &self.children }
    pub fn value(&self) -> &String { &self.value }
    pub fn span(&self) -> &Option<SourceSpan> { &self.span }
    pub fn binding(&self) -> &Option<String> { &self.binding }
    /// A branch is considered stopped if the production cursor didn't reach the end.
    pub fn stopped(&self) -> bool {
        self.production.cursor_value() < self.production.rhs_len()
    }
    pub fn as_node(&self) -> PartialASTNode {
        PartialASTNode::NonTerminal(vec![self.clone()])
    }

    pub fn into_complete_node(self) -> Result<CompleteAST, String> {
        if !self.production.complete(self.children.len()) {
            return Err("Production is not complete".to_string());
        }

        debug_trace!("PartialNonTerminal::into_complete_node", "Converting nonterminal with {} children", self.children.len());
        
        let mut children = Vec::new();
        for child in self.children {
            match child.into_complete_node() {
                Ok(complete_child) => children.push(complete_child),
                Err(e) => {
                    debug_trace!("PartialNonTerminal::into_complete_node", "Child could not be converted to CompleteAST: {}", e);
                    return Err(e);
                }
            }
        }

        Ok(CompleteAST::Nonterminal(
            NonTerminal {
                children,
                value: self.value,
                span: self.span,
                binding: self.binding,
                bound_typing_rule: self.bound_typing_rule,
            }
        ))
    }

    /// Strict completeness: production complete AND all required literals appear
    /// in the subtree AND all descendants are strictly complete as applicable.
    pub fn is_strict_complete(&self) -> bool {
        if !self.production.is_complete_state(self.children.len()) { return false; }
        // Check that required literals in RHS appear in the subtree
        fn child_has_literal(children: &[PartialASTNode], lit: &str) -> bool {
            for ch in children {
                match ch {
                    PartialASTNode::Terminal(t) => { if t.value == lit { return true; } }
                    PartialASTNode::NonTerminal(alts) => {
                        for a in alts { if child_has_literal(a.children(), lit) { return true; } }
                    }
                }
            }
            false
        }
        for sym in self.production.rhs_symbols() {
            if let crate::logic::grammar::Symbol::Litteral(l) = sym {
                if !child_has_literal(&self.children, l) { return false; }
            }
        }
        // All nonterminal descendants must be strictly complete too
        for ch in &self.children {
            if let PartialASTNode::NonTerminal(alts) = ch {
                // At least one strictly complete alternative must exist
                if !alts.iter().any(|a| a.is_strict_complete()) { return false; }
            }
        }
        true
    }
}


use crate::logic::ast::{ASTNode as CompleteAST, NonTerminal, SourceSpan, Terminal};

impl PartialAST {
    pub fn new(root: PartialASTNode, input: String) -> Self {
        Self { root, input }
    }

    pub fn root(&self) -> &PartialASTNode {
        &self.root
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn end(&self) -> usize {
        match &self.root {
            PartialASTNode::Terminal(t) => t.span.as_ref().map_or(0, |s| s.end),
            PartialASTNode::NonTerminal(parallels) => {
                parallels.iter().filter_map(|p| p.span.as_ref().map(|s| s.end)).max().unwrap_or(0)
            }
            
        }
    }

    pub fn consumed_input(&self) -> &str {
        &self.input[0..self.end().min(self.input.len())]
    }

    /// Check if this partial AST represents a complete parse of the input
    pub fn is_complete(&self) -> bool {
        // Strict completeness: a parse is complete if at least one root parallel
        // alternative is strictly complete (see PartialNonTerminal::is_strict_complete)
        // and the entire input is consumed.
        let root_complete = match &self.root {
            PartialASTNode::Terminal(_) => false,
            PartialASTNode::NonTerminal(parallels) => parallels.iter().any(|p| p.is_strict_complete()),
            
        };
        root_complete && self.end() == self.input.len()
    }

    pub fn into_complete(self) -> Result<CompleteAST, String> {
        // Check if the entire input was consumed
        let end_pos = self.end();
        if end_pos != self.input.len() {
            return Err(format!("Incomplete parse: consumed {} of {} characters: \n '{}' of '{}'", 
                              end_pos, self.input.len(), self.consumed_input(), self.input()));
        }
        // Check if at least one strictly complete alternative exists
        if !self.is_complete() {
            return Err("Partial AST is not complete".to_string());
        }

        // Convert the root to a complete AST (will pick one strictly complete alternative)
        self.root.into_complete_node()
    }

    pub fn divide(&self) -> Vec<PartialASTNode> {
        self.root.divide()
    }

}

impl PartialASTNode {
    pub fn into_complete_node(self) -> Result<CompleteAST, String> {
        match self {
            PartialASTNode::Terminal(term) => {
                Ok(CompleteAST::Terminal(Terminal {
                    value: term.value,
                    span: term.span,
                    binding: term.binding,
                }))
            }
            PartialASTNode::NonTerminal(parallels) => {
                if parallels.is_empty() {
                    return Err("No productions in NonTerminal".to_string());
                }
                debug_trace!("PartialASTNode::into_complete_node", "Converting NonTerminal with {} parallel productions", parallels.len());
                
                // Find a strictly complete production
                for p in &parallels {
                    if p.is_strict_complete() {
                        debug_trace!("PartialASTNode::into_complete_node", "Found complete production with {} children", p.children.len());
                        let mut children = Vec::new();
                        for child in &p.children {
                            match child.clone().into_complete_node() {
                                Ok(complete_child) => children.push(complete_child),
                                Err(_) => {
                                    debug_trace!("PartialASTNode::into_complete_node", "Child could not be converted to CompleteAST");
                                    continue;
                                }
                            }
                        }
                        return Ok(CompleteAST::Nonterminal(
                            NonTerminal {
                                children,
                                value: p.value.clone(),
                                span: p.span.clone(),
                                binding: p.binding.clone(),
                                bound_typing_rule: p.bound_typing_rule.clone(),
                            })
                        )
                    }
                }
                
                Err("No complete production found in NonTerminal".to_string())
            }
        }
    }

    pub fn divide(&self) -> Vec<PartialASTNode> {
        match self {
            PartialASTNode::Terminal(term) => vec![PartialASTNode::Terminal(term.clone())],
            PartialASTNode::NonTerminal(parallels) => {
                parallels.iter().map(|p| {
                    PartialNonTerminal {
                        production: p.production.clone(),
                        children: p.children.iter().flat_map(|c| c.divide()).collect(),
                        value: p.value.clone(),
                        span: p.span.clone(),
                        binding: p.binding.clone(),
                        bound_typing_rule: p.bound_typing_rule.clone(),
                    }.as_node()
                }).collect()
            }
        }   
    }
}