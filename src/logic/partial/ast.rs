use crate::debug_trace;
use crate::logic::bind::BoundTypingRule;
use crate::logic::grammar::Symbol;
use crate::logic::partial::production::PartialProduction;

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

// Methods for PartialNonTerminal and PartialAST remain below

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
            PartialASTNode::NonTerminal(parallels) => parallels
                .iter()
                .filter_map(|p| p.span.as_ref().map(|s| s.end))
                .max()
                .unwrap_or(0),
        }
    }

    pub fn consumed(&self) -> &str {
        &self.input[0..self.end().min(self.input.len())]
    }

    /// Check if this partial AST represents a complete parse of the input
    pub fn complete(&self) -> bool {
        self.root.complete() && self.end() == self.input.len()
    }

    pub fn into_complete(self) -> Result<CompleteAST, String> {
        let end_pos = self.end();
        if end_pos != self.input.len() {
            return Err(format!(
                "Incomplete parse: consumed {} of {} characters: \n '{}' of '{}'",
                end_pos,
                self.input.len(),
                self.consumed(),
                self.input()
            ));
        }
        if !self.complete() {
            return Err("Partial AST is not complete".to_string());
        }
        self.root.into_complete()
    }
}

impl PartialASTNode {
    pub fn into_complete(self) -> Result<CompleteAST, String> {
        match self {
            PartialASTNode::Terminal(term) => Ok(CompleteAST::Terminal(Terminal {
                value: term.value,
                span: term.span,
                binding: term.binding,
            })),
            PartialASTNode::NonTerminal(parallels) => {
                if parallels.is_empty() {
                    return Err("No productions in NonTerminal".to_string());
                }
                debug_trace!(
                    "PartialASTNode::into_complete_node",
                    "Converting NonTerminal with {} parallel productions",
                    parallels.len()
                );
                'alts: for p in &parallels {
                    if p.production.complete(p.children.len()) {
                        debug_trace!(
                            "PartialASTNode::into_complete_node",
                            "Found complete production with {} children",
                            p.children.len()
                        );
                        let mut children = Vec::new();
                        for child in &p.children {
                            match child.clone().into_complete() {
                                Ok(complete_child) => children.push(complete_child),
                                Err(e) => {
                                    debug_trace!(
                                        "PartialASTNode::into_complete_node",
                                        "Rejecting alternative due to child conversion error: {}",
                                        e
                                    );
                                    continue 'alts;
                                }
                            }
                        }
                        return Ok(CompleteAST::Nonterminal(NonTerminal {
                            children,
                            value: p.value.clone(),
                            span: p.span.clone(),
                            binding: p.binding.clone(),
                            bound_typing_rule: p.bound_typing_rule.clone(),
                        }));
                    }
                }
                Err("No complete production found in NonTerminal".to_string())
            }
        }
    }

    pub fn complete(&self) -> bool {
        match self {
            PartialASTNode::Terminal(t) => t.span.is_some(),
            PartialASTNode::NonTerminal(parallels) => parallels.iter().any(|p| p.is_complete()),
        }
    }
}

impl PartialNonTerminal {
    pub fn production(&self) -> &PartialProduction {
        &self.production
    }
    pub fn children(&self) -> &Vec<PartialASTNode> {
        &self.children
    }
    pub fn value(&self) -> &String {
        &self.value
    }
    pub fn span(&self) -> &Option<SourceSpan> {
        &self.span
    }
    pub fn binding(&self) -> &Option<String> {
        &self.binding
    }
    pub fn stopped(&self) -> bool {
        self.production.cursor_value() < self.production.rhs_len()
    }
    pub fn as_node(&self) -> PartialASTNode {
        PartialASTNode::NonTerminal(vec![self.clone()])
    }
    pub fn is_complete(&self) -> bool {
        if !self.production.complete(self.children.len()) {
            return false;
        }
        self.children.iter().all(|child| child.complete())
    }
    pub fn is_progressing(&self) -> bool {
        self.production.fully_parsed_symbols_count() > 0
            || self.children.iter().any(PartialASTNode::complete)
    }
    pub fn next_symbol(&self) -> Option<&Symbol> {
        self.production
            .next_symbol_index()
            .and_then(|idx| self.production.symbol_at(idx))
    }
}
