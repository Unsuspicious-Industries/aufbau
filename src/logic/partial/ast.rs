use crate::debug_trace;
use crate::logic::bind::BoundTypingRule;
use crate::logic::grammar::{Production, RepetitionKind, Symbol};

#[derive(Clone, Debug)]
pub enum PartialAST {
    Terminal(PartialTerminal),
    NonTerminal(Vec<PartialNonTerminal>),
    Mismatch,
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
    pub children: Vec<PartialAST>,
    pub value: String,
    pub span: Option<SourceSpan>,
    pub binding: Option<String>,
    pub bound_typing_rule: Option<Box<BoundTypingRule>>,
}



#[derive(Clone, Debug)]
pub struct PartialProduction {
    production: Production,
    cursor: usize, // position in the production's RHS
}

impl PartialProduction {
    pub fn new(production: Production) -> Self {
        Self {
            production,
            cursor: 0,
        }
    }

    fn is_finished(&self) -> bool {
        self.cursor >= self.production.rhs.len()
    }

    /// A production is complete if it has parsed all its symbols AND
    /// has children that correspond to meaningful content
    fn complete(&self, children_count: usize) -> bool {
        self.is_finished() && children_count > 0
    }

    pub fn set_cursor(&mut self, cursor: usize) {
        self.cursor = cursor;
    }

    // --- Public introspection for visualization ---
    pub fn rhs_len(&self) -> usize { self.production.rhs.len() }
    pub fn cursor_value(&self) -> usize { self.cursor }
    pub fn is_complete_state(&self, children_count: usize) -> bool { self.complete(children_count) }
}

impl PartialNonTerminal {
    pub fn production(&self) -> &PartialProduction { &self.production }
    pub fn children(&self) -> &Vec<PartialAST> { &self.children }
    pub fn value(&self) -> &String { &self.value }
    pub fn span(&self) -> &Option<SourceSpan> { &self.span }
    pub fn binding(&self) -> &Option<String> { &self.binding }
    /// A branch is considered stopped if the production cursor didn't reach the end.
    pub fn stopped(&self) -> bool {
        self.production.cursor_value() < self.production.rhs_len()
    }
}


use crate::logic::ast::{ASTNode as CompleteAST, NonTerminal, SourceSpan, Terminal};

impl PartialAST {
    pub fn into_complete(self) -> Result<CompleteAST, String> {
        match self {
            PartialAST::Terminal(term) => {
                Ok(CompleteAST::Terminal(Terminal {
                    value: term.value,
                    span: term.span,
                    binding: term.binding,
                }))
            }
            PartialAST::NonTerminal(parallels) => {
                if parallels.is_empty() {
                    return Err("No productions in NonTerminal".to_string());
                }
                debug_trace!("PartialAST::into_complete", "Converting NonTerminal with {} parallel productions", parallels.len());
                
                // Find a complete production
                for p in &parallels {
                    if p.production.complete(p.children.len()) {
                        debug_trace!("PartialAST::into_complete", "Found complete production with {} children", p.children.len());
                        let mut children = Vec::new();
                        for child in &p.children {
                            match child.clone().into_complete() {
                                Ok(complete_child) => children.push(complete_child),
                                Err(_) => {
                                    debug_trace!("PartialAST::into_complete", "Child could not be converted to CompleteAST");
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
            PartialAST::Mismatch => Err("Cannot convert Mismatch to CompleteAST".to_string()),
        }
    }
}