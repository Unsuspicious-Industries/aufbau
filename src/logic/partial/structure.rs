 

use crate::logic::ast::SegmentRange;
use crate::logic::grammar::Production;
use crate::regex::Regex as DerivativeRegex;
use std::collections::HashMap;

/// Top-level partial AST result
#[derive(Clone, Debug)]
pub struct PartialAST {
    pub root: NonTerminal,
    pub input: String,
}

impl PartialAST {
    pub fn new(root: NonTerminal, input: String) -> Self {
        Self { root, input }
    }

    pub fn root(&self) -> &NonTerminal {
        &self.root
    }

    pub fn input(&self) -> &str {
        &self.input
    }
    /// Check if the AST is complete (has at least one complete alternative)
    pub fn complete(&self) -> bool {
        self.root.is_complete()
    }

    /// Convert to a completed AST by selecting a fully matched alternative path
    pub fn into_complete(self) -> Result<crate::logic::ast::ASTNode, String> {
        use crate::logic::ast::{ASTNode, NonTerminal as FullNT};

        let root = FullNT::from_partial(&self.root)?;
        Ok(ASTNode::Nonterminal(root))
    }
}

/// A nonterminal node with parallel alternatives
#[derive(Clone, Debug)]
pub struct NonTerminal {
    /// Name of the nonterminal (e.g., "Expr", "start")
    pub name: String,
    /// Parallel alternatives (different ways to parse this nonterminal)
    pub alts: Vec<Alt>,
    /// Optional binding from grammar
    pub binding: Option<String>,
}

#[derive(Clone, Debug)]
pub enum Terminal {
    Complete {
        value: String,
        span: Option<SegmentRange>,
        binding: Option<String>,
        extension: Option<DerivativeRegex>,
    },
    Partial {
        value: String,
        span: Option<SegmentRange>,
        binding: Option<String>,
        remainder: Option<DerivativeRegex>,
    },
}

impl NonTerminal {
    pub fn new(name: String, alts: Vec<Alt>, binding: Option<String>) -> Self {
        Self {
            name,
            alts,
            binding,
        }
    }

    /// Check if any alternative is complete
    pub fn is_complete(&self) -> bool {
        self.alts.iter().any(|alt| alt.is_complete())
    }
    pub fn pick_complete_alt(&self) -> Option<&Alt> {
        self.alts.iter().find(|alt| alt.is_complete())
    }
}

#[derive(Clone, Debug)]
pub enum Node {
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}

/// A slot represents the state of one RHS symbol of a production.
#[derive(Clone, Debug)]
pub struct Slot {
    nodes: Vec<Node>, // for repetitions 
    repetition: (usize, Option<usize>),
}
impl Slot {

    pub fn nodes(&self) -> &[Node] {
        &self.nodes
    }

    pub fn is_complete(&self) -> bool {
        let (min, _) = self.repetition;
        let count = self.nodes.len();
        match count {
            c if (c < min) => false,
            c if (c == min) => match self.nodes.last() {
                Some(Node::NonTerminal(nt)) => nt.is_complete(),
                Some(Node::Terminal(t)) => match t {
                    Terminal::Complete { .. } => true,
                    Terminal::Partial { .. } => false,
                },
                None => false,
            },
            _ => true,
        }

    }
}


/// One concrete alternative of a non-terminal.
#[derive(Clone, Debug)]
pub struct Alt {
    /// Grammar production this alternative implements.
    pub production: Production,
    /// Tape of slots; HashMap allows sparse representation (missing = zero matches for optional symbols)
    pub slots: HashMap<usize, Slot>,
    /// Span covering consumed segments from first Filled/Partial to last.
    pub span: Option<SegmentRange>,
}

impl Alt {
    pub fn new(production: Production) -> Self {
        Self {
            production,
            slots: HashMap::new(),
            span: None,
        }
    }

    /// Get slot for a given symbol index (None if no entry = zero matches)
    pub fn get_slot(&self, idx: usize) -> Option<&Slot> {
        self.slots.get(&idx)
    }

    /// Get mutable slot for a given symbol index
    pub fn get_slot_mut(&mut self, idx: usize) -> Option<&mut Slot> {
        self.slots.get_mut(&idx)
    }

    /// Check if symbol at index has no slot
    pub fn symbol_empty(&self, idx: usize) -> bool {
        self.slots.get(&idx).is_none()
    }

    // ------------------------
    // Alt state queries
    // ------------------------

    /// Check if this alternative is complete (all symbols satisfied)
    pub fn is_complete(&self) -> bool {
        self.production
            .rhs
            .iter()
            .enumerate()
            .all(|(idx, _)| 
                match self.get_slot(idx) {
                    Some(slot) => slot.is_complete(),
                    None => false,
                }
            )
    }
    pub fn span(&self) -> Option<SegmentRange> {
        self.span.clone()
    }
}

 