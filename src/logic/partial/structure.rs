use crate::logic::ast::SegmentRange;
use crate::logic::grammar::Production;
use crate::regex::Regex as DerivativeRegex;

/// Top-level partial AST result
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PartialAST {
    pub roots: Vec<NonTerminal>,
    pub input: String,
}

impl PartialAST {
    pub fn new(roots: Vec<NonTerminal>, input: String) -> Self {
        Self { roots, input }
    }

    pub fn roots(&self) -> &[NonTerminal] {
        &self.roots
    }

    pub fn input(&self) -> &str {
        &self.input
    }
    /// Check if the AST is complete (has at least one complete tree)
    pub fn complete(&self) -> bool {
        self.roots.iter().any(|root| root.is_complete())
    }

    /// Convert to a completed AST by selecting the first fully matched tree
    pub fn into_complete(self) -> Result<crate::logic::ast::ASTNode, String> {
        use crate::logic::ast::{ASTNode, NonTerminal as FullNT};

        let root = self
            .roots
            .into_iter()
            .find(|r| r.is_complete())
            .ok_or_else(|| "No complete tree found".to_string())?;

        let root = FullNT::from_partial(&root)?;
        Ok(ASTNode::Nonterminal(root))
    }
}

/// A nonterminal node representing a specific choice of production
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NonTerminal {
    /// Name of the nonterminal (e.g., "Expr", "start")
    pub name: String,
    /// The production rule used for this node
    pub production: Production,
    /// The index of the alternative chosen
    pub alternative_index: usize,
    /// The children nodes
    pub children: Vec<Node>,
    /// Optional binding from grammar
    pub binding: Option<String>,
    /// Number of segments consumed by this node
    pub consumed_segments: usize,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum Terminal {
    Complete {
        value: String,
        binding: Option<String>,
        extension: Option<DerivativeRegex>,
    },
    Partial {
        value: String,
        binding: Option<String>,
        remainder: Option<DerivativeRegex>,
    },
}

impl Terminal {
    /// Get the length (in bytes) that this terminal matches.
    pub fn len(&self) -> usize {
        match self {
            Terminal::Complete { value, .. } => value.len(),
            Terminal::Partial { value, .. } => value.len(),
        }
    }
}

impl NonTerminal {
    pub fn new(
        name: String,
        production: Production,
        alternative_index: usize,
        children: Vec<Node>,
        binding: Option<String>,
        consumed_segments: usize,
    ) -> Self {
        Self {
            name,
            production,
            alternative_index,
            children,
            binding,
            consumed_segments,
        }
    }

    pub fn is_complete(&self) -> bool {
        if self.children.len() != self.production.rhs.len() {
            return false;
        }
        self.children.iter().all(|child| match child {
            Node::NonTerminal(nt) => nt.is_complete(),
            Node::Terminal(Terminal::Complete { .. }) => true,
            Node::Terminal(Terminal::Partial { .. }) => false,
        })
    }

    pub fn consumed_segments(&self) -> usize {
        self.consumed_segments
    }

    /// Get the segment range covered by this nonterminal.
    pub fn complete_len(
        &self,
        segments: &[crate::logic::tokenizer::Segment],
    ) -> Option<SegmentRange> {
        if !self.is_complete() {
            return None;
        }

        let mut min_seg: Option<usize> = None;
        let mut max_seg: Option<usize> = None;

        for child in &self.children {
            match child {
                Node::Terminal(Terminal::Complete { value, .. }) => {
                    for seg in segments {
                        if seg.text() == *value {
                            let seg_idx = seg.index;
                            min_seg = Some(min_seg.map_or(seg_idx, |m| m.min(seg_idx)));
                            max_seg = Some(max_seg.map_or(seg_idx, |m| m.max(seg_idx)));
                            break;
                        }
                    }
                }
                Node::Terminal(Terminal::Partial { .. }) => return None,
                Node::NonTerminal(nt) => {
                    if let Some(range) = nt.complete_len(segments) {
                        min_seg = Some(min_seg.map_or(range.start, |m| m.min(range.start)));
                        max_seg = Some(max_seg.map_or(range.end, |m| m.max(range.end)));
                    } else {
                        return None;
                    }
                }
            }
        }

        match (min_seg, max_seg) {
            (Some(start), Some(end)) => Some(SegmentRange::new(start, end)),
            _ => None,
        }
    }
}

#[derive(Clone, Debug,Hash, PartialEq, Eq)]
pub enum Node {
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}

impl Node {
    pub fn is_complete(&self) -> bool {
        match self {
            Node::NonTerminal(nt) => nt.is_complete(),
            Node::Terminal(Terminal::Complete { .. }) => true,
            Node::Terminal(Terminal::Partial { .. }) => false,
        }
    }
}
