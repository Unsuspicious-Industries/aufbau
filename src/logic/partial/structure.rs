use crate::logic::grammar::Production;
use crate::logic::segment::SegmentRange;
use crate::regex::Regex as DerivativeRegex;

use crate::*;

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
    /// returns the first complete tree if it exists
    pub fn complete(&self) -> Option<NonTerminal> {
        self.roots
            .iter()
            .filter(|root| root.is_complete())
            .next()
            .cloned()
    }

    pub fn completes(&self) -> Vec<NonTerminal> {
        self.roots
            .iter()
            .filter(|root| root.is_complete())
            .cloned()
            .collect()
    }

    pub fn is_complete(&self) -> bool {
        self.complete().is_some()
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
    pub fn value(&self) -> &str {
        match self {
            Terminal::Complete { value, .. } => value,
            Terminal::Partial { value, .. } => value,
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
        // Empty productions (epsilon) are complete by definition
        if self.production.rhs.is_empty() {
            return true;
        }

        // Must have exactly as many children as the RHS expects
        if self.children.len() != self.production.rhs.len() {
            return false;
        }

        // All children must themselves be complete terminals or non-terminals.
        // (Checking only the last child is unsound: earlier children could still be partial.)
        self.children.iter().all(|child| match child {
            Node::NonTerminal(nt) => nt.is_complete(),
            Node::Terminal(Terminal::Complete { .. }) => true,
            Node::Terminal(Terminal::Partial { .. }) => false,
        })
    }

    // max parsed len
    // idx of the partial node
    pub fn frontier(&self) -> Option<usize> {
        if self.is_complete() {
            None
        } else {
            // simple but should work
            // Assuming parser correctness
            Some(self.children.len())
        }
    }

    pub fn size(&self) -> usize {
        self.children.iter().map(|c| c.size()).sum::<usize>() + 1
    }

    pub fn consumed_segments(&self) -> usize {
        self.consumed_segments
    }

    /// Get the segment range covered by this nonterminal.
    /// Ugly code
    /// Might need to be replaced or optimized
    pub fn complete_len(
        &self,
        segments: &[crate::logic::grammar::Segment],
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

    pub fn is_frontier(&self, index: usize) -> bool {
        debug_trace!(
            "partial",
            "checking frontier for index {} with len {}",
            index,
            self.children.len()
        );
        if !(index == self.children.len() - 1) {
            return false;
        }
        match self.children.get(index) {
            Some(child) => match child {
                Node::Terminal(_) => true,
                Node::NonTerminal(nt) => {
                    if nt.children.len() == 0 {
                        return true;
                    } else if nt.children.len() == 1 {
                        return nt.is_frontier(0);
                    } else {
                        return false; // Not sure of this one
                    }
                }
            },
            None => false,
        }
    }

    pub fn get(&self, index: usize) -> Result<Option<&Node>, String> {
        if index >= self.production.rhs.len() {
            return Err("Index out of bounds".to_string());
        }
        Ok(self.children.get(index))
    }

    pub fn get_path(&self, path: &[usize]) -> Option<Node> {
        if path.is_empty() {
            return Some(Node::NonTerminal(self.clone()));
        }
        self.children
            .get(path[0])
            .and_then(|child| child.get_path(&path[1..]))
    }

    pub fn get_path_as_nt(&self, path: &[usize]) -> Option<&NonTerminal> {
        if path.is_empty() {
            return Some(self);
        }
        self.children
            .get(path[0])
            .and_then(|child| child.get_path_as_nt(&path[1..]))
    }

    pub fn is_path_nt(&self, path: &[usize]) -> bool {
        self.get_path_as_nt(path).is_some()
    }

    pub fn path_exists(&self, path: &[usize]) -> bool {
        if path.is_empty() {
            return true;
        }
        self.children
            .get(path[0])
            .and_then(|child| Some(child.path_exists(&path[1..])))
            .unwrap_or(false)
    }

    pub fn text(&self) -> Option<String> {
        // Beware this returns some String
        // Only if all strings are some
        // weird rustycity
        self.children.iter().map(|child| child.text()).collect()
    }

    pub fn node_text_path(&self, path: &[usize]) -> Option<String> {
        if path.is_empty() {
            return self.text();
        }
        self.children.get(path[0]).and_then(|child| match child {
            Node::NonTerminal(nt) => nt.node_text_path(&path[1..]),
            Node::Terminal(t) => {
                if path.len() == 1 {
                    Some(t.value().to_string())
                } else {
                    None
                }
            }
        })
    }
}

impl PartialOrd for NonTerminal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.size().cmp(&other.size()))
    }
}

impl Ord for NonTerminal {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.size().cmp(&other.size())
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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

    pub fn size(&self) -> usize {
        match self {
            Node::NonTerminal(nt) => nt.size(),
            Node::Terminal(Terminal::Complete { .. }) => 1,
            Node::Terminal(Terminal::Partial { .. }) => 1,
        }
    }

    pub fn get_path(&self, path: &[usize]) -> Option<Node> {
        if path.is_empty() {
            return Some(self.clone());
        }

        match self {
            Node::NonTerminal(nt) => nt.get_path(path),
            Node::Terminal(_) => None,
        }
    }

    pub fn get_path_as_nt(&self, path: &[usize]) -> Option<&NonTerminal> {
        match self {
            Node::NonTerminal(nt) => nt.get_path_as_nt(path),
            Node::Terminal(_) => None,
        }
    }

    pub fn path_exists(&self, path: &[usize]) -> bool {
        if path.is_empty() {
            return true;
        }
        match self {
            Node::NonTerminal(nt) => nt.path_exists(path),
            Node::Terminal(_) => false,
        }
    }

    pub fn text(&self) -> Option<String> {
        match self {
            Node::NonTerminal(nt) => nt.text(),
            Node::Terminal(Terminal::Complete { value, .. }) => Some(value.clone()),
            Node::Terminal(Terminal::Partial { value, .. }) => Some(value.clone()),
        }
    }
}

impl PartialOrd for Node {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.size().cmp(&other.size()))
    }
}
impl Ord for Node {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.size().cmp(&other.size())
    }
}
