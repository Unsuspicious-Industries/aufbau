//! Arena-backed parse hypergraph.
//!
//! Formally, the arena stores a finite directed hypergraph:
//! - node set: `ArenaNode`
//! - hyperedge set: `PackedAlt`
//! - path algebra: `Path`
//!
//! A packed alternative is a hyperedge from one parent node to an ordered list
//! of child refs. A parse forest is then a rooted sub-hypergraph selected by a
//! root set plus deterministic path choices.

/// Maximum iterations when parsing repetition patterns (e.g., `a*`, `a+`).
/// This bounds the worst-case for zero-or-more and one-or-more repetitions
/// to prevent infinite loops when the inner pattern can match empty strings.
pub const MAX_REPETITION_ITERATIONS: usize = 1000;

use std::cell::RefCell;

use crate::logic::typing::ContextTransition;
use crate::{debug_trace, logic::Segment};

pub use crate::logic::grammar::{AltId, NtId, ProdId};

pub type NodeId = usize;
pub type CtxId = usize;

pub type TypeId = usize;

/// The interned ID for `Type::Any` — always the first type registered.
pub const ANY_TYPE: TypeId = 0;

/// A byte span `[start, end)`.
///
/// >T Span Bounding
/// A node's span always bounds its children's spans,
/// and children are strictly ordered left-to-right.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[derive(Clone, Hash, Copy, Debug, PartialEq, Eq)]
pub struct Lexeme {
    pub matched: Span,
    pub open: bool,
    pub complete: bool,
}

impl Lexeme {
    pub fn new(span: Span, complete: bool, open: bool) -> Self {
        Self {
            matched: span,
            open,
            complete,
        }
    }

    pub fn value<'a>(&self, s: &[Segment]) -> Option<String> {
        if self.matched.end as usize <= s.len() {
            Some(
                s[self.matched.start as usize..self.matched.end as usize]
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<&str>>()
                    .join(" "),
            )
        } else {
            None
        }
    }
}

/// Whether a node is a fully accepted parse or only a typed prefix.
///
/// This is a semantic status, not just a structural one:
/// - `Complete` means the production is fully matched and typing accepted it
///   without relying on prefix-only reasoning.
/// - `Partial` means either the syntax is still open, or typing accepted it
///   only as a prefix approximation.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeStatus {
    Closed, // complete closed
    Partial, // partial
    Extensible, // complete but open to extension at end-of-input
}
impl NodeStatus {
    pub fn complete(&self) -> bool {
        matches!(self, NodeStatus::Closed) || matches!(self, NodeStatus::Extensible)
    }
    pub fn open(&self) -> bool {
        matches!(self, NodeStatus::Partial) || matches!(self, NodeStatus::Extensible)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AltRange {
    pub start: usize,
    pub len: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum ChildRef {
    Node(NodeId),
    /// Consumed terminal: `(span, complete, open)`.
    ///
    /// `complete` — the regex is fully satisfied, so the Earley item advances.
    /// `open` — at end-of-input the lexeme could still grow (Extensible/Prefix).
    ///
    /// A node whose syntax is all-complete but has an open child is `Partial`,
    /// because the same text might resolve differently with more input.
    Terminal(Lexeme),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackedAlt {
    pub prod: ProdId,
    pub children: Vec<ChildRef>,
}

/// Resolved binding stored on arena nodes for downstream use (display, search).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Binding {
    pub name: String,
    pub value: Option<Lexeme>,
    pub ty: Option<TypeId>,
}

/// Shared typed parse node.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArenaNode {
    pub nt: NtId,
    pub span: Span,
    pub status: NodeStatus,
    pub ty: TypeId,
    pub ctr: Option<ContextTransition>,
    pub bindings: Vec<Binding>,
    pub alts: AltRange,
}

impl ArenaNode {
    pub fn is_complete(&self) -> bool {
        self.status.complete()
    }
}

impl PartialOrd for ArenaNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // cmp only if nt same
        if self.nt == other.nt {
            // a node is bigger if it covers more input or if ties, if it has more complete children
            Some(self.span.end.cmp(&other.span.end).then_with(|| {
                let self_complete = self.is_complete() as usize;
                let other_complete = other.is_complete() as usize;
                self_complete.cmp(&other_complete)
            }))
        } else {
            None
        }
    }
}

#[derive(Debug, Default)]
pub struct ParseArena {
    nodes: RefCell<Vec<ArenaNode>>,
    alts: RefCell<Vec<PackedAlt>>,
}

impl ParseArena {
    #[track_caller]
    pub fn snapshot(&self) -> Self {
        debug_trace!(
            "fusion_memory",
            "parse_arena_snapshot file={} line={}",
            std::panic::Location::caller().file(),
            std::panic::Location::caller().line()
        );
        Self {
            nodes: RefCell::new(self.nodes.borrow().clone()),
            alts: RefCell::new(self.alts.borrow().clone()),
        }
    }

    /// Time: O(1) amortized. Space: O(1) additional.
    pub fn new() -> Self {
        Self::default()
    }

    /// Time: O(k), where k is the number of alternatives attached to the node.
    /// Space: O(k) additional.
    pub fn push_node(&self, mut node: ArenaNode, alts: Vec<PackedAlt>) -> NodeId {
        let mut alts_vec = self.alts.borrow_mut();
        let start = alts_vec.len();
        let len = alts.len();
        alts_vec.extend(alts);
        drop(alts_vec);
        node.alts = AltRange { start, len };
        let id = self.nodes.borrow().len();
        self.nodes.borrow_mut().push(node);
        id
    }

    /// Time: O(1). Space: O(1).
    pub fn node(&self, id: NodeId) -> Option<std::cell::Ref<'_, ArenaNode>> {
        std::cell::Ref::filter_map(self.nodes.borrow(), |nodes| nodes.get(id)).ok()
    }

    /// Time: O(1). Space: O(1).
    pub fn alts_for(&self, id: NodeId) -> Option<std::cell::Ref<'_, [PackedAlt]>> {
        let nodes = self.nodes.borrow();
        let node = nodes.get(id)?;
        let range = node.alts.start..node.alts.start + node.alts.len;
        drop(nodes);
        std::cell::Ref::filter_map(self.alts.borrow(), |alts| alts.get(range)).ok()
    }

    /// Time: O(1). Space: O(1).
    pub fn node_count(&self) -> usize {
        self.nodes.borrow().len()
    }

    /// Time: O(1). Space: O(1).
    pub fn alt_count(&self) -> usize {
        self.alts.borrow().len()
    }

    pub fn order(&self) -> usize {
        self.node_count()
    }

    pub fn size(&self) -> usize {
        self.alt_count()
    }

    // child ref methods
    pub fn complete(&self, child: &ChildRef) -> bool {
        match child {
            ChildRef::Node(node_id) => self
                .nodes
                .borrow()
                .get(*node_id)
                .is_some_and(|node| node.status.complete()),
            ChildRef::Terminal(Lexeme {
                matched: _,
                open: _,
                complete,
            }) => *complete,
        }
    }
    pub fn open(&self, child: &ChildRef) -> bool {
        match child {
            ChildRef::Node(node_id) => self
                .nodes
                .borrow()
                .get(*node_id)
                .is_some_and(|node| node.status.open()),
            ChildRef::Terminal(Lexeme {
                matched: _,
                open,
                complete: _,
            }) => *open,
        }
    }
}
