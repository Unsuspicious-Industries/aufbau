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

use crate::{debug_trace, engine::grammar::Segment};

pub use crate::engine::grammar::{AltId, NtId, ProdId};

pub type NodeId = usize;
pub type CtxId = usize;

pub type EvidenceId = usize;
pub type EffectId = usize;
pub type TypeId = EvidenceId;

/// The interned ID for the domain's "top" / "any" evidence — always the
/// first evidence value registered by the domain. Corresponds to the
/// "no constraint yet" sentinel used as default for untyped nodes.
pub const TOP: EvidenceId = 0;

/// The interned ID for the domain's "bottom" / "none" evidence.
/// Represents contradictory evidence that can never be satisfied.
pub const BOT: EvidenceId = 1;

pub const DEFAULT_EVIDENCE: EvidenceId = TOP;
pub const ANY_TYPE: TypeId = TOP;

/// A byte span `[start, end)`.
///
/// >T Span Bounding
/// > A node's span always bounds its children's spans,
/// > and children are strictly ordered left-to-right.
#[derive(Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Clone for Span {
    fn clone(&self) -> Self {
        *self
    }
}

#[derive(Clone, Hash, Copy, Debug, PartialEq, Eq)]
pub struct Lexeme {
    pub matched: Span,
    pub open: bool,
    pub complete: bool,
}

impl Lexeme {
    #[must_use]
    pub fn new(span: Span, complete: bool, open: bool) -> Self {
        Self {
            matched: span,
            open,
            complete,
        }
    }

    #[must_use]
    pub fn value(&self, s: &[Segment]) -> Option<String> {
        if self.matched.end as usize <= s.len() {
            Some(
                s[self.matched.start as usize..self.matched.end as usize]
                    .iter()
                    .map(super::super::grammar::tokenizer::Segment::as_str)
                    .collect::<Vec<&str>>()
                    .join(" "),
            )
        } else {
            None
        }
    }
}

/// `ρ` from `def:evidence-summary`, representing `def:evidence-graph` node
/// frontier status. Together with `def:open-closed`: Exact nodes are
/// non-frontier (graph may be closed), Prefix/Extensible carry frontier
/// vertices (graph is open).
///
/// - `Exact`: exact on the current input; node may export context transforms.
/// - `Extensible`: fully recognized but some descendant is still extendable at
///   the right frontier — an accepted prefix, not an exact parse.
/// - `Prefix`: only prefix-accepted; some required descendant is syntactically
///   incomplete at EOF.
///
/// Renamed `Closed` → `Exact` to avoid shadowing the set `Closed` of
/// `def:constraint-domain`. `Partial` → `Prefix` matches the draft.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeStatus {
    Exact,
    Prefix,
    Extensible,
}
impl NodeStatus {
    /// Recognized on the current prefix.
    #[must_use]
    pub fn complete(&self) -> bool {
        matches!(self, NodeStatus::Exact | NodeStatus::Extensible)
    }

    /// Exact on the current input.
    #[must_use]
    pub fn exact(&self) -> bool {
        matches!(self, NodeStatus::Exact)
    }

    /// Still open to extension or syntactically unfinished.
    #[must_use]
    pub fn open(&self) -> bool {
        matches!(self, NodeStatus::Prefix | NodeStatus::Extensible)
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
    /// A fully recognized node with an open child is `Extensible`: accepted as a
    /// prefix, but not exact on the current input.
    Terminal(Lexeme),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackedAlt {
    pub prod: ProdId,
    pub children: Vec<ChildRef>,
}

/// Status of binding resolution at this node, materializing β(ν,b) (Def. 5).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BindingStatus {
    /// Child evidence is available (binding position ≤ dot).
    Resolved {
        span: Span,
        complete: bool,
        open: bool,
        evidence: EvidenceId,
    },
    /// Dot has not yet reached the binding's position.
    Pending { position: usize },
}

/// Map from binding name → resolution status: the concrete β(ν,b) map.
pub type BindingMap = std::collections::HashMap<String, BindingStatus>;

/// Shared parse node with opaque semantic evidence.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArenaNode {
    pub nt: NtId,
    pub span: Span,
    pub status: NodeStatus,
    pub semantic_complete: bool,
    pub evidence: EvidenceId,
    pub effect: Option<EffectId>,
    /// Explicit β(ν,b) map: binding name → resolved/pending status.
    pub binding_map: BindingMap,
    pub alts: AltRange,
}

impl ArenaNode {
    #[must_use]
    pub fn is_complete(&self) -> bool {
        self.status.complete() && self.semantic_complete
    }

    /// Compatibility helper for the current typing domain.
    #[must_use]
    pub fn type_id(&self) -> TypeId {
        self.evidence
    }
}

impl PartialOrd for ArenaNode {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // cmp only if nt same
        if self.nt == other.nt {
            // a node is bigger if it covers more input or if ties, if it has more complete children
            Some(self.span.end.cmp(&other.span.end).then_with(|| {
                let self_complete = usize::from(self.is_complete());
                let other_complete = usize::from(other.is_complete());
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
    #[must_use]
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
