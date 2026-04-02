//! Arena-backed parse hypergraph.
//!
//! Formally, the arena stores a finite directed hypergraph:
//! - node set: `ArenaNode`
//! - hyperedge set: `PackedAlt`
//! - path algebra: `PathId` / `PathStep`
//!
//! A packed alternative is a hyperedge from one parent node to an ordered list
//! of child refs. A parse forest is then a rooted sub-hypergraph selected by a
//! root set plus deterministic path choices.

use std::cell::RefCell;
use std::collections::HashMap;

use crate::debug_trace;

use crate::logic::fusion::BindingValue;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NtId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct ProdId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct NodeId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct AltId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct CtxId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct PathId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FrontierId(pub usize);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PathStep {
    pub child: u16,
    pub alt: u16,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PathNode {
    pub parent: Option<PathId>,
    pub step: PathStep,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NodeStatus {
    Complete,
    Partial,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TypeStatus {
    Valid(TypeId),
    Partial(TypeId),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct AltRange {
    pub start: usize,
    pub len: usize,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenRef {
    pub start: u32,
    pub end: u32,
    pub complete: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ChildRef {
    Node(NodeId),
    Terminal(TokenRef),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PackedAlt {
    pub prod: ProdId,
    pub children: Vec<ChildRef>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ArenaNode {
    pub nt: NtId,
    pub span: Span,
    pub status: NodeStatus,
    pub ty: TypeStatus,
    pub env_in: CtxId,
    pub env_out: CtxId,
    pub bindings: Vec<BindingValue>,
    pub alts: AltRange,
}

#[derive(Debug, Default)]
pub struct ParseArena {
    nodes: RefCell<Vec<ArenaNode>>,
    alts: RefCell<Vec<PackedAlt>>,
    paths: RefCell<Vec<PathNode>>,
    path_index: RefCell<HashMap<(Option<usize>, u16, u16), PathId>>,
}

pub struct Walk {
    steps: Vec<PathStep>,
    idx: usize,
}

impl Iterator for Walk {
    type Item = PathStep;

    fn next(&mut self) -> Option<Self::Item> {
        let step = self.steps.get(self.idx).copied()?;
        self.idx += 1;
        Some(step)
    }
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
            paths: RefCell::new(self.paths.borrow().clone()),
            path_index: RefCell::new(self.path_index.borrow().clone()),
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
        let id = NodeId(self.nodes.borrow().len());
        self.nodes.borrow_mut().push(node);
        id
    }

    /// Time: O(1) amortized. Space: O(1) additional.
    pub fn push_path(&self, parent: Option<PathId>, child: u16, alt: u16) -> PathId {
        let key = (parent.map(|id| id.0), child, alt);
        if let Some(id) = self.path_index.borrow().get(&key).copied() {
            return id;
        }
        let id = PathId(self.paths.borrow().len());
        self.paths.borrow_mut().push(PathNode {
            parent,
            step: PathStep { child, alt },
        });
        self.path_index.borrow_mut().insert(key, id);
        id
    }

    /// Time: O(1). Space: O(1).
    pub fn path(&self, id: PathId) -> Option<std::cell::Ref<'_, PathNode>> {
        std::cell::Ref::filter_map(self.paths.borrow(), |paths| paths.get(id.0)).ok()
    }

    /// Time: O(d), where d is the depth of the path.
    /// Space: O(d).
    pub fn path_steps(&self, id: PathId) -> Vec<PathStep> {
        let mut steps = Vec::new();
        let mut current = Some(id);
        while let Some(path_id) = current {
            let paths = self.paths.borrow();
            let Some(node) = paths.get(path_id.0) else {
                break;
            };
            steps.push(node.step);
            current = node.parent;
            drop(paths);
        }
        steps.reverse();
        steps
    }

    pub fn walk(&self, id: PathId) -> Walk {
        Walk {
            steps: self.path_steps(id),
            idx: 0,
        }
    }

    pub fn descends_from(&self, mut child: PathId, ancestor: PathId) -> bool {
        if child == ancestor {
            return true;
        }
        loop {
            match self.path(child).and_then(|p| p.parent) {
                Some(parent) if parent == ancestor => return true,
                Some(parent) => child = parent,
                None => return false,
            }
        }
    }

    pub fn subtree_bindings(
        &self,
        bindings: &[BindingValue],
        ancestor: PathId,
    ) -> Vec<BindingValue> {
        bindings
            .iter()
            .filter(|binding| self.descends_from(binding.path, ancestor))
            .cloned()
            .collect()
    }

    /// Time: O(1). Space: O(1).
    pub fn node(&self, id: NodeId) -> Option<std::cell::Ref<'_, ArenaNode>> {
        std::cell::Ref::filter_map(self.nodes.borrow(), |nodes| nodes.get(id.0)).ok()
    }

    /// Time: O(1). Space: O(1).
    pub fn alts_for(&self, id: NodeId) -> Option<std::cell::Ref<'_, [PackedAlt]>> {
        let nodes = self.nodes.borrow();
        let node = nodes.get(id.0)?;
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
}
