use crate::logic::grammar::Production;
use crate::logic::segment::SegmentRange;
use crate::regex::Regex as DerivativeRegex;
use std::collections::{HashMap, HashSet};
use std::sync::Arc;

pub type SppfNodeId = usize;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SppfChild {
    Node(SppfNodeId),
    Terminal(Terminal),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PackedAlternative {
    pub alternative_index: usize,
    pub production: Arc<Production>,
    pub children: Vec<SppfChild>,
}

#[derive(Clone, Debug)]
pub struct SppfNode {
    pub name: String,
    pub binding: Option<String>,
    pub abs_pos: usize,
    pub consumed_segments: usize,
    pub alternatives: Vec<PackedAlternative>,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct SppfNodeKey {
    pub name: String,
    pub binding: Option<String>,
    pub abs_pos: usize,
    pub consumed_segments: usize,
}

#[derive(Clone, Debug, Default)]
pub struct SppfForest {
    nodes: Vec<SppfNode>,
    node_lookup: HashMap<SppfNodeKey, SppfNodeId>,
}

impl SppfForest {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern_node(&mut self, key: SppfNodeKey) -> SppfNodeId {
        if let Some(existing) = self.node_lookup.get(&key) {
            return *existing;
        }

        let id = self.nodes.len();
        self.nodes.push(SppfNode {
            name: key.name.clone(),
            binding: key.binding.clone(),
            abs_pos: key.abs_pos,
            consumed_segments: key.consumed_segments,
            alternatives: Vec::new(),
        });
        self.node_lookup.insert(key, id);
        id
    }

    pub fn add_alternative(&mut self, node_id: SppfNodeId, alt: PackedAlternative) {
        if let Some(node) = self.nodes.get_mut(node_id) {
            if !node.alternatives.contains(&alt) {
                node.alternatives.push(alt);
            }
        }
    }

    pub fn consumed_segments(&self, node_id: SppfNodeId) -> usize {
        self.nodes
            .get(node_id)
            .map(|n| n.consumed_segments)
            .unwrap_or(0)
    }

    pub fn node_is_complete(&self, node_id: SppfNodeId) -> bool {
        fn rec(
            forest: &SppfForest,
            id: SppfNodeId,
            memo: &mut HashMap<SppfNodeId, bool>,
            seen: &mut HashSet<SppfNodeId>,
        ) -> bool {
            if let Some(v) = memo.get(&id) {
                return *v;
            }
            if !seen.insert(id) {
                return false;
            }

            let complete = forest.nodes.get(id).is_some_and(|node| {
                node.alternatives.iter().any(|alt| {
                    alt.children.iter().all(|c| match c {
                        SppfChild::Terminal(Terminal::Complete { .. }) => true,
                        SppfChild::Terminal(Terminal::Partial { .. }) => false,
                        SppfChild::Node(child_id) => rec(forest, *child_id, memo, seen),
                    })
                })
            });

            seen.remove(&id);
            memo.insert(id, complete);
            complete
        }

        rec(self, node_id, &mut HashMap::new(), &mut HashSet::new())
    }

    pub fn merge_from(&mut self, other: &SppfForest) -> Vec<SppfNodeId> {
        let mut id_map = vec![0; other.nodes.len()];

        for (old_id, node) in other.nodes.iter().enumerate() {
            let key = SppfNodeKey {
                name: node.name.clone(),
                binding: node.binding.clone(),
                abs_pos: node.abs_pos,
                consumed_segments: node.consumed_segments,
            };
            id_map[old_id] = self.intern_node(key);
        }

        for (old_id, node) in other.nodes.iter().enumerate() {
            let new_id = id_map[old_id];
            for alt in &node.alternatives {
                let children = alt
                    .children
                    .iter()
                    .map(|c| match c {
                        SppfChild::Terminal(t) => SppfChild::Terminal(t.clone()),
                        SppfChild::Node(cid) => SppfChild::Node(id_map[*cid]),
                    })
                    .collect();
                self.add_alternative(
                    new_id,
                    PackedAlternative {
                        alternative_index: alt.alternative_index,
                        production: Arc::clone(&alt.production),
                        children,
                    },
                );
            }
        }

        id_map
    }

    pub fn materialize_roots(&self, root_ids: &[SppfNodeId]) -> Vec<NonTerminal> {
        let mut memo: HashMap<SppfNodeId, Vec<NonTerminal>> = HashMap::new();
        let mut seen: HashSet<SppfNodeId> = HashSet::new();
        let mut out = Vec::new();
        for root_id in root_ids {
            out.extend(self.materialize_node(*root_id, &mut memo, &mut seen));
        }
        out
    }

    pub fn materialize_root(&self, root_id: SppfNodeId) -> Vec<NonTerminal> {
        let mut memo: HashMap<SppfNodeId, Vec<NonTerminal>> = HashMap::new();
        let mut seen: HashSet<SppfNodeId> = HashSet::new();
        self.materialize_node(root_id, &mut memo, &mut seen)
    }

    fn materialize_node(
        &self,
        node_id: SppfNodeId,
        memo: &mut HashMap<SppfNodeId, Vec<NonTerminal>>,
        seen: &mut HashSet<SppfNodeId>,
    ) -> Vec<NonTerminal> {
        if let Some(v) = memo.get(&node_id) {
            return v.clone();
        }
        if !seen.insert(node_id) {
            return Vec::new();
        }

        let Some(node) = self.nodes.get(node_id) else {
            seen.remove(&node_id);
            return Vec::new();
        };

        let mut trees = Vec::new();
        for packed in &node.alternatives {
            let child_sequences = self.materialize_children(&packed.children, memo, seen);
            for children in child_sequences {
                trees.push(NonTerminal::new(
                    node.name.clone(),
                    Arc::clone(&packed.production),
                    packed.alternative_index,
                    children,
                    node.binding.clone(),
                    node.consumed_segments,
                ));
            }
        }

        seen.remove(&node_id);
        memo.insert(node_id, trees.clone());
        trees
    }

    fn materialize_children(
        &self,
        children: &[SppfChild],
        memo: &mut HashMap<SppfNodeId, Vec<NonTerminal>>,
        seen: &mut HashSet<SppfNodeId>,
    ) -> Vec<Vec<Node>> {
        let mut sequences: Vec<Vec<Node>> = vec![Vec::new()];

        for child in children {
            let choices: Vec<Node> = match child {
                SppfChild::Terminal(t) => vec![Node::Terminal(t.clone())],
                SppfChild::Node(id) => self
                    .materialize_node(*id, memo, seen)
                    .into_iter()
                    .map(Node::NonTerminal)
                    .collect(),
            };

            if choices.is_empty() {
                return Vec::new();
            }

            let mut next = Vec::new();
            for base in &sequences {
                for choice in &choices {
                    let mut seq = base.clone();
                    seq.push(choice.clone());
                    next.push(seq);
                }
            }
            sequences = next;
        }

        sequences
    }
}

#[derive(Clone, Debug)]
pub struct Sppf {
    forest: Arc<SppfForest>,
    root_ids: Vec<SppfNodeId>,
    pub input: String,
}

pub type PartialAST = Sppf;

impl Sppf {
    pub(crate) fn from_trees(roots: Vec<NonTerminal>, input: String) -> Self {
        let mut forest = SppfForest::new();
        let mut root_ids = Vec::new();

        for root in roots {
            root_ids.push(Self::insert_nt(&mut forest, root, 0));
        }

        Self {
            forest: Arc::new(forest),
            root_ids,
            input,
        }
    }

    pub fn from_forest(forest: SppfForest, root_ids: Vec<SppfNodeId>, input: String) -> Self {
        Self {
            forest: Arc::new(forest),
            root_ids,
            input,
        }
    }

    fn insert_nt(forest: &mut SppfForest, nt: NonTerminal, abs_pos: usize) -> SppfNodeId {
        let key = SppfNodeKey {
            name: nt.name.clone(),
            binding: nt.binding.clone(),
            abs_pos,
            consumed_segments: nt.consumed_segments,
        };
        let node_id = forest.intern_node(key);

        let mut offset = abs_pos;
        let mut packed_children = Vec::with_capacity(nt.children.len());
        for child in nt.children {
            match child {
                Node::Terminal(t) => {
                    if matches!(t, Terminal::Complete { .. } | Terminal::Partial { .. }) {
                        offset += 1;
                    }
                    packed_children.push(SppfChild::Terminal(t));
                }
                Node::NonTerminal(child_nt) => {
                    let consumed = child_nt.consumed_segments;
                    let child_id = Self::insert_nt(forest, child_nt, offset);
                    offset += consumed;
                    packed_children.push(SppfChild::Node(child_id));
                }
            }
        }

        forest.add_alternative(
            node_id,
            PackedAlternative {
                alternative_index: nt.alternative_index,
                production: Arc::clone(&nt.production),
                children: packed_children,
            },
        );

        node_id
    }

    pub fn roots(&self) -> Vec<NonTerminal> {
        self.forest.materialize_roots(self.root_ids.as_slice())
    }

    pub fn forest(&self) -> &SppfForest {
        self.forest.as_ref()
    }

    pub fn root_ids(&self) -> &[SppfNodeId] {
        self.root_ids.as_slice()
    }

    pub fn root_count(&self) -> usize {
        self.root_ids.len()
    }

    pub fn is_empty(&self) -> bool {
        self.root_ids.is_empty()
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn complete(&self) -> Option<NonTerminal> {
        self.roots().into_iter().find(|root| root.is_complete())
    }

    pub fn completes(&self) -> Vec<NonTerminal> {
        self.roots()
            .into_iter()
            .filter(|root| root.is_complete())
            .collect()
    }

    pub fn is_complete(&self) -> bool {
        self.root_ids
            .iter()
            .any(|root_id| self.forest.node_is_complete(*root_id))
    }
}

/// A nonterminal node representing a specific choice of production
#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct NonTerminal {
    /// Name of the nonterminal (e.g., "Expr", "start")
    pub name: String,
    /// The production rule used for this node
    pub production: Arc<Production>,
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
        production: impl Into<Arc<Production>>,
        alternative_index: usize,
        children: Vec<Node>,
        binding: Option<String>,
        consumed_segments: usize,
    ) -> Self {
        Self {
            name,
            production: production.into(),
            alternative_index,
            children,
            binding,
            consumed_segments,
        }
    }

    pub fn is_complete(&self) -> bool {
        if self.production.rhs.is_empty() {
            return true;
        }
        if self.children.len() != self.production.rhs.len() {
            return false;
        }
        self.children.iter().all(|child| child.is_complete())
    }

    pub fn is_extensible(&self) -> bool {
        if !self.is_complete() {
            return true;
        }
        match self.children.last() {
            Some(Node::NonTerminal(nt)) => nt.is_extensible(),
            Some(Node::Terminal(Terminal::Complete { extension: e, .. })) => e.is_some(),
            Some(Node::Terminal(Terminal::Partial { .. })) => true,
            None => false,
        }
    }

    pub fn frontier(&self) -> Option<usize> {
        if self.is_complete() {
            None
        } else {
            Some(self.children.len())
        }
    }

    pub fn size(&self) -> usize {
        self.children.iter().map(|c| c.size()).sum::<usize>() + 1
    }

    pub fn consumed_segments(&self) -> usize {
        self.consumed_segments
    }

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
        self.frontier_child_index() == Some(index)
    }

    pub fn frontier_child_index(&self) -> Option<usize> {
        if self.is_complete() {
            return None;
        }

        if self.children.len() < self.production.rhs.len() {
            return Some(self.children.len());
        }

        self.children.iter().rposition(|child| !child.is_complete())
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
            .map(|child| child.path_exists(&path[1..]))
            .unwrap_or(false)
    }

    pub fn text(&self) -> Option<String> {
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
            Node::Terminal(_) => 1,
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
