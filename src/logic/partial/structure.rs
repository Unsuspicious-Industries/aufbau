use crate::logic::grammar::{Grammar, Production};
use crate::logic::segment::SegmentRange;
use crate::logic::typing::Type;
use crate::regex::Regex as DerivativeRegex;
use std::collections::{HashMap, HashSet};
use std::sync::{Arc, Mutex, OnceLock};

pub type SppfNodeId = usize;

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum SppfChild {
    Node(SppfNodeId),
    Terminal(Terminal),
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub struct PackedAlternative {
    pub alternative_index: usize,
    pub children: Vec<SppfChild>,
}

#[derive(Clone, Debug)]
pub struct SppfNode {
    pub name: String,
    pub grammar: String,
    pub binding: Option<String>,
    pub abs_pos: usize,
    pub consumed_segments: usize,
    pub alternatives: Vec<PackedAlternative>,
    pub ty: Option<Type>,
}

#[derive(Clone, Debug)]
pub struct SppfForest {
    roots: Vec<SppfNodeId>,
    grammar_name: String,
    input: String,
}

impl Default for SppfForest {
    fn default() -> Self {
        Self {
            roots: Vec::new(),
            grammar_name: String::new(),
            input: String::new(),
        }
    }
}

// -- global store: nodes by grammar name --

struct GlobalStore {
    nodes: HashMap<String, Vec<SppfNode>>,
    grammars: HashMap<String, Grammar>,
}

fn global_store() -> &'static Mutex<GlobalStore> {
    static STORE: OnceLock<Mutex<GlobalStore>> = OnceLock::new();
    STORE.get_or_init(|| {
        Mutex::new(GlobalStore {
            nodes: HashMap::new(),
            grammars: HashMap::new(),
        })
    })
}

pub fn register_node(grammar_name: &str, node: SppfNode) -> SppfNodeId {
    let mut store = global_store().lock().expect("global store poisoned");
    let pool = store.nodes.entry(grammar_name.to_string()).or_default();
    let id = pool.len();
    pool.push(node);
    id
}

pub fn get_node(grammar_name: &str, id: SppfNodeId) -> Option<SppfNode> {
    let store = global_store().lock().expect("global store poisoned");
    store.nodes.get(grammar_name)?.get(id).cloned()
}

pub fn register_grammar(key: String, grammar: Grammar) {
    let mut store = global_store().lock().expect("global store poisoned");
    store.grammars.entry(key).or_insert(grammar);
}

impl SppfForest {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_forest(forest: SppfForest, roots: Vec<SppfNodeId>, input: String) -> Self {
        Self {
            roots,
            grammar_name: forest.grammar_name,
            input,
        }
    }

    pub fn set_roots(&mut self, roots: Vec<SppfNodeId>) {
        self.roots = roots;
    }

    pub fn set_grammar_name(&mut self, name: String) {
        self.grammar_name = name;
    }

    pub fn set_input(&mut self, input: String) {
        self.input = input;
    }

    pub fn grammar_name(&self) -> &str {
        &self.grammar_name
    }

    pub fn input(&self) -> &str {
        &self.input
    }

    pub fn root_ids(&self) -> &[SppfNodeId] {
        self.roots.as_slice()
    }

    pub fn roots(&self) -> Vec<NonTerminal> {
        self.materialize_roots(&self.roots.clone())
    }

    pub fn complete(&self) -> Option<NonTerminal> {
        for root_id in &self.roots {
            if self.node_is_complete(*root_id) {
                let trees = self.materialize_root(*root_id);
                if let Some(t) = trees.into_iter().find(|t| t.is_complete()) {
                    return Some(t);
                }
            }
        }
        None
    }

    pub fn is_complete(&self) -> bool {
        self.roots.iter().any(|id| self.node_is_complete(*id))
    }

    pub fn completes(&self) -> Vec<NonTerminal> {
        self.roots()
            .into_iter()
            .filter(|r| r.is_complete())
            .collect()
    }

    pub fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }

    pub fn intern_node(&mut self, node: SppfNode) -> SppfNodeId {
        register_node(&self.grammar_name, node)
    }

    pub fn node(&self, id: SppfNodeId) -> Option<SppfNode> {
        get_node(&self.grammar_name, id)
    }

    pub fn from_trees(roots: Vec<NonTerminal>, input: String, grammar: &Grammar) -> Self {
        register_grammar(grammar.name.clone(), grammar.clone());
        let root_ids: Vec<SppfNodeId> = roots
            .into_iter()
            .map(|r| register_nt_tree(&grammar.name, r))
            .collect();
        Self {
            roots: root_ids,
            grammar_name: grammar.name.clone(),
            input,
        }
    }

    pub fn set_node_type(&mut self, node_id: SppfNodeId, ty: Option<Type>) {
        let mut store = global_store().lock().expect("store poisoned");
        if let Some(pool) = store.nodes.get_mut(&self.grammar_name) {
            if let Some(node) = pool.get_mut(node_id) {
                node.ty = ty;
            }
        }
    }

    pub fn node_type(&self, node_id: SppfNodeId) -> Option<Type> {
        let store = global_store().lock().expect("store poisoned");
        store
            .nodes
            .get(&self.grammar_name)
            .and_then(|pool| pool.get(node_id))
            .and_then(|n| n.ty.clone())
    }

    pub fn nodes(&self) -> Vec<SppfNode> {
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&self.grammar_name) else {
            return Vec::new();
        };
        self.collect_reachable_ids(pool)
            .into_iter()
            .filter_map(|id| pool.get(id).cloned())
            .collect()
    }

    pub fn add_alt(&mut self, node_id: SppfNodeId, alt: PackedAlternative) {
        let mut store = global_store().lock().expect("store poisoned");
        if let Some(pool) = store.nodes.get_mut(&self.grammar_name) {
            if let Some(node) = pool.get_mut(node_id) {
                if !node.alternatives.contains(&alt) {
                    node.alternatives.push(alt);
                }
            }
        }
    }

    pub fn add_alternative(&mut self, node_id: SppfNodeId, alt: PackedAlternative) {
        self.add_alt(node_id, alt);
    }

    pub fn consumed_segments(&self, node_id: SppfNodeId) -> usize {
        let store = global_store().lock().expect("store poisoned");
        store
            .nodes
            .get(&self.grammar_name)
            .and_then(|pool| pool.get(node_id))
            .map(|n| n.consumed_segments)
            .unwrap_or(0)
    }

    pub fn node_is_complete(&self, node_id: SppfNodeId) -> bool {
        fn rec(nodes: &[SppfNode], id: SppfNodeId, seen: &mut HashSet<SppfNodeId>) -> bool {
            if !seen.insert(id) {
                return false;
            }
            let complete = nodes.get(id).is_some_and(|node| {
                node.alternatives.iter().any(|alt| {
                    alt.children.iter().all(|c| match c {
                        SppfChild::Terminal(Terminal::Complete { .. }) => true,
                        SppfChild::Terminal(Terminal::Partial { .. }) => false,
                        SppfChild::Node(child_id) => rec(nodes, *child_id, seen),
                    })
                })
            });
            seen.remove(&id);
            complete
        }

        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&self.grammar_name) else {
            return false;
        };
        rec(pool.as_slice(), node_id, &mut HashSet::new())
    }

    pub fn node_count(&self) -> usize {
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&self.grammar_name) else {
            return 0;
        };
        self.collect_reachable_ids(pool).len()
    }

    pub fn total_alternatives(&self) -> usize {
        self.nodes().iter().map(|n| n.alternatives.len()).sum()
    }

    pub fn max_alternatives(&self) -> usize {
        self.nodes()
            .iter()
            .map(|n| n.alternatives.len())
            .max()
            .unwrap_or(0)
    }

    pub fn merge_from(&mut self, other: &SppfForest) -> HashMap<SppfNodeId, SppfNodeId> {
        // Approximation: just extend roots, identity map.
        // Same-grammar forests share the same node pool so IDs are compatible.
        let mut id_map = HashMap::new();
        for &id in &other.roots {
            id_map.insert(id, id);
        }
        self.roots.extend(other.roots.iter().copied());
        id_map
    }

    fn collect_reachable_ids(&self, nodes: &[SppfNode]) -> Vec<SppfNodeId> {
        fn visit(nodes: &[SppfNode], id: SppfNodeId, seen: &mut HashSet<SppfNodeId>) {
            if !seen.insert(id) {
                return;
            }
            let Some(node) = nodes.get(id) else {
                return;
            };
            for alt in &node.alternatives {
                for child in &alt.children {
                    if let SppfChild::Node(child_id) = child {
                        visit(nodes, *child_id, seen);
                    }
                }
            }
        }

        let mut seen = HashSet::new();
        for root_id in &self.roots {
            visit(nodes, *root_id, &mut seen);
        }
        seen.into_iter().collect()
    }

    pub fn materialize_roots(&self, root_ids: &[SppfNodeId]) -> Vec<NonTerminal> {
        let mut memo: HashMap<SppfNodeId, Vec<NonTerminal>> = HashMap::new();
        let mut seen: HashSet<SppfNodeId> = HashSet::new();
        let mut out = Vec::new();
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&self.grammar_name) else {
            return out;
        };
        for root_id in root_ids {
            out.extend(self.materialize_node(pool, *root_id, &mut memo, &mut seen));
        }
        out
    }

    pub fn materialize_root(&self, root_id: SppfNodeId) -> Vec<NonTerminal> {
        let mut memo: HashMap<SppfNodeId, Vec<NonTerminal>> = HashMap::new();
        let mut seen: HashSet<SppfNodeId> = HashSet::new();
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&self.grammar_name) else {
            return Vec::new();
        };
        self.materialize_node(pool, root_id, &mut memo, &mut seen)
    }

    fn materialize_node(
        &self,
        nodes: &[SppfNode],
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

        let Some(node) = nodes.get(node_id) else {
            seen.remove(&node_id);
            return Vec::new();
        };

        let g = {
            let store = global_store().lock().expect("store poisoned");
            store.grammars.get(&self.grammar_name).unwrap().clone()
        };

        let mut trees = Vec::new();
        for packed in &node.alternatives {
            let child_sequences = self.materialize_children(nodes, &packed.children, memo, seen);
            for children in child_sequences {
                trees.push(NonTerminal::new(
                    node.name.clone(),
                    Arc::new(
                        g.productions.get(node.name.as_str()).unwrap()[packed.alternative_index]
                            .clone(),
                    ),
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
        nodes: &[SppfNode],
        children: &[SppfChild],
        memo: &mut HashMap<SppfNodeId, Vec<NonTerminal>>,
        seen: &mut HashSet<SppfNodeId>,
    ) -> Vec<Vec<Node>> {
        let mut sequences: Vec<Vec<Node>> = vec![Vec::new()];

        for child in children {
            let choices: Vec<Node> = match child {
                SppfChild::Terminal(t) => vec![Node::Terminal(t.clone())],
                SppfChild::Node(id) => self
                    .materialize_node(nodes, *id, memo, seen)
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

fn register_nt_tree(grammar_name: &str, nt: NonTerminal) -> SppfNodeId {
    let children: Vec<SppfChild> = nt
        .children
        .into_iter()
        .map(|c| match c {
            Node::Terminal(t) => SppfChild::Terminal(t),
            Node::NonTerminal(child_nt) => {
                SppfChild::Node(register_nt_tree(grammar_name, child_nt))
            }
        })
        .collect();
    register_node(
        grammar_name,
        SppfNode {
            name: nt.name,
            grammar: grammar_name.to_string(),
            binding: nt.binding,
            abs_pos: 0,
            consumed_segments: nt.consumed_segments,
            alternatives: vec![PackedAlternative {
                alternative_index: nt.alternative_index,
                children,
            }],
            ty: None,
        },
    )
}

// representing a specific choice of production
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

    pub fn height(&self) -> usize {
        if self.children.is_empty() {
            1
        } else {
            1 + self.children.iter().map(|c| c.height()).max().unwrap_or(0)
        }
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

    pub fn height(&self) -> usize {
        match self {
            Node::NonTerminal(nt) => nt.height(),
            Node::Terminal(_) => 1,
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
