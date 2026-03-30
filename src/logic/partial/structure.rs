use crate::logic::grammar::{Grammar, Production, Symbol};
use crate::logic::partial::stats::{GlobalCacheStats, GrammarCacheStats, InputCacheEntry};
use crate::logic::segment::SegmentRange;
use crate::logic::typing::Type;
use crate::regex::Regex as DerivativeRegex;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};
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
    grammar: Option<Grammar>,
    input: String,
}

impl Default for SppfForest {
    fn default() -> Self {
        Self {
            roots: Vec::new(),
            grammar_name: String::new(),
            grammar: None,
            input: String::new(),
        }
    }
}

// -- global store: nodes by grammar name --

struct GlobalStore {
    // Grammar name -> node pool (id -> node)
    nodes: HashMap<String, Vec<SppfNode>>,
    // Grammar name -> grammar
    grammars: HashMap<String, Grammar>,
    // (Grammar name, input, nt) -> node id
    icache: HashMap<(String, String, String), SppfNodeId>,
}

fn global_store() -> &'static Mutex<GlobalStore> {
    static STORE: OnceLock<Mutex<GlobalStore>> = OnceLock::new();
    STORE.get_or_init(|| {
        Mutex::new(GlobalStore {
            nodes: HashMap::new(),
            grammars: HashMap::new(),
            icache: HashMap::new(),
        })
    })
}

pub(crate) fn grammar_store_key(grammar: &Grammar) -> String {
    if !grammar.name.is_empty() {
        return grammar.name.clone();
    }

    let mut hasher = std::collections::hash_map::DefaultHasher::new();
    grammar.hash(&mut hasher);
    format!("anon:{:016x}", hasher.finish())
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

pub fn get_cached_node(grammar_name: &str, input: &str, nt_name: &str) -> Option<SppfNodeId> {
    let store = global_store().lock().expect("global store poisoned");
    store
        .icache
        .get(&(
            grammar_name.to_string(),
            input.to_string(),
            nt_name.to_string(),
        ))
        .copied()
}

fn cache_full_node(grammar_name: &str, input: &str, nt_name: &str, node_id: SppfNodeId) {
    let mut store = global_store().lock().expect("global store poisoned");
    store.icache.insert(
        (
            grammar_name.to_string(),
            input.to_string(),
            nt_name.to_string(),
        ),
        node_id,
    );
}

pub fn global_cache_stats() -> GlobalCacheStats {
    let store = global_store().lock().expect("global store poisoned");
    let per_grammar = grammar_cache_stats_locked(&store);
    let total_nodes = per_grammar.iter().map(|stats| stats.node_count).sum();
    let unique_nodes = per_grammar.iter().map(|stats| stats.unique_nodes).sum();

    GlobalCacheStats {
        grammar_count: store.grammars.len(),
        node_pool_count: store.nodes.len(),
        total_nodes,
        unique_nodes,
        duplicate_nodes: total_nodes.saturating_sub(unique_nodes),
        input_cache_entries: store.icache.len(),
    }
}

pub fn grammar_cache_stats() -> Vec<GrammarCacheStats> {
    let store = global_store().lock().expect("global store poisoned");
    grammar_cache_stats_locked(&store)
}

pub fn input_cache_entries() -> Vec<InputCacheEntry> {
    let store = global_store().lock().expect("global store poisoned");
    store
        .icache
        .iter()
        .map(|((grammar, input, nonterminal), node_id)| InputCacheEntry {
            grammar: grammar.clone(),
            input: input.clone(),
            nonterminal: nonterminal.clone(),
            node_id: *node_id,
        })
        .collect()
}

pub fn reset_global_store() {
    let mut store = global_store().lock().expect("global store poisoned");
    store.nodes.clear();
    store.grammars.clear();
    store.icache.clear();
}

fn grammar_cache_stats_locked(store: &GlobalStore) -> Vec<GrammarCacheStats> {
    store
        .nodes
        .iter()
        .map(|(grammar, pool)| {
            let mut seen = HashSet::new();
            let unique_nodes = pool
                .iter()
                .filter(|node| seen.insert(node_fingerprint(node)))
                .count();

            GrammarCacheStats {
                grammar: grammar.clone(),
                node_count: pool.len(),
                unique_nodes,
                duplicate_nodes: pool.len().saturating_sub(unique_nodes),
            }
        })
        .collect()
}

fn node_fingerprint(node: &SppfNode) -> String {
    format!(
        "{}|{}|{:?}|{}|{}|{:?}",
        node.name,
        node.grammar,
        node.binding,
        node.abs_pos,
        node.consumed_segments,
        node.alternatives
    )
}

impl SppfForest {
    fn production_for(
        grammar: &Grammar,
        node_name: &str,
        alternative_index: usize,
        arity: usize,
    ) -> Production {
        grammar
            .productions
            .get(node_name)
            .and_then(|alts| alts.get(alternative_index))
            .cloned()
            .unwrap_or_else(|| Production {
                rule: None,
                rhs: (0..arity)
                    .map(|_| Symbol::Nonterminal {
                        name: "_".to_string(),
                        binding: None,
                    })
                    .collect(),
            })
    }

    pub fn new() -> Self {
        Self::default()
    }

    pub fn from_forest(forest: SppfForest, roots: Vec<SppfNodeId>, input: String) -> Self {
        Self {
            roots,
            grammar_name: forest.grammar_name,
            grammar: forest.grammar,
            input,
        }
    }

    pub fn set_roots(&mut self, roots: Vec<SppfNodeId>) {
        self.roots = roots;
    }

    pub fn set_grammar(&mut self, grammar: Grammar) {
        self.grammar_name = grammar_store_key(&grammar);
        self.grammar = Some(grammar);
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
        self.has_complete()
    }

    /// At least one complete derivation exists (may also have partials)
    pub fn has_complete(&self) -> bool {
        self.roots.iter().any(|id| self.node_is_complete(*id))
    }

    /// All derivations are fully complete (no partial nodes at all)
    pub fn is_full(&self) -> bool {
        if self.roots.is_empty() {
            return false;
        }
        !self.has_partial()
    }

    /// Has at least one partial derivation (contains incomplete terminals)
    pub fn has_partial(&self) -> bool {
        for root_id in &self.roots {
            if self.node_has_partial(*root_id) {
                return true;
            }
        }
        false
    }

    /// Node has at least one partial derivation
    pub fn node_has_partial(&self, node_id: SppfNodeId) -> bool {
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&self.grammar_name) else {
            return false;
        };
        let Some(node) = pool.get(node_id) else {
            return false;
        };

        for alt in &node.alternatives {
            for child in &alt.children {
                match child {
                    SppfChild::Terminal(Terminal::Partial { .. }) => return true,
                    SppfChild::Terminal(Terminal::Complete { .. }) => continue,
                    SppfChild::Node(child_id) => {
                        if self.node_has_partial_in_store(*child_id, &store) {
                            return true;
                        }
                    }
                }
            }
        }
        false
    }

    fn node_has_partial_in_store(&self, node_id: SppfNodeId, store: &GlobalStore) -> bool {
        let Some(pool) = store.nodes.get(&self.grammar_name) else {
            return false;
        };
        let Some(node) = pool.get(node_id) else {
            return false;
        };

        for alt in &node.alternatives {
            for child in &alt.children {
                match child {
                    SppfChild::Terminal(Terminal::Partial { .. }) => return true,
                    SppfChild::Terminal(Terminal::Complete { .. }) => continue,
                    SppfChild::Node(child_id) => {
                        if self.node_has_partial_in_store(*child_id, store) {
                            return true;
                        }
                    }
                }
            }
        }
        false
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

    pub fn node(&self, id: SppfNodeId) -> Option<SppfNode> {
        get_node(&self.grammar_name, id)
    }

    pub fn from_trees(roots: Vec<NonTerminal>, input: String, grammar: Grammar) -> Self {
        let grammar_key = grammar_store_key(&grammar);
        register_grammar(grammar_key.clone(), grammar.clone());

        // Find start nt from first root
        let start_nt = roots.first().map(|r| r.name.clone()).unwrap_or_default();

        // Check cache first
        if let Some(cached_id) = get_cached_node(&grammar_key, &input, &start_nt) {
            return Self {
                roots: vec![cached_id],
                grammar_name: grammar_key.clone(),
                grammar: Some(grammar),
                input,
            };
        }

        let root_ids: Vec<SppfNodeId> = roots
            .into_iter()
            .map(|r| register_nt_tree(&grammar.name, r))
            .collect();

        // If we have exactly one root and it's full, cache it
        if root_ids.len() == 1 {
            let root_id = root_ids[0];
            let store = global_store().lock().expect("store poisoned");
            if let Some(pool) = store.nodes.get(&grammar.name) {
                if let Some(node) = pool.get(root_id) {
                    let is_full = node.alternatives.iter().all(|alt| {
                        alt.children.iter().all(|c| match c {
                            SppfChild::Terminal(Terminal::Complete { .. }) => true,
                            _ => false,
                        })
                    });
                    if is_full {
                        drop(store);
                        cache_full_node(&grammar_key, &input, &start_nt, root_id);
                    }
                }
            }
        }

        Self {
            roots: root_ids,
            grammar_name: grammar_key,
            grammar: Some(grammar),
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
        self.node_has_complete(node_id)
    }

    /// Node has at least one complete derivation
    pub fn node_has_complete(&self, node_id: SppfNodeId) -> bool {
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

    /// Node is fully complete (all alternatives complete, no partials)
    pub fn node_is_full(&self, node_id: SppfNodeId) -> bool {
        !self.node_has_partial(node_id)
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
        if self.grammar.is_none() {
            self.grammar = other.grammar.clone();
        }
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
        let grammar = match &self.grammar {
            Some(g) => g.clone(),
            None => {
                let store = global_store().lock().expect("store poisoned");
                match store.grammars.get(&self.grammar_name) {
                    Some(g) => g.clone(),
                    None => return out,
                }
            }
        };
        let grammar_name = if !self.grammar_name.is_empty() {
            self.grammar_name.clone()
        } else {
            grammar_store_key(&grammar)
        };
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&grammar_name) else {
            return out;
        };
        for root_id in root_ids {
            out.extend(self.materialize_node(pool, &grammar, *root_id, &mut memo, &mut seen));
        }
        out
    }

    pub fn materialize_root(&self, root_id: SppfNodeId) -> Vec<NonTerminal> {
        let mut memo: HashMap<SppfNodeId, Vec<NonTerminal>> = HashMap::new();
        let mut seen: HashSet<SppfNodeId> = HashSet::new();

        // Try cache first - if cached, use it directly (it's full)
        if let Some(g) = &self.grammar {
            if !self.input.is_empty() {
                // Get node name from root_id for cache lookup
                let store = global_store().lock().expect("store poisoned");
                let pool = store.nodes.get(&self.grammar_name);
                if let Some(pool) = pool {
                    if let Some(node) = pool.get(root_id) {
                        let nt_name = node.name.clone();
                        drop(store); // Release lock before cache lookup
                        if let Some(cached_id) =
                            get_cached_node(&self.grammar_name, &self.input, &nt_name)
                        {
                            if let Some(cached_root) = self.materialize_cached_root(cached_id, &g) {
                                return vec![cached_root];
                            }
                        }
                    }
                }
            }
        }

        let grammar = match &self.grammar {
            Some(g) => g.clone(),
            None => {
                let store = global_store().lock().expect("store poisoned");
                match store.grammars.get(&self.grammar_name) {
                    Some(g) => g.clone(),
                    None => return Vec::new(),
                }
            }
        };
        let grammar_name = if !self.grammar_name.is_empty() {
            self.grammar_name.clone()
        } else {
            grammar_store_key(&grammar)
        };
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&grammar_name) else {
            return Vec::new();
        };
        self.materialize_node(pool, &grammar, root_id, &mut memo, &mut seen)
    }

    fn materialize_cached_root(
        &self,
        node_id: SppfNodeId,
        grammar: &Grammar,
    ) -> Option<NonTerminal> {
        // For cached full nodes, just use materialize_node directly
        // Cache guarantees the node is full (complete, no partials)
        let mut memo: HashMap<SppfNodeId, Vec<NonTerminal>> = HashMap::new();
        let mut seen: HashSet<SppfNodeId> = HashSet::new();

        let store = global_store().lock().expect("store poisoned");
        let pool = store.nodes.get(&self.grammar_name)?;
        let trees = self.materialize_node(pool, grammar, node_id, &mut memo, &mut seen);

        // For cached nodes, we expect exactly one full tree
        trees.into_iter().next()
    }

    pub fn for_each_materialized_root<F>(&self, root_id: SppfNodeId, mut visit: F)
    where
        F: FnMut(NonTerminal) -> bool,
    {
        let grammar = match &self.grammar {
            Some(g) => g.clone(),
            None => {
                let store = global_store().lock().expect("store poisoned");
                match store.grammars.get(&self.grammar_name) {
                    Some(g) => g.clone(),
                    None => return,
                }
            }
        };
        let grammar_name = if !self.grammar_name.is_empty() {
            self.grammar_name.clone()
        } else {
            grammar_store_key(&grammar)
        };
        let store = global_store().lock().expect("store poisoned");
        let Some(pool) = store.nodes.get(&grammar_name) else {
            return;
        };
        let mut seen: HashSet<SppfNodeId> = HashSet::new();
        self.materialize_node_each(pool, &grammar, root_id, &mut seen, &mut visit);
    }

    fn materialize_node(
        &self,
        nodes: &[SppfNode],
        grammar: &Grammar,
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

        let mut trees = Vec::new();
        for packed in &node.alternatives {
            let prod = Self::production_for(
                grammar,
                node.name.as_str(),
                packed.alternative_index,
                packed.children.len(),
            );
            let child_sequences =
                self.materialize_children(nodes, grammar, &packed.children, memo, seen);
            for children in child_sequences {
                trees.push(NonTerminal::new(
                    node.name.clone(),
                    Arc::new(prod.clone()),
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
        grammar: &Grammar,
        children: &[SppfChild],
        memo: &mut HashMap<SppfNodeId, Vec<NonTerminal>>,
        seen: &mut HashSet<SppfNodeId>,
    ) -> Vec<Vec<Node>> {
        let mut sequences: Vec<Vec<Node>> = vec![Vec::new()];

        for child in children {
            let choices: Vec<Vec<Node>> = match child {
                SppfChild::Terminal(t) => vec![vec![Node::Terminal(t.clone())]],
                SppfChild::Node(id) => self
                    .materialize_node(nodes, grammar, *id, memo, seen)
                    .into_iter()
                    .map(|nt| self.materialized_nodes_for(grammar, nt))
                    .collect(),
            };

            if choices.is_empty() {
                return Vec::new();
            }

            let mut next = Vec::new();
            for base in &sequences {
                for choice in &choices {
                    let mut seq = base.clone();
                    seq.extend(choice.clone());
                    next.push(seq);
                }
            }
            sequences = next;
        }

        sequences
    }

    fn materialize_node_each(
        &self,
        nodes: &[SppfNode],
        grammar: &Grammar,
        node_id: SppfNodeId,
        seen: &mut HashSet<SppfNodeId>,
        visit: &mut dyn FnMut(NonTerminal) -> bool,
    ) -> bool {
        if !seen.insert(node_id) {
            return true;
        }

        let Some(node) = nodes.get(node_id) else {
            return true;
        };

        for packed in &node.alternatives {
            let prod = Self::production_for(
                grammar,
                node.name.as_str(),
                packed.alternative_index,
                packed.children.len(),
            );
            let mut emit_children = |children: Vec<Node>| {
                let nt = NonTerminal::new(
                    node.name.clone(),
                    Arc::new(prod.clone()),
                    packed.alternative_index,
                    children,
                    node.binding.clone(),
                    node.consumed_segments,
                );
                visit(nt)
            };
            if !self.materialize_children_each(
                nodes,
                grammar,
                &packed.children,
                0,
                &mut Vec::new(),
                seen,
                &mut emit_children,
            ) {
                return false;
            }
        }

        true
    }

    fn materialize_children_each(
        &self,
        nodes: &[SppfNode],
        grammar: &Grammar,
        children: &[SppfChild],
        index: usize,
        current: &mut Vec<Node>,
        seen: &mut HashSet<SppfNodeId>,
        emit: &mut dyn FnMut(Vec<Node>) -> bool,
    ) -> bool {
        if index == children.len() {
            return emit(current.clone());
        }

        match &children[index] {
            SppfChild::Terminal(t) => {
                current.push(Node::Terminal(t.clone()));
                let keep = self.materialize_children_each(
                    nodes,
                    grammar,
                    children,
                    index + 1,
                    current,
                    seen,
                    emit,
                );
                current.pop();
                keep
            }
            SppfChild::Node(id) => {
                let mut child_seen: HashSet<SppfNodeId> = HashSet::new();
                let mut emit_nt = |nt: NonTerminal| -> bool {
                    let flattened = self.materialized_nodes_for(grammar, nt);
                    let added = flattened.len();
                    current.extend(flattened);
                    let keep = self.materialize_children_each(
                        nodes,
                        grammar,
                        children,
                        index + 1,
                        current,
                        seen,
                        emit,
                    );
                    current.truncate(current.len().saturating_sub(added));
                    keep
                };
                if !self.materialize_node_each(nodes, grammar, *id, &mut child_seen, &mut emit_nt) {
                    return false;
                }
                true
            }
        }
    }

    fn materialized_nodes_for(&self, grammar: &Grammar, nt: NonTerminal) -> Vec<Node> {
        if grammar.is_hidden_nonterminal(&nt.name) {
            nt.children
        } else {
            vec![Node::NonTerminal(nt)]
        }
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

fn symbol_is_repetition_helper(symbol: &Symbol) -> bool {
    matches!(symbol, Symbol::Nonterminal { name, .. } if name.starts_with("__rep_"))
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
        if !self.is_variadic_repetition() && self.children.len() != self.production.rhs.len() {
            return false;
        }
        self.children.iter().all(|child| child.is_complete())
    }

    pub fn expected_children_len(&self) -> usize {
        if self.is_variadic_repetition() {
            self.children.len()
        } else {
            self.production.rhs.len()
        }
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

    fn is_variadic_repetition(&self) -> bool {
        self.production.rhs.iter().any(symbol_is_repetition_helper)
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
