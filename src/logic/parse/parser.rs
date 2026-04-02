use crate::debug_trace;
use crate::logic::grammar::{Grammar, Production, Segment, Symbol};
use crate::regex::PrefixStatus;
use std::collections::HashSet;

const EOF_FRONTIER_SLACK: u16 = 2;

fn depth_increment_for(production: &Production) -> u16 {
    if production.rule.is_none() && production.rhs.len() == 1 {
        0
    } else {
        1
    }
}

use crate::logic::fusion::ast::{FusionAST, FusionForest};
use crate::logic::fusion::{
    BindingValue, DepthMeta, FrontierItem, TransitionError, TypedPrefixError, TypedPrefixState,
    TypingContextSummary, TypingRuntime, TypingState,
};

use crate::logic::parse::arena::{
    ArenaNode, ChildRef, CtxId, FrontierId, NodeId, NodeStatus, NtId, PackedAlt, ParseArena,
    PathId, ProdId, Span, TokenRef, TypeId, TypeStatus,
};

#[derive(Debug)]
pub struct TypedParser<T> {
    grammar: Grammar,
    typing: T,
    arena: ParseArena,
    frontier: Vec<FrontierItem>,
    prod_offsets: Vec<usize>,
    left_components: Vec<Vec<NtId>>,
    self_left_recursive: Vec<bool>,
    max_depth: u16,
    depth_failures: u32,
}

#[derive(Clone, Debug)]
pub(crate) struct Branch {
    pub children: Vec<ChildRef>,
    pub states: Vec<TypingState>,
    pub end: usize,
    pub status: NodeStatus,
}

impl<T> TypedParser<T>
where
    T: TypingRuntime,
{
    #[track_caller]
    pub fn fork(&self) -> Self
    where
        T: Clone,
    {
        Self {
            grammar: self.grammar.clone(),
            typing: self.typing.clone(),
            arena: self.arena.snapshot(),
            frontier: self.frontier.clone(),
            prod_offsets: self.prod_offsets.clone(),
            left_components: self.left_components.clone(),
            self_left_recursive: self.self_left_recursive.clone(),
            max_depth: self.max_depth,
            depth_failures: self.depth_failures,
        }
    }

    pub fn materialize(
        &self,
        roots: &[NodeId],
        segments: Vec<Segment>,
        input: String,
    ) -> FusionAST {
        debug_trace!(
            "fusion_memory",
            "typed_parser_materialize roots={} segments={} input_len={}",
            roots.len(),
            segments.len(),
            input.len()
        );
        FusionAST::new(self.arena.snapshot(), segments, roots.to_vec(), input)
    }

    pub fn forest<'a>(
        &'a self,
        roots: &'a [NodeId],
        segments: &'a [Segment],
        input: &'a str,
    ) -> FusionForest<'a> {
        FusionForest::new(self.arena(), segments, roots, input)
    }

    fn alt_is_complete(&self, children: &[ChildRef]) -> bool {
        children.iter().all(|ch| match ch {
            ChildRef::Terminal(t) => t.complete,
            ChildRef::Node(id) => self
                .arena
                .node(*id)
                .is_some_and(|n| matches!(n.status, NodeStatus::Complete)),
        })
    }

    // A seed is an already-parsed typed node reused as the leftmost child of a
    // larger production. Left-recursive growth anchors child 0 to that seed and
    // parses only the suffix after it.
    pub(crate) fn seed_child_state(
        &mut self,
        prod: ProdId,
        alt_idx: usize,
        state: &TypingState,
        symbol: &Symbol,
        seed: ArenaNode,
        segments: &[Segment],
    ) -> Result<Option<TypingState>, TypedPrefixError> {
        let path = self.arena.push_path(state.path, 0, alt_idx as u16);
        let prep = match self.typing.prepare_child(
            prod,
            0,
            symbol.binding().map(String::as_str),
            state,
            &[],
        ) {
            Ok(state) => state,
            Err(TransitionError::Rejected) => return Ok(None),
            Err(TransitionError::TooDeep) => {
                self.depth_failures += 1;
                return Ok(None);
            }
        };
        let descended = match self
            .typing
            .descend(&prep, path, symbol.binding().map(String::as_str))
        {
            Ok(state) => state,
            Err(TransitionError::Rejected) => return Ok(None),
            Err(TransitionError::TooDeep) => {
                self.depth_failures += 1;
                return Ok(None);
            }
        };
        Ok(Some(self.typing.finish_node_child(
            &descended,
            path,
            &seed,
            &seed.bindings,
            segments,
        )))
    }

    pub(crate) fn finalize_node(
        &mut self,
        nt: NtId,
        alt_idx: usize,
        state: &TypingState,
        input_idx: usize,
        branches: Vec<Branch>,
    ) -> Result<Option<NodeId>, TypedPrefixError> {
        let mut packed = Vec::new();
        let mut chosen: Option<(u32, bool, NodeStatus, TypeStatus, CtxId, Vec<BindingValue>)> =
            None;
        let mut span_end = input_idx as u32;

        for branch in branches {
            match self.typing.finish_production(
                self.prod_id(nt, alt_idx),
                state,
                &branch.states,
                branch.status,
            ) {
                Ok(out) => {
                    let inferred = match out.inferred {
                        Some(inferred) => inferred,
                        None if matches!(branch.status, NodeStatus::Partial) => TypeId(0),
                        None => continue,
                    };
                    let rhs_len = self
                        .prod(self.prod_id(nt, alt_idx))
                        .map(|prod| prod.rhs.len())
                        .unwrap_or(branch.children.len());
                    let status = if matches!(branch.status, NodeStatus::Complete)
                        && (branch.children.len() != rhs_len
                            || !self.alt_is_complete(&branch.children))
                    {
                        NodeStatus::Partial
                    } else {
                        branch.status
                    };
                    let end = branch.end as u32;
                    span_end = span_end.max(end);
                    if chosen.as_ref().is_none_or(|(best_end, best_complete, ..)| {
                        end > *best_end
                            || (end == *best_end
                                && matches!(status, NodeStatus::Complete)
                                && !best_complete)
                    }) {
                        chosen = Some((
                            end,
                            matches!(status, NodeStatus::Complete),
                            status,
                            match status {
                                NodeStatus::Complete => TypeStatus::Valid(inferred),
                                NodeStatus::Partial => TypeStatus::Partial(inferred),
                            },
                            out.ctx,
                            out.bindings.clone(),
                        ));
                    }
                    packed.push(PackedAlt {
                        prod: self.prod_id(nt, alt_idx),
                        children: branch.children,
                    });
                }
                Err(TransitionError::TooDeep) => self.depth_failures += 1,
                Err(TransitionError::Rejected) => {}
            }
        }

        let Some((_, _, status, ty, env_out, bindings)) = chosen else {
            return Ok(None);
        };

        Ok(Some(self.arena.push_node(
            ArenaNode {
                nt,
                span: Span {
                    start: input_idx as u32,
                    end: span_end,
                },
                status,
                ty,
                env_in: state.ctx,
                env_out,
                bindings,
                alts: crate::logic::parse::arena::AltRange { start: 0, len: 0 },
            },
            packed,
        )))
    }

    /// Time: O(P), where P is the number of productions in the grammar.
    /// Space: O(P).
    pub fn new(grammar: Grammar, typing: T) -> Self {
        let mut prod_offsets = Vec::new();
        let mut next = 0usize;
        for idx in 0..grammar.production_count() {
            let len = grammar
                .productions_by_idx(idx)
                .map(|ps| ps.len())
                .unwrap_or(0);
            prod_offsets.push(next);
            next += len;
        }
        let left_edges: Vec<Vec<usize>> = (0..grammar.production_count())
            .map(|nt_idx| {
                grammar
                    .productions_by_idx(nt_idx)
                    .into_iter()
                    .flat_map(|productions| productions.iter())
                    .filter_map(|production| match production.rhs.first() {
                        Some(Symbol::Nonterminal { name, .. }) => grammar.nt_index(name),
                        _ => None,
                    })
                    .collect()
            })
            .collect();
        let reverse_left_edges: Vec<Vec<usize>> = (0..grammar.production_count())
            .map(|target| {
                left_edges
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, edges)| edges.contains(&target).then_some(idx))
                    .collect()
            })
            .collect();
        let self_left_recursive: Vec<bool> = (0..grammar.production_count())
            .map(|nt| reverse_left_edges[nt].contains(&nt))
            .collect();
        let left_components: Vec<Vec<NtId>> = (0..grammar.production_count())
            .map(|start| {
                let mut forward = HashSet::new();
                let mut stack = vec![start];
                while let Some(idx) = stack.pop() {
                    if !forward.insert(idx) {
                        continue;
                    }
                    for &next in &left_edges[idx] {
                        stack.push(next);
                    }
                }

                let mut backward = HashSet::new();
                let mut stack = vec![start];
                while let Some(idx) = stack.pop() {
                    if !backward.insert(idx) {
                        continue;
                    }
                    for &prev in &reverse_left_edges[idx] {
                        stack.push(prev);
                    }
                }

                forward.intersection(&backward).copied().map(NtId).collect()
            })
            .collect();
        Self {
            grammar,
            typing,
            arena: ParseArena::new(),
            frontier: Vec::new(),
            prod_offsets,
            left_components,
            self_left_recursive,
            max_depth: 32,
            depth_failures: 0,
        }
    }

    /// Time: O(1). Space: O(1).
    pub fn with_max_depth(mut self, max_depth: u16) -> Self {
        self.max_depth = max_depth;
        self
    }

    /// Time: O(1). Space: O(1).
    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    /// Time: O(1). Space: O(1).
    pub(crate) fn arena(&self) -> &ParseArena {
        &self.arena
    }

    /// Time: O(1). Space: O(1).
    pub(crate) fn arena_mut(&mut self) -> &mut ParseArena {
        &mut self.arena
    }

    /// Time: O(1). Space: O(1).
    pub(crate) fn typing(&self) -> &T {
        &self.typing
    }

    #[cfg(test)]
    pub(crate) fn frontier(&self) -> &[FrontierItem] {
        &self.frontier
    }

    /// Time: O(1). Space: O(1).
    pub fn seed_state(&self, input_len: usize, ctx: CtxId) -> TypedPrefixState {
        let _ = self.start_states(ctx);
        TypedPrefixState {
            input_len,
            roots: Vec::new(),
            frontier: Vec::<FrontierId>::new(),
            depth: DepthMeta::default(),
        }
    }

    /// Time: O(B), where B is the number of explored parse branches.
    /// Space: O(B) arena and recursion state.
    pub fn parse(&mut self, input: &str, ctx: CtxId) -> Result<TypedPrefixState, TypedPrefixError> {
        self.arena = ParseArena::new();
        self.frontier.clear();
        self.depth_failures = 0;

        let segments = self
            .grammar
            .tokenize(input)
            .map_err(|err| TypedPrefixError::rejected(input.len(), 0, err))?;
        let start = self
            .grammar
            .start_nonterminal()
            .and_then(|nt| self.grammar.nt_index(nt))
            .map(NtId)
            .ok_or_else(|| TypedPrefixError::rejected(input.len(), 0, "missing start symbol"))?;

        let mut roots = Vec::new();
        for state in self.start_states(ctx) {
            roots.extend(self.parse_nonterminal(start, &segments, 0, 0, state)?);
        }

        roots.retain(|node_id| {
            self.arena.node(*node_id).is_some_and(|node| {
                let at_end = node.span.end as usize == segments.len();
                at_end
                    && (node.status == NodeStatus::Complete || node.status == NodeStatus::Partial)
            })
        });

        // When multiple surviving complete roots cover the same prefix, prefer the ones
        // that progressed furthest within their top-level production (i.e. have
        // the most children in their first packed alternative). This prunes
        // shallow complete parses, but we must retain partial roots because
        // incremental extension depends on them remaining available.
        //
        // HEURISTIC: do NOT collapse ambiguity for partial-only parses, since
        // constrained generation/completion benefits from retaining multiple
        // viable partial roots (otherwise completions can get "stuck" on one
        // syntactic choice like forcing '(').
        if roots.len() > 1
            && roots.iter().all(|id| {
                self.arena
                    .node(*id)
                    .is_some_and(|n| matches!(n.status, NodeStatus::Complete))
            })
        {
            let mut best = 0usize;
            for &id in &roots {
                let c = self
                    .arena
                    .alts_for(id)
                    .and_then(|alts| alts.first().map(|a| a.children.len()))
                    .unwrap_or(0);
                best = best.max(c);
            }
            roots.retain(|&id| {
                self.arena
                    .alts_for(id)
                    .and_then(|alts| alts.first().map(|a| a.children.len()))
                    .unwrap_or(0)
                    == best
            });
            // If ambiguity remains, pick a deterministic representative.
            roots.sort_by_key(|id| id.0);
            roots.truncate(1);
        }

        if roots.is_empty() {
            return if self.depth_failures > 0 {
                Err(TypedPrefixError::too_deep(
                    input.len(),
                    self.max_depth,
                    self.depth_failures,
                ))
            } else {
                Err(TypedPrefixError::rejected(
                    input.len(),
                    self.max_depth,
                    "no typed branches survived",
                ))
            };
        }

        Ok(TypedPrefixState {
            input_len: input.len(),
            roots,
            frontier: Vec::new(),
            depth: DepthMeta {
                searched_depth: self.max_depth,
                hit_depth_limit: self.depth_failures > 0,
                depth_failures: self.depth_failures,
            },
        })
    }

    /// Incremental advance: reuse arena, only extend existing roots.
    ///
    /// Invariant: advance can only REDUCE or EXTEND roots. New roots are
    /// never created — they must be extensions of existing partial parses.
    ///
    /// Time: O(new_tokens × root_count × frontier_depth). Space: O(new_nodes).
    pub fn advance(
        &mut self,
        prev: &TypedPrefixState,
        input: &str,
        ctx: CtxId,
    ) -> Result<TypedPrefixState, TypedPrefixError> {
        super::advance::incremental(self, prev, input, ctx)
    }

    fn start_states(&self, ctx: CtxId) -> Vec<TypingState> {
        self.typing.enter_nonterminal(
            self.grammar
                .start_nonterminal()
                .and_then(|nt| self.grammar.nt_index(nt))
                .map(NtId)
                .unwrap_or(NtId(0)),
            &TypingContextSummary {
                ctx,
                expected: None,
                path: None,
            },
        )
    }

    fn prod_id(&self, nt: NtId, alt: usize) -> ProdId {
        ProdId(self.prod_offsets.get(nt.0).copied().unwrap_or(0) + alt)
    }

    pub(crate) fn prod(&self, id: ProdId) -> Option<&Production> {
        let nt = self
            .prod_offsets
            .partition_point(|&offset| offset <= id.0)
            .saturating_sub(1);
        let base = self.prod_offsets.get(nt).copied()?;
        self.grammar.productions_by_idx(nt)?.get(id.0 - base)
    }

    pub(crate) fn alt(&self, id: ProdId) -> usize {
        let nt = self
            .prod_offsets
            .partition_point(|&offset| offset <= id.0)
            .saturating_sub(1);
        id.0 - self.prod_offsets.get(nt).copied().unwrap_or(0)
    }

    fn parse_nonterminal(
        &mut self,
        nt: NtId,
        segments: &[Segment],
        input_idx: usize,
        depth: u16,
        state: TypingState,
    ) -> Result<Vec<NodeId>, TypedPrefixError> {
        let at_eof = input_idx >= segments.len();
        let depth_exhausted = if at_eof {
            depth > self.max_depth.saturating_add(EOF_FRONTIER_SLACK)
        } else {
            depth >= self.max_depth
        };
        if depth_exhausted {
            self.depth_failures += 1;
            debug_trace!(
                "fusion_parser",
                "depth_limit nt={} input_idx={} depth={} max_depth={}",
                self.grammar.nt_name(nt.0).unwrap_or("<?>"),
                input_idx,
                depth,
                self.max_depth
            );
            return Ok(Vec::new());
        }

        let component = self.left_component(nt);
        if component.len() > 1 || self.has_self_left_recursion(nt) {
            return self.parse_component(&component, nt, segments, input_idx, depth, state);
        }

        let Some(productions) = self.grammar.productions_by_idx(nt.0).cloned() else {
            return Ok(Vec::new());
        };

        let mut nodes = Vec::new();
        let mut seen = HashSet::new();
        let nt_name = self.grammar.nt_name(nt.0).unwrap_or("").to_string();
        let mut recursive = Vec::new();
        for (alt_idx, production) in productions.iter().enumerate() {
            if matches!(
                production.rhs.first(),
                Some(Symbol::Nonterminal { name, .. }) if name == &nt_name
            ) {
                recursive.push((alt_idx, production.clone()));
                continue;
            }
            if let Some(node_id) = self.parse_production(
                nt,
                alt_idx,
                production,
                segments,
                input_idx,
                depth + depth_increment_for(production),
                &state,
            )? {
                if let Some(node) = self.arena.node(node_id) {
                    seen.insert((node.span.start, node.span.end, alt_idx));
                }
                nodes.push(node_id)
            }
        }

        let mut changed = true;
        while changed && !recursive.is_empty() {
            changed = false;
            let seeds = nodes.clone();
            for (alt_idx, production) in &recursive {
                for seed in &seeds {
                    if let Some(node_id) = self.parse_recursive_with_seed(
                        nt,
                        *alt_idx,
                        production,
                        *seed,
                        segments,
                        depth + 1,
                        &state,
                    )? {
                        let Some(node) = self.arena.node(node_id) else {
                            continue;
                        };
                        let sig = (node.span.start, node.span.end, *alt_idx);
                        if !seen.contains(&sig) {
                            seen.insert(sig);
                            nodes.push(node_id);
                            changed = true;
                        }
                    }
                }
            }
        }

        Ok(nodes)
    }

    fn parse_component(
        &mut self,
        component: &[NtId],
        target: NtId,
        segments: &[Segment],
        input_idx: usize,
        depth: u16,
        state: TypingState,
    ) -> Result<Vec<NodeId>, TypedPrefixError> {
        let component_set = component.iter().map(|nt| nt.0).collect::<HashSet<_>>();
        let mut results = component
            .iter()
            .copied()
            .map(|nt| (nt, Vec::<NodeId>::new()))
            .collect::<std::collections::HashMap<_, _>>();
        let mut seen = component
            .iter()
            .copied()
            .map(|nt| (nt, HashSet::<(u32, u32, usize)>::new()))
            .collect::<std::collections::HashMap<_, _>>();

        for &nt in component {
            let Some(productions) = self.grammar.productions_by_idx(nt.0).cloned() else {
                continue;
            };
            for (alt_idx, production) in productions.iter().enumerate() {
                let recursive = matches!(
                    production.rhs.first(),
                    Some(Symbol::Nonterminal { name, .. })
                        if self.grammar.nt_index(name).is_some_and(|idx| component_set.contains(&idx))
                );
                if recursive {
                    continue;
                }
                if let Some(node_id) = self.parse_production(
                    nt,
                    alt_idx,
                    production,
                    segments,
                    input_idx,
                    depth + depth_increment_for(production),
                    &state,
                )? && let Some(node) = self.arena.node(node_id)
                {
                    let sig = (node.span.start, node.span.end, alt_idx);
                    if seen.get_mut(&nt).unwrap().insert(sig) {
                        results.get_mut(&nt).unwrap().push(node_id);
                    }
                }
            }
        }

        let mut changed = true;
        while changed {
            changed = false;
            for &nt in component {
                let Some(productions) = self.grammar.productions_by_idx(nt.0).cloned() else {
                    continue;
                };
                for (alt_idx, production) in productions.iter().enumerate() {
                    let Some(Symbol::Nonterminal { name, .. }) = production.rhs.first() else {
                        continue;
                    };
                    let Some(first_idx) = self.grammar.nt_index(name) else {
                        continue;
                    };
                    if !component_set.contains(&first_idx) {
                        continue;
                    }
                    let seeds = results.get(&NtId(first_idx)).cloned().unwrap_or_default();
                    for seed in seeds {
                        if let Some(node_id) = self.parse_recursive_with_seed(
                            nt,
                            alt_idx,
                            production,
                            seed,
                            segments,
                            depth + 1,
                            &state,
                        )? && let Some(node) = self.arena.node(node_id)
                        {
                            let sig = (node.span.start, node.span.end, alt_idx);
                            if seen.get_mut(&nt).unwrap().insert(sig) {
                                results.get_mut(&nt).unwrap().push(node_id);
                                changed = true;
                            }
                        }
                    }
                }
            }
        }

        Ok(results.remove(&target).unwrap_or_default())
    }

    #[allow(clippy::too_many_arguments)]
    // Grow a production from an existing typed seed node. The seed must match
    // the production's first child; the parser then parses only the remaining
    // suffix and wraps the result back into the parent node.
    pub(crate) fn parse_recursive_with_seed(
        &mut self,
        nt: NtId,
        alt_idx: usize,
        production: &Production,
        seed_id: NodeId,
        segments: &[Segment],
        depth: u16,
        state: &TypingState,
    ) -> Result<Option<NodeId>, TypedPrefixError> {
        let seed = {
            let Some(node_ref) = self.arena.node(seed_id) else {
                return Ok(None);
            };
            node_ref.clone()
        };
        let Some(first) = production.rhs.first() else {
            return Ok(None);
        };
        let span = seed.span;
        let Some(first_state) = self.seed_child_state(
            self.prod_id(nt, alt_idx),
            alt_idx,
            state,
            first,
            seed,
            segments,
        )?
        else {
            return Ok(None);
        };
        let branches = self.parse_symbols(
            &production.rhs[1..],
            segments,
            span.end as usize,
            depth,
            first_state.clone(),
            self.prod_id(nt, alt_idx),
            vec![first_state.clone()],
            state.path,
            alt_idx as u16,
            1,
        )?;
        let mut with_seed = Vec::with_capacity(branches.len());
        for b in branches {
            let mut children = Vec::with_capacity(1 + b.children.len());
            children.push(ChildRef::Node(seed_id));
            children.extend(b.children);

            let mut states = Vec::with_capacity(1 + b.states.len());
            states.push(first_state.clone());
            states.extend(b.states);

            let status = if matches!(b.status, NodeStatus::Partial) {
                NodeStatus::Partial
            } else {
                NodeStatus::Complete
            };
            with_seed.push(Branch {
                children,
                states,
                end: b.end,
                status,
            });
        }
        self.finish_branches(nt, alt_idx, state, span.start as usize, with_seed)
    }

    pub(crate) fn has_self_left_recursion(&self, nt: NtId) -> bool {
        self.self_left_recursive.get(nt.0).copied().unwrap_or(false)
    }

    pub(crate) fn left_component(&self, start: NtId) -> Vec<NtId> {
        self.left_components
            .get(start.0)
            .cloned()
            .unwrap_or_default()
    }

    fn finish_branches(
        &mut self,
        nt: NtId,
        alt_idx: usize,
        state: &TypingState,
        input_idx: usize,
        branches: Vec<Branch>,
    ) -> Result<Option<NodeId>, TypedPrefixError> {
        self.finalize_node(nt, alt_idx, state, input_idx, branches)
    }

    #[allow(clippy::too_many_arguments)]
    fn parse_production(
        &mut self,
        nt: NtId,
        alt_idx: usize,
        production: &Production,
        segments: &[Segment],
        input_idx: usize,
        depth: u16,
        state: &TypingState,
    ) -> Result<Option<NodeId>, TypedPrefixError> {
        let branches = self.parse_symbols(
            &production.rhs,
            segments,
            input_idx,
            depth,
            state.clone(),
            self.prod_id(nt, alt_idx),
            Vec::new(),
            state.path,
            alt_idx as u16,
            0,
        )?;
        self.finish_branches(nt, alt_idx, state, input_idx, branches)
    }

    #[allow(clippy::too_many_arguments)]
    fn parse_symbols(
        &mut self,
        symbols: &[Symbol],
        segments: &[Segment],
        input_idx: usize,
        depth: u16,
        state: TypingState,
        prod: ProdId,
        parsed_prefix: Vec<TypingState>,
        parent_path: Option<PathId>,
        alt_idx: u16,
        child_idx: u16,
    ) -> Result<Vec<Branch>, TypedPrefixError> {
        if symbols.is_empty() {
            return Ok(vec![Branch {
                children: Vec::new(),
                states: Vec::new(),
                end: input_idx,
                status: NodeStatus::Complete,
            }]);
        }

        let first = &symbols[0];
        let rest = &symbols[1..];
        let state = match self.typing.prepare_child(
            prod,
            child_idx as usize,
            first.binding().map(String::as_str),
            &state,
            &parsed_prefix,
        ) {
            Ok(state) => state,
            Err(TransitionError::Rejected) => return Ok(Vec::new()),
            Err(TransitionError::TooDeep) => {
                self.depth_failures += 1;
                return Ok(Vec::new());
            }
        };

        let first_branches = self.parse_symbol(
            first,
            segments,
            input_idx,
            depth,
            state,
            parent_path,
            alt_idx,
            child_idx,
        )?;
        let mut out = Vec::new();

        for first_branch in first_branches {
            if first_branch.status == NodeStatus::Partial || rest.is_empty() {
                out.push(first_branch);
                continue;
            }
            let Some(next_state) = first_branch.states.last().cloned() else {
                continue;
            };
            let mut next_prefix = parsed_prefix.clone();
            next_prefix.extend(first_branch.states.clone());
            let rest_branches = self.parse_symbols(
                rest,
                segments,
                first_branch.end,
                depth,
                next_state,
                prod,
                next_prefix,
                parent_path,
                alt_idx,
                child_idx + 1,
            )?;
            for rest_branch in rest_branches {
                let mut children = first_branch.children.clone();
                children.extend(rest_branch.children);
                let mut states = first_branch.states.clone();
                states.extend(rest_branch.states);
                let status = if first_branch.status == NodeStatus::Partial
                    || rest_branch.status == NodeStatus::Partial
                {
                    NodeStatus::Partial
                } else {
                    NodeStatus::Complete
                };
                out.push(Branch {
                    children,
                    states,
                    end: rest_branch.end,
                    status,
                });
            }
        }

        Ok(out)
    }

    #[allow(clippy::too_many_arguments)]
    fn parse_symbol(
        &mut self,
        symbol: &Symbol,
        segments: &[Segment],
        input_idx: usize,
        depth: u16,
        state: TypingState,
        parent_path: Option<PathId>,
        alt_idx: u16,
        child_idx: u16,
    ) -> Result<Vec<Branch>, TypedPrefixError> {
        let path = self.arena.push_path(parent_path, child_idx, alt_idx);
        let state = match self
            .typing
            .descend(&state, path, symbol.binding().map(String::as_str))
        {
            Ok(state) => state,
            Err(TransitionError::Rejected) => return Ok(Vec::new()),
            Err(TransitionError::TooDeep) => {
                self.depth_failures += 1;
                return Ok(Vec::new());
            }
        };

        match symbol {
            Symbol::Terminal { regex, .. } => {
                self.parse_terminal(regex, segments, input_idx, state)
            }
            Symbol::Nonterminal { name, .. } => {
                let Some(nt_idx) = self.grammar.nt_index(name) else {
                    return Ok(Vec::new());
                };
                let nodes = self.parse_nonterminal(
                    NtId(nt_idx),
                    segments,
                    input_idx,
                    depth,
                    state.clone(),
                )?;
                let mut out = Vec::new();
                for node_id in nodes {
                    let node = {
                        let Some(node_ref) = self.arena.node(node_id) else {
                            continue;
                        };
                        node_ref.clone()
                    };
                    out.push(Branch {
                        children: vec![ChildRef::Node(node_id)],
                        states: vec![self.typing.finish_node_child(
                            &state,
                            path,
                            &node,
                            &node.bindings,
                            segments,
                        )],
                        end: node.span.end as usize,
                        status: node.status,
                    });
                }
                Ok(out)
            }
        }
    }

    fn parse_terminal(
        &mut self,
        regex: &crate::regex::Regex,
        segments: &[Segment],
        input_idx: usize,
        state: TypingState,
    ) -> Result<Vec<Branch>, TypedPrefixError> {
        let segment = segments.get(input_idx);
        let next = self.typing.finish_terminal_child(
            &state,
            state.path.unwrap_or(PathId(0)),
            regex,
            segment,
        );
        let Ok(mut next) = next else {
            if matches!(next, Err(TransitionError::TooDeep)) {
                self.depth_failures += 1;
            }
            return Ok(Vec::new());
        };
        // For missing terminals (segment=None), ensure we can still carry a typed
        // state forward for prefix parsing even if the runtime hasn't inferred
        // anything yet.
        if next.inferred.is_none() {
            next.inferred = Some(TypeId(0));
        }

        let (end, status, child) = match segment {
            Some(segment) => match regex.prefix_match(segment.as_str()) {
                PrefixStatus::NoMatch => return Ok(Vec::new()),
                PrefixStatus::Complete | PrefixStatus::Extensible(_) => (
                    input_idx + 1,
                    NodeStatus::Complete,
                    ChildRef::Terminal(TokenRef {
                        start: segment.index as u32,
                        end: segment.index as u32 + 1,
                        complete: true,
                    }),
                ),
                // IMPORTANT:
                // Segment-based parsing treats each `Segment` as an atomic token. A terminal
                // regex that only *prefix-matches* a segment (e.g. "+." against "+") should
                // NOT be allowed to consume that segment as a "partial terminal", because
                // we cannot extend a token across segment boundaries.
                //
                // Allowing this creates bogus partial parses that "cover" a complete input
                // using wrong terminals, making valid complete strings appear incomplete.
                PrefixStatus::Prefix(_) => return Ok(Vec::new()),
            },
            None => (
                input_idx,
                NodeStatus::Partial,
                ChildRef::Terminal(TokenRef {
                    start: input_idx as u32,
                    end: input_idx as u32,
                    complete: false,
                }),
            ),
        };

        Ok(vec![Branch {
            children: vec![child],
            states: vec![next],
            end,
            status,
        }])
    }
}
