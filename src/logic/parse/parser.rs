use crate::debug_trace;
use crate::logic::grammar::{Grammar, Segment, Symbol};
use crate::regex::PrefixStatus;
use std::collections::{HashMap, HashSet, VecDeque};

use crate::logic::error::{PrefixError, TransitionError};
use crate::logic::structure::ast::{FusionAST, FusionForest};
use crate::logic::typing::{ContextTransition, Obligations, TypingRuntime};

use crate::logic::parse::arena::{
    ANY_TYPE, // identifier for the ANY type, default kind
    AltRange,
    ArenaNode,
    Binding,
    ChildRef,
    CtxId,
    Lexeme,
    NodeId,
    NodeStatus,
    NtId,
    PackedAlt,
    ParseArena,
    ProdId,
    Span,
    TypeId,
};

use super::TypedParser;

// ── Data structures ──────────────────────────────────────────────────────────

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Item {
    pub prod: ProdId,
    pub dot: usize,
    pub start: usize,
    pub pos: usize,
    pub ctx: CtxId,
    pub obligations: Obligations,
    pub children: Vec<ChildRef>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Completion {
    pub nt: NtId,
    pub start: usize,
    pub end: usize,
    pub node: NodeId,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Waiter {
    pub item: Item,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Task {
    Process(Item),
    Complete(Completion),
}

#[derive(Clone, Debug, Default)]
pub struct Tables {
    pub agenda: VecDeque<Task>,

    // Dedup with: prod, dot, start, pos, ctx
    pub seen_process: HashSet<(ProdId, usize, usize, usize, CtxId)>,
    pub results: HashMap<(NtId, usize), Vec<usize>>,
    pub completed_nodes: HashMap<(NtId, usize, usize), Vec<NodeId>>,
    // (NT, start)
    pub waiters: HashMap<(NtId, usize), Vec<Waiter>>,
    pub frontier: Vec<Item>,
}

// inveriants and theoretical basis to the engine

// Items procesing
//
// > For every live item,
// > item.ctx is the effective context after applying the exported transforms
// > of all already-resumed closed children to the left.
//
// Obligation fr future nodes processing
// In order to save up useless stuff we state, unproven that:
// > Any two obligation stores,
// > compatible with the same process key are future-equivalent for the parser.
// --- Obligation Reconstructibility Hypothesis ---

// ── Engine ───────────────────────────────────────────────────────────────────

impl<T> TypedParser<T>
where
    T: TypingRuntime,
{
    pub(super) fn enqueue_process(&mut self, item: Item) {
        let key = (item.prod, item.dot, item.start, item.pos, item.ctx);
        if self.tables.seen_process.insert(key) {
            self.tables.agenda.push_back(Task::Process(item));
        }
    }

    pub(super) fn enqueue_complete(&mut self, completion: Completion) {
        self.tables.agenda.push_back(Task::Complete(completion));
    }

    pub(super) fn seed(
        &mut self,
        prods: &[ProdId],
        pos: usize,
        ctx: CtxId,
        parent_obs: &Obligations,
    ) {
        for &prod in prods {
            let parent_obligations = parent_obs.for_seed(prod.1);
            let mut obligations =
                Obligations::create(&self.grammar, prod, parent_obligations.root().clone());
            obligations.extend(parent_obligations);
            self.enqueue_process(Item {
                prod,
                dot: 0,
                start: pos,
                pos,
                ctx,
                obligations,
                children: Vec::new(),
            });
        }
    }

    /// Try to match a regex terminal against the current segment.
    ///
    /// Four cases:
    ///   - Past end of input -> item goes to frontier (waiting for more input).
    ///   - NoMatch -> item is dead, dropped.
    ///   - Prefix -> the segment starts a valid match but needs more characters.
    ///     At end of input this becomes a frontier lexeme that does not yet
    ///     satisfy the terminal symbol. Mid-input it's dead.
    ///   - Complete / Extensible -> full match, item advances normally.
    pub(super) fn consume(
        &mut self,
        item: &Item,
        regex: &crate::regex::Regex,
        symbol: &Symbol,
    ) -> Result<Option<Item>, PrefixError> {
        if item.pos >= self.segs().len() {
            #[cfg(test)]
            debug_trace!(
                "fusion_parser",
                "consume eof nt={} alt={} dot={} start={} pos={} -> frontier",
                self.grammar.nt(item.prod.0).unwrap_or("<?>"),
                item.prod.1,
                item.dot,
                item.start,
                item.pos
            );
            self.tables.frontier.push(item.clone());
            return Ok(None);
        }

        let Some(segment) = self.segs().get(item.pos).cloned() else {
            #[cfg(test)]
            debug_trace!(
                "fusion_parser",
                "consume missing-segment nt={} alt={} dot={} start={} pos={} -> frontier",
                self.grammar.nt(item.prod.0).unwrap_or("<?>"),
                item.prod.1,
                item.dot,
                item.start,
                item.pos
            );
            self.tables.frontier.push(item.clone());
            return Ok(None);
        };

        let status = regex.prefix_match(segment.as_str());
        let at_end = item.pos + 1 >= self.segs().len();
        let (complete, open) = match status {
            PrefixStatus::NoMatch => return Ok(None),
            PrefixStatus::Prefix(_) => {
                if !at_end {
                    return Ok(None);
                }
                (false, true)
            }
            PrefixStatus::Complete => (true, false),
            PrefixStatus::Extensible(_) => (true, at_end),
        };
        let span = Span {
            start: segment.index as u32,
            end: segment.index as u32 + 1,
        };
        let l = Lexeme {
            matched: span,
            complete,
            open,
        };

        #[cfg(test)]
        debug_trace!(
            "fusion_parser",
            "consume nt={} alt={} dot={} start={} pos={} seg='{}' status={:?} at_end={} -> complete={} open={}",
            self.grammar.nt(item.prod.0).unwrap_or("<?>"),
            item.prod.1,
            item.dot,
            item.start,
            item.pos,
            segment.as_str(),
            status,
            at_end,
            complete,
            open
        );

        let mut next = item.clone();
        if symbol.binding().is_some() {
            // Terminal bindings discharge only the obligations that point here.
            next.obligations.resolve_terminal(item.dot, item.prod.1, &l);
        }
        next.dot += 1;
        next.pos = item.pos + 1;
        next.children.push(ChildRef::Terminal(l));

        if complete {
            Ok(Some(next))
        } else {
            self.tables.frontier.push(next);
            Ok(None)
        }
    }

    pub(super) fn finish(&mut self, item: &Item) -> Result<Option<NodeId>, PrefixError> {
        let any_open = item.children.iter().any(|child| self.arena.open(child));
        let status = match (self.alt_is_complete(item), any_open) {
            (true, true) => NodeStatus::Extensible,
            (true, false) => NodeStatus::Closed,
            (false, _) => NodeStatus::Partial, // partial always at EOI
        };

        #[cfg(test)]
        debug_trace!(
            "fusion_parser",
            "finish nt={} alt={} dot={} span={}..{} status={:?} children={}",
            self.grammar.nt(item.prod.0).unwrap_or("<?>"),
            item.prod.1,
            item.dot,
            item.start,
            item.pos,
            status,
            item.children.len()
        );
        match self
            .typing
            .finalize(item.prod, item.ctx, &item.obligations, status)
        {
            Ok((mut ty, ctr, semantic_complete)) => {
                let nt_name = self.grammar.nt(item.prod.0).unwrap_or("");
                let has_rule = self.grammar.rule_for_prod(item.prod).is_some();
                // Invariant: inheritance is production-local. A structurally
                // transparent production has exactly one nonterminal child and
                // no bound terminals. Such a production contributes no local
                // semantic evidence beyond its child.
                let transparent_prod = self.grammar.prod(item.prod).is_some_and(|prod| {
                    let nonterminal_children = prod
                        .rhs
                        .iter()
                        .filter(|sym| {
                            matches!(sym, crate::logic::grammar::Symbol::Nonterminal { .. })
                        })
                        .count();
                    let bound_terminals = prod.rhs.iter().any(|sym| {
                        matches!(
                            sym,
                            crate::logic::grammar::Symbol::Terminal {
                                binding: Some(_),
                                ..
                            }
                        )
                    });
                    nonterminal_children == 1 && !bound_terminals
                });
                let bridge = self.grammar.is_bridge_nt(nt_name);
                let child_inheriting = transparent_prod && (bridge || !has_rule);
                let inherited_child = if child_inheriting {
                    let mut node_children = item.children.iter().filter_map(|child| match child {
                        ChildRef::Node(child_id) => self.arena.node(*child_id).map(|n| n.clone()),
                        ChildRef::Terminal(_) => None,
                    });
                    let first = node_children.next();
                    if node_children.next().is_none() {
                        first
                    } else {
                        None
                    }
                } else {
                    None
                };

                if status == NodeStatus::Partial && item.start == item.pos {
                    ty = ANY_TYPE;
                } else if let Some(child) = &inherited_child {
                    ty = child.ty;
                }

                let ctr = if status == NodeStatus::Closed {
                    if child_inheriting {
                        let mut composed_transforms = Vec::new();
                        for child in &item.children {
                            if let ChildRef::Node(child_id) = child
                                && let Some(child) = self.arena.node(*child_id)
                                && let Some(child_ctr) = &child.ctr
                            {
                                composed_transforms.extend(child_ctr.transforms.clone());
                            }
                        }
                        if let Some(local_ctr) = ctr {
                            composed_transforms.extend(local_ctr.transforms);
                        }
                        (!composed_transforms.is_empty()).then_some(ContextTransition {
                            transforms: composed_transforms,
                        })
                    } else {
                        ctr.filter(|ctr| !ctr.transforms.is_empty())
                    }
                } else {
                    None
                };
                let bindings: Vec<Binding> = item
                    .obligations
                    .iter()
                    .filter(|o| o.value.is_some() || o.actual.is_some())
                    .map(|o| o.to_binding())
                    .collect();
                let node = ArenaNode {
                    nt: item.prod.0,
                    span: Span {
                        start: item.start as u32,
                        end: item.pos as u32,
                    },
                    status,
                    semantic_complete,
                    ty,
                    ctr: ctr,
                    bindings,
                    alts: AltRange { start: 0, len: 0 },
                };
                let packed = vec![PackedAlt {
                    prod: item.prod,
                    children: item.children.clone(),
                }];
                #[cfg(test)]
                debug_trace!(
                    "fusion_parser",
                    "push node nt={} status={:?} ty_id={}",
                    nt_name,
                    status,
                    ty
                );
                Ok(Some(self.arena.push_node(node, packed)))
            }
            Err(TransitionError::Rejected) => {
                #[cfg(test)]
                debug_trace!(
                    "fusion_parser",
                    "finalize rejected nt={} alt={} status={:?}",
                    self.grammar.nt(item.prod.0).unwrap_or("<?>"),
                    item.prod.1,
                    status,
                );
                Ok(None)
            }
        }
    }

    // node id is a completions to parent
    fn resume(&mut self, parent: &Item, node_id: NodeId) -> Result<Item, PrefixError> {
        let node = self
            .arena
            .node(node_id)
            .ok_or(PrefixError::rejected(
                parent.pos,
                "missing node during resume",
            ))?
            .clone();
        let mut resumed = parent.clone();
        resumed
            .obligations
            .resolve_nonterminal(parent.dot, parent.prod.1, &node);
        resumed.dot += 1;
        resumed.pos = node.span.end as usize;
        resumed.children.push(ChildRef::Node(node_id));
        // apply the context transforms
        if let Some(ctr) = node.ctr.clone() {
            resumed.ctx = self.typing.apply_transform(resumed.ctx, ctr).map_err(|e| {
                PrefixError::rejected(
                    resumed.pos,
                    format!("context transition failed during resume: {:?}", e),
                )
            })?;
        }
        Ok(resumed)
    }

    pub(super) fn complete(&mut self, completion: Completion) -> Result<(), PrefixError> {
        #[cfg(test)]
        debug_trace!(
            "fusion_parser",
            "complete nt={} span={}..{} node={}",
            self.grammar.nt(completion.nt).unwrap_or("<?>"),
            completion.start,
            completion.end,
            completion.node
        );
        self.tables
            .completed_nodes
            .entry((completion.nt, completion.start, completion.end))
            .or_default()
            .push(completion.node);

        // Invariant: every semantically distinct completed node must be allowed
        // to wake waiters. Span-only completion dedup is too coarse because
        // resumption depends on child type/effect/bindings.
        let ends = self
            .tables
            .results
            .entry((completion.nt, completion.start))
            .or_default();
        if !ends.contains(&completion.end) {
            ends.push(completion.end);
        }

        if let Some(waiters) = self
            .tables
            .waiters
            .get(&(completion.nt, completion.start))
            .cloned()
        {
            for waiter in waiters {
                if let Ok(resumed) = self.resume(&waiter.item, completion.node) {
                    self.enqueue_process(resumed);
                }
            }
        }

        Ok(())
    }

    pub(super) fn process(&mut self, item: Item) -> Result<(), PrefixError> {
        let production = self
            .grammar
            .prod(item.prod)
            .ok_or_else(|| PrefixError::rejected(self.input.len(), "missing production"))?;

        if item.dot == production.rhs.len() {
            if let Some(node) = self.finish(&item)? {
                self.enqueue_complete(Completion {
                    nt: item.prod.0,
                    start: item.start,
                    end: item.pos,
                    node,
                });
            }
            return Ok(());
        }

        let symbol = &production.rhs[item.dot];
        match symbol {
            Symbol::Terminal { regex, .. } => {
                if let Some(next) = self.consume(&item, regex, symbol)? {
                    self.enqueue_process(next);
                }
                Ok(())
            }
            Symbol::Nonterminal { name, .. } => {
                let nt = self.grammar.nt_index(name).ok_or_else(|| {
                    PrefixError::rejected(
                        self.input.len(),
                        "missing nonterminal: ".to_string() + name,
                    )
                })?;
                let binding = symbol.binding().map(String::as_str);

                let child_ctx =
                    match self
                        .typing
                        .descend(item.prod, binding, item.ctx, &item.obligations)
                    {
                        Ok(ctx) => ctx,
                        Err(TransitionError::Rejected) => return Ok(()),
                    };

                self.tables
                    .waiters
                    .entry((nt, item.pos))
                    .or_default()
                    .push(Waiter { item: item.clone() });

                // Resume from already-completed children
                let ends = self
                    .tables
                    .results
                    .get(&(nt, item.pos))
                    .cloned()
                    .unwrap_or_default();
                for end in ends {
                    let nodes = self
                        .tables
                        .completed_nodes
                        .get(&(nt, item.pos, end))
                        .cloned()
                        .unwrap_or_default();
                    for node_id in nodes {
                        if let Ok(resumed) = self.resume(&item, node_id) {
                            self.enqueue_process(resumed);
                        }
                    }
                }

                // Seed child productions with stepped obligations for pruning
                let stepped = item.obligations.step(item.dot, item.prod.1);
                let prods = stepped.prune(nt, &self.grammar);
                self.seed(&prods, item.pos, child_ctx, &stepped.at_child(item.dot));
                Ok(())
            }
        }
    }

    /// Promote frontier items to partial nodes and propagate completions
    /// through the existing waiter network. Unlike the main Earley loop,
    /// this never predicts or consumes — it only finishes and resumes.
    ///
    /// Two levels of dedup ensure termination in recursive grammars:
    ///
    ///   1. **Item-level**: `(prod, dot, start, pos)` — prevents the same
    ///      item from being processed twice, cutting cycles like
    ///      `Expression → Choice → Expression` at the same span.
    ///
    ///   2. **Node-level**: `(nt, span, type)` — prevents propagating
    ///      type-identical nodes at the same span. Type-DISTINCT nodes
    ///      DO propagate (e.g. Integer vs partial-Float at position 2..3).
    fn lift_frontier(&mut self) -> Result<(), PrefixError> {
        // item-level dedup prevents re-processing the same frontier item, which could
        // prod, dot, start, pos, ctx
        let mut seen_items: HashSet<(ProdId, usize, usize, usize, CtxId)> = HashSet::new();
        // node-level dedup prevents re-propagating the same node as a completion, which could
        // nt, start, end, type
        let mut seen_nodes: HashSet<(NtId, u32, u32, TypeId)> = HashSet::new();
        let mut queue: VecDeque<Item> = self.tables.frontier.drain(..).collect();

        while let Some(item) = queue.pop_front() {
            #[cfg(test)]
            debug_trace!(
                "fusion_parser",
                "frontier pop nt={} alt={} dot={} span={}..{} children={}",
                self.grammar.nt(item.prod.0).unwrap_or("<?>"),
                item.prod.1,
                item.dot,
                item.start,
                item.pos,
                item.children.len()
            );
            if !seen_items.insert((item.prod, item.dot, item.start, item.pos, item.ctx)) {
                #[cfg(test)]
                debug_trace!(
                    "fusion_parser",
                    "frontier skip-item nt={} alt={} dot={} span={}..{}",
                    self.grammar.nt(item.prod.0).unwrap_or("<?>"),
                    item.prod.1,
                    item.dot,
                    item.start,
                    item.pos
                );
                continue;
            }

            let Some(node_id) = self.finish(&item)? else {
                continue;
            };
            let Some(node) = self.arena.node(node_id).map(|n| n.clone()) else {
                continue;
            };

            if !seen_nodes.insert((node.nt, node.span.start, node.span.end, node.ty)) {
                #[cfg(test)]
                debug_trace!(
                    "fusion_parser",
                    "frontier skip-node nt={} span={}..{} ty={}",
                    self.grammar.nt(node.nt).unwrap_or("<?>"),
                    node.span.start,
                    node.span.end,
                    node.ty
                );
                continue;
            }

            #[cfg(test)]
            debug_trace!(
                "fusion_parser",
                "frontier accept-node nt={} span={}..{} ty={} node={}",
                self.grammar.nt(node.nt).unwrap_or("<?>"),
                node.span.start,
                node.span.end,
                node.ty,
                node_id
            );

            self.tables
                .completed_nodes
                .entry((node.nt, node.span.start as usize, node.span.end as usize))
                .or_default()
                .push(node_id);
            let ends = self
                .tables
                .results
                .entry((node.nt, node.span.start as usize))
                .or_default();
            if !ends.contains(&(node.span.end as usize)) {
                ends.push(node.span.end as usize);
            }

            let waiters = self
                .tables
                .waiters
                .get(&(node.nt, node.span.start as usize))
                .cloned()
                .unwrap_or_default();
            for waiter in waiters {
                if let Ok(resumed) = self.resume(&waiter.item, node_id) {
                    queue.push_back(resumed);
                }
            }
        }
        Ok(())
    }

    fn alt_is_complete(&self, item: &Item) -> bool {
        let Some(production) = self.grammar.prod(item.prod) else {
            return false;
        };
        if item.children.len() != production.rhs.len() {
            return false;
        }
        item.children.iter().all(|child| self.arena.complete(child))
    }

    #[track_caller]
    pub fn fork(&self) -> Self
    where
        T: Clone,
    {
        Self {
            grammar: self.grammar.clone(),
            typing: self.typing.clone(),
            arena: self.arena.snapshot(),
            tables: self.tables.clone(),
            input: self.input.clone(),
            segments: self.segments.clone(),
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
        FusionAST::new(
            self.grammar.clone(),
            self.arena.snapshot(),
            segments,
            roots.to_vec(),
            input,
        )
    }

    pub fn forest<'a>(
        &'a self,
        roots: &'a [NodeId],
        segments: &'a [Segment],
        input: &'a str,
    ) -> FusionForest<'a> {
        FusionForest::new(&self.grammar, self.arena(), segments, roots, input)
    }

    pub fn new(grammar: Grammar, typing: T) -> Self {
        Self {
            grammar,
            typing,
            arena: ParseArena::new(),
            tables: Tables::default(),
            input: String::new(),
            segments: Vec::new(),
        }
    }

    pub(crate) fn set_input(&mut self, input: &str) -> Result<(), PrefixError> {
        self.input = input.to_string();
        self.segments = self
            .grammar
            .tokenize(input)
            .map_err(|err| PrefixError::rejected(input.len(), err))?;
        self.typing.set_segs(&self.segments);
        Ok(())
    }

    pub fn parse(&mut self, input: &str, ctx: CtxId) -> Result<FusionAST, PrefixError> {
        #[cfg(test)]
        debug_trace!("fusion_parser", "parse start input='{}' ctx={}", input, ctx);

        self.set_input(input)?;
        self.arena = ParseArena::new();
        self.tables = Tables::default();
        let end = self.segs().len();
        let start = self
            .grammar
            .start()
            .and_then(|nt| self.grammar.nt_index(nt))
            .ok_or_else(|| PrefixError::rejected(input.len(), "missing start symbol"))?;

        // Seed start
        let start_prods: Vec<ProdId> = self
            .grammar
            .productions_at(start)
            .map(|prods| (0..prods.len()).map(|idx| (start, idx)).collect())
            .unwrap_or_default();
        self.seed(&start_prods, 0, ctx, &Obligations::empty());

        // Main loop
        while let Some(task) = self.tables.agenda.pop_front() {
            match task {
                Task::Process(item) => self.process(item)?,
                Task::Complete(completion) => self.complete(completion)?,
            }
        }

        // ── Frontier lifting ──────────────────────────────────────────────
        //
        // After the main loop, `frontier` holds items that couldn't advance
        // (past end of input, or partial regex match on the last segment).
        // We promote them to partial nodes and propagate through the waiter
        // network that was built during the main loop.
        //
        // This is a SEPARATE algorithm from the main Earley loop:
        //   - No `process()`, no `seed()`, no `consume()`.
        //   - Just `finish()` items and resume waiters with the new nodes.
        //   - Dedup on OUTPUT nodes by (nt, span, type): ensures type-distinct
        //     derivations both propagate (e.g. Integer vs partial-Float at "2")
        //     while circular grammars terminate (same type = dedup).
        //
        self.lift_frontier()?;

        let mut roots = self
            .tables
            .completed_nodes
            .get(&(start, 0, end))
            .cloned()
            .unwrap_or_default();
        roots.sort_unstable();
        roots.dedup();
        if roots.is_empty() {
            return Err(PrefixError::rejected(input.len(), "no parse found"));
        }
        Ok(self.materialize(&roots, self.segs().to_vec(), self.input.clone()))
    }
}
