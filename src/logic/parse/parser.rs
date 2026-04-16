use crate::debug_trace;
use crate::logic::binding::GrammarPath;
use crate::logic::grammar::{Grammar, Segment, Symbol};
use crate::regex::PrefixStatus;
use std::collections::{HashMap, HashSet, VecDeque};

use crate::logic::fusion::ast::{FusionAST, FusionForest};
use crate::logic::fusion::{PrefixError, TransitionError};
use crate::logic::typing::state::{Obligation, TypingRuntime};

use crate::logic::parse::arena::{
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
    ANY_TYPE, // identifier for the ANY type, default kind
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
    pub ctx_in: CtxId,
    pub obligations: Vec<Obligation>,
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
    /// Classic Earley dedup: one item per (production, dot, start, pos, ctx).
    /// Children are NOT part of the key — different derivations at the same
    /// position are packed into the forest, not split into separate items.
    pub seen_process: HashSet<(ProdId, usize, usize, usize, CtxId)>,
    /// Classic Earley: one completion chain per (nt, start, end) span.
    /// Multiple nodes at the same span are recorded in `completed_nodes`
    /// but only the first triggers waiter resumption during the main loop.
    /// Frontier lifting has its own type-aware propagation (see `lift_frontier`).
    pub seen_complete: HashSet<(NtId, usize, usize)>,
    pub results: HashMap<(NtId, usize), Vec<usize>>,
    pub completed_nodes: HashMap<(NtId, usize, usize), Vec<NodeId>>,
    pub waiters: HashMap<(NtId, usize), Vec<Waiter>>,
    pub frontier: Vec<Item>,
}


fn create_obligations(grammar: &Grammar, prod: ProdId) -> Vec<Obligation> {
    let Some(production) = grammar.prod(prod) else {
        return vec![];
    };
    let Some(rule_name) = &production.rule else {
        return vec![];
    };
    let Some(binding_map) = &grammar.bindings else {
        return vec![];
    };
    let Some(rule) = grammar.rules().get(rule_name.as_str()) else {
        return vec![];
    };

    let alt = prod.1;
    rule.used_bindings()
        .into_iter()
        .filter_map(|name| {
            let paths = binding_map.get(name, rule_name)?;
            let filtered: Vec<GrammarPath> = paths
                .iter()
                .filter(|p| {
                    p.steps()
                        .first()
                        .map_or(true, |s| s.a.map_or(true, |a| a == alt))
                })
                .cloned()
                .collect();
            if filtered.is_empty() {
                return None;
            }
            Some(Obligation {
                name: name.to_string(),
                paths: filtered,
                value: None,
                actual: None,
            })
        })
        .collect()
}

fn step_obligations(obligations: &[Obligation], dot: usize, alt: usize) -> Vec<Obligation> {
    obligations
        .iter()
        .filter_map(|ob| {
            let stepped: Vec<GrammarPath> = ob
                .paths
                .iter()
                .filter_map(|p| {
                    let steps = p.steps();
                    let first = steps.first()?;
                    if first.i == dot && first.a.map_or(true, |a| a == alt) {
                        Some(GrammarPath::from(steps[1..].to_vec()))
                    } else {
                        None
                    }
                })
                .collect();
            if stepped.is_empty() {
                return None;
            }
            Some(Obligation {
                name: ob.name.clone(),
                paths: stepped,
                value: ob.value.clone(),
                actual: ob.actual,
            })
        })
        .collect()
}


fn fill_nonterminal_obligation(
    obligations: &mut [Obligation],
    dot: usize,
    alt: usize,
    node: &ArenaNode,
) {
    for ob in obligations.iter_mut() {
        if !ob.has_matched() && ob.matches(dot, alt) {
            let complete = node.status == NodeStatus::Complete;
            ob.value = Some(Lexeme::new(node.span, complete, node.open));
            ob.actual = Some(node.ty);
            continue;
        }
        // Multi-step: inherit from child's resolved bindings
        for child_binding in &node.bindings {
            if child_binding.name == ob.name {
                ob.value = child_binding.value.clone();
                ob.actual = child_binding.ty;
                break;
            }
        }
    }
}

fn prune_from_obligations(obligations: &[Obligation], nt: NtId, grammar: &Grammar) -> Vec<ProdId> {
    let total = grammar.productions_at(nt).map_or(0, |p| p.len());
    if total == 0 {
        return vec![];
    }
    let all = || (0..total).map(|i| (nt, i)).collect::<Vec<_>>();

    let mut constrained: HashSet<usize> = HashSet::new();
    let mut any_none = false;

    for ob in obligations {
        for path in &ob.paths {
            if let Some(first) = path.steps().first() {
                match first.a {
                    Some(a) if a < total => {
                        constrained.insert(a);
                    }
                    None => {
                        any_none = true;
                    }
                    _ => {}
                }
            }
        }
    }

    if any_none || constrained.is_empty() {
        all()
    } else {
        constrained.into_iter().map(|a| (nt, a)).collect()
    }
}

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
        parent_obs: &[Obligation],
    ) {
        for &prod in prods {
            let mut obligations = step_obligations_for_seed(parent_obs, prod.1);
            obligations.extend(create_obligations(&self.grammar, prod));
            self.enqueue_process(Item {
                prod,
                dot: 0,
                start: pos,
                pos,
                ctx,
                ctx_in: ctx,
                obligations,
                children: Vec::new(),
            });
        }
    }

    fn child_text(&self, child: &ChildRef) -> Option<String> {
        match child {
            ChildRef::Terminal(l) => l.value(
                // combine all segs[i].text() for i in the span, separated by spaces
                &self.segs(),
        ).map(|s| s.to_string()),
            ChildRef::Node(id) => {
                let node = self.arena.node(*id)?;
                // convert segments to text and join with spaces
                Some(self.segs()[node.span.start as usize..node.span.end as usize]
                    .iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<&str>>()
                    .join(" "))
            }
        }
    }

    /// Try to match a regex terminal against the current segment.
    ///
    /// Four cases:
    ///   - Past end of input → item goes to frontier (waiting for more input).
    ///   - NoMatch → item is dead, dropped.
    ///   - Prefix → the segment starts a valid match but needs more characters.
    ///     At end of input this becomes a frontier lexeme that does not yet
    ///     satisfy the terminal symbol. Mid-input it's dead.
    ///   - Complete / Extensible → full match, item advances normally.
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
            PrefixStatus::Prefix(_) => (false, at_end), // only open if at end of input
            PrefixStatus::Complete => (true, false),
            PrefixStatus::Extensible(_) => (true, true),
        };
        let span = Span {
            start: segment.index as u32,
            end: segment.index as u32 + 1,
        };
        let l = Lexeme { matched: span, complete, open };

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
            // checking if any obligation matches this terminal
            next.obligations.iter_mut().filter(|o| !o.has_matched()).for_each(|ob| {
                let hit = ob.paths.iter().any(|p| {
                    let s = p.steps();
                    s.len() == 1 && s[0].i == item.dot && s[0].a.map_or(true, |a| a == item.prod.1)
                });
                // setting obligaiton value to the terminal
                if hit {
                    ob.value = Some(l.clone());
                }
            });
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
        let syntax_complete = self.alt_is_complete(item);
        let syntax_status = if syntax_complete {
            NodeStatus::Complete
        } else {
            NodeStatus::Partial
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
            syntax_status,
            item.children.len()
        );
        let has_rule = self
            .grammar
            .prod(item.prod)
            .is_some_and(|p| p.rule.is_some());
        let finalize_ctx = if has_rule { item.ctx_in } else { item.ctx };
        match self
            .typing
            .finalize(item.prod, finalize_ctx, &item.obligations, syntax_status)
        {
            Ok((ty, ctx_out, typed_complete)) => {
                let status = if syntax_complete && typed_complete {
                    NodeStatus::Complete
                } else {
                    NodeStatus::Partial
                };
                // Propagate open flag bottom-up: any leaf terminal extensible
                // at end-of-input, or any child node with open=true.
                let any_open = item.children.iter().any(|child|self.arena.open(child));
                let ty = if ty == ANY_TYPE {
                    self.infer_type_from_children(&item.children).unwrap_or(ty)
                } else {
                    ty
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
                    ty,
                    open: any_open,
                    env_in: Some(item.ctx_in),
                    env_out: Some(ctx_out),
                    bindings,
                    alts: AltRange { start: 0, len: 0 },
                };
                let packed = vec![PackedAlt {
                    prod: item.prod,
                    children: item.children.clone(),
                }];
                Ok(Some(self.arena.push_node(node, packed)))
            }
            Err(TransitionError::Rejected) => {
                #[cfg(test)]
                debug_trace!(
                    "fusion_parser",
                    "finalize rejected nt={} alt={} status={:?}",
                    self.grammar.nt(item.prod.0).unwrap_or("<?>"),
                    item.prod.1,
                    syntax_status,
                );
                Ok(None)
            }
        }
    }

    fn infer_type_from_children(&self, children: &[ChildRef]) -> Option<TypeId> {
        let mut found = None;
        for child in children {
            if let ChildRef::Node(id) = child {
                let ty = self.arena.node(*id)?.ty;
                if ty != ANY_TYPE {
                    if found.is_some() {
                        return None; // ambiguous: multiple typed children
                    }
                    found = Some(ty);
                }
            }
        }
        found
    }

    fn resume_from_child(&mut self, parent: &Item, node_id: NodeId) -> Option<Item> {
        let node = self.arena.node(node_id)?.clone();
        let mut resumed = parent.clone();
        fill_nonterminal_obligation(
            &mut resumed.obligations,
            parent.dot,
            parent.prod.1,
            &node
        );
        resumed.dot += 1;
        resumed.pos = node.span.end as usize;
        resumed.ctx = node.env_out.unwrap_or(parent.ctx);
        resumed.children.push(ChildRef::Node(node_id));
        Some(resumed)
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

        if self
            .tables
            .seen_complete
            .insert((completion.nt, completion.start, completion.end))
        {
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
                    if let Some(resumed) = self.resume_from_child(&waiter.item, completion.node) {
                        self.enqueue_process(resumed);
                    }
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

                let child_ctx = match self.typing.descend(
                    item.prod,
                    item.dot,
                    binding,
                    item.ctx,
                    &item.obligations,
                ) {
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
                        if let Some(resumed) = self.resume_from_child(&item, node_id) {
                            self.enqueue_process(resumed);
                        }
                    }
                }

                // Seed child productions with stepped obligations for pruning
                let stepped = step_obligations(&item.obligations, item.dot, item.prod.1);
                let prods = prune_from_obligations(&stepped, nt, &self.grammar);
                self.seed(&prods, item.pos, child_ctx, &stepped);
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
        let mut seen_items: HashSet<(ProdId, usize, usize, usize)> = HashSet::new();
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
            if !seen_items.insert((item.prod, item.dot, item.start, item.pos)) {
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
                if let Some(resumed) = self.resume_from_child(&waiter.item, node_id) {
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
        self.seed(&start_prods, 0, ctx, &[]);

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

/// When seeding a child nonterminal, the stepped obligations from the parent
/// need to be further distributed per-alt. For each alt `a`, we keep only
/// obligations whose (now first) path step has `a` matching, or `a == None`.
fn step_obligations_for_seed(stepped: &[Obligation], alt: usize) -> Vec<Obligation> {
    stepped
        .iter()
        .filter_map(|ob| {
            let kept: Vec<GrammarPath> = ob
                .paths
                .iter()
                .filter(|p| {
                    p.steps()
                        .first()
                        .map_or(true, |s| s.a.map_or(true, |a| a == alt))
                })
                .cloned()
                .collect();
            if kept.is_empty() && !ob.paths.iter().any(|p| p.is_empty()) {
                return None;
            }
            Some(Obligation {
                name: ob.name.clone(),
                paths: if kept.is_empty() {
                    ob.paths.clone()
                } else {
                    kept
                },
                value: ob.value.clone(),
                actual: ob.actual,
            })
        })
        .collect()
}
