//! Arena-backed typed AST — owns its data, zero materialization overhead.
//!
//! `FusionAST` holds a `ParseArena`, segments, and input text. All properties
//! (text, type, completeness, children, scoring signals) are computed on-demand
//! by traversing the arena. No intermediate tree is ever cloned.
//!
//! `FusionForest` is the borrowed hypertree semantics over that arena. Search,
//! scoring, and completion should prefer the forest view; `FusionAST` is the
//! owned realization boundary.

use crate::logic::grammar::{Grammar, Production, Segment, Symbol};
use crate::logic::typing::{SharedType, Type, intern_type};
use crate::regex::Regex as DerivativeRegex;
use std::collections::{BTreeSet, HashSet};

use super::runtime::RuleRuntime;
use crate::logic::parse::arena::{
    ChildRef, NodeId, NodeStatus, NtId, ParseArena, ProdId, TokenRef, TypeStatus,
};

// ============================================================================
// FusionAST — owns arena, computes everything on-demand
// ============================================================================

pub struct FusionAST {
    arena: ParseArena,
    segments: Vec<Segment>,
    roots: Vec<NodeId>,
    input: String,
}

pub struct FusionForest<'a> {
    arena: &'a ParseArena,
    segments: &'a [Segment],
    roots: &'a [NodeId],
    input: &'a str,
}

impl Clone for FusionAST {
    fn clone(&self) -> Self {
        Self {
            arena: self.arena.snapshot(),
            segments: self.segments.clone(),
            roots: self.roots.clone(),
            input: self.input.clone(),
        }
    }
}

impl std::fmt::Debug for FusionAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusionAST")
            .field("roots", &self.roots.len())
            .field("input", &self.input)
            .field("nodes", &self.arena.node_count())
            .finish()
    }
}

impl FusionAST {
    pub fn new(
        arena: ParseArena,
        segments: Vec<Segment>,
        roots: Vec<NodeId>,
        input: String,
    ) -> Self {
        Self {
            arena,
            segments,
            roots,
            input,
        }
    }

    pub fn is_empty(&self) -> bool {
        self.view().is_empty()
    }

    pub fn len(&self) -> usize {
        self.view().len()
    }

    pub fn first(&self) -> Option<FusionNode<'_>> {
        self.roots.first().map(|&id| FusionNode {
            ast: self,
            node_id: id,
        })
    }

    pub fn text(&self) -> &str {
        self.view().text()
    }

    pub fn completes(self) -> Result<Self, String> {
        let roots: Vec<_> = self
            .roots
            .into_iter()
            .filter(|&id| node_has_complete_alt(&self.arena, id))
            .collect();
        if roots.is_empty() {
            Err("No complete trees".into())
        } else {
            Ok(Self {
                arena: self.arena,
                segments: self.segments,
                roots,
                input: self.input,
            })
        }
    }

    pub fn is_complete(&self) -> bool {
        self.view().is_complete()
    }

    pub fn has_complete_root(&self) -> bool {
        self.is_complete()
    }

    pub fn has_well_typed_root(&self) -> bool {
        self.view().has_well_typed_root()
    }

    pub fn grounded_root_count(&self, runtime: &RuleRuntime) -> usize {
        self.roots
            .iter()
            .filter(|&&id| {
                self.arena.node(id).is_some_and(|n| match n.ty {
                    TypeStatus::Valid(ty) | TypeStatus::Partial(ty) => {
                        runtime.type_of(ty).is_some_and(|t| !matches!(t, Type::Any))
                    }
                })
            })
            .count()
    }

    pub fn roots(&self) -> impl Iterator<Item = FusionNode<'_>> {
        self.roots.iter().map(|&id| FusionNode {
            ast: self,
            node_id: id,
        })
    }

    pub fn completions(&self, grammar: &Grammar) -> Vec<DerivativeRegex> {
        self.view().completions(grammar)
    }

    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    #[cfg(test)]
    pub(crate) fn arena(&self) -> &ParseArena {
        &self.arena
    }

    pub fn root_ids(&self) -> &[NodeId] {
        &self.roots
    }

    /// Arena node count (for stats).
    pub fn node_count(&self) -> usize {
        self.view().node_count()
    }

    /// Count completeness signal: fraction of complete terminals.
    pub fn completeness_score(&self) -> f64 {
        self.view().completeness_score()
    }

    pub fn production_fullness_score(&self, grammar: &Grammar) -> f64 {
        self.view().production_fullness_score(grammar)
    }

    pub fn leaf_terminal_count(&self) -> usize {
        self.view().leaf_terminal_count()
    }

    pub fn min_open_slots(&self, grammar: &Grammar) -> usize {
        self.view().min_open_slots(grammar)
    }

    pub fn min_tree_depth(&self) -> usize {
        self.view().min_tree_depth()
    }

    pub fn bound_texts(&self) -> Vec<String> {
        self.view().bound_texts()
    }

    pub(crate) fn view(&self) -> FusionForest<'_> {
        FusionForest {
            arena: &self.arena,
            segments: &self.segments,
            roots: &self.roots,
            input: &self.input,
        }
    }
}

impl<'a> FusionForest<'a> {
    pub(crate) fn new(
        arena: &'a ParseArena,
        segments: &'a [Segment],
        roots: &'a [NodeId],
        input: &'a str,
    ) -> Self {
        Self {
            arena,
            segments,
            roots,
            input,
        }
    }

    pub(crate) fn is_empty(&self) -> bool {
        self.roots.is_empty()
    }
    pub(crate) fn len(&self) -> usize {
        self.roots.len()
    }
    pub(crate) fn text(&self) -> &'a str {
        self.input
    }
    pub(crate) fn is_complete(&self) -> bool {
        self.roots
            .iter()
            .any(|&id| node_has_complete_alt(self.arena, id))
    }
    pub(crate) fn has_well_typed_root(&self) -> bool {
        !self.roots.is_empty()
    }
    pub(crate) fn node_count(&self) -> usize {
        self.arena.node_count()
    }
    pub(crate) fn bound_texts(&self) -> Vec<String> {
        let mut out = BTreeSet::new();
        for &id in self.roots {
            collect_bound_texts_rec(self.arena, id, &mut out);
        }
        out.into_iter().collect()
    }
    pub(crate) fn roots(&self) -> impl Iterator<Item = FusionForestNode<'_>> + '_ {
        self.roots.iter().map(move |&id| FusionForestNode {
            forest: self,
            node_id: id,
        })
    }
    pub(crate) fn completions(&self, grammar: &Grammar) -> Vec<DerivativeRegex> {
        let mut ranked = Vec::new();
        for node in self.roots() {
            node.collect_valid_tokens_ranked(grammar, &mut ranked, 0);
        }
        ranked.sort_by(|(ap, _), (bp, _)| ap.cmp(bp));
        let mut seen = HashSet::new();
        ranked
            .into_iter()
            .filter_map(|(_, token)| seen.insert(token.clone()).then_some(token))
            .collect()
    }
    pub(crate) fn completeness_score(&self) -> f64 {
        self.roots()
            .map(|node| node.completeness_score())
            .fold(0.0_f64, f64::max)
    }
    pub(crate) fn production_fullness_score(&self, grammar: &Grammar) -> f64 {
        self.roots()
            .map(|node| node.production_fullness_score(grammar))
            .fold(0.0_f64, f64::max)
    }
    pub(crate) fn leaf_terminal_count(&self) -> usize {
        self.leafs().count()
    }
    pub(crate) fn min_open_slots(&self, grammar: &Grammar) -> usize {
        self.roots()
            .map(|node| node.count_open_slots(grammar))
            .min()
            .unwrap_or(0)
    }
    pub(crate) fn min_tree_depth(&self) -> usize {
        self.roots().map(|node| node.max_depth()).min().unwrap_or(0)
    }

    // Leafs are the terminal leaves of the forest, read as an iterable view.
    pub(crate) fn leafs(&self) -> Leafs<'_> {
        Leafs::new(self.roots().collect())
    }
}

#[derive(Clone, Copy)]
pub struct FusionForestNode<'a> {
    forest: &'a FusionForest<'a>,
    node_id: NodeId,
}

// ============================================================================
// FusionNode — borrowed view into a FusionAST
// ============================================================================

#[derive(Clone, Copy)]
pub struct FusionNode<'a> {
    ast: &'a FusionAST,
    node_id: NodeId,
}

impl<'a> FusionNode<'a> {
    pub fn node_id(&self) -> NodeId {
        self.node_id
    }

    pub fn ty(&self, runtime: &RuleRuntime) -> SharedType {
        let Some(node) = self.ast.arena.node(self.node_id) else {
            return intern_type(Type::Any);
        };
        let ty_id = match node.ty {
            TypeStatus::Valid(id) | TypeStatus::Partial(id) => id,
        };
        let ty = runtime.type_of(ty_id).unwrap_or(Type::Any);
        intern_type(ty)
    }

    pub fn is_complete(&self) -> bool {
        node_has_complete_alt(&self.ast.arena, self.node_id)
    }

    pub fn text(&self) -> String {
        text_from_node(&self.ast.arena, &self.ast.segments, self.node_id)
    }

    pub fn child_count(&self) -> usize {
        self.ast
            .arena
            .alts_for(self.node_id)
            .map(|alts| alts.first().map(|alt| alt.children.len()).unwrap_or(0))
            .unwrap_or(0)
    }

    pub fn rhs_len(&self, grammar: &Grammar) -> usize {
        self.ast
            .arena
            .alts_for(self.node_id)
            .map(|alts| {
                alts.first()
                    .map(|alt| {
                        grammar
                            .productions_by_idx(alt.prod.0)
                            .and_then(|prods| prods.iter().find(|p| p.rule.is_some()))
                            .map(|p| p.rhs.len())
                            .unwrap_or(alt.children.len())
                    })
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }

    pub fn children(&self) -> impl Iterator<Item = FusionChild<'a>> + 'a {
        let alts_data = self
            .ast
            .arena
            .alts_for(self.node_id)
            .and_then(|alts| alts.first().cloned());
        let children: Vec<FusionChild<'a>> = alts_data
            .map(|alt| {
                alt.children
                    .iter()
                    .map(|c| match c {
                        ChildRef::Node(id) => FusionChild::Node(FusionNode {
                            ast: self.ast,
                            node_id: *id,
                        }),
                        ChildRef::Terminal(tok) => FusionChild::Terminal {
                            text: render_token(tok, &self.ast.segments),
                            complete: tok.complete,
                        },
                    })
                    .collect()
            })
            .unwrap_or_default();
        children.into_iter()
    }
}

impl<'a> FusionForestNode<'a> {
    fn child_count(&self) -> usize {
        self.forest
            .arena
            .alts_for(self.node_id)
            .map(|alts| alts.first().map(|alt| alt.children.len()).unwrap_or(0))
            .unwrap_or(0)
    }
    fn rhs_len(&self, grammar: &Grammar) -> usize {
        self.forest
            .arena
            .alts_for(self.node_id)
            .map(|alts| {
                alts.first()
                    .map(|alt| {
                        grammar
                            .productions_by_idx(alt.prod.0)
                            .and_then(|prods| prods.iter().find(|p| p.rule.is_some()))
                            .map(|p| p.rhs.len())
                            .unwrap_or(alt.children.len())
                    })
                    .unwrap_or(0)
            })
            .unwrap_or(0)
    }
    fn children(&self) -> impl Iterator<Item = FusionForestChild<'a>> + 'a {
        let alts_data = self
            .forest
            .arena
            .alts_for(self.node_id)
            .and_then(|alts| alts.first().cloned());
        let children: Vec<_> = alts_data
            .map(|alt| {
                alt.children
                    .iter()
                    .map(|c| match c {
                        ChildRef::Node(id) => FusionForestChild::Node(FusionForestNode {
                            forest: self.forest,
                            node_id: *id,
                        }),
                        ChildRef::Terminal(tok) => FusionForestChild::Terminal {
                            text: render_token(tok, self.forest.segments),
                            complete: tok.complete,
                        },
                    })
                    .collect()
            })
            .unwrap_or_default();
        children.into_iter()
    }
    fn collect_valid_tokens_ranked(
        &self,
        grammar: &Grammar,
        tokens: &mut Vec<(usize, DerivativeRegex)>,
        priority: usize,
    ) {
        if let Some(child) = self.transparent_single_child(grammar) {
            if let (Some(owner), Some(seed)) = (
                self.forest.arena.node(self.node_id),
                self.forest.arena.node(child.node_id),
            ) && seed.status == NodeStatus::Complete
            {
                let mut visited = HashSet::new();
                collect_seed_extensions_rec(
                    owner.nt,
                    seed.nt,
                    grammar,
                    tokens,
                    priority + 1,
                    &mut visited,
                );
            }
            child.collect_valid_tokens_ranked(grammar, tokens, priority);
            return;
        }
        if let Some(node) = self.forest.arena.node(self.node_id)
            && node.status == NodeStatus::Partial
            && node.span.start == node.span.end
        {
            if let Some(nt_name) = grammar.nt_name(node.nt.0)
                && let Some(prods) = grammar.productions.get(nt_name)
            {
                for p in prods {
                    if let Some(sym) = p.rhs.first() {
                        push_first_set(tokens, first_set(sym, grammar), priority);
                    }
                }
            }
            return;
        }
        let Some(alts) = self.forest.arena.alts_for(self.node_id) else {
            return;
        };
        if alts.is_empty() {
            return;
        }
        for alt in alts.iter() {
            let rhs = production_by_id(grammar, alt.prod)
                .map(|p| p.rhs.as_slice())
                .unwrap_or(&[]);
            let alt_priority = priority + progress_penalty(alt.children.len());
            let Some(last_child) = alt.children.last() else {
                if let Some(symbol) = rhs.first() {
                    push_first_set(tokens, first_set(symbol, grammar), alt_priority);
                }
                continue;
            };
            match last_child {
                ChildRef::Terminal(tok) if tok.complete => {
                    if let Some(symbol) = rhs.get(alt.children.len()) {
                        push_first_set(tokens, first_set(symbol, grammar), alt_priority);
                    }
                }
                ChildRef::Terminal(_) => {
                    if let Some(symbol) = rhs.get(alt.children.len().saturating_sub(1)) {
                        push_first_set(tokens, first_set(symbol, grammar), alt_priority);
                    }
                    if alt.children.len() >= 2
                        && let Some(ChildRef::Terminal(tok)) = alt.children.last()
                        && tok.start == tok.end
                        && let Some(ChildRef::Node(prev_id)) =
                            alt.children.get(alt.children.len().saturating_sub(2))
                        && let Some(prev_node) = self.forest.arena.node(*prev_id)
                    {
                        if prev_node.status == NodeStatus::Partial {
                            FusionForestNode {
                                forest: self.forest,
                                node_id: *prev_id,
                            }
                            .collect_valid_tokens_ranked(grammar, tokens, alt_priority);
                        } else if prev_node.status == NodeStatus::Complete {
                            FusionForestNode {
                                forest: self.forest,
                                node_id: *prev_id,
                            }
                            .collect_valid_tokens_ranked(
                                grammar,
                                tokens,
                                alt_priority + EXTENSION_PENALTY,
                            );
                        }
                    }
                }
                ChildRef::Node(child_id) => {
                    let Some(child_node) = self.forest.arena.node(*child_id) else {
                        continue;
                    };
                    if child_node.status == NodeStatus::Partial {
                        FusionForestNode {
                            forest: self.forest,
                            node_id: *child_id,
                        }
                        .collect_valid_tokens_ranked(
                            grammar,
                            tokens,
                            alt_priority,
                        );
                    } else {
                        FusionForestNode {
                            forest: self.forest,
                            node_id: *child_id,
                        }
                        .collect_valid_tokens_ranked(
                            grammar,
                            tokens,
                            alt_priority + EXTENSION_PENALTY,
                        );
                        if child_node.span.start == child_node.span.end {
                            FusionForestNode {
                                forest: self.forest,
                                node_id: *child_id,
                            }
                            .collect_valid_tokens_ranked(grammar, tokens, alt_priority);
                        } else {
                            self.collect_extensions_from_child(
                                last_child,
                                grammar,
                                tokens,
                                alt_priority + EXTENSION_PENALTY,
                            );
                        }
                        if let Some(symbol) = rhs.get(alt.children.len()) {
                            push_first_set(tokens, first_set(symbol, grammar), alt_priority);
                        }
                    }
                }
            }
        }
    }
    fn collect_extensions_from_child(
        &self,
        child: &ChildRef,
        grammar: &Grammar,
        tokens: &mut Vec<(usize, DerivativeRegex)>,
        priority: usize,
    ) {
        match child {
            ChildRef::Terminal(_) => {}
            ChildRef::Node(child_id) => {
                if let Some(node) = self.forest.arena.node(*child_id)
                    && node.status == NodeStatus::Complete
                {
                    if let Some(alts) = self.forest.arena.alts_for(*child_id)
                        && let Some(alt) = alts.first()
                        && let Some(last) = alt.children.last()
                    {
                        FusionForestNode {
                            forest: self.forest,
                            node_id: *child_id,
                        }
                        .collect_extensions_from_child(last, grammar, tokens, priority);
                    }
                }
            }
        }
    }
    fn transparent_single_child(&self, grammar: &Grammar) -> Option<FusionForestNode<'a>> {
        let alts = self.forest.arena.alts_for(self.node_id)?;
        if alts.len() != 1 {
            return None;
        }
        let alt = alts.first()?;
        let production = production_by_id(grammar, alt.prod)?;
        if production.rule.is_some() || production.rhs.len() != 1 {
            return None;
        }
        match alt.children.as_slice() {
            [ChildRef::Node(node_id)] => Some(FusionForestNode {
                forest: self.forest,
                node_id: *node_id,
            }),
            _ => None,
        }
    }
    fn completeness_score(&self) -> f64 {
        let (mut score, mut total) = (0.0_f64, 0usize);
        self.count_completeness(&mut score, &mut total);
        if total == 0 {
            0.0
        } else {
            ((score / total as f64) * 2.0).min(2.0)
        }
    }
    fn count_completeness(&self, score: &mut f64, total: &mut usize) {
        *total += 1;
        for child in self.children() {
            match child {
                FusionForestChild::Terminal { text, complete } => {
                    if complete {
                        *score += 1.0
                    } else {
                        *score += 0.5 * (1.0 / (text.len() as f64 + 1.0));
                    }
                }
                FusionForestChild::Node(node) => node.count_completeness(score, total),
            }
        }
    }
    fn production_fullness_score(&self, grammar: &Grammar) -> f64 {
        let (mut sum_sq, mut count) = (0.0_f64, 0usize);
        self.collect_fullness(grammar, &mut sum_sq, &mut count);
        if count == 0 {
            0.0
        } else {
            (sum_sq / count as f64).sqrt()
        }
    }
    fn collect_fullness(&self, grammar: &Grammar, sum_sq: &mut f64, count: &mut usize) {
        let expected = self.rhs_len(grammar);
        let filled = self.child_count();
        if expected > 0 && filled > 0 {
            let ratio = (filled.min(expected) as f64) / (expected as f64);
            *sum_sq += ratio * ratio;
            *count += 1;
        }
        for child in self.children() {
            if let FusionForestChild::Node(node) = child {
                node.collect_fullness(grammar, sum_sq, count);
            }
        }
    }
    fn count_open_slots(&self, grammar: &Grammar) -> usize {
        let expected = self.rhs_len(grammar);
        let filled = self.child_count();
        let mut open = expected.saturating_sub(filled);
        for child in self.children() {
            if let FusionForestChild::Node(node) = child {
                open += node.count_open_slots(grammar);
            }
        }
        open
    }
    fn max_depth(&self) -> usize {
        1 + self
            .children()
            .filter_map(|child| match child {
                FusionForestChild::Node(node) => Some(node.max_depth()),
                FusionForestChild::Terminal { .. } => None,
            })
            .max()
            .unwrap_or(0)
    }
}

fn collect_seed_extensions_rec(
    owner_nt: NtId,
    seed_nt: NtId,
    grammar: &Grammar,
    tokens: &mut Vec<(usize, DerivativeRegex)>,
    priority: usize,
    visited: &mut HashSet<(usize, usize)>,
) {
    if !visited.insert((owner_nt.0, seed_nt.0)) {
        return;
    }
    let Some(prods) = grammar.productions_by_idx(owner_nt.0) else {
        return;
    };
    for prod in prods {
        let Some(Symbol::Nonterminal { name, .. }) = prod.rhs.first() else {
            continue;
        };
        let Some(first_nt) = grammar.nt_index(name).map(NtId) else {
            continue;
        };
        if first_nt == seed_nt && let Some(next) = prod.rhs.get(1) {
            push_first_set(tokens, first_set(next, grammar), priority);
        }
        collect_seed_extensions_rec(first_nt, seed_nt, grammar, tokens, priority + 1, visited);
    }
}

enum FusionForestChild<'a> {
    Node(FusionForestNode<'a>),
    Terminal { text: String, complete: bool },
}

pub(crate) struct Leafs<'a> {
    nodes: Vec<FusionForestNode<'a>>,
    terms: Vec<String>,
}

impl<'a> Leafs<'a> {
    fn new(nodes: Vec<FusionForestNode<'a>>) -> Self {
        Self {
            nodes,
            terms: Vec::new(),
        }
    }
}

impl<'a> Iterator for Leafs<'a> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if let Some(term) = self.terms.pop() {
                return Some(term);
            }
            let node = self.nodes.pop()?;
            for child in node.children() {
                match child {
                    FusionForestChild::Node(node) => self.nodes.push(node),
                    FusionForestChild::Terminal { text, .. } => self.terms.push(text),
                }
            }
        }
    }
}

const EXTENSION_PENALTY: usize = 256;

fn progress_penalty(consumed_children: usize) -> usize {
    255usize.saturating_sub(consumed_children.min(255))
}

fn production_by_id(grammar: &Grammar, prod: ProdId) -> Option<&Production> {
    let mut offset = 0usize;
    for nt_idx in 0..grammar.production_count() {
        let prods = grammar.productions_by_idx(nt_idx)?;
        if prod.0 < offset + prods.len() {
            return prods.get(prod.0 - offset);
        }
        offset += prods.len();
    }
    None
}

// ============================================================================
// FusionChild — either a node or a terminal
// ============================================================================

pub enum FusionChild<'a> {
    Node(FusionNode<'a>),
    Terminal { text: String, complete: bool },
}

// ============================================================================
// Display
// ============================================================================

impl std::fmt::Display for FusionAST {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, &root_id) in self.roots.iter().enumerate() {
            writeln!(f, "\nTree {}:", i)?;
            let node = FusionNode {
                ast: self,
                node_id: root_id,
            };
            write!(f, "{}", node)?;
        }
        Ok(())
    }
}

impl std::fmt::Display for FusionNode<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_tree(f, "", true)
    }
}

impl FusionNode<'_> {
    fn fmt_tree(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        prefix: &str,
        is_last: bool,
    ) -> std::fmt::Result {
        let branch = if is_last { "└─ " } else { "├─ " };
        let (nt_name, ty_str) = self
            .ast
            .arena
            .node(self.node_id)
            .map(|n| {
                let nt = format!("nt{}", n.nt.0);
                let ty = match n.ty {
                    TypeStatus::Valid(id) | TypeStatus::Partial(id) => format!(":TypeId({})", id.0),
                };
                (nt, ty)
            })
            .unwrap_or(("?".into(), String::new()));

        writeln!(f, "{}{}{}{}", prefix, branch, nt_name, ty_str)?;

        let child_prefix = format!("{}{}", prefix, if is_last { "   " } else { "│  " });
        let children: Vec<_> = self.children().collect();
        for (i, child) in children.iter().enumerate() {
            match child {
                FusionChild::Terminal { text, .. } => {
                    writeln!(
                        f,
                        "{}{}{}",
                        child_prefix,
                        if i == children.len() - 1 {
                            "└─ "
                        } else {
                            "├─ "
                        },
                        text
                    )?;
                }
                FusionChild::Node(node) => {
                    node.fmt_tree(f, &child_prefix, i == children.len() - 1)?;
                }
            }
        }
        Ok(())
    }
}

// ============================================================================
// Helpers
// ============================================================================

fn text_from_node(arena: &ParseArena, segments: &[Segment], node_id: NodeId) -> String {
    let Some(alts) = arena.alts_for(node_id) else {
        return String::new();
    };
    let Some(alt) = alts.first() else {
        return String::new();
    };
    let mut parts = Vec::new();
    for child in &alt.children {
        match child {
            ChildRef::Node(child_id) => {
                let s = text_from_node(arena, segments, *child_id);
                if !s.is_empty() {
                    parts.push(s);
                }
            }
            ChildRef::Terminal(tok) => {
                let s = render_token(tok, segments);
                if !s.is_empty() {
                    parts.push(s);
                }
            }
        }
    }
    parts.join(" ")
}

fn render_token(tok: &TokenRef, segments: &[Segment]) -> String {
    if !tok.complete {
        return String::new();
    }
    (tok.start as usize..tok.end as usize)
        .filter_map(|idx| segments.get(idx).map(|s| s.as_str().to_string()))
        .collect::<Vec<_>>()
        .join(" ")
}

fn push_first_set(
    tokens: &mut Vec<(usize, DerivativeRegex)>,
    next: Vec<DerivativeRegex>,
    priority: usize,
) {
    tokens.extend(next.into_iter().map(|token| (priority, token)));
}

fn collect_bound_texts_rec(arena: &ParseArena, node_id: NodeId, out: &mut BTreeSet<String>) {
    let Some(node) = arena.node(node_id) else {
        return;
    };

    for binding in &node.bindings {
        if let Some(value) = &binding.value {
            out.insert(value.clone());
        }
    }

    let child_nodes: Vec<NodeId> = arena
        .alts_for(node_id)
        .map(|alts| {
            alts.iter()
                .flat_map(|alt| alt.children.iter())
                .filter_map(|child| match child {
                    ChildRef::Node(child_id) => Some(*child_id),
                    ChildRef::Terminal(_) => None,
                })
                .collect()
        })
        .unwrap_or_default();

    drop(node);

    for child_id in child_nodes {
        collect_bound_texts_rec(arena, child_id, out);
    }
}

fn node_has_complete_alt(arena: &ParseArena, node_id: NodeId) -> bool {
    arena
        .node(node_id)
        .is_some_and(|node| node.status == NodeStatus::Complete)
        && arena.alts_for(node_id).is_some_and(|alts| {
        alts.iter().any(|alt| {
            alt.children.iter().all(|child| match child {
                ChildRef::Terminal(tok) => tok.complete,
                ChildRef::Node(child_id) => node_has_complete_alt(arena, *child_id),
            })
        })
    })
}

fn first_set(symbol: &Symbol, grammar: &Grammar) -> Vec<DerivativeRegex> {
    fn first_set_rec(
        symbol: &Symbol,
        grammar: &Grammar,
        visited: &mut std::collections::HashSet<String>,
    ) -> Vec<DerivativeRegex> {
        match symbol {
            Symbol::Terminal { regex, .. } => vec![regex.clone()],
            Symbol::Nonterminal { name: nt_name, .. } => {
                if visited.contains(nt_name) {
                    return vec![];
                }
                visited.insert(nt_name.clone());
                let res = if let Some(productions) = grammar.productions.get(nt_name) {
                    productions
                        .iter()
                        .flat_map(|prod| {
                            prod.rhs.first().map(|s| first_set_rec(s, grammar, visited))
                        })
                        .flatten()
                        .collect()
                } else {
                    vec![]
                };
                visited.remove(nt_name);
                res
            }
        }
    }
    let mut out = first_set_rec(symbol, grammar, &mut std::collections::HashSet::new());
    // Deterministic + completion-friendly ordering: prefer tokens whose examples
    // start with alphanumerics (e.g. numbers/identifiers) over punctuation like '('.
    out.sort_by(|a, b| {
        let ea = a.example().unwrap_or_default();
        let eb = b.example().unwrap_or_default();
        let ka = (
            ea.chars()
                .next()
                .is_some_and(|c| !c.is_ascii_alphanumeric()),
            ea.len(),
            a.to_pattern(),
        );
        let kb = (
            eb.chars()
                .next()
                .is_some_and(|c| !c.is_ascii_alphanumeric()),
            eb.len(),
            b.to_pattern(),
        );
        ka.cmp(&kb)
    });
    out.dedup_by(|a, b| a.to_pattern() == b.to_pattern());
    out
}
