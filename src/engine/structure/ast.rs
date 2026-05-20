//! Arena-backed typed structural views over parser output.

use crate::debug_trace;
use crate::engine::grammar::{Segment, SPG};
use crate::engine::parse::arena::Lexeme;
use crate::semantics::domain::ConstraintDomain;
use std::collections::BTreeSet;

use crate::engine::parse::arena::{ChildRef, NodeId, ParseArena, Span};

// ============================================================================
// FusionAST — owns arena, computes everything on-demand
// ============================================================================

pub struct FusionAST<D: ConstraintDomain> {
    grammar: SPG<D>,
    arena: ParseArena,
    segments: Vec<Segment>,
    roots: Vec<NodeId>,
    input: String,
}

pub struct FusionForest<'a, D: ConstraintDomain> {
    _grammar: &'a SPG<D>,
    arena: &'a ParseArena,
    roots: &'a [NodeId],
    input: &'a str,
}

impl<D: ConstraintDomain> Clone for FusionAST<D> {
    fn clone(&self) -> Self {
        Self {
            grammar: self.grammar.clone(),
            arena: self.arena.snapshot(),
            segments: self.segments.clone(),
            roots: self.roots.clone(),
            input: self.input.clone(),
        }
    }
}

impl<D: ConstraintDomain> std::fmt::Debug for FusionAST<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FusionAST")
            .field("roots", &self.roots.len())
            .field("input", &self.input)
            .field("nodes", &self.arena.node_count())
            .finish()
    }
}

impl<D: ConstraintDomain> FusionAST<D> {
    pub fn new(
        grammar: SPG<D>,
        arena: ParseArena,
        segments: Vec<Segment>,
        roots: Vec<NodeId>,
        input: String,
    ) -> Self {
        Self {
            grammar,
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

    pub fn first(&self) -> Option<FusionNode<'_, D>> {
        self.roots.first().map(|&id| FusionNode {
            ast: self,
            node_id: id,
        })
    }

    pub fn text(&self) -> &str {
        self.view().text()
    }

    pub fn segs(&self) -> &[Segment] {
        &self.segments
    }

    pub fn completes(self) -> Result<Self, String> {
        let roots: Vec<_> = self
            .roots
            .into_iter()
            .filter(|&id| self.arena.node(id).is_some_and(|node| node.is_complete()))
            .collect();
        if roots.is_empty() {
            Err("No complete trees".into())
        } else {
            Ok(Self {
                grammar: self.grammar,
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

    pub fn is_exact(&self) -> bool {
        self.view().is_exact()
    }

    pub fn has_complete_root(&self) -> bool {
        self.is_complete()
    }

    pub fn roots(&self) -> impl Iterator<Item = FusionNode<'_, D>> {
        self.roots.iter().map(|&id| FusionNode {
            ast: self,
            node_id: id,
        })
    }

    pub fn segments(&self) -> &[Segment] {
        &self.segments
    }

    pub fn grammar(&self) -> &SPG<D> {
        &self.grammar
    }

    pub fn arena(&self) -> &ParseArena {
        &self.arena
    }

    pub fn root_ids(&self) -> &[NodeId] {
        &self.roots
    }

    /// Arena node count (for stats).
    pub fn node_count(&self) -> usize {
        self.view().node_count()
    }

    pub fn bound_texts(&self) -> Vec<String> {
        self.view().bound_texts(&self.segments)
    }

    pub(crate) fn view(&self) -> FusionForest<'_, D> {
        FusionForest {
            _grammar: &self.grammar,
            arena: &self.arena,
            roots: &self.roots,
            input: &self.input,
        }
    }
}

impl<'a, D: ConstraintDomain> FusionForest<'a, D> {
    pub(crate) fn new(
        grammar: &'a SPG<D>,
        arena: &'a ParseArena,
        _segments: &'a [Segment],
        roots: &'a [NodeId],
        input: &'a str,
    ) -> Self {
        Self {
            _grammar: grammar,
            arena,
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
        debug_trace!(
            "fusion",
            "FusionForest::is_complete: input='{}' roots={}",
            self.input,
            self.roots.len()
        );
        let result = self
            .roots
            .iter()
            .any(|&id| node_has_complete_alt(self.arena, id));
        debug_trace!("fusion", "is_complete: result={}", result);
        result
    }

    pub(crate) fn is_exact(&self) -> bool {
        self.roots
            .iter()
            .any(|&id| node_has_exact_alt(self.arena, id))
    }
    pub(crate) fn node_count(&self) -> usize {
        self.arena.node_count()
    }
    pub(crate) fn bound_texts(&self, s: &[Segment]) -> Vec<String> {
        let mut out = BTreeSet::new();
        for &id in self.roots {
            collect_bound_texts_rec(self.arena, id, s, &mut out);
        }
        out.into_iter().collect()
    }
}

// ============================================================================
// FusionNode — borrowed view into a FusionAST
// ============================================================================

#[derive(Clone, Copy)]
pub struct FusionNode<'a, D: ConstraintDomain> {
    ast: &'a FusionAST<D>,
    node_id: NodeId,
}

impl<'a, D: ConstraintDomain> FusionNode<'a, D> {
    #[must_use] 
    pub fn node_id(&self) -> NodeId {
        self.node_id
    }

    #[must_use] 
    pub fn evidence(&self) -> crate::engine::parse::arena::EvidenceId {
        self.ast
            .arena
            .node(self.node_id)
            .map_or(0, |n| n.evidence)
    }

    #[must_use] 
    pub fn is_complete(&self) -> bool {
        node_has_complete_alt(&self.ast.arena, self.node_id)
    }

    #[must_use] 
    pub fn text(&self) -> String {
        text_from_node(&self.ast.arena, &self.ast.segments, self.node_id)
    }

    #[must_use] 
    pub fn child_count(&self) -> usize {
        self.ast
            .arena
            .alts_for(self.node_id)
            .map_or(0, |alts| alts.first().map_or(0, |alt| alt.children.len()))
    }

    #[must_use] 
    pub fn rhs_len(&self) -> usize {
        self.ast
            .arena
            .alts_for(self.node_id)
            .map_or(0, |alts| {
                alts.first()
                    .map_or(0, |alt| {
                        self.ast
                            .grammar
                            .prod(alt.prod)
                            .map_or(0, |p| p.rhs.len())
                    })
            })
    }

    pub fn children(&self) -> impl Iterator<Item = FusionChild<'a, D>> + 'a {
        let alts_data = self
            .ast
            .arena
            .alts_for(self.node_id)
            .and_then(|alts| alts.first().cloned());
        let children: Vec<FusionChild<'a, D>> = alts_data
            .map(|alt| {
                alt.children
                    .iter()
                    .map(|c| match c {
                        ChildRef::Node(id) => FusionChild::Node(FusionNode {
                            ast: self.ast,
                            node_id: *id,
                        }),
                        ChildRef::Terminal(Lexeme {
                            matched: span,
                            complete,
                            open: _,
                        }) => FusionChild::Terminal {
                            text: render_span(*span, &self.ast.segments),
                            complete: *complete,
                        },
                    })
                    .collect()
            })
            .unwrap_or_default();
        children.into_iter()
    }
}

// ============================================================================
// FusionChild — either a node or a terminal
// ============================================================================

#[derive(Clone)]
pub enum FusionChild<'a, D: ConstraintDomain> {
    Node(FusionNode<'a, D>),
    Terminal { text: String, complete: bool },
}

// ============================================================================
// Display
// ============================================================================

impl<D: ConstraintDomain> std::fmt::Display for FusionAST<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, &root_id) in self.roots.iter().enumerate() {
            writeln!(f, "\nTree {i}:")?;
            let node = FusionNode {
                ast: self,
                node_id: root_id,
            };
            write!(f, "{node}")?;
        }
        Ok(())
    }
}

impl<D: ConstraintDomain> std::fmt::Display for FusionNode<'_, D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.fmt_tree(f, "", true)
    }
}

impl<D: ConstraintDomain> FusionNode<'_, D> {
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
            .map_or(("?".into(), String::new()), |n| {
                let nt = format!("nt{}", n.nt);
                let ty = format!(":EvidenceId({})", n.evidence);
                (nt, ty)
            });

        writeln!(f, "{prefix}{branch}{nt_name}{ty_str}")?;

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
            ChildRef::Terminal(Lexeme {
                matched: span,
                complete,
                open: _,
            }) => {
                if *complete {
                    let s = render_span(*span, segments);
                    if !s.is_empty() {
                        parts.push(s);
                    }
                }
            }
        }
    }
    parts.join(" ")
}

fn render_span(span: Span, segments: &[Segment]) -> String {
    (span.start as usize..span.end as usize)
        .filter_map(|idx| segments.get(idx).map(|s| s.as_str().to_string()))
        .collect::<Vec<_>>()
        .join(" ")
}

fn collect_bound_texts_rec(
    arena: &ParseArena,
    node_id: NodeId,
    s: &[Segment],
    out: &mut BTreeSet<String>,
) {
    let Some(node) = arena.node(node_id) else {
        return;
    };

    for status in node.binding_map.values() {
        if let crate::engine::parse::arena::BindingStatus::Resolved {
            span,
            complete: _,
            open: _,
            evidence: _,
        } = status
        {
            let l = Lexeme::new(*span, true, false);
            if let Some(value) = l.value(s) {
                out.insert(value.clone());
            }
        }
    }

    let child_nodes: Vec<NodeId> = arena
        .alts_for(node_id)
        .map(|alts| {
            alts.iter()
                .flat_map(|alt| alt.children.iter())
                .filter_map(|child| match child {
                    ChildRef::Node(child_id) => Some(*child_id),
                    ChildRef::Terminal(Lexeme {
                        matched: _,
                        complete: _,
                        open: _,
                    }) => None,
                })
                .collect()
        })
        .unwrap_or_default();

    drop(node);

    for child_id in child_nodes {
        collect_bound_texts_rec(arena, child_id, s, out);
    }
}

fn node_has_complete_alt(arena: &ParseArena, node_id: NodeId) -> bool {
    arena.node(node_id).is_some_and(|node| node.is_complete())
        && arena.alts_for(node_id).is_some_and(|alts| {
            alts.iter().any(|alt| {
                alt.children.iter().all(|child| match child {
                    ChildRef::Terminal(Lexeme {
                        matched: _,
                        complete,
                        open: _,
                    }) => *complete,
                    ChildRef::Node(child_id) => node_has_complete_alt(arena, *child_id),
                })
            })
        })
}

fn node_has_exact_alt(arena: &ParseArena, node_id: NodeId) -> bool {
    arena
        .node(node_id)
        .is_some_and(|node| node.status.exact() && node.semantic_complete)
        && arena.alts_for(node_id).is_some_and(|alts| {
            alts.iter().any(|alt| {
                alt.children.iter().all(|child| match child {
                    ChildRef::Terminal(Lexeme {
                        matched: _,
                        complete,
                        open,
                    }) => *complete && !open,
                    ChildRef::Node(child_id) => node_has_exact_alt(arena, *child_id),
                })
            })
        })
}
