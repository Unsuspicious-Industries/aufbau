//! Structural elaboration — parser-side defaulting, NOT constraint semantics.
//!
//! `finalize` (the constraint domain in `typing`) assigns evidence and
//! effects from typing rules. But some finished nodes carry no rule or no
//! content: empty spans, rule-less wrappers, and rule-less nodes generally.
//! Elaboration fills those in from grammar *shape* alone, the single place a
//! node's evidence is decided when the domain says nothing (`finalize` returns
//! `None`). It never consults the meaning of the constraint domain, and is
//! excluded from realizability proofs.
//!
//! Every rule here is structural and generic. None may name a specific
//! nonterminal or construct — hardcoding a construct here would be smuggling
//! semantics in as elaboration, which is exactly what this module exists to
//! prevent.

use super::parser::Item;
use crate::engine::error::PrefixError;
use crate::engine::grammar::SPG;
use crate::engine::parse::TypedParser;
use crate::engine::parse::arena::{
    ArenaNode, ChildRef, EffectId, EvidenceId, NodeStatus, Span, TOP,
};
use crate::semantics::SemanticSummary;

/// How a finished node's stored evidence and effect are derived, by shape alone.
enum Elaboration {
    /// An empty span: nothing to type. Evidence is `TOP`, no effect.
    Bare,
    /// Rule-less single-nonterminal wrapper with no bound terminal: the node has
    /// no semantics of its own, so it inherits its one child's evidence/effects.
    Transparent,
    /// A rule-less node with content: its evidence is its own parse tree, a `Con`
    /// over its children (or a `Leaf` of its text). This is how a type annotation
    /// becomes a tree the rules can read — syntax and typing share one object.
    Structural,
    /// The node carries its own semantics (a typing rule): the constraint
    /// domain's summary passes through unchanged.
    Opaque,
}

impl Elaboration {
    /// Classify a finished item by grammar shape only. Transparency is the
    /// grammar's one definition ([`SPG::is_transparent`]); a rule-less node with
    /// content carries its own structural tree.
    fn classify(grammar: &SPG, item: &Item, status: NodeStatus) -> Self {
        if status == NodeStatus::Prefix && item.start == item.pos {
            return Self::Bare;
        }
        if grammar.is_transparent(item.prod) {
            return Self::Transparent;
        }
        if grammar.rule_of_prod(item.prod).is_none() {
            return Self::Structural;
        }
        Self::Opaque
    }
}

impl TypedParser {
    /// Structural elaboration of a finished item into the evidence and effect
    /// actually stored on its node. Parser defaulting, not the constraint
    /// domain — see the module docs.
    pub(super) fn elaborate(
        &self,
        item: &Item,
        status: NodeStatus,
        summary: Option<&SemanticSummary>,
    ) -> Result<(EvidenceId, Option<EffectId>), PrefixError> {
        // Right-bound effects export only from exact nodes.
        let exact = status == NodeStatus::Exact;
        match Elaboration::classify(&self.grammar, item, status) {
            Elaboration::Bare => Ok((TOP, None)),
            Elaboration::Structural => {
                let nt = self.grammar.nt(item.prod.0).unwrap_or("?");
                let kids = self.child_node_evidence(item);
                let span = Span {
                    start: item.start as u32,
                    end: item.pos as u32,
                };
                Ok((self.typing.structural_evidence(nt, &kids, span), None))
            }
            Elaboration::Opaque => Ok(summary
                .map_or((TOP, None), |s| (s.evidence, if exact { s.effect } else { None }))),
            Elaboration::Transparent => {
                let evidence = self
                    .first_child_node(item)
                    .map_or(TOP, |child| child.evidence);
                let effect = if exact {
                    let mut effects = self.child_effects(item);
                    if let Some(local) = summary.and_then(|s| s.effect) {
                        effects.push(local);
                    }
                    self.typing.compose_effects(effects).map_err(|e| {
                        PrefixError::rejected(
                            item.pos,
                            format!("semantic effect composition failed: {e:?}"),
                        )
                    })?
                } else {
                    None
                };
                Ok((evidence, effect))
            }
        }
    }

    fn first_child_node(&self, item: &Item) -> Option<ArenaNode> {
        item.children.iter().find_map(|child| match child {
            ChildRef::Node(id) => self.arena.node(*id).map(|n| n.clone()),
            ChildRef::Terminal(_) => None,
        })
    }

    fn child_node_evidence(&self, item: &Item) -> Vec<EvidenceId> {
        item.children
            .iter()
            .filter_map(|child| match child {
                ChildRef::Node(id) => self.arena.node(*id).map(|n| n.evidence),
                ChildRef::Terminal(_) => None,
            })
            .collect()
    }

    fn child_effects(&self, item: &Item) -> Vec<EffectId> {
        item.children
            .iter()
            .filter_map(|child| match child {
                ChildRef::Node(id) => self.arena.node(*id).and_then(|n| n.effect),
                ChildRef::Terminal(_) => None,
            })
            .collect()
    }
}
