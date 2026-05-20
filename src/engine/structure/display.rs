use crate::engine::grammar::Segment;
use crate::engine::parse::arena::EvidenceId;
use crate::engine::parse::arena::Lexeme;
use crate::semantics::SemanticRuntime;

use crate::engine::parse::arena::{ChildRef, NodeId, ParseArena, Span};
use crate::engine::parse::{State, TypedParser};
use crate::semantics::domain::ConstraintDomain;

pub fn render_node_text<D: ConstraintDomain, T: SemanticRuntime>(
    parser: &TypedParser<D, T>,
    node_id: NodeId,
    segments: &[Segment],
) -> String {
    let Some(alts) = parser.arena().alts_for(node_id) else {
        return String::new();
    };
    let Some(alt) = alts.first() else {
        return String::new();
    };

    let mut parts = Vec::new();
    for child in &alt.children {
        match child {
            ChildRef::Node(child_id) => parts.push(render_node_text(parser, *child_id, segments)),
            ChildRef::Terminal(Lexeme {
                matched: span,
                complete,
                open: _,
            }) => {
                if *complete {
                    parts.push(render_span(*span, segments));
                }
            }
        }
    }
    parts
        .into_iter()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn pretty_node<D: ConstraintDomain, T: SemanticRuntime>(
    parser: &TypedParser<D, T>,
    node_id: NodeId,
    segments: &[Segment],
) -> String {
    let mut out = String::new();
    render_pretty(parser.arena(), node_id, segments, 0, &mut out);
    out
}

fn render_pretty(
    arena: &ParseArena,
    node_id: NodeId,
    segments: &[Segment],
    indent: usize,
    out: &mut String,
) {
    let Some(node) = arena.node(node_id) else {
        return;
    };
    let pad = "  ".repeat(indent);
    out.push_str(&format!(
        "{}node {:?} nt={:?} span=({},{}) status={:?} evidence={}\n",
        pad,
        node_id,
        node.nt,
        node.span.start,
        node.span.end,
        node.status,
        render_evidence_label(Some(node.evidence))
    ));
    out.push_str(&format!("{}binding_map={:?}\n", pad, node.binding_map));
    if let Some(alts) = arena.alts_for(node_id) {
        for (idx, alt) in alts.iter().enumerate() {
            out.push_str(&format!("{}alt {} prod={:?}\n", pad, idx, alt.prod));
            for child in &alt.children {
                match child {
                    ChildRef::Node(child_id) => {
                        render_pretty(arena, *child_id, segments, indent + 1, out);
                    }
                    ChildRef::Terminal(Lexeme {
                        matched: span,
                        complete,
                        open,
                    }) => out.push_str(&format!(
                        "{}  term {:?} complete={} open={} text='{}'\n",
                        pad,
                        span,
                        complete,
                        open,
                        render_span(*span, segments)
                    )),
                }
            }
        }
    }
}

fn render_evidence_label(evidence_id: Option<EvidenceId>) -> String {
    match evidence_id {
        Some(id) => format!("EvidenceId({id})"),
        None => "None".to_string(),
    }
}

fn render_span(span: Span, segments: &[Segment]) -> String {
    (span.start as usize..span.end as usize)
        .filter_map(|idx| segments.get(idx).map(|s| s.as_str().to_string()))
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn pretty_prefix_state<D: ConstraintDomain, T: SemanticRuntime>(
    parser: &TypedParser<D, T>,
    state: &State,
    segments: &[Segment],
) -> String {
    let mut out = String::new();
    out.push_str(&format!(
        "State span=({}, {}) root={} frontier={}\n",
        state.span.start,
        state.span.end,
        state.root,
        state.frontier.as_ref().map_or(0, std::vec::Vec::len),
    ));
    out.push_str("root:\n");
    let pretty = pretty_node(parser, state.root, segments);
    for line in pretty.lines() {
        out.push_str("  ");
        out.push_str(line);
        out.push('\n');
    }

    out
}
impl std::fmt::Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "State span=({}, {}) root={} frontier={}",
            self.span.start,
            self.span.end,
            self.root,
            self.frontier.as_ref().map_or(0, std::vec::Vec::len),
        )
    }
}
