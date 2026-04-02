use crate::logic::grammar::Segment;

use super::{ChildRef, NodeId, TypedParser, TypingRuntime};
use crate::logic::parse::arena::{ParseArena, TokenRef};

pub fn render_node_text<T: TypingRuntime>(
    parser: &TypedParser<T>,
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
            ChildRef::Terminal(tok) => parts.push(render_token(tok, segments)),
        }
    }
    parts
        .into_iter()
        .filter(|part| !part.is_empty())
        .collect::<Vec<_>>()
        .join(" ")
}

pub fn pretty_node<T: TypingRuntime>(
    parser: &TypedParser<T>,
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
        "{}node {:?} nt={:?} span=({},{}) status={:?} ty={:?}\n",
        pad, node_id, node.nt, node.span.start, node.span.end, node.status, node.ty
    ));
    out.push_str(&format!("{}bindings={:?}\n", pad, node.bindings));
    if let Some(alts) = arena.alts_for(node_id) {
        for (idx, alt) in alts.iter().enumerate() {
            out.push_str(&format!("{}alt {} prod={:?}\n", pad, idx, alt.prod));
            for child in &alt.children {
                match child {
                    ChildRef::Node(child_id) => {
                        render_pretty(arena, *child_id, segments, indent + 1, out)
                    }
                    ChildRef::Terminal(tok) => out.push_str(&format!(
                        "{}  term {:?} text='{}'\n",
                        pad,
                        tok,
                        render_token(tok, segments)
                    )),
                }
            }
        }
    }
}

fn render_token(tok: &TokenRef, segments: &[Segment]) -> String {
    if !tok.complete {
        return String::new();
    }
    (tok.start as usize..tok.end as usize)
        .filter_map(|idx| {
            segments
                .get(idx)
                .map(|segment: &Segment| segment.as_str().to_string())
        })
        .collect::<Vec<_>>()
        .join(" ")
}
