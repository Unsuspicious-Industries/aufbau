use crate::logic::partial::PartialAST;
use crate::logic::partial::{Alt, NonTerminal, Node};
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
pub struct GraphNode {
    pub id: String,
    pub label: String,
    pub status: String, // "complete" | "partial" | "warning" | "terminal"
    pub meta: NodeMeta,
}

#[derive(Debug, Serialize, Clone)]
pub struct GraphEdge {
    pub from: String,
    pub to: String,
    pub style: String, // "solid" | "dashed"
}

#[derive(Debug, Serialize, Clone)]
pub struct GraphData {
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
}

#[derive(Debug, Serialize, Clone)]
pub struct NodeMeta {
    pub kind: String,
    pub value: Option<String>,
    pub binding: Option<String>,
    pub span: Option<SpanView>,
    pub production: Option<ProductionInfo>,
    pub typing_rule: Option<String>,
}

#[derive(Debug, Serialize, Clone)]
pub struct SpanView {
    pub start: usize,
    pub end: usize,
}

#[derive(Debug, Serialize, Clone)]
pub struct ProductionInfo {
    pub rhs: Vec<String>,
    pub cursor: usize,
    pub rhs_len: usize,
    pub complete: bool,
    pub has_partial: bool,
}

pub fn build_graph(ast: &PartialAST) -> GraphData {
    collect_graph_from_alt("root", &ast)
}

fn collect_graph_from_alt(root_id: &str, alt: &PartialAST) -> GraphData {
    let mut nodes: Vec<GraphNode> = Vec::new();
    let mut edges: Vec<GraphEdge> = Vec::new();

    walk_nt(root_id, alt.root(), &mut nodes, &mut edges);

    GraphData { nodes, edges }
}

fn walk_nt(
    node_id: &str,
    nt: &NonTerminal,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
) {
    // NonTerminal container node
    let is_complete = nt.is_complete();
    let has_partial = nt
        .alts
        .iter()
        .any(|alt| !alt.is_complete());

    nodes.push(GraphNode {
        id: node_id.to_string(),
        label: nt.name.clone(),
        status: if is_complete && !has_partial {
            "complete"
        } else if is_complete {
            "warning"
        } else {
            "partial"
        }
        .to_string(),
        meta: NodeMeta {
            kind: "nonterminal-container".to_string(),
            value: Some(nt.name.clone()),
            binding: nt.binding.clone(),
            span: nt.span.as_ref().map(|s| SpanView {
                start: s.start,
                end: s.end,
            }),
            production: None,
            typing_rule: None,
        },
    });

    // Walk each alternative
    for (idx, alt) in nt.alts.iter().enumerate() {
        let alt_id = format!("{node_id}::alt{idx}");
        edges.push(GraphEdge {
            from: node_id.to_string(),
            to: alt_id.clone(),
            style: "dashed".to_string(),
        });
        walk_alt(&alt_id, alt, nodes, edges);
    }
}

fn walk_alt(alt_id: &str, alt: &Alt, nodes: &mut Vec<GraphNode>, edges: &mut Vec<GraphEdge>) {
    let is_complete = alt.is_complete();
    let has_partial_desc = false; // TODO: implement proper descendant check

    let status = if is_complete && !has_partial_desc {
        "complete"
    } else if !is_complete {
        "partial"
    } else {
        "error"
    };

    let cursor = alt.slots.len(); // Use number of filled slots as cursor approximation

    let prod_info = ProductionInfo {
        rhs: alt
            .production
            .rhs
            .iter()
            .map(|s| format!("{:?}", s))
            .collect(),
        cursor,
        rhs_len: alt.production.rhs.len(),
        complete: is_complete,
        has_partial: false, // TODO: check if any slot is partial
    };

    nodes.push(GraphNode {
        id: alt_id.to_string(),
        label: format!("alt"),
        status: status.to_string(),
        meta: NodeMeta {
            kind: "alternative".to_string(),
            value: None,
            binding: None,
            span: alt.span.as_ref().map(|s| SpanView {
                start: s.start,
                end: s.end,
            }),
            production: Some(prod_info),
            typing_rule: None, // TODO: get typing rule if available
        },
    });

    // Walk slot contents
    for (sym_idx, slot) in &alt.slots {
        let parsed_nodes = slot.nodes();
        for (node_idx, pnode) in parsed_nodes.iter().enumerate() {
            let child_id = format!("{alt_id}.s{sym_idx}_{node_idx}");
            edges.push(GraphEdge {
                from: alt_id.to_string(),
                to: child_id.clone(),
                style: "solid".to_string(),
            });
            walk_parsed_node(&child_id, pnode, nodes, edges);
        }
    }
}

fn walk_parsed_node(
    node_id: &str,
    node: &Node,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
) {
    match node {
        Node::Terminal(t) => {
            let (value, binding, span) = match t {
                crate::logic::partial::Terminal::Complete { value, binding, span, .. } => {
                    (value.clone(), binding.clone(), span.clone())
                }
                crate::logic::partial::Terminal::Partial { value, binding, span, .. } => {
                    (value.clone(), binding.clone(), span.clone())
                }
            };
            
            nodes.push(GraphNode {
                id: node_id.to_string(),
                label: value.clone(),
                status: "terminal".to_string(),
                meta: NodeMeta {
                    kind: "terminal".to_string(),
                    value: Some(value),
                    binding,
                    span: span.as_ref().map(|s| SpanView {
                        start: s.start,
                        end: s.end,
                    }),
                    production: None,
                    typing_rule: None,
                },
            });
        }
        Node::NonTerminal(nt) => {
            walk_nt(node_id, nt, nodes, edges);
        }
    }
}
