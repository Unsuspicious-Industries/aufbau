use crate::logic::partial::PartialAST;
use crate::logic::partial::structure::Rule;
use crate::logic::partial::{Alt, NonTerminal, ParsedNode, Slot};
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
        .any(|alt| !alt.is_complete() && alt.is_progressing());

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
    } else if alt.is_progressing() {
        "partial"
    } else {
        "error"
    };

    let prod_info = ProductionInfo {
        rhs: alt
            .production
            .rhs
            .iter()
            .map(|s| format!("{:?}", s))
            .collect(),
        cursor: alt.cursor(),
        rhs_len: alt.production.rhs.len(),
        complete: is_complete,
        has_partial: alt.slots.values().any(|s| s.is_partial()),
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
            typing_rule: alt.typing_rule.as_ref().map(|r| r.name.clone()),
        },
    });

    // Walk slot contents
    for (sym_idx, slot) in &alt.slots {
        match slot {
            Slot::Filled {
                nodes: parsed_nodes,
                ..
            } => {
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
            Slot::Partial {
                node,
                partial_symbol,
                type_constraint,
            } => {
                // If there's a partial node, walk it
                if let Some(pnode) = node {
                    let child_id = format!("{alt_id}.s{sym_idx}_partial");
                    edges.push(GraphEdge {
                        from: alt_id.to_string(),
                        to: child_id.clone(),
                        style: "dashed".to_string(),
                    });
                    walk_parsed_node(&child_id, pnode, nodes, edges);
                } else {
                    // Only show placeholder for terminal partial symbols
                    let should_show = matches!(
                        partial_symbol,
                        crate::logic::partial::PartialSymbol::Terminal { .. }
                    );

                    if should_show {
                        // Mark partial slot visually without node
                        let child_id = format!("{alt_id}.s{sym_idx}_partial");
                        edges.push(GraphEdge {
                            from: alt_id.to_string(),
                            to: child_id.clone(),
                            style: "dashed".to_string(),
                        });
                        nodes.push(GraphNode {
                            id: child_id,
                            label: "...".to_string(),
                            status: "partial".to_string(),
                            meta: NodeMeta {
                                kind: "partial-symbol".to_string(),
                                value: None,
                                binding: None,
                                span: None,
                                production: None,
                                typing_rule: None,
                            },
                        });
                    }
                }
            }
            Slot::Group {
                iterations,
                partial_iteration,
                ..
            } => {
                // Walk complete iterations
                for (iter_idx, iteration) in iterations.iter().enumerate() {
                    for (slot_idx, inner_slot) in iteration.iter().enumerate() {
                        walk_slot_in_graph(
                            &format!("{alt_id}.g{sym_idx}_i{iter_idx}_s{slot_idx}"),
                            inner_slot,
                            nodes,
                            edges,
                            &alt_id,
                        );
                    }
                }
                // Walk partial iteration if any
                if let Some(partial) = partial_iteration {
                    for (slot_idx, inner_slot) in partial.iter().enumerate() {
                        walk_slot_in_graph(
                            &format!("{alt_id}.g{sym_idx}_partial_s{slot_idx}"),
                            inner_slot,
                            nodes,
                            edges,
                            &alt_id,
                        );
                    }
                }
            }
        }
    }
}

/// Helper to recursively walk a slot in the graph
fn walk_slot_in_graph(
    slot_id: &str,
    slot: &Box<Slot>,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
    parent_id: &str,
) {
    match slot.as_ref() {
        Slot::Filled {
            nodes: parsed_nodes,
            ..
        } => {
            for (node_idx, pnode) in parsed_nodes.iter().enumerate() {
                let child_id = format!("{slot_id}_n{node_idx}");
                edges.push(GraphEdge {
                    from: parent_id.to_string(),
                    to: child_id.clone(),
                    style: "solid".to_string(),
                });
                walk_parsed_node(&child_id, pnode, nodes, edges);
            }
        }
        Slot::Partial { node, .. } => {
            if let Some(pnode) = node {
                let child_id = format!("{slot_id}_partial");
                edges.push(GraphEdge {
                    from: parent_id.to_string(),
                    to: child_id.clone(),
                    style: "dashed".to_string(),
                });
                walk_parsed_node(&child_id, pnode, nodes, edges);
            }
        }
        Slot::Group {
            iterations,
            partial_iteration,
            ..
        } => {
            for (iter_idx, iteration) in iterations.iter().enumerate() {
                for (inner_idx, inner_slot) in iteration.iter().enumerate() {
                    walk_slot_in_graph(
                        &format!("{slot_id}_i{iter_idx}_s{inner_idx}"),
                        inner_slot,
                        nodes,
                        edges,
                        parent_id,
                    );
                }
            }
            if let Some(partial) = partial_iteration {
                for (inner_idx, inner_slot) in partial.iter().enumerate() {
                    walk_slot_in_graph(
                        &format!("{slot_id}_partial_s{inner_idx}"),
                        inner_slot,
                        nodes,
                        edges,
                        parent_id,
                    );
                }
            }
        }
    }
}

fn walk_parsed_node(
    node_id: &str,
    node: &ParsedNode,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
) {
    match node {
        ParsedNode::Terminal(t) => {
            nodes.push(GraphNode {
                id: node_id.to_string(),
                label: t.value.clone(),
                status: "terminal".to_string(),
                meta: NodeMeta {
                    kind: "terminal".to_string(),
                    value: Some(t.value.clone()),
                    binding: t.binding.clone(),
                    span: t.span.as_ref().map(|s| SpanView {
                        start: s.start,
                        end: s.end,
                    }),
                    production: None,
                    typing_rule: None,
                },
            });
        }
        ParsedNode::NonTerminal(nt) => {
            walk_nt(node_id, nt, nodes, edges);
        }
    }
}
