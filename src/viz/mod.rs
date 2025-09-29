use serde::{Serialize, Deserialize};
use crate::logic::partial::{PartialAST, PartialASTNode, PartialNonTerminal};
use rouille::{router, Request, Response};
use std::sync::{Arc, Mutex};

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
}

#[derive(Debug, Serialize, Clone)]
pub struct SpanView { pub start: usize, pub end: usize }

#[derive(Debug, Serialize, Clone)]
pub struct ProductionInfo {
    pub rhs: Vec<String>,
    pub cursor: usize,
    pub rhs_len: usize,
    pub complete: bool,
    pub has_partial: bool,
}

fn node_color(status: &str) -> &str {
    match status {
        "complete" => "green",
        "partial" => "red",
        "terminal" => "#888",
        _ => "#aaa",
    }
}

fn collect_graph_from_partial(root_id: &str, ast: &PartialAST) -> GraphData {
    let mut nodes: Vec<GraphNode> = Vec::new();
    let mut edges: Vec<GraphEdge> = Vec::new();

    fn walk(node_id: &str, node: &PartialASTNode, nodes: &mut Vec<GraphNode>, edges: &mut Vec<GraphEdge>, _is_parallel_child: bool) {
        match node {
            PartialASTNode::Terminal(t) => {
                nodes.push(GraphNode {
                    id: node_id.to_string(),
                    label: t.value.clone(),
                    status: "terminal".to_string(),
                    meta: NodeMeta {
                        kind: "terminal".to_string(),
                        value: Some(t.value.clone()),
                        binding: t.binding.clone(),
                        span: t.span.as_ref().map(|s| SpanView { start: s.start, end: s.end }),
                        production: None,
                    }
                });
            }
            PartialASTNode::NonTerminal(parallels) => {
                // A NonTerminal is a set of parallel alternatives. This container will be represented by a synthetic node.
                let label = if let Some(first) = parallels.first() { first.value.clone() } else { "?".to_string() };
                // Container status is complete if any parallel is complete
                let is_complete = parallels.iter().any(|p| p.production().is_complete_state(p.children().len()));
                let has_partial_child = parallels.iter().any(|p| p.children().iter().any(|ch| match ch { PartialASTNode::NonTerminal(inner) => !inner.iter().any(|alt| alt.production().is_complete_state(alt.children().len()) || !alt.children().is_empty()), _ => false }));
                nodes.push(GraphNode {
                    id: node_id.to_string(),
                    label,
                    status: if is_complete && !has_partial_child { "complete" } else if is_complete { "warning" } else { "partial" }.to_string(),
                    meta: NodeMeta { kind: "nonterminal-container".to_string(), value: None, binding: None, span: None, production: None }
                });

                for (idx, alt) in parallels.iter().enumerate() {
                    let alt_id = format!("{node_id}::alt{idx}");
                    // Parallel edges from the container are dashed
                    edges.push(GraphEdge { from: node_id.to_string(), to: alt_id.clone(), style: "dashed".to_string() });
                    walk_alt(&alt_id, alt, nodes, edges);
                }
            }
        }
    }

    fn walk_alt(alt_id: &str, alt: &PartialNonTerminal, nodes: &mut Vec<GraphNode>, edges: &mut Vec<GraphEdge>) {
        // Determine completion and partial descendants
        fn has_partial_desc(node: &PartialASTNode) -> bool {
            match node {
                PartialASTNode::Terminal(_) => false,
                PartialASTNode::NonTerminal(alts) => {
                    for a in alts {
                        let a_complete = a.production().is_complete_state(a.children().len());
                        if !a_complete { return true; }
                        for ch in a.children() {
                            if has_partial_desc(ch) { return true; }
                        }
                    }
                    false
                }
            }
        }
        let is_complete = alt.production().is_complete_state(alt.children().len());
        let has_partial_descendant = alt.children().iter().any(|c| has_partial_desc(c));
        // Strict check that required literals appear in subtree (heuristic to avoid false-complete)
        fn child_has_literal(children: &[PartialASTNode], lit: &str) -> bool {
            for ch in children {
                match ch {
                    PartialASTNode::Terminal(t) => { if t.value == lit { return true; } }
                    PartialASTNode::NonTerminal(alts) => { for a in alts { if child_has_literal(a.children(), lit) { return true; } } }
                }
            }
            false
        }
        let mut literals_missing = false;
        for sym in alt.production().rhs_symbols() {
            if let crate::logic::grammar::Symbol::Litteral(l) = sym {
                if !child_has_literal(alt.children(), l) { literals_missing = true; break; }
            }
        }
        let is_complete_strict = is_complete && !literals_missing;
        let has_valid_child = !alt.children().is_empty();
        let status = if is_complete_strict && !has_partial_descendant { "complete" } else if is_complete_strict { "warning" } else if has_valid_child { "partial" } else { "error" };
        let label = format!("{}", alt.value());
        let prod = alt.production();
        let prod_info = ProductionInfo {
            rhs: prod.rhs_symbols().iter().map(|s| format!("{:?}", s)).collect(),
            cursor: prod.cursor_value(),
            rhs_len: prod.rhs_len(),
            complete: prod.is_complete_state(alt.children().len()),
            has_partial: prod.has_partial_in_progress(),
        };
        let meta = NodeMeta {
            kind: "nonterminal".to_string(),
            value: Some(alt.value().clone()),
            binding: alt.binding().clone(),
            span: alt.span().as_ref().map(|s| SpanView { start: s.start, end: s.end }),
            production: Some(prod_info),
        };
        nodes.push(GraphNode { id: alt_id.to_string(), label, status: status.to_string(), meta });
        for (i, child) in alt.children().iter().enumerate() {
            let child_id = format!("{alt_id}.{i}");
            // Children edges are solid
            edges.push(GraphEdge { from: alt_id.to_string(), to: child_id.clone(), style: "solid".to_string() });
            walk(&child_id, child, nodes, edges, false);
        }
    }

    walk(root_id, ast.root(), &mut nodes, &mut edges, false);
    GraphData { nodes, edges }
}

pub fn build_graph(ast: &PartialAST) -> GraphData {
    collect_graph_from_partial("root", ast)
}

#[derive(Debug, Deserialize)]
struct GraphRequest {
    spec: String,
    input: String,
}

pub fn serve(bind_addr: &str) {
    let state: Arc<Mutex<()>> = Arc::new(Mutex::new(()));
    rouille::start_server(bind_addr, move |request: &Request| {
        router!(request,
            (GET) (/) => {
                let html = include_str!("./static/index.html");
                Response::html(html)
            },
            (POST) (/graph) => {
                let body = rouille::input::json_input::<GraphRequest>(request);
                let body = match body { Ok(b) => b, Err(e) => return Response::text(format!("bad json: {}", e)).with_status_code(400) };
                let spec = body.spec;
                let input = body.input;

                // Build grammar and partial AST
                let grammar = match crate::logic::grammar::Grammar::load(&spec) { Ok(g) => g, Err(e) => return Response::text(format!("spec error: {}", e)).with_status_code(400) };
                let mut parser = crate::logic::partial::PartialParser::new(grammar);
                let partial = match parser.partial(&input) { Ok(p) => p, Err(e) => return Response::text(format!("parse error: {}", e)).with_status_code(400) };
                let graph = build_graph(&partial);
                Response::json(&graph)
            },
            (GET) (/static/{file: String}) => {
                match file.as_str() {
                    "app.js" => {
                        let js = include_str!("./static/app.js");
                        Response::from_data("application/javascript", js.to_string())
                    }
                    "styles.css" => {
                        let css = include_str!("./static/styles.css");
                        Response::from_data("text/css", css.to_string())
                    }
                    _ => Response::empty_404()
                }
            },
            _ => Response::empty_404()
        )
    });
}

