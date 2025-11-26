use crate::logic::grammar::Grammar;
use crate::logic::partial::PartialAST;
use crate::logic::partial::{Node, NonTerminal, Terminal};
use crate::logic::typing::eval::check_tree;
use crate::logic::typing::core::TreeStatus;
use serde::Serialize;

#[derive(Debug, Serialize, Clone)]
pub struct GraphNode {
    pub id: String,
    pub label: String,
    pub status: String, // "complete" | "partial" | "terminal"
    pub meta: NodeMeta,
}

#[derive(Debug, Serialize, Clone)]
pub struct GraphEdge {
    pub from: String,
    pub to: String,
    pub label: Option<String>,
    pub style: String, // "solid" | "dashed"
}

#[derive(Debug, Serialize, Clone)]
pub struct GraphData {
    pub nodes: Vec<GraphNode>,
    pub edges: Vec<GraphEdge>,
    pub trees: Vec<TreeInfo>,
}

#[derive(Debug, Serialize, Clone)]
pub struct TreeInfo {
    pub id: String,
    pub index: usize,
    pub complete: bool,           // Syntactically complete
    pub well_typed: bool,         // Passes type checking
    pub type_status: String,      // "valid" | "malformed" | "partial"
    pub node_count: usize,
    pub context: Vec<ContextEntry>, // Variables in scope after this tree
}

#[derive(Debug, Serialize, Clone)]
pub struct ContextEntry {
    pub name: String,
    pub ty: String,
}

#[derive(Debug, Serialize, Clone)]
pub struct NodeMeta {
    pub kind: String,
    pub value: Option<String>,
    pub binding: Option<String>,
    pub production: Option<ProductionInfo>,
    pub typing_rule: Option<TypingRuleInfo>,
    pub alternative: usize,
}

#[derive(Debug, Serialize, Clone)]
pub struct ProductionInfo {
    pub rhs: Vec<String>,
    pub cursor: usize,
    pub complete: bool,
}

#[derive(Debug, Serialize, Clone)]
pub struct TypingRuleInfo {
    pub name: String,
    pub premises: Vec<String>,
    pub conclusion: String,
}

pub fn build_graph(ast: &PartialAST, grammar: &Grammar) -> GraphData {
    let mut nodes: Vec<GraphNode> = Vec::new();
    let mut edges: Vec<GraphEdge> = Vec::new();
    let mut trees: Vec<TreeInfo> = Vec::new();

    // Create a virtual root for the forest
    let root_id = "root";
    nodes.push(GraphNode {
        id: root_id.to_string(),
        label: format!("Forest ({})", ast.roots.len()),
        status: if ast.complete() { "complete" } else { "partial" }.to_string(),
        meta: NodeMeta {
            kind: "forest".to_string(),
            value: None,
            binding: None,
            production: None,
            typing_rule: None,
            alternative: 0,
        },
    });

    for (i, root) in ast.roots.iter().enumerate() {
        let child_id = format!("t{}", i);
        let tree_complete = root.is_complete();
        let node_count_before = nodes.len();
        
        // Run type checking on the tree
        let type_result = check_tree(root, grammar);
        let (well_typed, type_status) = match &type_result {
            TreeStatus::Valid(_) => (true, "valid"),
            TreeStatus::Partial(_) => (true, "partial"),  // Partial is OK for incomplete trees
            TreeStatus::Malformed => (false, "malformed"),
        };
        
        // Extract context from the tree (for let expressions etc)
        let context = extract_context_from_tree(root, grammar);
        
        edges.push(GraphEdge {
            from: root_id.to_string(),
            to: child_id.clone(),
            label: Some(format!("alt {}", i)),
            style: if well_typed { "solid" } else { "dashed" }.to_string(),
        });
        walk_nt(&child_id, root, grammar, &mut nodes, &mut edges, well_typed);
        
        let node_count = nodes.len() - node_count_before;
        trees.push(TreeInfo {
            id: child_id,
            index: i,
            complete: tree_complete,
            well_typed,
            type_status: type_status.to_string(),
            node_count,
            context,
        });
    }

    GraphData { nodes, edges, trees }
}

fn walk_nt(
    node_id: &str,
    nt: &NonTerminal,
    grammar: &Grammar,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
    tree_well_typed: bool,
) {
    let is_complete = nt.is_complete();

    let prod_info = ProductionInfo {
        rhs: nt.production.rhs.iter().map(|s| format!("{:?}", s)).collect(),
        cursor: nt.children.len(),
        complete: is_complete,
    };

    // Get typing rule info if present
    let typing_rule = nt.production.rule.as_ref().and_then(|rule_name| {
        grammar.typing_rules.get(rule_name).map(|rule| {
            TypingRuleInfo {
                name: rule_name.clone(),
                premises: rule.premises.iter().map(|p| p.to_string()).collect(),
                conclusion: rule.conclusion.to_string(),
            }
        })
    });

    let label = if let Some(ref rule) = nt.production.rule {
        format!("{}({})", nt.name, rule)
    } else {
        nt.name.clone()
    };

    // Status combines syntactic completeness and type validity
    let status = if !tree_well_typed {
        "error"  // Type error in this tree
    } else if is_complete {
        "complete"
    } else {
        "partial"
    };

    nodes.push(GraphNode {
        id: node_id.to_string(),
        label,
        status: status.to_string(),
        meta: NodeMeta {
            kind: "nonterminal".to_string(),
            value: Some(nt.name.clone()),
            binding: nt.binding.clone(),
            production: Some(prod_info),
            typing_rule,
            alternative: nt.alternative_index,
        },
    });

    for (i, child) in nt.children.iter().enumerate() {
        let child_id = format!("{}_{}", node_id, i);
        
        // Get the symbol name for edge label
        let edge_label = nt.production.rhs.get(i).map(|sym| {
            match sym {
                crate::logic::grammar::Symbol::Expression { name, binding } => {
                    if let Some(b) = binding {
                        format!("{}[{}]", name, b)
                    } else {
                        name.clone()
                    }
                }
                crate::logic::grammar::Symbol::Regex { binding, .. } => {
                    if let Some(b) = binding {
                        format!("[{}]", b)
                    } else {
                        String::new()
                    }
                }
            }
        });
        
        edges.push(GraphEdge {
            from: node_id.to_string(),
            to: child_id.clone(),
            label: edge_label,
            style: "solid".to_string(),
        });
        walk_node(&child_id, child, grammar, nodes, edges, tree_well_typed);
    }
}

fn walk_node(
    node_id: &str,
    node: &Node,
    grammar: &Grammar,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
    tree_well_typed: bool,
) {
    match node {
        Node::Terminal(t) => {
            let (value, binding, status) = match t {
                Terminal::Complete { value, binding, .. } => {
                    let s = if tree_well_typed { "terminal" } else { "error" };
                    (value.clone(), binding.clone(), s)
                }
                Terminal::Partial { value, binding, .. } => {
                    (value.clone(), binding.clone(), "partial")
                }
            };

            let label = if value.is_empty() {
                "∅".to_string()
            } else {
                format!("\"{}\"", value)
            };

            nodes.push(GraphNode {
                id: node_id.to_string(),
                label,
                status: status.to_string(),
                meta: NodeMeta {
                    kind: "terminal".to_string(),
                    value: Some(value),
                    binding,
                    production: None,
                    typing_rule: None,
                    alternative: 0,
                },
            });
        }
        Node::NonTerminal(nt) => {
            walk_nt(node_id, nt, grammar, nodes, edges, tree_well_typed);
        }
    }
}

/// Extract context bindings from a tree (e.g., from let expressions)
fn extract_context_from_tree(root: &NonTerminal, grammar: &Grammar) -> Vec<ContextEntry> {
    let mut entries = Vec::new();
    collect_context_entries(&Node::NonTerminal(root.clone()), grammar, &mut entries);
    entries
}

fn collect_context_entries(node: &Node, grammar: &Grammar, entries: &mut Vec<ContextEntry>) {
    if let Node::NonTerminal(nt) = node {
        // Check if this node has a rule with context transform
        if let Some(rule_name) = &nt.production.rule {
            if let Some(rule) = grammar.typing_rules.get(rule_name) {
                // Check if conclusion has output context (let-style)
                if let Some(output) = &rule.conclusion.context.output {
                    for (var_name, ty_expr) in &output.extensions {
                        // Try to resolve var_name to actual value
                        if let Some(paths) = grammar.binding_map.get(var_name, rule_name) {
                            for path in paths {
                                if let Ok(results) = crate::logic::partial::binding::resolve_binding_path(node, path) {
                                    if let Some(res) = results.first() {
                                        if let Some(name) = get_node_text(res.node()) {
                                            entries.push(ContextEntry {
                                                name,
                                                ty: format!("{}", ty_expr),
                                            });
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        
        // Recurse into children
        for child in &nt.children {
            collect_context_entries(child, grammar, entries);
        }
    }
}

fn get_node_text(node: &Node) -> Option<String> {
    match node {
        Node::Terminal(Terminal::Complete { value, .. }) => Some(value.clone()),
        Node::Terminal(Terminal::Partial { value, .. }) if !value.is_empty() => Some(value.clone()),
        Node::Terminal(Terminal::Partial { .. }) => None,
        Node::NonTerminal(nt) => {
            let mut s = String::new();
            for child in &nt.children {
                s.push_str(&get_node_text(child)?);
            }
            Some(s)
        }
    }
}
