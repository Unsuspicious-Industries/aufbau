use crate::logic::grammar::Grammar;
use crate::logic::partial::PartialAST;
use crate::logic::partial::{Node, NonTerminal, Terminal};
use crate::logic::typing::core::TreeStatus;
use crate::logic::typing::eval::check_tree;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Debug, Serialize, Clone)]
pub struct GraphNode {
    pub id: String,
    pub label: String,
    pub status: String,                // "complete" | "partial" | "terminal"
    pub reconstructed: Option<String>, // Reconstructed input from terminals below this node
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
    /// Reconstructed inputs from all complete trees (for checking associativity etc)
    pub reconstructed_inputs: Vec<ReconstructedInput>,
}

#[derive(Debug, Serialize, Clone)]
pub struct ReconstructedInput {
    pub tree_index: usize,
    pub text: String,
    pub complete: bool,
}

#[derive(Debug, Serialize, Clone)]
pub struct TreeInfo {
    pub id: String,
    pub index: usize,
    pub complete: bool,      // Syntactically complete
    pub well_typed: bool,    // Passes type checking
    pub type_status: String, // "valid" | "malformed" | "partial"
    pub node_count: usize,
    pub context: Vec<ContextEntry>, // Variables in scope after this tree
    pub reconstructed: String,      // Reconstructed input text from this tree
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
    pub inferred_type: Option<String>,
    pub context: Vec<ContextEntry>,
    pub constraints: Vec<ConstraintInfo>,
    pub alternative: usize,
}

#[derive(Debug, Serialize, Clone)]
pub struct ConstraintInfo {
    pub kind: String, // "premise" | "setting" | "unify" | "membership"
    pub text: String,
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
    /// Pretty multiline formatting of the full inference rule.
    pub pretty: String,
}

pub fn build_graph(ast: &PartialAST, grammar: &Grammar) -> GraphData {
    let mut nodes: Vec<GraphNode> = Vec::new();
    let mut edges: Vec<GraphEdge> = Vec::new();
    let mut trees: Vec<TreeInfo> = Vec::new();
    let mut reconstructed_inputs: Vec<ReconstructedInput> = Vec::new();

    // Create a virtual root for the forest
    let root_id = "root";
    nodes.push(GraphNode {
        id: root_id.to_string(),
        label: format!("Forest ({})", ast.roots.len()),
        status: if ast.is_complete() {
            "complete"
        } else {
            "partial"
        }
        .to_string(),
        reconstructed: None,
        meta: NodeMeta {
            kind: "forest".to_string(),
            value: None,
            binding: None,
            production: None,
            typing_rule: None,
            inferred_type: None,
            context: vec![],
            constraints: vec![],
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
            TreeStatus::Partial(_) => (true, "partial"), // Partial is OK for incomplete trees
            TreeStatus::Malformed => (false, "malformed"),
            TreeStatus::TooDeep => (false, "too deep"),
        };

        // Extract context from the tree (for let expressions etc)
        let context = extract_context_from_tree(root, grammar);

        // Reconstruct input text from the tree
        let root_node = Node::NonTerminal(root.clone());
        let reconstructed = reconstruct_input(&root_node);

        edges.push(GraphEdge {
            from: root_id.to_string(),
            to: child_id.clone(),
            label: Some(format!("alt {}", i)),
            style: if well_typed { "solid" } else { "dashed" }.to_string(),
        });

        // Collect per-node typing metadata (context extensions / inferred types / constraints)
        let typing_meta = collect_typing_meta_with_root_id(&child_id, &root_node, grammar);

        walk_nt(
            &child_id,
            root,
            grammar,
            &mut nodes,
            &mut edges,
            well_typed,
            &typing_meta,
        );

        let node_count = nodes.len() - node_count_before;

        // Add to reconstructed inputs list
        reconstructed_inputs.push(ReconstructedInput {
            tree_index: i,
            text: reconstructed.clone(),
            complete: tree_complete,
        });

        trees.push(TreeInfo {
            id: child_id,
            index: i,
            complete: tree_complete,
            well_typed,
            type_status: type_status.to_string(),
            node_count,
            context,
            reconstructed,
        });
    }

    GraphData {
        nodes,
        edges,
        trees,
        reconstructed_inputs,
    }
}

fn walk_nt(
    node_id: &str,
    nt: &NonTerminal,
    grammar: &Grammar,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
    tree_well_typed: bool,
    typing_meta: &HashMap<String, NodeTypingMeta>,
) {
    let is_complete = nt.is_complete();

    let prod_info = ProductionInfo {
        rhs: nt
            .production
            .rhs
            .iter()
            .map(|s| format!("{:?}", s))
            .collect(),
        cursor: nt.children.len(),
        complete: is_complete,
    };

    // Get typing rule info if present
    let typing_rule = nt.production.rule.as_ref().and_then(|rule_name| {
        grammar
            .typing_rules
            .get(rule_name)
            .map(|rule| TypingRuleInfo {
                name: rule_name.clone(),
                premises: rule.premises.iter().map(|p| p.to_string()).collect(),
                conclusion: rule.conclusion.to_string(),
                pretty: rule.pretty(0),
            })
    });

    let label = if let Some(ref rule) = nt.production.rule {
        format!("{}({})", nt.name, rule)
    } else {
        nt.name.clone()
    };

    // Status combines syntactic completeness and type validity
    let status = if !tree_well_typed {
        "error" // Type error in this tree
    } else if is_complete {
        "complete"
    } else {
        "partial"
    };

    let inferred_type = if tree_well_typed {
        match check_tree(nt, grammar) {
            TreeStatus::Valid(ty) | TreeStatus::Partial(ty) => Some(format!("{ty}")),
            TreeStatus::Malformed | TreeStatus::TooDeep => None,
        }
    } else {
        None
    };

    let (context, constraints) = typing_meta
        .get(node_id)
        .map(|m| (m.context.clone(), m.constraints.clone()))
        .unwrap_or((vec![], vec![]));

    // Reconstruct input from this subtree
    let reconstructed = reconstruct_node_input(&Node::NonTerminal(nt.clone()));

    nodes.push(GraphNode {
        id: node_id.to_string(),
        label,
        status: status.to_string(),
        reconstructed: Some(reconstructed),
        meta: NodeMeta {
            kind: "nonterminal".to_string(),
            value: Some(nt.name.clone()),
            binding: nt.binding.clone(),
            production: Some(prod_info),
            typing_rule,
            inferred_type,
            context,
            constraints,
            alternative: nt.alternative_index,
        },
    });

    for (i, child) in nt.children.iter().enumerate() {
        let child_id = format!("{}_{}", node_id, i);

        // Get the symbol name for edge label
        let edge_label = nt.production.rhs.get(i).map(|sym| match sym {
            crate::logic::grammar::Symbol::Nonterminal { name, binding } => {
                if let Some(b) = binding {
                    format!("{}[{}]", name, b)
                } else {
                    name.clone()
                }
            }
            crate::logic::grammar::Symbol::Terminal { binding, .. } => {
                if let Some(b) = binding {
                    format!("[{}]", b)
                } else {
                    String::new()
                }
            }
        });

        edges.push(GraphEdge {
            from: node_id.to_string(),
            to: child_id.clone(),
            label: edge_label,
            style: "solid".to_string(),
        });
        walk_node(
            &child_id,
            child,
            grammar,
            nodes,
            edges,
            tree_well_typed,
            typing_meta,
        );
    }
}

fn walk_node(
    node_id: &str,
    node: &Node,
    grammar: &Grammar,
    nodes: &mut Vec<GraphNode>,
    edges: &mut Vec<GraphEdge>,
    tree_well_typed: bool,
    typing_meta: &HashMap<String, NodeTypingMeta>,
) {
    match node {
        Node::Terminal(t) => {
            let (value, binding, status, reconstructed) = match t {
                Terminal::Complete { value, binding, .. } => {
                    let s = if tree_well_typed { "terminal" } else { "error" };
                    (value.clone(), binding.clone(), s, value.clone())
                }
                Terminal::Partial { value, binding, .. } => {
                    let recon = if value.is_empty() {
                        "◌".to_string()
                    } else {
                        format!("{}…", value)
                    };
                    (value.clone(), binding.clone(), "partial", recon)
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
                reconstructed: Some(reconstructed),
                meta: NodeMeta {
                    kind: "terminal".to_string(),
                    value: Some(value),
                    binding,
                    production: None,
                    typing_rule: None,
                    inferred_type: None,
                    context: vec![],
                    constraints: vec![],
                    alternative: 0,
                },
            });
        }
        Node::NonTerminal(nt) => {
            walk_nt(
                node_id,
                nt,
                grammar,
                nodes,
                edges,
                tree_well_typed,
                typing_meta,
            );
        }
    }
}

#[derive(Debug, Clone)]
struct NodeTypingMeta {
    context: Vec<ContextEntry>,
    constraints: Vec<ConstraintInfo>,
}

fn collect_typing_meta_with_root_id_rec(
    root_id: &str,
    node: &Node,
    grammar: &Grammar,
    out: &mut HashMap<String, NodeTypingMeta>,
) {
    if let Node::NonTerminal(nt) = node {
        // Record context extensions for this non-terminal (if any)
        let mut ctx_entries: Vec<ContextEntry> = vec![];
        let mut constraints: Vec<ConstraintInfo> = vec![];

        if let Some(rule_name) = &nt.production.rule {
            if let Some(rule) = grammar.typing_rules.get(rule_name) {
                // Surface premises + conclusion as constraints for quick inspection.
                for p in &rule.premises {
                    constraints.push(ConstraintInfo {
                        kind: "premise".into(),
                        text: p.to_string(),
                    });
                }
                constraints.push(ConstraintInfo {
                    kind: "conclusion".into(),
                    text: rule.conclusion.to_string(),
                });

                // Also include the full inference rule in pretty multiline form.
                // The UI treats this as preformatted text.
                constraints.push(ConstraintInfo {
                    kind: "rule".into(),
                    text: rule.pretty(0),
                });

                // Context output extensions (let-style)
                if let Some(output) = &rule.conclusion.context.output {
                    for (var_name, ty_expr) in &output.extensions {
                        if let Some(paths) = grammar.binding_map.get(var_name, rule_name) {
                            for path in paths {
                                if let Ok(results) =
                                    crate::logic::binding::resolve_binding_path(node, path)
                                {
                                    if let Some(res) = results.first() {
                                        if let Some(name) = get_node_text(res.node()) {
                                            ctx_entries.push(ContextEntry {
                                                name,
                                                ty: format!("{ty_expr}"),
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

        out.insert(
            root_id.to_string(),
            NodeTypingMeta {
                context: ctx_entries,
                constraints,
            },
        );

        for (i, child) in nt.children.iter().enumerate() {
            let child_id = format!("{}_{}", root_id, i);
            collect_typing_meta_with_root_id_rec(&child_id, child, grammar, out);
        }
    }
}

fn collect_typing_meta_with_root_id(
    root_id: &str,
    root: &Node,
    grammar: &Grammar,
) -> HashMap<String, NodeTypingMeta> {
    let mut out: HashMap<String, NodeTypingMeta> = HashMap::new();
    collect_typing_meta_with_root_id_rec(root_id, root, grammar, &mut out);
    out
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
                                if let Ok(results) =
                                    crate::logic::binding::resolve_binding_path(node, path)
                                {
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

/// Reconstruct the input string from terminals in a tree.
/// Shows placeholder markers for incomplete/missing parts.
fn reconstruct_input(node: &Node) -> String {
    match node {
        Node::Terminal(Terminal::Complete { value, .. }) => value.clone(),
        Node::Terminal(Terminal::Partial { value, .. }) => {
            if value.is_empty() {
                "◌".to_string() // Placeholder for empty partial
            } else {
                format!("{}…", value) // Partial value with ellipsis
            }
        }
        Node::NonTerminal(nt) => {
            let mut parts: Vec<String> = Vec::new();
            for child in &nt.children {
                parts.push(reconstruct_input(child));
            }
            // If tree is incomplete, show what's missing
            if !nt.is_complete() && nt.children.len() < nt.production.rhs.len() {
                parts.push("◌".to_string());
            }
            parts.join("")
        }
    }
}

/// Reconstruct input for a specific node (used for per-node reconstruction)
fn reconstruct_node_input(node: &Node) -> String {
    reconstruct_input(node)
}
