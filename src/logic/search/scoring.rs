use crate::logic::partial::Node;
use crate::logic::PartialAST;

// heuristics for nice porgram synthesis / completion ranking
// allows for efficient completabilty checking by prioritizing promising paths

#[derive(Debug, Clone, Copy)]
pub struct StateScore {
    pub overall: f64,
    pub completeness: f64,
    pub tree_size: f64,
    pub typing_quality: f64,
    pub simplicity: f64,
    pub recursion: f64,
}

impl StateScore {
    pub fn new(
        completeness: f64,
        tree_size: f64,
        typing_quality: f64,
        simplicity: f64,
        recursion: f64,
    ) -> Self {
        let overall = completeness + tree_size + typing_quality + simplicity + recursion;
        Self {
            overall,
            completeness,
            tree_size,
            typing_quality,
            simplicity,
            recursion,
        }
    }
}

pub fn calculate_score(tree: &PartialAST, depth: usize, max_depth: usize) -> StateScore {
    let completeness = estimate_completeness(tree);
    let max_expected_size = (depth + 1) * 10;
    let tree_size = estimate_tree_size_penalty(tree, max_expected_size);
    let typing_quality = estimate_typing_quality(tree);
    let simplicity = estimate_simplicity(depth, max_depth);
    let recursion = estimate_recursion_penalty(tree, max_depth);
    StateScore::new(
        completeness,
        tree_size,
        typing_quality,
        simplicity,
        recursion,
    )
}

pub fn estimate_completeness(tree: &PartialAST) -> f64 {
    let mut score = 0.0;
    let mut total_nodes = 0;

    for root in &tree.roots {
        count_completeness(
            &Node::NonTerminal(root.clone()),
            &mut score,
            &mut total_nodes,
        );
    }

    if total_nodes == 0 {
        return 0.0;
    }

    (score / total_nodes as f64).min(1.0)
}

fn count_completeness(node: &Node, score: &mut f64, total: &mut usize) {
    *total += 1;

    match node {
        Node::Terminal(t) => match t {
            crate::logic::partial::Terminal::Complete { .. } => {
                *score += 1.0;
            }
            crate::logic::partial::Terminal::Partial { value, .. } => {
                let partial_len = value.len();
                *score += 0.5 * (1.0 / (partial_len as f64 + 1.0));
            }
        },
        Node::NonTerminal(nt) => {
            if nt.children.is_empty() {
                *score += 0.1;
            } else {
                for child in &nt.children {
                    count_completeness(child, score, total);
                }
            }
        }
    }
}

pub fn estimate_tree_size_penalty(tree: &PartialAST, max_expected_size: usize) -> f64 {
    let tree_size = count_tree_nodes(tree);

    if tree_size == 0 {
        return 0.0;
    }

    let normalized_size = (tree_size as f64 / max_expected_size as f64).min(1.0);
    -1.0 * normalized_size
}

fn count_tree_nodes(tree: &PartialAST) -> usize {
    let mut count = 0;
    for root in &tree.roots {
        count_nodes_in_subtree(&Node::NonTerminal(root.clone()), &mut count);
    }
    count
}

fn count_nodes_in_subtree(node: &Node, count: &mut usize) {
    *count += 1;
    if let Node::NonTerminal(nt) = node {
        for child in &nt.children {
            count_nodes_in_subtree(child, count);
        }
    }
}

pub fn estimate_typing_quality(tree: &PartialAST) -> f64 {
    let total = tree.roots.len();
    if total == 0 {
        return 0.0;
    }
    let complete_roots = tree.roots.iter().filter(|r| r.is_complete()).count();
    let completeness_ratio = complete_roots as f64 / total as f64;
    let ambiguity_penalty = if total > 1 {
        -0.2 * (total as f64 - 1.0)
    } else {
        0.0
    };
    (completeness_ratio + ambiguity_penalty).max(0.0).min(1.0)
}

pub fn estimate_simplicity(depth: usize, max_depth: usize) -> f64 {
    let normalized_depth = depth as f64 / max_depth as f64;
    1.0 - normalized_depth
}

pub fn estimate_recursion_penalty(tree: &PartialAST, max_depth: usize) -> f64 {
    let max_tree_depth = max_tree_depth(tree);
    if max_tree_depth == 0 {
        return 0.0;
    }
    let normalized = (max_tree_depth as f64 / (max_depth as f64 + 1.0)).min(1.0);
    let weight = 2.5;
    -1.0 * weight * normalized * normalized
}

fn max_tree_depth(tree: &PartialAST) -> usize {
    let mut max_depth = 0;
    for root in &tree.roots {
        let depth = max_depth_in_node(&Node::NonTerminal(root.clone()), 0);
        if depth > max_depth {
            max_depth = depth;
        }
    }
    max_depth
}

fn max_depth_in_node(node: &Node, depth: usize) -> usize {
    match node {
        Node::Terminal(_) => depth + 1,
        Node::NonTerminal(nt) => {
            let mut max_child = depth + 1;
            for child in &nt.children {
                let child_depth = max_depth_in_node(child, depth + 1);
                if child_depth > max_child {
                    max_child = child_depth;
                }
            }
            max_child
        }
    }
}
