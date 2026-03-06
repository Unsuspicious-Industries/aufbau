use crate::logic::typing::tree::{TypedAST, TypedNode};

// heuristics for nice program synthesis / completion ranking
// allows for efficient completability checking by prioritizing promising paths
//
// Score design: create wide spread so the priority queue has real signal.
//   Positive: completeness (0..2), production_fullness (0..1), token_length (0..~1.5)
//   Negative: open_slots (-0.3 each), recursion (light), simplicity (mild)
//
// Dominant signal is open_slots_penalty: a state needing 1 more token beats
// one needing 10 by ~2.7 points, so the heap focuses on near-complete paths.

#[derive(Debug, Clone, Copy)]
pub struct StateScore {
    pub overall: f64,
    pub completeness: f64,
    pub production_fullness: f64,
    pub token_length: f64,
    pub open_slots: f64,
    pub simplicity: f64,
    pub recursion: f64,
}

impl StateScore {
    pub fn new(
        completeness: f64,
        production_fullness: f64,
        token_length: f64,
        open_slots: f64,
        simplicity: f64,
        recursion: f64,
    ) -> Self {
        let overall =
            completeness + production_fullness + token_length + open_slots + simplicity + recursion;
        Self {
            overall,
            completeness,
            production_fullness,
            token_length,
            open_slots,
            simplicity,
            recursion,
        }
    }
}

pub fn calculate_score(tree: &TypedAST, depth: usize, max_depth: usize) -> StateScore {
    let completeness = estimate_completeness(tree);
    let production_fullness = estimate_production_fullness(tree);
    let token_length = estimate_token_length_bonus(tree);
    let open_slots = estimate_open_slots_penalty(tree);
    let simplicity = estimate_simplicity(depth, max_depth);
    let recursion = estimate_recursion_penalty(tree, max_depth);
    StateScore::new(
        completeness,
        production_fullness,
        token_length,
        open_slots,
        simplicity,
        recursion,
    )
}

/// Fraction of nodes that are fully matched terminals, weighted 2x.
/// Evaluated on the best root (highest score) since roots are alternatives.
pub fn estimate_completeness(tree: &TypedAST) -> f64 {
    tree.roots
        .iter()
        .map(|root| {
            let mut score = 0.0;
            let mut total = 0;
            count_completeness(root, &mut score, &mut total);
            if total == 0 {
                0.0
            } else {
                ((score / total as f64) * 2.0).min(2.0)
            }
        })
        .fold(0.0_f64, f64::max)
}

fn count_completeness(node: &TypedNode, score: &mut f64, total: &mut usize) {
    *total += 1;
    match node {
        TypedNode::Term { remainder, val, .. } => {
            if remainder.is_none() {
                *score += 1.0; // complete terminal
            } else {
                let partial_len = val.len();
                *score += 0.5 * (1.0 / (partial_len as f64 + 1.0));
            }
        }
        TypedNode::Expr { children, .. } => {
            if children.is_empty() {
                *score += 0.0; // unexpanded
            } else {
                for child in children {
                    count_completeness(child, score, total);
                }
            }
        }
    }
}

/// RMS of per-production fill ratios for the best (most filled) root.
pub fn estimate_production_fullness(tree: &TypedAST) -> f64 {
    tree.roots
        .iter()
        .map(|root| {
            let mut sum_sq = 0.0;
            let mut count = 0;
            collect_fullness(root, &mut sum_sq, &mut count);
            if count == 0 {
                0.0
            } else {
                (sum_sq / count as f64).sqrt()
            }
        })
        .fold(0.0_f64, f64::max)
}

fn collect_fullness(node: &TypedNode, sum_sq: &mut f64, count: &mut usize) {
    if let TypedNode::Expr {
        children, rhs_len, ..
    } = node
    {
        let expected = *rhs_len;
        if expected > 0 && !children.is_empty() {
            let filled = children.len().min(expected);
            let ratio = filled as f64 / expected as f64;
            *sum_sq += ratio * ratio;
            *count += 1;
        }
        for child in children {
            collect_fullness(child, sum_sq, count);
        }
    }
}

/// Bonus for tokens consumed — approximated by counting complete leaf terminals.
pub fn estimate_token_length_bonus(tree: &TypedAST) -> f64 {
    let max_tokens = tree
        .roots
        .iter()
        .map(|root| count_leaf_terminals(root))
        .max()
        .unwrap_or(0);
    if max_tokens == 0 {
        return 0.0;
    }
    (max_tokens as f64).sqrt() * 0.25
}

fn count_leaf_terminals(node: &TypedNode) -> usize {
    match node {
        TypedNode::Term { .. } => 1,
        TypedNode::Expr { children, .. } => children.iter().map(count_leaf_terminals).sum(),
    }
}

/// THE key signal: open slots on the BEST root (min open slots = most complete alternative).
pub fn estimate_open_slots_penalty(tree: &TypedAST) -> f64 {
    let min_open = tree
        .roots
        .iter()
        .map(|root| {
            let mut open = 0usize;
            count_open_slots(root, &mut open);
            open
        })
        .min()
        .unwrap_or(0);
    -(min_open as f64 * 0.3)
}

fn count_open_slots(node: &TypedNode, open: &mut usize) {
    if let TypedNode::Expr {
        children, rhs_len, ..
    } = node
    {
        if children.is_empty() {
            *open += 1; // unexpanded placeholder
        } else {
            let expected = *rhs_len;
            let filled = children.len().min(expected);
            *open += expected.saturating_sub(filled);
            for child in children {
                count_open_slots(child, open);
            }
        }
    }
}

/// Mild preference for shallower search depth (earlier solutions).
pub fn estimate_simplicity(depth: usize, max_depth: usize) -> f64 {
    let normalized_depth = depth as f64 / max_depth as f64;
    (1.0 - normalized_depth) * 0.3
}

/// Light recursion penalty on the shallowest root.
pub fn estimate_recursion_penalty(tree: &TypedAST, max_depth: usize) -> f64 {
    let min_tree_depth = tree
        .roots
        .iter()
        .map(|root| max_depth_in_node(root, 0))
        .min()
        .unwrap_or(0);
    if min_tree_depth == 0 {
        return 0.0;
    }
    let normalized = (min_tree_depth as f64 / (max_depth as f64 + 1.0)).min(1.0);
    -0.5 * normalized * normalized
}

fn max_depth_in_node(node: &TypedNode, depth: usize) -> usize {
    match node {
        TypedNode::Term { .. } => depth + 1,
        TypedNode::Expr { children, .. } => {
            let mut max_child = depth + 1;
            for child in children {
                let d = max_depth_in_node(child, depth + 1);
                if d > max_child {
                    max_child = d;
                }
            }
            max_child
        }
    }
}
