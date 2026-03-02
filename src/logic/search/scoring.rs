use crate::logic::partial::Node;
use crate::logic::PartialAST;

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

pub fn calculate_score(tree: &PartialAST, depth: usize, max_depth: usize) -> StateScore {
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
pub fn estimate_completeness(tree: &PartialAST) -> f64 {
    tree.roots
        .iter()
        .map(|root| {
            let mut score = 0.0;
            let mut total = 0;
            count_completeness(&Node::NonTerminal(root.clone()), &mut score, &mut total);
            if total == 0 {
                0.0
            } else {
                ((score / total as f64) * 2.0).min(2.0)
            }
        })
        .fold(0.0_f64, f64::max)
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
                // unexpanded — not counted as complete at all
                *score += 0.0;
            } else {
                for child in &nt.children {
                    count_completeness(child, score, total);
                }
            }
        }
    }
}

/// RMS of per-production fill ratios for the best (most filled) root.
pub fn estimate_production_fullness(tree: &PartialAST) -> f64 {
    tree.roots
        .iter()
        .map(|root| {
            let mut sum_sq = 0.0;
            let mut count = 0;
            collect_fullness(&Node::NonTerminal(root.clone()), &mut sum_sq, &mut count);
            if count == 0 {
                0.0
            } else {
                (sum_sq / count as f64).sqrt()
            }
        })
        .fold(0.0_f64, f64::max)
}

fn collect_fullness(node: &Node, sum_sq: &mut f64, count: &mut usize) {
    if let Node::NonTerminal(nt) = node {
        let expected = nt.production.rhs.len();
        if expected > 0 && !nt.children.is_empty() {
            let filled = nt.children.len().min(expected);
            let ratio = filled as f64 / expected as f64;
            *sum_sq += ratio * ratio;
            *count += 1;
        }
        for child in &nt.children {
            collect_fullness(child, sum_sq, count);
        }
    }
}

/// Bonus for tokens consumed by the best root.
/// sqrt scale: 1 token→0.25, 4→0.5, 9→0.75, 16→1.0.
pub fn estimate_token_length_bonus(tree: &PartialAST) -> f64 {
    let max_tokens = tree
        .roots
        .iter()
        .map(|root| root.consumed_segments)
        .max()
        .unwrap_or(0);

    if max_tokens == 0 {
        return 0.0;
    }

    (max_tokens as f64).sqrt() * 0.25
}

/// THE key signal: open slots on the BEST root (min open slots = most complete alternative).
/// Each unfilled slot = one required future step. -0.3 per slot, uncapped so spread is real.
/// A state 1 step away beats one 10 steps away by 2.7 points.
pub fn estimate_open_slots_penalty(tree: &PartialAST) -> f64 {
    let min_open = tree
        .roots
        .iter()
        .map(|root| {
            let mut open = 0usize;
            count_open_slots(&Node::NonTerminal(root.clone()), &mut open);
            open
        })
        .min()
        .unwrap_or(0);
    -(min_open as f64 * 0.3)
}

fn count_open_slots(node: &Node, open: &mut usize) {
    if let Node::NonTerminal(nt) = node {
        if nt.children.is_empty() {
            // unexpanded placeholder — this is an open slot
            *open += 1;
        } else {
            // count unfilled rhs positions as open
            let expected = nt.production.rhs.len();
            let filled = nt.children.len().min(expected);
            *open += expected.saturating_sub(filled);
            for child in &nt.children {
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

/// Light recursion penalty on the shallowest root — open_slots already discourages deep nesting.
pub fn estimate_recursion_penalty(tree: &PartialAST, max_depth: usize) -> f64 {
    let min_tree_depth = tree
        .roots
        .iter()
        .map(|root| max_depth_in_node(&Node::NonTerminal(root.clone()), 0))
        .min()
        .unwrap_or(0);
    if min_tree_depth == 0 {
        return 0.0;
    }
    let normalized = (min_tree_depth as f64 / (max_depth as f64 + 1.0)).min(1.0);
    -0.5 * normalized * normalized
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
