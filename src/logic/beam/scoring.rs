//! Scoring Heuristics for Beam Search
//!
//! This module provides scoring functions that help beam search
//! prioritize the most promising states.
//!
//! ## Key Heuristics
//!
//! 1. **Completeness**: How close is the AST to being complete?
//! 2. **Tree Size**: Prefer smaller trees (fewer nodes = less complexity)
//! 3. **Typing Quality**: How well-typed is the AST?
//! 4. **Simplicity**: Prefer shorter paths

use crate::logic::PartialAST;
use crate::logic::partial::Node;

/// Score for a beam search state
///
/// Higher scores = more promising states
#[derive(Debug, Clone, Copy)]
pub struct StateScore {
    /// Overall score (higher is better)
    pub overall: f64,

    /// Individual components (for debugging)
    pub completeness: f64,
    pub tree_size: f64,
    pub typing_quality: f64,
    pub simplicity: f64,
}

impl StateScore {
    /// Create a new score from components
    pub fn new(completeness: f64, tree_size: f64, typing_quality: f64, simplicity: f64) -> Self {
        let overall = completeness + tree_size + typing_quality + simplicity;
        Self {
            overall,
            completeness,
            tree_size,
            typing_quality,
            simplicity,
        }
    }
}

/// Estimate how complete an AST is (0.0 to 1.0)
///
/// Higher score means closer to a complete tree
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

    // Normalize to 0-1 range
    (score / total_nodes as f64).min(1.0)
}

/// Count completeness score by traversing AST
fn count_completeness(node: &Node, score: &mut f64, total: &mut usize) {
    *total += 1;

    match node {
        Node::Terminal(t) => match t {
            crate::logic::partial::Terminal::Complete { .. } => {
                // Complete terminals are good
                *score += 1.0;
            }
            crate::logic::partial::Terminal::Partial { value, .. } => {
                // Partial terminals: shorter partials are better
                let partial_len = value.len();
                *score += 0.5 * (1.0 / (partial_len as f64 + 1.0));
            }
        },
        Node::NonTerminal(nt) => {
            // NonTerminals are incomplete by definition
            // Empty NT is very incomplete
            if nt.children.is_empty() {
                *score += 0.1;
            } else {
                // Score based on child completeness
                for child in &nt.children {
                    count_completeness(child, score, total);
                }
            }
        }
    }
}

/// Estimate tree size (lower is better, normalized to negative score)
///
/// We want to PENALIZE large trees, so this returns a negative value
/// where smaller trees get less penalty (higher score).
///
/// Formula: -1.0 * (tree_size / max_expected_size)
pub fn estimate_tree_size_penalty(tree: &PartialAST, max_expected_size: usize) -> f64 {
    let tree_size = count_tree_nodes(tree);

    if tree_size == 0 {
        return 0.0; // Empty tree - no penalty
    }

    // Normalize to 0-1 range, then invert
    // Larger trees get more negative score
    let normalized_size = (tree_size as f64 / max_expected_size as f64).min(1.0);
    -1.0 * normalized_size
}

/// Count total number of nodes in AST
fn count_tree_nodes(tree: &PartialAST) -> usize {
    let mut count = 0;
    for root in &tree.roots {
        count_nodes_in_subtree(&Node::NonTerminal(root.clone()), &mut count);
    }
    count
}

/// Count nodes recursively
fn count_nodes_in_subtree(node: &Node, count: &mut usize) {
    *count += 1;
    if let Node::NonTerminal(nt) = node {
        for child in &nt.children {
            count_nodes_in_subtree(child, count);
        }
    }
}

/// Estimate typing quality of AST (0.0 to 1.0)
///
/// Higher score means more well-typed
///
/// Note: This is a heuristic. Proper type checking requires
/// the full grammar which may not be available during scoring.
pub fn estimate_typing_quality(tree: &PartialAST) -> f64 {
    // Use a simple heuristic: trees that parser accepted are likely
    // type-valid. We give higher scores to trees with:
    // 1. More complete roots
    // 2. Fewer roots (less ambiguity)

    let total_roots = tree.roots.len();
    let complete_roots = tree.roots.iter().filter(|r| r.is_complete()).count();

    if total_roots == 0 {
        return 0.0;
    }

    // Higher score for more complete roots, penalized by ambiguity
    let completeness_ratio = complete_roots as f64 / total_roots as f64;
    let ambiguity_penalty = if total_roots > 1 {
        // More roots = more ambiguity = lower score
        -0.2 * (total_roots as f64 - 1.0)
    } else {
        0.0
    };

    (completeness_ratio + ambiguity_penalty).max(0.0).min(1.0)
}

/// Estimate simplicity score (0.0 to 1.0)
///
/// Higher score means shorter/simpler paths are preferred
///
/// This is typically the depth in the beam search, but can be
/// calculated from tree structure too.
pub fn estimate_simplicity(depth: usize, max_depth: usize) -> f64 {
    // Prefer shallower depths
    let normalized_depth = depth as f64 / max_depth as f64;
    1.0 - normalized_depth
}

/// Calculate final score for a beam state
///
/// Combines all heuristics with configurable weights
pub fn calculate_score(
    tree: &PartialAST,
    depth: usize,
    config: &super::config::BeamConfig,
) -> StateScore {
    let completeness = estimate_completeness(tree);

    // Tree size penalty: strongly penalize large trees
    // max_expected_size = depth * 10 (reasonable upper bound)
    let max_expected_size = (depth + 1) * 10;
    let tree_size = estimate_tree_size_penalty(tree, max_expected_size);

    // Typing quality: assume reasonably typed if parser accepted
    let typing_quality = estimate_typing_quality(tree);

    // Simplicity: prefer shorter depths
    let simplicity = estimate_simplicity(depth, config.max_depth);

    // Weighted combination
    let weighted_completeness = config.completeness_weight * completeness;
    let weighted_tree_size = config.typing_weight * tree_size; // Use typing_weight for tree size too
    let weighted_typing = config.typing_weight * typing_quality;
    let weighted_simplicity = config.simplicity_weight * simplicity;

    StateScore::new(
        weighted_completeness,
        weighted_tree_size,
        weighted_typing,
        weighted_simplicity,
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_completeness_scoring() {
        use crate::logic::grammar::Grammar;

        let spec = r#"
            A ::= 'a' | 'a' B
            B ::= 'b'
            start ::= A
        "#;
        let grammar = Grammar::load(spec).unwrap();

        // Empty input should have low completeness
        let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
        let empty = parser.partial("").unwrap();
        let empty_score = estimate_completeness(&empty);
        assert!(empty_score < 0.5);

        // Complete input should have high completeness
        let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
        let complete = parser.partial("ab").unwrap();
        let complete_score = estimate_completeness(&complete);
        assert!(complete_score > 0.5);
    }

    #[test]
    fn test_tree_size_penalty() {
        use crate::logic::grammar::Grammar;

        let spec = r#"
            A ::= 'a' | 'a' B
            B ::= 'b'
            start ::= A
        "#;
        let grammar = Grammar::load(spec).unwrap();

        // Small tree should have small penalty
        let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
        let small = parser.partial("a").unwrap();
        let small_penalty = estimate_tree_size_penalty(&small, 10);
        assert!(small_penalty > -1.0); // Should be close to 0

        // Large tree should have large penalty
        let mut parser = crate::logic::partial::parse::Parser::new(grammar.clone());
        let large = parser.partial("aaaaabbbbbb").unwrap();
        let large_penalty = estimate_tree_size_penalty(&large, 10);
        assert!(large_penalty < -0.5); // Should be more negative
    }
}
