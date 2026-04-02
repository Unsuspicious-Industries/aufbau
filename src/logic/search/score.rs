use crate::logic::fusion::ast::FusionForest;
use crate::logic::grammar::Grammar;
use crate::logic::search::distance::levenshtein;
use std::collections::HashSet;
use std::ops::{Add, Sub};

/// Score component with monoid structure
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Score {
    pub val: usize,
}

impl Score {
    #[inline]
    pub const fn new(val: usize) -> Self {
        Self { val }
    }

    #[inline]
    pub const fn inv(val: usize) -> Self {
        Self {
            val: usize::MAX - val,
        }
    }

    #[inline]
    pub const fn scale(val: f64, scale: usize) -> Self {
        Self {
            val: (val * scale as f64) as usize,
        }
    }
}

impl Add for Score {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Self {
            val: self.val.saturating_add(rhs.val),
        }
    }
}

impl Sub for Score {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Self {
            val: self.val.saturating_sub(rhs.val),
        }
    }
}

/// Composite search score with lexicographic ordering
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Total {
    pub complete: Score,
    pub open_slots: Score,
    pub depth: Score,
    pub compactness: Score,
    pub fullness: Score,
    pub completeness: Score,
    pub terminals: Score,
    pub shortness: Score,
}

impl Total {
    pub fn map_fullness<F: FnOnce(Score) -> Score>(self, f: F) -> Self {
        Self {
            fullness: f(self.fullness),
            ..self
        }
    }
}

/// Scoring strategy
pub(crate) trait Scorer {
    fn score(&self, ast: &FusionForest<'_>, grammar: &Grammar) -> Total;
}

/// Default scorer
pub(crate) struct DefaultScorer;

impl Scorer for DefaultScorer {
    fn score(&self, ast: &FusionForest<'_>, grammar: &Grammar) -> Total {
        Total {
            complete: Score::new(usize::from(ast.is_complete()) * 1_000_000),
            open_slots: Score::inv(ast.min_open_slots(grammar)),
            shortness: Score::inv(ast.text().chars().count()),
            depth: Score::inv(ast.min_tree_depth()),
            compactness: Score::inv(ast.node_count()),
            fullness: Score::scale(ast.production_fullness_score(grammar), 1000),
            completeness: Score::scale(ast.completeness_score(), 1000),
            terminals: Score::new(ast.leaf_terminal_count()),
        }
    }
}

pub(crate) fn score(ast: &FusionForest<'_>, grammar: &Grammar) -> Total {
    DefaultScorer.score(ast, grammar)
}

/// Reranking strategy
pub(crate) trait Reranker {
    fn rerank(&self, base: Total, input: &str, popped: &HashSet<String>) -> Total;
}

/// Distance-based reranker
pub(crate) struct DistRerank {
    threshold: usize,
    penalty: usize,
}

impl DistRerank {
    pub const fn new(threshold: usize, penalty: usize) -> Self {
        Self { threshold, penalty }
    }

    pub const fn default() -> Self {
        Self::new(3, 500)
    }
}

impl Reranker for DistRerank {
    fn rerank(&self, base: Total, input: &str, popped: &HashSet<String>) -> Total {
        if popped.is_empty() {
            return base;
        }

        let min_dist = popped
            .iter()
            .map(|p| levenshtein(input, p))
            .min()
            .unwrap_or(usize::MAX);

        if min_dist < self.threshold {
            base.map_fullness(|s| s - Score::new(self.penalty))
        } else {
            base
        }
    }
}

pub(crate) fn rerank(base: Total, input: &str, popped: &HashSet<String>) -> Total {
    DistRerank::default().rerank(base, input, popped)
}
