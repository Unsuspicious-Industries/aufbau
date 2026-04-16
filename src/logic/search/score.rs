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
///
/// Field order determines priority (first field = highest priority).
/// For complete programs, shortness is prioritized to prefer simpler completions.
/// For incomplete programs, structural metrics guide search toward completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct Total {
    pub complete: Score,
    pub typedness: Score,
    pub open_slots: Score,
    /// Shortness score - boosted significantly for complete programs so that
    /// among valid completions, shorter ones win. For incomplete programs,
    /// this is just the inverted length without boost.
    pub shortness: Score,
    pub depth: Score,
    pub compactness: Score,
    pub fullness: Score,
    pub completeness: Score,
    pub terminals: Score,
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
        let text = ast.text();
        let mut repetitive_penalty = 0;
        let mut last_char = None;
        let mut run_length = 0;

        for c in text.chars() {
            if Some(c) == last_char {
                run_length += 1;
                if run_length > 2 {
                    repetitive_penalty += run_length * 10;
                }
            } else {
                last_char = Some(c);
                run_length = 1;
            }
        }

        let is_complete = ast.is_complete();
        let char_count = text.chars().count() + repetitive_penalty;

        // For complete programs, boost shortness to ensure shorter completions
        // beat longer ones even if the longer has marginally better structure.
        // For incomplete programs, shortness is just a tiebreaker.
        let shortness = if is_complete {
            // Complete: use a high base + inverted length, so shorter wins
            // The base ensures all complete short programs beat incomplete ones
            // on this metric after open_slots comparison.
            Score::new(usize::MAX / 2 + (usize::MAX / 2 - char_count.min(usize::MAX / 2)))
        } else {
            Score::inv(char_count)
        };

        Total {
            complete: Score::new(usize::from(is_complete) * 1_000_000),
            // Strong typedness bonus: prefer states with at least one valid root,
            // then more valid/partial roots. This prevents gibberish branches from
            // outranking structurally plausible typed continuations.
            typedness: {
                let (valid, partial) = ast.typing_quality();
                let has_valid = usize::from(valid > 0);
                Score::new(has_valid * 2_000_000 + valid * 10_000 + partial * 100)
            },
            open_slots: Score::inv(ast.min_open_slots(grammar)),
            shortness,
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
