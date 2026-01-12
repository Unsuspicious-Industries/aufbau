//! Beam State and Result Types

use crate::logic::PartialAST;
use crate::logic::partial::structure::NonTerminal;
use crate::regex::Regex as DerivativeRegex;
use std::cmp::Ordering;

/// A state in the beam search with associated score
///
/// Each state represents a partial AST that could potentially lead to a
/// complete, well-typed completion.
#[derive(Clone, Debug)]
pub struct BeamState {
    /// Current partial AST
    pub tree: PartialAST,

    /// Depth of this state (number of completion tokens added)
    pub depth: usize,

    /// Computed score for this state (higher is better)
    pub score: f64,

    /// Path of tokens taken to reach this state
    pub path: Vec<DerivativeRegex>,
}

impl BeamState {
    /// Create a new beam state
    pub fn new(tree: PartialAST, depth: usize, score: f64) -> Self {
        Self {
            tree,
            depth,
            score,
            path: Vec::new(),
        }
    }

    /// Create a beam state with a path
    pub fn with_path(
        tree: PartialAST,
        depth: usize,
        score: f64,
        path: Vec<DerivativeRegex>,
    ) -> Self {
        Self {
            tree,
            depth,
            score,
            path,
        }
    }
}

/// Implement PartialEq for beam states based on score
impl PartialEq for BeamState {
    fn eq(&self, other: &Self) -> bool {
        self.score.to_bits() == other.score.to_bits()
    }
}

impl Eq for BeamState {}

/// Implement PartialOrd for beam states based on score
///
/// Used by BinaryHeap to prioritize states
impl PartialOrd for BeamState {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.score.partial_cmp(&other.score)
    }
}

/// Implement Ord for beam states
///
/// Used for deterministic ordering when scores are exactly equal
impl Ord for BeamState {
    fn cmp(&self, other: &Self) -> Ordering {
        // Use total_cmp for float comparison to handle NaN
        self.score
            .partial_cmp(&other.score)
            .unwrap_or(Ordering::Equal)
    }
}

/// Result of beam search completion
#[derive(Debug)]
pub enum BeamResult {
    /// Successfully found a complete, well-typed AST
    Success {
        /// The complete input string (original + completion)
        complete_input: String,

        /// The resulting complete AST
        ast: NonTerminal,

        /// Sequence of completion tokens used
        completion_path: Vec<DerivativeRegex>,

        /// Final score of the successful state
        score: f64,

        /// Depth required to reach completion
        depth: usize,
    },

    /// Beam exhausted without finding a complete completion
    Exhausted {
        /// Maximum depth explored
        max_depth: usize,

        /// Total number of states explored
        states_explored: usize,

        /// Best incomplete state found (for debugging)
        best_state: Option<BeamState>,
    },

    /// State exploration limit exceeded
    StateOverflow {
        /// Maximum states limit that was exceeded
        limit: usize,

        /// Number of states explored before hitting limit
        states_explored: usize,
    },

    /// Initial input is invalid (cannot even parse partially)
    Invalid {
        /// Error message describing why input is invalid
        message: String,
    },
}

impl BeamResult {
    /// Check if result was successful
    pub fn is_success(&self) -> bool {
        matches!(self, BeamResult::Success { .. })
    }

    /// Check if beam was exhausted
    pub fn is_exhausted(&self) -> bool {
        matches!(self, BeamResult::Exhausted { .. })
    }

    /// Check if state limit was exceeded
    pub fn is_overflow(&self) -> bool {
        matches!(self, BeamResult::StateOverflow { .. })
    }

    /// Check if input was invalid
    pub fn is_invalid(&self) -> bool {
        matches!(self, BeamResult::Invalid { .. })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_state_ordering() {
        let mut heap = std::collections::BinaryHeap::new();

        let state1 = BeamState::new(PartialAST::new(vec![], String::new()), 1, 0.9);

        let state2 = BeamState::new(PartialAST::new(vec![], String::new()), 1, 0.7);

        heap.push(state1.clone());
        heap.push(state2.clone());

        // Max heap should give highest score first
        let first = heap.pop().unwrap();
        assert_eq!(first.score, 0.9);

        let second = heap.pop().unwrap();
        assert_eq!(second.score, 0.7);
    }

    #[test]
    fn test_state_equality() {
        let state1 = BeamState::new(PartialAST::new(vec![], String::new()), 1, 0.5);

        let state2 = BeamState::new(PartialAST::new(vec![], String::new()), 1, 0.5);

        assert_eq!(state1, state2);

        let state3 = BeamState::new(PartialAST::new(vec![], String::new()), 1, 0.6);

        assert_ne!(state1, state3);
    }
}
