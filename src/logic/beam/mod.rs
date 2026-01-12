//! Beam Search for Efficient Typed Completion
//!
//! This module implements beam search algorithm for efficient completion exploration.
//! Unlike BFS which explores all states, beam search keeps only to top K
//! best states at each depth level, dramatically reducing search space.
//!
//! ## Key Features
//!
//! - **Scoring**: States are ranked by completeness, typing quality, and simplicity
//! - **Pruning**: Only top K states are kept at each depth level
//! - **Type-aware**: Scores consider how well-typed a partial AST is
//!
//! ## Performance
//!
//! For grammars with high branching factor (like STLC, IMP), beam search
//! can reduce state exploration from O(b^d) to O(k*d) where:
//! - b = branching factor
//! - d = max depth
//! - k = beam width

pub mod config;
pub mod scoring;
pub mod search;
pub mod state;
pub mod test;

pub use config::BeamConfig;
pub use search::beam_complete;
pub use state::{BeamResult, BeamState};
