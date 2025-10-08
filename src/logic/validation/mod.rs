/// Validation Module
/// 
/// This module provides validation for the partial parsing system at two levels:
/// 
/// 1. **Structural Validation** (`structural.rs`): 
///    Verifies the tree structure conforms to the schema invariants.
///    These are syntactic properties that can be checked statically.
/// 
/// 2. **Completability Validation** (`completability.rs`):
///    Verifies the semantic property "correctness by completion" via brute-force.
///    This ensures partial trees can actually be completed to valid trees.

// TODO: Write more documentation about validation strategies and when to use them

pub mod completability;
// pub mod structural; // Temporarily disabled - needs update for new PartialAST structure