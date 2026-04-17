//! Structural views derived from parser state.

pub mod ast;
pub mod display;

#[cfg(test)]
mod tests;

pub use crate::logic::error::PrefixError;
pub use crate::logic::parse::State;
pub use ast::{FusionAST, FusionChild, FusionNode};
