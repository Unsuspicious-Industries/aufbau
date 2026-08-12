// The pipeline, in order. Each stage is a top-level module: `regex` lexes,
// `grammar` holds the SPG, `parse` derives, `ast` is the derivation, `typing`
// constrains it, `synth` drives the whole thing for a caller.
pub mod ast;
pub mod grammar;
pub mod parse;
pub mod regex;
pub mod synth;
pub mod typing;

pub mod error;
pub mod ffi;
pub mod path;
pub mod semantics;
pub mod validation;

#[macro_use]
mod utils;

#[cfg(test)]
pub mod testing;

// Re-export debug macros at crate level
pub mod debug;
pub use debug::*;

pub mod complexity;
