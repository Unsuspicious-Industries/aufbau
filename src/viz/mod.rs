// Visualization module for P7 parser and synthesizer
//
// This module provides web-based visualizations for:
// - Parser: visualize partial AST from parsing incomplete code
// - Synthesizer: step-by-step visualization of code synthesis with completions

mod graph;
mod parser_viz;
mod synth_viz;
mod server;

// Re-export the server function
pub use server::serve;
