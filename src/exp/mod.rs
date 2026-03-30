// experiments with the logic module
mod grammar;
pub mod parse;
pub mod safe;
mod synth;

pub use parse::{run, ExpConfig};
