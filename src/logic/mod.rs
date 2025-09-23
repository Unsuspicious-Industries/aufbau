pub mod tokenizer;
pub mod ast;
pub mod grammar;
pub mod partial;

pub mod check;
pub mod typing;
pub mod bind;
pub mod debug;
pub mod recursion;

pub mod tests;

pub mod parser {
    pub use crate::logic::partial::parse::PartialParser;
    pub type Parser = PartialParser;
}



