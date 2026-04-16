pub mod arena;
pub mod parser;

#[cfg(test)]
mod display;

#[cfg(test)]
mod testing;

#[cfg(test)]
mod tests;

pub use arena::{
    AltId, Binding, ChildRef, CtxId,  NodeId, NodeStatus, NtId, ParseArena, ProdId,
    Span, TypeId,
};
pub use parser::{Item, Tables, Task};

use super::grammar::{Grammar, Segment};

#[derive(Debug)]
pub struct TypedParser<T> {
    pub(crate) grammar: Grammar,
    pub(crate) typing: T,
    pub(crate) arena: ParseArena,
    pub(crate) tables: Tables,
    pub(crate) input: String,
    pub(crate) segments: Vec<Segment>,
}

impl<T> TypedParser<T> {
    pub fn grammar(&mut self) -> &mut Grammar {
        &mut self.grammar
    }

    pub fn typing(&self) -> &T {
        &self.typing
    }

    pub(crate) fn arena(&self) -> &ParseArena {
        &self.arena
    }

    pub(crate) fn segs(&self) -> &[Segment] {
        &self.segments
    }
}
