pub mod arena;
pub mod parser;
pub mod state;

#[cfg(test)]
mod display;

#[cfg(test)]
mod testing;

#[cfg(test)]
mod tests;

pub use crate::logic::error::PrefixError;
pub use arena::{
    AltId, BindingMap, BindingStatus, ChildRef, CtxId, EffectId, EvidenceId, NodeId, NodeStatus,
    NtId, ParseArena, ProdId, Span, TypeId,
};
pub use parser::{Item, Tables, Task};
pub use state::{Next, State};

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

/// Domain-neutral name for the parser. `TypedParser` is kept for existing code.
pub type SemanticParser<T> = TypedParser<T>;

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
