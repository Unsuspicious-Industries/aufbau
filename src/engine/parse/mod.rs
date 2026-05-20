pub mod arena;
pub mod parser;
pub mod state;

#[cfg(test)]
mod display;

#[cfg(test)]
mod testing;

#[cfg(test)]
mod tests;

pub use crate::engine::error::PrefixError;
pub use arena::{
    AltId, BindingMap, BindingStatus, ChildRef, CtxId, EffectId, EvidenceId, NodeId, NodeStatus,
    NtId, ParseArena, ProdId, Span, TypeId,
};
pub use parser::{Item, Tables, Task};
pub use state::{Next, State};

use super::grammar::{SPG, Segment};
use crate::semantics::domain::ConstraintDomain;

#[derive(Debug)]
pub struct TypedParser<D: ConstraintDomain, T> {
    pub(crate) grammar: SPG<D>,
    pub(crate) typing: T,
    pub(crate) arena: ParseArena,
    pub(crate) tables: Tables,
    pub(crate) input: String,
    pub(crate) segments: Vec<Segment>,
}

/// Domain-neutral name for the parser.
pub type SemanticParser<D, T> = TypedParser<D, T>;

impl<D: ConstraintDomain, T> TypedParser<D, T> {
    pub fn grammar(&mut self) -> &mut SPG<D> {
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
