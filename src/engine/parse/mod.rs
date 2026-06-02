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
use crate::semantics::TypingRuntime;

#[derive(Debug)]
pub struct TypedParser {
    pub(crate) grammar: SPG,
    pub(crate) typing: TypingRuntime,
    pub(crate) arena: ParseArena,
    pub(crate) tables: Tables,
    pub(crate) input: String,
    pub(crate) segments: Vec<Segment>,
}

impl TypedParser {
    pub fn grammar(&mut self) -> &mut SPG {
        &mut self.grammar
    }

    pub fn typing(&self) -> &TypingRuntime {
        &self.typing
    }

    pub(crate) fn arena(&self) -> &ParseArena {
        &self.arena
    }

    pub(crate) fn segs(&self) -> &[Segment] {
        &self.segments
    }
}
