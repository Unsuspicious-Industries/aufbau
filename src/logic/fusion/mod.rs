pub mod ast;
pub(crate) mod binding;
pub mod display;
mod meta;
mod runtime;
mod state;
mod synth;
mod typing;

pub use crate::logic::parse::arena::{
    AltId, ChildRef, CtxId, FrontierId, NodeId, NodeStatus, NtId, PathId, ProdId, Span, TypeId,
    TypeStatus,
};
pub use crate::logic::parse::parser::TypedParser;
pub use ast::{FusionAST, FusionChild, FusionNode};
pub use meta::MetaTypedParser;
pub use runtime::RuleRuntime;
pub use state::{DepthMeta, FrontierItem, TypedPrefixError, TypedPrefixState};
pub use synth::Synthesizer;
pub use typing::{
    BindingValue, TransitionError, TransitionResult, TypingContextSummary, TypingRuntime,
    TypingState,
};

#[cfg(test)]
mod tests;
