pub mod ast;
pub mod display;

mod state;

pub mod runtime {
    pub use crate::logic::typing::runtime::RuleRuntime;
}

pub use crate::logic::parse::arena::{
    AltId, Binding, ChildRef, CtxId, Lexeme, NodeId, NodeStatus, NtId, ProdId, Span, TypeId,
};
pub use crate::logic::parse::TypedParser;
pub use crate::logic::typing::runtime::RuleRuntime;
pub use crate::logic::typing::state::{Obligation, TransitionError, TypingRuntime};
pub use crate::logic::typing::core::TreePath;
pub use ast::{FusionAST, FusionChild, FusionNode};
pub use state::{PrefixError, State};

/// Stub for depth config - no longer used but needed for some tests.
#[derive(Debug, Clone, Default)]
pub struct DepthConfig {
    pub start: u16,
    pub max: u16,
    pub factor: f64,
}

impl DepthConfig {
    pub fn default() -> Self {
        Self { start: 4, max: 128, factor: 1.5 }
    }
}
