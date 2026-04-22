pub mod grammar;
pub mod error;
pub mod path;


pub mod debug;
pub mod exp;
pub mod parse;
pub mod structure;
pub mod typing;

pub mod binding;

pub mod synth;

use crate::logic::error::TransitionError;
use crate::logic::parse::arena::{CtxId, NodeStatus, ProdId, TypeId};
use crate::logic::typing::{ContextTransition, Obligations};

pub use grammar::{Segment, Tokenizer};



/// Parser-facing semantic interface.
pub trait TypingRuntime {
    fn descend(
        &self,
        prod: ProdId,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &Obligations,
    ) -> Result<CtxId, TransitionError>;

    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &Obligations,
        status: NodeStatus,
    ) -> Result<(TypeId, Option<ContextTransition>), TransitionError>;

    fn set_segs(&mut self, s: &[Segment]) {}
    fn apply_transform(&self, ctx: CtxId, transform: ContextTransition) -> Result<CtxId, TransitionError>;
}
