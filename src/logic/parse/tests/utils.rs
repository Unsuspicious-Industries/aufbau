use super::*;
use crate::logic::typing::{ContextTransition, Obligations};

#[derive(Clone, Debug, Default)]
pub(crate) struct StubTyping;

impl TypingRuntime for StubTyping {
    fn descend(
        &self,
        _prod: ProdId,
        _binding: Option<&str>,
        ctx: CtxId,
        _obligations: &Obligations,
    ) -> Result<CtxId, TransitionError> {
        Ok(ctx)
    }

    fn finalize(
        &self,
        _prod: ProdId,
        _ctx: CtxId,
        _obligations: &Obligations,
        _status: NodeStatus,
    ) -> Result<(TypeId, Option<ContextTransition>), TransitionError> {
        Ok((0, Some(ContextTransition::identity())))
    }
    fn apply_transform(
        &self,
        ctx: CtxId,
        _transform: ContextTransition,
    ) -> Result<CtxId, TransitionError> {
        Ok(ctx) // no-op for testing
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct RejectingTyping;

impl TypingRuntime for RejectingTyping {
    fn descend(
        &self,
        _prod: ProdId,
        _binding: Option<&str>,
        ctx: CtxId,
        _obligations: &Obligations,
    ) -> Result<CtxId, TransitionError> {
        Ok(ctx)
    }

    fn finalize(
        &self,
        _prod: ProdId,
        _ctx: CtxId,
        _obligations: &Obligations,
        _status: NodeStatus,
    ) -> Result<(TypeId, Option<ContextTransition>), TransitionError> {
        Err(TransitionError::Rejected)
    }
    fn apply_transform(
        &self,
        ctx: CtxId,
        _transform: ContextTransition,
    ) -> Result<CtxId, TransitionError> {
        Ok(ctx) // no-op for testing
    }
}
