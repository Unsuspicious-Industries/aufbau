use super::*;
use crate::logic::{Obligations, SemanticSummary};

#[derive(Clone, Debug, Default)]
pub(crate) struct StubTyping;

impl SemanticRuntime for StubTyping {
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
    ) -> Result<SemanticSummary, TransitionError> {
        Ok(SemanticSummary::new(0, None, true))
    }
    fn apply_effect(&self, ctx: CtxId, _effect: EffectId) -> Result<CtxId, TransitionError> {
        Ok(ctx)
    }

    fn compose_effects(&self, effects: Vec<EffectId>) -> Result<Option<EffectId>, TransitionError> {
        Ok(effects.into_iter().next())
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct RejectingTyping;

impl SemanticRuntime for RejectingTyping {
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
    ) -> Result<SemanticSummary, TransitionError> {
        Err(TransitionError::Rejected)
    }
    fn apply_effect(&self, ctx: CtxId, _effect: EffectId) -> Result<CtxId, TransitionError> {
        Ok(ctx)
    }

    fn compose_effects(&self, effects: Vec<EffectId>) -> Result<Option<EffectId>, TransitionError> {
        Ok(effects.into_iter().next())
    }
}
