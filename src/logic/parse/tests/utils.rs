use super::*;
use crate::logic::typing::state::Obligation;

#[derive(Clone, Debug, Default)]
pub(crate) struct StubTyping;

impl TypingRuntime for StubTyping {
    fn descend(
        &self,
        _prod: ProdId,
        _dot: usize,
        _binding: Option<&str>,
        ctx: CtxId,
        _obligations: &[Obligation],
    ) -> Result<CtxId, TransitionError> {
        Ok(ctx)
    }

    fn finalize(
        &self,
        _prod: ProdId,
        ctx: CtxId,
        _obligations: &[Obligation],
        _status: NodeStatus,
    ) -> Result<(TypeId, CtxId, bool), TransitionError> {
        Ok((0, ctx, true))
    }
}

#[derive(Clone, Debug, Default)]
pub(crate) struct RejectingTyping;

impl TypingRuntime for RejectingTyping {
    fn descend(
        &self,
        _prod: ProdId,
        _dot: usize,
        _binding: Option<&str>,
        ctx: CtxId,
        _obligations: &[Obligation],
    ) -> Result<CtxId, TransitionError> {
        Ok(ctx)
    }

    fn finalize(
        &self,
        _prod: ProdId,
        _ctx: CtxId,
        _obligations: &[Obligation],
        _status: NodeStatus,
    ) -> Result<(TypeId, CtxId, bool), TransitionError> {
        Err(TransitionError::Rejected)
    }
}

pub(crate) fn grammar_with_word() -> Grammar {
    Grammar::load("Start ::= /[a-z]+/").expect("failed to load test grammar")
}
pub(crate) fn grammar_with_direct_left_recursion() -> Grammar {
    Grammar::load("Start ::= A\nA ::= 'a' | A 'x'")
        .expect("failed to load direct left recursion grammar")
}
pub(crate) fn grammar_with_incomplete_production() -> Grammar {
    Grammar::load("Start ::= 'x' 'y'").expect("failed to load incomplete production grammar")
}
pub(crate) fn grammar_with_ambiguous_roots() -> Grammar {
    Grammar::load("Start ::= A | B\nA ::= 'x'\nB ::= 'x'")
        .expect("failed to load ambiguous roots grammar")
}
pub(crate) fn grammar_with_expression() -> Grammar {
    Grammar::load("Factor ::= 'a' | '(' Expr ')'\nTermTail ::= '*' Factor TermTail | ε\nTerm ::= Factor TermTail\nExprTail ::= '+' Term ExprTail | ε\nExpr ::= Term ExprTail\nStart ::= Expr").expect("failed to load expression grammar")
}
