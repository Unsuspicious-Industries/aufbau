use crate::logic::fusion::binding::{Bindings, bind_node, bind_terminal};
use crate::logic::grammar::Segment;
use crate::logic::parse::arena::{
    ArenaNode, CtxId, NodeStatus, NtId, PathId, ProdId, TypeId, TypeStatus,
};
use crate::regex::Regex;

/// A binding value resolved during typing.
///
/// `path` identifies the binding's location in the grammar (prod + child + alt).
/// This is deterministic and unique within a production, unlike arena node IDs
/// which depend on allocation order.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct BindingValue {
    pub name: String,
    pub path: PathId,
    pub value: Option<String>,
    pub ty: Option<TypeId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypingContextSummary {
    pub ctx: CtxId,
    pub expected: Option<TypeId>,
    pub path: Option<PathId>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypingState {
    pub ctx: CtxId,
    pub expected: Option<TypeId>,
    pub inferred: Option<TypeId>,
    pub path: Option<PathId>,
    pub bindings: Vec<BindingValue>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TransitionError {
    Rejected,
    TooDeep,
}

pub type TransitionResult<T> = Result<T, TransitionError>;

pub trait TypingRuntime {
    /// Time: implementation-defined. Space: implementation-defined.
    fn enter_nonterminal(&self, nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState>;

    /// Time: implementation-defined. Space: implementation-defined.
    fn prepare_child(
        &self,
        prod: ProdId,
        child_idx: usize,
        binding: Option<&str>,
        state: &TypingState,
        parsed_children: &[TypingState],
    ) -> TransitionResult<TypingState>;

    /// Time: implementation-defined. Space: implementation-defined.
    fn descend(
        &self,
        state: &TypingState,
        path: PathId,
        binding: Option<&str>,
    ) -> TransitionResult<TypingState>;

    /// Time: implementation-defined. Space: implementation-defined.
    fn consume_terminal(
        &self,
        state: &TypingState,
        regex: &Regex,
        segment: Option<&Segment>,
    ) -> TransitionResult<TypingState>;

    /// Time: implementation-defined. Space: implementation-defined.
    fn finish_production(
        &self,
        prod: ProdId,
        state: &TypingState,
        children: &[TypingState],
        status: NodeStatus,
    ) -> TransitionResult<TypingState>;

    fn finish_terminal_child(
        &self,
        descended: &TypingState,
        path: PathId,
        regex: &Regex,
        segment: Option<&Segment>,
    ) -> TransitionResult<TypingState> {
        let next = self.consume_terminal(descended, regex, segment)?;
        Ok(TypingState {
            bindings: bind_terminal(
                &descended.bindings,
                path,
                segment.map(|s| s.as_str().to_string()),
                next.inferred,
            ),
            ..next
        })
    }

    fn finish_node_child(
        &self,
        descended: &TypingState,
        path: PathId,
        child: &ArenaNode,
        child_bindings: &[BindingValue],
        segments: &[Segment],
    ) -> TypingState {
        let inferred = match child.ty {
            TypeStatus::Valid(ty) | TypeStatus::Partial(ty) => ty,
        };
        TypingState {
            ctx: child.env_out,
            expected: descended.expected,
            inferred: Some(inferred),
            path: Some(path),
            bindings: (Bindings::from(bind_node(
                &descended.bindings,
                path,
                child.span,
                child.ty,
                segments,
            )) + child_bindings)
                .into(),
        }
    }
}
