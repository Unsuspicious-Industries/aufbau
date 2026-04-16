use crate::logic::Segment;
use crate::logic::binding::GrammarPath;
use crate::logic::parse::arena::{Binding, CtxId, Lexeme, NodeStatus, ProdId, TypeId};

/// A binding obligation from a typing rule.
///
/// Created at production seed time from the rule's premises and the
/// BindingMap. Paths are stepped as the parser descends; `value` and
/// `actual` are filled when the target terminal/nonterminal is parsed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Obligation {
    pub name: String,
    pub paths: Vec<GrammarPath>,
    pub value: Option<Lexeme>,
    pub actual: Option<TypeId>,
}

impl Obligation {
    pub fn to_binding(&self) -> Binding {
        Binding {
            name: self.name.clone(),
            value: self.value.clone(),
            ty: self.actual,
        }
    }
    pub fn has_matched(&self) -> bool {
        self.value.is_some()
    }
    pub fn matches(&self, dot: usize, alt: usize) -> bool {
        self.paths.iter().any(|p| {
            let s = p.steps();
            // check if we are the last step and indices match
            s.len() == 1 && s[0].i == dot && s[0].a.map_or(true, |a| a == alt)
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TransitionError {
    Rejected,
}

pub type TransitionResult<T> = Result<T, TransitionError>;

/// Syntax-directed typing interface for the Earley parser.
///
/// `descend` is called when entering a child nonterminal.
/// `finalize` is called when a production completes.
pub trait TypingRuntime {
    /// Compute the child context for entering a nonterminal at position `dot`.
    /// The `obligations` carry resolved bindings from earlier children,
    /// enabling context extensions like `Γ[x:τ]`.
    fn descend(
        &self,
        prod: ProdId,
        dot: usize,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &[Obligation],
    ) -> Result<CtxId, TransitionError>;

    /// Evaluate rule premises and resolve conclusion type.
    /// Returns `(type_id, output_ctx_id, typed_complete)`.
    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &[Obligation],
        status: NodeStatus,
    ) -> Result<(TypeId, CtxId, bool), TransitionError>;

    fn set_segs(&mut self, s: &[Segment]) {}
}
