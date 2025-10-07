use super::typing::BoundType;
use crate::logic::typing::rule::ConclusionKind;
use crate::logic::typing::{Conclusion, Premise, TypeSetting, TypingJudgment, TypingRule};

use crate::logic::partial::{Alt, NonTerminal, ParsedNode, PartialAST, Slot};

/// A bound typing rule over PartialAST nodes
#[derive(Clone, Debug)]
pub struct BoundTypingRule {
    pub name: String,
    pub premises: Vec<BoundPremise>,
    pub conclusion: BoundConclusion,
}

/// A bound premise over PartialAST nodes
#[derive(Debug, Clone)]
pub struct BoundPremise {
    pub setting: Option<BoundTypeSetting>,
    pub judgment: Option<BoundTypingJudgment>,
}

/// A bound type setting with resolved partial-node references
#[derive(Debug, Clone)]
pub struct BoundTypeSetting {
    pub name: String,
    pub extensions: Vec<BoundTypeAscription>,
}

/// A bound type ascription linking a partial node to a type
#[derive(Debug, Clone)]
pub struct BoundTypeAscription {
    pub node: NonTerminal,
    pub ty: BoundType,
}

/// A bound typing judgment with resolved partial nodes
#[derive(Debug, Clone)]
pub enum BoundTypingJudgment {
    Ascription(BoundTypeAscription),
    Membership(NonTerminal, String),
}

/// Context specification for a bound conclusion (optional input/output context transforms)
#[derive(Debug, Clone, Default)]
pub struct BoundConclusionContext {
    pub input: String,
    pub output: Option<BoundTypeSetting>,
}

/// The kind of bound conclusion: either a type or a context lookup Γ(x)
#[derive(Debug, Clone)]
pub enum BoundConclusionKind {
    Type(BoundType),
    ContextLookup(String, NonTerminal),
}

/// A bound conclusion with resolved components
#[derive(Debug, Clone)]
pub struct BoundConclusion {
    pub context: BoundConclusionContext,
    pub kind: BoundConclusionKind,
}

/// Trait for resolving rule bindings over PartialASTs
pub trait BindingResolver {
    fn resolve_rule(
        &self,
        rule: &TypingRule,
        node: &NonTerminal,
    ) -> Result<BoundTypingRule, String>;
    fn resolve_premise(
        &self,
        premise: &Premise,
        node: &NonTerminal,
    ) -> Result<BoundPremise, String>;
    fn resolve_conclusion(
        &self,
        conclusion: &Conclusion,
        node: &NonTerminal,
    ) -> Result<BoundConclusion, String>;
}
