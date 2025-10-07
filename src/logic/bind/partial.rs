use super::typing::BoundType;
use crate::logic::typing::rule::ConclusionKind;
use crate::logic::typing::{Conclusion, Premise, TypeSetting, TypingJudgment, TypingRule};

use crate::logic::partial::{PartialASTNode, PartialNonTerminal};

/// A bound typing rule over PartialAST nodes
#[derive(Clone)]
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
    pub node: PartialNonTerminal,
    pub ty: BoundType,
}

/// A bound typing judgment with resolved partial nodes
#[derive(Debug, Clone)]
pub enum BoundTypingJudgment {
    Ascription(BoundTypeAscription),
    Membership(PartialNonTerminal, String),
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
    ContextLookup(String, PartialNonTerminal),
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
        node: &PartialNonTerminal,
    ) -> Result<BoundTypingRule, String>;
    fn resolve_premise(
        &self,
        premise: &Premise,
        node: &PartialNonTerminal,
    ) -> Result<BoundPremise, String>;
    fn resolve_conclusion(
        &self,
        conclusion: &Conclusion,
        node: &PartialNonTerminal,
    ) -> Result<BoundConclusion, String>;
}

/// Default implementation of binding resolver for PartialASTs
pub struct DefaultBindingResolver;

impl BindingResolver for DefaultBindingResolver {
    fn resolve_rule(
        &self,
        rule: &TypingRule,
        node: &PartialNonTerminal,
    ) -> Result<BoundTypingRule, String> {
        let mut bound_premises = Vec::new();
        for premise in &rule.premises {
            let many = self.resolve_premise_many(premise, node)?;
            bound_premises.extend(many);
        }
        let bound_conclusion = self.resolve_conclusion(&rule.conclusion, node)?;
        Ok(BoundTypingRule {
            name: rule.name.clone(),
            premises: bound_premises,
            conclusion: bound_conclusion,
        })
    }

    fn resolve_premise(
        &self,
        premise: &Premise,
        node: &PartialNonTerminal,
    ) -> Result<BoundPremise, String> {
        let bound_setting = if let Some(setting) = &premise.setting {
            Some(self.resolve_type_setting(setting, node)?)
        } else {
            None
        };

        let bound_judgment = match &premise.judgment {
            Some(TypingJudgment::Ascription((term, ty))) => {
                let var_node = get_nt_binding_partial(node, term.clone()).ok_or_else(|| {
                    format!("Could not resolve binding variable {} in ascription", term)
                })?;
                let resolved_type = bind_type_partial(node, ty.clone()).ok_or_else(|| {
                    format!(
                        "Could not resolve type binding for type {:?} in ascription",
                        ty
                    )
                })?;
                BoundTypingJudgment::Ascription(BoundTypeAscription {
                    node: var_node,
                    ty: resolved_type,
                })
            }
            Some(TypingJudgment::Membership(var, ctx)) => {
                let var_node = get_nt_binding_partial(node, var.clone()).ok_or_else(|| {
                    format!("Could not resolve binding variable {} in membership", var)
                })?;
                return Ok(BoundPremise {
                    setting: bound_setting,
                    judgment: Some(BoundTypingJudgment::Membership(var_node, ctx.clone())),
                });
            }
            None => {
                return Ok(BoundPremise {
                    setting: bound_setting,
                    judgment: None,
                });
            }
        };

        Ok(BoundPremise {
            setting: bound_setting,
            judgment: Some(bound_judgment),
        })
    }

    fn resolve_conclusion(
        &self,
        conclusion: &Conclusion,
        node: &PartialNonTerminal,
    ) -> Result<BoundConclusion, String> {
        match &conclusion.kind {
            ConclusionKind::Type(ty) => {
                let resolved_type = bind_type_partial(node, ty.clone()).ok_or_else(|| {
                    format!(
                        "Could not resolve type binding for type {:?} in conclusion",
                        ty
                    )
                })?;
                let ctx = if !conclusion.context.input.is_empty()
                    || conclusion.context.output.is_some()
                {
                    BoundConclusionContext {
                        input: conclusion.context.input.clone(),
                        output: if let Some(s) = &conclusion.context.output {
                            Some(self.resolve_type_setting(s, node)?)
                        } else {
                            None
                        },
                    }
                } else {
                    BoundConclusionContext::default()
                };
                Ok(BoundConclusion {
                    context: ctx,
                    kind: BoundConclusionKind::Type(resolved_type),
                })
            }
            ConclusionKind::ContextLookup(context, var) => {
                let var_node = get_nt_binding_partial(node, var.clone()).ok_or_else(|| {
                    format!(
                        "Could not resolve binding variable {} in context lookup",
                        var
                    )
                })?;
                Ok(BoundConclusion {
                    context: BoundConclusionContext::default(),
                    kind: BoundConclusionKind::ContextLookup(context.clone(), var_node),
                })
            }
        }
    }
}

impl DefaultBindingResolver {
    fn resolve_type_setting(
        &self,
        setting: &TypeSetting,
        node: &PartialNonTerminal,
    ) -> Result<BoundTypeSetting, String> {
        let mut bound_extensions = Vec::new();
        for (term, ty) in &setting.extensions {
            let var_node = get_nt_binding_partial(node, term.clone()).ok_or_else(|| {
                format!(
                    "Could not resolve binding variable {} in type setting",
                    term
                )
            })?;
            let resolved_type = bind_type_partial(node, ty.clone()).ok_or_else(|| {
                format!(
                    "Could not resolve type binding for type {:?} in type setting",
                    ty
                )
            })?;
            bound_extensions.push(BoundTypeAscription {
                node: var_node,
                ty: resolved_type,
            });
        }
        Ok(BoundTypeSetting {
            name: setting.name.clone(),
            extensions: bound_extensions,
        })
    }

    fn resolve_premise_many(
        &self,
        premise: &Premise,
        node: &PartialNonTerminal,
    ) -> Result<Vec<BoundPremise>, String> {
        let bound_setting = if let Some(setting) = &premise.setting {
            Some(self.resolve_type_setting(setting, node)?)
        } else {
            None
        };

        match &premise.judgment {
            Some(TypingJudgment::Ascription((term, ty))) => {
                let reps = collect_nt_bindings_same_level_partial(node, term);
                let resolved_type = bind_type_partial(node, ty.clone()).ok_or_else(|| {
                    format!(
                        "Could not resolve type binding for type {:?} in ascription",
                        ty
                    )
                })?;
                if !reps.is_empty() {
                    Ok(reps
                        .into_iter()
                        .map(|var_node| BoundPremise {
                            setting: bound_setting.clone(),
                            judgment: Some(BoundTypingJudgment::Ascription(BoundTypeAscription {
                                node: var_node,
                                ty: resolved_type.clone(),
                            })),
                        })
                        .collect())
                } else {
                    if let Some(var_node) = get_nt_binding_partial(node, term.clone()) {
                        Ok(vec![BoundPremise {
                            setting: bound_setting,
                            judgment: Some(BoundTypingJudgment::Ascription(BoundTypeAscription {
                                node: var_node,
                                ty: resolved_type,
                            })),
                        }])
                    } else {
                        Ok(vec![])
                    }
                }
            }
            Some(TypingJudgment::Membership(var, ctx)) => {
                let reps = collect_nt_bindings_same_level_partial(node, var);
                if !reps.is_empty() {
                    Ok(reps
                        .into_iter()
                        .map(|var_node| BoundPremise {
                            setting: bound_setting.clone(),
                            judgment: Some(BoundTypingJudgment::Membership(var_node, ctx.clone())),
                        })
                        .collect())
                } else {
                    if let Some(var_node) = get_nt_binding_partial(node, var.clone()) {
                        Ok(vec![BoundPremise {
                            setting: bound_setting,
                            judgment: Some(BoundTypingJudgment::Membership(var_node, ctx.clone())),
                        }])
                    } else {
                        Ok(vec![])
                    }
                }
            }
            None => Ok(vec![BoundPremise {
                setting: bound_setting,
                judgment: None,
            }]),
        }
    }
}

/// Helpers over PartialAST
fn direct_nonterminal_children(root: &PartialNonTerminal) -> Vec<PartialNonTerminal> {
    let mut out = Vec::new();
    for ch in &root.children {
        if let PartialASTNode::NonTerminal(alts) = ch {
            for alt in alts {
                out.push(alt.clone());
            }
        }
    }
    out
}

fn find_parent_with_binding_level_partial(
    root: &PartialNonTerminal,
    var: &str,
) -> Option<PartialNonTerminal> {
    let direct = direct_nonterminal_children(root);
    let has_any = direct.iter().any(|ch| ch.binding.as_deref() == Some(var));
    if has_any {
        return Some(root.clone());
    }
    for ch in direct {
        if let Some(p) = find_parent_with_binding_level_partial(&ch, var) {
            return Some(p);
        }
    }
    None
}

pub fn collect_nt_bindings_same_level_partial(
    root: &PartialNonTerminal,
    var: &str,
) -> Vec<PartialNonTerminal> {
    if let Some(parent) = find_parent_with_binding_level_partial(root, var) {
        direct_nonterminal_children(&parent)
            .into_iter()
            .filter(|ch| ch.binding.as_deref() == Some(var))
            .collect()
    } else {
        Vec::new()
    }
}

fn extract_terminals_partial(node: &PartialASTNode) -> Vec<String> {
    match node {
        PartialASTNode::Terminal(t) => vec![t.value.clone()],
        PartialASTNode::NonTerminal(alts) => {
            let mut terms = Vec::new();
            for alt in alts {
                for ch in &alt.children {
                    let mut inner = extract_terminals_partial(ch);
                    terms.append(&mut inner);
                }
            }
            terms
        }
    }
}

pub fn extract_terminal_value_partial(node: &PartialASTNode) -> Option<String> {
    let terms = extract_terminals_partial(node);
    let filtered: Vec<String> = terms.into_iter().filter(|t| t != "(" && t != ")").collect();
    match filtered.len() {
        1 => Some(filtered[0].clone()),
        _ => None,
    }
}

fn get_type_value_partial(nt: &PartialNonTerminal) -> Option<BoundType> {
    // Reuse same heuristic as AST version: a single terminal value under this subtree encodes the type atom
    let node = nt.as_node();
    extract_terminal_value_partial(&node).map(BoundType::Atom)
}

pub fn get_nt_binding_partial(
    node: &PartialNonTerminal,
    var: String,
) -> Option<PartialNonTerminal> {
    if let Some(b) = &node.binding {
        if *b == var {
            return Some(node.clone());
        }
    }
    for ch in direct_nonterminal_children(node) {
        if let Some(found) = get_nt_binding_partial(&ch, var.clone()) {
            return Some(found);
        }
    }
    None
}

pub fn get_var_binding_partial(
    node: &PartialNonTerminal,
    var: String,
) -> Result<Option<String>, String> {
    if let Some(binding) = get_nt_binding_partial(node, var) {
        let n = binding.as_node();
        if let Some(value) = extract_terminal_value_partial(&n) {
            return Ok(Some(value));
        } else {
            return Err("Failed to extract terminal value, malformed PartialAST".into());
        }
    }
    Ok(None)
}

pub fn bind_type_partial(
    node: &PartialNonTerminal,
    type_var: crate::logic::typing::Type,
) -> Option<BoundType> {
    use crate::logic::typing::Type;
    match type_var {
        Type::Atom(var) => {
            if var.starts_with('\'') && var.ends_with('\'') {
                let concrete_type = &var[1..var.len() - 1];
                return Some(BoundType::Atom(concrete_type.to_string()));
            }
            if let Some(nt) = get_nt_binding_partial(node, var.clone()) {
                if let Some(full_ty) = get_type_value_partial(&nt) {
                    return Some(full_ty);
                } else {
                    return None;
                }
            }
            None
        }
        Type::Raw(concrete_type) => Some(BoundType::Atom(concrete_type.clone())),
        Type::Arrow(t1, t2) => {
            let b1 = bind_type_partial(node, *t1)?;
            let b2 = bind_type_partial(node, *t2)?;
            Some(BoundType::Arrow(Box::new(b1), Box::new(b2)))
        }
        Type::Tuple(v) => {
            let elements = collect_nt_bindings_same_level_partial(node, &v);
            if elements.is_empty() {
                return Some(BoundType::Tuple(Vec::new()));
            }
            let tuple_elem_types: Vec<BoundType> = elements
                .into_iter()
                .filter_map(|nt| get_type_value_partial(&nt))
                .collect();
            Some(BoundType::Tuple(tuple_elem_types))
        }
        Type::Pointer(t) => {
            let b = bind_type_partial(node, *t)?;
            Some(BoundType::Pointer(Box::new(b)))
        }
        Type::Array(t, size) => {
            let b = bind_type_partial(node, *t)?;
            let size_str = get_var_binding_partial(node, size);
            if size_str.is_err() {
                return None;
            }
            let size_u64 = if let Some(s) = size_str.unwrap() {
                s.parse::<u64>().ok()
            } else {
                None
            };
            match size_u64 {
                Some(n) => Some(BoundType::Array(Box::new(b), n)),
                None => None,
            }
        }
        Type::Empty => Some(BoundType::Empty),
        Type::Not(t) => {
            let b = bind_type_partial(node, *t)?;
            Some(BoundType::Not(Box::new(b)))
        }
        Type::Intersection(t1, t2) => {
            let b1 = bind_type_partial(node, *t1)?;
            let b2 = bind_type_partial(node, *t2)?;
            Some(BoundType::Intersection(Box::new(b1), Box::new(b2)))
        }
        Type::Union(t1, t2) => {
            let b1 = bind_type_partial(node, *t1)?;
            let b2 = bind_type_partial(node, *t2)?;
            Some(BoundType::Union(Box::new(b1), Box::new(b2)))
        }
        Type::ContextCall(ctx, var) => {
            if let Some(bound_var) = get_var_binding_partial(node, var.clone()).ok().flatten() {
                Some(BoundType::ContextCall(ctx.clone(), bound_var))
            } else {
                None
            }
        }
        Type::Universe => Some(BoundType::Universe),
    }
}

/// Extension trait for PartialNonTerminal to support binding resolution
pub trait BindablePartialNonTerminal {
    fn resolve_typing_rule_with<R: BindingResolver>(
        &self,
        resolver: &R,
        rule: &TypingRule,
    ) -> Result<BoundTypingRule, String>;
}

impl BindablePartialNonTerminal for PartialNonTerminal {
    fn resolve_typing_rule_with<R: BindingResolver>(
        &self,
        resolver: &R,
        rule: &TypingRule,
    ) -> Result<BoundTypingRule, String> {
        resolver.resolve_rule(rule, self)
    }
}

/// --- Branch selection helpers over PartialAST ---
/// Try to get the concluded BoundType of a `TypingRule` when applied to a partial branch
pub fn conclude_type_with_rule(node: &PartialNonTerminal, rule: &TypingRule) -> Option<BoundType> {
    let resolver = DefaultBindingResolver;
    let bound = resolver.resolve_rule(rule, node).ok()?;
    match bound.conclusion.kind {
        BoundConclusionKind::Type(bt) => Some(bt),
        BoundConclusionKind::ContextLookup(_, _) => None,
    }
}
