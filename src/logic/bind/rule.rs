use crate::logic::ast::{ASTNode, NonTerminal};
use crate::logic::typing::{TypingRule, Premise, TypingJudgment, TypeSetting, Conclusion};
use crate::logic::typing::rule::ConclusionKind;
use super::typing::BoundType;
use super::utils::{get_nt_binding, bind_type, collect_nt_bindings_same_level};

/// A bound typing rule where all rule variables have been resolved to actual AST nodes
#[derive(Clone,PartialEq)]
pub struct BoundTypingRule {
    pub name: String,
    pub premises: Vec<BoundPremise>,
    pub conclusion: BoundConclusion,
}

/// A bound premise where rule variables are resolved to nodes
#[derive(Debug, Clone, PartialEq)]
pub struct BoundPremise {
    pub setting: Option<BoundTypeSetting>,
    pub judgment: Option<BoundTypingJudgment>,
}

/// A bound type setting with resolved node references
#[derive(Debug, Clone, PartialEq)]
pub struct BoundTypeSetting {
    pub name: String,
    pub extensions: Vec<BoundTypeAscription>,
}

/// A bound type ascription linking a node to a type
#[derive(Debug, Clone,PartialEq)]
pub struct BoundTypeAscription {
    pub node: NonTerminal,  // The actual AST node (instead of rule variable)
    pub ty: BoundType,          // Regular type, no need for special bound type
}

/// A bound typing judgment with resolved nodes
#[derive(Debug, Clone,PartialEq)]
pub enum BoundTypingJudgment {
    Ascription(BoundTypeAscription),
    Membership(NonTerminal, String), // (resolved node, context)
}

/// Context specification for a bound conclusion (optional input/output context transforms)
#[derive(Debug, Clone, PartialEq, Default)]
pub struct BoundConclusionContext {
    pub input: String,
    pub output: Option<BoundTypeSetting>,
}


/// The kind of bound conclusion: either a type or a context lookup Γ(x)
#[derive(Debug, Clone, PartialEq)]
pub enum BoundConclusionKind {
    Type(BoundType),
    ContextLookup(String, NonTerminal), // (context, resolved variable node)
}

/// A bound conclusion with resolved components
#[derive(Debug, Clone,PartialEq)]
pub struct BoundConclusion {
    pub context: BoundConclusionContext,
    pub kind: BoundConclusionKind,
}

/// Trait for resolving rule bindings to create bound rules
pub trait BindingResolver {
    /// Resolve a typing rule against a NonTerminal node to create a bound rule
    fn resolve_rule(&self, rule: &TypingRule, node: &NonTerminal) -> Result<BoundTypingRule, String>;
    
    /// Resolve a premise using bindings from the node
    fn resolve_premise(&self, premise: &Premise, node: &NonTerminal) -> Result<BoundPremise, String>;
    
    /// Resolve a conclusion using bindings from the node
    fn resolve_conclusion(&self, conclusion: &Conclusion, node: &NonTerminal) -> Result<BoundConclusion, String>;
}

/// Default implementation of binding resolver
pub struct DefaultBindingResolver;

impl BindingResolver for DefaultBindingResolver {
    fn resolve_rule(&self, rule: &TypingRule, node: &NonTerminal) -> Result<BoundTypingRule, String> {
        let mut bound_premises = Vec::new();
        
        for premise in &rule.premises {
            // Expand a single schematic premise into N premises if the bound term/var repeats at same level
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
    
    fn resolve_premise(&self, premise: &Premise, node: &NonTerminal) -> Result<BoundPremise, String> {
        let bound_setting = if let Some(setting) = &premise.setting {
            Some(self.resolve_type_setting(setting, node)?)
        } else {
            None
        };
        
        let bound_judgment = match &premise.judgment {
            Some(TypingJudgment::Ascription((term, ty))) => {
                // Resolve the term variable to an actual node
                let var_node = get_nt_binding(node, term.clone())
                    .ok_or_else(|| format!("Could not resolve binding variable {} in ascription", term))?;
                
                // For types, try to resolve type variables but keep the type structure
                let resolved_type = bind_type(node, ty.clone())
                    .ok_or_else(|| format!("Could not resolve type binding for type {:?} in ascription", ty))?;

                BoundTypingJudgment::Ascription(BoundTypeAscription {
                    node: var_node,
                    ty: resolved_type,
                })
            }
            Some(TypingJudgment::Membership(var, ctx)) => {
                // Resolve the variable to an actual node
                let var_node = get_nt_binding(node, var.clone())
                    .ok_or_else(|| format!("Could not resolve binding variable {} in membership", var))?;
                
                return Ok(BoundPremise { setting: bound_setting, judgment: Some(BoundTypingJudgment::Membership(var_node, ctx.clone())) });
            }
            None => {
                return Ok(BoundPremise { setting: bound_setting, judgment: None });
            }
        };
        
        Ok(BoundPremise {
            setting: bound_setting,
            judgment: Some(bound_judgment),
        })
    }
    
    fn resolve_conclusion(&self, conclusion: &Conclusion, node: &NonTerminal) -> Result<BoundConclusion, String> {
        match &conclusion.kind {
            ConclusionKind::Type(ty) => {
                let resolved_type = bind_type(node, ty.clone())
                    .ok_or_else(|| format!("Could not resolve type binding for type {:?} in conclusion", ty))?;
                // Map context transforms if any (input is now just a context name String with no extensions)
                let ctx = if !conclusion.context.input.is_empty() || conclusion.context.output.is_some() {
                    BoundConclusionContext {
                        input: conclusion.context.input.clone(),
                        output: if let Some(s) = &conclusion.context.output { Some(self.resolve_type_setting(s, node)?) } else { None },
                    }
                } else { BoundConclusionContext::default() };
                Ok(BoundConclusion { context: ctx, kind: BoundConclusionKind::Type(resolved_type) })
            }
            ConclusionKind::ContextLookup(context, var) => {
                // Resolve the variable to an actual node
                let var_node = get_nt_binding(node, var.clone())
                    .ok_or_else(|| format!("Could not resolve binding variable {} in context lookup", var))?;
                
                Ok(BoundConclusion { context: BoundConclusionContext::default(), kind: BoundConclusionKind::ContextLookup(context.clone(), var_node) })
            }
        }
    }
}

impl DefaultBindingResolver {
    /// Helper method to resolve type settings
    fn resolve_type_setting(&self, setting: &TypeSetting, node: &NonTerminal) -> Result<BoundTypeSetting, String> {
        let mut bound_extensions = Vec::new();
        
        for (term, ty) in &setting.extensions {
            // Resolve the term variable to an actual node (single binding in settings semantics)
            let var_node = get_nt_binding(node, term.clone())
                .ok_or_else(|| format!("Could not resolve binding variable {} in type setting", term))?;
            
            // For types, try to resolve type variables but keep the type structure
            let resolved_type = bind_type(node, ty.clone())
                .ok_or_else(|| format!("Could not resolve type binding for type {:?} in type setting", ty))?;

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

    /// Resolve a premise possibly into multiple bound premises if the bound variable repeats at the same AST level.
    fn resolve_premise_many(&self, premise: &Premise, node: &NonTerminal) -> Result<Vec<BoundPremise>, String> {
        // Pre-resolve the setting (same for all expanded premises)
        let bound_setting = if let Some(setting) = &premise.setting {
            Some(self.resolve_type_setting(setting, node)?)
        } else { None };

        match &premise.judgment {
            Some(TypingJudgment::Ascription((term, ty))) => {
                // Try to expand by repetition at same level
                let reps = collect_nt_bindings_same_level(node, term);
                let resolved_type = bind_type(node, ty.clone())
                    .ok_or_else(|| format!("Could not resolve type binding for type {:?} in ascription", ty))?;
                if !reps.is_empty() {
                    Ok(reps.into_iter().map(|var_node| BoundPremise {
                        setting: bound_setting.clone(),
                        judgment: Some(BoundTypingJudgment::Ascription(BoundTypeAscription { node: var_node, ty: resolved_type.clone() })),
                    }).collect())
                } else {
                    // Fallback: attempt a single binding; if not found, treat as zero premises (e.g., zero repetitions)
                    if let Some(var_node) = get_nt_binding(node, term.clone()) {
                        Ok(vec![BoundPremise { setting: bound_setting, judgment: Some(BoundTypingJudgment::Ascription(BoundTypeAscription { node: var_node, ty: resolved_type })) }])
                    } else {
                        Ok(vec![])
                    }
                }
            }
            Some(TypingJudgment::Membership(var, ctx)) => {
                let reps = collect_nt_bindings_same_level(node, var);
                if !reps.is_empty() {
                    Ok(reps.into_iter().map(|var_node| BoundPremise {
                        setting: bound_setting.clone(),
                        judgment: Some(BoundTypingJudgment::Membership(var_node, ctx.clone())),
                    }).collect())
                } else {
                    if let Some(var_node) = get_nt_binding(node, var.clone()) {
                        Ok(vec![BoundPremise { setting: bound_setting, judgment: Some(BoundTypingJudgment::Membership(var_node, ctx.clone())) }])
                    } else {
                        Ok(vec![])
                    }
                }
            }
            None => Ok(vec![BoundPremise { setting: bound_setting, judgment: None }]),
        }
    }
}

/// Extension trait for NonTerminal to support binding resolution
pub trait BindableNonTerminal {
    /// Resolve the typing rule of this nonterminal to create a bound rule
    fn resolve_typing_rule(&self) -> Result<Option<BoundTypingRule>, String>;
    
    /// Resolve the typing rule using a custom resolver
    fn resolve_typing_rule_with<R: BindingResolver>(&self, resolver: &R) -> Result<Option<BoundTypingRule>, String>;
}

impl BindableNonTerminal for NonTerminal {
    fn resolve_typing_rule(&self) -> Result<Option<BoundTypingRule>, String> {
        self.resolve_typing_rule_with(&DefaultBindingResolver)
    }
    
    fn resolve_typing_rule_with<R: BindingResolver>(&self, _resolver: &R) -> Result<Option<BoundTypingRule>, String> {
        // For the new system, we would create bound rules from regular rules at AST construction time
        // For now, just return the existing bound rule if present
        if let Some(bound_rule) = &self.bound_typing_rule {
            Ok(Some((**bound_rule).clone()))
        } else {
            Ok(None)
        }
    }
}

/// Extension trait for ASTNode to support binding resolution
pub trait BindableASTNode {
    /// Resolve the typing rule if this is a nonterminal with a rule
    fn resolve_typing_rule(&self) -> Result<Option<BoundTypingRule>, String>;
    
    /// Resolve the typing rule using a custom resolver
    fn resolve_typing_rule_with<R: BindingResolver>(&self, resolver: &R) -> Result<Option<BoundTypingRule>, String>;
}

impl BindableASTNode for ASTNode {
    fn resolve_typing_rule(&self) -> Result<Option<BoundTypingRule>, String> {
        if let Some(nt) = self.as_nonterminal() {
            nt.resolve_typing_rule()
        } else {
            Ok(None)
        }
    }
    
    fn resolve_typing_rule_with<R: BindingResolver>(&self, resolver: &R) -> Result<Option<BoundTypingRule>, String> {
        if let Some(nt) = self.as_nonterminal() {
            nt.resolve_typing_rule_with(resolver)
        } else {
            Ok(None)
        }
    }
}

impl BoundTypingRule {
    /// Get all nodes referenced in this bound rule
    pub fn referenced_nodes(&self) -> Vec<&NonTerminal> {
        let mut nodes = Vec::new();
        
        for premise in &self.premises {
            match &premise.judgment {
                Some(BoundTypingJudgment::Ascription(ascr)) => {
                    nodes.push(&ascr.node);
                }
                Some(BoundTypingJudgment::Membership(node, _)) => {
                    nodes.push(node);
                }
                None => {}
            }
            
            if let Some(setting) = &premise.setting {
                for ext in &setting.extensions {
                    nodes.push(&ext.node);
                }
            }
        }
        
        match &self.conclusion.kind {
            BoundConclusionKind::ContextLookup(_, node) => nodes.push(node),
            BoundConclusionKind::Type(_) => {}
        }
        // Updated: input is now a plain String (no extensions to collect)
        if let Some(s) = &self.conclusion.context.output { for ext in &s.extensions { nodes.push(&ext.node); } }
        nodes
    }
    
    /// Check if this bound rule is well-formed (all referenced nodes exist)
    pub fn is_well_formed(&self) -> bool {
        // A bound rule is well-formed if all its node references are valid
        // This is a basic check - more sophisticated validation could be added
        !self.referenced_nodes().is_empty() || self.premises.is_empty()
    }
}
