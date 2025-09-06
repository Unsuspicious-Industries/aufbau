use crate::logic::ast::{ASTNode, NonTerminal};
use crate::logic::typing::{Type, TypingRule, Premise, TypingJudgment, TypeSetting, Conclusion};
use super::utils::{get_nt_binding, get_type_binding};

/// A bound typing rule where all rule variables have been resolved to actual AST nodes
#[derive(Debug, Clone)]
pub struct BoundTypingRule {
    pub name: String,
    pub premises: Vec<BoundPremise>,
    pub conclusion: BoundConclusion,
}

/// A bound premise where rule variables are resolved to nodes
#[derive(Debug, Clone)]
pub struct BoundPremise {
    pub setting: Option<BoundTypeSetting>,
    pub judgment: BoundTypingJudgment,
}

/// A bound type setting with resolved node references
#[derive(Debug, Clone)]
pub struct BoundTypeSetting {
    pub name: String,
    pub extensions: Vec<BoundTypeAscription>,
}

/// A bound type ascription linking a node to a type
#[derive(Debug, Clone)]
pub struct BoundTypeAscription {
    pub node: NonTerminal,  // The actual AST node (instead of rule variable)
    pub ty: Type,          // Regular type, no need for special bound type
}

/// A bound typing judgment with resolved nodes
#[derive(Debug, Clone)]
pub enum BoundTypingJudgment {
    Ascription(BoundTypeAscription),
    Membership(NonTerminal, String), // (resolved node, context)
}

/// A bound conclusion with resolved components
#[derive(Debug, Clone)]
pub enum BoundConclusion {
    Type(Type),  // Regular type, no need for bound type
    ContextLookup(String, NonTerminal), // (context, resolved variable node)
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
            bound_premises.push(self.resolve_premise(premise, node)?);
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
            TypingJudgment::Ascription((term, ty)) => {
                // Resolve the term variable to an actual node
                let var_node = get_nt_binding(node, term.clone())
                    .ok_or_else(|| format!("Could not resolve binding variable {} in ascription", term))?;
                
                // For types, try to resolve type variables but keep the type structure
                let resolved_type = get_type_binding(node, ty.clone()).unwrap_or_else(|| ty.clone());
                
                BoundTypingJudgment::Ascription(BoundTypeAscription {
                    node: var_node,
                    ty: resolved_type,
                })
            }
            TypingJudgment::Membership(var, ctx) => {
                // Resolve the variable to an actual node
                let var_node = get_nt_binding(node, var.clone())
                    .ok_or_else(|| format!("Could not resolve binding variable {} in membership", var))?;
                
                BoundTypingJudgment::Membership(var_node, ctx.clone())
            }
        };
        
        Ok(BoundPremise {
            setting: bound_setting,
            judgment: bound_judgment,
        })
    }
    
    fn resolve_conclusion(&self, conclusion: &Conclusion, node: &NonTerminal) -> Result<BoundConclusion, String> {
        match conclusion {
            Conclusion::Type(ty) => {
                // For types, try to resolve type variables but keep the type structure
                let resolved_type = get_type_binding(node, ty.clone()).unwrap_or_else(|| ty.clone());
                Ok(BoundConclusion::Type(resolved_type))
            }
            Conclusion::ContextLookup(context, var) => {
                // Resolve the variable to an actual node
                let var_node = get_nt_binding(node, var.clone())
                    .ok_or_else(|| format!("Could not resolve binding variable {} in context lookup", var))?;
                
                Ok(BoundConclusion::ContextLookup(context.clone(), var_node))
            }
        }
    }
}

impl DefaultBindingResolver {
    /// Helper method to resolve type settings
    fn resolve_type_setting(&self, setting: &TypeSetting, node: &NonTerminal) -> Result<BoundTypeSetting, String> {
        let mut bound_extensions = Vec::new();
        
        for (term, ty) in &setting.extensions {
            // Resolve the term variable to an actual node
            let var_node = get_nt_binding(node, term.clone())
                .ok_or_else(|| format!("Could not resolve binding variable {} in type setting", term))?;
            
            // For types, try to resolve type variables but keep the type structure
            let resolved_type = get_type_binding(node, ty.clone()).unwrap_or_else(|| ty.clone());
            
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
                BoundTypingJudgment::Ascription(ascr) => {
                    nodes.push(&ascr.node);
                }
                BoundTypingJudgment::Membership(node, _) => {
                    nodes.push(node);
                }
            }
            
            if let Some(setting) = &premise.setting {
                for ext in &setting.extensions {
                    nodes.push(&ext.node);
                }
            }
        }
        
        match &self.conclusion {
            BoundConclusion::ContextLookup(_, node) => {
                nodes.push(node);
            }
            BoundConclusion::Type(_) => {
                // Type conclusions don't directly reference nodes
            }
        }
        
        nodes
    }
    
    /// Check if this bound rule is well-formed (all referenced nodes exist)
    pub fn is_well_formed(&self) -> bool {
        // A bound rule is well-formed if all its node references are valid
        // This is a basic check - more sophisticated validation could be added
        !self.referenced_nodes().is_empty() || self.premises.is_empty()
    }
}
