use crate::logic::ast::NonTerminal;
use crate::logic::typing::Type;

use super::ast::ASTNode;
use super::bind::{
    extract_terminal_value,
    // Bound rule types
    BoundTypingRule, BoundPremise, BoundTypingJudgment, BoundConclusion, BoundTypeAscription,
};
use crate::debug_debug;

pub mod context;
pub mod debug;

use context::TypingContext;
use debug::TypeCheckerDebug;

pub struct TypeChecker { 
    pub context: TypingContext,
    /// Debug helper for span information
    pub debug: TypeCheckerDebug,
}
impl TypeChecker { 
    pub fn new() -> Self { 
        Self { 
            context: TypingContext::new(),
            debug: TypeCheckerDebug::new(None),
        } 
    }

    /// Create a new TypeChecker with optional input string for debugging
    pub fn with_input(input: Option<String>) -> Self {
        Self { 
            context: TypingContext::new(),
            debug: TypeCheckerDebug::new(input),
        }
    }

    /// Create a new TypeChecker with the given context
    pub fn with_context(context: TypingContext) -> Self {
        Self { context , debug: TypeCheckerDebug::new(None) }
    }

    /// Get a mutable reference to the context
    pub fn context_mut(&mut self) -> &mut TypingContext {
        &mut self.context
    }

    /// Enter a new scope in the context
    pub fn enter_scope(&mut self) {
        self.context.enter_scope();
    }

    /// Exit the current scope in the context
    pub fn exit_scope(&mut self) -> Result<(), String> {
        self.context.exit_scope()
    }

    /// Add a reference to the current scope
    pub fn bind(&mut self, var: String, ty: Type) {
        self.context.bind(var, ty);
    }

    /// Format an error with span information
    pub fn format_error(&self, node: &ASTNode, message: &str) -> String {
        self.debug.format_error(node, message)
    }

    /// Extract the actual text content from a node using its span
    pub fn extract_text(&self, node: &ASTNode) -> String {
        self.debug.extract_text(node)
    }

    /// Log debug information with span context
    pub fn debug_at_span(&self, node: &ASTNode, message: &str) {
        self.debug.debug_at_span(node, message);
    }

    /// Format bound typing rule information for error messages
    pub fn format_bound_rule_info(&self, rule: &BoundTypingRule) -> String {
        format!("\nBound typing rule [{}]: {}", rule.name, rule)
    }
}

impl TypeChecker {
    pub fn check(&mut self, node: &ASTNode) -> Result<Option<Type>, String> { 
        match node {
            ASTNode::Nonterminal(nt) => {
                self.check_nt(nt)
            }
            ASTNode::Terminal(_) => Ok(None),
        }
    }

    pub fn check_nt(&mut self, node: &NonTerminal) -> Result<Option<Type>, String> {
        let bound_typing_rule = &node.bound_typing_rule;
        if let Some(rule) = bound_typing_rule {
            // Use already-bound typing rules directly
            let ty = self.apply_bound_rule(rule, node)?;
            Ok(Some(ty))
        } else {
            // For nodes without typing rules, try context lookup if it's a variable
            if node.value == "Variable" {
                // Extract variable name and look up in context
                if let Some(var_name) = extract_terminal_value(&node.as_node()) {
                    if let Some(ty) = self.context.lookup(&var_name) {
                        return Ok(Some(ty.clone()));
                    }
                }
            }
            
            let nt_children = node.nonterminal_children();
            if nt_children.len() == 1 {
                self.check(&nt_children[0].as_node())
            } else {
                Err(self.format_error(&node.as_node(), 
                    &format!("No typing rule associated with nonterminal '{}'", 
                            self.extract_text(&node.as_node()))))
            }
        }
    }

    /// Resolve bound type setting extensions (already-resolved nodes and types)
    fn resolve_bound_extensions(&self, extensions: &[BoundTypeAscription]) -> Result<TypingContext, String> {
        let mut pairs: Vec<(String, Type)> = Vec::with_capacity(extensions.len());
        for ext in extensions {
            let name = extract_terminal_value(&ext.node.as_node())
                .ok_or_else(|| self.format_error(&ext.node.as_node(), "Could not extract variable name in type setting"))?;
            pairs.push((name, ext.ty.clone()));
        }
        Ok(self.context.with_extended_scope(pairs.into_iter()))
    }

    /// Apply a bound typing rule where all meta-variables are already resolved
    pub fn apply_bound_rule(&self, rule: &BoundTypingRule, node: &NonTerminal) -> Result<Type, String> {
        // Keep track of the final context for conclusion evaluation
        let mut final_context = self.context.clone();
        
        for premise in &rule.premises {
            // Apply setting: create a new context with extended scope for this premise
            let mut checker = if let Some(setting) = &premise.setting {
                let new_context = self.resolve_bound_extensions(&setting.extensions)?;
                final_context = new_context.clone();  // Update final context
                TypeChecker { context: new_context, debug: TypeCheckerDebug::new(self.debug.input.clone()) }
            } else {
                // Use the current context if no setting
                TypeChecker { context: self.context.clone(), debug: TypeCheckerDebug::new(self.debug.input.clone()) }
            };

            // dump context
            debug_debug!("check", "Applying premise in context: {:?}", checker.context);

            // Check the bound judgment directly
            if let Err(mut error) = checker.check_bound_judgement(premise, node) {
                error.push_str(&self.format_bound_rule_info(rule));
                return Err(error);
            }
        }

        // Resolve the (already-bound) conclusion using the final context from premises
        match &rule.conclusion {
            BoundConclusion::Type(ty) => Ok(ty.clone()),
            BoundConclusion::ContextLookup(context, var_node) => {
                if let Some(var_name) = extract_terminal_value(&var_node.as_node()) {
                    if let Some(ty) = final_context.lookup(&var_name) {
                        Ok(ty.clone())
                    } else {
                        Err(self.format_error(&var_node.as_node(),
                            &format!("Variable {} not found in context {} for lookup", var_name, context)))
                    }
                } else {
                    Err(self.format_error(&var_node.as_node(),
                        "Could not extract variable name for context lookup"))
                }
            }
        }
    }

    /// Check a bound premise (no meta-variable lookups needed)
    pub fn check_bound_judgement(&mut self, premise: &BoundPremise, _node: &NonTerminal) -> Result<(), String> {
        match &premise.judgment {
            BoundTypingJudgment::Ascription(ascr) => {
                let var_nt = &ascr.node;
                let ty = ascr.ty.clone();

                if let Some(inferred_ty) = self.check(&var_nt.as_node())? {
                    if inferred_ty == ty {
                        Ok(())
                    } else {
                        let value = self.extract_text(&var_nt.as_node());
                        Err(self.format_error(&var_nt.as_node(),
                            &format!("Type mismatch for {}: expected {}, found {}", value, ty, inferred_ty)))
                    }
                } else {
                    // Strict mode: no fallback to context lookup; nodes in ascriptions must type-check to a concrete type
                    let value = self.extract_text(&var_nt.as_node());
                    Err(self.format_error(&var_nt.as_node(),
                        &format!("No type inferred for {}, required {}", value, ty)))
                }
            }
            BoundTypingJudgment::Membership(var_node, ctx) => {
                if let Some(var_name) = extract_terminal_value(&var_node.as_node()) {
                    if let Some(_t) = self.context.lookup(&var_name) {
                        Ok(())
                    } else {
                        let value = self.extract_text(&var_node.as_node());
                        Err(self.format_error(&var_node.as_node(),
                            &format!("Variable {} not found in context {}", value, ctx)))
                    }
                } else {
                    let value = self.extract_text(&var_node.as_node());
                    Err(self.format_error(&var_node.as_node(),
                        &format!("Could not extract variable name from {}", value)))
                }
            }
        }
    }
}


#[cfg(test)]
mod tests;