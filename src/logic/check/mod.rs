use crate::logic::partial::{PartialNonTerminal, PartialASTNode};

use super::bind::partial::{
    BoundTypingRule,
    BoundPremise,
    BoundTypingJudgment,
    BoundConclusionKind,
    BoundTypeAscription,
    extract_terminal_value_partial,
};
use super::bind::BoundType;
use crate::debug_trace; // added for trace-level context dumps

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

    /// Create a new TypeChecker with the given context
    pub fn with_context(context: TypingContext) -> Self {
        Self { context , debug: TypeCheckerDebug::new(None) }
    }

    /// Get a mutable reference to the context
    pub fn context_mut(&mut self) -> &mut TypingContext {
        &mut self.context
    }

    /// Format bound typing rule information for error messages
    pub fn format_bound_rule_info(&self, rule: &BoundTypingRule) -> String {
        format!("\nBound typing rule [{}]: {}", rule.name, rule)
    }
}

impl TypeChecker {
    // -------- Partial AST oriented API --------
    pub fn check_partial(&mut self, node: &PartialASTNode) -> Result<Option<BoundType>, String> {
        match node {
            PartialASTNode::NonTerminal(alts) => {
                // If multiple alts, we cannot disambiguate here; caller should pass a single branch
                if alts.len() == 1 { self.check_partial_nt(&alts[0]) } else { Ok(None) }
            }
            PartialASTNode::Terminal(_) => Ok(None),
        }
    }

    pub fn check_partial_nt(&mut self, node: &PartialNonTerminal) -> Result<Option<BoundType>, String> {
        if let Some(rule) = &node.bound_typing_rule {
            let ty = self.apply_bound_rule_partial(rule, node)?;
            Ok(Some(ty))
        } else {
            // walk children to establish context; no conclusion
            for ch in &node.children {
                let _ = self.check_partial(ch)?;
            }
            Ok(None)
        }
    }

    fn resolve_pbound_extensions(&self, extensions: &[BoundTypeAscription]) -> Result<Vec<(String, BoundType)>, String> {
        let mut pairs = Vec::with_capacity(extensions.len());
        for ext in extensions {
            let name = extract_terminal_value_partial(&ext.node.as_node())
                .ok_or_else(|| format!("Could not extract variable name in type setting for partial node {}", ext.node.value))?;
            pairs.push((name, ext.ty.clone()));
        }
        Ok(pairs)
    }

    pub fn apply_bound_rule_partial(&mut self, rule: &BoundTypingRule, node: &PartialNonTerminal) -> Result<BoundType, String> {
        // Premises
        for premise in &rule.premises {
            let mut premise_ctx = self.context.create_child();
            if let Some(setting) = &premise.setting {
                let exts = self.resolve_pbound_extensions(&setting.extensions)?;
                premise_ctx.extend(exts);
            }
            let mut premise_checker = TypeChecker::with_context(premise_ctx);
            premise_checker.debug = self.debug.clone();
            premise_checker.check_pbound_judgement(premise, node)?;
        }
        // Commit output context transform to the current (ambient) context, if any
        if let Some(out) = &rule.conclusion.context.output {
            let out_exts = self.resolve_pbound_extensions(&out.extensions)?;
            // If input context name differs from output name, create a child context
            if out.name != rule.conclusion.context.input {
                self.context = self.context.create_child();
            }
            for (vname, ty) in out_exts { self.context.add(vname, ty); }
        }

        // Conclusion
        match &rule.conclusion.kind {
            BoundConclusionKind::Type(ty) => Ok(ty.clone()),
            BoundConclusionKind::ContextLookup(_ctx, var_node) => {
                if let Some(var_name) = extract_terminal_value_partial(&var_node.as_node()) {
                    if let Some(ty) = self.context.lookup(&var_name) { Ok(ty.clone()) } else { Err(format!("Variable {} not found in context Γ for lookup", var_name)) }
                } else {
                    Err("Could not extract variable for context lookup".to_string())
                }
            }
        }
    }

    pub fn check_pbound_judgement(&mut self, premise: &BoundPremise, _current: &PartialNonTerminal) -> Result<(), String> {
        match &premise.judgment {
            Some(BoundTypingJudgment::Ascription(ascr)) => {
                let var_nt = &ascr.node;
                let mut expected_ty = ascr.ty.clone();
                if let Some(mut inferred_ty) = self.check_partial(&var_nt.as_node())? {
                    inferred_ty.resolve(&self.context);
                    expected_ty.resolve(&self.context);
                    if inferred_ty.is_compatible_with(&expected_ty) { Ok(()) } else { Err(format!("Type mismatch: expected {:?}, found {:?}", expected_ty, inferred_ty)) }
                } else {
                    Err("No type inferred for partial node".to_string())
                }
            }
            Some(BoundTypingJudgment::Membership(var_node, ctx)) => {
                if let Some(var_name) = extract_terminal_value_partial(&var_node.as_node()) {
                    if let Some(_ty) = self.context.lookup(&var_name) { Ok(()) } else { Err(format!("Variable {} not in context {}", var_name, ctx)) }
                } else { Err("Could not extract variable name for membership".to_string()) }
            }
            None => Ok(()),
        }
    }
}


#[cfg(test)]
mod tests;