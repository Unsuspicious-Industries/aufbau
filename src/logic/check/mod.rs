use crate::logic::ast::NonTerminal;


use super::ast::ASTNode;
use super::bind::{
    extract_terminal_value,
    // Bound rule types
    BoundTypingRule, 
    BoundPremise, 
    BoundTypingJudgment, 
    BoundConclusionKind,
    BoundTypeAscription,
    BoundType
};
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


    /// Add a reference to the current context
    pub fn add(&mut self, var: String, ty: BoundType) {
        self.context.add(var, ty);
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

    /// Build a module tag that includes the current expression (nonterminal) name for tracing
    fn trace_module_for(&self, nt: &NonTerminal) -> String {
        format!("typechecker:{}", nt.value)
    }
}

impl TypeChecker {
    pub fn check(&mut self, node: &ASTNode) -> Result<Option<BoundType>, String> { 

        debug_trace!("typechecker", "check:start node={}", node.show_simple());
        match node {
            ASTNode::Nonterminal(nt) => {
                self.check_nt(nt)
            }
            ASTNode::Terminal(_) => Ok(None),
        }
    }

    pub fn check_nt(&mut self, node: &NonTerminal) -> Result<Option<BoundType>, String> {
        let bound_typing_rule = &node.bound_typing_rule;
        if let Some(rule) = bound_typing_rule {
            // Use already-bound typing rules directly
            let ty = self.apply_bound_rule(rule, node)?;
            Ok(Some(ty))
        } else {

            let nt_children = node.nonterminal_children();
            if nt_children.len() == 1 {
                debug_trace!("typechecker", "check_nt: single child, recursing into child node={}", nt_children[0].as_node().show_simple());
                return self.check(&nt_children[0].as_node());
            } else {
                // No typing rule: just walk subtree to ensure descendants are processed; do not re-check child.
                debug_trace!("typechecker", "check_nt: no bound_typing_rule for node={}", node.as_node().show_simple());
                self.walk(&node.as_node())?;
                Ok(None)
            }
        }
    }

    /// Resolve bound type setting extensions into a vector of (name, type) pairs
    fn resolve_bound_extensions(&self, extensions: &[BoundTypeAscription]) -> Result<Vec<(String, BoundType)>, String> {
        let mut pairs: Vec<(String, BoundType)> = Vec::with_capacity(extensions.len());
        for ext in extensions {
            let name = extract_terminal_value(&ext.node.as_node())
                .ok_or_else(|| self.format_error(&ext.node.as_node(), "Could not extract variable name in type setting"))?;
            pairs.push((name, ext.ty.clone()));
        }
        Ok(pairs)
    }

    /// Apply a bound typing rule where all meta-variables are already resolved
    pub fn apply_bound_rule(&mut self, rule: &BoundTypingRule, node: &NonTerminal) -> Result<BoundType, String> {
        let module_tag = self.trace_module_for(node);
        debug_trace!(&module_tag, "apply_bound_rule:start rule={} node={}", rule.name, node.as_node().show_simple());
        debug_trace!(&module_tag, "context: BEFORE rule {}\n{}", rule.name, self.context.dump());
        // 1) Premise Validation: evaluate each premise in a temporary child context
        for premise in &rule.premises {
            // Build a child context for this premise, optionally seeded with setting extensions
            let mut premise_ctx = self.context.create_child();
            if let Some(setting) = &premise.setting {
                let extensions = self.resolve_bound_extensions(&setting.extensions)?;
                premise_ctx.extend(extensions);
                // debug dump the premise context if trace level
                debug_trace!(&module_tag, "premise: created child context with setting extensions; context=\n{}", premise_ctx.dump());
            }

            // crate a new checker with the premise context
            let mut premise_checker = TypeChecker::with_context(premise_ctx);
            premise_checker.debug = self.debug.clone();

            // Check the bound judgment in the child context
            if let Err(mut error) = premise_checker.check_bound_judgement(premise, node) {
                error.push_str(&self.format_bound_rule_info(rule));
                debug_trace!(&module_tag, "apply_bound_rule:premise_error rule={} error={}", rule.name, error);
                return Err(error);
            }

            // Merge any bindings produced during premise evaluation back into the ambient context (delta commit)
            //let deltas = self.context.current_context_references().clone();
            //for (k, v) in deltas.into_iter() {
            //    self.context.add(k, v);
            //}
            debug_trace!(&module_tag, "premise: merged deltas into parent; parent context now\n{}", self.context.dump());
        }

        // Commit output context transform to the current (ambient) context, if any
        if let Some(out) = &rule.conclusion.context.output {
            let out_exts = self.resolve_bound_extensions(&out.extensions)?;
            // if input context is same as output add to current, else create new child context
            if out.name != rule.conclusion.context.input {
                self.context = self.context.create_child();
                debug_trace!(&module_tag, "created new child context for conclusion.output (different from input)");
            }
            for (vname, ty) in out_exts { self.context.add(vname, ty); }
            debug_trace!(&module_tag, "after applying conclusion.output extensions; context now\n{}", self.context.dump());
        }


        // 2) Conclusion Inference and Context Threading
        match &rule.conclusion.kind {
            BoundConclusionKind::Type(ty) => {
                debug_trace!(&module_tag, "apply_bound_rule:end rule={} inferred_type={:?}", rule.name, ty);
                Ok(ty.clone())
            }
            BoundConclusionKind::ContextLookup(_context_name, var_node) => {
                if let Some(var_name) = extract_terminal_value(&var_node.as_node()) {
                    // debug node.show_simple()
                    debug_trace!(&module_tag, "apply_bound_rule:start rule={} node={}", rule.name, node.as_node().show_simple());
                    if let Some(ty) = self.context.lookup(&var_name) {
                        debug_trace!(&module_tag, "apply_bound_rule:end rule={} context_lookup {} => {:?}", rule.name, var_name, ty);
                        Ok(ty.clone())
                    } else {
                        Err(self.format_error(&var_node.as_node(),
                            &format!("Variable {} not found in context Γ for lookup", var_name)))
                    }
                } else {
                    Err(self.format_error(&var_node.as_node(),
                        "Could not extract variable name for context lookup"))
                }
            }
        }
    }

    /// Check a bound premise 
    pub fn check_bound_judgement(&mut self, premise: &BoundPremise, current: &NonTerminal) -> Result<(), String> {
        let module_tag = self.trace_module_for(current);
        match &premise.judgment {
            Some(BoundTypingJudgment::Ascription(ascr)) => {
                let var_nt = &ascr.node;
                let mut expected_ty = ascr.ty.clone();

                if let Some(mut inferred_ty) = self.check(&var_nt.as_node())? {
                    inferred_ty.resolve(&self.context);
                    expected_ty.resolve(&self.context);
                    // Use type compatibility checking instead of strict equality
                    if inferred_ty.is_compatible_with(&expected_ty) {
                        debug_trace!(&module_tag, "ascription ok var={} expected={:?} inferred={:?}", self.extract_text(&var_nt.as_node()), expected_ty, inferred_ty);
                        Ok(())
                    } else {
                        let value = self.extract_text(&var_nt.as_node());
                        debug_trace!(&module_tag, "ascription mismatch var={} expected={:?} inferred={:?} context=\n{}", value, expected_ty, inferred_ty, self.context.dump());
                        Err(self.format_error(&var_nt.as_node(),
                            &format!("Type mismatch for {}: expected {:?}, found {:?} (incompatible types)", 
                                    value, expected_ty, inferred_ty)))
                    }
                } else {
                    debug_trace!(&module_tag, "ascription no-infer var={} expected={:?} context=\n{}", var_nt.as_node().show_simple(), expected_ty, self.context.dump());
                    Err(self.format_error(&var_nt.as_node(),
                        &format!("No type inferred for node={}, required {:?}", var_nt.as_node().show_simple(), expected_ty)))
                }
            }
            Some(BoundTypingJudgment::Membership(var_node, ctx)) => {
                if let Some(var_name) = extract_terminal_value(&var_node.as_node()) {
                    if let Some(_t) = self.context.lookup(&var_name) {
                        debug_trace!(&module_tag, "membership ok {} in {}", var_name, ctx);
                        Ok(())
                    } else {
                        debug_trace!(&module_tag, "membership fail {} not in {} context=\n{}", var_name, ctx, self.context.dump());
                        Err(self.format_error(&var_node.as_node(),
                            &format!("Variable {} not found in context {}", var_name, ctx)))
                    }
                } else {
                    let value = self.extract_text(&var_node.as_node());
                    debug_trace!(&module_tag, "membership extract-fail raw={} context=\n{}", value, self.context.dump());
                    Err(self.format_error(&var_node.as_node(),
                        &format!("Could not extract variable name from {}", value)))
                }
            }
            None => Ok(()),
        }
    }

    pub fn walk(&mut self, node: &ASTNode) -> Result<(), String> {
        match node {
            ASTNode::Terminal(_) => Ok(()),
            ASTNode::Nonterminal(nt) => {
                // check() all children
                for child in nt.nonterminal_children() {
                    self.check(&child.as_node())?;
                }
                Ok(())
            }
        }
    }
}


#[cfg(test)]
mod tests;