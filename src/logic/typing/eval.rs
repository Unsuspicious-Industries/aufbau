//! Type Evaluation - Single Tree Checker
//!
//! Clean API: `check_tree` evaluates ONE tree, returning its status.
//! Forest filtering is done at a higher level by the caller.
use crate::debug_trace;
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{Node, NonTerminal, Terminal};
use crate::logic::typing::core::{Context, Substitution, TreeStatus, subst, unify};
use crate::logic::typing::rule::{ConclusionKind, TypeOperation};
use crate::logic::typing::{Type, TypingJudgment, TypingRule};
use std::collections::HashMap;

use super::binding::*;
// ============================================================================
// Core API - single tree checking
// ============================================================================

/// Check a single tree → TreeStatus
pub fn check_tree(root: &NonTerminal, grammar: &Grammar) -> TreeStatus {
    check_tree_with_context(root, grammar, &Context::new())
}

/// Check with initial context
pub fn check_tree_with_context(root: &NonTerminal, grammar: &Grammar, ctx: &Context) -> TreeStatus {
    check_node(&Node::NonTerminal(root.clone()), grammar, ctx, 0)
}

/// Predicate: any tree in forest is well-typed?
pub fn evaluate_typing(roots: &[NonTerminal], grammar: &Grammar) -> bool {
    roots.iter().any(|r| check_tree(r, grammar).is_ok())
}

// ============================================================================
// Node Checking (recursive descent)
// ============================================================================

fn check_node(node: &Node, grammar: &Grammar, ctx: &Context, depth: usize) -> TreeStatus {
    if depth > 50 {
        return TreeStatus::TooDeep;
    }

    match node {
        Node::Terminal(t) => check_terminal(t, ctx),
        Node::NonTerminal(nt) => check_nt(nt, grammar, ctx, depth),
    }
}

fn check_terminal(term: &Terminal, ctx: &Context) -> TreeStatus {
    match term {
        Terminal::Complete { value, .. } => TreeStatus::Valid(Type::Empty),
        Terminal::Partial { .. } => TreeStatus::Partial(Type::Universe),
    }
}

fn check_nt(nt: &NonTerminal, grammar: &Grammar, ctx: &Context, depth: usize) -> TreeStatus {
    // If this production has a typing rule, apply it
    if let Some(rule_name) = &nt.production.rule {
        if let Some(rule) = grammar.typing_rules.get(rule_name) {
            return apply_rule(nt, rule, grammar, ctx, depth);
        }
    }

    // No rule - check for transparent wrapper pattern
    // HEURISTIC: Only-child drilling
    //   - If production has exactly ONE non-terminal child, drill through it
    //   - This handles wrapper productions like `Term ::= BaseTerm`
    //   - Productions with multiple children or only terminals return Universe
    drill_only_child(nt, grammar, ctx, depth)
}

/// Drill through "only-child" wrapper productions
///
/// SEMANTICS:
/// - Productions without typing rules are "transparent" if they have exactly
///   one non-terminal child
/// - The type of the production is the type of that single child
/// - If there are 0 or 2+ non-terminal children, check in sequence
///   propagating context transforms from rules like `let`
/// - Terminals (like ')', literals) have no type significance
fn drill_only_child(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> TreeStatus {
    let (status, _) = drill_only_child_with_context(nt, grammar, ctx, depth);
    status
}

/// Check a node and return both its status AND any context transform it produces
fn check_node_with_context_output(
    node: &Node,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    if depth > 50 {
        return (TreeStatus::TooDeep, None);
    }

    match node {
        Node::Terminal(t) => (check_terminal(t, ctx), None),
        Node::NonTerminal(nt) => check_nt_with_context_output(nt, grammar, ctx, depth),
    }
}

/// Check non-terminal and return any context transform
fn check_nt_with_context_output(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    // If this production has a typing rule, apply it and check for context transform
    if let Some(rule_name) = &nt.production.rule {
        if let Some(rule) = grammar.typing_rules.get(rule_name) {
            return apply_rule_with_context_output(nt, rule, grammar, ctx, depth);
        }
    }

    // No rule - try to drill and propagate any context transform from children
    drill_only_child_with_context(nt, grammar, ctx, depth)
}

/// Drill through productions, propagating context transforms
fn drill_only_child_with_context(
    nt: &NonTerminal,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    // Collect non-terminal children
    let nt_children: Vec<_> = nt
        .children
        .iter()
        .filter(|c| matches!(c, Node::NonTerminal(_)))
        .collect();

    // Only drill if exactly one non-terminal child - propagate context transform
    if nt_children.len() == 1 {
        return check_node_with_context_output(nt_children[0], grammar, ctx, depth + 1);
    }

    // Zero NT children (only terminals)
    if nt_children.is_empty() {
        if is_at_frontier(&Node::NonTerminal(nt.clone())) {
            return (TreeStatus::Partial(Type::Universe), None);
        } else {
            return (TreeStatus::Valid(Type::Empty), None);
        }
    }

    // Multiple NT children - check in sequence with context propagation
    let (status, final_ctx) =
        check_children_with_context_propagation_full(&nt_children, grammar, ctx, depth);
    (status, final_ctx)
}

/// Check children in sequence, returning final context
///
/// The type of a multi-child production is the first non-Universe type found.
/// This handles patterns like `Program ::= Term ProgramTail` where
/// the type of Program is the type of Term.
fn check_children_with_context_propagation_full(
    children: &[&Node],
    grammar: &Grammar,
    initial_ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    let mut current_ctx = initial_ctx.clone();
    let mut has_partial = false;
    let mut ctx_changed = false;
    let mut result_type: Option<Type> = None;

    for child in children.iter() {
        // Check the child with the current context
        let (status, new_ctx) =
            check_node_with_context_output(child, grammar, &current_ctx, depth + 1);

        match &status {
            TreeStatus::Malformed => return (TreeStatus::Malformed, None),
            TreeStatus::TooDeep => return (TreeStatus::TooDeep, None),
            TreeStatus::Partial(ty) => {
                has_partial = true;
                // Capture first non-Universe type
                if result_type.is_none() && !matches!(ty, Type::Universe) {
                    result_type = Some(ty.clone());
                }
            }
            TreeStatus::Valid(ty) => {
                // Capture first non-Universe type
                if result_type.is_none() && !matches!(ty, Type::Universe) {
                    result_type = Some(ty.clone());
                }
            }
        }

        // Update context for next child if this child transformed it
        if let Some(ctx) = new_ctx {
            current_ctx = ctx;
            ctx_changed = true;
        }
    }

    // Use the captured type or default to Universe
    let ty = result_type.unwrap_or(Type::Universe);

    let status = if has_partial || children.iter().any(|c| is_at_frontier(c)) {
        TreeStatus::Partial(ty)
    } else {
        TreeStatus::Valid(ty)
    };

    // Return the transformed context if any child changed it
    if ctx_changed {
        (status, Some(current_ctx))
    } else {
        (status, None)
    }
}

// ============================================================================
// Rule Application
// ============================================================================

fn apply_rule(
    nt: &NonTerminal,
    rule: &TypingRule,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> TreeStatus {
    let (status, _) = apply_rule_with_context_output(nt, rule, grammar, ctx, depth);
    status
}

/// Apply a rule and return both the status and any context transform
fn apply_rule_with_context_output(
    nt: &NonTerminal,
    rule: &TypingRule,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
) -> (TreeStatus, Option<Context>) {
    let bound = match resolve_bindings(nt, &rule.name, grammar) {
        Ok(b) => b,
        Err(BindError::AtFrontier) => return (TreeStatus::Partial(Type::Universe), None),
        Err(BindError::Malformed) => return (TreeStatus::Malformed, None),
    };

    debug_trace!("typing", "got bindings: {:#?}", bound);

    // 2. Initialize substitution from type bindings
    let mut subst_map = extract_type_bindings(&bound);

    // 3. Check all premises
    for premise in &rule.premises {
        match check_premise(premise, &bound, grammar, ctx, depth, &mut subst_map) {
            PremiseResult::Ok => {}
            PremiseResult::Fail => return (TreeStatus::Malformed, None),
            PremiseResult::Partial => return (TreeStatus::Partial(Type::Universe), None),
        }
    }

    // 4. Evaluate conclusion and extract context transform
    let status = eval_conclusion(&rule.conclusion, &bound, ctx, &subst_map);

    // 5. Check for context transform in conclusion (e.g., Γ -> Γ[x:τ])
    let new_ctx = extract_context_transform(&rule.conclusion, &bound, ctx, &subst_map);

    (status, new_ctx)
}

/// Extract context transform from a conclusion like `Γ -> Γ[x:τ] ⊢ τ`
fn extract_context_transform(
    conc: &crate::logic::typing::Conclusion,
    bound: &HashMap<String, Node>,
    base_ctx: &Context,
    subst_map: &Substitution,
) -> Option<Context> {
    // Check if conclusion has an output context transform
    let output = conc.context.output.as_ref()?;

    // Build extended context from output setting
    let mut new_ctx = base_ctx.clone();

    for (var_name, ty_expr) in &output.extensions {
        // Resolve the variable name binding to get actual name
        let node = bound.get(var_name)?;
        let name = node_text(node)?;

        // Apply substitution to type expression
        let ty = subst(ty_expr, subst_map);

        new_ctx = new_ctx.extend(name, ty);
    }

    Some(new_ctx)
}

// ============================================================================
// Premise Checking
// ============================================================================

enum PremiseResult {
    Ok,
    Fail,
    Partial,
}

fn check_premise(
    premise: &crate::logic::typing::Premise,
    bound: &HashMap<String, Node>,
    grammar: &Grammar,
    base_ctx: &Context,
    depth: usize,
    subst_map: &mut Substitution,
) -> PremiseResult {
    // Build extended context from setting
    let ctx = match &premise.setting {
        Some(setting) => {
            match build_ctx_extension(setting, bound, base_ctx, subst_map) {
                Some(c) => c,
                None => return PremiseResult::Partial, // Can't build context yet
            }
        }
        None => base_ctx.clone(),
    };

    // Check judgment
    match &premise.judgment {
        Some(TypingJudgment::Ascription((term_var, expected_ty))) => {
            let node = match bound.get(term_var) {
                Some(n) => n,
                None => return PremiseResult::Partial,
            };

            match check_node(node, grammar, &ctx, depth + 1) {
                TreeStatus::Valid(actual) => {
                    if unify(&actual, expected_ty, subst_map) {
                        PremiseResult::Ok
                    } else {
                        PremiseResult::Fail
                    }
                }
                TreeStatus::Partial(_) => PremiseResult::Partial,
                TreeStatus::Malformed | TreeStatus::TooDeep => PremiseResult::Fail,
            }
        }

        Some(TypingJudgment::Membership(var_name, _)) => {
            let name = match bound.get(var_name).and_then(node_text) {
                Some(n) => n,
                None => return PremiseResult::Partial,
            };

            if ctx.lookup(&name).is_some() {
                PremiseResult::Ok
            } else {
                PremiseResult::Fail
            }
        }

        Some(TypingJudgment::Operation { left, op, right }) => {
            // Apply current substitution to both types first
            let left_ty = subst(left, subst_map);
            let right_ty = subst(right, subst_map);

            match op {
                TypeOperation::Equality => {
                    // Types must unify (be structurally equal or unifiable via meta vars)
                    if unify(&left_ty, &right_ty, subst_map) {
                        PremiseResult::Ok
                    } else {
                        PremiseResult::Fail
                    }
                }
                TypeOperation::Inclusion => {
                    // τ₁ ⊆ τ₂ means τ₁ is a subtype of τ₂
                    // For now: structural equality or left is Empty or right is Universe
                    if unify(&left_ty, &right_ty, subst_map)
                        || matches!(left_ty, Type::Empty)
                        || matches!(right_ty, Type::Universe)
                    {
                        PremiseResult::Ok
                    } else {
                        PremiseResult::Fail
                    }
                }
            }
        }

        None => PremiseResult::Ok, // Setting-only premise
    }
}

fn build_ctx_extension(
    setting: &crate::logic::typing::rule::TypeSetting,
    bound: &HashMap<String, Node>,
    base: &Context,
    subst_map: &Substitution,
) -> Option<Context> {
    let mut ctx = base.clone();

    for (var_name, ty_expr) in &setting.extensions {
        let node = bound.get(var_name)?;
        let name = node_text(node)?;
        let ty = subst(ty_expr, subst_map);
        ctx = ctx.extend(name, ty);
    }

    Some(ctx)
}

// ============================================================================
// Conclusion Evaluation
// ============================================================================

fn eval_conclusion(
    conc: &crate::logic::typing::Conclusion,
    bound: &HashMap<String, Node>,
    ctx: &Context,
    subst_map: &Substitution,
) -> TreeStatus {
    let ty = match &conc.kind {
        ConclusionKind::Type(t) => subst(t, subst_map),

        ConclusionKind::ContextLookup(_, var_name) => {
            let node = match bound.get(var_name) {
                Some(n) => n,
                None => return TreeStatus::Partial(Type::Universe),
            };
            let name = match node_text(node) {
                Some(n) => n,
                None => return TreeStatus::Partial(Type::Universe),
            };
            match ctx.lookup(&name) {
                Some(t) => t.clone(),
                None => return TreeStatus::Malformed,
            }
        }
    };

    TreeStatus::Valid(ty)
}
