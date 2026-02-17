//! Type Evaluation - Single Tree Checker
//!
//! Clean API: `check_tree` evaluates ONE tree, returning its status.
//! Forest filtering is done at a higher level by the caller.

use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{NonTerminal, Terminal};
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::ops::{equal, subtype, Unifier, UnifyResult};
use crate::logic::typing::rule::{ConclusionKind, TypeOperation};
use crate::logic::typing::{Conclusion, Premise, Type, TypeSetting, TypingJudgment, TypingRule};
use crate::{debug_error, debug_info, debug_trace};
use std::cell::Cell;
use std::collections::HashMap;

use super::binding::*;
use super::core::*;

thread_local! {
    static TYPE_CACHE_ENABLED: Cell<bool> = Cell::new(true);
}

pub fn set_type_cache_enabled(enabled: bool) -> bool {
    TYPE_CACHE_ENABLED.with(|flag| {
        let prev = flag.get();
        flag.set(enabled);
        prev
    })
}

fn type_cache_enabled() -> bool {
    TYPE_CACHE_ENABLED.with(|flag| flag.get())
}

// ============================================================================
// Core API - single tree checking
// ============================================================================
/// Check a single tree → TreeStatus
pub fn check_tree(root: &NonTerminal, grammar: &Grammar) -> TreeStatus {
    check_tree_with_context(root, grammar, &Context::new())
}

/// Check with initial context
pub fn check_tree_with_context(root: &NonTerminal, grammar: &Grammar, ctx: &Context) -> TreeStatus {
    // Create empty type cache and start checking from root (empty path)
    let mut type_cache = HashMap::new();
    let tref = TreeRef::new(root, vec![]);
    let (status, _) = check_node_with_context_output(&tref, grammar, ctx, 0, &mut type_cache);
    status
}

/// Predicate: any tree in forest is well-typed?
pub fn evaluate_typing(roots: &[NonTerminal], grammar: &Grammar) -> bool {
    roots.iter().any(|r| check_tree(r, grammar).is_ok())
}

// ============================================================================
// Node Checking (recursive descent)
// ============================================================================

pub fn check_node(
    node: &TreeRef,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> TreeStatus {
    if depth > 50 {
        return TreeStatus::TooDeep;
    }

    // Check if we already computed the type for this path
    if type_cache_enabled() {
        if let Some(cached_type) = type_cache.get(node.path()) {
            debug_trace!("typing", "Got cached type : {:?}", cached_type);
            return TreeStatus::Valid(cached_type.clone());
        }
    }

    // Handle terminals uniformly (OK to clone terminals)
    if node.is_terminal() {
        if let Some(term) = node.as_terminal() {
            match term {
                // Terminals are unconstrained because they're just syntax, not type constraints
                Terminal::Complete { .. } => TreeStatus::Valid(Type::Any),
                Terminal::Partial { .. } => TreeStatus::Partial(Type::Any),
            }
        } else {
            TreeStatus::Malformed
        }
    } else if node.is_nt() {
        check_nt(node, grammar, ctx, depth, type_cache)
    } else {
        TreeStatus::Malformed
    }
}

fn check_nt(
    tref: &TreeRef,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> TreeStatus {
    // If this production has a typing rule, apply it
    if let Some(rule_name) = tref.rule() {
        if let Some(rule) = grammar.typing_rules.get(rule_name) {
            let name = tref.name().unwrap_or("?");
            debug_trace!(
                "eval",
                "Applying rule {} to {} at path {:?}",
                rule_name,
                name,
                tref.path()
            );
            let status = apply_rule(tref, rule, grammar, ctx, depth, type_cache);
            if let Some(nt) = tref.get_nt() {
                if nt.is_complete() && matches!(status, TreeStatus::Partial(_)) {
                    return TreeStatus::Malformed;
                }
            }
            return status;
        }
    }
    let name = tref.name().unwrap_or("?");
    debug_trace!(
        "eval",
        "No typing rule for {} at path {:?}, drilling",
        name,
        tref.path()
    );

    // No rule - check for transparent wrapper pattern
    // HEURISTIC: Only-child drilling
    //   - If production has exactly ONE non-terminal child, drill through it
    //   - This handles wrapper productions like `Term ::= BaseTerm`
    //   - Productions with multiple children or only terminals return Any
    let status = drill_only_child(tref, grammar, ctx, depth, type_cache);
    if let Some(nt) = tref.get_nt() {
        if nt.is_complete() && matches!(status, TreeStatus::Partial(_)) {
            return TreeStatus::Malformed;
        }
    }
    status
}

/// Drill through "only-child" wrapper productions
///
/// SEMANTICS:
/// - Productions without typing rules are "transparent" if they have exactly
///   one non-terminal child
/// - The type of the production is the type of that single child
/// - If there are 0 or 2+ non-terminal children, it's an error (no typing rule and no direct NT descendant)
/// - Terminals (like ')', literals) have no type significance
fn drill_only_child(
    tref: &TreeRef,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> TreeStatus {
    let (status, _) = drill_only_child_with_context(tref, grammar, ctx, depth, type_cache);

    // Cache only fully valid types
    if type_cache_enabled() {
        if let TreeStatus::Valid(ty) = &status {
            type_cache.insert(tref.path_vec(), ty.clone());
        }
    }

    status
}

/// Check a node and return both its status AND any context transform it produces
fn check_node_with_context_output(
    tref: &TreeRef,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> (TreeStatus, Option<Context>) {
    if depth > 50 {
        return (TreeStatus::TooDeep, None);
    }

    // Check if we already computed the type for this path
    if type_cache_enabled() {
        if let Some(cached_type) = type_cache.get(tref.path()) {
            return (TreeStatus::Valid(cached_type.clone()), None);
        }
    }

    // Handle terminals uniformly (OK to clone terminals)
    if tref.is_terminal() {
        if let Some(term) = tref.as_terminal() {
            let status = match term {
                // Terminals are unconstrained - they're just syntax, not type constraints
                Terminal::Complete { .. } => TreeStatus::Valid(Type::Any),
                Terminal::Partial { .. } => TreeStatus::Partial(Type::Any),
            };
            return (status, None);
        }
    }

    // Handle non-terminals
    if tref.is_nt() {
        check_nt_with_context_output(tref, grammar, ctx, depth, type_cache)
    } else {
        (TreeStatus::Malformed, None)
    }
}

/// Check non-terminal and return any context transform
fn check_nt_with_context_output(
    tref: &TreeRef,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> (TreeStatus, Option<Context>) {
    // If this production has a typing rule, apply it and check for context transform
    if let Some(rule_name) = tref.rule() {
        if let Some(rule) = grammar.typing_rules.get(rule_name) {
            let (status, ctx_out) =
                apply_rule_with_context_output(tref, rule, grammar, ctx, depth, type_cache);
            if let Some(nt) = tref.get_nt() {
                if nt.is_complete() && matches!(status, TreeStatus::Partial(_)) {
                    return (TreeStatus::Malformed, None);
                }
            }
            return (status, ctx_out);
        }
    }

    // No rule - try to drill and propagate any context transform from children
    let (status, ctx_out) = drill_only_child_with_context(tref, grammar, ctx, depth, type_cache);
    if let Some(nt) = tref.get_nt() {
        if nt.is_complete() && matches!(status, TreeStatus::Partial(_)) {
            return (TreeStatus::Malformed, None);
        }
    }
    (status, ctx_out)
}

/// Drill through productions, propagating context transforms
fn drill_only_child_with_context(
    tref: &TreeRef,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> (TreeStatus, Option<Context>) {
    // Collect non-terminal children indices using path-based access
    let nt_indices = tref.nt_child_indices();

    // Only drill if exactly one non-terminal child - propagate context transform
    if nt_indices.len() == 1 {
        let child_tref = tref.get(nt_indices[0]);
        let result =
            check_node_with_context_output(&child_tref, grammar, ctx, depth + 1, type_cache);

        // Cache only fully valid types from the single child for the parent
        if type_cache_enabled() {
            if let TreeStatus::Valid(ty) = &result.0 {
                type_cache.insert(tref.path_vec(), ty.clone());
            }
        }

        return result;
    }

    // Zero NT children (only terminals)
    if nt_indices.is_empty() {
        // Productions with only terminals and no typing rule are unconstrained
        // At frontier: partial (could accept more input)
        // Not at frontier: Any (no constraint on what this represents)
        if tref.is_at_frontier() {
            return (TreeStatus::Partial(Type::Any), None);
        } else {
            return (TreeStatus::Valid(Type::Any), None);
        }
    }

    // Multiple NT children (2+) without a typing rule.
    // Evaluate siblings left-to-right and thread context updates so list-like
    // productions can model statement-level context effects.
    let mut sibling_ctx = ctx.clone();
    let mut sibling_ctx_changed = false;
    let mut any_partial = false;

    for idx in nt_indices {
        let child_tref = tref.get(idx);
        let (child_status, child_ctx) = check_node_with_context_output(
            &child_tref,
            grammar,
            &sibling_ctx,
            depth + 1,
            type_cache,
        );

        match child_status {
            TreeStatus::Valid(_) => {}
            TreeStatus::Partial(_) => any_partial = true,
            TreeStatus::Malformed => return (TreeStatus::Malformed, None),
            TreeStatus::TooDeep => return (TreeStatus::TooDeep, None),
        }

        if let Some(next_ctx) = child_ctx {
            sibling_ctx = next_ctx;
            sibling_ctx_changed = true;
        }
    }

    let status = if any_partial {
        TreeStatus::Partial(Type::Any)
    } else {
        TreeStatus::Valid(Type::Any)
    };

    // Cache the computed type for this wrapper.
    type_cache.insert(tref.path_vec(), Type::Any);

    if sibling_ctx_changed {
        (status, Some(sibling_ctx))
    } else {
        (status, None)
    }
}
// ============================================================================
// Rule Application
// ============================================================================

fn apply_rule(
    tref: &TreeRef,
    rule: &TypingRule,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> TreeStatus {
    let (status, _) = apply_rule_with_context_output(tref, rule, grammar, ctx, depth, type_cache);
    status
}

/// Apply a rule and return both the status and any context transform
fn apply_rule_with_context_output(
    tref: &TreeRef,
    rule: &TypingRule,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> (TreeStatus, Option<Context>) {
    // Get NT only when needed for resolve_bindings (which requires cloning for validation)
    // But we minimize cloning by only getting it once
    let nt = match tref.get_nt() {
        Some(nt) => nt.clone(), // nocopy
        None => return (TreeStatus::Malformed, None),
    };

    let bound = match resolve_bindings(&nt, &rule.name, grammar) {
        Ok(b) => {
            debug_trace!("eval", "Bindings for rule {}: {:?}", rule.name, b);
            b
        }
        Err(BindError::AtFrontier) => {
            debug_trace!("eval", "Bindings at frontier for rule {}", rule.name);
            return (TreeStatus::Partial(Type::Any), None);
        }
        Err(BindError::Malformed) => {
            debug_trace!("eval", "Malformed bindings for rule {}", rule.name);
            return (TreeStatus::Malformed, None);
        }
    };

    debug_trace!("typing", "got bindings: {:#?}", bound);

    // Unifier for meta variable resolution across premises.
    // Replaces the ad-hoc HashMap<String, Type> with proper unification.
    let mut unifier = Unifier::new();
    let mut any_partial = false;

    // Context lanes let a rule express multiple sibling scopes.
    // The primary lane is the one whose effects propagate upward by default.
    let primary_ctx_name = if !rule.conclusion.context.input.is_empty() {
        rule.conclusion.context.input.clone()
    } else {
        rule.premises
            .iter()
            .find_map(|p| p.setting.as_ref().map(|s| s.name.clone()))
            .unwrap_or_else(|| "Γ".to_string())
    };

    let mut primary_ctx = ctx.clone();
    let mut lane_ctxs: HashMap<String, Context> = HashMap::new();
    lane_ctxs.insert(primary_ctx_name.clone(), primary_ctx.clone());
    let mut primary_ctx_changed = false;

    for premise in &rule.premises {
        let premise_input_ctx = match &premise.setting {
            Some(setting) => lane_ctxs
                .get(&setting.name)
                .cloned()
                .unwrap_or_else(|| primary_ctx.clone()),
            None => primary_ctx.clone(),
        };

        match check_premise(
            tref,
            premise,
            &bound,
            grammar,
            &premise_input_ctx,
            depth,
            &mut unifier,
            type_cache,
        ) {
            PremiseResult::Ok(next_ctx) => {
                if let Some(c) = next_ctx {
                    if let Some(setting) = &premise.setting {
                        lane_ctxs.insert(setting.name.clone(), c.clone());
                        if setting.name == primary_ctx_name {
                            primary_ctx = c;
                            primary_ctx_changed = true;
                        }
                    } else {
                        primary_ctx = c.clone();
                        lane_ctxs.insert(primary_ctx_name.clone(), c);
                        primary_ctx_changed = true;
                    }
                }
            }
            PremiseResult::Fail => return (TreeStatus::Malformed, None),
            PremiseResult::Partial => {
                // Don't short-circuit: keep checking remaining premises.
                // A later premise may definitively Fail, making the tree Malformed.
                any_partial = true;
            }
        }
        debug_trace!("typing", "Unifier after premise : {:?}", unifier);
    }

    // If any premise was partial (but none failed), the rule is partial.
    if any_partial {
        return (TreeStatus::Partial(Type::Any), None);
    }

    let map = unifier.as_map();
    let status = eval_conclusion(tref, &rule.conclusion, &bound, &primary_ctx, map);

    debug_trace!("typing", "Unifier after conclusion : {:?}", unifier);
    debug_trace!("typing", "Status after conclusion : {:?}", status);

    // Cache only fully valid types
    if type_cache_enabled() {
        if let TreeStatus::Valid(ty) = &status {
            type_cache.insert(tref.path_vec(), ty.clone());
        }
    }

    // Check for context transform in conclusion (e.g., Γ -> Γ[x:τ])
    let map = unifier.as_map();
    let mut new_ctx = extract_context_transform(tref, &rule.conclusion, &bound, &primary_ctx, map);
    // If the rule did not define an explicit context transform, pass through
    // primary-lane effects so surrounding siblings/parents can observe updates.
    if new_ctx.is_none() && primary_ctx_changed {
        new_ctx = Some(primary_ctx.clone());
    }

    (status, new_ctx)
}

/// Extract context transform from a conclusion like `Γ -> Γ[x:τ] ⊢ τ`
/// This is modifying global scope context
fn extract_context_transform(
    tref: &TreeRef,
    conc: &Conclusion,
    bound: &Bindings,
    base_ctx: &Context,
    map: &HashMap<String, Type>,
) -> Option<Context> {
    // Check if conclusion has an output context transform
    let output = conc.context.output.as_ref()?;

    // Build extended context from output setting
    let mut new_ctx = base_ctx.clone();

    for (var_name, ty_expr) in &output.extensions {
        // Resolve the variable name binding to get actual name
        let path = bound.get_full(var_name)?;
        let name = tref.node_text_path(path)?;
        let ty = match solve_meta(ty_expr, map) {
            Ok(ty) => ty,
            Err(_) => return None,
        };
        // solve binding for the type
        let ty = match solve_binding(tref, &ty, bound) {
            Ok(ty) => ty,
            Err(_) => return None,
        };
        new_ctx = match new_ctx.extend(name, ty) {
            Ok(ctx) => ctx,
            Err(_) => return None,
        };
    }

    Some(new_ctx)
}

// ============================================================================
// Premise Checking
// ============================================================================
// this is where we do descent
// important

enum PremiseResult {
    Ok(Option<Context>),
    Fail,
    Partial,
}

/// Shared helper: recurse into a bound term and return its `TreeStatus` + propagated context.
/// Returns Ok((binding_kind, tree_status, propagated_ctx)) or Err(PremiseResult::Fail).
fn recurse_bound_term(
    tref: &TreeRef,
    term_var: &str,
    bound: &Bindings,
    grammar: &Grammar,
    ctx: &Context,
    depth: usize,
    type_cache: &mut HashMap<TreePath, Type>,
) -> Result<(Binding, TreeStatus, Option<Context>), PremiseResult> {
    match bound.get(term_var) {
        Binding::Full(p) => {
            let (child_status, child_ctx) = check_node_with_context_output(
                &tref.get_path(p.clone()),
                grammar,
                ctx,
                depth + 1,
                type_cache,
            );
            Ok((Binding::Full(p.clone()), child_status, child_ctx))
        }
        Binding::Partial(p) => {
            let child_ref = tref.get_path(p.clone());
            if child_ref.exists() {
                let (child_status, child_ctx) =
                    check_node_with_context_output(&child_ref, grammar, ctx, depth + 1, type_cache);
                Ok((Binding::Partial(p.clone()), child_status, child_ctx))
            } else {
                // No concrete child yet — treat as a partial path-of Any
                Ok((
                    Binding::Partial(p.clone()),
                    TreeStatus::Partial(Type::PathOf(Box::new(Type::Any), p.clone())),
                    None,
                ))
            }
        }
        Binding::None => Err(PremiseResult::Fail),
    }
}

fn check_premise(
    tref: &TreeRef,
    premise: &Premise,
    bound: &Bindings,
    grammar: &Grammar,
    base_ctx: &Context,
    depth: usize,
    unifier: &mut Unifier,
    type_cache: &mut HashMap<TreePath, Type>,
) -> PremiseResult {
    let name = tref.name().unwrap_or("?");
    debug_trace!("typing", "Checking premise {} for {}", premise, name);

    // Build extended context from setting
    let ctx = match &premise.setting {
        Some(setting) => {
            match build_ctx_extension(tref, setting, bound, base_ctx, Some(unifier.as_map())) {
                ContextExtensionResult::Ok(c) => c,
                ContextExtensionResult::Partial => {
                    debug_trace!("typing", "Failed to build context (partial)");
                    return PremiseResult::Partial; // Can't build context yet
                }
                ContextExtensionResult::Failed => {
                    debug_trace!("typing", "Failed to build context (failed)");
                    return PremiseResult::Fail; // Context extension failed
                }
            }
        }
        None => base_ctx.clone(),
    };

    unifier.set_context(&ctx);
    // Keep unifier in sync with the latest binding names so Γ(name) can
    // resolve through the current tree's bindings during subtyping/unification.
    unifier.set_binding_values(build_binding_value_map(tref, bound));

    let mut propagated_ctx: Option<Context> = None;
    let no_propagate = premise
        .setting
        .as_ref()
        .map(|s| s.no_propagate)
        .unwrap_or(false);

    // Check judgment
    match &premise.judgment {
        Some(TypingJudgment::Ascription((term_var, expected_ty))) => {
            debug_trace!(
                "typing",
                "Checking ascription {} : {}",
                term_var,
                expected_ty
            );

            if matches!(bound.get(term_var), Binding::None) {
                if let Some(nt) = tref.get_nt() {
                    if nt.production.rhs.is_empty() {
                        return PremiseResult::Ok(None);
                    }
                }
            }

            // Use shared helper to fetch bound term status + propagated ctx
            let (binding_kind, child_status, child_ctx) =
                match recurse_bound_term(tref, term_var, bound, grammar, &ctx, depth, type_cache) {
                    Ok(v) => v,
                    Err(e) => return e,
                };

            if let Some(next_ctx) = child_ctx {
                debug_info!("typing", "Propagating context from child: {:?}", next_ctx);
                if !no_propagate {
                    propagated_ctx = Some(next_ctx);
                }
            }

            // Determine variable type according to binding kind & child status
            let var_ty = match (binding_kind, child_status) {
                (Binding::Full(_), TreeStatus::Valid(t)) => t,
                (Binding::Full(_), TreeStatus::Partial(_)) => return PremiseResult::Partial,
                (Binding::Full(_), TreeStatus::Malformed | TreeStatus::TooDeep) => {
                    return PremiseResult::Fail;
                }
                (Binding::Partial(_), TreeStatus::Valid(t)) => t,
                (Binding::Partial(_), TreeStatus::Partial(t)) => t,
                (Binding::Partial(_), TreeStatus::Malformed | TreeStatus::TooDeep) => {
                    return PremiseResult::Fail;
                }
                _ => return PremiseResult::Fail,
            };

            debug_trace!("typing", "Got variable type : {}", var_ty);
            debug_trace!("typing", "Unifier before check : {:?}", unifier);

            // Resolve the expected type: first resolve bindings (Atom → Raw),
            // then use the unifier for meta variables.
            let right_ty = match has_meta(&expected_ty) {
                true => match unifier.apply(&expected_ty) {
                    Ok(t) => match solve_binding(tref, &t, bound) {
                        Ok(ty) => ty,
                        Err(_) => return PremiseResult::Fail,
                    },
                    Err(_) => {
                        // Meta variables not yet bound — use unification to learn them.
                        // First, resolve any grammar bindings in the expected type.
                        let expected_resolved = match solve_binding(tref, expected_ty, bound) {
                            Ok(ty) => ty,
                            // If binding resolution fails (Atom not in bound), keep original
                            Err(_) => expected_ty.clone(),
                        };
                        debug_trace!(
                            "typing",
                            "Unifying expected {:?} with actual {:?}",
                            expected_resolved,
                            var_ty
                        );
                        match unifier.unify(&expected_resolved, &var_ty) {
                            UnifyResult::Ok => {
                                debug_trace!("typing", "Unification succeeded: {:?}", unifier);
                                // After unification, var_ty is accepted — return Ok
                                return PremiseResult::Ok(propagated_ctx);
                            }
                            UnifyResult::Indeterminate => {
                                debug_trace!("typing", "Unification indeterminate (partial)");
                                // Can't determine yet — treat as partial (loose constraint)
                                return PremiseResult::Partial;
                            }
                            UnifyResult::Fail(e) => {
                                debug_trace!("typing", "Unification failed: {}", e);
                                return PremiseResult::Fail;
                            }
                        }
                    }
                },
                false => match solve_binding(tref, expected_ty, bound) {
                    Ok(ty) => ty,
                    Err(_) => return PremiseResult::Fail,
                },
            };

            debug_trace!(
                "typing",
                "Got ascription variables : {} ⊆ {}",
                var_ty,
                right_ty
            );

            // Both sides are now fully resolved (no meta variables).
            // Use subtyping with context call resolution.
            let var_ty_r = unifier.resolve_for_subtyping(&var_ty);
            let right_ty_r = unifier.resolve_for_subtyping(&right_ty);
            if is_indeterminate_subtyping_check(&var_ty_r, &right_ty_r) {
                debug_trace!("premise", "Subtyping indeterminate (unresolved)");
                PremiseResult::Partial
            } else if subtype(&var_ty_r, &right_ty_r) {
                debug_trace!("premise", "Subtype holds");
                PremiseResult::Ok(propagated_ctx)
            } else {
                debug_trace!("premise", "Subtype fails");
                PremiseResult::Fail
            }
        }

        Some(TypingJudgment::Check(term_var)) => {
            // `check(e)` — ensure the bound term `e` typechecks (we don't care about its type)
            debug_trace!("typing", "Checking (check) premise for {}", term_var);

            if matches!(bound.get(term_var), Binding::None) {
                if let Some(nt) = tref.get_nt() {
                    if nt.production.rhs.is_empty() {
                        return PremiseResult::Ok(None);
                    }
                }
            }

            // Use the shared helper and then treat any Partial status as Partial,
            // Valid status as Ok, and Malformed/TooDeep as Fail.
            let (_binding_kind, child_status, child_ctx) =
                match recurse_bound_term(tref, term_var, bound, grammar, &ctx, depth, type_cache) {
                    Ok(v) => v,
                    Err(e) => return e,
                };

            match child_status {
                TreeStatus::Valid(_) => {
                    if no_propagate {
                        PremiseResult::Ok(None)
                    } else {
                        PremiseResult::Ok(child_ctx)
                    }
                }
                TreeStatus::Partial(_) => PremiseResult::Partial,
                TreeStatus::Malformed | TreeStatus::TooDeep => PremiseResult::Fail,
            }
        }
        Some(TypingJudgment::Membership(var_name, _)) => {
            debug_trace!(
                "eval",
                "Checking membership judgement with context : {:?}",
                ctx
            );
            match bound
                .get_full(var_name)
                .and_then(|path| tref.node_text_path(path))
            {
                Some(name_value) => {
                    debug_trace!("eval", "Got full {} for context lookup", name_value);
                    if ctx.lookup(&name_value).is_some() {
                        debug_trace!("eval", "Context lookup succeeded for {}", name_value);
                        PremiseResult::Ok(None)
                    } else {
                        debug_trace!("eval", "Context lookup failed for {}", name_value);
                        PremiseResult::Fail
                    }
                }
                None => {
                    // search for partial
                    if let Some(path) = bound.get_partial(var_name) {
                        match tref.node_text_path(path) {
                            Some(val) => {
                                // Check for context entries matching this prefix
                                if let Some(_starts_with) = ctx.lookup_starts_with(&val) {
                                    PremiseResult::Partial // ok but partial
                                } else {
                                    PremiseResult::Fail
                                }
                            }
                            None => {
                                // Unresolved context lookup
                                PremiseResult::Partial
                            }
                        }
                    } else {
                        // error
                        return PremiseResult::Fail;
                    }
                }
            }
        }

        Some(TypingJudgment::Operation { left, op, right }) => {
            let left_applied = match unifier.apply(left) {
                Ok(ty) => ty,
                Err(_) => return PremiseResult::Fail,
            };
            let right_applied = match unifier.apply(right) {
                Ok(ty) => ty,
                Err(_) => return PremiseResult::Fail,
            };

            // Operations may refer to bound grammar variables (e.g. τ in ?A ⊆ τ).
            // Resolve those bindings before comparing/subtyping.
            let left_ty = match solve_binding(tref, &left_applied, bound) {
                Ok(ty) => ty,
                Err(_) => left_applied,
            };
            let right_ty = match solve_binding(tref, &right_applied, bound) {
                Ok(ty) => ty,
                Err(_) => right_applied,
            };

            match op {
                TypeOperation::Equality => {
                    // Types must unify (be structurally equal or unifiable via meta vars)
                    let left_ty_r = unifier.resolve_for_subtyping(&left_ty);
                    let right_ty_r = unifier.resolve_for_subtyping(&right_ty);
                    match equal(&left_ty_r, &right_ty_r) {
                        Some(true) => PremiseResult::Ok(None),
                        Some(false) => PremiseResult::Fail,
                        None => PremiseResult::Partial,
                    }
                }
                TypeOperation::Inclusion => {
                    // τ₁ ⊆ τ₂ means τ₁ is a subtype of τ₂
                    // For now: structural equality or left is None or right is Any
                    let left_ty_r = unifier.resolve_for_subtyping(&left_ty);
                    let right_ty_r = unifier.resolve_for_subtyping(&right_ty);
                    if is_indeterminate_subtyping_check(&left_ty_r, &right_ty_r) {
                        PremiseResult::Partial
                    } else if subtype(&left_ty_r, &right_ty_r) {
                        PremiseResult::Ok(None)
                    } else {
                        PremiseResult::Fail
                    }
                }
            }
        }

        None => {
            // error
            debug_trace!("typing", "Premise has no judgment");
            return PremiseResult::Fail;
        }
    }
}

enum ContextExtensionResult {
    Ok(Context),
    Partial, // Can't build context yet (incomplete bindings)
    Failed,  // Context extension failed (e.g., duplicate variable)
}

// Build a premise-local context by extending the selected input context lane.
// Upward/sibling propagation is handled by apply_rule_with_context_output.
fn build_ctx_extension(
    tref: &TreeRef,
    setting: &TypeSetting,
    bound: &Bindings,
    base: &Context,
    map: Option<&HashMap<String, Type>>,
) -> ContextExtensionResult {
    let mut ctx = base.clone();

    for (var_name, ty_expr) in &setting.extensions {
        let ty = if let Some(map) = map {
            match solve_meta(ty_expr, map) {
                Ok(ty) => ty,
                Err(_) => {
                    debug_trace!(
                        "build_ctx_extension",
                        "Failed to solve meta for {}",
                        var_name
                    );
                    return ContextExtensionResult::Partial;
                }
            }
        } else {
            ty_expr.clone()
        };
        // here we need to solve the binding
        let ty = match solve_binding(tref, &ty, bound) {
            Ok(ty) => ty,
            Err(_) => {
                debug_trace!(
                    "build_ctx_extension",
                    "Failed to solve binding for {}",
                    var_name
                );
                return ContextExtensionResult::Partial;
            }
        };

        ctx = match bound.get_full(var_name) {
            Some(n) => {
                let name = match tref.node_text_path(n) {
                    Some(n) => n,
                    None => {
                        debug_trace!("build_ctx_extension", "Failed to get name for {}", var_name);
                        return ContextExtensionResult::Partial;
                    }
                };
                debug_trace!(
                    "build_ctx_extension",
                    "Extending context with {} := {}",
                    var_name,
                    ty_expr
                );
                match ctx.extend(name, ty) {
                    Ok(c) => c,
                    Err(e) => {
                        debug_trace!(
                            "build_ctx_extension",
                            "Failed to extend context for {}: {}",
                            var_name,
                            e
                        );
                        return ContextExtensionResult::Failed;
                    }
                }
            }
            None => match bound.get_partial(var_name) {
                Some(p) => {
                    debug_trace!("build_ctx_extension", "Partial binding for {}", var_name);
                    match ctx.extend_unresolved(p.to_vec(), ty) {
                        Ok(c) => c,
                        Err(e) => {
                            debug_trace!(
                                "build_ctx_extension",
                                "Failed to extend unresolved context for {}: {}",
                                var_name,
                                e
                            );
                            return ContextExtensionResult::Failed;
                        }
                    }
                }
                None => {
                    debug_trace!("build_ctx_extension", "No binding for {}", var_name);
                    return ContextExtensionResult::Partial;
                }
            },
        };
    }
    debug_trace!("build_ctx_extension", "Got now context {:?}", ctx);
    ContextExtensionResult::Ok(ctx)
}

// ============================================================================
// Conclusion Evaluation
// ============================================================================

fn eval_conclusion(
    tref: &TreeRef,
    conc: &Conclusion,
    bound: &Bindings,
    ctx: &Context,
    map: &HashMap<String, Type>,
) -> TreeStatus {
    debug_trace!("typing_conclusion", "Using subst map : {:?}", map);

    match &conc.kind {
        ConclusionKind::Type(t) => {
            debug_trace!("typing_conclusion", "Got type {:?}", t);

            let bty = match solve_meta(t, map) {
                Ok(ty) => ty,
                Err(_) => {
                    debug_trace!("typing_conclusion", "Failed to solve meta type");
                    return TreeStatus::Malformed;
                }
            };
            match solve_binding(tref, &bty, &bound) {
                Ok(ty) => TreeStatus::Valid(ty),
                Err(_) => {
                    debug_trace!("typing_conclusion", "Failed to solve binding");
                    TreeStatus::Malformed
                }
            }
        }
        ConclusionKind::ContextLookup(_, var_name) => {
            debug_trace!(
                "typing_conclusion",
                "Looking up variable {:?} in context",
                var_name
            );
            if let Some(path) = bound.get_full(var_name) {
                let name = match tref.node_text_path(path) {
                    Some(n) => n,
                    None => return TreeStatus::Partial(Type::Path(path.clone())),
                };
                debug_trace!(
                    "typing_conclusion",
                    "Resolved variable name to {} in context",
                    name
                );
                match ctx.lookup(&name) {
                    Some(t) => TreeStatus::Valid(t.clone()),
                    None => return TreeStatus::Malformed, // variable fully resolved but not in Γ → type error
                }
            } else {
                if let Some(path) = bound.get_partial(var_name) {
                    match tref.node_text_path(path) {
                        Some(val) => {
                            // Check for context entries matching this prefix
                            if let Some(t) = ctx.lookup(&val) {
                                TreeStatus::Valid(t.clone())
                            } else if let Some(_starts_with) = ctx.lookup_starts_with(&val) {
                                TreeStatus::Partial(Type::PathOf(Box::new(Type::Any), path.clone()))
                            } else {
                                TreeStatus::Malformed
                            }
                        }
                        None => {
                            // Unresolved context lookup
                            TreeStatus::Partial(Type::PathOf(Box::new(Type::Any), path.clone()))
                        }
                    }
                } else {
                    // error
                    TreeStatus::Malformed
                }
            }
        }
    }
}

fn has_meta(ty: &Type) -> bool {
    debug_trace!("eval", "Checking is {} has Meta", ty);
    match ty {
        Type::Meta(_) => true,
        Type::Arrow(a, b) => has_meta(a) || has_meta(b),
        Type::Union(parts) => parts.iter().any(has_meta),
        Type::Not(a) => has_meta(a),
        _ => false,
    }
}

fn has_unresolved_subtyping_hole(ty: &Type) -> bool {
    match ty {
        // In this engine Any commonly means "unknown yet" for partial trees.
        // Treat it as indeterminate for strict inclusion checks.
        Type::Any | Type::Meta(_) | Type::Path(_) | Type::ContextCall(_, _) => true,
        Type::PathOf(inner, _) | Type::Partial(inner, _) | Type::Not(inner) => {
            has_unresolved_subtyping_hole(inner)
        }
        Type::Arrow(a, b) => has_unresolved_subtyping_hole(a) || has_unresolved_subtyping_hole(b),
        Type::Union(parts) => parts.iter().any(has_unresolved_subtyping_hole),
        Type::Atom(_) | Type::Raw(_) | Type::None => false,
    }
}

fn is_indeterminate_subtyping_check(left: &Type, right: &Type) -> bool {
    // τ ⊆ ⊤ is always true, so this case is never indeterminate.
    if matches!(right, Type::Any) {
        return false;
    }
    has_unresolved_subtyping_hole(left) || has_unresolved_subtyping_hole(right)
}

fn solve_meta(ty: &Type, map: &HashMap<String, Type>) -> Result<Type, String> {
    // check if value is a meta
    // atom is a binding | meta
    match ty {
        Type::Meta(name) => {
            if let Some(r) = map.get(name) {
                return Ok(r.clone());
            }
            // Unbound Meta are not allowed to remain unresolved.
            return Err(format!("Unbound Meta: {}", name));
        }
        Type::Arrow(a, b) => {
            let a = solve_meta(a, map)?;
            let b = solve_meta(b, map)?;
            return Ok(Type::Arrow(Box::new(a), Box::new(b)));
        }
        Type::Union(parts) => {
            let mut resolved = Vec::with_capacity(parts.len());
            for p in parts {
                resolved.push(solve_meta(p, map)?);
            }
            return Ok(Type::Union(resolved));
        }
        Type::Not(a) => {
            let a = solve_meta(a, map)?;
            return Ok(Type::Not(Box::new(a)));
        }
        _ => {}
    }
    Ok(ty.clone())
}

// utils
fn solve_binding(tref: &TreeRef, ty: &Type, bound: &Bindings) -> Result<Type, String> {
    debug_trace!(
        "binding",
        "Solving binding for type var {} in {:?}",
        ty,
        bound
    );
    match ty {
        Type::Atom(id) => {
            if let Some(r) = bound.get_full(id) {
                if let Some(tval) = tref.node_text_path(r) {
                    debug_trace!("typing", "Resolved type var {} to {}", id, tval);
                    // Solving bindings gets us a raw type. Try strict parse first,
                    // but accept partial parses (useful for completable/partial inputs).
                    match Type::parse_raw(&tval) {
                        Ok(ty) => return Ok(ty),
                        Err(e) => {
                            debug_trace!(
                                "typing",
                                "parse_raw mismatch for binding id='{}' text='{}': {}. Falling back to partial parse",
                                id,
                                tval,
                                e
                            );
                            match Type::parse_partial(&tval) {
                                Ok(ty) => {
                                    debug_trace!(
                                        "typing",
                                        "Parsed partial raw type '{}' as {:?}",
                                        tval,
                                        ty
                                    );
                                    return Ok(ty);
                                }
                                Err(e2) => {
                                    debug_error!(
                                        "typing",
                                        "Failed to parse partial raw type from binding id='{}' text='{}': {}",
                                        id,
                                        tval,
                                        e2
                                    );
                                    return Err(e2);
                                }
                            }
                        }
                    }
                } else {
                    debug_error!("binding", "Bad path in binding: {}", id);
                    return Err(format!("Bad path in binding: {}", id));
                }
            } else {
                if let Some(p) = bound.get_partial(id) {
                    debug_trace!(
                        "binding",
                        "Partial binding (unresolvable path) {} to {:?}",
                        id,
                        p
                    );
                    // Solving bindings gets us a raw type
                    // Partial bindings indicate incomplete resolution
                    return Ok(Type::Path(tref.get_path(p.clone()).path_vec()));
                } else {
                    debug_trace!("binding", "Unbound binding variable: {}", id);
                    return Err(format!("Unbound binding variable: {}", id));
                }
            }
        }
        // Meta variables (e.g. ?A, ?B) are not grammar bindings.
        // They can remain unresolved during inference.
        Type::Meta(name) => {
            return Ok(Type::Meta(name.clone()));
        }
        Type::Arrow(a, b) => {
            let a = solve_binding(tref, a, bound)?;
            let b = solve_binding(tref, b, bound)?;
            return Ok(Type::Arrow(Box::new(a), Box::new(b)));
        }
        Type::Union(parts) => {
            let mut resolved = Vec::with_capacity(parts.len());
            for p in parts {
                resolved.push(solve_binding(tref, p, bound)?);
            }
            return Ok(Type::Union(resolved));
        }
        Type::Not(a) => {
            let a = solve_binding(tref, a, bound)?;
            return Ok(Type::Not(Box::new(a)));
        }
        _ => {
            return Ok(ty.clone());
        }
    }
}

fn build_binding_value_map(tref: &TreeRef, bound: &Bindings) -> HashMap<String, String> {
    let mut values = HashMap::new();

    // Only include bindings that can be read as concrete text right now.
    // Missing paths remain absent so lookups stay indeterminate on partial trees.
    for (name, path) in bound.iter_full() {
        if let Some(text) = tref.node_text_path(path) {
            values.insert(name.clone(), text);
        }
    }

    for (name, path) in bound.iter_partial() {
        if let Some(text) = tref.node_text_path(path) {
            values.insert(name.clone(), text);
        }
    }

    values
}
