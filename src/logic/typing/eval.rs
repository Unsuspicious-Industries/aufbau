//! Type Evaluation - Single Tree Checker
//!
//! Clean API: `check_tree` evaluates ONE tree, returning its status.
//! Forest filtering is done at a higher level by the caller.
use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::{NonTerminal, Terminal};
use crate::logic::typing::core::{Context, TreeStatus};
use crate::logic::typing::ops::{equal, subtype};
use crate::logic::typing::rule::{ConclusionKind, TypeOperation};
use crate::logic::typing::{Conclusion, Premise, Type, TypeSetting, TypingJudgment, TypingRule};
use crate::{debug_error, debug_trace};
use std::collections::HashMap;

use super::binding::*;
use super::core::*;

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
    check_node(&tref, grammar, ctx, 0, &mut type_cache)
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
    if let Some(cached_type) = type_cache.get(node.path()) {
        debug_trace!("typing", "Got cached type : {:?}", cached_type);
        return TreeStatus::Valid(cached_type.clone());
    }

    // Handle terminals uniformly (OK to clone terminals)
    if node.is_terminal() {
        if let Some(term) = node.as_terminal() {
            match term {
                // Terminals are unconstrained - they're just syntax, not type constraints
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
            return apply_rule(tref, rule, grammar, ctx, depth, type_cache);
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
    drill_only_child(tref, grammar, ctx, depth, type_cache)
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

    // Cache the computed type (both valid and partial)
    match &status {
        TreeStatus::Valid(ty) | TreeStatus::Partial(ty) => {
            type_cache.insert(tref.path_vec(), ty.clone());
        }
        _ => {}
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
    if let Some(cached_type) = type_cache.get(tref.path()) {
        return (TreeStatus::Valid(cached_type.clone()), None);
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
            return apply_rule_with_context_output(tref, rule, grammar, ctx, depth, type_cache);
        }
    }

    // No rule - try to drill and propagate any context transform from children
    drill_only_child_with_context(tref, grammar, ctx, depth, type_cache)
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

        // Cache the type from the single child for the parent
        match &result.0 {
            TreeStatus::Valid(ty) | TreeStatus::Partial(ty) => {
                type_cache.insert(tref.path_vec(), ty.clone());
            }
            _ => {}
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

    debug_trace!(
        "eval",
        "Multiple NT children (2+) without typing rule - returning Any : {:?}",
        tref.get_nt()
    );
    // Multiple NT children (2+) without a typing rule
    // Return Any because a tree with no rule can be anything (no constraint)
    (TreeStatus::Valid(Type::Any), None)
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

    // map for pattern matching meta variables
    // ?A -> ?B
    let mut map: HashMap<String, Type> = HashMap::new();

    for premise in &rule.premises {
        match check_premise(
            tref, premise, &bound, grammar, ctx, depth, &mut map, type_cache,
        ) {
            PremiseResult::Ok | PremiseResult::Partial => {}
            PremiseResult::Fail => return (TreeStatus::Malformed, None),
            // do something about the apartial state
            //PremiseResult::Partial => return (TreeStatus::Partial(Type::Universe), None),
        }
        debug_trace!("typing", "Map after premise : {:?}", map);
    }

    let status = eval_conclusion(tref, &rule.conclusion, &bound, ctx, &mut map);

    debug_trace!("typing", "Map after conclusion : {:?}", map);
    debug_trace!("typing", "Status after conclusion : {:?}", status);

    // Cache the computed type (both valid and partial)
    match &status {
        TreeStatus::Valid(ty) | TreeStatus::Partial(ty) => {
            type_cache.insert(tref.path_vec(), ty.clone());
        }
        _ => {}
    }

    // Check for context transform in conclusion (e.g., Γ -> Γ[x:τ])
    let new_ctx = extract_context_transform(tref, &rule.conclusion, &bound, ctx, &mut map);

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

enum PremiseResult {
    Ok,
    Fail,
    Partial,
}

fn check_premise(
    tref: &TreeRef,
    premise: &Premise,
    bound: &Bindings,
    grammar: &Grammar,
    base_ctx: &Context,
    depth: usize,
    map: &mut HashMap<String, Type>,
    type_cache: &mut HashMap<TreePath, Type>,
) -> PremiseResult {
    let name = tref.name().unwrap_or("?");
    debug_trace!("typing", "Checking premise {} for {}", premise, name);

    // Build extended context from setting
    let ctx = match &premise.setting {
        Some(setting) => {
            match build_ctx_extension(tref, setting, bound, base_ctx, None) {
                Some(c) => c,
                None => {
                    debug_trace!("typing", "Failed to build context");
                    return PremiseResult::Partial; // Can't build context yet
                }
            }
        }
        None => base_ctx.clone(),
    };

    // Check judgment
    match &premise.judgment {
        Some(TypingJudgment::Ascription((term_var, expected_ty))) => {
            debug_trace!(
                "typing",
                "Checking ascription {} : {}",
                term_var,
                expected_ty
            );

            // Get the path to the bound variable and construct full path
            let var_ty = match bound.get(term_var) {
                Binding::Full(p) => {
                    // getting a **real type** here
                    match check_node(
                        &tref.get_path(p.clone()),
                        grammar,
                        &ctx,
                        depth + 1,
                        type_cache,
                    ) {
                        TreeStatus::Valid(t) => t,
                        TreeStatus::Partial(t) => t,
                        TreeStatus::Malformed | TreeStatus::TooDeep => return PremiseResult::Fail,
                    }
                }
                Binding::Partial(p) => Type::PathOf(Box::new(Type::Any), p.clone()),
                Binding::None => return PremiseResult::Fail,
            };

            debug_trace!("typing", "Got variable type : {:?}", var_ty);
            debug_trace!("typing", "Map before check : {:?}", map);
            let right_ty = match has_meta(&expected_ty) {
                true => match solve_meta(&expected_ty, map) {
                    Ok(t) => match solve_binding(tref, &t, bound) {
                        Ok(ty) => ty,
                        Err(_) => return PremiseResult::Fail,
                    },
                    Err(_) => {
                        debug_trace!(
                            "typing",
                            "Extending map with {:?} = {:?}",
                            expected_ty,
                            var_ty
                        );
                        match set_meta(&expected_ty, &var_ty, map) {
                            Ok(_) => {
                                debug_trace!("typing", "Extended meta map to {:?}", map);
                                var_ty.clone() // allow pass
                            }
                            Err(e) => {
                                debug_trace!("typing", "Failed to extend meta map: {}", e);
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
                "Got ascription variables : {} ?= {}",
                var_ty,
                right_ty
            );

            // If the expected type does not contain a meta variable,
            // we can simply check if the variable's type matches it
            if equal(&var_ty, &right_ty, &ctx) {
                debug_trace!("premise", "Equal types");
                PremiseResult::Ok
            } else {
                debug_trace!("premise", "Unequal types");
                PremiseResult::Fail
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
                Some(_n) => {
                    debug_trace!("eval", "Got full {} for context lookup", name);
                    if ctx.lookup(&name).is_some() {
                        PremiseResult::Ok
                    } else {
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
            let left_ty = match solve_meta(left, map) {
                Ok(ty) => ty,
                Err(_) => return PremiseResult::Fail,
            };
            let right_ty = match solve_meta(right, map) {
                Ok(ty) => ty,
                Err(_) => return PremiseResult::Fail,
            };
            match op {
                TypeOperation::Equality => {
                    // Types must unify (be structurally equal or unifiable via meta vars)
                    if equal(&left_ty, &right_ty, &ctx) {
                        PremiseResult::Ok
                    } else {
                        PremiseResult::Fail
                    }
                }
                TypeOperation::Inclusion => {
                    // τ₁ ⊆ τ₂ means τ₁ is a subtype of τ₂
                    // For now: structural equality or left is None or right is Any
                    if subtype(&left_ty, &right_ty, &ctx) {
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

// this is recusrively extending context
// this means only nodes below are affected, not parralels.
fn build_ctx_extension(
    tref: &TreeRef,
    setting: &TypeSetting,
    bound: &Bindings,
    base: &Context,
    map: Option<&HashMap<String, Type>>,
) -> Option<Context> {
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
                    return None;
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
                return None;
            }
        };

        ctx = match bound.get_full(var_name) {
            Some(n) => {
                let name = match tref.node_text_path(n) {
                    Some(n) => n,
                    None => {
                        debug_trace!("build_ctx_extension", "Failed to get name for {}", var_name);
                        return None;
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
                        return None;
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
                            return None;
                        }
                    }
                }
                None => {
                    debug_trace!("build_ctx_extension", "No binding for {}", var_name);
                    return None;
                }
            },
        };
    }
    debug_trace!("build_ctx_extension", "Got now context {:?}", ctx);
    Some(ctx)
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
            debug_trace!("typing_conclusion", "Looking up variable {:?}", var_name);
             if let Some(path) = bound.get_full(var_name) {
                    let name = match tref.node_text_path(path) {
                        Some(n) => n,
                        None => return TreeStatus::Partial(Type::Path(path.clone())),
                    };
                    match ctx.lookup(&name) {
                        Some(t) => TreeStatus::Valid(t.clone()),
                        None => return TreeStatus::Partial(Type::None), // Variable might be in partial context
                    }
                }
            else {
                if let Some(path) = bound.get_partial(var_name) {
                    match tref.node_text_path(path) {
                        Some(val) => {
                            // Check for context entries matching this prefix
                            if  let Some(t) = ctx.lookup(&val) {
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

// utils

fn has_meta(ty: &Type) -> bool {
    debug_trace!("eval", "Checking is {} has Meta", ty);
    match ty {
        Type::Meta(_) => true,
        Type::Arrow(a, b) => has_meta(a) || has_meta(b),
        Type::Not(a) => has_meta(a),
        _ => false,
    }
}

fn set_meta(base: &Type, setter: &Type, map: &mut HashMap<String, Type>) -> Result<(), String> {
    // both meta and sertter should have the same structure
    match (base, setter) {
        (Type::Meta(name), s) => {
            debug_trace!("set_meta", "Inserting ?{} := {}", name, setter);
            map.insert(name.clone(), s.clone());
            Ok(())
        }
        (Type::Arrow(a, b), Type::Arrow(a2, b2)) => {
            set_meta(a, a2, map)?;
            set_meta(b, b2, map)?;
            Ok(())
        }
        (Type::Not(a), Type::Not(a2)) => {
            set_meta(a, a2, map)?;
            Ok(())
        }
        (_, Type::PathOf(t, _)) | (_, Type::Partial(t, _)) => set_meta(base, t, map),

        // Anys
        (Type::Arrow(t1, t2), Type::Any) => {
            set_meta(t1, &Type::Any, map)?;
            set_meta(t2, &Type::Any, map)?;
            Ok(())
        }
        (Type::Not(t1), Type::Any) => {
            set_meta(t1, &Type::Any, map)?;
            Ok(())
        }

        _ => Err("Couldn't pattern match".to_string()),
    }
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
                    // Solving bindings gets us a raw type
                    return Type::parse_raw(&tval).map_err(|e| {
                        debug_error!(
                            "typing",
                            "Failed to parse raw type from binding id='{}' text='{}': {}",
                            id,
                            tval,
                            e
                        );
                        e
                    });
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
        Type::Not(a) => {
            let a = solve_binding(tref, a, bound)?;
            return Ok(Type::Not(Box::new(a)));
        }
        _ => {
            return Ok(ty.clone());
        }
    }
}
