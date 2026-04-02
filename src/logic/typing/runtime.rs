use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::debug_trace;
use crate::logic::fusion::{
    BindingValue, TransitionError, TransitionResult, TypingContextSummary, TypingRuntime,
    TypingState,
};
use crate::logic::grammar::{Grammar, Production};
use crate::logic::parse::arena::{CtxId, NodeStatus, NtId, ParseArena, PathId, ProdId, TypeId};
use crate::logic::typing::rule::{ConclusionKind, Premise, TypeOperation, TypingJudgment};
use crate::logic::typing::{Context, Type, TypingRule, equal};
use crate::regex::Regex;

#[derive(Clone, Debug)]
pub struct RuleRuntime {
    grammar: Grammar,
    types: Rc<RefCell<Vec<Type>>>,
    type_ids: Rc<RefCell<HashMap<Type, TypeId>>>,
    contexts: Rc<RefCell<Vec<Context>>>,
}

impl RuleRuntime {
    /// Time: O(1). Space: O(1).
    pub fn new(grammar: Grammar) -> Self {
        let runtime = Self {
            grammar,
            types: Rc::new(RefCell::new(Vec::new())),
            type_ids: Rc::new(RefCell::new(HashMap::new())),
            contexts: Rc::new(RefCell::new(Vec::new())),
        };
        runtime.intern_type(Type::Any);
        runtime.intern_context(Context::new());
        runtime
    }

    /// Time: O(1) average. Space: O(1) amortized.
    pub fn intern_type(&self, ty: Type) -> TypeId {
        if let Some(id) = self.type_ids.borrow().get(&ty) {
            return *id;
        }
        let mut types = self.types.borrow_mut();
        let id = TypeId(types.len());
        types.push(ty.clone());
        self.type_ids.borrow_mut().insert(ty, id);
        id
    }

    /// Time: O(1). Space: O(1).
    pub fn type_of(&self, id: TypeId) -> Option<Type> {
        self.types.borrow().get(id.0).cloned()
    }

    /// Time: O(1) amortized. Space: O(1) amortized.
    pub fn intern_context(&self, ctx: Context) -> CtxId {
        let mut contexts = self.contexts.borrow_mut();
        let id = CtxId(contexts.len());
        contexts.push(ctx);
        id
    }

    /// Time: O(1). Space: O(1).
    pub fn context(&self, id: CtxId) -> Option<Context> {
        let ctx = self.contexts.borrow().get(id.0).cloned();
        if let Some(ref c) = ctx {
            debug_trace!("fusion_typing", "ctx[{}] = {:?}", id.0, c);
        }
        ctx
    }

    /// Benchmark/statistics helper: number of interned types.
    pub fn interned_type_count(&self) -> usize {
        self.types.borrow().len()
    }

    /// Benchmark/statistics helper: number of interned contexts.
    pub fn interned_context_count(&self) -> usize {
        self.contexts.borrow().len()
    }

    /// Time: O(P), where P is the number of productions in the grammar.
    /// Space: O(1).
    pub fn production_rule_name(&self, prod: ProdId) -> Option<&str> {
        self.production(prod).and_then(|prod| prod.rule.as_deref())
    }

    /// Time: O(P), where P is the number of productions in the grammar.
    /// Space: O(1).
    pub fn production(&self, prod: ProdId) -> Option<&Production> {
        let mut offset = 0usize;
        for idx in 0..self.grammar.production_count() {
            let productions = self.grammar.productions_by_idx(idx)?;
            if prod.0 < offset + productions.len() {
                return productions.get(prod.0 - offset);
            }
            offset += productions.len();
        }
        None
    }

    /// Time: O(d), where d is the path depth. Space: O(d).
    pub fn tree_path(&self, arena: &ParseArena, path: PathId) -> Vec<usize> {
        arena.walk(path).map(|step| step.child as usize).collect()
    }

    /// Design choice: productions without an explicit typing rule are transparent
    /// when they have exactly one typed child, and default to `Any` only for
    /// empty or multi-child structural wrappers.
    fn inferred_type(
        &self,
        prod: ProdId,
        state: &TypingState,
        children: &[TypingState],
    ) -> Option<TypeId> {
        let rule = self
            .production_rule_name(prod)
            .and_then(|name| self.grammar.typing_rules.get(name));
        match rule.map(|rule| &rule.conclusion.kind) {
            Some(ConclusionKind::Type(ty)) => Some(self.intern_type(ty.clone())),
            _ => match children {
                [child] => child.inferred.or(state.inferred).or(Some(TypeId(0))),
                [] => state.inferred.or(Some(TypeId(0))),
                // Heuristic transparency for syntactic wrappers like '(' Expr ')':
                // if there is exactly one non-Any child type, propagate it.
                _ => {
                    let meaningful: Vec<TypeId> = children
                        .iter()
                        .filter_map(|c| c.inferred)
                        .filter(|id| id.0 != 0)
                        .collect();
                    if meaningful.len() == 1 {
                        Some(meaningful[0])
                    } else {
                        Some(TypeId(0))
                    }
                }
            },
        }
    }

    fn binding_value<'a>(&self, state: &'a TypingState, name: &str) -> Option<&'a BindingValue> {
        state
            .bindings
            .iter()
            .rev()
            .find(|binding| binding.name == name)
    }

    fn child_binding_value<'a>(
        &self,
        children: &'a [TypingState],
        name: &str,
    ) -> Option<&'a BindingValue> {
        // Scope safety: only consider bindings *owned by the child itself* (at its direct path).
        // This prevents accidentally satisfying premises using leaked bindings from nested
        // subtrees or siblings, which is especially important for STLC variable shadowing.
        children.iter().rev().find_map(|child| {
            let p = child.path?;
            child
                .bindings
                .iter()
                .rev()
                .find(|b| b.name == name && b.path == p && b.value.is_some())
                .or_else(|| {
                    child
                        .bindings
                        .iter()
                        .rev()
                        .find(|b| b.name == name && b.path == p)
                })
        })
    }

    fn resolve_type(
        &self,
        ty: &Type,
        subst: &HashMap<String, Type>,
        state: &TypingState,
        children: &[TypingState],
        ctx: &Context,
    ) -> Option<Type> {
        match ty {
            Type::Meta(name) => subst
                .get(name)
                .cloned()
                .or_else(|| self.binding_type(state, children, name))
                .or(Some(Type::Meta(name.clone()))),
            // Atoms are not concrete types in the typing engine.
            // They are only placeholders for *bound names* (e.g. `τ` from `Type[τ]`),
            // and must resolve to a real type (typically `Raw(...)`) before typechecking.
            //
            // Per design: atoms never unify and are equal to nothing, so if we can't
            // resolve an atom via a binding, the rule must fail/partial.
            Type::Atom(name) => self.binding_type(state, children, name),
            Type::Arrow(left, right) => Some(Type::Arrow(
                Box::new(self.resolve_type(left, subst, state, children, ctx)?),
                Box::new(self.resolve_type(right, subst, state, children, ctx)?),
            )),
            Type::Array(inner) => Some(Type::Array(Box::new(
                self.resolve_type(inner, subst, state, children, ctx)?,
            ))),
            Type::Union(items) => Some(Type::Union(
                items
                    .iter()
                    .map(|item| self.resolve_type(item, subst, state, children, ctx))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Type::ContextCall(_, var) => {
                let name = self
                    .child_binding_value(children, var)
                    .or_else(|| self.binding_value(state, var))?
                    .value
                    .as_ref()?;
                ctx.lookup(name).cloned()
            }
            _ => Some(ty.clone()),
        }
    }

    fn binding_type(
        &self,
        state: &TypingState,
        children: &[TypingState],
        name: &str,
    ) -> Option<Type> {
        self.child_binding_value(children, name)
            .or_else(|| self.binding_value(state, name))
            .and_then(|binding| binding.value.as_ref())
            .and_then(|value| Type::parse_raw(value).ok())
    }

    fn unify(&self, expected: &Type, actual: &Type, subst: &mut HashMap<String, Type>) -> bool {
        match expected {
            Type::Meta(name) => {
                if let Some(bound) = subst.get(name) {
                    equal(bound, actual).unwrap_or(false)
                } else {
                    subst.insert(name.clone(), actual.clone());
                    true
                }
            }
            // Atoms are outlawed during typechecking; they must resolve away first.
            Type::Atom(_) => false,
            // Treat union expectations as "actual must match one arm".
            // This matches the common typing-rule intent `e : τ1 | τ2`.
            Type::Union(parts) => parts.iter().any(|p| self.unify(p, actual, subst)),
            Type::Arrow(a, b) => match actual {
                Type::Arrow(x, y) => self.unify(a, x, subst) && self.unify(b, y, subst),
                _ => false,
            },
            _ => equal(expected, actual).unwrap_or(false),
        }
    }

    fn apply_premise(
        &self,
        premise: &Premise,
        state: &TypingState,
        children: &[TypingState],
        ctx: &mut Context,
        subst: &mut HashMap<String, Type>,
    ) -> bool {
        let base_ctx = ctx.clone();
        let mut premise_ctx = ctx.clone();
        let setting_extends = premise
            .setting
            .as_ref()
            .is_some_and(|s| !s.extensions.is_empty());
        if let Some(setting) = &premise.setting {
            for (name, ext_ty) in &setting.extensions {
                let Some(binding) = self
                    .child_binding_value(children, name)
                    .or_else(|| self.binding_value(state, name))
                else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail reason=no_binding name={}",
                        name
                    );
                    return false;
                };
                let Some(value) = &binding.value else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail reason=no_value name={}",
                        name
                    );
                    return false;
                };
                let Some(resolved) =
                    self.resolve_type(ext_ty, subst, state, children, &premise_ctx)
                else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail reason=unresolved name={}",
                        name
                    );
                    return false;
                };
                if premise_ctx
                    .extend(value.clone(), resolved)
                    .map(|next| premise_ctx = next)
                    .is_err()
                {
                    return false;
                }
            }
        }

        let Some(judgment) = &premise.judgment else {
            *ctx = premise_ctx;
            return true;
        };
        match judgment {
            TypingJudgment::Membership(var, _) => {
                // Check if the binding `var` has a value that exists in context
                let binding = self
                    .child_binding_value(children, var)
                    .or_else(|| self.binding_value(state, var));
                match binding.and_then(|b| b.value.as_ref()) {
                    Some(name) => {
                        let ok = premise_ctx.lookup(name).is_some();
                        // Premise settings like `Γ[a:τ]` are local to this judgment and must not
                        // leak to subsequent premises unless the rule explicitly outputs context.
                        if ok && !setting_extends {
                            *ctx = premise_ctx;
                        } else {
                            *ctx = base_ctx;
                        }
                        ok
                    }
                    None => false,
                }
            }
            TypingJudgment::Ascription((term, ty)) => {
                // For ascriptions, we want the type of the *directly-bound term*.
                // Using `child.inferred` is too error-prone when bindings from nested
                // subtrees share names (e.g. `left`, `right`) and leak into siblings.
                //
                // Prefer the binding record at the child's own `path` (created by `descend`
                // for this symbol), which carries a stable `ty` once the symbol is parsed.
                let mut actual_id: Option<TypeId> = None;
                for child in children {
                    let Some(p) = child.path else { continue };
                    if let Some(b) = child
                        .bindings
                        .iter()
                        .find(|b| b.name == *term && b.path == p && b.ty.is_some())
                    {
                        actual_id = b.ty;
                        break;
                    }
                }
                let actual_id = if let Some(id) = actual_id {
                    id
                } else if let Some(binding) = self
                    .child_binding_value(children, term)
                    .or_else(|| self.binding_value(state, term))
                {
                    let Some(actual_id) = binding.ty else {
                        debug_trace!(
                            "fusion_typing",
                            "premise_fail reason=no_binding_ty term={}",
                            term
                        );
                        return false;
                    };
                    actual_id
                } else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail reason=no_child_or_binding term={}",
                        term
                    );
                    return false;
                };
                let Some(actual) = self.type_of(actual_id) else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail reason=no_type term={} id={}",
                        term,
                        actual_id.0
                    );
                    return false;
                };
                let Some(expected) = self.resolve_type(ty, subst, state, children, &premise_ctx)
                else {
                    return false;
                };
                let ok = self.unify(&expected, &actual, subst);
                if !ok {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail reason=unify_failed term={} expected={} actual={}",
                        term,
                        expected,
                        actual
                    );
                }
                if ok {
                    *ctx = if setting_extends {
                        base_ctx
                    } else {
                        premise_ctx
                    };
                }
                ok
            }
            TypingJudgment::Operation { left, op, right } => {
                let (Some(l), Some(r)) = (
                    self.resolve_type(left, subst, state, children, &premise_ctx),
                    self.resolve_type(right, subst, state, children, &premise_ctx),
                ) else {
                    return false;
                };
                let ok = match op {
                    TypeOperation::Equality => self.unify(&l, &r, subst),
                    TypeOperation::Inclusion => equal(&l, &r).unwrap_or(false),
                };
                if ok {
                    *ctx = if setting_extends {
                        base_ctx
                    } else {
                        premise_ctx
                    };
                }
                ok
            }
            TypingJudgment::Check(term) => {
                // Check judgment: Γ ▷ term
                //
                // This is used for statement/block checking. When propagation is allowed,
                // we thread the child's resulting context forward (so statement lists can
                // extend Γ sequentially). When `[Γ]` is used, the parser sets
                // `no_propagate=true`; in that case we intentionally keep `premise_ctx`.
                if premise.setting.as_ref().is_some_and(|s| s.no_propagate) {
                    *ctx = premise_ctx;
                    return true;
                }

                let child = children.iter().find(|child| {
                    child.path.is_some_and(|p| {
                        child
                            .bindings
                            .iter()
                            .any(|b| b.name == *term && b.path == p)
                    })
                });
                let Some(child) = child else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail reason=no_child_for_check term={}",
                        term
                    );
                    return false;
                };
                let Some(next_ctx) = self.context(child.ctx) else {
                    return false;
                };
                *ctx = next_ctx;
                true
            }
        }
    }
}

impl TypingRuntime for RuleRuntime {
    fn enter_nonterminal(&self, _nt: NtId, summary: &TypingContextSummary) -> Vec<TypingState> {
        vec![TypingState {
            ctx: summary.ctx,
            expected: summary.expected,
            inferred: summary.expected,
            path: summary.path,
            bindings: Vec::new(),
        }]
    }

    fn prepare_child(
        &self,
        prod: ProdId,
        child_idx: usize,
        binding: Option<&str>,
        state: &TypingState,
        parsed_children: &[TypingState],
    ) -> TransitionResult<TypingState> {
        let rule = self
            .production_rule_name(prod)
            .and_then(|name| self.grammar.typing_rules.get(name));
        let Some(rule) = rule else {
            return Ok(state.clone());
        };

        let child_binding_names: Vec<_> = parsed_children
            .iter()
            .flat_map(|c| c.bindings.iter().map(|b| b.name.clone()))
            .collect();
        debug_trace!(
            "fusion_typing",
            "prepare rule={} child={} binding={:?} parsed_bindings={:?}",
            rule.name,
            child_idx,
            binding,
            child_binding_names
        );

        // If we're about to parse the ascribed term in a premise like:
        //   Γ[ ... ] ⊢ e : τ
        // then we must apply the setting's context extensions *before* parsing `e`,
        // otherwise the child parse can't rely on the extended context.
        for premise in &rule.premises {
            let Some(setting) = &premise.setting else {
                continue;
            };
            let Some(TypingJudgment::Ascription((term, _))) = &premise.judgment else {
                continue;
            };
            if binding != Some(term.as_str()) {
                continue;
            }

            let mut ctx = self.context(state.ctx).unwrap_or_default();
            let subst = HashMap::<String, Type>::new();

            for (name, ext_ty) in &setting.extensions {
                let Some(bound) = self
                    .child_binding_value(parsed_children, name)
                    .or_else(|| self.binding_value(state, name))
                else {
                    debug_trace!(
                        "fusion_typing",
                        "partial rule={} reason=no_binding name={}",
                        rule.name,
                        name
                    );
                    return Ok(state.clone());
                };
                let Some(value) = &bound.value else {
                    debug_trace!(
                        "fusion_typing",
                        "partial rule={} reason=no_value name={}",
                        rule.name,
                        name
                    );
                    return Ok(state.clone());
                };
                let Some(resolved) =
                    self.resolve_type(ext_ty, &subst, state, parsed_children, &ctx)
                else {
                    debug_trace!(
                        "fusion_typing",
                        "partial rule={} reason=unresolved name={}",
                        rule.name,
                        name
                    );
                    return Ok(state.clone());
                };
                let Some(next) = self.extend_context(&ctx, value, resolved) else {
                    debug_trace!(
                        "fusion_typing",
                        "partial rule={} reason=ctx_extend_failed name={}",
                        rule.name,
                        name
                    );
                    return Ok(state.clone());
                };
                ctx = next;
            }

            let mut next = state.clone();
            next.ctx = self.intern_context(ctx);
            return Ok(next);
        }

        Ok(state.clone())
    }

    fn descend(
        &self,
        state: &TypingState,
        path: PathId,
        binding: Option<&str>,
    ) -> TransitionResult<TypingState> {
        let mut next = state.clone();
        next.path = Some(path);
        if let Some(name) = binding {
            next.bindings.push(BindingValue {
                name: name.to_string(),
                path,
                value: None,
                ty: None,
            });
        }
        Ok(next)
    }

    fn consume_terminal(
        &self,
        state: &TypingState,
        _regex: &Regex,
        _segment: Option<&crate::logic::grammar::Segment>,
    ) -> TransitionResult<TypingState> {
        // Terminals should not "carry" the inferred type of the previous symbol.
        // If we keep `state.inferred` here, wrapper productions like '(' Expr ')'
        // can mistakenly see multiple meaningful inferred children and collapse to `Any`.
        let mut next = state.clone();
        next.inferred = None;
        Ok(next)
    }

    fn finish_production(
        &self,
        prod: ProdId,
        state: &TypingState,
        children: &[TypingState],
        status: NodeStatus,
    ) -> TransitionResult<TypingState> {
        let rule = self
            .production_rule_name(prod)
            .and_then(|name| self.grammar.typing_rules.get(name));
        let inherited = children.last().cloned().unwrap_or_else(|| state.clone());
        let mut ctx = if rule.is_some() {
            self.context(state.ctx).unwrap_or_default()
        } else {
            // Transparent wrappers should thread context forward from their child.
            self.context(inherited.ctx).unwrap_or_default()
        };
        let mut subst = HashMap::new();

        if let Some(rule) = rule {
            debug_trace!(
                "fusion_typing",
                "finish rule={} children={} status={:?}",
                rule.name,
                children.len(),
                status
            );
            for (i, child) in children.iter().enumerate() {
                let inferred = child
                    .inferred
                    .and_then(|id| self.type_of(id))
                    .map(|t| format!("{:?}", t))
                    .unwrap_or_else(|| "None".into());
                let names: Vec<_> = child.bindings.iter().map(|b| &b.name).collect();
                debug_trace!(
                    "fusion_typing",
                    "child[{}] inferred={} bindings={:?}",
                    i,
                    inferred,
                    names
                );
            }

            if !self.apply_rule(rule, state, children, &mut ctx, &mut subst) {
                debug_trace!(
                    "fusion_typing",
                    "reject rule={} reason=rule_failed status={:?}",
                    rule.name,
                    status
                );
                if matches!(status, NodeStatus::Partial) {
                    debug_trace!("fusion_typing", "partial_ok rule={}", rule.name);
                    let mut partial = inherited.clone();
                    // Apply any conclusion context transform even on partials when possible.
                    if let Some(output) = &rule.conclusion.context.output
                        && let Ok(next_ctx) =
                            self.apply_context_output(output, state, children, &ctx, &subst, status)
                    {
                        ctx = next_ctx;
                    }
                    partial.ctx = self.intern_context(ctx);
                    // Keep whatever type we've already inferred from children.
                    // For partial nodes, we avoid forcing the conclusion type (which may
                    // depend on missing subterms) and we also avoid expensive type resolution.
                    partial.inferred = children
                        .iter()
                        .rev()
                        .find_map(|child| child.inferred)
                        .or(inherited.inferred)
                        .or(state.inferred);
                    return Ok(partial);
                }
                return Err(TransitionError::Rejected);
            }

            // Rule succeeded: apply conclusion context transform (Γ → Γ[...]) if present.
            if let Some(output) = &rule.conclusion.context.output {
                ctx = self.apply_context_output(output, state, children, &ctx, &subst, status)?;
            }
        }

        let mut next = inherited;
        next.ctx = self.intern_context(ctx);
        next.inferred = rule
            .and_then(|rule| match &rule.conclusion.kind {
                ConclusionKind::Type(ty) => self.resolve_type(
                    ty,
                    &subst,
                    state,
                    children,
                    &self.context(next.ctx).unwrap_or_default(),
                ),
                ConclusionKind::ContextLookup(_, var) => {
                    let name = self
                        .child_binding_value(children, var)
                        .or_else(|| self.binding_value(state, var))?
                        .value
                        .as_ref()?;
                    self.context(next.ctx)?.lookup(name).cloned()
                }
            })
            .map(|ty| self.intern_type(ty))
            .or_else(|| self.inferred_type(prod, state, children));
        if next.inferred.is_none() {
            debug_trace!("fusion_typing", "reject prod={} reason=no_inferred", prod.0);
            return Err(TransitionError::Rejected);
        }
        debug_trace!(
            "fusion_typing",
            "ok prod={} inferred={:?}",
            prod.0,
            next.inferred.and_then(|id| self.type_of(id))
        );
        Ok(next)
    }
}

impl RuleRuntime {
    fn apply_rule(
        &self,
        rule: &TypingRule,
        state: &TypingState,
        children: &[TypingState],
        ctx: &mut Context,
        subst: &mut HashMap<String, Type>,
    ) -> bool {
        rule.premises
            .iter()
            .all(|premise| self.apply_premise(premise, state, children, ctx, subst))
    }

    fn apply_context_output(
        &self,
        output: &crate::logic::typing::rule::TypeSetting,
        state: &TypingState,
        children: &[TypingState],
        ctx: &Context,
        subst: &HashMap<String, Type>,
        status: NodeStatus,
    ) -> TransitionResult<Context> {
        let mut out = ctx.clone();
        for (name, ext_ty) in &output.extensions {
            let Some(binding) = self
                .child_binding_value(children, name)
                .or_else(|| self.binding_value(state, name))
            else {
                return if matches!(status, NodeStatus::Partial) {
                    Ok(out)
                } else {
                    Err(TransitionError::Rejected)
                };
            };
            let Some(value) = &binding.value else {
                return if matches!(status, NodeStatus::Partial) {
                    Ok(out)
                } else {
                    Err(TransitionError::Rejected)
                };
            };
            let Some(resolved) = self.resolve_type(ext_ty, subst, state, children, &out) else {
                return if matches!(status, NodeStatus::Partial) {
                    Ok(out)
                } else {
                    Err(TransitionError::Rejected)
                };
            };
            out = self.extend_context(&out, value, resolved).unwrap_or(out);
        }
        Ok(out)
    }

    fn extend_context(&self, ctx: &Context, value: &str, resolved: Type) -> Option<Context> {
        ctx.extend(value.to_string(), resolved.clone())
            .ok()
            .or_else(|| Some(ctx.shadow(value.to_string(), resolved)))
    }
}
