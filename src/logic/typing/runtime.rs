use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use crate::debug_trace;
use crate::logic::Segment;
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{CtxId, Lexeme, NodeStatus, ProdId, TypeId, ANY_TYPE};
use crate::logic::error::{TransitionError, TransitionResult};
use crate::logic::typing::{Obligations, TypingRuntime};
use crate::logic::typing::rule::{ConclusionKind, Premise, TypeOperation, TypingJudgment};
use crate::logic::typing::{Context, Type, TypingRule, equal};

#[derive(Clone, Debug)]
pub struct RuleRuntime {
    grammar: Grammar,
    types: Rc<RefCell<Vec<Type>>>,
    type_ids: Rc<RefCell<HashMap<Type, TypeId>>>,
    contexts: Rc<RefCell<Vec<Context>>>,
    context_ids: Rc<RefCell<HashMap<Context, CtxId>>>,
    s: Vec<Segment>,
}

impl RuleRuntime {
    pub fn new(grammar: Grammar) -> Self {
        let runtime = Self {
            grammar,
            types: Rc::new(RefCell::new(Vec::new())),
            type_ids: Rc::new(RefCell::new(HashMap::new())),
            contexts: Rc::new(RefCell::new(Vec::new())),
            context_ids: Rc::new(RefCell::new(HashMap::new())),
            s: Vec::new(),
        };
        let any_id = runtime.intern_type(Type::Any);
        debug_assert_eq!(any_id, ANY_TYPE);
        runtime.intern_context(Context::new());
        runtime
    }


    pub fn segs(&self) -> &[Segment] {
        &self.s
    }

    pub fn intern_type(&self, ty: Type) -> TypeId {
        if let Some(id) = self.type_ids.borrow().get(&ty) {
            return *id;
        }
        let mut types = self.types.borrow_mut();
        let id = types.len();
        types.push(ty.clone());
        self.type_ids.borrow_mut().insert(ty, id);
        id
    }

    pub fn type_of(&self, id: TypeId) -> Option<Type> {
        self.types.borrow().get(id).cloned()
    }

    pub fn intern_context(&self, ctx: Context) -> CtxId {
        if let Some(&id) = self.context_ids.borrow().get(&ctx) {
            return id;
        }
        let mut contexts = self.contexts.borrow_mut();
        let id = contexts.len();
        contexts.push(ctx.clone());
        drop(contexts);
        self.context_ids.borrow_mut().insert(ctx, id);
        id
    }

    pub fn context(&self, id: Option<CtxId>) -> Option<Context> {
        id.and_then(|id| {
            self.contexts
                .borrow()
                .get(id)
                .cloned()
                .inspect(|c| {
                    debug_trace!("fusion_typing", "ctx[{}] = {:?}", id, c);
                })
        })
    }

    pub fn interned_type_count(&self) -> usize {
        self.types.borrow().len()
    }

    pub fn interned_context_count(&self) -> usize {
        self.contexts.borrow().len()
    }

    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    fn production_rule_name(&self, prod: ProdId) -> Option<String> {
        self.grammar.prod(prod).and_then(|p| p.rule)
    }

    // ── Obligation-based helpers ─────────────────────────────────────────────

    fn ob_lexeme<'a>(obligations: &'a Obligations, name: &str) -> Option<&'a Lexeme> {
        obligations
            .iter()
            .find(|o| o.name == name)
            .and_then(|o| o.value.as_ref())
    }

    fn ob_value(&self, obligations: &Obligations, name: &str) -> Option<String> {
        Self::ob_lexeme(obligations, name).and_then(|lexeme| lexeme.value(&self.s))
    }

    fn ob_type(&self, obligations: &Obligations, name: &str) -> Option<TypeId> {
        obligations.iter().find(|o| o.name == name).and_then(|o| o.actual)
    }

    fn ob_type_resolved(&self, obligations: &Obligations, name: &str) -> Option<Type> {
        if let Some(id) = self.ob_type(obligations, name) {
            if id != ANY_TYPE {
                return self.type_of(id);
            }
        }
        self.ob_value(obligations, name)
            .and_then(|v| Type::parse_raw(&v).ok())
    }

    // ── Type resolution ──────────────────────────────────────────────────────

    fn resolve_type(
        &self,
        ty: &Type,
        subst: &HashMap<String, Type>,
        obligations: &Obligations,
        ctx: &Context,
        typed_partial: &mut bool,
    ) -> Option<Type> {
        match ty {
            Type::Meta(name) => subst
                .get(name)
                .cloned()
                .or_else(|| self.ob_type_resolved(obligations, name))
                .or(Some(Type::Meta(name.clone()))),
            Type::Arrow(left, right) => Some(Type::Arrow(
                Box::new(self.resolve_type(left, subst, obligations, ctx, typed_partial)?),
                Box::new(self.resolve_type(right, subst, obligations, ctx, typed_partial)?),
            )),
            Type::Array(inner) => Some(Type::Array(Box::new(
                self.resolve_type(inner, subst, obligations, ctx, typed_partial)?,
            ))),
            Type::Union(items) => Some(Type::Union(
                items
                    .iter()
                    .map(|item| self.resolve_type(item, subst, obligations, ctx, typed_partial))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Type::ContextCall(_, var) => {
                let lexeme = Self::ob_lexeme(obligations, var)?;
                self.lookup_context(ctx, lexeme, typed_partial).cloned()
            }
            _ => Some(ty.clone()),
        }
    }

    fn lookup_context<'a>(
        &self,
        ctx: &'a Context,
        lexeme: &Lexeme,
        typed_partial: &mut bool,
    ) -> Option<&'a Type> {

        #[cfg(test)] {
            debug_trace!(
                "fusion_typing",
                "ctx_lookup ctx={:?} gstring={:?}, lexeme='{:?}'",
                ctx,
                self.s,
                lexeme
            );
        }

        // Exact lookup: the lexeme text is a known binding in the context.
        if let Some(found) = ctx.lookup(&lexeme.value(&self.s)?) {
            #[cfg(test)] {
                debug_trace!(
                    "fusion_typing",
                    "ctx_lookup exact lexeme='{}' found={:?}",
                    lexeme.value(&self.s)?,
                    found
                );
            }
            return Some(found);
        }
        // Prefix lookup: the lexeme is open (can still grow at end-of-input)
        // and its current text is a prefix of some context binding.
        // This is accepted only as a partial fact — typing must not mark
        // the node as fully complete.
        if lexeme.open && let Some(found) = ctx.lookup_starts_with(&lexeme.value(&self.s)?) {
            #[cfg(test)] {
                debug_trace!(
                    "fusion_typing",
                    "ctx_lookup prefix lexeme='{}' found={:?}",
                    lexeme.value(&self.s)?,
                    found
                );
            }
            *typed_partial = true;
            return Some(found);
        }
        #[cfg(test)] {
            debug_trace!(
                "fusion_typing",
                "ctx_lookup fail lexeme='{}'",
                lexeme.value(&self.s)?
            );
        }
        None
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
            Type::Union(parts) => parts.iter().any(|p| self.unify(p, actual, subst)),
            Type::Arrow(a, b) => match actual {
                Type::Arrow(x, y) => self.unify(a, x, subst) && self.unify(b, y, subst),
                _ => false,
            },
            _ => equal(expected, actual).unwrap_or(false),
        }
    }

    // ── Premise checking ─────────────────────────────────────────────────────

    fn apply_premise(
        &self,
        premise: &Premise,
        obligations: &Obligations,
        ctx: &mut Context,
        subst: &mut HashMap<String, Type>,
        typed_partial: &mut bool,
    ) -> bool {
        let base_ctx = ctx.clone();
        let mut premise_ctx = ctx.clone();
        let setting_extends = premise
            .setting
            .as_ref()
            .is_some_and(|s| !s.extensions.is_empty());

        if let Some(setting) = &premise.setting {
            for (name, ext_ty) in &setting.extensions {
                let Some(value) = self.ob_value(obligations, name) else {
                    debug_trace!("fusion_typing", "premise_fail no_binding name={}", name);
                    return false;
                };
                let Some(resolved) =
                    self.resolve_type(ext_ty, subst, obligations, &premise_ctx, typed_partial)
                else {
                    debug_trace!("fusion_typing", "premise_fail unresolved name={}", name);
                    return false;
                };
                if premise_ctx
                    .extend(value.to_string(), resolved)
                    .map(|next| premise_ctx = next)
                    .is_err()
                {
                    #[cfg(test)] {
                        debug_trace!(
                            "fusion_typing",
                            "premise_fail extend name={} value={} ext_ty={:?}",
                            name,
                            value,
                            ext_ty
                        );
                    }
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
                let Some(lexeme) = Self::ob_lexeme(obligations, var) else {
                    #[cfg(test)] {
                        debug_trace!(
                            "fusion_typing",
                            "premise_fail no_lexeme var={}",
                            var
                        );
                    }
                    return false;
                };
                let ok = self.lookup_context(&premise_ctx, lexeme, typed_partial).is_some();
                if ok && !setting_extends {
                    #[cfg(test)] {
                        debug_trace!(
                            "fusion_typing",
                            "premise_ok membership var={} value={:?}",
                            var,
                            lexeme.value(&self.s)
                        );
                    }
                    *ctx = premise_ctx;
                } else {
                    *ctx = base_ctx;
                }
                #[cfg(test)] {
                    debug_trace!(
                        "fusion_typing",
                        "premise_membership var={} value={:?} ok={}",
                        var,
                        lexeme.value(&self.s),
                        ok
                    );
                }
                ok
            }
            TypingJudgment::Ascription((term, ty)) => {
                let actual_id = self.ob_type(obligations, term);
                let Some(actual_id) = actual_id else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail no_actual term={}",
                        term
                    );
                    return false;
                };
                let Some(actual) = self.type_of(actual_id) else {
                    return false;
                };
                let Some(expected) =
                    self.resolve_type(ty, subst, obligations, &premise_ctx, typed_partial)
                else {
                    return false;
                };
                let ok = self.unify(&expected, &actual, subst);
                if !ok {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail unify term={} expected={} actual={}",
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
                    self.resolve_type(left, subst, obligations, &premise_ctx, typed_partial),
                    self.resolve_type(right, subst, obligations, &premise_ctx, typed_partial),
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
                if premise.setting.as_ref().is_some_and(|s| s.no_propagate) {
                    *ctx = premise_ctx;
                    return true;
                }
                // For check premises, we need the child's output context.
                // With obligations, the child's ctx is not directly available
                // — accept the check and keep current context.
                // Full check-premise support requires ctx threading through obligations.
                *ctx = premise_ctx;
                self.ob_value(obligations, term).is_some()
            }
        }
    }

    fn apply_rule(
        &self,
        rule: &TypingRule,
        obligations: &Obligations,
        ctx: &mut Context,
        subst: &mut HashMap<String, Type>,
        typed_partial: &mut bool,
    ) -> bool {
        rule.premises
            .iter()
            .all(|premise| self.apply_premise(premise, obligations, ctx, subst, typed_partial))
    }

    fn inferred_type(
        &self,
        prod: ProdId,
        obligations: &Obligations,
    ) -> Option<TypeId> {
        let rule = self
            .production_rule_name(prod)
            .and_then(|name| self.grammar.rules().get(name.as_str()));
        match rule.map(|rule| &rule.conclusion.kind) {
            Some(ConclusionKind::Type(ty)) => Some(self.intern_type(ty.clone())),
            _ => {
                // Heuristic: if exactly one obligation has an actual type, propagate it.
                let meaningful: Vec<TypeId> = obligations
                    .iter()
                    .filter_map(|o| o.actual)
                    .filter(|id| *id != ANY_TYPE)
                    .collect();
                if meaningful.len() == 1 {
                    Some(meaningful[0])
                } else {
                    Some(ANY_TYPE)
                }
            }
        }
    }

    fn extend_context(&self, ctx: &Context, value: &str, resolved: Type) -> Option<Context> {
        ctx.extend(value.to_string(), resolved.clone())
            .ok()
            .or_else(|| Some(ctx.shadow(value.to_string(), resolved)))
    }

    fn apply_context_output(
        &self,
        output: &crate::logic::typing::rule::TypeSetting,
        obligations: &Obligations,
        ctx: &Context,
        subst: &HashMap<String, Type>,
        status: NodeStatus,
        typed_partial: &mut bool,
    ) -> TransitionResult<Context> {
        let mut out = ctx.clone();
        for (name, ext_ty) in &output.extensions {
            let Some(value) = self.ob_value(obligations, name) else {
                return if matches!(status, NodeStatus::Partial) {
                    Ok(out)
                } else {
                    Err(TransitionError::Rejected)
                };
            };
            let Some(resolved) =
                self.resolve_type(ext_ty, subst, obligations, &out, typed_partial)
            else {
                return if matches!(status, NodeStatus::Partial) {
                    Ok(out)
                } else {
                    Err(TransitionError::Rejected)
                };
            };
            out = self.extend_context(&out, &value, resolved).unwrap_or(out);
        }
        Ok(out)
    }
}

// ── TypingRuntime implementation ─────────────────────────────────────────────

impl TypingRuntime for RuleRuntime {
    fn descend(
        &self,
        prod: ProdId,
        _dot: usize,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &Obligations,
    ) -> Result<CtxId, TransitionError> {
        let rule_name = self.production_rule_name(prod);
        let rule = rule_name
            .as_ref()
            .and_then(|name| self.grammar.rules().get(name.as_str()));
        let Some(rule) = rule else {
            return Ok(ctx);
        };

        let Some(binding) = binding else {
            return Ok(ctx);
        };

        debug_trace!(
            "fusion_typing",
            "descend rule={} binding={} ctx={} obs={}",
            rule.name, binding, ctx, obligations.len()
        );
        for ob in obligations {
            debug_trace!(
                "fusion_typing",
                "  desc_ob name={} value={:?} actual={:?}",
                ob.name, ob.value, ob.actual.and_then(|id| self.type_of(id))
            );
        }

        for premise in &rule.premises {
            let Some(setting) = &premise.setting else {
                continue;
            };
            let Some(TypingJudgment::Ascription((term, _))) = &premise.judgment else {
                continue;
            };
            if binding != term.as_str() {
                continue;
            }

            let mut current_ctx = self.context(Some(ctx)).unwrap_or_default();
            let subst = HashMap::<String, Type>::new();
            let mut unused = false;

            for (name, ext_ty) in &setting.extensions {
                let Some(value) = self.ob_value(obligations, name) else {
                    debug_trace!(
                        "fusion_typing",
                        "descend partial: no value for {} in rule {}",
                        name,
                        rule.name,
                    );
                    return Ok(ctx);
                };
                let Some(resolved) =
                    self.resolve_type(ext_ty, &subst, obligations, &current_ctx, &mut unused)
                else {
                    return Ok(ctx);
                };
                let Some(next) = self.extend_context(&current_ctx, &value, resolved) else {
                    return Ok(ctx);
                };
                current_ctx = next;
            }

            return Ok(self.intern_context(current_ctx));
        }

        Ok(ctx)
    }

    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &Obligations,
        status: NodeStatus,
    ) -> Result<(TypeId, CtxId, bool), TransitionError> {
        let rule = self
            .production_rule_name(prod)
            .and_then(|name| self.grammar.rules().get(name.as_str()));

        let mut current_ctx = self.context(Some(ctx)).unwrap_or_default();
        let mut subst = HashMap::new();
        let mut typed_partial = matches!(status, NodeStatus::Partial);

        if let Some(rule) = rule {
            #[cfg(test)]
            {
                debug_trace!(
                    "fusion_typing",
                    "finalize rule={} obligations={} status={:?}",
                    rule.name,
                    obligations.len(),
                    status
                );
                for ob in obligations {
                    debug_trace!(
                        "fusion_typing",
                        "  ob name={} value={:?} open={:?} actual={:?}",
                        ob.name,
                        ob.value.as_ref().map(|v| v.value(&self.s)),
                        ob.value.as_ref().map(|v| v.open),
                        ob.actual.and_then(|id| self.type_of(id))
                    );
                }
            }

            if !self.apply_rule(
                rule,
                obligations,
                &mut current_ctx,
                &mut subst,
                &mut typed_partial,
            ) {
                debug_trace!(
                    "fusion_typing",
                    "reject rule={} status={:?}",
                    rule.name,
                    status
                );
                if matches!(status, NodeStatus::Partial) {
                    if let Some(output) = &rule.conclusion.context.output {
                        if let Ok(next) = self.apply_context_output(
                            output,
                            obligations,
                            &current_ctx,
                            &subst,
                            status,
                            &mut typed_partial,
                        ) {
                            current_ctx = next;
                        }
                    }
                    let ctx_out = self.intern_context(current_ctx);
                    let ty = self.inferred_type(prod, obligations).unwrap_or(ANY_TYPE);
                    return Ok((ty, ctx_out, false));
                }
                return Err(TransitionError::Rejected);
            }

            if let Some(output) = &rule.conclusion.context.output {
                current_ctx = self.apply_context_output(
                    output,
                    obligations,
                    &current_ctx,
                    &subst,
                    status,
                    &mut typed_partial,
                )?;
            }
        }

        let ctx_out = self.intern_context(current_ctx.clone());

        let inferred = rule
            .and_then(|rule| match &rule.conclusion.kind {
                ConclusionKind::Type(ty) => self.resolve_type(
                    ty,
                    &subst,
                    obligations,
                    &current_ctx,
                    &mut typed_partial,
                ),
                ConclusionKind::ContextLookup(_, var) => {
                    let lexeme = Self::ob_lexeme(obligations, var)?;
                    self.lookup_context(&self.context(Some(ctx_out))?, lexeme, &mut typed_partial)
                        .cloned()
                }
            })
            .map(|ty| self.intern_type(ty))
            .or_else(|| self.inferred_type(prod, obligations));

        let Some(ty) = inferred else {
            debug_trace!("fusion_typing", "reject prod={} no_inferred", prod.0);
            return Err(TransitionError::Rejected);
        };

        debug_trace!(
            "fusion_typing",
            "ok prod={} inferred={:?}",
            prod.0,
            self.type_of(ty)
        );

        Ok((ty, ctx_out, !typed_partial))
    }

    fn set_segs(&mut self, input: &[Segment]) {
        self.s = input.to_vec();
    }
}
