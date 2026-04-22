use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use rayon::iter;

use crate::debug_trace;
use crate::logic::Segment;
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{CtxId, Lexeme, NodeStatus, ProdId, TypeId, ANY_TYPE};
use crate::logic::error::{TransitionError, TransitionResult};
use crate::logic::typing::{ContextTransition, Obligation, Obligations, TypingRuntime, Unifier, UnifyResult};
use crate::logic::typing::rule::{ConclusionKind, Premise, PremiseStatus, RuleResult, TypeOperation, TypingJudgment};
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
        self.grammar.rule_for_prod(prod).cloned()
    }

    // ── Obligation-based helpers ─────────────────────────────────────────────

    // get string from the obligations
    fn ob_resolve<'a>(obligations: &'a Obligations, name: &str) -> Option<&'a Lexeme> {
        obligations
            .iter()
            .find(|o| o.name == name)
            .and_then(|o| o.value.as_ref())
    }

    fn ob_type(&self, obligations: &Obligations, name: &str) -> Option<TypeId> {
        obligations.iter().find(|o| o.name == name).and_then(|o| o.actual)
    }

    fn ob_get(&self, obligations: &Obligations, name: &str) -> Option<Obligation> {
        obligations.iter().find(|o| o.name == name).cloned()
    }

    fn ob_type_resolved(&self, obligations: &Obligations, name: &str) -> Option<Type> {
        if let Some(id) = self.ob_type(obligations, name) {
            if id != ANY_TYPE {
                return self.type_of(id);
            }
        }
        Self::ob_resolve(obligations, name)
            .and_then(|lex| lex.value(&self.s))
            .and_then(|v| Type::parse_raw(&v).ok())
    }

    // ── Type resolution ──────────────────────────────────────────────────────

    fn resolve_type(
        &self,
        ty: &Type,
        unifier: &mut Unifier,
        obligations: &Obligations,
        ctx: &Context,
    ) -> Option<Type> {
        match ty {
            Type::Meta(name) => unifier.resolve_meta(name)
                .cloned()
                .or_else(|| self.ob_type_resolved(obligations, name))
                .or(Some(Type::Meta(name.clone()))),
            Type::Arrow(left, right) => Some(Type::Arrow(
                Box::new(self.resolve_type(left, unifier, obligations, ctx)?),
                Box::new(self.resolve_type(right, unifier, obligations, ctx)?),
            )),
            Type::Array(inner) => Some(Type::Array(Box::new(
                self.resolve_type(inner, unifier, obligations, ctx)?,
            ))),
            Type::Union(items) => Some(Type::Union(
                items
                    .iter()
                    .map(|item| self.resolve_type(item, unifier, obligations, ctx))
                    .collect::<Option<Vec<_>>>()?,
            )),
            Type::ContextCall(_, var) => {
                let lexeme = Self::ob_resolve(obligations, var)?;
                self.lookup_context(ctx, lexeme).cloned()
            }
            _ => Some(ty.clone()),
        }
    }

    fn lookup_context<'a>(
        &self,
        ctx: &'a Context,
        lexeme: &Lexeme
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
        if lexeme.open && let Some(found) = ctx.lookup_starts_with(&lexeme.value(&self.s)?) {
            #[cfg(test)] {
                debug_trace!(
                    "fusion_typing",
                    "ctx_lookup prefix lexeme='{}' found={:?}",
                    lexeme.value(&self.s)?,
                    found
                );
            }
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


    // ── Premise checking ─────────────────────────────────────────────────────

    fn apply_premise(
        &self,
        premise: &Premise,
        obligations: &Obligations,
        ctx: &mut Context,
        unifier: &mut Unifier,
    ) -> PremiseStatus {
        let base_ctx = ctx.clone();
        let mut premise_ctx = ctx.clone();
        let setting_extends = premise
            .setting
            .as_ref()
            .is_some_and(|s| !s.extensions.is_empty());

        if let Some(setting) = &premise.setting {
            for (name, ext_ty) in &setting.extensions {
                let Some(value) = Self::ob_resolve(obligations, name).and_then(|lex| lex.value(&self.s)) else {
                    debug_trace!("fusion_typing", "premise_partial no_binding name={}", name);
                    return PremiseStatus::Unknown;
                };
                let Some(resolved) =
                    self.resolve_type(ext_ty, unifier, obligations, &premise_ctx)
                else {
                    debug_trace!("fusion_typing", "premise_fail unresolved name={}", name);
                    // Unsure about this one
                    return PremiseStatus::Unknown;
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
                    return PremiseStatus::Contradiction;
                }
            }
        }

        let Some(judgment) = &premise.judgment else {
            *ctx = premise_ctx;
            return PremiseStatus::Satisfied;
        };

        match judgment {
            TypingJudgment::Membership(var, _) => {
                let Some(lexeme) = Self::ob_resolve(obligations, var) else {
                    #[cfg(test)] {
                        debug_trace!(
                            "fusion_typing",
                            "premise_fail no_lexeme var={}",
                            var
                        );
                    }
                    return PremiseStatus::Contradiction;
                };
                let ok = self.lookup_context(&premise_ctx, lexeme).is_some();
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
                match ok {
                    true => PremiseStatus::Satisfied,
                    false => PremiseStatus::Contradiction,
                }
            }
            TypingJudgment::Ascription((term, ty)) => {
                // getting an ptional type id from the obligation
                let ob = match self.ob_get(obligations, term) {
                    Some(ob) => ob,
                    None => {
                        debug_trace!(
                            "fusion_typing",
                            "premise_fail no_obligation term={}",
                            term
                        );
                        // no obligaiton at al is a probleù
                        return PremiseStatus::Contradiction;
                    }
                };
                // if no lexeme matched, unknown
                if let None = ob.value {
                    debug_trace!(
                        "fusion_typing",
                        "premise_partial no_value term={}",
                        term
                    );
                    return PremiseStatus::Unknown;
                }
                // is the value attached o the obligaiton still open
                let open: bool = match ob.value {
                    Some(ref v)  => {
                        v.open
                    }
                    _ => false,
                };
                let Some(actual_id) = ob.actual else {
                    debug_trace!(
                        "fusion_typing",
                        "premise_fail no_actual term={}",
                        term
                    );
                    // weird case though
                    if open {
                        return PremiseStatus::Unknown;
                    } else {
                        return PremiseStatus::Contradiction;
                    }
                };
                let Some(actual) = self.type_of(actual_id) else {
                    return PremiseStatus::Contradiction;
                };
                let Some(expected) =
                    self.resolve_type(ty, unifier, obligations, &premise_ctx)
                else {
                    return PremiseStatus::Unknown;
                };
                let r = unifier.unify(&actual, &expected);
                match r {
                    UnifyResult::Ok => {
                        debug_trace!(
                            "fusion_typing",
                            "premise_ok unify term={} expected={:?} actual={:?}",
                            term,
                            expected,
                            actual
                        );
                        // extending downwards context
                        *ctx = if setting_extends {
                            base_ctx
                        } else {
                            premise_ctx
                        };
                        PremiseStatus::Satisfied    
                    }
                    UnifyResult::Indeterminate => {
                        debug_trace!(
                            "fusion_typing",
                            "premise_indeterminate unify term={} expected={:?} actual={:?}",
                            term,
                            expected,
                            actual
                        );
                        PremiseStatus::Unknown
                    }
                    UnifyResult::Fail(reason) => {
                        debug_trace!(
                            "fusion_typing",
                            "premise_fail unify term={} expected={:?} actual={:?} reason={:?}",
                            term,
                            expected,
                            actual,
                            reason
                        );
                        PremiseStatus::Contradiction
                    }
                }
            }
            // TODO: fix
            // this is unwell handled by who cares
            TypingJudgment::Operation { left, op, right } => {
                let (Some(l), Some(r)) = (
                    self.resolve_type(left, unifier, obligations, &premise_ctx),
                    self.resolve_type(right, unifier, obligations, &premise_ctx),
                ) else {
                    return PremiseStatus::Unknown;
                };
                let r = unifier.unify(&l, &r);
                match r {
                    UnifyResult::Ok => {
                        // extending downwards context
                        *ctx = if setting_extends {
                            base_ctx
                        } else {
                            premise_ctx
                        };
                        PremiseStatus::Satisfied    
                    }
                    UnifyResult::Indeterminate => {
                        PremiseStatus::Unknown
                    }
                    UnifyResult::Fail(reason) => {
                        PremiseStatus::Contradiction
                    }
                }
            }
        }
    }

   fn apply_rule(
        &self,
        rule: &TypingRule,
        obligations: &Obligations,
        ctx: Context,
    ) -> RuleResult {
        let mut unifier = Unifier::new();
        let mut mctx = ctx.clone();
        // Short-circuit on Contradiction; track if any premise was Unknown
        let all_satisfied = match rule.premises
            .iter()
            .try_fold(true, |all_satisfied, premise| {
                match self.apply_premise(premise, obligations, &mut mctx, &mut unifier) {
                    PremiseStatus::Contradiction => Err(()),
                    PremiseStatus::Unknown       => Ok(false),
                    PremiseStatus::Satisfied     => Ok(all_satisfied),
                }
            })
        {
            Err(()) => {
                debug_trace!(
                    "fusion_typing",
                    "apply_rule contradiction rule={} obligations={:?} ctx={:?}",
                    rule.name, obligations, ctx
                );
                return RuleResult::Contradiction;
            }
            Ok(all_satisfied) => all_satisfied,
        };

        // Resolve the conclusion type
        let ty = match &rule.conclusion.kind {
            ConclusionKind::Type(ty) => {
                self.resolve_type(ty, &mut unifier, obligations, &mctx)
            }
            ConclusionKind::ContextLookup(_, var) => {
                let lexeme = Self::ob_resolve(obligations, var);
                lexeme.and_then(|l| self.lookup_context(&mctx, l).cloned())
            }
        };

        let ty = match ty {
            Some(ty) => ty,
            None => {
                debug_trace!(
                    "fusion_typing",
                    "apply_rule unresolved conclusion rule={} obligations={:?} ctx={:?}",
                    rule.name, obligations, ctx
                );
                return RuleResult::Contradiction;
            }
        };

        // Premises had unknowns 
        if !all_satisfied {
            return RuleResult::Partial(ty);
        }

        // All premises satisfied — resolve context extensions and return success
        let transition = ContextTransition {
            transforms: match &rule.conclusion.context.output {
                Some(output_ctx) => output_ctx
                    .extensions
                    .iter()
                    .filter_map(|(v, t)| {
                        self.resolve_type(t, &mut unifier, obligations, &mctx)
                            .map(|resolved| (v.clone(), resolved))
                    })
                    .collect(),
                None => Vec::new(),
            },
        };

        RuleResult::Success((ty, Some(transition)))
    }
    fn extend_context(&self, ctx: &Context, value: &str, resolved: Type) -> Option<Context> {
        ctx.extend(value.to_string(), resolved.clone())
            .ok()
            .or_else(|| Some(ctx.shadow(value.to_string(), resolved)))
    }

}

// ── TypingRuntime implementation ─────────────────────────────────────────────

impl TypingRuntime for RuleRuntime {
    fn descend(
        &self,
        prod: ProdId,
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

        // if no binding, nothing to do i guesse
        let Some(binding) = binding else {
            return Ok(ctx);
        };

        #[cfg(test)] {
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
        }

        // evaluating typing rule
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
            let mut unifier = Unifier::new();
            for (name, ext_ty) in &setting.extensions {
                let Some(value) = Self::ob_resolve(obligations, name)
                    .and_then(|lex| lex.value(&self.s)) else {
                    debug_trace!(
                        "fusion_typing",
                        "descend partial: no value for {} in rule {}",
                        name,
                        rule.name,
                    );
                    return Ok(ctx);
                };
                let Some(resolved) =
                    self.resolve_type(ext_ty, &mut unifier, obligations, &current_ctx)
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
    ) -> Result<(TypeId, Option<ContextTransition>), TransitionError> {
        let rule = match self
            .production_rule_name(prod)
            .and_then(|name| self.grammar.rules().get(name.as_str())) {
                Some(r) => r,
                None => {
                    // no rule, just return the same context and any type
                    return Ok((ANY_TYPE, None));
                }
            };

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
        let context = self.context(Some(ctx)).ok_or_else(|| TransitionError::Rejected)?;

        match self.apply_rule(rule, obligations, context) {
            RuleResult::Success((ty, transition)) => {
                if status == NodeStatus::Closed {
                    debug_trace!(
                        "fusion_typing",
                        "finalize ok rule={} type={:?} transition={:?}",
                        rule.name,
                        ty,
                        transition
                    );
                    Ok((self.intern_type(ty), transition))
                } else {
                    debug_trace!(
                        "fusion_typing",
                        "finalize pending rule={} type={:?} transition={:?}",
                        rule.name,
                        ty,
                        transition
                    );
                    // context transitions are not allowed coming from partial nodes
                    Ok((self.intern_type(ty), None))
                }
            }
            RuleResult::Partial(ty) => {
                debug_trace!(
                    "fusion_typing",
                    "finalize partial rule={} type={:?}",
                    rule.name,
                    ty
                );
                if status == NodeStatus::Partial {
                    Ok((self.intern_type(ty), None))
                } else {
                    // complete but unknowns is very bad stuff
                    Err(TransitionError::Rejected)
                }
            }
            RuleResult::Contradiction => {
                debug_trace!(
                    "fusion_typing",
                    "finalize fail rule={} obligations={:?}",
                    rule.name,
                    obligations,
                );
                Err(TransitionError::Rejected)
            }
        }

    }

    fn apply_transform(&self, ctx: CtxId, transform: ContextTransition) -> Result<CtxId, TransitionError> {
        let mut current_ctx = self.context(Some(ctx)).unwrap_or_default();
        for (var, ty) in transform.transforms {
            if let Some(next) = self.extend_context(&current_ctx, &var, ty) {
                current_ctx = next;
            } else {
                // if we fail to extend, just return the original context
                return Ok(ctx);
            }
        }
        Ok(self.intern_context(current_ctx))
    }

    fn set_segs(&mut self, input: &[Segment]) {
        self.s = input.to_vec();
    }
}
