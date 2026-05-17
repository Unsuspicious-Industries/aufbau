use std::cell::RefCell;
use std::collections::HashMap;
use std::collections::HashSet;
use std::rc::Rc;

use crate::debug_trace;
use crate::logic::Segment;
use crate::logic::error::TransitionError;
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{ANY_TYPE, CtxId, EffectId, Lexeme, NodeStatus, ProdId, TypeId};
use crate::logic::semantic::{SemanticRuntime, SemanticSummary};
use crate::logic::typing::rule::{
    ConclusionKind, Premise, PremiseStatus, RuleResult, TypeOperation, TypingJudgment,
};
use crate::logic::typing::{Context, Type, TypingRule, subtype};
use crate::logic::typing::{ContextTransition, Obligation, Obligations, Unifier, UnifyResult};

#[derive(Clone, Debug)]
pub struct RuleRuntime {
    grammar: Grammar,
    types: Rc<RefCell<Vec<Type>>>,
    type_ids: Rc<RefCell<HashMap<Type, TypeId>>>,
    contexts: Rc<RefCell<Vec<Context>>>,
    context_ids: Rc<RefCell<HashMap<Context, CtxId>>>,
    effects: Rc<RefCell<Vec<ContextTransition>>>,
    effect_ids: Rc<RefCell<HashMap<ContextTransition, EffectId>>>,
    s: Vec<Segment>,
}

impl RuleRuntime {
    fn binding_values(
        &self,
        obligations: &Obligations,
    ) -> std::collections::HashMap<String, String> {
        obligations
            .iter()
            .filter_map(|ob| {
                let value = ob.value?;
                if value.complete && !value.open {
                    value.value(&self.s).map(|text| (ob.name.clone(), text))
                } else {
                    None
                }
            })
            .collect()
    }

    pub fn new(grammar: Grammar) -> Self {
        let runtime = Self {
            grammar,
            types: Rc::new(RefCell::new(Vec::new())),
            type_ids: Rc::new(RefCell::new(HashMap::new())),
            contexts: Rc::new(RefCell::new(Vec::new())),
            context_ids: Rc::new(RefCell::new(HashMap::new())),
            effects: Rc::new(RefCell::new(Vec::new())),
            effect_ids: Rc::new(RefCell::new(HashMap::new())),
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

    pub fn intern_effect(&self, effect: ContextTransition) -> Option<EffectId> {
        if effect.transforms.is_empty() {
            return None;
        }
        if let Some(&id) = self.effect_ids.borrow().get(&effect) {
            return Some(id);
        }
        let mut effects = self.effects.borrow_mut();
        let id = effects.len();
        effects.push(effect.clone());
        drop(effects);
        self.effect_ids.borrow_mut().insert(effect, id);
        Some(id)
    }

    pub fn effect(&self, id: EffectId) -> Option<ContextTransition> {
        self.effects.borrow().get(id).cloned()
    }

    pub fn context(&self, id: Option<CtxId>) -> Option<Context> {
        id.and_then(|id| {
            self.contexts.borrow().get(id).cloned().inspect(|c| {
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

    pub fn interned_effect_count(&self) -> usize {
        self.effects.borrow().len()
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
        obligations
            .iter()
            .find(|o| o.name == name)
            .and_then(|o| o.evidence)
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

    fn resolve_object_field_name(&self, name: &str, obligations: &Obligations) -> String {
        Self::ob_resolve(obligations, name)
            .and_then(|lexeme| lexeme.value(&self.s))
            .filter(|value| !value.is_empty())
            .unwrap_or_else(|| name.to_string())
    }

    fn normalize_object_extend(name: String, field_ty: Type, rest: Type) -> Type {
        if let Type::Object(mut fields) = rest {
            let mut all = vec![(name, field_ty)];
            all.append(&mut fields);
            Type::Object(all)
        } else {
            Type::ObjectExtend(name, Box::new(field_ty), Box::new(rest))
        }
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
            // Invariant: metas are local placeholders, not implicit references
            // to obligation names. Obligation-projected types must be modeled
            // explicitly, not smuggled through Meta lookup.
            Type::Meta(name) => unifier
                .resolve_meta(name)
                .cloned()
                .or(Some(Type::Meta(name.clone()))),
            Type::Arrow(left, right) => Some(Type::Arrow(
                Box::new(self.resolve_type(left, unifier, obligations, ctx)?),
                Box::new(self.resolve_type(right, unifier, obligations, ctx)?),
            )),
            Type::Array(inner) => Some(Type::Array(Box::new(self.resolve_type(
                inner,
                unifier,
                obligations,
                ctx,
            )?))),
            Type::Object(fields) => Some(Type::Object(
                fields
                    .iter()
                    .map(|(name, ty)| {
                        Some((
                            self.resolve_object_field_name(name, obligations),
                            self.resolve_type(ty, unifier, obligations, ctx)?,
                        ))
                    })
                    .collect::<Option<Vec<_>>>()?,
            )),
            Type::ObjectExtend(name, field_ty, rest) => {
                let name = self.resolve_object_field_name(name, obligations);
                let field_ty = self.resolve_type(field_ty, unifier, obligations, ctx)?;
                let rest = self.resolve_type(rest, unifier, obligations, ctx)?;
                Some(Self::normalize_object_extend(name, field_ty, rest))
            }
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

    // Property: each meta name is collected at most once.
    fn collect_metas(ty: &Type, out: &mut HashSet<String>) {
        match ty {
            Type::Meta(name) => {
                out.insert(name.clone());
            }
            Type::Arrow(left, right) => {
                Self::collect_metas(left, out);
                Self::collect_metas(right, out);
            }
            Type::Array(inner) | Type::Not(inner) => Self::collect_metas(inner, out),
            Type::Object(fields) => {
                for (_, ty) in fields {
                    Self::collect_metas(ty, out);
                }
            }
            Type::ObjectExtend(_, field_ty, rest) => {
                Self::collect_metas(field_ty, out);
                Self::collect_metas(rest, out);
            }
            Type::Union(items) => {
                for item in items {
                    Self::collect_metas(item, out);
                }
            }
            Type::Partial(inner, _) => Self::collect_metas(inner, out),
            _ => {}
        }
    }

    // Invariant: metas in `ty` are seeded from exact obligation witnesses
    // before `ty` is used in a semantic operation.
    fn seed_type_metas(&self, ty: &Type, unifier: &mut Unifier, obligations: &Obligations) {
        let mut metas = HashSet::new();
        Self::collect_metas(ty, &mut metas);
        unifier.seed(metas, |name| self.ob_type_resolved(obligations, name));
    }

    fn lookup_context<'a>(&self, ctx: &'a Context, lexeme: &Lexeme) -> Option<&'a Type> {
        let text = lexeme.value(&self.s)?;
        if text.is_empty() {
            #[cfg(test)]
            {
                debug_trace!("fusion_typing", "ctx_lookup empty lexeme");
            }
            return None;
        }
        #[cfg(test)]
        {
            debug_trace!(
                "fusion_typing",
                "ctx_lookup ctx={:?} gstring={:?}, lexeme='{:?}'",
                ctx,
                self.s,
                lexeme
            );
        }
        // Exact lookup: the lexeme text is a known binding in the context.
        if let Some(found) = ctx.lookup(&text) {
            #[cfg(test)]
            {
                debug_trace!(
                    "fusion_typing",
                    "ctx_lookup exact lexeme='{}' found={:?}",
                    text,
                    found
                );
            }
            return Some(found);
        }
        if lexeme.open
            && let Some(found) = ctx.lookup_starts_with(&text)
        {
            #[cfg(test)]
            {
                debug_trace!(
                    "fusion_typing",
                    "ctx_lookup prefix lexeme='{}' found={:?}",
                    text,
                    found
                );
            }
            return Some(found);
        }
        #[cfg(test)]
        {
            debug_trace!("fusion_typing", "ctx_lookup fail lexeme='{}'", text);
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
        allow_missing_obligation: bool,
    ) -> PremiseStatus {
        let base_ctx = ctx.clone();
        let mut premise_ctx = ctx.clone();
        let setting_extends = premise
            .setting
            .as_ref()
            .is_some_and(|s| !s.extensions.is_empty());

        if let Some(setting) = &premise.setting {
            for (name, ext_ty) in &setting.extensions {
                let Some(value) =
                    Self::ob_resolve(obligations, name).and_then(|lex| lex.value(&self.s))
                else {
                    debug_trace!("fusion_typing", "premise_partial no_binding name={}", name);
                    return PremiseStatus::Unknown;
                };
                let Some(resolved) = self.resolve_type(ext_ty, unifier, obligations, &premise_ctx)
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
                    #[cfg(test)]
                    {
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
                    #[cfg(test)]
                    {
                        debug_trace!("fusion_typing", "premise_partial no_lexeme var={}", var);
                    }
                    return PremiseStatus::Unknown;
                };

                let text = lexeme.value(&self.s).unwrap_or_default();
                let exact = !text.is_empty() && premise_ctx.lookup(&text).is_some();
                let prefix = lexeme.open
                    && !text.is_empty()
                    && premise_ctx.lookup_starts_with(&text).is_some();
                let found = exact || prefix;
                if found && !setting_extends {
                    #[cfg(test)]
                    {
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

                match (found, lexeme.open, text.is_empty()) {
                    (true, _, _) => PremiseStatus::Satisfied,
                    (false, _, true) => PremiseStatus::Unknown,
                    (false, true, false) => PremiseStatus::Contradiction,
                    (false, false, false) => PremiseStatus::Contradiction,
                }
            }
            TypingJudgment::Ascription((term, ty)) => {
                // Property: expected types are instantiated from exact witnesses
                // before any ascription check. Unresolved metas are not treated
                // as semantic evidence during unification.
                // getting an ptional type id from the obligation
                let ob = match self.ob_get(obligations, term) {
                    Some(ob) => ob,
                    None => {
                        if allow_missing_obligation {
                            debug_trace!(
                                "fusion_typing",
                                "premise_partial no_obligation term={}",
                                term
                            );
                            return PremiseStatus::Unknown;
                        }
                        debug_trace!("fusion_typing", "premise_fail no_obligation term={}", term);
                        return PremiseStatus::Contradiction;
                    }
                };
                // if no lexeme matched, unknown
                if let None = ob.value {
                    debug_trace!("fusion_typing", "premise_partial no_value term={}", term);
                    return PremiseStatus::Unknown;
                }
                // is the value attached o the obligaiton still open
                let open: bool = match ob.value {
                    Some(ref v) => v.open,
                    _ => false,
                };
                let Some(actual_id) = ob.evidence else {
                    debug_trace!("fusion_typing", "premise_fail no_actual term={}", term);
                    // An open child can still become informative after more input.
                    if open {
                        return PremiseStatus::Unknown;
                    } else {
                        return PremiseStatus::Contradiction;
                    }
                };
                let Some(actual) = self.type_of(actual_id) else {
                    return PremiseStatus::Contradiction;
                };
                self.seed_type_metas(ty, unifier, obligations);
                let Some(expected) = self.resolve_type(ty, unifier, obligations, &premise_ctx)
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
                        if subtype(&actual, &expected) {
                            debug_trace!(
                                "fusion_typing",
                                "premise_ok subtype term={} expected={:?} actual={:?}",
                                term,
                                expected,
                                actual
                            );
                            *ctx = if setting_extends {
                                base_ctx
                            } else {
                                premise_ctx
                            };
                            return PremiseStatus::Satisfied;
                        }
                        if open {
                            debug_trace!(
                                "fusion_typing",
                                "premise_open_mismatch term={} expected={:?} actual={:?} reason={:?}",
                                term,
                                expected,
                                actual,
                                reason
                            );
                            return PremiseStatus::Unknown;
                        }
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
            TypingJudgment::Operation { left, op, right } => {
                let (Some(l), Some(r)) = (
                    self.resolve_type(left, unifier, obligations, &premise_ctx),
                    self.resolve_type(right, unifier, obligations, &premise_ctx),
                ) else {
                    return PremiseStatus::Unknown;
                };

                let status = match op {
                    TypeOperation::Equality => match unifier.unify(&l, &r) {
                        UnifyResult::Ok => PremiseStatus::Satisfied,
                        UnifyResult::Indeterminate => PremiseStatus::Unknown,
                        UnifyResult::Fail(_) => PremiseStatus::Contradiction,
                    },
                    TypeOperation::Inclusion => {
                        if subtype(&l, &r) {
                            PremiseStatus::Satisfied
                        } else if matches!(l, Type::Any) || matches!(r, Type::Any) {
                            PremiseStatus::Unknown
                        } else {
                            PremiseStatus::Contradiction
                        }
                    }
                };

                match status {
                    PremiseStatus::Satisfied => {
                        // extending downwards context
                        *ctx = if setting_extends {
                            base_ctx
                        } else {
                            premise_ctx
                        };
                        PremiseStatus::Satisfied
                    }
                    PremiseStatus::Unknown => PremiseStatus::Unknown,
                    PremiseStatus::Contradiction => PremiseStatus::Contradiction,
                }
            }
        }
    }

    fn apply_rule(
        &self,
        rule: &TypingRule,
        obligations: &Obligations,
        ctx: Context,
        status: NodeStatus,
    ) -> RuleResult {
        let mut unifier = Unifier::new();
        unifier.set_binding_values(self.binding_values(obligations));
        let mut mctx = ctx.clone();
        let allow_missing_obligation = status.open();
        // Short-circuit on Contradiction; track if any premise was Unknown
        let all_satisfied = match rule
            .premises
            .iter()
            .try_fold(true, |all_satisfied, premise| {
                match self.apply_premise(
                    premise,
                    obligations,
                    &mut mctx,
                    &mut unifier,
                    allow_missing_obligation,
                ) {
                    PremiseStatus::Contradiction => Err(()),
                    PremiseStatus::Unknown => Ok(false),
                    PremiseStatus::Satisfied => Ok(all_satisfied),
                }
            }) {
            Err(()) => {
                debug_trace!(
                    "fusion_typing",
                    "apply_rule contradiction rule={} obligations={:?} ctx={:?}",
                    rule.name,
                    obligations,
                    ctx
                );
                return RuleResult::Contradiction;
            }
            Ok(all_satisfied) => all_satisfied,
        };

        // Resolve the conclusion type
        let ty = match &rule.conclusion.kind {
            ConclusionKind::Type(ty) => {
                // Property: conclusion types are instantiated from exact
                // witnesses before they are resolved or exported.
                self.seed_type_metas(ty, &mut unifier, obligations);
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
                if !all_satisfied {
                    debug_trace!(
                        "fusion_typing",
                        "apply_rule partial unresolved conclusion rule={} obligations={:?} ctx={:?}",
                        rule.name,
                        obligations,
                        ctx
                    );
                    return RuleResult::Partial(Type::Any);
                }
                debug_trace!(
                    "fusion_typing",
                    "apply_rule unresolved conclusion rule={} obligations={:?} ctx={:?}",
                    rule.name,
                    obligations,
                    ctx
                );
                return RuleResult::Contradiction;
            }
        };

        // Premises had unknowns
        if !all_satisfied {
            return RuleResult::Partial(ty);
        }

        // All premises are satisfied. Resolve right-bound conclusion exports such
        // as `Γ → Γ[x:τ] ⊢ ?R`; the parser applies these only when the enclosing
        // node is closed and then resumed by a right sibling.
        let transition = ContextTransition {
            transforms: match &rule.conclusion.context.output {
                Some(output_ctx) => output_ctx
                    .extensions
                    .iter()
                    .filter_map(|(v, t)| {
                        let resolved_name = unifier
                            .binding_values
                            .get(v)
                            .cloned()
                            .unwrap_or_else(|| v.clone());
                        self.seed_type_metas(t, &mut unifier, obligations);
                        self.resolve_type(t, &mut unifier, obligations, &mctx)
                            .map(|resolved| (resolved_name, resolved))
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

// ── SemanticRuntime implementation ───────────────────────────────────────────

impl SemanticRuntime for RuleRuntime {
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

        // Child-bound contexts are selected by the grammar binding on the child
        // being entered. Without a bound child, there is no local refinement.
        let Some(binding) = binding else {
            return Ok(ctx);
        };

        #[cfg(test)]
        {
            debug_trace!(
                "fusion_typing",
                "descend rule={} binding={} ctx={} obs={}",
                rule.name,
                binding,
                ctx,
                obligations.len()
            );
            for ob in obligations {
                debug_trace!(
                    "fusion_typing",
                    "  desc_ob name={} value={:?} actual={:?}",
                    ob.name,
                    ob.value,
                    ob.evidence.and_then(|id| self.type_of(id))
                );
            }
        }

        // Apply premise-local context extensions only to the addressed child,
        // e.g. `Γ[x:τ] ⊢ body : ?R` when `binding == "body"`.
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
                let Some(value) =
                    Self::ob_resolve(obligations, name).and_then(|lex| lex.value(&self.s))
                else {
                    debug_trace!(
                        "fusion_typing",
                        "descend partial: no value for {} in rule {}",
                        name,
                        rule.name,
                    );
                    return Ok(ctx);
                };
                self.seed_type_metas(ext_ty, &mut unifier, obligations);
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
    ) -> Result<SemanticSummary, TransitionError> {
        let rule = match self
            .production_rule_name(prod)
            .and_then(|name| self.grammar.rules().get(name.as_str()))
        {
            Some(r) => r,
            None => {
                // no rule, just return the same context and any type
                return Ok(SemanticSummary::new(ANY_TYPE, None, true));
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
                    ob.evidence.and_then(|id| self.type_of(id))
                );
            }
        }
        let context = self
            .context(Some(ctx))
            .ok_or_else(|| TransitionError::Rejected)?;

        match self.apply_rule(rule, obligations, context, status) {
            RuleResult::Success((ty, transition)) => {
                if status == NodeStatus::Closed {
                    debug_trace!(
                        "fusion_typing",
                        "finalize ok rule={} type={:?} transition={:?}",
                        rule.name,
                        ty,
                        transition
                    );
                    Ok(SemanticSummary::new(
                        self.intern_type(ty),
                        transition.and_then(|transition| self.intern_effect(transition)),
                        true,
                    ))
                } else {
                    debug_trace!(
                        "fusion_typing",
                        "finalize pending rule={} type={:?} transition={:?}",
                        rule.name,
                        ty,
                        transition
                    );
                    // context transitions are not allowed coming from partial nodes
                    Ok(SemanticSummary::new(self.intern_type(ty), None, true))
                }
            }
            RuleResult::Partial(ty) => {
                debug_trace!(
                    "fusion_typing",
                    "finalize partial rule={} type={:?}",
                    rule.name,
                    ty
                );
                if rule.name.starts_with("__br_") {
                    Ok(SemanticSummary::new(self.intern_type(ty), None, true))
                } else if status.open() {
                    Ok(SemanticSummary::new(self.intern_type(ty), None, false))
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

    fn apply_effect(&self, ctx: CtxId, effect: EffectId) -> Result<CtxId, TransitionError> {
        let Some(transform) = self.effect(effect) else {
            return Err(TransitionError::Rejected);
        };
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

    fn compose_effects(&self, effects: Vec<EffectId>) -> Result<Option<EffectId>, TransitionError> {
        let mut composed = ContextTransition::identity();
        for effect in effects {
            let Some(transition) = self.effect(effect) else {
                return Err(TransitionError::Rejected);
            };
            composed = composed.compose(&transition);
        }
        Ok(self.intern_effect(composed))
    }

    fn set_segs(&mut self, input: &[Segment]) {
        self.s = input.to_vec();
    }
}
