//! Typing constraint domain implementation — §3 of `draft/`.
//!
//! `TypingDomain` implements `ConstraintDomain` with:
//! - `Rule`     = `TypingRule`
//! - `Evidence` = `Type` (interned as `TypeId = EvidenceId`)
//! - `Context`  = `Context` (ordered `Identifier → Type` map via `BTreeMap`)
//! - `Effect`   = `ContextTransition` (ordered list of `(name, Type)` extensions)

use crate::domains::typing::rule::{
    Premise, PremiseStatus, RuleResult, TypeOperation, TypeSetting, TypingJudgment,
};
use crate::domains::typing::{
    subtype, Context, ContextTransition, Type, TypeExpr, TypingRule, Unifier, UnifyResult,
};
use crate::engine::parse::arena::{Lexeme, NodeStatus};
use crate::engine::Segment;
use crate::semantics::domain::Verdict;
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::Obligations;

// ── TypingDomain ──────────────────────────────────────────────────────────────

/// Pure, value-level typing evaluation: `descend`/`finalize`/`apply_effect`/
/// `compose_effects` over `Type`/`Context`/`ContextTransition`. The stateful
/// id-interning shell lives in `semantics::runtime::TypingRuntime`.
#[derive(Clone, Debug)]
pub struct TypingDomain;

impl Default for TypingDomain {
    fn default() -> Self {
        Self
    }
}

impl TypingDomain {

    // ── Obligation helpers ───────────────────────────────────────────────────

    fn ob_resolve<'a>(obligations: &'a Obligations, name: &str) -> Option<&'a Lexeme> {
        obligations
            .iter()
            .find(|o| o.name == name)
            .and_then(|o| o.value.as_ref())
    }

    fn ob_type(obligations: &Obligations, name: &str) -> Option<usize> {
        obligations
            .iter()
            .find(|o| o.name == name)
            .and_then(|o| o.evidence)
    }

    // ── TypeExpr evaluation ──────────────────────────────────────────────────

    /// Evaluate a `TypeExpr` to a closed `Type`, resolving `TypeOf` and
    /// `ContextExt` against obligations and context.
    fn eval_type(
        evidence: &EvidenceStore<Type>,
        expr: &TypeExpr,
        obligations: &Obligations,
        ctx: &Context,
        segs: &[Segment],
        metas: &std::collections::HashMap<String, Type>,
    ) -> Option<Type> {
        match expr {
            TypeExpr::Lit(s) => Some(Type::Raw(s.clone())),
            TypeExpr::Arrow(domain, codomain) => Some(Type::Arrow(
                Box::new(Self::eval_type(
                    evidence,
                    domain,
                    obligations,
                    ctx,
                    segs,
                    metas,
                )?),
                Box::new(Self::eval_type(
                    evidence,
                    codomain,
                    obligations,
                    ctx,
                    segs,
                    metas,
                )?),
            )),
            TypeExpr::Union(parts) => {
                let types: Vec<Type> = parts
                    .iter()
                    .map(|p| Self::eval_type(evidence, p, obligations, ctx, segs, metas))
                    .collect::<Option<_>>()?;
                Some(Type::Union(types))
            }
            TypeExpr::Not(inner) => Some(Type::Not(Box::new(Self::eval_type(
                evidence,
                inner,
                obligations,
                ctx,
                segs,
                metas,
            )?))),
            TypeExpr::Any => Some(Type::Any),
            TypeExpr::None => Some(Type::None),
            TypeExpr::Meta(name) => metas.get(name).cloned(),
            TypeExpr::ContextExt(var) => {
                let lexeme = Self::ob_resolve(obligations, var)?;
                let text = lexeme.value(segs).unwrap_or_default();
                if let Some(found) = ctx.lookup(&text) {
                    return Some(found.clone());
                }
                if lexeme.open
                    && let Some(found) = ctx.lookup_starts_with(&text) {
                        return Some(found.clone());
                    }
                None
            }
            TypeExpr::TypeOf(binding) => {
                if let Some(ev_id) = Self::ob_type(obligations, binding)
                    && ev_id != crate::engine::parse::arena::TOP {
                        return evidence.get(ev_id);
                    }
                Self::ob_resolve(obligations, binding)
                    .and_then(|lex| lex.value(segs))
                    .and_then(|v| Type::parse_raw(&v).ok())
            }
        }
    }

    // ── Meta binding helpers ───────────────────────────────────────────────

/// Bind unbound metas in an ascription's expected type by pattern-matching
/// against the actual type.  E.g., `l : ?A → ?B` against actual `Int → Bool`
/// binds `A := Int`, `B := Bool`.
///
/// Draft §3 "Meta compilation pipeline" (`sec:meta-compilation`) —
/// evaluation-time decomposition. Arrow patterns decompose the actual
/// type into domain and codomain components, binding fresh metas for each.
///
/// This function together with `eval_type` implements the evidence
/// realizability lemmas: `lem:evidence-realizable` (type evidence
/// reaches Exact) and `lem:typeof-realizable` (binding evidence
/// resolves to its rule conclusion).
    fn bind_asc(
        _evidence: &EvidenceStore<Type>,
        ty: &TypeExpr,
        actual: &Type,
        metas: &mut std::collections::HashMap<String, Type>,
    ) {
        match ty {
            TypeExpr::Meta(name) => {
                metas.entry(name.clone()).or_insert_with(|| actual.clone());
            }
            TypeExpr::Arrow(domain, codomain) => {
                if let Type::Arrow(d, c) = actual {
                    Self::bind_asc(_evidence, domain, d, metas);
                    Self::bind_asc(_evidence, codomain, c, metas);
                }
            }
            _ => {}
        }
    }

    /// Bind unbound metas on either side of an equality constraint.
    /// If one side is a Meta and the other is evaluable, bind the Meta.
    fn bind_eq(
        evidence: &EvidenceStore<Type>,
        left: &TypeExpr,
        right: &TypeExpr,
        obligations: &Obligations,
        ctx: &Context,
        segs: &[Segment],
        metas: &mut std::collections::HashMap<String, Type>,
    ) {
        match (left, right) {
            (TypeExpr::Meta(name), other) | (other, TypeExpr::Meta(name)) => {
                if !metas.contains_key(name)
                    && let Some(t) = Self::eval_type(evidence, other, obligations, ctx, segs, metas)
                    {
                        metas.insert(name.clone(), t);
                    }
            }
            _ => {}
        }
    }

    // ── Context helpers ──────────────────────────────────────────────────────

    fn extend(ctx: &Context, value: &str, resolved: Type) -> Context {
        ctx.shadow(value.to_string(), resolved)
    }

    // ── Premise checking ─────────────────────────────────────────────────────

    fn setting(
        evidence: &EvidenceStore<Type>,
        setting: &TypeSetting,
        obligations: &Obligations,
        premise_ctx: &mut Context,
        segs: &[Segment],
        metas: &std::collections::HashMap<String, Type>,
    ) -> Result<(), PremiseStatus> {
        for (name, ext_ty) in &setting.extensions {
            let Some(value) = Self::ob_resolve(obligations, name).and_then(|lex| lex.value(segs))
            else {
                return Err(PremiseStatus::Unknown);
            };
            let resolved = Self::eval_type(evidence, ext_ty, obligations, premise_ctx, segs, metas);
            let Some(resolved) = resolved else {
                return Err(PremiseStatus::Unknown);
            };
            *premise_ctx = Self::extend(premise_ctx, &value, resolved);
        }
        Ok(())
    }

    fn check(
        evidence: &EvidenceStore<Type>,
        premise: &Premise,
        obligations: &Obligations,
        ctx: &mut Context,
        allow_missing_obligation: bool,
        segs: &[Segment],
        metas: &mut std::collections::HashMap<String, Type>,
    ) -> PremiseStatus {
        let base_ctx = ctx.clone();
        let mut premise_ctx = ctx.clone();
        let setting_extends = premise
            .setting
            .as_ref()
            .is_some_and(|s| !s.extensions.is_empty());

        if let Some(setting) = &premise.setting {
            match Self::setting(
                evidence,
                setting,
                obligations,
                &mut premise_ctx,
                segs,
                metas,
            ) {
                Ok(()) => {}
                Err(status) => return status,
            }
        }

        let Some(judgment) = &premise.judgment else {
            *ctx = premise_ctx;
            return PremiseStatus::Satisfied;
        };

        match judgment {
            TypingJudgment::Membership(var, _) => {
                let Some(lexeme) = Self::ob_resolve(obligations, var) else {
                    return PremiseStatus::Unknown;
                };
                let text = lexeme.value(segs).unwrap_or_default();
                let exact = !text.is_empty() && premise_ctx.lookup(&text).is_some();
                let prefix = lexeme.open
                    && !text.is_empty()
                    && premise_ctx.lookup_starts_with(&text).is_some();
                let found = exact || prefix;
                *ctx = if found && !setting_extends {
                    premise_ctx
                } else {
                    base_ctx
                };
                match (found, lexeme.open, text.is_empty()) {
                    (true, _, _) => PremiseStatus::Satisfied,
                    (false, _, true) => PremiseStatus::Unknown,
                    (false, _, false) => PremiseStatus::Contradiction,
                }
            }

            TypingJudgment::Ascription((term, ty)) => {
                let Some(ob) = obligations.iter().find(|o| o.name == *term) else {
                    if allow_missing_obligation {
                        return PremiseStatus::Unknown;
                    }
                    return PremiseStatus::Contradiction;
                };
                if ob.value.is_none() {
                    return PremiseStatus::Unknown;
                }
                let open = ob.value.as_ref().is_some_and(|v| v.open);
                let Some(actual_id) = ob.evidence else {
                    return if open {
                        PremiseStatus::Unknown
                    } else {
                        PremiseStatus::Contradiction
                    };
                };
                let Some(actual) = evidence.get(actual_id) else {
                    return PremiseStatus::Contradiction;
                };

                // Resolve any unbound metas in the expected type before evaluating
                Self::bind_asc(evidence, ty, &actual, metas);

                let Some(expected) =
                    Self::eval_type(evidence, ty, obligations, &premise_ctx, segs, metas)
                else {
                    return PremiseStatus::Unknown;
                };
                match Unifier::unify(&actual, &expected) {
                    UnifyResult::Ok => {
                        *ctx = if setting_extends {
                            base_ctx
                        } else {
                            premise_ctx
                        };
                        PremiseStatus::Satisfied
                    }
                    UnifyResult::Indeterminate => PremiseStatus::Unknown,
                    UnifyResult::Fail(_) => {
                        if subtype(&actual, &expected) {
                            *ctx = if setting_extends {
                                base_ctx
                            } else {
                                premise_ctx
                            };
                            return PremiseStatus::Satisfied;
                        }
                        if open {
                            PremiseStatus::Unknown
                        } else {
                            PremiseStatus::Contradiction
                        }
                    }
                }
            }

            TypingJudgment::Operation { left, op, right } => {
                let Some(l) =
                    Self::eval_type(evidence, left, obligations, &premise_ctx, segs, metas)
                else {
                    return PremiseStatus::Unknown;
                };
                let Some(r) =
                    Self::eval_type(evidence, right, obligations, &premise_ctx, segs, metas)
                else {
                    return PremiseStatus::Unknown;
                };
                let status = match op {
                    TypeOperation::Equality => match Unifier::unify(&l, &r) {
                        UnifyResult::Ok => PremiseStatus::Satisfied,
                        UnifyResult::Indeterminate => PremiseStatus::Unknown,
                        UnifyResult::Fail(_) => PremiseStatus::Contradiction,
                    },
                    TypeOperation::Inclusion => {
                        if subtype(&l, &r) {
                            PremiseStatus::Satisfied
                        } else if l.is_any() || r.is_any() {
                            PremiseStatus::Unknown
                        } else {
                            PremiseStatus::Contradiction
                        }
                    }
                };
                match status {
                    PremiseStatus::Satisfied => {
                        *ctx = if setting_extends {
                            base_ctx
                        } else {
                            premise_ctx
                        };
                        PremiseStatus::Satisfied
                    }
                    other => other,
                }
            }

            TypingJudgment::Equality { left, right } => {
                Self::bind_eq(
                    evidence,
                    left,
                    right,
                    obligations,
                    &premise_ctx,
                    segs,
                    metas,
                );
                let Some(l) =
                    Self::eval_type(evidence, left, obligations, &premise_ctx, segs, metas)
                else {
                    return PremiseStatus::Unknown;
                };
                let Some(r) =
                    Self::eval_type(evidence, right, obligations, &premise_ctx, segs, metas)
                else {
                    return PremiseStatus::Unknown;
                };
                match Unifier::unify(&l, &r) {
                    UnifyResult::Ok => {
                        *ctx = if setting_extends {
                            base_ctx
                        } else {
                            premise_ctx
                        };
                        PremiseStatus::Satisfied
                    }
                    UnifyResult::Indeterminate => PremiseStatus::Unknown,
                    UnifyResult::Fail(_) => PremiseStatus::Contradiction,
                }
            }
        }
    }

    fn eval_rule(
        evidence: &EvidenceStore<Type>,
        rule: &TypingRule,
        obligations: &Obligations,
        ctx: Context,
        status: NodeStatus,
        segs: &[Segment],
    ) -> RuleResult {
        let allow_missing_obligation = status.open();
        let mut mctx = ctx.clone();
        let mut metas: std::collections::HashMap<String, Type> = std::collections::HashMap::new();

        let all_satisfied = match rule
            .premises
            .iter()
            .try_fold(true, |all_satisfied, premise| {
                match Self::check(
                    evidence,
                    premise,
                    obligations,
                    &mut mctx,
                    allow_missing_obligation,
                    segs,
                    &mut metas,
                ) {
                    PremiseStatus::Contradiction => Err(()),
                    PremiseStatus::Unknown => Ok(false),
                    PremiseStatus::Satisfied => Ok(all_satisfied),
                }
            }) {
            Err(()) => return RuleResult::Contradiction,
            Ok(v) => v,
        };

        let ty = Self::eval_type(
            evidence,
            &rule.conclusion.kind,
            obligations,
            &mctx,
            segs,
            &metas,
        );

        let ty = match ty {
            Some(ty) => ty,
            None => return RuleResult::Partial(Type::Any),
        };

        if !all_satisfied {
            return RuleResult::Partial(ty);
        }

        let transforms: Vec<(String, Type)> = match &rule.conclusion.context.output {
            Some(output_ctx) => output_ctx
                .extensions
                .iter()
                .filter_map(|(v, t): &(String, TypeExpr)| {
                    let resolved_name = Self::ob_resolve(obligations, v)
                        .and_then(|lex| lex.value(segs))
                        .unwrap_or_else(|| v.clone());
                    Self::eval_type(evidence, t, obligations, &mctx, segs, &metas)
                        .map(|resolved| (resolved_name, resolved))
                })
                .collect(),
            None => Vec::new(),
        };
        let transition = ContextTransition { transforms };

        RuleResult::Success((ty, Some(transition)))
    }
}

// ── Value-level evaluation ───────────────────────────────────────────────────

impl TypingDomain {
    /// The top evidence sentinel (interns to `TOP = 0`).
    pub fn top_evidence() -> Type {
        Type::Any
    }

    /// The bottom evidence sentinel (interns to `BOT = 1`).
    pub fn bottom_evidence() -> Type {
        Type::None
    }

    /// Context to use when entering the child bound by `binding` — `sec:context-flow`.
    pub fn descend(
        &self,
        rule: &TypingRule,
        binding: Option<&str>,
        ctx: &Context,
        obligations: &Obligations,
        segs: &[Segment],
        evidence: &EvidenceStore<Type>,
    ) -> Context {
        let Some(binding) = binding else {
            return ctx.clone();
        };

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

            let mut current_ctx = ctx.clone();
            for (name, ext_ty) in &setting.extensions {
                let Some(value) =
                    Self::ob_resolve(obligations, name).and_then(|lex| lex.value(segs))
                else {
                    return ctx.clone();
                };
                let Some(resolved) = Self::eval_type(
                    evidence,
                    ext_ty,
                    obligations,
                    &current_ctx,
                    segs,
                    &std::collections::HashMap::new(),
                ) else {
                    return ctx.clone();
                };
                current_ctx = Self::extend(&current_ctx, &value, resolved);
            }
            return current_ctx;
        }

        ctx.clone()
    }

    /// Per-node verdict, evidence, and exported effect — `def:eval-impl`.
    pub fn finalize(
        &self,
        rule: &TypingRule,
        ctx: &Context,
        obligations: &Obligations,
        segs: &[Segment],
        status: NodeStatus,
        evidence: &EvidenceStore<Type>,
    ) -> (Verdict, Type, Option<ContextTransition>) {
        match Self::eval_rule(evidence, rule, obligations, ctx.clone(), status, segs) {
            RuleResult::Contradiction => (Verdict::Lost, Type::Any, None),
            RuleResult::Partial(ty) => {
                if status.open() {
                    (Verdict::Live, ty, None)
                } else {
                    (Verdict::Lost, Type::Any, None)
                }
            }
            RuleResult::Success((ty, transition)) => {
                let effect = if status == NodeStatus::Exact {
                    transition.and_then(|t| {
                        if t.transforms.is_empty() {
                            None
                        } else {
                            Some(t)
                        }
                    })
                } else {
                    None
                };
                (Verdict::Satisfied, ty, effect)
            }
        }
    }

    /// Apply a right-bound effect to a sibling context — `⊕` of `def:constraint-domain`.
    pub fn apply_effect(&self, ctx: Context, effect: &ContextTransition) -> Context {
        effect
            .transforms
            .iter()
            .fold(ctx, |acc, (var, ty)| Self::extend(&acc, var, ty.clone()))
    }

    /// Left-to-right composition of effects for transparent productions.
    pub fn compose_effects(&self, effects: &[&ContextTransition]) -> Option<ContextTransition> {
        let mut composed = ContextTransition::identity();
        for &effect in effects {
            composed = composed.compose(effect);
        }
        if composed.transforms.is_empty() {
            None
        } else {
            Some(composed)
        }
    }
}
