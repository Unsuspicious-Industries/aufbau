//! Typing evaluation — §2. One operation: ascription, discharged by term
//! unification. A type is a tree over the grammar (`Term`); a rule's flat
//! `TypeExpr` is parsed into a `TyExpr` pattern (by the runtime), resolved to a
//! `Term`, and unified. No subtyping: every relation is unifiability.

use std::collections::HashMap;

use crate::engine::Segment;
use crate::engine::parse::arena::{Lexeme, NodeStatus};
use crate::semantics::Obligations;
use crate::semantics::domain::Verdict;
use crate::semantics::evidence::EvidenceStore;
use crate::typing::ir::{Instr, Program, compile};
use crate::typing::normalize::{Normalizer, unify_modulo};
use crate::typing::pattern::Pattern;
use crate::typing::rule::{PremiseStatus, RuleResult, TypingJudgment};
use crate::typing::term::Term;
use crate::typing::{Context, ContextTransition, Subst, TyExpr, Type, TypeExpr, TypingRule};

/// A rule's flat `TypeExpr`s mapped to their parsed trees. Precomputed by the
/// runtime, which holds the grammar.
pub type Trees = HashMap<TypeExpr, TyExpr>;

/// Pure, value-level typing evaluation. The stateful id-interning shell lives in
/// `semantics::runtime::TypingRuntime`.
#[derive(Clone, Debug, Default)]
pub struct TypingDomain;

impl TypingDomain {
    // ── Obligation helpers ──────────────────────────────────────────────────

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

    // ── Expression evaluation ───────────────────────────────────────────────

    /// Resolve a flat `TypeExpr` to a `Term` via its precomputed tree.
    fn eval(
        trees: &Trees,
        evidence: &EvidenceStore<Type>,
        expr: &TypeExpr,
        obligations: &Obligations,
        ctx: &Context,
        segs: &[Segment],
        subst: &Subst,
    ) -> Option<Term> {
        let ty = trees.get(expr)?;
        Self::eval_ty(evidence, ty, obligations, ctx, segs, subst)
    }

    /// Resolve a `TyExpr` pattern to a concrete `Term`: holes from `subst` (or
    /// left as variables), refs from evidence, ctx from the context.
    fn eval_ty(
        evidence: &EvidenceStore<Type>,
        ty: &TyExpr,
        obligations: &Obligations,
        ctx: &Context,
        segs: &[Segment],
        subst: &Subst,
    ) -> Option<Term> {
        Some(match ty {
            TyExpr::Top => Term::top(),
            TyExpr::Bot => Term::bottom(),
            TyExpr::Lit(s) => Term::Leaf(Pattern::raw(s)),
            TyExpr::Var(n) => subst.get(n).cloned().unwrap_or_else(|| Term::Var(n.clone())),
            TyExpr::Ref(b) => Self::resolve_ref(evidence, obligations, b)?,
            TyExpr::Ctx(v) => Self::resolve_ctx(obligations, ctx, segs, v)?,
            TyExpr::Con(label, kids) => {
                let mut out = Vec::with_capacity(kids.len());
                for k in kids {
                    out.push(Self::eval_ty(evidence, k, obligations, ctx, segs, subst)?);
                }
                Term::Con(label.clone(), out)
            }
        })
    }

    /// The type of a binding: its evidence tree. `⊤` (no constraint yet) is not
    /// yet a type, so it reads as unresolved.
    fn resolve_ref(
        evidence: &EvidenceStore<Type>,
        obligations: &Obligations,
        b: &str,
    ) -> Option<Term> {
        let id = Self::ob_type(obligations, b)?;
        let t = evidence.get(id)?;
        (!t.is_top()).then_some(t)
    }

    /// `Γ(v)`: the type bound to `v`'s value in the context.
    fn resolve_ctx(
        obligations: &Obligations,
        ctx: &Context,
        segs: &[Segment],
        v: &str,
    ) -> Option<Term> {
        let lex = Self::ob_resolve(obligations, v)?;
        let text = lex.value(segs).unwrap_or_default();
        if let Some(t) = ctx.lookup(&text) {
            return Some(t.clone());
        }
        if lex.open {
            return ctx.lookup_starts_with(&text).cloned();
        }
        None
    }

    // ── Ascription ──────────────────────────────────────────────────────────

    /// Discharge `actual : expected` by unification modulo the rewrite theory.
    /// Success binds holes into `subst` and is `Satisfied`; a clash is
    /// `Contradiction`, or `Unknown` when the node may still grow (directional via
    /// openness).
    fn ascribe(
        norm: &Normalizer,
        actual: &Term,
        expected: &Term,
        subst: &mut Subst,
        open: bool,
    ) -> PremiseStatus {
        let mut s = subst.clone();
        if unify_modulo(norm, expected, actual, &mut s, true) {
            *subst = s;
            PremiseStatus::Satisfied
        } else if open {
            PremiseStatus::Unknown
        } else {
            PremiseStatus::Contradiction
        }
    }

    // ── Context helpers ─────────────────────────────────────────────────────

    fn extend(ctx: &Context, value: &str, resolved: Type) -> Context {
        ctx.shadow(value.to_string(), resolved)
    }

    /// Top of the premise-local context stack (never empty).
    fn top(ctxs: &[Context]) -> &Context {
        ctxs.last().expect("context stack is never empty")
    }

    /// The term in register `i`, if any.
    fn reg(regs: &[Option<Term>], i: usize) -> Option<Term> {
        regs.get(i).cloned().flatten()
    }

    /// Write register `dst`, growing the file as needed.
    fn set(regs: &mut Vec<Option<Term>>, dst: usize, v: Option<Term>) {
        if dst >= regs.len() {
            regs.resize(dst + 1, None);
        }
        regs[dst] = v;
    }

    // ── IR execution ─────────────────────────────────────────────────────────

    /// Discharge an `ascribe` instruction: the obligation bound to `binding` must
    /// have a type that unifies with `expected` (the evaluated register).
    fn run_ascribe(
        norm: &Normalizer,
        evidence: &EvidenceStore<Type>,
        obligations: &Obligations,
        binding: &str,
        expected: Option<Term>,
        subst: &mut Subst,
        allow_missing: bool,
    ) -> PremiseStatus {
        let Some(ob) = obligations.iter().find(|o| o.name.as_str() == binding) else {
            return if allow_missing {
                PremiseStatus::Unknown
            } else {
                PremiseStatus::Contradiction
            };
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
        let Some(expected) = expected else {
            return PremiseStatus::Unknown;
        };
        Self::ascribe(norm, &actual, &expected, subst, open)
    }

    /// Discharge a `member` instruction: `binding`'s value is in the context (an
    /// open prefix may still match).
    fn run_member(
        obligations: &Obligations,
        ctx: &Context,
        binding: &str,
        segs: &[Segment],
    ) -> PremiseStatus {
        let Some(lex) = Self::ob_resolve(obligations, binding) else {
            return PremiseStatus::Unknown;
        };
        let text = lex.value(segs).unwrap_or_default();
        let exact = !text.is_empty() && ctx.lookup(&text).is_some();
        let prefix = lex.open && !text.is_empty() && ctx.lookup_starts_with(&text).is_some();
        match (exact || prefix, text.is_empty()) {
            (true, _) => PremiseStatus::Satisfied,
            (false, true) => PremiseStatus::Unknown,
            (false, false) => PremiseStatus::Contradiction,
        }
    }

    /// Execute a compiled rule program: a flat fold over the instruction stream
    /// threading a substitution and a stack of premise-local contexts. The control
    /// flow the tree-walk did implicitly (premise scoping) is the `Push`/`Pop`
    /// instructions, so this is the single rule evaluator.
    fn run(
        program: &Program,
        norm: &Normalizer,
        evidence: &EvidenceStore<Type>,
        obligations: &Obligations,
        ctx: Context,
        status: NodeStatus,
        segs: &[Segment],
    ) -> RuleResult {
        let allow_missing = status.open();
        let mut subst = Subst::new();
        let mut regs: Vec<Option<Term>> = Vec::new();
        let mut ctxs = vec![ctx];
        let mut satisfied = true;
        let mut output: Option<Term> = None;
        let mut effects: Vec<(String, Type)> = Vec::new();

        // A premise status folds into the running verdict; a contradiction ends it.
        macro_rules! combine {
            ($st:expr) => {
                match $st {
                    PremiseStatus::Contradiction => return RuleResult::Contradiction,
                    PremiseStatus::Unknown => satisfied = false,
                    PremiseStatus::Satisfied => {}
                }
            };
        }

        for instr in &program.instrs {
            match instr {
                Instr::Eval { dst, expr } => {
                    let v = Self::eval_ty(evidence, expr, obligations, Self::top(&ctxs), segs, &subst);
                    Self::set(&mut regs, *dst, v);
                }
                Instr::Ascribe { binding, expected } => {
                    let exp = Self::reg(&regs, *expected);
                    combine!(Self::run_ascribe(
                        norm, evidence, obligations, binding, exp, &mut subst, allow_missing
                    ));
                }
                Instr::Equate { left, right } => {
                    let st = match (Self::reg(&regs, *left), Self::reg(&regs, *right)) {
                        // No subtyping: equality reduces to unifiability (no binding
                        // is exported, matching the prior operation semantics).
                        (Some(l), Some(r)) => {
                            let mut s = subst.clone();
                            if unify_modulo(norm, &l, &r, &mut s, true) {
                                PremiseStatus::Satisfied
                            } else {
                                PremiseStatus::Contradiction
                            }
                        }
                        _ => PremiseStatus::Unknown,
                    };
                    combine!(st);
                }
                Instr::Member { binding } => {
                    combine!(Self::run_member(obligations, Self::top(&ctxs), binding, segs));
                }
                Instr::PushScope => {
                    let t = Self::top(&ctxs).clone();
                    ctxs.push(t);
                }
                Instr::PopScope => {
                    if ctxs.len() > 1 {
                        ctxs.pop();
                    }
                }
                Instr::Extend { binding, ty } => {
                    let value =
                        Self::ob_resolve(obligations, binding).and_then(|lex| lex.value(segs));
                    match (value, Self::reg(&regs, *ty)) {
                        (Some(v), Some(r)) => {
                            let top = ctxs.last_mut().expect("context stack is never empty");
                            *top = top.shadow(v, r);
                        }
                        // A setting that cannot resolve leaves the rule unsatisfied.
                        _ => satisfied = false,
                    }
                }
                Instr::Emit { ty } => output = Self::reg(&regs, *ty),
                Instr::Effect { binding, ty } => {
                    let name = Self::ob_resolve(obligations, binding)
                        .and_then(|lex| lex.value(segs))
                        .unwrap_or_else(|| binding.clone());
                    if let Some(r) = Self::reg(&regs, *ty) {
                        effects.push((name, r));
                    }
                }
            }
        }

        let Some(ty) = output else {
            return RuleResult::Partial(Type::top());
        };
        if !satisfied {
            return RuleResult::Partial(ty);
        }
        RuleResult::Success((ty, Some(ContextTransition { transforms: effects })))
    }
}

// ── Value-level evaluation ──────────────────────────────────────────────────

impl TypingDomain {
    /// The top evidence sentinel (interns to `TOP = 0`).
    #[must_use]
    pub fn top_evidence() -> Type {
        Type::top()
    }

    /// The bottom evidence sentinel (interns to `BOT = 1`).
    #[must_use]
    pub fn bottom_evidence() -> Type {
        Type::bottom()
    }

    /// Context to use when entering the child bound by `binding`.
    #[allow(clippy::too_many_arguments)]
    pub fn descend(
        &self,
        trees: &Trees,
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
            let mut cur = ctx.clone();
            for (name, ext_ty) in &setting.extensions {
                let Some(value) =
                    Self::ob_resolve(obligations, name).and_then(|lex| lex.value(segs))
                else {
                    return ctx.clone();
                };
                let Some(resolved) =
                    Self::eval(trees, evidence, ext_ty, obligations, &cur, segs, &Subst::new())
                else {
                    return ctx.clone();
                };
                cur = Self::extend(&cur, &value, resolved);
            }
            return cur;
        }
        ctx.clone()
    }

    /// Per-node verdict, evidence, and exported effect.
    #[allow(clippy::too_many_arguments)]
    pub fn finalize(
        &self,
        trees: &Trees,
        norm: &Normalizer,
        rule: &TypingRule,
        ctx: &Context,
        obligations: &Obligations,
        segs: &[Segment],
        status: NodeStatus,
        evidence: &EvidenceStore<Type>,
    ) -> (Verdict, Type, Option<ContextTransition>) {
        let program = compile(rule, trees);
        match Self::run(&program, norm, evidence, obligations, ctx.clone(), status, segs) {
            RuleResult::Contradiction => (Verdict::Lost, Type::top(), None),
            RuleResult::Partial(ty) => {
                if status.open() {
                    (Verdict::Live, ty, None)
                } else {
                    (Verdict::Lost, Type::top(), None)
                }
            }
            RuleResult::Success((ty, transition)) => {
                let effect = if status == NodeStatus::Exact {
                    transition.filter(|t| !t.transforms.is_empty())
                } else {
                    None
                };
                (Verdict::Satisfied, ty, effect)
            }
        }
    }

    /// Apply a right-bound effect to a sibling context.
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
        (!composed.transforms.is_empty()).then_some(composed)
    }
}
