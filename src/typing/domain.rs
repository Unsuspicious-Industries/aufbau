//! Typing evaluation — §2. One operation: ascription, discharged by term
//! unification. A type is a tree over the grammar (`Term`); a rule's flat
//! `TypeExpr` is parsed into a `TyExpr` pattern (by the runtime), resolved to a
//! `Term`, and unified. No subtyping: every relation is unifiability.
//!
//! Metavariables are global: a rule's holes are α-renamed per evaluation
//! (`?A` → `A#run`), and the bindings an evaluation discovers on *foreign*
//! variables (a child's, a sibling's) are exported as the residual equations of
//! its [`Evidence`]. Parents merge children's equations before running their own
//! rule, so a constraint discovered deep in one subtree reaches every other use
//! of the same metavariable at their least common ancestor (`def:solve`).

use std::cell::Cell;
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::engine::error::{TransitionError, UnresolvedKind};
use crate::engine::parse::arena::{Lexeme, NodeStatus};
use crate::engine::Segment;
use crate::semantics::domain::Verdict;
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::Obligations;
use crate::typing::ir::{Instr, Program};
use crate::typing::normalize::{failure_is_stable, unify_modulo, Normalizer};
use crate::typing::pattern::Pattern;
use crate::typing::rule::{PremiseStatus, RuleResult};
use crate::typing::term::{apply, Evidence, Term};
use crate::typing::{Context, ContextTransition, Subst, TyExpr, TypeExpr};

/// A rule's flat `TypeExpr`s mapped to their parsed trees. Precomputed by the
/// runtime, which holds the grammar.
pub type Trees = HashMap<TypeExpr, TyExpr>;

/// Rename a term's variables to globally-fresh ones — instantiation of a
/// polymorphic scheme at a use site. Reached only through `inst(x)`; ordinary
/// lookups share metavariable identity (the monomorphic default).
fn instantiate(t: &Term) -> Term {
    static NEXT: AtomicU64 = AtomicU64::new(0);
    let mut n = NEXT.fetch_add(1 << 20, Ordering::Relaxed);
    t.freshen(&mut n)
}

/// A fresh evaluation id: the α-renaming scope of one rule run.
fn rid() -> u64 {
    static NEXT: AtomicU64 = AtomicU64::new(1);
    NEXT.fetch_add(1, Ordering::Relaxed)
}

/// Per-instruction execution counts. Each `Cell` is incremented in the hot
/// path with a single atomic-free store; the bench snapshots the values via
/// `TypingDomain::stats`. Zero-cost when the domain is freshly default-
/// constructed (the field is dropped unused).
#[derive(Clone, Debug, Default)]
pub struct Stats {
    pub eval: Cell<usize>,
    pub ascribe: Cell<usize>,
    pub equate: Cell<usize>,
    pub member: Cell<usize>,
    pub push_scope: Cell<usize>,
    pub pop_scope: Cell<usize>,
    pub extend: Cell<usize>,
    pub emit: Cell<usize>,
    pub effect: Cell<usize>,
    pub unify: Cell<usize>,
    pub unify_fail: Cell<usize>,
}

/// Plain snapshot of [`Stats`] for benches and tests.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct StatsSnap {
    pub eval: usize,
    pub ascribe: usize,
    pub equate: usize,
    pub member: usize,
    pub push_scope: usize,
    pub pop_scope: usize,
    pub extend: usize,
    pub emit: usize,
    pub effect: usize,
    pub unify: usize,
    pub unify_fail: usize,
}

impl StatsSnap {
    fn snap(s: &Stats) -> Self {
        Self {
            eval: s.eval.get(),
            ascribe: s.ascribe.get(),
            equate: s.equate.get(),
            member: s.member.get(),
            push_scope: s.push_scope.get(),
            pop_scope: s.pop_scope.get(),
            extend: s.extend.get(),
            emit: s.emit.get(),
            effect: s.effect.get(),
            unify: s.unify.get(),
            unify_fail: s.unify_fail.get(),
        }
    }
}

/// Pure, value-level typing evaluation. The stateful id-interning shell lives
/// in `semantics::runtime::TypingRuntime`.
///
/// `TRACK` selects whether per-instruction execution counts are accumulated.
/// When `false` (the default), every `tick` is a constant-folded no-op — the
/// compiler eliminates the `Cell::set` from the hot path entirely. Set `true`
/// in benches and tests that need a `StatsSnap`; the production parser path
/// (and `TypingRuntime`) always uses the default `false`.
#[derive(Clone, Debug, Default)]
pub struct TypingDomain<const TRACK: bool = false> {
    stats: Stats,
}

impl<const TRACK: bool> TypingDomain<TRACK> {
    /// Snapshot the per-instruction execution counts. Cheap: one `Cell::get`
    /// per field.
    #[must_use]
    pub fn stats(&self) -> StatsSnap {
        StatsSnap::snap(&self.stats)
    }

    /// Zero the per-instruction counts. Used by the bench to isolate a single
    /// `parse` call's IR cost.
    pub fn reset_stats(&self) {
        self.stats.eval.set(0);
        self.stats.ascribe.set(0);
        self.stats.equate.set(0);
        self.stats.member.set(0);
        self.stats.push_scope.set(0);
        self.stats.pop_scope.set(0);
        self.stats.extend.set(0);
        self.stats.emit.set(0);
        self.stats.effect.set(0);
        self.stats.unify.set(0);
        self.stats.unify_fail.set(0);
    }

    /// Increment a per-instruction counter, or do nothing. The `if TRACK` is
    /// constant-folded; in the production (`false`) case the body vanishes.
    #[inline(always)]
    fn tick(cell: &Cell<usize>) {
        if TRACK {
            cell.set(cell.get() + 1);
        }
    }

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

    /// Resolve a `TyExpr` pattern to a concrete `Term`: holes α-renamed into
    /// this run's scope, refs from evidence, ctx from the context.
    fn eval(
        evidence: &EvidenceStore<Evidence>,
        ty: &TyExpr,
        obligations: &Obligations,
        ctx: &Context,
        segs: &[Segment],
        run: u64,
    ) -> Option<Term> {
        Some(match ty {
            TyExpr::Top => Term::top(),
            TyExpr::Bot => Term::bottom(),
            TyExpr::Lit(s) => Term::Leaf(Pattern::raw(s)),
            TyExpr::Var(n) => Term::Var(format!("{n}#{run}")),
            TyExpr::Ref(b) => Self::resolve_ref(evidence, obligations, b)?,
            TyExpr::Ctx(v) => Self::resolve_ctx(obligations, ctx, segs, v)?,
            // `inst(x)` is `Γ(x)` with its variables freshened — a polymorphic
            // scheme made concrete at this use. The one instantiation point.
            TyExpr::Inst(v) => instantiate(&Self::resolve_ctx(obligations, ctx, segs, v)?),
            TyExpr::Con(label, kids) => {
                let mut out = Vec::with_capacity(kids.len());
                for k in kids {
                    out.push(Self::eval(evidence, k, obligations, ctx, segs, run)?);
                }
                Term::Con(label.clone(), out)
            }
        })
    }

    /// The type of a binding: its evidence term, identity shared. `⊤` (no
    /// constraint yet) is not yet a type, so it reads as unresolved.
    fn resolve_ref(
        evidence: &EvidenceStore<Evidence>,
        obligations: &Obligations,
        b: &str,
    ) -> Option<Term> {
        let id = Self::ob_type(obligations, b)?;
        let ev = evidence.get(id)?;
        (!ev.is_top()).then_some(ev.term)
    }

    /// `Γ(v)`: the type bound to `v`'s value in the context, identity shared.
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
    /// Success binds holes into `subst` and is `Satisfied`; a failure is
    /// `Contradiction` only when stable under instantiation
    /// ([`failure_is_stable`]) and the node can no longer grow.
    fn ascribe(
        &self,
        norm: &Normalizer,
        actual: &Term,
        expected: &Term,
        subst: &mut Subst,
        open: bool,
    ) -> PremiseStatus {
        let mut s = subst.clone();
        Self::tick(&self.stats.unify);
        if unify_modulo(norm, expected, actual, &mut s, true) {
            *subst = s;
            PremiseStatus::Satisfied
        } else {
            Self::tick(&self.stats.unify_fail);
            if open || !failure_is_stable(norm, expected, actual) {
                PremiseStatus::Unknown
            } else {
                PremiseStatus::Contradiction
            }
        }
    }

    /// Merge the residual equations exported by resolved children into `subst`:
    /// the incremental closure of the constraint graph (`def:solve`). A stable
    /// conflict between subtrees is a `Contradiction`; an unstable one is
    /// postponed.
    fn merge_eqs(
        &self,
        norm: &Normalizer,
        evidence: &EvidenceStore<Evidence>,
        obligations: &Obligations,
        subst: &mut Subst,
    ) -> PremiseStatus {
        let mut st = PremiseStatus::Satisfied;
        for ob in obligations {
            let Some(ev) = ob.evidence.and_then(|id| evidence.get(id)) else {
                continue;
            };
            for (x, t) in &ev.eqs {
                let v = Term::Var(x.clone());
                Self::tick(&self.stats.unify);
                if !unify_modulo(norm, &v, t, subst, true) {
                    Self::tick(&self.stats.unify_fail);
                    if failure_is_stable(norm, &v, t) {
                        return PremiseStatus::Contradiction;
                    }
                    st = PremiseStatus::Unknown;
                }
            }
        }
        st
    }

    /// Bindings on variables visible outside this evaluation — everything not
    /// α-renamed by this run — with values fully resolved.
    fn export(subst: &Subst, run: u64) -> Vec<(String, Term)> {
        let suffix = format!("#{run}");
        let mut eqs: Vec<(String, Term)> = subst
            .keys()
            .filter(|k| !k.ends_with(&suffix))
            .map(|k| (k.clone(), apply(&Term::Var(k.clone()), subst)))
            .collect();
        eqs.sort_by(|a, b| a.0.cmp(&b.0));
        eqs
    }

    // ── Context helpers ─────────────────────────────────────────────────────

    fn extend(ctx: &Context, value: &str, resolved: Term) -> Context {
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

    /// Run one `Eval` instruction: resolve `expr` against `ctx` and store it in
    /// register `dst`. The single implementation of evaluation-into-a-register,
    /// shared by the `descend` and `run` interpreters.
    #[allow(clippy::too_many_arguments)]
    fn eval_to_reg(
        &self,
        regs: &mut Vec<Option<Term>>,
        dst: usize,
        expr: &TyExpr,
        evidence: &EvidenceStore<Evidence>,
        obligations: &Obligations,
        ctx: &Context,
        segs: &[Segment],
        run: u64,
    ) {
        Self::tick(&self.stats.eval);
        let v = Self::eval(evidence, expr, obligations, ctx, segs, run);
        Self::set(regs, dst, v);
    }

    /// The `(context key, type term)` an `Extend` shadows, when both its binding
    /// value and its type register are available. `Err` names which half is
    /// missing; the two interpreters differ only in what they do with that
    /// (`descend` retries later, `run` stays `Unknown`), not in how it resolves.
    fn extend_inputs(
        obligations: &Obligations,
        regs: &[Option<Term>],
        binding: &str,
        ty: usize,
        segs: &[Segment],
    ) -> Result<(String, Term), UnresolvedKind> {
        let value = Self::ob_resolve(obligations, binding)
            .and_then(|lex| lex.value(segs))
            .ok_or(UnresolvedKind::Value)?;
        let r = Self::reg(regs, ty).ok_or(UnresolvedKind::Type)?;
        Ok((value, r))
    }

    // ── IR execution ─────────────────────────────────────────────────────────

    /// Discharge an `ascribe` instruction: the obligation bound to `binding` must
    /// have a type that unifies with `expected` (the evaluated register).
    #[allow(clippy::too_many_arguments)]
    fn run_ascribe(
        &self,
        norm: &Normalizer,
        evidence: &EvidenceStore<Evidence>,
        obligations: &Obligations,
        binding: &str,
        expected: Option<Term>,
        subst: &mut Subst,
        allow_missing: bool,
    ) -> PremiseStatus {
        Self::tick(&self.stats.ascribe);
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
        self.ascribe(norm, &actual.term, &expected, subst, open)
    }

    /// Discharge a `member` instruction: `binding`'s value is in the context (an
    /// open prefix may still match).
    fn run_member(
        &self,
        obligations: &Obligations,
        ctx: &Context,
        binding: &str,
        segs: &[Segment],
    ) -> PremiseStatus {
        Self::tick(&self.stats.member);
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
    #[allow(clippy::too_many_arguments)]
    fn run(
        &self,
        program: &Program,
        norm: &Normalizer,
        evidence: &EvidenceStore<Evidence>,
        obligations: &Obligations,
        ctx: Context,
        status: NodeStatus,
        segs: &[Segment],
    ) -> RuleResult {
        let allow_missing = status.open();
        let run = rid();
        let mut subst = Subst::new();
        let mut regs: Vec<Option<Term>> = Vec::new();
        let mut ctxs = vec![ctx];
        let mut satisfied = true;
        let mut output: Option<Term> = None;
        let mut effects: Vec<(String, Term)> = Vec::new();

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

        combine!(self.merge_eqs(norm, evidence, obligations, &mut subst));

        for instr in &program.instrs {
            match instr {
                Instr::Eval { dst, expr } => {
                    let top = Self::top(&ctxs).clone();
                    self.eval_to_reg(&mut regs, *dst, expr, evidence, obligations, &top, segs, run);
                }
                Instr::Ascribe { binding, expected } => {
                    let exp = Self::reg(&regs, *expected);
                    combine!(self.run_ascribe(
                        norm,
                        evidence,
                        obligations,
                        binding,
                        exp,
                        &mut subst,
                        allow_missing
                    ));
                }
                Instr::Equate { left, right } => {
                    Self::tick(&self.stats.equate);
                    let st = match (Self::reg(&regs, *left), Self::reg(&regs, *right)) {
                        // No subtyping: equality reduces to unifiability.
                        (Some(l), Some(r)) => {
                            let mut s = subst.clone();
                            Self::tick(&self.stats.unify);
                            if unify_modulo(norm, &l, &r, &mut s, true) {
                                subst = s;
                                PremiseStatus::Satisfied
                            } else {
                                Self::tick(&self.stats.unify_fail);
                                if failure_is_stable(norm, &l, &r) {
                                    PremiseStatus::Contradiction
                                } else {
                                    PremiseStatus::Unknown
                                }
                            }
                        }
                        _ => PremiseStatus::Unknown,
                    };
                    combine!(st);
                }
                Instr::Member { binding } => {
                    combine!(self.run_member(obligations, Self::top(&ctxs), binding, segs));
                }
                Instr::PushScope => {
                    Self::tick(&self.stats.push_scope);
                    let t = Self::top(&ctxs).clone();
                    ctxs.push(t);
                }
                Instr::PopScope => {
                    Self::tick(&self.stats.pop_scope);
                    if ctxs.len() > 1 {
                        ctxs.pop();
                    }
                }
                Instr::Extend { binding, ty } => {
                    Self::tick(&self.stats.extend);
                    match Self::extend_inputs(obligations, &regs, binding, *ty, segs) {
                        Ok((v, r)) => {
                            let top = ctxs.last_mut().expect("context stack is never empty");
                            *top = top.shadow(v, r);
                        }
                        // A setting that cannot resolve leaves the rule unsatisfied.
                        Err(_) => satisfied = false,
                    }
                }
                Instr::Emit { ty } => {
                    Self::tick(&self.stats.emit);
                    output = Self::reg(&regs, *ty);
                }
                Instr::Effect { binding, ty } => {
                    Self::tick(&self.stats.effect);
                    let name = Self::ob_resolve(obligations, binding)
                        .and_then(|lex| lex.value(segs))
                        .unwrap_or_else(|| binding.clone());
                    if let Some(r) = Self::reg(&regs, *ty) {
                        effects.push((name, r));
                    }
                }
            }
        }

        let eqs = Self::export(&subst, run);
        let Some(ty) = output else {
            return RuleResult::Partial(Evidence {
                term: Term::top(),
                eqs,
            });
        };
        let ev = Evidence {
            term: apply(&ty, &subst),
            eqs,
        };
        if !satisfied {
            return RuleResult::Partial(ev);
        }
        let transforms = effects
            .into_iter()
            .map(|(n, t)| (n, apply(&t, &subst)))
            .collect();
        RuleResult::Success((ev, Some(ContextTransition { transforms })))
    }
}

// ── Value-level evaluation ──────────────────────────────────────────────────

impl<const TRACK: bool> TypingDomain<TRACK> {
    /// Context to use when entering the child bound by `binding`.
    ///
    /// Typing runs per node during the parse, so a binding's setting must be
    /// resolved against the premises *already* discharged before the parser
    /// reaches it — not later, in `finalize`. So this first replays the program
    /// up to `b`'s setting, threading the substitution those premises determine
    /// (e.g. `scrutinee : ?A list` binds `?A` to the element type), then applies
    /// `b`'s setting with that substitution. Without the replay a setting like a
    /// `match` arm's `[head:?A]` would bind `head` to a free variable, and the
    /// arm could be typed against *any* type — unsound.
    ///
    /// `Err(Unresolved)` when a setting extension can't be resolved yet: the
    /// parser treats that as a soft no-op and retries when more input arrives.
    #[allow(clippy::too_many_arguments)]
    pub fn descend(
        &self,
        program: &Program,
        norm: &Normalizer,
        binding: Option<&str>,
        ctx: &Context,
        obligations: &Obligations,
        segs: &[Segment],
        evidence: &EvidenceStore<Evidence>,
    ) -> Result<Context, TransitionError> {
        let Some(b) = binding else {
            return Ok(ctx.clone());
        };
        let Some(range) = program.splices.get(b).cloned() else {
            return Ok(ctx.clone());
        };
        let run = rid();
        let mut regs: Vec<Option<Term>> = Vec::new();
        let mut subst = Subst::new();
        let _ = self.merge_eqs(norm, evidence, obligations, &mut subst);
        // Replay the earlier premises for the substitution they fix. Their own
        // settings are premise-local, so only the judgments that bind metavariables
        // (ascriptions against a resolved sibling, equalities) matter here.
        for instr in &program.instrs[..range.start] {
            match instr {
                Instr::Eval { dst, expr } => {
                    self.eval_to_reg(&mut regs, *dst, expr, evidence, obligations, ctx, segs, run);
                }
                Instr::Ascribe { binding, expected } => {
                    if let Some(actual) = Self::resolve_ref(evidence, obligations, binding)
                        && let Some(exp) = Self::reg(&regs, *expected)
                    {
                        let _ = unify_modulo(norm, &exp, &actual, &mut subst, true);
                    }
                }
                Instr::Equate { left, right } => {
                    if let (Some(l), Some(r)) = (Self::reg(&regs, *left), Self::reg(&regs, *right)) {
                        let _ = unify_modulo(norm, &l, &r, &mut subst, true);
                    }
                }
                _ => {}
            }
        }
        // Apply `b`'s own setting, now that the substitution is known.
        let mut out = ctx.clone();
        for instr in &program.instrs[range] {
            match instr {
                Instr::Eval { dst, expr } => {
                    self.eval_to_reg(&mut regs, *dst, expr, evidence, obligations, &out, segs, run);
                }
                Instr::Extend { binding, ty } => {
                    Self::tick(&self.stats.extend);
                    let (value, r) = Self::extend_inputs(obligations, &regs, binding, *ty, segs)
                        .map_err(|kind| TransitionError::Unresolved {
                            rule: program.name.clone(),
                            binding: b.to_string(),
                            setting: binding.clone(),
                            kind,
                        })?;
                    out = out.shadow(value, apply(&r, &subst));
                }
                _ => {}
            }
        }
        Ok(out)
    }

    /// Per-node verdict, evidence, and exported effect.
    #[allow(clippy::too_many_arguments)]
    pub fn finalize(
        &self,
        program: &Program,
        norm: &Normalizer,
        ctx: &Context,
        obligations: &Obligations,
        segs: &[Segment],
        status: NodeStatus,
        evidence: &EvidenceStore<Evidence>,
    ) -> (Verdict, Evidence, Option<ContextTransition>) {
        match self.run(
            program,
            norm,
            evidence,
            obligations,
            ctx.clone(),
            status,
            segs,
        ) {
            RuleResult::Contradiction => (Verdict::Lost, Evidence::top(), None),
            RuleResult::Partial(ev) => {
                if status.open() {
                    (Verdict::Live, ev, None)
                } else {
                    (Verdict::Lost, Evidence::top(), None)
                }
            }
            RuleResult::Success((ev, transition)) => {
                let effect = if status == NodeStatus::Exact {
                    transition.filter(|t| !t.transforms.is_empty())
                } else {
                    None
                };
                (Verdict::Satisfied, ev, effect)
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
