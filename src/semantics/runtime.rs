//! Generic domain runtime — bridges `ConstraintDomain` to `SemanticRuntime`.
//!
//! `DomainRuntime<D>` owns an `SPG<D::Rule>` plus three interners for
//! `D::Evidence`, `D::Context`, and `D::Effect`. It implements the
//! parser-facing `SemanticRuntime` by:
//! 1. Looking up the concrete rule via `SPG::rule_for_prod`.
//! 2. Delegating to `D::descend` / `D::finalize` / `D::apply_effect` /
//!    `D::compose_effects` with concrete types.
//! 3. Interning the returned concrete values into opaque IDs.
//! 4. Translating `Verdict::Lost` → `Err(TransitionError::Rejected)`.
//!
//! The parser sees only opaque `CtxId`, `EvidenceId`, `EffectId` integers;
//! all domain-specific types are hidden inside this struct.

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;

use crate::logic::error::TransitionError;
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{
    ANY_TYPE, CtxId, EffectId, EvidenceId, NodeStatus, ProdId,
};
use crate::logic::semantic::{Obligations, SemanticRuntime, SemanticSummary};
use crate::logic::Segment;
use crate::semantics::domain::{ConstraintDomain, Verdict};

// ── Generic interner ─────────────────────────────────────────────────────────

struct Interner<T: Hash + Eq + Clone> {
    values: RefCell<Vec<T>>,
    index: RefCell<HashMap<T, usize>>,
}

impl<T: Hash + Eq + Clone> Interner<T> {
    fn new() -> Self {
        Self {
            values: RefCell::new(Vec::new()),
            index: RefCell::new(HashMap::new()),
        }
    }

    fn intern(&self, value: T) -> usize {
        if let Some(&id) = self.index.borrow().get(&value) {
            return id;
        }
        let mut values = self.values.borrow_mut();
        let id = values.len();
        values.push(value.clone());
        drop(values);
        self.index.borrow_mut().insert(value, id);
        id
    }

    fn get(&self, id: usize) -> Option<T> {
        self.values.borrow().get(id).cloned()
    }

    fn len(&self) -> usize {
        self.values.borrow().len()
    }
}

// ── DomainRuntime ─────────────────────────────────────────────────────────────

/// Generic bridge: `ConstraintDomain` → `SemanticRuntime`.
///
/// Holds the domain, the loaded grammar, and interners for the three
/// domain-specific opaque types. Segments are threaded per-call (§4.6).
pub struct DomainRuntime<D: ConstraintDomain> {
    domain: D,
    grammar: Grammar,
    evidence: Interner<D::Evidence>,
    contexts: Interner<D::Context>,
    effects: Interner<D::Effect>,
    segs: Vec<Segment>,
}

impl<D: ConstraintDomain> DomainRuntime<D> {
    pub fn new(domain: D, grammar: Grammar) -> Self {
        let rt = Self {
            domain,
            grammar,
            evidence: Interner::new(),
            contexts: Interner::new(),
            effects: Interner::new(),
            segs: Vec::new(),
        };
        // Intern the sentinels at known IDs (must match arena constants).
        let any = rt.domain.any_evidence();
        let any_id = rt.evidence.intern(any);
        debug_assert_eq!(any_id, ANY_TYPE);
        let empty_ctx = rt.domain.empty_context();
        let _ = rt.contexts.intern(empty_ctx);
        rt
    }

    pub fn grammar(&self) -> &Grammar {
        &self.grammar
    }

    pub fn intern_context(&self, ctx: D::Context) -> CtxId {
        self.contexts.intern(ctx)
    }

    pub fn context(&self, id: CtxId) -> Option<D::Context> {
        self.contexts.get(id)
    }

    pub fn intern_evidence(&self, ev: D::Evidence) -> EvidenceId {
        self.evidence.intern(ev)
    }

    pub fn evidence_of(&self, id: EvidenceId) -> Option<D::Evidence> {
        self.evidence.get(id)
    }

    pub fn intern_effect(&self, eff: D::Effect) -> EffectId {
        self.effects.intern(eff)
    }

    pub fn effect_of(&self, id: EffectId) -> Option<D::Effect> {
        self.effects.get(id)
    }

    pub fn interned_evidence_count(&self) -> usize { self.evidence.len() }
    pub fn interned_context_count(&self) -> usize  { self.contexts.len() }
    pub fn interned_effect_count(&self) -> usize   { self.effects.len() }

    fn rule_for_prod(&self, prod: ProdId) -> Option<&D::Rule> {
        let rule_name = self.grammar.rule_for_prod(prod)?;
        self.grammar.rules().get(rule_name.as_str()).and_then(|_| {
            // TODO (Phase 3): grammar will be SPG<D::Rule>; for now
            // this returns None until the grammar is genericized.
            None
        })
    }
}

// ── SemanticRuntime impl ──────────────────────────────────────────────────────

impl<D: ConstraintDomain> SemanticRuntime for DomainRuntime<D> {
    fn descend(
        &self,
        prod: ProdId,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &Obligations,
    ) -> Result<CtxId, TransitionError> {
        let Some(rule) = self.rule_for_prod(prod) else {
            return Ok(ctx);
        };
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let next = self.domain.descend(rule, binding, &ctx_val, obligations, &self.segs);
        Ok(self.intern_context(next))
    }

    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &Obligations,
        status: NodeStatus,
    ) -> Result<SemanticSummary, TransitionError> {
        let any_summary = SemanticSummary::new(ANY_TYPE, None, true);
        let Some(rule) = self.rule_for_prod(prod) else {
            return Ok(any_summary);
        };
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let (verdict, evidence, effect) =
            self.domain.finalize(rule, &ctx_val, obligations, &self.segs, status);
        match verdict {
            Verdict::Lost => Err(TransitionError::Rejected),
            Verdict::Live => Ok(SemanticSummary::new(
                self.intern_evidence(evidence),
                None,
                false,
            )),
            Verdict::Satisfied => Ok(SemanticSummary::new(
                self.intern_evidence(evidence),
                effect.map(|e| self.intern_effect(e)),
                true,
            )),
        }
    }

    fn set_segs(&mut self, s: &[Segment]) {
        self.segs = s.to_vec();
    }

    fn apply_effect(&self, ctx: CtxId, effect: EffectId) -> Result<CtxId, TransitionError> {
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let eff_val = self.effect_of(effect).ok_or(TransitionError::Rejected)?;
        let next = self.domain.apply_effect(ctx_val, &eff_val);
        Ok(self.intern_context(next))
    }

    fn compose_effects(&self, effects: Vec<EffectId>) -> Result<Option<EffectId>, TransitionError> {
        let vals: Result<Vec<D::Effect>, _> = effects
            .iter()
            .map(|&id| self.effect_of(id).ok_or(TransitionError::Rejected))
            .collect();
        let vals = vals?;
        let refs: Vec<&D::Effect> = vals.iter().collect();
        Ok(self.domain.compose_effects(&refs).map(|e| self.intern_effect(e)))
    }
}
