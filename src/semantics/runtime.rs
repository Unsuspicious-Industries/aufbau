//! Generic domain runtime — bridges `ConstraintDomain` to `SemanticRuntime`.

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use crate::engine::error::TransitionError;
use crate::engine::grammar::SPG;
use crate::engine::parse::arena::{CtxId, EffectId, EvidenceId, NodeStatus, ProdId, TOP};
use crate::engine::Segment;
use crate::semantics::domain::{ConstraintDomain, Verdict};
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::obligation::Obligations;
use crate::semantics::{SemanticRuntime, SemanticSummary};

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
}

// ── DomainRuntime ─────────────────────────────────────────────────────────────

/// Generic bridge: `ConstraintDomain` → `SemanticRuntime`.
pub struct DomainRuntime<D: ConstraintDomain> {
    domain: D,
    spg: SPG<D>,
    evidence: Rc<EvidenceStore<D::Evidence>>,
    contexts: Rc<Interner<D::Context>>,
    effects: Rc<Interner<D::Effect>>,
    segs: Vec<Segment>,
}

impl<D: ConstraintDomain + Clone> Clone for DomainRuntime<D> {
    fn clone(&self) -> Self {
        Self {
            domain: self.domain.clone(),
            spg: self.spg.clone(),
            evidence: Rc::clone(&self.evidence),
            contexts: Rc::clone(&self.contexts),
            effects: Rc::clone(&self.effects),
            segs: self.segs.clone(),
        }
    }
}

impl<D: ConstraintDomain> DomainRuntime<D> {
    pub fn new(domain: D, spg: SPG<D>) -> Self {
        let top = domain.top_evidence();
        let bot = domain.bottom_evidence();
        let evidence = Rc::new(EvidenceStore::new(top, bot));
        let rt = Self {
            domain,
            spg,
            evidence,
            contexts: Rc::new(Interner::new()),
            effects: Rc::new(Interner::new()),
            segs: Vec::new(),
        };
        let empty_ctx = rt.domain.empty_context();
        let _ = rt.contexts.intern(empty_ctx);
        rt
    }

    pub fn grammar(&self) -> &SPG<D> {
        &self.spg
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

    fn rule_for_prod(&self, prod: ProdId) -> Option<&D::Rule> {
        let rule_name = self.spg.nt(prod.0).and_then(|nt| self.spg.nt_rule(nt))?;
        self.spg.rules.get(rule_name.as_str())
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
        let next = self.domain.descend(
            rule,
            binding,
            &ctx_val,
            obligations,
            &self.segs,
            &self.evidence,
        );
        Ok(self.intern_context(next))
    }

    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &Obligations,
        status: NodeStatus,
    ) -> Result<SemanticSummary, TransitionError> {
        let any_summary = SemanticSummary::new(TOP, None, true);
        let Some(rule) = self.rule_for_prod(prod) else {
            return Ok(any_summary);
        };
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let (verdict, evidence, effect) = self.domain.finalize(
            rule,
            &ctx_val,
            obligations,
            &self.segs,
            status,
            &self.evidence,
        );
        match verdict {
            Verdict::Lost => Err(TransitionError::Rejected),
            Verdict::Live => {
                let id = self.evidence.intern(evidence);
                Ok(SemanticSummary::new(id, None, false))
            }
            Verdict::Satisfied => {
                let id = self.evidence.intern(evidence);
                Ok(SemanticSummary::new(
                    id,
                    effect.map(|e| self.intern_effect(e)),
                    true,
                ))
            }
        }
    }

    fn load_segs(&mut self, s: &[Segment]) {
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
        Ok(self
            .domain
            .compose_effects(&refs)
            .map(|e| self.intern_effect(e)))
    }
}
