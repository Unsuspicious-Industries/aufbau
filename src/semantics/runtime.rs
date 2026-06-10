//! Concrete typing runtime.
//!
//! Interns the typing domain's value-level `Type`/`Context`/`ContextTransition`
//! into arena ids and exposes the id-based hooks the prefix parser calls:
//! `descend`, `finalize`, `apply_effect`, `compose_effects`.

use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::Hash;
use std::rc::Rc;

use crate::engine::error::TransitionError;
use crate::engine::grammar::SPG;
use crate::engine::parse::arena::{CtxId, EffectId, EvidenceId, Lexeme, NodeStatus, ProdId, Span};
use crate::engine::Segment;
use crate::semantics::domain::Verdict;
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::obligation::Obligations;
use crate::semantics::SemanticSummary;
use crate::typing::ir::{compile, Program};
use crate::typing::pattern::Pattern;
use crate::typing::{Context, ContextTransition, Normalizer, Term, Type, TypingDomain};

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

// ── TypingRuntime ─────────────────────────────────────────────────────────────

/// Interning bridge between the typing domain's value-level evaluation and the
/// parser's id-based interface.
pub struct TypingRuntime {
    domain: TypingDomain<false>,
    spg: SPG,
    /// Each rule lowered to its IR `Program` once, keyed by rule name. The
    /// parser's hot path (`descend`/`finalize`) looks up here instead of
    /// recompiling per node.
    programs: Rc<HashMap<String, Program>>,
    /// The grammar's type-rewrite theory; empty ⇒ `normalize` is the identity.
    norm: Rc<Normalizer>,
    evidence: Rc<EvidenceStore<Type>>,
    contexts: Rc<Interner<Context>>,
    effects: Rc<Interner<ContextTransition>>,
    segs: Vec<Segment>,
}

impl std::fmt::Debug for TypingRuntime {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TypingRuntime").finish_non_exhaustive()
    }
}

impl Clone for TypingRuntime {
    fn clone(&self) -> Self {
        Self {
            domain: self.domain.clone(),
            spg: self.spg.clone(),
            programs: Rc::clone(&self.programs),
            norm: Rc::clone(&self.norm),
            evidence: Rc::clone(&self.evidence),
            contexts: Rc::clone(&self.contexts),
            effects: Rc::clone(&self.effects),
            segs: self.segs.clone(),
        }
    }
}

impl TypingRuntime {
    pub fn new(domain: TypingDomain<false>, spg: SPG) -> Self {
        let evidence = Rc::new(EvidenceStore::new(
            TypingDomain::<false>::top_evidence(),
            TypingDomain::<false>::bottom_evidence(),
        ));
        let trees = spg.type_trees();
        let norm = spg.normalizer();
        let programs = spg
            .rules
            .iter()
            .map(|(name, rule)| (name.clone(), compile(rule, &trees)))
            .collect();
        let rt = Self {
            domain,
            spg,
            programs: Rc::new(programs),
            norm: Rc::new(norm),
            evidence,
            contexts: Rc::new(Interner::new()),
            effects: Rc::new(Interner::new()),
            segs: Vec::new(),
        };
        let _ = rt.contexts.intern(Context::new());
        rt
    }

    /// Structural evidence for a rule-less node: its own parse tree. A node with
    /// child nodes is a `Con` over their evidence; a bare span is a `Leaf` of its
    /// text. This is how syntax and typing share one object (evidence is a tree).
    pub fn structural_evidence(
        &self,
        nt: &str,
        child_evidence: &[EvidenceId],
        span: Span,
    ) -> EvidenceId {
        let term = if child_evidence.is_empty() {
            Term::Leaf(Pattern::raw(self.render(span)))
        } else {
            let kids = child_evidence
                .iter()
                .filter_map(|&id| self.evidence.get(id))
                .collect();
            Term::Con(nt.to_string(), kids)
        };
        self.evidence.intern(term)
    }

    fn render(&self, span: Span) -> String {
        Lexeme::new(span, true, false)
            .value(&self.segs)
            .unwrap_or_default()
    }

    pub fn grammar(&self) -> &SPG {
        &self.spg
    }

    pub fn intern_context(&self, ctx: Context) -> CtxId {
        self.contexts.intern(ctx)
    }

    pub fn context(&self, id: CtxId) -> Option<Context> {
        self.contexts.get(id)
    }

    pub fn intern_evidence(&self, ev: Type) -> EvidenceId {
        self.evidence.intern(ev)
    }

    pub fn evidence_of(&self, id: EvidenceId) -> Option<Type> {
        self.evidence.get(id)
    }

    pub fn intern_effect(&self, eff: ContextTransition) -> EffectId {
        self.effects.intern(eff)
    }

    pub fn effect_of(&self, id: EffectId) -> Option<ContextTransition> {
        self.effects.get(id)
    }

    /// The precompiled IR program for `prod`'s rule, if it has one.
    fn program_for_prod(&self, prod: ProdId) -> Option<&Program> {
        let rule_name = self.spg.nt(prod.0).and_then(|nt| self.spg.nt_rule(nt))?;
        self.programs.get(rule_name.as_str())
    }

    // ── Parser-facing hooks ───────────────────────────────────────────────────

    /// Context selected before entering the child at the current dot.
    pub fn descend(
        &self,
        prod: ProdId,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &Obligations,
    ) -> Result<CtxId, TransitionError> {
        let Some(program) = self.program_for_prod(prod) else {
            return Ok(ctx);
        };
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let next = self.domain.descend(
            program,
            &self.norm,
            binding,
            &ctx_val,
            obligations,
            &self.segs,
            &self.evidence,
        )?;
        Ok(self.intern_context(next))
    }

    /// Evaluate the typing rule of a closed or prefix parser item. `None` means
    /// the production has no rule, so the domain says nothing and the node's
    /// evidence is decided structurally (parser elaboration). A `Lost` verdict
    /// rejects the item.
    pub fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &Obligations,
        status: NodeStatus,
    ) -> Result<Option<SemanticSummary>, TransitionError> {
        let Some(program) = self.program_for_prod(prod) else {
            return Ok(None);
        };
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let (verdict, evidence, effect) = self.domain.finalize(
            program,
            &self.norm,
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
                Ok(Some(SemanticSummary::new(id, None, false)))
            }
            Verdict::Satisfied => {
                let id = self.evidence.intern(evidence);
                Ok(Some(SemanticSummary::new(
                    id,
                    effect.map(|e| self.intern_effect(e)),
                    true,
                )))
            }
        }
    }

    /// Update the input segmentation visible to evidence lexemes.
    pub fn load_segs(&mut self, s: &[Segment]) {
        self.segs = s.to_vec();
    }

    /// Apply a right-bound effect exported by an exact left sibling.
    pub fn apply_effect(&self, ctx: CtxId, effect: EffectId) -> Result<CtxId, TransitionError> {
        let ctx_val = self.context(ctx).ok_or(TransitionError::Rejected)?;
        let eff_val = self.effect_of(effect).ok_or(TransitionError::Rejected)?;
        let next = self.domain.apply_effect(ctx_val, &eff_val);
        Ok(self.intern_context(next))
    }

    /// Compose exact child effects left-to-right for transparent productions.
    pub fn compose_effects(
        &self,
        effects: Vec<EffectId>,
    ) -> Result<Option<EffectId>, TransitionError> {
        let vals: Result<Vec<ContextTransition>, _> = effects
            .iter()
            .map(|&id| self.effect_of(id).ok_or(TransitionError::Rejected))
            .collect();
        let vals = vals?;
        let refs: Vec<&ContextTransition> = vals.iter().collect();
        Ok(self
            .domain
            .compose_effects(&refs)
            .map(|e| self.intern_effect(e)))
    }
}
