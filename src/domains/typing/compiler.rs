//! Meta compilation — §3 of the draft.
//!
//! Compiles `TypingRule` (+ `TypeExpr::Meta`) into `CompiledRule`
//! (fresh internal metas retained; resolved at evaluation time).

use super::rule::{
    Conclusion, ConclusionContext, Premise, TypeSetting, TypingJudgment, TypingRule,
};
use super::TypeExpr;
use std::collections::HashMap;

/// A compiled rule ready for evaluation.  Fresh internal Metas (`_0`, `_1`, …)
/// may remain; they are resolved at evaluation time by the meta-substitution
/// map in `apply_rule` via `bind_ascription_metas` and `bind_equality_metas`.
#[derive(Debug, Clone)]
pub struct CompiledRule {
    pub name: String,
    pub premises: Vec<Premise>,
    pub conclusion: Conclusion,
}

impl CompiledRule {
    #[must_use]
    pub fn new(name: String, premises: Vec<Premise>, conclusion: Conclusion) -> Self {
        Self {
            name,
            premises,
            conclusion,
        }
    }

    pub fn validate(&self) -> Result<(), String> {
        if self.conclusion.kind.has_metas() {
            return Err(format!("conclusion of {} has Metas", self.name));
        }
        for (i, p) in self.premises.iter().enumerate() {
            if let Some(j) = &p.judgment {
                Self::check_judgment_no_metas(j, &self.name, i)?;
            }
            if let Some(s) = &p.setting {
                for (name, ty) in &s.extensions {
                    if ty.has_metas() {
                        return Err(format!("setting {}.{} of {} has Metas", name, i, self.name));
                    }
                }
            }
        }
        Ok(())
    }

    fn check_judgment_no_metas(j: &TypingJudgment, rule: &str, i: usize) -> Result<(), String> {
        match j {
            TypingJudgment::Ascription((_, ty)) if ty.has_metas() => {
                Err(format!("ascription premise {i} of {rule} has Metas"))
            }
            TypingJudgment::Operation { left, right, .. } => {
                if left.has_metas() {
                    return Err(format!("operation left premise {i} of {rule} has Metas"));
                }
                if right.has_metas() {
                    return Err(format!("operation right premise {i} of {rule} has Metas"));
                }
                Ok(())
            }
            TypingJudgment::Equality { left, right } => {
                if left.has_metas() {
                    return Err(format!("equality left premise {i} of {rule} has Metas"));
                }
                if right.has_metas() {
                    return Err(format!("equality right premise {i} of {rule} has Metas"));
                }
                Ok(())
            }
            _ => Ok(()),
        }
    }
}

/// Compiles a `TypingRule` (with `TypeExpr::Meta`) into a `CompiledRule`.
///
/// Pipeline (draft §3 "Meta compilation pipeline", `sec:meta-compilation`):
///
/// **Phase 1 — Fresh meta generation.**
/// Each user-named `?A` → fresh `_k`.  Top-level ascriptions `b : ?A`
/// emit `typeof(b) = _k`.  Nested metas (inside arrows) skip the typeof
/// constraint — decomposition is deferred to evaluation.
///
/// **Phase 2 — Shared-meta equality.**
/// Same-named metas at distinct positions link their fresh metas: `_i = _j`.
#[must_use = "discarding compile errors silently masks ill-formed rules"]
pub fn compile_rule(rule: &TypingRule) -> Result<CompiledRule, String> {
    if !has_metas(rule) {
        return Ok(CompiledRule {
            name: rule.name.clone(),
            premises: rule.premises.clone(),
            conclusion: rule.conclusion.clone(),
        });
    }
    let compiler = MetaCompiler::new(rule);
    let (premises, conclusion) = compiler.compile()?;
    Ok(CompiledRule::new(rule.name.clone(), premises, conclusion))
}

pub(crate) fn has_metas(rule: &TypingRule) -> bool {
    rule.conclusion.kind.has_metas() || rule.premises.iter().any(super::rule::Premise::has_metas)
}

struct MetaCompiler<'a> {
    rule: &'a TypingRule,
    meta_bindings: HashMap<String, TypeExpr>,
    equalities: Vec<(TypeExpr, TypeExpr)>,
    fresh_counter: usize,
}

impl<'a> MetaCompiler<'a> {
    fn new(rule: &'a TypingRule) -> Self {
        Self {
            rule,
            meta_bindings: HashMap::new(),
            equalities: Vec::new(),
            fresh_counter: 0,
        }
    }

    fn compile(mut self) -> Result<(Vec<Premise>, Conclusion), String> {
        let mut premises: Vec<Premise> = Vec::new();
        for premise in &self.rule.premises {
            premises.push(self.compile_premise(premise)?);
        }
        for (left, right) in &self.equalities {
            premises.push(Premise {
                setting: None,
                judgment: Some(TypingJudgment::Equality {
                    left: left.clone(),
                    right: right.clone(),
                }),
            });
        }
        let conclusion = self.compile_conclusion(&self.rule.conclusion)?;
        Ok((premises, conclusion))
    }

    fn compile_premise(&mut self, premise: &Premise) -> Result<Premise, String> {
        let setting = premise.setting.as_ref().map(|s| {
            let exts: Vec<(String, TypeExpr)> = s
                .extensions
                .iter()
                .map(|(n, ty)| (n.clone(), self.compile_expression(ty, n)))
                .collect();
            TypeSetting {
                name: s.name.clone(),
                extensions: exts,
                no_propagate: s.no_propagate,
            }
        });
        let judgment = premise
            .judgment
            .as_ref()
            .map(|j| self.compile_judgment(j))
            .transpose()?;
        Ok(Premise { setting, judgment })
    }

    fn compile_judgment(&mut self, judgment: &TypingJudgment) -> Result<TypingJudgment, String> {
        match judgment {
            TypingJudgment::Ascription((term, ty)) => {
                let compiled_ty = self.compile_expression(ty, term);
                Ok(TypingJudgment::Ascription((term.clone(), compiled_ty)))
            }
            TypingJudgment::Membership(v, c) => {
                Ok(TypingJudgment::Membership(v.clone(), c.clone()))
            }
            TypingJudgment::Operation { left, op, right } => Ok(TypingJudgment::Operation {
                left: self.compile_expression(left, ""),
                op: op.clone(),
                right: self.compile_expression(right, ""),
            }),
            TypingJudgment::Equality { left, right } => Ok(TypingJudgment::Equality {
                left: self.compile_expression(left, ""),
                right: self.compile_expression(right, ""),
            }),
        }
    }

    fn compile_expression(&mut self, ty: &TypeExpr, binding: &str) -> TypeExpr {
        match ty {
            TypeExpr::Arrow(domain, codomain) => TypeExpr::Arrow(
                Box::new(self.compile_expression(domain, "")),
                Box::new(self.compile_expression(codomain, "")),
            ),
            TypeExpr::Meta(name) => {
                let fresh = self.fresh_meta_name();
                let fresh_ty = TypeExpr::Meta(fresh);
                if let Some(existing) = self.meta_bindings.get(name) {
                    self.equalities.push((existing.clone(), fresh_ty.clone()));
                } else {
                    self.meta_bindings.insert(name.clone(), fresh_ty.clone());
                }
                if !binding.is_empty() {
                    self.equalities
                        .push((TypeExpr::TypeOf(binding.to_string()), fresh_ty.clone()));
                }
                fresh_ty
            }
            other => other.clone(),
        }
    }

    fn compile_conclusion(&mut self, conclusion: &Conclusion) -> Result<Conclusion, String> {
        let output = conclusion.context.output.as_ref().map(|out| {
            let exts: Vec<(String, TypeExpr)> = out
                .extensions
                .iter()
                .map(|(n, ty)| (n.clone(), self.compile_expression(ty, n)))
                .collect();
            TypeSetting {
                name: out.name.clone(),
                extensions: exts,
                no_propagate: out.no_propagate,
            }
        });
        let ctx = ConclusionContext {
            input: conclusion.context.input.clone(),
            output,
        };
        let kind = self.compile_expression(&conclusion.kind, "");
        Ok(Conclusion { context: ctx, kind })
    }

    fn fresh_meta_name(&mut self) -> String {
        let n = self.fresh_counter;
        self.fresh_counter += 1;
        format!("_{n}")
    }
}

impl crate::semantics::domain::HasBindings for TypingRule {
    fn referenced_bindings(&self) -> Box<dyn Iterator<Item = &str> + '_> {
        Box::new(self.used_bindings().into_iter())
    }
}

impl Premise {
    fn has_metas(&self) -> bool {
        self.setting
            .as_ref()
            .is_some_and(|s| s.extensions.iter().any(|(_, ty)| ty.has_metas()))
            || self
                .judgment
                .as_ref()
                .is_some_and(super::rule::TypingJudgment::has_metas)
    }
}

impl TypingJudgment {
    fn has_metas(&self) -> bool {
        match self {
            TypingJudgment::Ascription((_, ty)) => ty.has_metas(),
            TypingJudgment::Operation { left, right, .. } => left.has_metas() || right.has_metas(),
            TypingJudgment::Equality { left, right } => left.has_metas() || right.has_metas(),
            TypingJudgment::Membership(_, _) => false,
        }
    }
}
