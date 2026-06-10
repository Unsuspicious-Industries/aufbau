//! Rule IR — §2. A typing rule is surface sugar; this is what it compiles to.
//!
//! [`compile`] lowers a [`TypingRule`] to a flat instruction stream. Each
//! instruction is one call to a `domain` primitive (`eval_ty`, `unify_modulo`,
//! context ops), so the IR adds no logic of its own — it only fixes the *schedule*
//! of those calls. The control flow the tree-walk did implicitly (premise-local
//! context scoping) is made explicit here as `PushScope`/`PopScope`, so the
//! executor is a flat fold and the compiler holds the structure once.
//!
//! Execution (the fold over these instructions, threading substitution, context,
//! and a three-valued verdict) replaces `domain::eval_rule`; until that swap lands
//! behind an equivalence check, this module is the inspectable compiled form.

use crate::typing::domain::Trees;
use crate::typing::rule::{Conclusion, Premise, TypingJudgment, TypingRule};
use crate::typing::{TyExpr, TypeExpr};
use std::collections::HashMap;
use std::fmt;
use std::ops::Range;

/// A virtual register holding an evaluated [`Term`](crate::typing::Term).
pub type Reg = usize;

/// One lowered step of a typing rule. Each maps to a single domain primitive.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instr {
    /// `r := <type-expr>` — resolve a type expression to a term (`eval_ty`).
    Eval { dst: Reg, expr: TyExpr },
    /// `ascribe b : r` — unify the bound child `b`'s type with register `r`
    /// (`unify_modulo`); the binding carries the openness for the verdict.
    Ascribe { binding: String, expected: Reg },
    /// `equate ra = rb` — a type operation; unify two evaluated terms, hard-fail.
    Equate { left: Reg, right: Reg },
    /// `member b` — context membership of binding `b`'s value.
    Member { binding: String },
    /// Begin a premise-local context scope (a setting extension that must not leak).
    PushScope,
    /// End the innermost context scope.
    PopScope,
    /// `extend b := r` — bind `b`'s value to register `r` in the current scope.
    Extend { binding: String, ty: Reg },
    /// `emit r` — the conclusion type.
    Emit { ty: Reg },
    /// `effect b := r` — a context transition exported to siblings.
    Effect { binding: String, ty: Reg },
}

/// A compiled typing rule: its name and instruction stream.
///
/// The `splices` map is the *structural decomposition* of the program into
/// per-premise spans, computed once at compile time. A rule has at most one
/// premise per binding, so `splices` is a partial function from binding name
/// to instruction range. The IR is well-bracketed by construction: every
/// premise with a non-empty setting is bracketed by a balanced
/// `PushScope`/`PopScope` pair, so the span of each premise is unambiguous.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub name: String,
    pub instrs: Vec<Instr>,
    pub splices: HashMap<String, Range<usize>>,
}

impl Program {
    /// The slice of instructions belonging to the premise whose ascription
    /// binds `b`: from the premise's opening `PushScope` through the `Ascribe`
    /// (inclusive). `None` when no premise ascribes `b` — typically because
    /// the rule's only premise for that name is a `Member` or `Operation`.
    /// Resolution failures inside the splice are reported by the executor.
    #[must_use]
    pub fn splice(&self, b: &str) -> Option<&[Instr]> {
        self.splices.get(b).map(|r| &self.instrs[r.clone()])
    }
}

/// Lower a rule to its instruction stream. `trees` supplies the parsed tree for
/// each `TypeExpr` (the runtime precomputes it); a missing tree resolves to `⊤`.
#[must_use]
pub fn compile(rule: &TypingRule, trees: &Trees) -> Program {
    let mut c = Compiler {
        trees,
        instrs: Vec::new(),
        splices: HashMap::new(),
        next: 0,
    };
    for premise in &rule.premises {
        c.premise(premise);
    }
    c.conclusion(&rule.conclusion);
    Program {
        name: rule.name.clone(),
        instrs: c.instrs,
        splices: c.splices,
    }
}

struct Compiler<'a> {
    trees: &'a Trees,
    instrs: Vec<Instr>,
    splices: HashMap<String, Range<usize>>,
    next: Reg,
}

impl Compiler<'_> {
    fn fresh(&mut self) -> Reg {
        let r = self.next;
        self.next += 1;
        r
    }

    /// Emit `r := expr` and return the destination register.
    fn eval(&mut self, expr: &TypeExpr) -> Reg {
        let ty = self.trees.get(expr).cloned().unwrap_or(TyExpr::Top);
        let dst = self.fresh();
        self.instrs.push(Instr::Eval { dst, expr: ty });
        dst
    }

    fn premise(&mut self, p: &Premise) {
        let setting_extends = p.setting.as_ref().is_some_and(|s| !s.extensions.is_empty());
        // A setting extension is premise-local only when the premise also checks a
        // judgment; a setting-only premise propagates its extension forward.
        let scoped = setting_extends && p.judgment.is_some();
        if scoped {
            self.instrs.push(Instr::PushScope);
        }
        // The start of the setting-extension span. A setting `Γ[a:τ]` is
        // premise-local: it applies only to the descent into *this premise's
        // term* (the ascription subject or membership variable), never to the
        // descent into the binder `a` itself — at the point the parser enters
        // `a`'s provider node, neither `a` nor `τ` is resolved, so evaluating the
        // extension there would spuriously fail and drop the prediction. So we do
        // not key a splice by an extension's binder name; the only splice is the
        // premise term's, recorded below.
        let setting_start = self.instrs.len();
        if let Some(setting) = &p.setting {
            for (name, ext) in &setting.extensions {
                let r = self.eval(ext);
                self.instrs.push(Instr::Extend {
                    binding: name.clone(),
                    ty: r,
                });
            }
        }
        match &p.judgment {
            Some(TypingJudgment::Ascription((term, expr))) => {
                let ascribe_eval_start = self.instrs.len();
                let r = self.eval(expr);
                // The splice is the settings span (which may be empty). The
                // ascription's type eval and the ascription itself are not
                // part of the descent — they are the rule's check, applied
                // during `finalize`.
                self.splices
                    .insert(term.clone(), setting_start..ascribe_eval_start);
                self.instrs.push(Instr::Ascribe {
                    binding: term.clone(),
                    expected: r,
                });
            }
            Some(TypingJudgment::Membership(var, _)) => {
                let member_start = self.instrs.len();
                self.instrs.push(Instr::Member {
                    binding: var.clone(),
                });
                self.splices
                    .insert(var.clone(), setting_start..member_start);
            }
            Some(TypingJudgment::Operation { left, right, .. }) => {
                let l = self.eval(left);
                let r = self.eval(right);
                self.instrs.push(Instr::Equate { left: l, right: r });
                // Equate has no single binding; nothing to record.
            }
            None => {}
        }
        if scoped {
            self.instrs.push(Instr::PopScope);
        }
    }

    fn conclusion(&mut self, c: &Conclusion) {
        if let Some(out) = &c.context.output {
            for (var, ty) in &out.extensions {
                let r = self.eval(ty);
                self.instrs.push(Instr::Effect {
                    binding: var.clone(),
                    ty: r,
                });
            }
        }
        let r = self.eval(&c.kind);
        self.instrs.push(Instr::Emit { ty: r });
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Eval { dst, expr } => write!(f, "r{dst} = {expr}"),
            Instr::Ascribe { binding, expected } => write!(f, "ascribe {binding} : r{expected}"),
            Instr::Equate { left, right } => write!(f, "equate r{left} = r{right}"),
            Instr::Member { binding } => write!(f, "member {binding}"),
            Instr::PushScope => write!(f, "push_scope"),
            Instr::PopScope => write!(f, "pop_scope"),
            Instr::Extend { binding, ty } => write!(f, "extend {binding} := r{ty}"),
            Instr::Emit { ty } => write!(f, "emit r{ty}"),
            Instr::Effect { binding, ty } => write!(f, "effect {binding} := r{ty}"),
        }
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{}:", self.name)?;
        let mut indent = 1usize;
        for instr in &self.instrs {
            if matches!(instr, Instr::PopScope) {
                indent = indent.saturating_sub(1);
            }
            writeln!(f, "{}{instr}", "  ".repeat(indent))?;
            if matches!(instr, Instr::PushScope) {
                indent += 1;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine::grammar::SPG;
    use crate::typing::TypingRule;

    fn stlc() -> SPG {
        SPG::load(include_str!("../../examples/stlc.auf")).unwrap()
    }

    fn trees(g: &SPG, rule: &TypingRule) -> Trees {
        let bindings = g.rule_bindings(&rule.name);
        rule.type_exprs()
            .into_iter()
            .filter_map(|te| TyExpr::build(g, te, &bindings).ok().map(|ty| (te.clone(), ty)))
            .collect()
    }

    fn compile_src(g: &SPG, premises: &str, conclusion: &str, name: &str) -> Program {
        let rule = TypingRule::new(premises.into(), conclusion.into(), name.into()).unwrap();
        compile(&rule, &trees(g, &rule))
    }

    #[test]
    fn app_lowers_to_two_ascriptions_and_an_emit() {
        let g = stlc();
        let prog = compile_src(&g, "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A", "?B", "app");
        let kinds: Vec<_> = prog
            .instrs
            .iter()
            .map(|i| match i {
                Instr::Eval { .. } => "eval",
                Instr::Ascribe { .. } => "ascribe",
                Instr::Emit { .. } => "emit",
                _ => "other",
            })
            .collect();
        assert_eq!(
            kinds,
            vec!["eval", "ascribe", "eval", "ascribe", "eval", "emit"]
        );
        // The first ascription unifies l against the arrow constructor.
        assert!(
            matches!(&prog.instrs[0], Instr::Eval { expr: TyExpr::Con(label, _), .. } if label == "FunctionType")
        );
        assert!(matches!(&prog.instrs[1], Instr::Ascribe { binding, .. } if binding == "l"));
    }

    #[test]
    fn lambda_scopes_its_context_extension() {
        let g = stlc();
        let prog = compile_src(&g, "Γ[a:τ] ⊢ e : ?B", "τ -> ?B", "lambda");
        // The premise extends Γ with a:τ inside a scope that is popped before the
        // conclusion, so the extension does not leak.
        assert!(prog.instrs.contains(&Instr::PushScope));
        assert!(prog.instrs.contains(&Instr::PopScope));
        let push = prog
            .instrs
            .iter()
            .position(|i| *i == Instr::PushScope)
            .unwrap();
        let pop = prog
            .instrs
            .iter()
            .position(|i| *i == Instr::PopScope)
            .unwrap();
        let ascribe = prog
            .instrs
            .iter()
            .position(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "e"))
            .unwrap();
        let emit = prog
            .instrs
            .iter()
            .position(|i| matches!(i, Instr::Emit { .. }))
            .unwrap();
        assert!(
            push < ascribe && ascribe < pop,
            "ascription is inside the scope"
        );
        assert!(pop < emit, "conclusion is emitted after the scope closes");
    }

    #[test]
    fn var_lowers_to_member_and_ctx_emit() {
        let g = stlc();
        let prog = compile_src(&g, "x ∈ Γ", "Γ(x)", "var");
        assert!(prog
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::Member { binding } if binding == "x")));
        // The conclusion Γ(x) evaluates a context lookup and emits it.
        assert!(matches!(prog.instrs.last(), Some(Instr::Emit { .. })));
        assert!(prog
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::Eval { expr: TyExpr::Ctx(v), .. } if v == "x")));
    }

    #[test]
    fn display_is_readable() {
        let g = stlc();
        let prog = compile_src(&g, "Γ[a:τ] ⊢ e : ?B", "τ -> ?B", "lambda");
        let s = prog.to_string();
        assert!(s.starts_with("lambda:\n"));
        assert!(s.contains("push_scope"));
        assert!(s.contains("ascribe e : r"));
    }

    #[test]
    fn splice_for_app_separates_l_and_r() {
        let g = stlc();
        let prog = compile_src(&g, "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A", "?B", "app");
        // Both premises carry no setting, so each splice is empty.
        let l = prog.splice("l").unwrap();
        let r = prog.splice("r").unwrap();
        assert!(l.is_empty(), "expected empty splice for `l`, got {l:?}");
        assert!(r.is_empty(), "expected empty splice for `r`, got {r:?}");
        // The ascriptions themselves remain in the program — they are not in
        // either splice, since the splice is for context, not the check.
        assert!(prog
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "l")));
        assert!(prog
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "r")));
    }

    #[test]
    fn setting_is_premise_local_across_siblings() {
        let g = stlc();
        // First premise checks `l` under Γ[a:τ]; the second checks `r` under bare
        // Γ. A setting is premise-local: `a` is in scope for `l`'s descent only,
        // not for `r`'s. (Sequential context threading between siblings is the
        // job of conclusion effects, not settings.)
        let prog = compile_src(&g, "Γ[a:τ] ⊢ l : ?A -> ?B, Γ ⊢ r : ?A", "?B", "app_set");
        let l_splice = prog.splice("l").unwrap();
        assert!(
            l_splice
                .iter()
                .any(|i| matches!(i, Instr::Extend { binding, .. } if binding == "a")),
            "splice for `l` should include setting extension `a`, got {l_splice:?}"
        );
        let r_splice = prog.splice("r").unwrap();
        assert!(
            r_splice
                .iter()
                .all(|i| !matches!(i, Instr::Extend { binding, .. } if binding == "a")),
            "splice for `r` must not include sibling setting `a`, got {r_splice:?}"
        );
        // The binder `a` itself is not a descent target.
        assert!(prog.splice("a").is_none());
    }

    #[test]
    fn splice_for_lambda_includes_setting_not_ascribe() {
        let g = stlc();
        let prog = compile_src(&g, "Γ[a:τ] ⊢ e : ?B", "τ -> ?B", "lambda");
        // A descent into `e` needs the premise's setting extensions (so `a:τ`
        // is in scope) but not the ascription itself (the rule's check).
        let s_e = prog.splice("e").unwrap();
        assert!(s_e
            .iter()
            .any(|i| matches!(i, Instr::Extend { binding, .. } if binding == "a")));
        assert!(s_e
            .iter()
            .all(|i| !matches!(i, Instr::Ascribe { binding, .. } if binding == "e")));
        assert!(s_e.iter().all(|i| !matches!(i, Instr::PushScope)));
        assert!(s_e.iter().all(|i| !matches!(i, Instr::PopScope)));
        // The setting binder `a` is not a descent target of its own extension:
        // entering `a`'s provider node happens before `a` (and `τ`) resolve, so
        // there is no splice keyed by the binder name.
        assert!(prog.splice("a").is_none());
        // The ascription of `e` is in the program but not in its splice.
        assert!(prog
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "e")));
    }

    #[test]
    fn premise_term_splice_covers_all_its_settings() {
        let g = stlc();
        // Two settings in one premise (the syntax is bracket-per-extension,
        // `Γ[a:τ][b:σ]`), then an ascription of `e`. The descent into `e` must
        // apply both extensions, in order; the binders themselves are not
        // descent targets.
        let prog = compile_src(&g, "Γ[a:τ][b:σ] ⊢ e : ?A", "?A", "two_set");
        let s_e = prog.splice("e").unwrap();
        assert_eq!(
            s_e.iter()
                .filter(|i| matches!(i, Instr::Extend { .. }))
                .count(),
            2,
            "e's descent applies both settings, got {s_e:?}"
        );
        assert!(prog.splice("a").is_none());
        assert!(prog.splice("b").is_none());
        // The ascription is in the program but not in the splice.
        assert!(prog
            .instrs
            .iter()
            .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "e")));
        assert!(s_e.iter().all(|i| !matches!(i, Instr::Ascribe { .. })));
        assert!(s_e.iter().all(|i| !matches!(i, Instr::PushScope)));
        assert!(s_e.iter().all(|i| !matches!(i, Instr::PopScope)));
    }

    #[test]
    fn splice_for_var_is_setting_only() {
        let g = stlc();
        let prog = compile_src(&g, "x ∈ Γ", "Γ(x)", "var");
        // Member has a splice covering any (here: no) setting extensions; the
        // Member instruction itself is irrelevant to a descent.
        let s = prog.splice("x").unwrap();
        assert!(s.iter().all(|i| !matches!(i, Instr::Member { .. })));
    }
}
