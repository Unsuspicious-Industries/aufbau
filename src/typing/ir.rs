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
use std::fmt;

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
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub name: String,
    pub instrs: Vec<Instr>,
}

/// Lower a rule to its instruction stream. `trees` supplies the parsed tree for
/// each `TypeExpr` (the runtime precomputes it); a missing tree resolves to `⊤`.
#[must_use]
pub fn compile(rule: &TypingRule, trees: &Trees) -> Program {
    let mut c = Compiler {
        trees,
        instrs: Vec::new(),
        next: 0,
    };
    for premise in &rule.premises {
        c.premise(premise);
    }
    c.conclusion(&rule.conclusion);
    Program {
        name: rule.name.clone(),
        instrs: c.instrs,
    }
}

struct Compiler<'a> {
    trees: &'a Trees,
    instrs: Vec<Instr>,
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
        let setting_extends = p
            .setting
            .as_ref()
            .is_some_and(|s| !s.extensions.is_empty());
        // A setting extension is premise-local only when the premise also checks a
        // judgment; a setting-only premise propagates its extension forward.
        let scoped = setting_extends && p.judgment.is_some();
        if scoped {
            self.instrs.push(Instr::PushScope);
        }
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
                let r = self.eval(expr);
                self.instrs.push(Instr::Ascribe {
                    binding: term.clone(),
                    expected: r,
                });
            }
            Some(TypingJudgment::Membership(var, _)) => {
                self.instrs.push(Instr::Member {
                    binding: var.clone(),
                });
            }
            Some(TypingJudgment::Operation { left, right, .. }) => {
                let l = self.eval(left);
                let r = self.eval(right);
                self.instrs.push(Instr::Equate { left: l, right: r });
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
        rule.type_exprs()
            .into_iter()
            .filter_map(|te| TyExpr::build(g, te).ok().map(|ty| (te.clone(), ty)))
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
        assert!(matches!(&prog.instrs[0], Instr::Eval { expr: TyExpr::Con(label, _), .. } if label == "FunctionType"));
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
        let push = prog.instrs.iter().position(|i| *i == Instr::PushScope).unwrap();
        let pop = prog.instrs.iter().position(|i| *i == Instr::PopScope).unwrap();
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
        assert!(push < ascribe && ascribe < pop, "ascription is inside the scope");
        assert!(pop < emit, "conclusion is emitted after the scope closes");
    }

    #[test]
    fn var_lowers_to_member_and_ctx_emit() {
        let g = stlc();
        let prog = compile_src(&g, "x ∈ Γ", "Γ(x)", "var");
        assert!(prog.instrs.iter().any(|i| matches!(i, Instr::Member { binding } if binding == "x")));
        // The conclusion Γ(x) evaluates a context lookup and emits it.
        assert!(matches!(prog.instrs.last(), Some(Instr::Emit { .. })));
        assert!(prog.instrs.iter().any(|i| matches!(i, Instr::Eval { expr: TyExpr::Ctx(v), .. } if v == "x")));
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
}
