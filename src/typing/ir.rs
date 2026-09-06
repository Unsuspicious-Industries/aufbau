//! Rule IR — §2. A typing rule is surface sugar; this is what it compiles to.
//!
//! [`compile`] lowers a [`TypingRule`] to a flat instruction stream. Each
//! instruction is one call to a `domain` primitive (`eval_ty`, `unify_modulo`,
//! context ops), so at compile time the IR adds no logic of its own: it fixes the
//! *schedule* of those calls. Premise-local context scoping, implicit in the old
//! tree-walk, is explicit here as `PushScope`/`PopScope`, so the executor is a flat
//! fold and the compiler holds the structure once.
//!
//! `compile` is the cut, and safety lives *before* it. Constraints are introduced
//! by the grammar and the surface rule; a new safety check belongs there, not in the
//! executor. Downstream only discharges: `domain`'s `run` folds the stream to a
//! [`RuleResult`], `descend` replays the prefix before a binding's splice. `descend`
//! adds semantics of its own (replay, splice application), but neither it nor `run`
//! can invent an obligation the `Program` does not carry; they satisfy, leave
//! `Unknown`, or prune. So a `Program` is the stable artifact between the halves,
//! and a new backend consumes it rather than changing the engine.
//! See `docs/architecture.md`.
//!
//! [`RuleResult`]: crate::typing::rule::RuleResult

use crate::typing::domain::Trees;
use crate::typing::rule::{Conclusion, Judgment, Premise, TypingRule};
use crate::typing::{Key, TyExpr, TypeExpr};
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
    /// `member k` — context membership of key `k` (a binding's value, or a
    /// fixed name).
    Member { key: Key },
    /// `fresh k` — the complete binding name must not already be in Γ.
    Fresh { key: Key },
    /// Begin a premise-local context scope (a setting extension that must not leak).
    PushScope,
    /// End the innermost context scope.
    PopScope,
    /// `extend k := r` — bind key `k` to register `r` in the current scope.
    Extend { key: Key, ty: Reg },
    /// `emit r` — the conclusion type.
    Emit { ty: Reg },
    /// `effect k := r` — a context transition exported to siblings.
    Effect { key: Key, ty: Reg },
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
    /// the rule's only premise for that name is a `Member` or `Equation`.
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
        let scoped = !p.extensions.is_empty();
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
        for (key, ext) in &p.extensions {
            let r = self.eval(ext);
            self.instrs.push(Instr::Extend {
                key: key.clone(),
                ty: r,
            });
        }
        match &p.judgment {
            Judgment::Ascription { binding, ty } => {
                let ascribe_eval_start = self.instrs.len();
                let r = self.eval(ty);
                // The splice is the settings span (which may be empty). The
                // ascription's type eval and the ascription itself are not
                // part of the descent — they are the rule's check, applied
                // during `finalize`.
                self.splices
                    .insert(binding.clone(), setting_start..ascribe_eval_start);
                self.instrs.push(Instr::Ascribe {
                    binding: binding.clone(),
                    expected: r,
                });
            }
            Judgment::Membership { key } => {
                let member_start = self.instrs.len();
                self.instrs.push(Instr::Member { key: key.clone() });
                // A splice is a descent target, and only a binding names a
                // child to descend into. A literal-keyed membership asks about
                // the ambient context, so it has no subtree and no splice.
                if let Some(b) = key.binding() {
                    self.splices
                        .insert(b.to_string(), setting_start..member_start);
                }
            }
            Judgment::Freshness { key } => {
                let start = self.instrs.len();
                self.instrs.push(Instr::Fresh { key: key.clone() });
                if let Some(b) = key.binding() {
                    self.splices.insert(b.to_string(), setting_start..start);
                }
            }
            Judgment::Equation { left, right } => {
                let l = self.eval(left);
                let r = self.eval(right);
                self.instrs.push(Instr::Equate { left: l, right: r });
                // Equate has no single binding; nothing to record.
            }
        }
        if scoped {
            self.instrs.push(Instr::PopScope);
        }
    }

    fn conclusion(&mut self, c: &Conclusion) {
        for (key, ty) in &c.effects {
            let r = self.eval(ty);
            self.instrs.push(Instr::Effect {
                key: key.clone(),
                ty: r,
            });
        }
        let r = self.eval(&c.ty);
        self.instrs.push(Instr::Emit { ty: r });
    }
}

impl fmt::Display for Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instr::Eval { dst, expr } => write!(f, "r{dst} = {expr}"),
            Instr::Ascribe { binding, expected } => write!(f, "ascribe {binding} : r{expected}"),
            Instr::Equate { left, right } => write!(f, "equate r{left} = r{right}"),
            Instr::Member { key } => write!(f, "member {key}"),
            Instr::Fresh { key } => write!(f, "fresh {key}"),
            Instr::PushScope => write!(f, "push_scope"),
            Instr::PopScope => write!(f, "pop_scope"),
            Instr::Extend { key, ty } => write!(f, "extend {key} := r{ty}"),
            Instr::Emit { ty } => write!(f, "emit r{ty}"),
            Instr::Effect { key, ty } => write!(f, "effect {key} := r{ty}"),
        }
    }
}

impl fmt::Display for Program {
    /// The whole program, including `splices`. A `Program` has exactly two
    /// fields beyond its name, and both are printed: the debug view must not
    /// hide state the executor reads.
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
        let mut spliced: Vec<_> = self.splices.iter().collect();
        spliced.sort_by(|a, b| a.0.cmp(b.0));
        for (binding, r) in spliced {
            writeln!(f, "  splice {binding} = [{}..{}]", r.start, r.end)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::SPG;
    use crate::typing::TypingRule;

    fn stlc() -> SPG {
        SPG::load(include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/stlc.auf"
        )))
        .unwrap()
    }

    fn trees(g: &SPG, rule: &TypingRule) -> Trees {
        let bindings = g.rule_bindings(&rule.name);
        rule.type_exprs()
            .into_iter()
            .filter_map(|te| {
                TyExpr::build(g, te, &bindings)
                    .ok()
                    .map(|ty| (te.clone(), ty))
            })
            .collect()
    }

    fn compile_src(g: &SPG, premises: &str, conclusion: &str, name: &str) -> Program {
        let rule = TypingRule::new(premises.into(), conclusion.into(), name.into()).unwrap();
        compile(&rule, &trees(g, &rule))
    }

    /// Every `.auf` under `examples/`, discovered at run time rather than listed.
    /// A hand-maintained list would let a newly added grammar escape the freeze,
    /// and these tests exist precisely to hold for grammars nobody wrote them for.
    fn example_sources() -> Vec<(String, String)> {
        let dir = concat!(env!("CARGO_MANIFEST_DIR"), "/examples");
        let mut out: Vec<(String, String)> = std::fs::read_dir(dir)
            .unwrap_or_else(|e| panic!("{dir}: {e}"))
            .map(|e| e.unwrap().path())
            .filter(|p| p.extension().is_some_and(|x| x == "auf"))
            .map(|p| {
                let name = p.file_name().unwrap().to_string_lossy().into_owned();
                (name, std::fs::read_to_string(&p).unwrap())
            })
            .collect();
        assert!(!out.is_empty(), "no grammars found in {dir}");
        out.sort();
        out
    }

    /// Every rule of every example grammar, compiled, in a deterministic order.
    /// Loading the grammars dominates these tests, so it happens once.
    fn all_programs() -> &'static [(String, Program)] {
        static ALL: std::sync::LazyLock<Vec<(String, Program)>> = std::sync::LazyLock::new(|| {
            let mut out = Vec::new();
            for (file, src) in example_sources() {
                let g = SPG::load(&src).unwrap_or_else(|e| panic!("{file}: {e}"));
                let ts = crate::typing::loader::type_trees(&g);
                let mut names: Vec<_> = g.rules.keys().cloned().collect();
                names.sort();
                for name in names {
                    out.push((file.clone(), compile(&g.rules[&name], &ts)));
                }
            }
            out
        });
        &ALL
    }

    /// The IR text of every rule of every example grammar, frozen. `Program` is
    /// the artifact the executor and any future backend consume, so a change to
    /// it is a change to the compilation interface and must be deliberate.
    ///
    /// Regenerate with `UPDATE_GOLDEN=1 cargo test ir_golden`.
    #[test]
    fn ir_golden() {
        let mut got = String::new();
        for (file, prog) in all_programs() {
            got.push_str(&format!("# {file}\n{prog}\n"));
        }
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/src/typing/ir.golden");
        if std::env::var_os("UPDATE_GOLDEN").is_some() {
            std::fs::write(path, &got).unwrap();
            return;
        }
        let want = std::fs::read_to_string(path).unwrap_or_default();
        assert_eq!(
            want, got,
            "IR changed. If deliberate: UPDATE_GOLDEN=1 cargo test ir_golden"
        );
    }

    /// Scopes are balanced in every compiled rule, and never close below zero.
    /// `descend` slices `instrs` by splice range, so an unbalanced program would
    /// give a premise the wrong context.
    #[test]
    fn scopes_are_balanced() {
        for (file, prog) in all_programs() {
            let mut depth = 0i32;
            for instr in &prog.instrs {
                match instr {
                    Instr::PushScope => depth += 1,
                    Instr::PopScope => depth -= 1,
                    _ => {}
                }
                assert!(depth >= 0, "{file}/{}: pop below zero", prog.name);
            }
            assert_eq!(depth, 0, "{file}/{}: unbalanced scopes", prog.name);
        }
    }

    /// Splices are in-bounds, non-empty-or-empty but well-formed, and disjoint.
    /// Two premises sharing instructions would let one premise's setting leak
    /// into another's descent.
    #[test]
    fn splices_are_disjoint_subranges() {
        for (file, prog) in all_programs() {
            let mut ranges: Vec<_> = prog.splices.values().cloned().collect();
            ranges.sort_by_key(|r| (r.start, r.end));
            for r in &ranges {
                assert!(
                    r.start <= r.end && r.end <= prog.instrs.len(),
                    "{file}/{}: bad range {r:?} over {} instrs",
                    prog.name,
                    prog.instrs.len()
                );
            }
            for w in ranges.windows(2) {
                assert!(
                    w[0].end <= w[1].start,
                    "{file}/{}: overlapping splices {:?} and {:?}",
                    prog.name,
                    w[0],
                    w[1]
                );
            }
        }
    }

    /// A splice contains only what a descent needs: `Eval`s and the `Extend`s of
    /// that premise's setting. Never an `Ascribe`/`Member` (the rule's own check,
    /// discharged by `run`, not by descending) and never a scope marker.
    #[test]
    fn splices_contain_only_setting_instructions() {
        for (file, prog) in all_programs() {
            for binding in prog.splices.keys() {
                for instr in prog.splice(binding).unwrap() {
                    assert!(
                        matches!(instr, Instr::Eval { .. } | Instr::Extend { .. }),
                        "{file}/{}: splice {binding} holds {instr}",
                        prog.name
                    );
                }
            }
        }
    }

    /// Behaviour is a function of the compiled `Program`, not of the rule text it
    /// came from. Rendering a grammar back to `.auf` and reloading must yield
    /// identical programs: if it does not, either `to_spec_string` loses
    /// information or `compile` reads something outside the rule.
    #[test]
    fn program_survives_source_round_trip() {
        for (file, src) in example_sources() {
            let g = SPG::load(&src).unwrap_or_else(|e| panic!("{file}: {e}"));
            let rendered = g.to_spec_string();
            let g2 = SPG::load(&rendered).unwrap_or_else(|e| panic!("{file} reload: {e}"));

            let mut names: Vec<_> = g.rules.keys().cloned().collect();
            let mut names2: Vec<_> = g2.rules.keys().cloned().collect();
            names.sort();
            names2.sort();
            assert_eq!(names, names2, "{file}: rule set changed across round trip");

            let (ts, ts2) = (
                crate::typing::loader::type_trees(&g),
                crate::typing::loader::type_trees(&g2),
            );
            for name in names {
                let (p1, p2) = (
                    compile(&g.rules[&name], &ts),
                    compile(&g2.rules[&name], &ts2),
                );
                assert_eq!(
                    p1, p2,
                    "{file}/{name}: program changed across round trip\n--- before\n{p1}--- after\n{p2}"
                );
            }
        }
    }

    /// The other half of the same claim, from the executor's side: a grammar and
    /// its round-tripped twin must agree on every curated input. Equal programs
    /// should imply equal `descend`/`finalize` outcomes; this checks it end to end
    /// rather than trusting the implication.
    #[test]
    fn parse_outcomes_survive_source_round_trip() {
        use crate::typing::TypingSynth;
        use crate::validation::parseable::{all_suites, build_context};

        for (suite, g, valid, invalid) in all_suites() {
            let g2 =
                SPG::load(&g.to_spec_string()).unwrap_or_else(|e| panic!("{suite} reload: {e}"));
            for case in valid.iter().chain(invalid.iter()) {
                let ctx = build_context(&g, &case.context);
                let before = TypingSynth::new(g.clone(), case.input)
                    .parse_with(&ctx)
                    .is_ok();
                let after = TypingSynth::new(g2.clone(), case.input)
                    .parse_with(&ctx)
                    .is_ok();
                assert_eq!(
                    before, after,
                    "{suite}: {} ({:?}) disagrees across round trip",
                    case.description, case.input
                );
            }
        }
    }

    /// `SPG.ir(rule)` is the FFI debug view, and it must work for *every* rule,
    /// including the degenerate shapes: no premises at all, and a rule whose only
    /// judgment is `Member`.
    #[test]
    fn every_rule_renders() {
        let (mut saw_no_premise, mut saw_member_only) = (false, false);
        for (file, prog) in all_programs() {
            let s = prog.to_string();
            assert!(
                s.starts_with(&format!("{}:\n", prog.name)),
                "{file}: bad header for {}",
                prog.name
            );
            let judgments = prog
                .instrs
                .iter()
                .filter(|i| matches!(i, Instr::Ascribe { .. } | Instr::Member { .. }))
                .count();
            if judgments == 0 {
                saw_no_premise = true;
            }
            if prog
                .instrs
                .iter()
                .any(|i| matches!(i, Instr::Member { .. }))
                && !prog
                    .instrs
                    .iter()
                    .any(|i| matches!(i, Instr::Ascribe { .. }))
            {
                saw_member_only = true;
            }
        }
        assert!(saw_no_premise, "no premise-less rule in the corpus");
        assert!(saw_member_only, "no Member-only rule in the corpus");
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
        assert!(
            prog.instrs
                .iter()
                .any(|i| matches!(i, Instr::Member { key } if key.binding() == Some("x")))
        );
        // The conclusion Γ(x) evaluates a context lookup and emits it.
        assert!(matches!(prog.instrs.last(), Some(Instr::Emit { .. })));
        assert!(prog.instrs.iter().any(
            |i| matches!(i, Instr::Eval { expr: TyExpr::Ctx(k), .. } if k.binding() == Some("x"))
        ));
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
        assert!(
            prog.instrs
                .iter()
                .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "l"))
        );
        assert!(
            prog.instrs
                .iter()
                .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "r"))
        );
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
                .any(|i| matches!(i, Instr::Extend { key, .. } if key.binding() == Some("a"))),
            "splice for `l` should include setting extension `a`, got {l_splice:?}"
        );
        let r_splice = prog.splice("r").unwrap();
        assert!(
            r_splice
                .iter()
                .all(|i| !matches!(i, Instr::Extend { key, .. } if key.binding() == Some("a"))),
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
        assert!(
            s_e.iter()
                .any(|i| matches!(i, Instr::Extend { key, .. } if key.binding() == Some("a")))
        );
        assert!(
            s_e.iter()
                .all(|i| !matches!(i, Instr::Ascribe { binding, .. } if binding == "e"))
        );
        assert!(s_e.iter().all(|i| !matches!(i, Instr::PushScope)));
        assert!(s_e.iter().all(|i| !matches!(i, Instr::PopScope)));
        // The setting binder `a` is not a descent target of its own extension:
        // entering `a`'s provider node happens before `a` (and `τ`) resolve, so
        // there is no splice keyed by the binder name.
        assert!(prog.splice("a").is_none());
        // The ascription of `e` is in the program but not in its splice.
        assert!(
            prog.instrs
                .iter()
                .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "e"))
        );
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
        assert!(
            prog.instrs
                .iter()
                .any(|i| matches!(i, Instr::Ascribe { binding, .. } if binding == "e"))
        );
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
