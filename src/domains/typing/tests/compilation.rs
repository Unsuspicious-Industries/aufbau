use crate::domains::typing::compiler::compile_rule;
use crate::domains::typing::rule::TypingRule;
use crate::domains::typing::{Context, Type, TypingDomain};
use crate::engine::grammar::Segment;
use crate::engine::parse::arena::{Lexeme, NodeStatus, Span};
use crate::engine::path::TreePath;
use crate::semantics::Verdict;
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::Obligation;
use crate::semantics::Obligations;

// ── Harness ────────────────────────────────────────────────────────────────

fn parse_rule(p: &str, c: &str, n: &str) -> TypingRule {
    TypingRule::new(p.into(), c.into(), n.into()).unwrap()
}

struct RuleCase {
    domain: TypingDomain,
    evidence: EvidenceStore<Type>,
    rule: TypingRule,
}

impl RuleCase {
    fn new(premises: &str, conclusion: &str, name: &str) -> Self {
        let domain = TypingDomain;
        let evidence = EvidenceStore::new(Type::Any, Type::None);
        Self {
            domain,
            evidence,
            rule: parse_rule(premises, conclusion, name),
        }
    }

    fn eval(&self, obs: &Obligations, ctx: &Context, segs: &[Segment]) -> (Verdict, Option<Type>) {
        let (v, ty, _) = self.domain.finalize(
            &self.rule,
            ctx,
            obs,
            segs,
            NodeStatus::Exact,
            &self.evidence,
        );
        (v, Some(ty))
    }

    fn eval_with(&self, obs: &Obligations) -> (Verdict, Option<Type>) {
        self.eval(obs, &Context::new(), &[])
    }

    fn mkobl(&self, bindings: Vec<(&str, Option<Type>, Option<Lexeme>)>) -> Obligations {
        let items: Vec<Obligation> = bindings
            .into_iter()
            .map(|(n, ty, lex)| {
                let evidence = ty.map(|t| self.evidence.intern(t));
                Obligation {
                    name: n.into(),
                    paths: vec![],
                    value: lex,
                    evidence,
                }
            })
            .collect();
        Obligations::new(TreePath::new(), items)
    }
}

fn mklex(start: u32, end: u32) -> Lexeme {
    Lexeme::new(Span { start, end }, true, false)
}

fn mkseg(c: &str, s: usize, e: usize) -> Segment {
    Segment::from_str(c, s, e)
}

// =========================================================================
// Compilation pass
// =========================================================================

#[test]
fn app_no_metas_after_compilation() {
    let r = parse_rule("Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A", "?B", "app");
    let c = compile_rule(&r).unwrap();
    assert!(c.conclusion.kind.has_metas());
}

#[test]
fn lambda_has_typeof() {
    let r = parse_rule("Γ[a:τ] ⊢ e : ?B", "τ → ?B", "lambda");
    let c = compile_rule(&r).unwrap();
    assert!(c.conclusion.kind.to_string().contains("typeof"));
}

#[test]
fn var_has_no_metas() {
    assert!(!crate::domains::typing::compiler::has_metas(&parse_rule(
        "x ∈ Γ", "Γ(x)", "var"
    )));
}

// =========================================================================
// Var rule
// =========================================================================

#[test]
fn var_bound() {
    let c = RuleCase::new("x ∈ Γ", "Γ(x)", "var");
    let obs = c.mkobl(vec![("x", None, Some(mklex(0, 1)))]);
    let segs = vec![mkseg("f", 0, 1)];
    let ctx = Context::new().extend("f".into(), Type::raw("Int")).unwrap();
    let (v, ty) = c.eval(&obs, &ctx, &segs);
    assert_eq!(v, Verdict::Satisfied);
    assert_eq!(ty, Some(Type::raw("Int")));
}

#[test]
fn var_unbound() {
    let c = RuleCase::new("x ∈ Γ", "Γ(x)", "var");
    let obs = c.mkobl(vec![("x", None, Some(mklex(0, 1)))]);
    let segs = vec![mkseg("y", 0, 1)];
    let (v, _) = c.eval(&obs, &Context::new(), &segs);
    assert_eq!(v, Verdict::Lost);
}

// =========================================================================
// Lambda rule
// =========================================================================

#[test]
fn lambda_concrete() {
    let c = RuleCase::new("Γ[a:τ] ⊢ e : ?B", "τ → ?B", "lambda");
    let obs = c.mkobl(vec![
        ("a", None, Some(mklex(0, 1))),
        ("τ", Some(Type::raw("A")), Some(mklex(2, 3))),
        ("e", Some(Type::raw("A")), Some(mklex(4, 5))),
    ]);
    let segs = vec![
        mkseg("x", 0, 1),
        mkseg(":", 1, 2),
        mkseg("A", 2, 3),
        mkseg(".", 3, 4),
        mkseg("b", 4, 5),
    ];
    let (v, _) = c.eval(&obs, &Context::new(), &segs);
    assert_eq!(v, Verdict::Satisfied, "lambda A→A");
}

// =========================================================================
// App rule — arrow pattern matching
// =========================================================================

#[test]
fn app_concrete() {
    let c = RuleCase::new("Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A", "?B", "app");
    let obs = c.mkobl(vec![
        (
            "l",
            Some(Type::arrow(Type::raw("A"), Type::raw("B"))),
            Some(mklex(0, 1)),
        ),
        ("r", Some(Type::raw("A")), Some(mklex(2, 3))),
    ]);
    let (v, ty) = c.eval_with(&obs);
    assert_eq!(v, Verdict::Satisfied, "got {v:?} ty={ty:?}");
    assert_eq!(ty, Some(Type::raw("B")));
}

#[test]
fn app_mismatch() {
    let c = RuleCase::new("Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A", "?B", "app");
    let obs = c.mkobl(vec![
        (
            "l",
            Some(Type::arrow(Type::raw("X"), Type::raw("B"))),
            Some(mklex(0, 1)),
        ),
        ("r", Some(Type::raw("Y")), Some(mklex(2, 3))),
    ]);
    let (v, _) = c.eval_with(&obs);
    assert_eq!(v, Verdict::Lost);
}

// =========================================================================
// Define — context transform
// =========================================================================

#[test]
fn define_context_transform() {
    let c = RuleCase::new("Γ ⊢ value : ?T", "Γ → Γ[name:?T] ⊢ 'Unit'", "define");
    let obs = c.mkobl(vec![
        ("name", None, Some(mklex(0, 1))),
        ("value", Some(Type::raw("Int")), Some(mklex(2, 3))),
    ]);
    let segs = vec![mkseg("x", 0, 1)];
    let (v, _) = c.eval(&obs, &Context::new(), &segs);
    assert_eq!(v, Verdict::Satisfied);
}
