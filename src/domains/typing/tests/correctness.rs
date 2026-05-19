use crate::domains::typing::rule::{CompilationPass, TypingRule};
use crate::domains::typing::{subtype, Context, Type, TypingDomain, Unifier, UnifyResult};
use crate::engine::parse::arena::{Lexeme, NodeStatus, Span};
use crate::engine::path::TreePath;
use crate::semantics::domain::{ConstraintDomain, Verdict};
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::{Obligation, Obligations};
use proptest::prelude::*;

fn parse_rule(p: &str, c: &str, n: &str) -> TypingRule {
    TypingRule::new(p.into(), c.into(), n.into()).unwrap()
}

fn dom_finalize(
    domain: &TypingDomain,
    evidence: &EvidenceStore<Type>,
    rule: &TypingRule,
    obs: &Obligations,
) -> (Verdict, Option<Type>) {
    let (v, ty, _) = domain.finalize(rule, &Context::new(), obs, &[], NodeStatus::Exact, evidence);
    (v, Some(ty))
}

fn mkob(evidence: &EvidenceStore<Type>, name: &str, ty: Type) -> Obligation {
    Obligation {
        name: name.into(),
        paths: vec![],
        value: Some(Lexeme::new(Span { start: 0, end: 1 }, true, false)),
        evidence: Some(evidence.intern(ty)),
    }
}

fn setup() -> (TypingDomain, EvidenceStore<Type>) {
    let domain = TypingDomain::new();
    let evidence = EvidenceStore::new(Type::Any, Type::None);
    (domain, evidence)
}

// ── Strategies ──────────────────────────────────────────────────────────────

fn closed_type() -> impl Strategy<Value = Type> {
    let leaf = prop_oneof![
        Just(Type::Any),
        Just(Type::None),
        "[A-Z][a-z]*".prop_map(Type::raw),
    ];
    leaf.prop_recursive(3, 4, 2, |inner| {
        prop_oneof![
            (inner.clone(), inner.clone()).prop_map(|(d, c)| Type::Arrow(Box::new(d), Box::new(c))),
            prop::collection::vec(inner.clone(), 1..3).prop_map(Type::Union),
            inner.clone().prop_map(|t| Type::Not(Box::new(t))),
        ]
    })
}

// ── P1 — Verdict monotonicity (Lemma 3.1) ──────────────────────────────────

proptest! {
    #[test]
    fn verdict_monotonicity(premises in "[a-zA-Z ⊢:?→'.|,Γ()\\[\\]τ _\n\t0-9]{0,60}",
                            conclusion in "[a-zA-Z ⊢:?→'.|,Γ()\\[\\]τ _\n\t0-9]{0,30}") {
        let Ok(rule) = TypingRule::new(premises, conclusion, "test".into()) else { return Ok(()); };
        let (domain, evidence) = setup();
        let empty = Obligations::new(TreePath::new(), vec![]);
        let (ve, _) = dom_finalize(&domain, &evidence, &rule, &empty);

        let with = Obligations::new(TreePath::new(), vec![Obligation {
            name: "x".into(), paths: vec![],
            value: Some(Lexeme::new(Span { start: 0, end: 1 }, true, false)),
            evidence: Some(evidence.intern(Type::raw("X"))),
        }]);
        let (vw, _) = dom_finalize(&domain, &evidence, &rule, &with);

        let downgrade = matches!((ve, vw),
            (Verdict::Satisfied, Verdict::Lost) | (Verdict::Satisfied, Verdict::Live)
        );
        prop_assert!(!downgrade, "verdict downgrade: {:?} → {:?}", ve, vw);
    }
}

// ── P2 — Meta locality (§3) ────────────────────────────────────────────────

#[test]
fn meta_map_is_fresh_per_evaluation() {
    let rule = parse_rule("Γ ⊢ x : ?A -> ?B, Γ ⊢ y : ?A", "?B", "app");
    let (domain, evidence) = setup();
    let obs1 = Obligations::new(
        TreePath::new(),
        vec![
            mkob(
                &evidence,
                "x",
                Type::arrow(Type::raw("Int"), Type::raw("Bool")),
            ),
            mkob(&evidence, "y", Type::raw("Int")),
        ],
    );
    let (v1, t1) = dom_finalize(&domain, &evidence, &rule, &obs1);
    let obs2 = Obligations::new(
        TreePath::new(),
        vec![
            mkob(
                &evidence,
                "x",
                Type::arrow(Type::raw("String"), Type::raw("Int")),
            ),
            mkob(&evidence, "y", Type::raw("String")),
        ],
    );
    let (v2, t2) = dom_finalize(&domain, &evidence, &rule, &obs2);
    assert_eq!(v1, Verdict::Satisfied);
    assert_eq!(v2, Verdict::Satisfied);
    assert_eq!(t1, Some(Type::raw("Bool")));
    assert_eq!(t2, Some(Type::raw("Int")));
}

// ── P3 — Arrow decomposition (§3) ─────────────────────────────────────────

#[test]
fn arrow_decomposition_binds_domain_and_codomain() {
    let rule = parse_rule("Γ ⊢ f : ?A -> ?B, Γ ⊢ x : ?A", "?B", "app");
    let (domain, evidence) = setup();
    let obs = Obligations::new(
        TreePath::new(),
        vec![
            mkob(
                &evidence,
                "f",
                Type::arrow(Type::raw("Int"), Type::raw("Bool")),
            ),
            mkob(&evidence, "x", Type::raw("Int")),
        ],
    );
    let (v, ty) = dom_finalize(&domain, &evidence, &rule, &obs);
    assert_eq!(v, Verdict::Satisfied);
    assert_eq!(ty, Some(Type::raw("Bool")));
}

#[test]
fn arrow_decomposition_rejects_domain_mismatch() {
    let rule = parse_rule("Γ ⊢ f : ?A -> ?B, Γ ⊢ x : ?A", "?B", "app");
    let (domain, evidence) = setup();
    let obs = Obligations::new(
        TreePath::new(),
        vec![
            mkob(
                &evidence,
                "f",
                Type::arrow(Type::raw("Int"), Type::raw("Bool")),
            ),
            mkob(&evidence, "x", Type::raw("String")),
        ],
    );
    let (v, _) = dom_finalize(&domain, &evidence, &rule, &obs);
    assert_eq!(v, Verdict::Lost);
}

// ── P4 — Compilation structure ─────────────────────────────────────────────

#[test]
fn compilation_preserves_premise_cardinality() {
    let rule = parse_rule("Γ ⊢ f : ?A -> ?B, Γ ⊢ x : ?A", "?B", "app");
    let compiled = CompilationPass::compile(&rule).unwrap();
    assert!(
        compiled.premises.len() >= 2,
        "compiled has {} premises",
        compiled.premises.len()
    );
}

// ── P5 — Unifier (idempotent, commutative) ─────────────────────────────────

proptest! {
    #[test]
    fn unify_idempotent(t in closed_type()) {
        assert!(Unifier::unify(&t, &t).is_ok(), "{t:?} should unify with itself");
    }

    #[test]
    fn unify_commutative(t1 in closed_type(), t2 in closed_type()) {
        let a = Unifier::unify(&t1, &t2);
        let b = Unifier::unify(&t2, &t1);
        let same = matches!((&a, &b),
            (UnifyResult::Ok, UnifyResult::Ok) |
            (UnifyResult::Indeterminate, UnifyResult::Indeterminate) |
            (UnifyResult::Fail(_), UnifyResult::Fail(_))
        );
        prop_assert!(same, "unify({t1:?},{t2:?})={a:?} but unify({t2:?},{t1:?})={b:?}");
    }
}

// ── P6 — Subtype lattice (§3) ──────────────────────────────────────────────

proptest! {
    #[test]
    fn subtype_reflexive(t in closed_type()) { prop_assert!(subtype(&t, &t)); }
    #[test]
    fn subtype_none_is_bottom(t in closed_type()) { prop_assert!(subtype(&Type::None, &t)); }
    #[test]
    fn subtype_any_is_top(t in closed_type()) { prop_assert!(subtype(&t, &Type::Any)); }
}

// ── P7 — eval determinism (Theorem 3.5) ────────────────────────────────────

#[test]
fn eval_deterministic() {
    let rule = parse_rule("Γ ⊢ x : ?A, Γ ⊢ y : ?A", "?A", "test");
    let (domain, evidence) = setup();
    let obs = Obligations::new(
        TreePath::new(),
        vec![
            mkob(&evidence, "x", Type::raw("Int")),
            mkob(&evidence, "y", Type::raw("Int")),
        ],
    );
    let (v1, t1) = dom_finalize(&domain, &evidence, &rule, &obs);
    let (v2, t2) = dom_finalize(&domain, &evidence, &rule, &obs);
    assert_eq!(v1, v2);
    assert_eq!(t1, t2);
}

// ── P8 — ContextExt with open lexeme (§3) ──────────────────────────────────

#[test]
fn context_ext_accepts_open_prefix() {
    let rule = parse_rule("x ∈ Γ", "Γ(x)", "var");
    let (domain, evidence) = setup();
    let obs = Obligations::new(
        TreePath::new(),
        vec![Obligation {
            name: "x".into(),
            paths: vec![],
            value: Some(Lexeme::new(Span { start: 0, end: 1 }, false, true)),
            evidence: None,
        }],
    );
    let segs = vec![crate::engine::grammar::Segment::from_str("fo", 0, 1)];
    let ctx = Context::new()
        .extend("foo".into(), Type::raw("Int"))
        .unwrap();
    let (v, _) = dom_finalize_ctx(&domain, &evidence, &rule, &obs, &ctx, &segs);
    assert_eq!(v, Verdict::Satisfied, "open prefix 'fo' should match 'foo'");
}

fn dom_finalize_ctx(
    domain: &TypingDomain,
    evidence: &EvidenceStore<Type>,
    rule: &TypingRule,
    obs: &Obligations,
    ctx: &Context,
    segs: &[crate::engine::grammar::Segment],
) -> (Verdict, Option<Type>) {
    let (v, ty, _) = domain.finalize(rule, ctx, obs, segs, NodeStatus::Exact, evidence);
    (v, Some(ty))
}
