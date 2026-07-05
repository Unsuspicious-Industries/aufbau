use crate::engine::parse::arena::{Lexeme, NodeStatus, Span};
use crate::engine::path::TreePath;
use crate::semantics::Verdict;
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::{Obligation, Obligations};
use crate::typing::domain::Trees;
use crate::typing::ir::compile;
use crate::typing::rule::TypingRule;
use crate::typing::{Context, Evidence, Normalizer, TyExpr, Type, TypingDomain};
use proptest::prelude::*;

fn parse_rule(p: &str, c: &str, n: &str) -> TypingRule {
    TypingRule::new(p.into(), c.into(), n.into()).unwrap()
}

fn stlc() -> crate::engine::grammar::SPG {
    crate::engine::grammar::SPG::load(include_str!("../../../examples/stlc.auf")).unwrap()
}

/// Build the type trees for a rule by parsing each type-expression with `g`.
/// Arrow-shaped expressions need the grammar to recover their structure; a
/// trivial (leaf/hole/ctx) expression resolves against any grammar.
fn trees(g: &crate::engine::grammar::SPG, rule: &TypingRule) -> Trees {
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

/// Trees for a rule whose type-expressions are all trivial (no grammar needed).
fn trivial_trees(rule: &TypingRule) -> Trees {
    trees(&crate::engine::grammar::SPG::new(), rule)
}

fn dom_finalize(
    domain: &TypingDomain,
    evidence: &EvidenceStore<Evidence>,
    trees: &Trees,
    rule: &TypingRule,
    obs: &Obligations,
) -> (Verdict, Option<Type>) {
    let program = compile(rule, trees);
    let (v, ev, _) = domain.finalize(
        &program,
        &Normalizer::new(),
        &Context::new(),
        obs,
        &[],
        NodeStatus::Exact,
        evidence,
    );
    (v, Some(ev.term))
}

fn mkob(evidence: &EvidenceStore<Evidence>, name: &str, ty: Type) -> Obligation {
    Obligation {
        name: name.into(),
        paths: vec![],
        value: Some(Lexeme::new(Span { start: 0, end: 1 }, true, false)),
        evidence: Some(evidence.intern(ty.into())),
    }
}

fn setup() -> (TypingDomain, EvidenceStore<Evidence>) {
    let domain: TypingDomain = TypingDomain::default();
    let evidence = EvidenceStore::new(Evidence::top(), Evidence::bottom());
    (domain, evidence)
}

// ── Verdict monotonicity (lem:safe-pruning) ─────────────────────────────────

proptest! {
    #[test]
    fn verdict_monotonicity(premises in "[a-zA-Z ⊢:?→'.|,Γ()\\[\\]τ _\n\t0-9]{0,60}",
                            conclusion in "[a-zA-Z ⊢:?→'.|,Γ()\\[\\]τ _\n\t0-9]{0,30}") {
        let Ok(rule) = TypingRule::new(premises, conclusion, "test".into()) else { return Ok(()); };
        let (domain, evidence) = setup();
        let trees = trivial_trees(&rule);
        let empty = Obligations::new(TreePath::new(), vec![]);
        let (ve, _) = dom_finalize(&domain, &evidence, &trees, &rule, &empty);

        let with = Obligations::new(TreePath::new(), vec![Obligation {
            name: "x".into(), paths: vec![],
            value: Some(Lexeme::new(Span { start: 0, end: 1 }, true, false)),
            evidence: Some(evidence.intern(Type::raw("X").into())),
        }]);
        let (vw, _) = dom_finalize(&domain, &evidence, &trees, &rule, &with);

        let downgrade = matches!((ve, vw),
            (Verdict::Satisfied, Verdict::Lost) | (Verdict::Satisfied, Verdict::Live)
        );
        prop_assert!(!downgrade, "verdict downgrade: {:?} → {:?}", ve, vw);
    }
}

// ── App rule: holes capture across premises, no arrow construct ─────────────

#[test]
fn app_rule_binds_holes_per_evaluation() {
    let g = stlc();
    let rule = parse_rule("Γ ⊢ x : ?A -> ?B, Γ ⊢ y : ?A", "?B", "app");
    let trees = trees(&g, &rule);
    let (domain, evidence) = setup();
    let obs1 = Obligations::new(
        TreePath::new(),
        vec![
            mkob(&evidence, "x", Type::parse(&g, "Int->Bool").unwrap()),
            mkob(&evidence, "y", Type::parse(&g, "Int").unwrap()),
        ],
    );
    let (v1, t1) = dom_finalize(&domain, &evidence, &trees, &rule, &obs1);
    let obs2 = Obligations::new(
        TreePath::new(),
        vec![
            mkob(&evidence, "x", Type::parse(&g, "String->Int").unwrap()),
            mkob(&evidence, "y", Type::parse(&g, "String").unwrap()),
        ],
    );
    let (v2, t2) = dom_finalize(&domain, &evidence, &trees, &rule, &obs2);
    assert_eq!(v1, Verdict::Satisfied);
    assert_eq!(v2, Verdict::Satisfied);
    assert_eq!(t1, Some(Type::parse(&g, "Bool").unwrap()));
    assert_eq!(t2, Some(Type::parse(&g, "Int").unwrap()));
}

#[test]
fn app_rule_rejects_argument_mismatch() {
    let g = stlc();
    let rule = parse_rule("Γ ⊢ f : ?A -> ?B, Γ ⊢ x : ?A", "?B", "app");
    let trees = trees(&g, &rule);
    let (domain, evidence) = setup();
    let obs = Obligations::new(
        TreePath::new(),
        vec![
            mkob(&evidence, "f", Type::parse(&g, "Int->Bool").unwrap()),
            mkob(&evidence, "x", Type::parse(&g, "String").unwrap()),
        ],
    );
    let (v, _) = dom_finalize(&domain, &evidence, &trees, &rule, &obs);
    assert_eq!(v, Verdict::Lost);
}

#[test]
fn eval_deterministic() {
    let rule = parse_rule("Γ ⊢ x : ?A, Γ ⊢ y : ?A", "?A", "test");
    let trees = trivial_trees(&rule);
    let (domain, evidence) = setup();
    let obs = Obligations::new(
        TreePath::new(),
        vec![
            mkob(&evidence, "x", Type::raw("Int")),
            mkob(&evidence, "y", Type::raw("Int")),
        ],
    );
    let (v1, t1) = dom_finalize(&domain, &evidence, &trees, &rule, &obs);
    let (v2, t2) = dom_finalize(&domain, &evidence, &trees, &rule, &obs);
    assert_eq!(v1, v2);
    assert_eq!(t1, t2);
}

// ── Context lookup with an open prefix lexeme ───────────────────────────────

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
    let ctx = Context::new().shadow("foo".into(), Type::raw("Int"));
    let program = compile(&rule, &trivial_trees(&rule));
    let (v, ev, _) = domain.finalize(
        &program,
        &Normalizer::new(),
        &ctx,
        &obs,
        &segs,
        NodeStatus::Exact,
        &evidence,
    );
    assert_eq!(v, Verdict::Satisfied, "open prefix 'fo' should match 'foo'");
    assert_eq!(ev.term, Type::raw("Int"));
}
