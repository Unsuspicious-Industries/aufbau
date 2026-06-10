use crate::engine::grammar::SPG;
use crate::engine::parse::arena::{AltRange, ArenaNode, Lexeme, NodeStatus, Span};
use crate::engine::path::TreePath;
use crate::engine::Segment;
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::Obligations;
use crate::semantics::Verdict;
use crate::typing::domain::Trees;
use crate::typing::ir::compile;
use crate::typing::rule::TypingRule;
use crate::typing::Context;
use crate::typing::Type;
use crate::typing::{Evidence, Normalizer, TyExpr, TypingDomain};
use std::collections::HashMap;

fn trees(g: &SPG, rule: &TypingRule) -> Trees {
    let bindings = g.rule_bindings(&rule.name);
    rule.type_exprs()
        .into_iter()
        .filter_map(|te| TyExpr::build(g, te, &bindings).ok().map(|ty| (te.clone(), ty)))
        .collect()
}

fn segs(parts: &[&str]) -> Vec<Segment> {
    parts
        .iter()
        .enumerate()
        .map(|(i, part)| {
            let mut seg = Segment::from_str(part, i, i + 1);
            seg.index = i;
            seg
        })
        .collect()
}

#[test]
fn descend_extends_context_with_constant_type() {
    let grammar = SPG::load(
        r#"
        Identifier ::= /[a-z]+/
        Body ::= Identifier[e]
        Bind(bind) ::= Identifier[a] Body[e]

        Γ[a:'A'] ⊢ e : ?R
        ------------------ (bind)
        ?R
        "#,
    )
    .unwrap();
    let s = segs(&["x", "body"]);
    let domain: TypingDomain = TypingDomain::default();
    let evidence = EvidenceStore::new(Evidence::top(), Evidence::bottom());
    let rule = grammar.rules().get("bind").unwrap();
    let trees = trees(&grammar, rule);

    let mut obs = Obligations::create(
        &grammar,
        (grammar.nt_index("Bind").unwrap(), 0),
        TreePath::new(),
    );
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));

    let ctx = Context::new();
    let program = compile(rule, &trees);
    // The setting `Γ[a:'A']` is the premise of `e`, so it applies when entering
    // `e` — once `a` (= "x") is already resolved. Entering the binder `a` itself
    // would predate `a`'s own resolution, so it carries no extension.
    let next = domain
        .descend(&program, &Normalizer::new(), Some("e"), &ctx, &obs, &s, &evidence)
        .expect("setting extension should resolve");
    assert!(
        next.lookup("x").is_some(),
        "expected 'x' bound to 'A' in extended context, got {:?}",
        next
    );
}

#[test]
fn finalize_rejects_closed_ascription_mismatch() {
    let grammar = SPG::load(
        r#"
        Identifier ::= /[a-z]+/
        TypeName ::= 'Int' | 'Bool'
        Type ::= TypeName
        Boolean(bool) ::= 'true'
        Decl(decl) ::= Identifier[name] ':' Type[τ] '=' Boolean[value]

        ----------- (bool)
        'Bool'

        Γ ⊢ value : τ
        ----------------------- (decl)
        Γ → Γ[name:τ] ⊢ 'Unit'
        "#,
    )
    .unwrap();
    let prod = (grammar.nt_index("Decl").unwrap(), 0);
    let s = segs(&["x", ":", "Int", "=", "true"]);
    let domain: TypingDomain = TypingDomain::default();
    let evidence = EvidenceStore::new(Evidence::top(), Evidence::bottom());
    let rule = grammar.rules().get("decl").unwrap();

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));
    let type_nt = grammar.nt_index("Type").unwrap();
    let type_node = ArenaNode {
        nt: type_nt,
        span: Span { start: 2, end: 3 },
        status: NodeStatus::Exact,
        semantic_complete: true,
        evidence: evidence.intern(Type::raw("Int").into()),
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(2, 0, &type_node);
    let bool_nt = grammar.nt_index("Boolean").unwrap();
    let bool_ty = evidence.intern(Type::parse_raw("Bool").unwrap().into());
    let bool_node = ArenaNode {
        nt: bool_nt,
        span: Span { start: 4, end: 5 },
        status: NodeStatus::Exact,
        semantic_complete: true,
        evidence: bool_ty,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(4, 0, &bool_node);

    let ctx = Context::new();
    let program = compile(rule, &trees(&grammar, rule));
    let (verdict, _, _) = domain.finalize(
        &program,
        &Normalizer::new(),
        &ctx,
        &obs,
        &s,
        NodeStatus::Exact,
        &evidence,
    );
    assert!(
        matches!(verdict, Verdict::Lost),
        "expected Lost, got {:?}",
        verdict
    );
}
