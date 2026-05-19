use crate::domains::typing::Type;
use crate::domains::typing::TypingDomain;
use crate::engine::grammar::SPG;
use crate::engine::parse::arena::{AltRange, ArenaNode, Lexeme, NodeStatus, Span, TOP};
use crate::engine::path::TreePath;
use crate::engine::Segment;
use crate::semantics::domain::{ConstraintDomain, Verdict};
use crate::semantics::evidence::EvidenceStore;
use crate::semantics::Obligations;
use std::collections::HashMap;

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
    let grammar = SPG::<TypingDomain>::load(
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
    let prod = (grammar.nt_index("Bind").unwrap(), 0);
    let s = segs(&["x", "body"]);
    let domain = TypingDomain::new();
    let evidence = EvidenceStore::new(Type::Any, Type::None);
    let rule = grammar.rules().get("bind").unwrap();

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));

    let ctx = domain.empty_context();
    let result_ctx = domain.descend(rule, Some("e"), &ctx, &obs, &s, &evidence);
    assert_eq!(result_ctx.lookup("x"), Some(&Type::parse_raw("A").unwrap()));
}

#[test]
fn descend_resolves_type_binding_before_context_extension() {
    let grammar = SPG::<TypingDomain>::load(
        r#"
        TypeName ::= /[A-Z]+/
        Type ::= TypeName
        Identifier ::= /[a-z]+/
        Body ::= Identifier[e]
        Bind(bind) ::= Identifier[a] ':' Type[τ] '.' Body[e]

        Γ[a:τ] ⊢ e : ?R
        ------------------ (bind)
        ?R
        "#,
    )
    .unwrap();
    let prod = (grammar.nt_index("Bind").unwrap(), 0);
    let s = segs(&["x", ":", "A", ".", "body"]);
    let domain = TypingDomain::new();
    let evidence = EvidenceStore::new(Type::Any, Type::None);
    let rule = grammar.rules().get("bind").unwrap();

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));
    let type_nt = grammar.nt_index("Type").unwrap();
    let type_node = ArenaNode {
        nt: type_nt,
        span: Span { start: 2, end: 3 },
        status: NodeStatus::Exact,
        semantic_complete: true,
        evidence: TOP,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(2, 0, &type_node);

    let ctx = domain.empty_context();
    let result_ctx = domain.descend(rule, Some("e"), &ctx, &obs, &s, &evidence);
    assert_eq!(result_ctx.lookup("x"), Some(&Type::parse_raw("A").unwrap()));
}

#[test]
fn finalize_resolves_output_context_types_before_export() {
    let grammar = SPG::<TypingDomain>::load(
        r#"
        TypeName ::= /[A-Z]+/
        Type ::= TypeName
        Identifier ::= /[a-z]+/
        Decl(decl) ::= Identifier[name] ':' Type[τ]

        ------------------ (decl)
        Γ → Γ[name:τ] ⊢ 'Unit'
        "#,
    )
    .unwrap();
    let prod = (grammar.nt_index("Decl").unwrap(), 0);
    let s = segs(&["x", ":", "A"]);
    let domain = TypingDomain::new();
    let evidence = EvidenceStore::new(Type::Any, Type::None);
    let rule = grammar.rules().get("decl").unwrap();

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));
    let type_nt = grammar.nt_index("Type").unwrap();
    let type_node = ArenaNode {
        nt: type_nt,
        span: Span { start: 2, end: 3 },
        status: NodeStatus::Exact,
        semantic_complete: true,
        evidence: TOP,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(2, 0, &type_node);

    let ctx = domain.empty_context();
    let (verdict, _, _) = domain.finalize(rule, &ctx, &obs, &s, NodeStatus::Exact, &evidence);
    assert!(
        matches!(verdict, Verdict::Satisfied),
        "expected Satisfied, got {:?}",
        verdict
    );
    let _effect = verdict; // keep alive, no access to effect in new API
}

#[test]
fn finalize_rejects_closed_ascription_mismatch() {
    let grammar = SPG::<TypingDomain>::load(
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
    let domain = TypingDomain::new();
    let evidence = EvidenceStore::new(Type::Any, Type::None);
    let rule = grammar.rules().get("decl").unwrap();

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));
    let type_nt = grammar.nt_index("Type").unwrap();
    let type_node = ArenaNode {
        nt: type_nt,
        span: Span { start: 2, end: 3 },
        status: NodeStatus::Exact,
        semantic_complete: true,
        evidence: TOP,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(2, 0, &type_node);
    let bool_nt = grammar.nt_index("Boolean").unwrap();
    let bool_ty = evidence.intern(Type::parse_raw("Bool").unwrap());
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

    let ctx = domain.empty_context();
    let (verdict, _, _) = domain.finalize(rule, &ctx, &obs, &s, NodeStatus::Exact, &evidence);
    assert!(
        matches!(verdict, Verdict::Lost),
        "expected Lost, got {:?}",
        verdict
    );
}
