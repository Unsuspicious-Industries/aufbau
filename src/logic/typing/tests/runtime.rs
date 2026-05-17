use crate::logic::Segment;
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{ANY_TYPE, AltRange, ArenaNode, Lexeme, NodeStatus, Span};
use crate::logic::path::TreePath;
use crate::logic::typing::runtime::RuleRuntime;
use crate::logic::typing::{Obligations, SemanticRuntime, Type};
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
    let grammar = Grammar::load(
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
    let mut runtime = RuleRuntime::new(grammar.clone());
    runtime.set_segs(&segs(&["x", "body"]));

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));

    let body_ctx = runtime.descend(prod, Some("e"), 0, &obs).unwrap();
    let ctx = runtime.context(Some(body_ctx)).unwrap();
    assert_eq!(ctx.lookup("x"), Some(&Type::parse_raw("A").unwrap()));
}

#[test]
fn descend_resolves_type_binding_before_context_extension() {
    let grammar = Grammar::load(
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
    let mut runtime = RuleRuntime::new(grammar.clone());
    runtime.set_segs(&segs(&["x", ":", "A", ".", "body"]));

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));
    let type_nt = grammar.nt_index("Type").unwrap();
    let type_node = ArenaNode {
        nt: type_nt,
        span: Span { start: 2, end: 3 },
        status: NodeStatus::Closed,
        semantic_complete: true,
        evidence: ANY_TYPE,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(2, 0, &type_node);

    let body_ctx = runtime.descend(prod, Some("e"), 0, &obs).unwrap();
    let ctx = runtime.context(Some(body_ctx)).unwrap();
    assert_eq!(ctx.lookup("x"), Some(&Type::parse_raw("A").unwrap()));
}

#[test]
fn finalize_resolves_output_context_types_before_export() {
    let grammar = Grammar::load(
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
    let mut runtime = RuleRuntime::new(grammar.clone());
    runtime.set_segs(&segs(&["x", ":", "A"]));

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));
    let type_nt = grammar.nt_index("Type").unwrap();
    let type_node = ArenaNode {
        nt: type_nt,
        span: Span { start: 2, end: 3 },
        status: NodeStatus::Closed,
        semantic_complete: true,
        evidence: ANY_TYPE,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(2, 0, &type_node);

    let summary = runtime.finalize(prod, 0, &obs, NodeStatus::Closed).unwrap();
    let ctr = summary
        .effect
        .and_then(|effect| runtime.effect(effect))
        .expect("decl should export context transform");
    assert_eq!(
        ctr.transforms,
        vec![("x".to_string(), Type::parse_raw("A").unwrap())]
    );
}

#[test]
fn finalize_rejects_closed_ascription_mismatch() {
    let grammar = Grammar::load(
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
    let mut runtime = RuleRuntime::new(grammar.clone());
    runtime.set_segs(&segs(&["x", ":", "Int", "=", "true"]));

    let mut obs = Obligations::create(&grammar, prod, TreePath::new());
    obs.resolve_terminal(0, 0, &Lexeme::new(Span { start: 0, end: 1 }, true, false));
    let type_nt = grammar.nt_index("Type").unwrap();
    let type_node = ArenaNode {
        nt: type_nt,
        span: Span { start: 2, end: 3 },
        status: NodeStatus::Closed,
        semantic_complete: true,
        evidence: ANY_TYPE,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(2, 0, &type_node);
    let bool_nt = grammar.nt_index("Boolean").unwrap();
    let bool_ty = runtime.intern_type(Type::parse_raw("Bool").unwrap());
    let bool_node = ArenaNode {
        nt: bool_nt,
        span: Span { start: 4, end: 5 },
        status: NodeStatus::Closed,
        semantic_complete: true,
        evidence: bool_ty,
        effect: None,
        binding_map: HashMap::new(),
        alts: AltRange { start: 0, len: 0 },
    };
    obs.resolve_nonterminal(4, 0, &bool_node);

    assert!(runtime.finalize(prod, 0, &obs, NodeStatus::Closed).is_err());
}
