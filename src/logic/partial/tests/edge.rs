use crate::logic::grammar::Grammar;
use crate::logic::partial::structure::SppfChild;
use crate::logic::partial::{MetaParser, Parser};

#[test]
fn hard_left_recursive_application_chain_stays_parseable() {
    let spec = r#"
    Atom ::= 'x'
    Expr ::= Expr Atom | Atom
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar);

    let ast = parser.parse("x x x x").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn mutual_recursion_with_parenthesized_escape_holds() {
    let spec = r#"
    Expr ::= Expr '+' Term | Term
    Term ::= '(' Expr ')' | 'n'
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = Parser::new(grammar);
    let ast = parser.parse("( n + n ) + n").unwrap();
    assert!(ast.is_complete());
}

#[test]
fn partial_operator_prefix_keeps_future_shape() {
    let spec = r#"
    Number ::= /[0-9]+/
    Expr ::= Expr '+' Number | Number
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = Parser::new(grammar);

    let ast = parser.partial("1 +").unwrap();
    assert!(!ast.roots().is_empty());
    assert!(!ast.is_complete());
}

#[test]
fn append_only_prefix_state_roundtrip_is_conservative() {
    let spec = r#"
    Atom ::= 'x'
    Expr ::= Expr Atom | Atom
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = Parser::new(grammar.clone());
    let prefix = parser.prefix("x x").unwrap();
    let advanced = parser.advance(&prefix, "x x").unwrap();

    assert!(advanced.forest().is_complete());
    assert!(!advanced.forest().roots().is_empty());
    assert_eq!(advanced.input(), "x x");
}

#[test]
fn repetition_star_materializes_flat_children() {
    let spec = r#"
    Number ::= /[0-9]+/
    start ::= Number*
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar);

    let ast = parser.parse("1 2 3").unwrap();
    let root = ast.complete().expect("complete tree");
    assert_eq!(root.children.len(), 3);
}

#[test]
fn repetition_plus_requires_one_item() {
    let spec = r#"
    Number ::= /[0-9]+/
    start ::= Number+
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar.clone());

    assert!(parser.parse("1 2").is_ok());
    let mut parser = Parser::new(grammar);
    assert!(parser.parse("").is_err());
}

#[test]
fn repetition_optional_accepts_absence_without_extra_node() {
    let spec = r#"
    start ::= 'x'?
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = MetaParser::new(grammar);

    let ast = parser.parse("").unwrap();
    let root = ast.complete().expect("complete tree");
    assert!(root.children.is_empty());
}

#[test]
fn packed_nodes_share_children() {
    let spec = r#"
    X ::= 'x'
    A ::= X
    B ::= X
    start ::= A | B
    "#;
    let grammar = Grammar::load(spec).unwrap();
    let mut parser = Parser::new(grammar);

    let ast = parser.parse("x").unwrap();
    assert_eq!(
        ast.root_ids().len(),
        1,
        "root should be packed into one node"
    );

    let root_id = ast.root_ids()[0];
    let root = ast.node(root_id).expect("packed root node");
    assert_eq!(
        root.alternatives.len(),
        2,
        "root should carry both alternatives"
    );

    let child_ids = root
        .alternatives
        .iter()
        .map(|alt| match alt.children.as_slice() {
            [SppfChild::Node(child_id)] => *child_id,
            other => panic!("unexpected root children: {:?}", other),
        })
        .collect::<Vec<_>>();

    let a = ast.node(child_ids[0]).expect("A node");
    let b = ast.node(child_ids[1]).expect("B node");
    let ax = match a.alternatives[0].children.as_slice() {
        [SppfChild::Node(child_id)] => *child_id,
        other => panic!("unexpected A children: {:?}", other),
    };
    let bx = match b.alternatives[0].children.as_slice() {
        [SppfChild::Node(child_id)] => *child_id,
        other => panic!("unexpected B children: {:?}", other),
    };

    assert_eq!(
        ax, bx,
        "confluent substructure should share the same global node id"
    );
}

#[test]
fn append_only_advance_reuses_seed() {
    let spec = r#"
    Atom ::= 'x'
    Expr ::= Expr Atom | Atom
    start ::= Expr
    "#;
    let grammar = Grammar::load(spec).unwrap();

    let mut fresh = Parser::new(grammar.clone()).with_max_recursion(32);
    let _ = fresh.prefix("x x x x").unwrap();
    let fresh_stats = fresh.last_stats().clone();

    let mut incremental = Parser::new(grammar).with_max_recursion(32);
    let prefix = incremental.prefix("x x").unwrap();
    let _ = incremental.advance(&prefix, "x x x x").unwrap();
    let incremental_stats = incremental.last_stats().clone();

    assert!(
        incremental_stats.nt_cache_hits > 0,
        "append-only advance should reuse exact seed entries from the prior prefix",
    );
    assert!(
        incremental_stats.nt_cache_stores < fresh_stats.nt_cache_stores,
        "append-only advance should store fewer fresh memo entries than a cold parse: fresh={}, incremental={}",
        fresh_stats.nt_cache_stores,
        incremental_stats.nt_cache_stores,
    );
}
