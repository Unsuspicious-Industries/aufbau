use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{AltRange, ArenaNode, NodeStatus, Span};
use crate::logic::path::TreePath;
use crate::logic::typing::{ContextTransition, Obligations};

fn grammar_with_rule_obligations() -> Grammar {
    Grammar::load(
        r#"
        Number ::= /[0-9]+/
        Pair(pair) ::= Number[x] ',' Number[x]
        Start ::= Pair

        Γ ⊢ x : 'number'
        ----------------- (pair)
        'number'
        "#,
    )
    .unwrap()
}

#[test]
fn create_obligations_reads_rule_bindings_from_grammar() {
    let grammar = grammar_with_rule_obligations();
    let pair = grammar.nt_index("Pair").unwrap();

    let obligations = Obligations::create(&grammar, (pair, 0), TreePath::new());

    assert_eq!(obligations.len(), 1);
    assert_eq!(obligations.as_slice()[0].name, "x");
    assert_eq!(obligations.as_slice()[0].paths.len(), 2);
    assert!(obligations.root().is_empty());
}

#[test]
fn step_obligations_advances_only_matching_paths() {
    let grammar = grammar_with_rule_obligations();
    let pair = grammar.nt_index("Pair").unwrap();
    let obligations = Obligations::create(&grammar, (pair, 0), TreePath::new());

    let stepped = obligations.step(0, 0);

    assert_eq!(stepped.len(), 1);
    assert_eq!(stepped.as_slice()[0].name, "x");
    assert!(
        stepped.as_slice()[0]
            .paths
            .iter()
            .all(|path| path.is_empty())
    );
}

#[test]
fn step_seed_obligations_filters_to_seed_alternative() {
    let grammar = grammar_with_rule_obligations();
    let pair = grammar.nt_index("Pair").unwrap();
    let obligations = Obligations::create(&grammar, (pair, 0), TreePath::new());

    let seeded = obligations.for_seed(0);

    assert_eq!(seeded.len(), obligations.len());
    let other = obligations.for_seed(1);
    assert!(other.is_empty());
}

#[test]
fn resolve_nonterminal_obligations_captures_child_type() {
    let grammar = grammar_with_rule_obligations();
    let pair = grammar.nt_index("Pair").unwrap();
    let mut obligations = Obligations::create(&grammar, (pair, 0), TreePath::new());
    let node = ArenaNode {
        nt: pair,
        span: Span { start: 0, end: 1 },
        status: NodeStatus::Closed,
        ty: 7,
        ctr: Some(ContextTransition::identity()),
        bindings: Vec::new(),
        alts: AltRange { start: 0, len: 0 },
    };

    obligations.resolve_nonterminal(0, 0, &node);

    let matched = obligations.iter().find(|ob| ob.name == "x").unwrap();
    assert_eq!(matched.actual, Some(7));
    assert!(matched.value.is_some());
}

#[test]
fn constrained_productions_uses_obligation_alternative_constraints() {
    let grammar = grammar_with_rule_obligations();
    let pair = grammar.nt_index("Pair").unwrap();
    let number = grammar.nt_index("Number").unwrap();
    let obligations = Obligations::create(&grammar, (pair, 0), TreePath::new());

    let constrained = obligations.prune(number, &grammar);

    assert_eq!(constrained, vec![(number, 0)]);
}

#[test]
fn child_root_tracks_absolute_tree_position() {
    let grammar = grammar_with_rule_obligations();
    let pair = grammar.nt_index("Pair").unwrap();
    let obligations = Obligations::create(&grammar, (pair, 0), TreePath::new());

    let child = obligations.step(0, 0).at_child(0);

    assert_eq!(child.root().as_slice(), &[0]);
}
