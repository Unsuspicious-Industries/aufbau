use super::*;
use crate::logic::fusion::Synthesizer;
use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;
use crate::set_debug_level;

fn load(spec: &str) -> Grammar {
    Grammar::load(spec).unwrap()
}

#[test]
fn score_prefers_more_complete() {
    let grammar = load(
        r#"
        Identifier ::= /[a-z]+/
        Item ::= 'let' Identifier | 'let' Identifier ':' 't' '=' Identifier ';' Identifier
        Start ::= Item
        "#,
    );

    let mut partial = Synthesizer::new_with_max_depth(grammar.clone(), "let x", 8);
    let partial_ast = partial.parse_with(&Context::new()).unwrap();

    let mut deeper = Synthesizer::new_with_max_depth(grammar.clone(), "let x: t = y;", 8);
    let deeper_ast = deeper.parse_with(&Context::new()).unwrap();

    let partial_score = score(&partial_ast.view(), &grammar);
    let deeper_score = score(&deeper_ast.view(), &grammar);

    assert!(deeper_score.open_slots >= partial_score.open_slots);
    assert!(deeper_score.fullness >= partial_score.fullness);
    assert!(deeper_score.terminals >= partial_score.terminals);
}

#[test]
fn completes_structural_prefix() {
    set_debug_level(crate::DebugLevel::Debug);
    let grammar = load(
        r#"
        Identifier ::= /[a-z]+/
        Expr ::= 'begin' Identifier ';' Identifier 'end' | "!" Identifier
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "begin", 6, Some(Context::new()));
    assert!(matches!(result, CompletionResult::Success { .. }));
}

#[test]
fn keeps_completion_generic() {
    let grammar = load(
        r#"
        Name ::= /[A-Za-z]+/
        Ty ::= /[A-Za-z]+/
        Expr ::= 'λ' Name ':' Ty '.' Name
        Start ::= Expr
        "#,
    );

    let result = complete(&grammar, "λ", 8, Some(Context::new()));
    match result {
        CompletionResult::Success { complete_input, .. } => {
            assert!(complete_input.starts_with('λ'));
            assert!(complete_input.contains(':'));
            assert!(complete_input.contains('.'));
        }
        other => panic!("expected success, got {other:?}"),
    }
}

#[test]
fn uses_incremental_advance() {
    let grammar = load(
        r#"
        Start ::= 'x' 'y'
        "#,
    );
    let ctx = Context::new();
    let searcher = Searcher::new(grammar.clone(), 8);
    let ctx_id = searcher.runtime.intern_context(ctx.clone());

    let initial = searcher.parse("x", ctx_id).unwrap();
    let initial_nodes = initial.parser.arena().node_count();

    let next = extend(&initial, grammar.extend_input("x", "y"), &grammar).unwrap();

    assert!(next.view().is_complete());
    assert!(next.parser.arena().node_count() > initial_nodes);
}
