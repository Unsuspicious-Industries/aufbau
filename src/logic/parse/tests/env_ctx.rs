//! Tests for the `env_in` / `env_out` invariant on `ArenaNode`.

use super::*;
use crate::logic::fusion::{TransitionError, TypingRuntime};
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{CtxId, NodeStatus, ProdId, TypeId};
use crate::logic::typing::state::Obligation;

#[derive(Clone, Debug)]
struct CtxExtendingTyping {
    output_ctx: CtxId,
}

impl TypingRuntime for CtxExtendingTyping {
    fn descend(
        &self,
        _prod: ProdId,
        _dot: usize,
        _binding: Option<&str>,
        ctx: CtxId,
        _obligations: &[Obligation],
    ) -> Result<CtxId, TransitionError> {
        Ok(ctx)
    }

    fn finalize(
        &self,
        _prod: ProdId,
        _ctx: CtxId,
        _obligations: &[Obligation],
        _status: NodeStatus,
    ) -> Result<(TypeId, CtxId, bool), TransitionError> {
        Ok((0, self.output_ctx, true))
    }
}

#[test]
fn env_out_reflects_finish_production_output_ctx() {
    let grammar = Grammar::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, CtxExtendingTyping { output_ctx: 42 });

    let initial_ctx = 0;
    let ast = parser.parse("a", initial_ctx).unwrap();

    assert!(!ast.is_empty(), "should produce at least one root");

    let arena = ast.arena();
    let root_id = ast.root_ids()[0];
    let node = arena.node(root_id).expect("root node must exist");

    assert_eq!(node.env_in, Some(initial_ctx));
    assert_eq!(node.env_out, Some(42));
}

#[test]
fn env_out_differs_from_env_in_when_production_extends_context() {
    let grammar = Grammar::load("Start ::= 'x'").unwrap();
    let mut parser = TypedParser::new(grammar, CtxExtendingTyping { output_ctx: 99 });
    let ast = parser.parse("x", 0).unwrap();

    assert!(!ast.is_empty());

    let arena = ast.arena();
    let root = arena.node(ast.root_ids()[0]).unwrap();
    assert_ne!(root.env_in, root.env_out);
}

#[test]
fn env_out_equals_env_in_when_production_preserves_context() {
    let grammar = Grammar::load("Start ::= 'a'").unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let ast = parser.parse("a", 0).unwrap();

    assert!(!ast.is_empty());

    let arena = ast.arena();
    let root = arena.node(ast.root_ids()[0]).unwrap();
    assert_eq!(root.env_in, root.env_out);
}

#[test]
fn env_out_from_child_becomes_right_sibling_ctx() {
    let grammar = Grammar::load("A ::= 'x'\nB ::= 'y'\nStart ::= A B").unwrap();
    let mut parser = TypedParser::new(grammar, CtxExtendingTyping { output_ctx: 7 });
    let ast = parser.parse("x y", 0).unwrap();

    assert!(ast.is_complete());

    let arena = ast.arena();
    let root = arena.node(ast.root_ids()[0]).unwrap();
    assert_eq!(root.env_in, Some(0));
    assert_eq!(root.env_out, Some(7));
}
