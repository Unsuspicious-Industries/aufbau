use crate::domains::typing::TypingDomain;
use crate::engine::grammar::SPG;
use crate::engine::parse::{ParseArena, State};

use super::ast::FusionAST;

#[test]
fn empty_ast_reports_empty_view() {
    let ast = FusionAST::<TypingDomain>::new(
        SPG::<TypingDomain>::new(),
        ParseArena::new(),
        Vec::new(),
        Vec::new(),
        String::new(),
    );

    assert!(ast.is_empty());
    assert_eq!(ast.len(), 0);
    assert_eq!(ast.text(), "");
    assert!(ast.first().is_none());
}

#[test]
fn state_display_exposes_root_and_frontier_size() {
    let state = State::default();
    let rendered = state.to_string();

    assert!(rendered.contains("root=0"));
    assert!(rendered.contains("frontier=0"));
}
