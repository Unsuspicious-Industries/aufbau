use crate::set_debug_level;
use crate::DebugLevel;
use crate::logic::fusion::{NodeStatus, TransitionError, TypingRuntime};
use crate::logic::grammar::Grammar;
use crate::logic::parse::arena::{CtxId, ProdId, TypeId};

use super::*;

mod utils;
pub(crate) use utils::*;

mod dedup;
mod env_ctx;
mod frontier;
mod parse;
mod prune;
mod repro;

#[test]
fn debug_smoke() {
    set_debug_level(DebugLevel::Trace);
    let grammar = Grammar::load(
        r#"
    
    A ::= A ':' A | 'a'
    Start ::= A
    "#,
    )
    .unwrap();
    let mut parser = TypedParser::new(grammar, StubTyping);
    let res = parser.parse("a : ", 0).unwrap();
    assert!(!res.is_empty());
}
