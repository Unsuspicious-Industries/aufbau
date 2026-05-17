use crate::DebugLevel;
use crate::logic::SemanticRuntime;
use crate::logic::error::TransitionError;
use crate::logic::grammar::Grammar;
use crate::logic::parse::NodeStatus;
use crate::logic::parse::arena::{CtxId, EffectId, EvidenceId, ProdId};
use crate::set_debug_level;

use super::*;

mod utils;
pub(crate) use utils::*;

mod dedup;
mod frontier;
mod parse;
mod prune;
mod repro;
mod status;

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
