use crate::DebugLevel;
use crate::domains::typing::TypingDomain;
use crate::semantics::SemanticRuntime;
use crate::engine::error::TransitionError;
use crate::engine::grammar::SPG;
use crate::engine::parse::NodeStatus;
use crate::engine::parse::arena::{CtxId, EffectId, EvidenceId, ProdId};
use crate::set_debug_level;

use super::*;

mod utils;
pub(crate) use utils::*;

mod dedup;
mod frontier;
mod parse;
mod proptest;
mod prune;
mod repro;
mod status;

#[test]
fn debug_smoke() {
    set_debug_level(DebugLevel::Trace);
    let grammar = SPG::<TypingDomain>::load(
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
