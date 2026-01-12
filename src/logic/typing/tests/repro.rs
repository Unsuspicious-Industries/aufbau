//! Reproduction tests for specific parsing issues

#![allow(unused_imports)]

use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::partial::structure::NonTerminal;
use crate::logic::typing::Type;
use crate::logic::typing::core::Context;
use crate::logic::typing::eval::{check_tree, check_tree_with_context};

#[test]
fn test_parse_silent_var_arrow() {
    let t = Type::parse("?A -> ?B");
    assert!(t.is_ok(), "Failed to parse ?A -> ?B: {:?}", t.err());
}
