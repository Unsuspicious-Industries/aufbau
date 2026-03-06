// Typing tests for STLC via the λ-calculus (lc) grammar.
//
// ## Parser depth bounds
//
// MetaParser defaults to max_depth=256, causing OOM under parallel tests.
// We use STLC_MAX_DEPTH=62 — the same cap as redeclaration.rs.  See that
// module for the full geometric-ladder analysis and per-input observations.
//
// HACKY: no formal proof this is tight for all STLC inputs.

// HACKY depth cap — see module comment above.
const STLC_MAX_DEPTH: usize = 62;
use crate::logic::grammar::Grammar;
use crate::logic::partial::MetaParser;
use crate::logic::typing::Context;
use crate::set_debug_level;
use crate::validation::completable::load_example_grammar;

fn lc() -> Grammar {
    load_example_grammar("stlc")
}

#[test]
fn test_identity() {
    // P => P
    let g = lc();
    let mut p = MetaParser::new(g.clone()).with_max_depth(STLC_MAX_DEPTH);
    set_debug_level(crate::DebugLevel::Trace);
    let mut ctx = Context::new();
    ctx.add(
        "y".to_string(),
        crate::logic::typing::Type::parse_raw("B").unwrap(),
    );
    ctx.add(
        "x".to_string(),
        crate::logic::typing::Type::parse_raw("A").unwrap(),
    );
    ctx.add(
        "f".to_string(),
        crate::logic::typing::Type::parse_raw("A -> B -> C").unwrap(),
    );

    let ast = match p.partial_typed_ctx(
        r#"
            f x y
            "#,
        &ctx,
    ) {
        Ok(t) => t,
        Err(e) => {
            println!("Parse error: {}", e);
            panic!("Failed to parse");
        }
    };

    set_debug_level(crate::DebugLevel::Info);
    assert!(ast.complete().ok().is_some(), "Identity should be provable");
}
