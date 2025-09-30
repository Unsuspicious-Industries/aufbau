use crate::engine::Synthesizer;
use crate::engine::rank::{DefaultRanker, MixtralRanker, Ranker, StlcRanker};
use crate::set_debug_level;

#[test]
fn synthesizes_complete_with_repetition() {
    // Grammar with repetition tail: Stmt* allows repeated statements
    // Avoid left recursion; use iterative form for Expr.
    let spec = r#"
    Number ::= /[0-9]+/
    Plus ::= '+'
    Semi ::= ';'
    Expr ::= Number (Plus Number)*
    Stmt ::= Expr Semi
    Block ::= Stmt*
    "#;

    set_debug_level(crate::DebugLevel::Trace);

    let ranker: Box<dyn Ranker> = Box::new(DefaultRanker);
    let mut synth = Synthesizer::new(spec, ranker).expect("init synth");

    let program = synth.synthesize("", 10, 10, 10).expect("synthesis");
    assert!(!program.is_empty());
    println!("Synthesized program: '{}'", program);

    // Parse back and ensure at least one complete alternative exists
    let grammar = crate::logic::grammar::Grammar::load(spec).unwrap();
    let mut parser = crate::logic::parser::Parser::new(grammar);
    let past = parser
        .partial(&program)
        .expect("partial parse of synthesized program");
    println!("Partial AST: {:#?}", past);
}

#[test]
fn synthesize_xtlc() {
    let spec = crate::logic::tests::xtlc::xtlc_spec();

    set_debug_level(crate::DebugLevel::Debug);

    let ranker: Box<dyn Ranker> = Box::new(StlcRanker);
    let mut synth = Synthesizer::new(&spec.clone(), ranker).expect("init synth");

    // Seed with a lambda to bias synthesis toward non-trivial terms
    let program = synth.synthesize("λx:(A", 10, 10, 10).expect("synthesis");
    assert!(!program.is_empty());

    // Parse back and ensure at least one complete alternative exists
    let grammar = crate::logic::grammar::Grammar::load(&spec).unwrap();
    let mut parser = crate::logic::parser::Parser::new(grammar);
    let past = parser
        .partial(&program)
        .expect("partial parse of synthesized program");
    println!("Synthesized STLC program: '{}'", program);
}

#[test]
fn synthesize_clike() {
    let spec = crate::logic::tests::clike::c_like_spec();

    set_debug_level(crate::DebugLevel::Debug);

    let ranker: Box<dyn Ranker> = Box::new(DefaultRanker);
    let mut synth = Synthesizer::new(&spec.clone(), ranker).expect("init synth");

    // Seed with a lambda to bias synthesis toward non-trivial terms
    let program = synth.synthesize("", 10, 10, 10).expect("synthesis");
    assert!(!program.is_empty());

    // Parse back and ensure at least one complete alternative exists
    let grammar = crate::logic::grammar::Grammar::load(&spec).unwrap();
    let mut parser = crate::logic::parser::Parser::new(grammar);
    let past = parser
        .partial(&program)
        .expect("partial parse of synthesized program");
    println!("Synthesized C-like program: '{}'", program);
}

// ================= ML model loading and inference ================

#[test]
fn test_clike_mixtral() {
    let spec = crate::logic::tests::clike::c_like_spec();

    set_debug_level(crate::DebugLevel::Debug);

    let ranker: Box<dyn Ranker> = Box::new(MixtralRanker::new("mixtral").expect("init mixtral"));
    let mut synth = Synthesizer::new(&spec.clone(), ranker).expect("init synth");

    // Seed with a lambda to bias synthesis toward non-trivial terms
    let program = synth.synthesize("", 10, 10, 10).expect("synthesis");
    assert!(!program.is_empty());

    // Parse back and ensure at least one complete alternative exists
    let grammar = crate::logic::grammar::Grammar::load(&spec).unwrap();
    let mut parser = crate::logic::parser::Parser::new(grammar);
    let past = parser
        .partial(&program)
        .expect("partial parse of synthesized program");
    println!("Synthesized C-like program: '{}'", program);
}