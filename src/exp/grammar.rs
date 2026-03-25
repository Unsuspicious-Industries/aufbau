// grammar experiments

#![allow(unused_imports)]
#[cfg(test)]
use crate::{
    logic::{grammar::*, Parser},
    set_debug_level,
    testing::load_example_grammar,
};

// define a simple grammar
const SIMPLE: &str = r#"
A ::= "a" | "b"
B ::= "c" | "d"
W ::= '(' W ')' | A B
S ::= W B | W A
"#;

#[test]
fn test_simple() {
    let grammar = Grammar::load(SIMPLE).unwrap();
    println!("{:#?}", grammar);
    let mut p = Parser::new(grammar);
    set_debug_level(crate::DebugLevel::Trace);
    let expr = "(((((a c))))) c";
    let ast = p.parse(expr).unwrap();
    println!("Parsed: {}", ast);
    println!(
        "Height: {}",
        ast.roots().first().map(|r| r.height()).unwrap_or(0)
    );
}
#[cfg(test)]
#[test]
fn test_stlc() {
    let grammar = load_example_grammar("stlc");
    println!("{:#?}", grammar);
    let mut p = Parser::new(grammar);
    set_debug_level(crate::DebugLevel::Trace);
    let expr = "f x y";
    let ast = p.parse(expr).unwrap();
    println!("Parsed: {}", ast);
    println!(
        "Height: {}",
        ast.roots().first().map(|r| r.height()).unwrap_or(0)
    );
}
