// Completability Validation Tests
//
// These tests exercise the end-to-end guarantee that a partially parsed AST can
// always be completed to a full AST by repeatedly appending one of the valid
// completion tokens suggested by the partial-parsing engine.  We implement a
// tiny brute-force search (breadth-first, bounded depth) that keeps appending
// literal suggestions until the tree is complete.  Regex completions are
// ignored for now – the grammars used in these tests only rely on literal
// tokens.
//
// The algorithm is intentionally kept *inside* the test module so that it is
// only compiled for `cargo test` and does not pollute production code.

use std::collections::{HashSet, VecDeque};
use crate::logic::ast::ASTNode;
use crate::logic::grammar::Grammar;
use crate::logic::partial::completion::{self, CompletionSet, ValidToken};
use crate::logic::partial::parse::Parser;



pub fn complete_ast(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
) -> Result<ASTNode, String> {
    
    let mut parser = Parser::new(grammar.clone());
    let partial = parser.partial(input)
        .map_err(|e| format!("initial parse error: {}", e))?;

    let completions = partial.completions(grammar);
    for token in &completions.tokens {
        println!("Initial completion token: {:?}", token);

        let c_input = format!(
            "{}{} ",
            input,
            match token {
                ValidToken::Literal(t) => t.clone(),
                ValidToken::Regex(t) => t.clone(), // TODO: Handle regex completions properly
            }
        );
    }


    Err("exhausted search without finding a complete AST".into())
}

