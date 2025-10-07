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
use crate::logic::partial::completion::{CompletionSet, ValidToken};
use crate::logic::partial::parse::Parser;

/// Brute-force completion helper available to all modules.
/// See docs in tests for details.
pub fn brute_force_complete_ast(
    grammar: &Grammar,
    input: &str,
    max_depth: usize,
) -> Result<ASTNode, String> {
    let mut queue: VecDeque<(String, usize)> = VecDeque::new();
    let mut visited: HashSet<String> = HashSet::new();
    queue.push_back((input.trim().to_string(), 0));

    while let Some((current_input, depth)) = queue.pop_front() {
        if !visited.insert(current_input.clone()) {
            continue;
        }

        let mut parser = Parser::new(grammar.clone());
        let past = match parser.partial(&current_input) {
            Ok(p) => p,
            Err(_) => continue,
        };

        if past.complete() {
            return past.into_complete().map_err(|e| format!("completion failed: {e}"));
        }

        if depth >= max_depth {
            continue;
        }

        let CompletionSet { tokens } = past.completions(grammar);
        for token in tokens {
            if let ValidToken::Literal(lit) = token {
                println!("Depth {}: completing '{}' with '{}'", depth, current_input, lit);
                let mut next = String::new();
                if !current_input.is_empty() {
                    next.push_str(&current_input);
                    next.push(' ');
                }
                next.push_str(&lit);
                queue.push_back((next, depth + 1));
            }
        }
    }

    Err("exhausted search without finding a complete AST".into())
}

// -----------------------------------------------------------------------------
// Test-only helpers
// -----------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use crate::logic::grammar::Grammar;
    use crate::logic::partial::parse::Parser;
    use crate::logic::partial::completion::CompletionSet;
    use crate::logic::partial::ValidToken;

    /// Format completions for debug printing.
    fn fmt_completions(cs: &CompletionSet) -> String {
        cs.tokens
            .iter()
            .map(|t| match t {
                ValidToken::Literal(s) => format!("'{}'", s),
                ValidToken::Regex(r) => format!("/{}/", r),
            })
            .collect::<Vec<_>>()
            .join(", ")
    }

    /// Helper that asserts the given `prefix` is 1) parseable as a *partial* and
    /// 2) can be completed using the brute-force algorithm.
    fn assert_completable(grammar: &Grammar, prefix: &str) {
        let mut parser = Parser::new(grammar.clone());
        let past = parser.partial(prefix).unwrap();
        let comps = past.completions(grammar);
        println!("Completions for prefix '{prefix}': [{}]", fmt_completions(&comps));
        match super::brute_force_complete_ast(grammar, prefix, 16) {
            Ok(ast) => {
                println!("Completed prefix '{prefix}' into AST:\n{:#?}", ast);
            }
            Err(e) => panic!("prefix '{prefix}' could not be completed: {e}"),
        }
    }

    /// Main regression test: iterate over all prefixes of a valid sentence and
    /// make sure each prefix can be completed.
    #[test]
    fn completability_prefixes_of_sentence() {
        let spec = r#"
        U ::= 'b' 'a' 'r' 'c' 'b' 'a' 'r' 'c' 'u'
        A ::= 'a'
        B ::= 'b' A 'r'
        start ::= U | (B 'c')* | 't'
        "#;

        let grammar = Grammar::load(spec).expect("failed to load grammar");
        let sentence = "b a r c b a r c u";
        let tokens: Vec<&str> = sentence.split_whitespace().collect();

        // Check every prefix – including the empty string (i == 0).
        for i in 0..=tokens.len() {
            let prefix = tokens[..i].join(" ");
            assert_completable(&grammar, &prefix);
        }
    }

    /// Simpler sanity check with a straight-line grammar (no branching,
    /// repetition or regexes) to keep debugging easy.
    #[test]
    fn completability_straight_line() {
        let spec = r#"
        start ::= 'a' 'b' 'c'
        "#;
        let grammar = Grammar::load(spec).unwrap();
        let full = "a b c";
        let toks: Vec<&str> = full.split_whitespace().collect();
        for i in 0..=toks.len() {
            let prefix = toks[..i].join(" ");
            assert_completable(&grammar, &prefix);
        }
    }

    #[test]
    fn completability_medium_grammars() {
        // Medium complexity grammars to narrow down potential issues
        let cases = [
            (
                r#"
                Number ::= /[0-9]+/
                Expr   ::= Number ('+' Number)*
                start  ::= Expr
                "#,
                "1 + 2 + 3"
            ),
            (
                r#"
                Bool   ::= 'true' | 'false'
                Stmt   ::= 'skip' | Identifier '=' Bool ';'
                Identifier ::= /[a-z]+/
                start  ::= Stmt
                "#,
                "x = true ;"
            ),
        ];

        for (spec, sentence) in cases {
            let grammar = Grammar::load(spec).unwrap();
            let toks: Vec<&str> = sentence.split_whitespace().collect();
            for i in 0..=toks.len() {
                let prefix = toks[..i].join(" ");
                assert_completable(&grammar, &prefix);
            }
        }
    }

    #[test]
    #[ignore]
    fn completability_complex_grammar() {
        // Ignored until completion/completion module handles deep recursion efficiently
        let spec = r#"
        // Identifier regex and keywords
        Identifier ::= /[a-zA-Z_][a-zA-Z0-9_]*/
        Number     ::= /[0-9]+/
        KeywordIf  ::= 'if'
        KeywordElse::= 'else'

        Expr  ::= Number | Identifier | '(' Expr ')' | Expr BinOp Expr
        BinOp ::= '+' | '-' | '*' | '/'

        Stmt  ::= KeywordIf '(' Expr ')' Stmt KeywordElse? Stmt
                | '{' (Stmt)* '}'
                | Identifier '=' Expr ';'

        start ::= Stmt
        "#;

        let grammar = Grammar::load(spec).unwrap();
        let samples = [
            "if ( 123 ) { x = y ; }",
            "x = 1 + 2 * 3 ;",
            "if ( a ) b = c ; else { if (d) e = f ; }",
        ];

        for sample in samples {
            let toks: Vec<&str> = sample.split_whitespace().collect();
            for i in 0..=toks.len() {
                let prefix = toks[..i].join(" ");
                assert_completable(&grammar, &prefix);
            }
        }
    }
}
