pub mod parse;
pub use parse::*;

pub mod structure;
// Export new types with different names to avoid conflicts
pub use structure::{Node, NonTerminal, PartialAST, Terminal};

pub mod completion;
pub use completion::*;

pub mod display;

pub mod binding;
pub use binding::*;

pub mod typing;
pub use typing::{TypedAST, TypedNode};

mod tests {

    #[test]
    fn test_debug() {
        crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);

        let spec = r#"
    U ::= 'barcbarcu'
    A ::= 'a' 
    B ::= 'b' A 'r'
    Loop ::= B 'c' Loop | B 'c'
    start ::= U | Loop | 't' 
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        println!("Grammar: {:#?}", g);
        let mut p = crate::logic::partial::Parser::new(g);
        let input = "barcbarc";
        let ast = p.partial(input).unwrap();
        println!("Partial AST: {}", ast);
        let complete = ast.into_complete().unwrap();
        println!("Complete AST: {}", complete.pretty());
    }

    #[test]
    fn test_complete_len() {
        // Test that complete_len correctly computes segment range for complete alternatives
        let spec = r#"
        A ::= 'hello'
        B ::= 'world'
        start ::= A  B
        "#;

        crate::set_debug_level(crate::DebugLevel::Trace);

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::Parser::new(g);

        // Test complete parse
        let input = "hello world";
        println!("Input: {}", input);
        let ast = p.partial(input).unwrap();

        assert!(ast.complete(), "AST should be complete");

        // Tokenize to get segments
        let tokenizer = crate::logic::tokenizer::Tokenizer::new(
            vec!["hello".to_string(), "world".to_string()],
            vec![' ', '\n', '\t'],
            None,
        );
        let segments = tokenizer.tokenize(input).unwrap();

        // Get the complete alternative's segment range
        let root = ast
            .roots
            .iter()
            .find(|r| r.is_complete())
            .expect("Expected a complete root");
        let range = root.complete_len(&segments);
        assert!(range.is_some(), "Complete root should have a segment range");

        if let Some(seg_range) = range {
            // Convert to byte range to verify coverage
            let (start_byte, end_byte) = seg_range.to_byte_range(&segments).unwrap();
            assert_eq!(
                end_byte - start_byte,
                11,
                "Should cover all 11 bytes of 'hello world'"
            );
        }
    }

    #[test]
    fn test_complete_len_partial() {
        // Test that complete_len returns None for partial alternatives
        let spec = r#"
        start ::= 'complete' 'sentence'
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::Parser::new(g);

        // Partial input
        let input = "complete";
        let ast = p.partial(input).unwrap();

        let tokenizer = crate::logic::tokenizer::Tokenizer::new(
            vec!["complete".to_string(), "sentence".to_string()],
            vec![' ', '\n', '\t'],
            None,
        );
        let segments = tokenizer.tokenize(input).unwrap();

        // The AST may have partial alternatives
        // Check if any root claims to be complete (should be none)
        let complete_root = ast.roots.iter().find(|r| r.is_complete());
        assert!(
            complete_root.is_none(),
            "Partial parse should not have complete root"
        );

        // Even if we check complete_len on a partial root, it should return None
        if let Some(root) = ast.roots.first() {
            let range = root.complete_len(&segments);
            assert_eq!(
                range, None,
                "Partial parse should return None for complete_len"
            );
        }
    }

    #[test]
    fn test_complete_len_nested() {
        // Test complete_len with nested nonterminals
        let spec = r#"
        Inner ::= 'foo'
        Outer ::= Inner 'bar'
        start ::= Outer
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        let mut p = crate::logic::partial::Parser::new(g);

        let input = "foobar";
        let ast = p.partial(input).unwrap();

        assert!(ast.complete(), "Nested parse should be complete");

        let tokenizer = crate::logic::tokenizer::Tokenizer::new(
            vec!["foo".to_string(), "bar".to_string()],
            vec![' ', '\n', '\t'],
            None,
        );
        let segments = tokenizer.tokenize(input).unwrap();

        let root = ast
            .roots
            .iter()
            .find(|r| r.is_complete())
            .expect("Expected a complete root");
        let range = root.complete_len(&segments);
        assert!(
            range.is_some(),
            "Complete nested alt should have a segment range"
        );

        if let Some(seg_range) = range {
            let (start_byte, end_byte) = seg_range.to_byte_range(&segments).unwrap();
            assert_eq!(
                end_byte - start_byte,
                6,
                "Should cover all 6 bytes of 'foobar'"
            );
        }
    }
}
