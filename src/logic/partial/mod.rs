pub mod ast;
pub use ast::{PartialAST, PartialASTNode, PartialNonTerminal, PartialTerminal};

pub mod parse;
pub use parse::*;

pub mod production;
pub use production::{PartialProduction, PartialSymbol};

pub mod completion;
pub use completion::*;

pub mod structure;
pub use structure::*;



mod tests {
    #[test]
    fn test_debug() {
        crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);

        let spec = r#"
        U ::= 'barcbarcu'
        A ::= 'a' 
        B ::= 'b' A 'r'
        start ::= U | (B 'c')* | 't' 
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        println!("{:#?}", g);
        let mut p = crate::logic::partial::PartialParser::new(g);
        let input = "barcbarc";
        let ast = p.partial(input).unwrap();
        println!("Partial AST: {:#?}", ast);
        let complete = ast.into_complete().unwrap();
        println!("Complete AST: {}", complete.pretty());
    }
}
