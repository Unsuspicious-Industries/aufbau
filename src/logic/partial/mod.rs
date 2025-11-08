pub mod parse;
pub use parse::*;

pub mod production;
pub use production::{PartialProduction, PartialSymbol};

pub mod structure;
// Export new types with different names to avoid conflicts
pub use structure::{Alt, NonTerminal, ParsedNode, PartialAST, Slot, Terminal};

pub mod completion;
pub use completion::*;

pub mod display;


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
        println!("Grammar: {:#?}", g);
        let mut p = crate::logic::partial::Parser::new(g);
        let input = "barcbarc";
        let ast = p.partial(input).unwrap();
        println!("Partial AST: {}", ast);
        let complete = ast.into_complete().unwrap();
        println!("Complete AST: {}", complete.pretty());
    }
}
