pub mod ast;
pub use ast::*;

pub mod parse;
pub use parse::*;


mod tests {
    use crate::logic::{parser::PartialParser, partial::PartialProduction};


    #[test]
    fn test_debug() {
        crate::set_debug_level(crate::logic::debug::DebugLevel::Trace);
        
        let spec = r#"
        A ::= 'a' 
        B ::= 'b' A 'r'
        start ::= (B 'c')* | 't'
        "#;

        let g = crate::logic::grammar::Grammar::load(spec).unwrap();
        println!("{:#?}",g);
        let mut p = PartialParser::new(g);
        let input = "barcbarc";
        let ast = p.partial(input).unwrap();
        println!("Partial AST: {:#?}", ast);
        let complete = ast.into_complete().unwrap();
        println!("Complete AST: {}", complete.pretty());

    }
}