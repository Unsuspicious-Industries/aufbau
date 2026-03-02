use crate::logic::grammar::Grammar;
use crate::logic::partial::parse::Parser;
use crate::logic::typing::core::Context;
use crate::logic::typing::Type;

#[test]
fn test_f_x() {
    crate::set_debug_level(crate::DebugLevel::Trace);
    let spec = std::fs::read_to_string("examples/stlc.auf").unwrap();
    let g = Grammar::load(&spec).unwrap();
    let mut p = Parser::new(g.clone());
    
    let mut ctx = Context::new();
    ctx.add("f".to_string(), Type::parse("A->B").unwrap());
    ctx.add("x".to_string(), Type::parse("A").unwrap());
    
    let ast = p.partial("f x").unwrap();
    
    let typed = ast.filter_typed_ctx(&g, &ctx);
    println!("typed: {:?}", typed);
}
