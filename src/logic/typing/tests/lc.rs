use crate::logic::grammar::Grammar;
use crate::logic::partial::MetaParser;
use crate::logic::typing::Context;
use crate::logic::typing::eval::check_tree;
use crate::set_debug_level;
use crate::validation::completable::load_example_grammar;

fn lc() -> Grammar {
    load_example_grammar("stlc")
}

#[test]
fn test_identity() {
    // P => P
    let g = lc();
    let mut p = MetaParser::new(g.clone());
    set_debug_level(crate::DebugLevel::Trace);
    let mut ctx = Context::new();
    ctx.add("y".to_string(), crate::logic::typing::Type::parse_raw("B").unwrap());
    ctx.add("x".to_string(), crate::logic::typing::Type::parse_raw("A").unwrap());
    ctx.add("f".to_string(), crate::logic::typing::Type::parse_raw("A -> B -> C").unwrap());

    let ast = match p       
        .partial_typed_ctx(
            r#"
            f x y
            "#,
            &ctx
        ) {     
            Ok(t) => t,
            Err(e) => {
                println!("Parse error: {}", e);
                panic!("Failed to parse");
            }
    };
        
    set_debug_level(crate::DebugLevel::Info);
    assert!(ast.is_complete(), "Identity should be provable");
    let complete = ast.completes();
    for c in complete {
        println!("{}", &c);
        let typed = c.typed(&g);
        if let Some(typed) = typed {
            println!("{}", typed);
        } else {
            // check tree
            set_debug_level(crate::DebugLevel::Trace);
            let status = check_tree(&c, &g);
            print!("{:#?}", status);
        }
    }
}
