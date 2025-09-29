
use crate::logic::grammar::Grammar;
use crate::logic::parser::Parser;
use crate::logic::check::TypeChecker;
use crate::{debug_info, set_debug_level, set_debug_input, DebugLevel};

// Extended Typed Lambda Calculus
pub fn xtlc_spec() -> String {
    use std::path::Path;
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples").join("xtlc.spec");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read XTLC spec at {:?}: {}", path, e))
}

#[test]
fn test_pass_xtlc() {

    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);

    let grammar = Grammar::load(&xtlc_spec()).expect("Failed to load XTLC grammar");
    debug_info!("test", "Loaded grammar with {} rules", grammar.typing_rules.len());
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");
    // Debug: Print all loaded productions
    println!("=== LOADED PRODUCTIONS ===");
    for (nt, prods) in &grammar.productions {
        println!("Nonterminal: {}", nt);
        for (i, prod) in prods.iter().enumerate() {
            println!("  Production {}: {:?}", i, prod);
        }
    }
    println!("=== END PRODUCTIONS ===");
    

    let exprs = [
        "λx:a->b.x",
    ];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        let mut tc = TypeChecker::new();
        debug_info!("test", "Initialized type checker");

        let past = parser.partial(expr).unwrap();
        let rt = tc.check_partial(past.root()).unwrap();

        if let Some(ty) = rt {
            println!("return type: {:?}", ty);
        } else {
            println!("no return type");
        }
        println!("---");
    }
}