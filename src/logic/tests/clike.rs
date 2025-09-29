#[cfg(test)]

use crate::logic::{
    grammar::Grammar, 
    parser::Parser, 
    check::TypeChecker, 
};
use crate::{debug_info, set_debug_level, set_debug_input, DebugLevel};

pub fn c_like_spec() -> String {
    use std::path::Path;
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let path = Path::new(manifest_dir).join("examples").join("clike.spec");
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read C-like spec at {:?}: {}", path, e))
}

#[test]
fn test_pass_clike() {

    // Enable debug output for this test
    set_debug_level(DebugLevel::Trace);

    let grammar = Grammar::load(&c_like_spec()).expect("Failed to load C-like grammar");
    debug_info!("test", "Loaded grammar with {} rules", grammar.typing_rules.len());
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");
    // Debug: Print all loaded psroductions
    println!("=== LOADED PRODUCTIONS ===");
    for (nt, prods) in &grammar.productions {
        println!("Nonterminal: {}", nt);
        for (i, prod) in prods.iter().enumerate() {
            println!("  Production {}: {:?}", i, prod);
        }
    }
    println!("=== END PRODUCTIONS ===");
    

    let exprs = [
        "int main() {return 10;}",
        "int main() {int x = 5; return x;}",
        "int main() {int x = 5; int y = x + 2; return y;}",
        "int main() {if (1) {return 10;} return 0;}",
        "int main() {if (1) {return 10;} else {return 20;}}",
        "int main() {int x = 0; while (x < 10) {x = x + 1;} return x;}",
        "int main() {for (int i = 0; i < 10; i = i + 1) {} return 0;}",
        "int add(int a, int b) {return a + b;} int main() {return add(3, 4);}",
        // Test variable declarations
        "int x = 5;",
        "float y;",
        "int main() {int a; int b = 10; return b+a;}",
    ];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        let mut tc = TypeChecker::new();
        debug_info!("test", "Initialized type checker");
        

        println!("Parsing expression: {}", expr);
        println!("---==---");
        let past = parser.partial(expr).unwrap();
        let pasts = past.divide();
        for (i, part) in pasts.iter().enumerate() {
            //debug_info!("test", "Part {}: {:#?}", i, part);
        }

        let ast = past.into_complete().unwrap();

        let rt = tc.check(&ast).unwrap();
        if let Some(ty) = rt {
            println!("return type: {:?}", ty);
        } else {
            println!("no return type");
        }
        println!("---");

        println!("AST: {}", ast.pretty());

    }
}

#[test]
fn test_fail_clike() {
    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);

    let grammar = Grammar::load(&c_like_spec()).expect("Failed to load C-like grammar");
    debug_info!("test", "Loaded grammar with {} rules", grammar.typing_rules.len());
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");

    let exprs = [
        "int main() {return \"r\";}",
        "int main() {int x = 5; return y;}",
        // Test variable declaration type errors
        "int x = \"string\";",  // type mismatch
        "float z = true;",     // type mismatch
    ];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        let mut tc = TypeChecker::new();
        debug_info!("test", "Initialized type checker");

        let ast = parser.parse(expr).unwrap();
        debug_info!("test", "AST: {}", ast.pretty());
        let err = tc.check(&ast).unwrap_err();

        println!("Type error: {}", err);
        println!("---");
    }
}