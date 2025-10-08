#[cfg(test)]
use crate::logic::{ grammar::Grammar, Parser};
use crate::{DebugLevel, debug_info, set_debug_input, set_debug_level};

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
    debug_info!(
        "test",
        "Loaded grammar with {} rules",
        grammar.typing_rules.len()
    );
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
        // Test nested blocks and scopes
        "int main() {int x = 1; {int x = 2; x = x + 1;} return x;}",
        // Test function with no parameters
        "int get_five() {return 5;} int main() {return get_five();}",
        // Test function with multiple parameters
        "int add(int a, int b) {return a + b;} int main() {return add(3, 4);}",
        // test complex while if else for nested
        r#"int main() {
            int x = 0;
            for (int i = 0; i < 10; i = i + 1) {
                if (i % 2 == 0) {
                    x = x + 1;
                } else {
                    x = x + 2;
                }
            }
            while (x < 30) {
                x = x + 3;
            }
            return x;
        }"#,
    ];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        println!("Parsing expression: {}", expr);
        println!("---==---");
        let past = parser.partial(expr).unwrap();
        println!("Partial AST: {:#?}", past.root);


        let ast = past.into_complete().unwrap();
        println!("AST: {}", ast.pretty());
    }
}

#[test]
fn test_fail_clike() {
    // Enable debug output for this test
    set_debug_level(DebugLevel::Debug);

    let grammar = Grammar::load(&c_like_spec()).expect("Failed to load C-like grammar");
    debug_info!(
        "test",
        "Loaded grammar with {} rules",
        grammar.typing_rules.len()
    );
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");

    let exprs = [r#"int main() {
            int x = 0;
            for (int i = 0; i < 10; i = i + 1) {
                if (i % 2 == 0) {
                    x = x + 1;
                } else {
                    x = x + 2;
                }
            }
            while (x < 30) {
                x = x + 3;
            }"#];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        let _ = parser.parse(expr).unwrap_err();
        println!("---");
    }
}
