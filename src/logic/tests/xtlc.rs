use crate::logic::Parser;
use crate::logic::grammar::Grammar;
use crate::{DebugLevel, debug_info, set_debug_input, set_debug_level};

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
    set_debug_level(DebugLevel::Debug);

    let grammar = Grammar::load(&xtlc_spec()).expect("Failed to load XTLC grammar");
    debug_info!(
        "test",
        "Loaded grammar with {} rules",
        grammar.typing_rules.len()
    );
    let mut parser = Parser::new(grammar.clone());
    debug_info!("test", "Initialized parser");

    let exprs = ["λx:a->b.x"];

    for expr in exprs {
        set_debug_input(Some(expr.to_string()));

        let past = parser.partial(expr).unwrap();
        println!("Partial AST: {:#?}", past.root);
    }
}
