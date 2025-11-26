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
    crate::set_debug_level(crate::DebugLevel::Debug);

    let grammar =
        crate::logic::grammar::Grammar::load(&xtlc_spec()).expect("Failed to load XTLC grammar");
    crate::debug_info!(
        "test",
        "Loaded grammar with {} rules",
        grammar.typing_rules.len()
    );
    let mut parser = crate::logic::Parser::new(grammar.clone());
    crate::debug_info!("test", "Initialized parser");

    let exprs = ["λx:a->b.x"];

    for expr in exprs {
        crate::set_debug_input(Some(expr.to_string()));

        let past = parser.partial(expr).unwrap();
        println!("Partial AST: {:#?}", past.roots);
    }
}
