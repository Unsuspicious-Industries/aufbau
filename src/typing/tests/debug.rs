#[cfg(test)]
mod debug_tests {
    use crate::engine::grammar::SPG;
    use crate::typing::{Context, Type, TypingSynth};

    fn load_stlc() -> SPG {
        let src = include_str!("../../../examples/stlc.auf");
        SPG::load(src).unwrap()
    }

    #[test]
    fn parse_lambda_identity_works() {
        let mut synth = TypingSynth::new(load_stlc(), "λx:A.x");
        let result = synth.parse_with(&Context::new());
        assert!(result.is_ok(), "λx:A.x should parse: {:?}", result);
    }

    #[test]
    fn parse_arrow_lambda_works() {
        let mut synth = TypingSynth::new(load_stlc(), "λf:A->B.f");
        let result = synth.parse_with(&Context::new());
        assert!(result.is_ok(), "λf:A->B.f should parse: {:?}", result);
    }

    #[test]
    fn parse_application_with_context_works() {
        let g = load_stlc();
        let mut ctx = Context::new();
        ctx.add("f".into(), Type::parse(&g, "A->B").unwrap());
        ctx.add("x".into(), Type::parse(&g, "A").unwrap());
        let mut synth = TypingSynth::new(g, "(f x)");
        let result = synth.parse_with(&ctx);
        assert!(
            result.is_ok(),
            "(f x) should parse with context: {:?}",
            result
        );
    }

    #[test]
    fn parse_chained_application_works() {
        let g = load_stlc();
        let mut ctx = Context::new();
        ctx.add("f".into(), Type::parse(&g, "A->B->C").unwrap());
        ctx.add("x".into(), Type::parse(&g, "A").unwrap());
        ctx.add("y".into(), Type::parse(&g, "B").unwrap());
        let mut synth = TypingSynth::new(g, "f x y");
        let result = synth.parse_with(&ctx);
        assert!(result.is_ok(), "f x y should parse: {:?}", result);
    }

    #[test]
    fn parse_paren_app_works() {
        let g = load_stlc();
        let mut ctx = Context::new();
        ctx.add("f".into(), Type::parse(&g, "A->B").unwrap());
        ctx.add("x".into(), Type::parse(&g, "A").unwrap());
        let mut synth = TypingSynth::new(g, "(f x)");
        match synth.parse_with(&ctx) {
            Ok(ast) => {
                let arena = ast.arena();
                for &root_id in ast.root_ids() {
                    if let Some(node) = arena.node(root_id) {
                        let ty = synth.runtime().evidence_of(node.evidence);
                        println!(
                            "root {} nt={:?} ty={:?}",
                            root_id,
                            synth.grammar().nt(node.nt),
                            ty
                        );
                    }
                }
            }
            Err(e) => panic!("FAIL: {:?}", e),
        }
    }
}
