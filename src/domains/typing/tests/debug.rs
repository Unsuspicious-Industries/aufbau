#[cfg(test)]
mod debug_tests {
    use crate::domains::typing::TypingDomain;
    use crate::domains::typing::{Context, Type, TypingSynth};
    use crate::engine::grammar::SPG;

    fn load_stlc() -> SPG<TypingDomain> {
        let src = include_str!("../../../../examples/stlc.auf");
        SPG::<TypingDomain>::load(src).unwrap()
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
        let mut ctx = Context::new();
        ctx.add("f".into(), Type::parse_raw("A->B").unwrap());
        ctx.add("x".into(), Type::parse_raw("A").unwrap());
        let mut synth = TypingSynth::new(load_stlc(), "(f x)");
        let result = synth.parse_with(&ctx);
        assert!(
            result.is_ok(),
            "(f x) should parse with context: {:?}",
            result
        );
    }

    #[test]
    fn parse_chained_application_works() {
        let mut ctx = Context::new();
        ctx.add("f".into(), Type::parse_raw("A->B->C").unwrap());
        ctx.add("x".into(), Type::parse_raw("A").unwrap());
        ctx.add("y".into(), Type::parse_raw("B").unwrap());
        let mut synth = TypingSynth::new(load_stlc(), "f x y");
        let result = synth.parse_with(&ctx);
        assert!(result.is_ok(), "f x y should parse: {:?}", result);
    }

    #[test]
    fn parse_paren_app_works() {
        let mut ctx = Context::new();
        ctx.add("f".into(), Type::parse_raw("A->B").unwrap());
        ctx.add("x".into(), Type::parse_raw("A").unwrap());
        let mut synth = TypingSynth::new(load_stlc(), "(f x)");
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
