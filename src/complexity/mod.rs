//! Bounded empirical complexity tests.
//!
//! The implementation is not a benchmark harness. These tests are deterministic
//! smoke monitors for the parser's expected operating regime on representative
//! grammars. They assert both semantic behavior and a generous time/node budget
//! so regressions are visible in CI without making normal debug builds flaky.

#[cfg(test)]
mod tests {
    use std::time::{Duration, Instant};

    use rand::rngs::StdRng;
    use rand::{Rng, SeedableRng};

    use crate::domains::typing::Type;
    use crate::domains::typing::{Context, TypingDomain, TypingSynth};
    use crate::engine::grammar::SPG;

    const BATCH_BUDGET: Duration = Duration::from_secs(1);

    fn parse_complete(grammar: &SPG<TypingDomain>, input: &str, ctx: &Context) -> usize {
        let mut synth = TypingSynth::new(grammar.clone(), input);
        let ast = synth
            .parse_with(ctx)
            .unwrap_or_else(|err| panic!("failed to parse {input:?}: {err}"));
        assert!(ast.is_complete(), "expected complete parse for {input:?}");
        ast.node_count()
    }

    fn assert_batch_budget(name: &str, start: Instant, max_nodes: usize, observed_nodes: usize) {
        let elapsed = start.elapsed();
        assert!(
            elapsed <= BATCH_BUDGET,
            "{name} exceeded budget: {elapsed:?} > {BATCH_BUDGET:?}"
        );
        assert!(
            observed_nodes <= max_nodes,
            "{name} produced too many arena nodes: {observed_nodes} > {max_nodes}"
        );
    }

    fn stlc_chain_case(arg_count: usize) -> (String, Context) {
        let type_names: Vec<_> = (0..=arg_count).map(|idx| format!("T{idx}")).collect();
        let mut f_ty = type_names[arg_count].clone();
        for idx in (0..arg_count).rev() {
            f_ty = format!("{}->{}", type_names[idx], f_ty);
        }

        let mut ctx = Context::new();
        ctx.add("f".to_string(), Type::parse_raw(&f_ty).unwrap());

        let mut names = vec!["f".to_string()];
        for idx in 0..arg_count {
            let name = format!("x{idx}");
            ctx.add(name.clone(), Type::parse_raw(&type_names[idx]).unwrap());
            names.push(name);
        }

        (names.join(" "), ctx)
    }

    fn imp_block_case(decl_count: usize, rng: &mut StdRng) -> String {
        let mut parts = vec!["{".to_string()];
        for idx in 0..decl_count {
            let name = format!("v{idx}");
            if rng.gen_bool(0.5) {
                parts.extend([
                    "let".to_string(),
                    name,
                    ":".to_string(),
                    "Int".to_string(),
                    "=".to_string(),
                    (idx + 1).to_string(),
                    ";".to_string(),
                ]);
            } else {
                parts.extend([
                    "let".to_string(),
                    name,
                    ":".to_string(),
                    "Bool".to_string(),
                    "=".to_string(),
                    if idx % 2 == 0 { "true" } else { "false" }.to_string(),
                    ";".to_string(),
                ]);
            }
        }
        parts.push("}".to_string());
        parts.join(" ")
    }

    #[test]
    fn random_stlc_application_chains_stay_bounded() {
        let grammar = SPG::<TypingDomain>::load(include_str!("../../examples/stlc.auf")).unwrap();
        let mut rng = StdRng::seed_from_u64(10);
        let start = Instant::now();
        let mut max_nodes = 0usize;

        for _ in 0..24 {
            let arg_count = rng.gen_range(1..=5);
            let (input, ctx) = stlc_chain_case(arg_count);
            max_nodes = max_nodes.max(parse_complete(&grammar, &input, &ctx));
        }

        assert_batch_budget("stlc random application chains", start, 2_000, max_nodes);
    }

    #[test]
    fn random_imp_declaration_blocks_stay_bounded() {
        let grammar = SPG::<TypingDomain>::load(include_str!("../../examples/imp.auf")).unwrap();
        let mut rng = StdRng::seed_from_u64(10);
        let start = Instant::now();
        let mut max_nodes = 0usize;

        for _ in 0..16 {
            let decl_count = rng.gen_range(1..=5);
            let input = imp_block_case(decl_count, &mut rng);
            max_nodes = max_nodes.max(parse_complete(&grammar, &input, &Context::new()));
        }

        assert_batch_budget("imp random declaration blocks", start, 3_000, max_nodes);
    }

    #[test]
    fn weird_recursive_grammars_stay_bounded() {
        let right_recursive = SPG::<TypingDomain>::load(
            r#"
            A ::= 'a' A | 'b'
            start ::= A
            "#,
        )
        .unwrap();
        let epsilon_heavy = SPG::<TypingDomain>::load(
            r#"
            A ::= 'a' B | ε
            B ::= 'b' C | ε
            C ::= 'c' | ε
            start ::= A B C
            "#,
        )
        .unwrap();
        let mut rng = StdRng::seed_from_u64(0x5EED_1234);
        let start = Instant::now();
        let mut max_nodes = 0usize;

        for _ in 0..24 {
            let depth = rng.gen_range(0..=8);
            let input = std::iter::repeat_n("a", depth)
                .chain(std::iter::once("b"))
                .collect::<Vec<_>>()
                .join(" ");
            max_nodes = max_nodes.max(parse_complete(&right_recursive, &input, &Context::new()));

            let eps_input = match rng.gen_range(0..=3) {
                0 => "",
                1 => "a",
                2 => "a b",
                _ => "a b c",
            };
            max_nodes = max_nodes.max(parse_complete(&epsilon_heavy, eps_input, &Context::new()));
        }

        assert_batch_budget("weird recursive grammars", start, 1_000, max_nodes);
    }
}
