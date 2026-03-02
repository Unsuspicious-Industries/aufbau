// synthesizer experiments

#[cfg(test)]
mod tests {
    use crate::logic::{partial::Synthesizer, typing::Context};

    #[test]
    fn test_synthesizer() {
        let grammar = crate::testing::load_example_grammar("fun");
        let input = "let";
        let mut synthesizer = Synthesizer::new(grammar, input);
        let ctx = Context::new();
        let start = std::time::Instant::now();
        for i in 0..20 {
            // get completions
            let cset = synthesizer.typed_completions(&ctx);
            // pick and add
            loop {
                let comp = cset.iter().next().unwrap();
                if let Some(comp) = synthesizer.extend_with_regex(comp, &ctx, 2) {
                    println!("Adding completion: '{}'", comp.1);
                    synthesizer.set_input(comp.1);
                    break;
                }
            }
            println!(
                "Iteration {}: found {:?} completions for input '{}'",
                i,
                cset,
                synthesizer.input()
            );
        }
        let duration = start.elapsed();
        println!("Total time: {:?}", duration);
        // average time per iteration
        println!("Average time per iteration: {:?}", duration / 20);
    }
    #[test]
    fn test_synthesizer_new() {
        let grammar = crate::testing::load_example_grammar("fun");
        let input = "let";
        let mut synthesizer = Synthesizer::new(grammar.clone(), input);
        let ctx = Context::new();
        let start = std::time::Instant::now();
        for i in 0..20 {
            // get completions
            let cset = synthesizer.typed_completions(&ctx);
            // pick and add
            let comp = cset.iter().next().unwrap();
            let start_extend = std::time::Instant::now();
            if let Some(comp) = synthesizer.extend_with_regex(comp, &ctx, 2) {
                println!(
                    "Adding completion: '{}' (extend took {:?})",
                    comp.1,
                    start_extend.elapsed()
                );
                synthesizer = Synthesizer::new(grammar.clone(), comp.1);
            }
            println!(
                "Iteration {}: found {} completions for input '{}'",
                i,
                cset.len(),
                synthesizer.input()
            );
        }
        let duration = start.elapsed();
        println!("Total time: {:?}", duration);
        // average time per iteration
        println!("Average time per iteration: {:?}", duration / 20);
    }
}
