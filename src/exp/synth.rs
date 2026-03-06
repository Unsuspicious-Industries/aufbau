// synthesizer experiments

#[cfg(test)]
mod tests {
    use crate::logic::{partial::Synthesizer, typing::Context};
    use rand::seq::IteratorRandom as _;

    #[test]
    fn test_synthesizer() {
        let grammar = crate::testing::load_example_grammar("fun");
        let input = "";
        let mut synthesizer = Synthesizer::new(grammar, input);
        let ctx = Context::new();
        let mut rng = rand::thread_rng();
        let start = std::time::Instant::now();
        for i in 0..50 {
            // get completions
            let cset = synthesizer.completions_ctx(&ctx);
            // pick and add
            loop {
                let comp = cset.iter().choose(&mut rng).unwrap();
                if let Some(comp) = synthesizer.extend_with_regex(comp, &ctx) {
                    println!("Adding completion: '{}'", comp.1);
                    synthesizer.set_input(comp.1);
                    break;
                } else {
                    println!("Failed to extend with completion: '{}'", comp);
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
        let mut rng = rand::thread_rng();
        let start = std::time::Instant::now();
        for i in 0..10 {
            // get completions
            let cset = synthesizer.completions_ctx(&ctx);
            // pick and add
            let comp = cset.iter().choose(&mut rng).unwrap();
            let start_extend = std::time::Instant::now();
            if let Some(comp) = synthesizer.extend_with_regex(comp, &ctx) {
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

    /// Benchmark the synthesizer's `completions_ctx` call at exponentially
    /// spaced depths (1, 2, 4, 8, 16, …) to empirically estimate the time
    /// complexity exponent x in T(n) ≈ C · n^x.
    ///
    /// Strategy:
    ///   1. Grow the synthesizer by extending it one step at a time.
    ///   2. At each power-of-two depth d, time a single `completions_ctx` call.
    ///   3. After collecting (d, t) pairs, fit a log-log linear regression to
    ///      estimate x = Δlog(t) / Δlog(d).
    #[test]
    fn benchmark_synthesizer() {
        let grammar = crate::testing::load_example_grammar("fun");
        let initial_input = "let";
        let ctx = Context::new();

        let sample_depths: Vec<u32> = (0..=16).collect(); 
        let max_depth = *sample_depths.last().unwrap();

        let mut synthesizer = Synthesizer::new(grammar.clone(), initial_input);
        let mut rng = rand::thread_rng();
        let mut depth: u32 = 0;
        let mut measurements: Vec<(f64, f64)> = Vec::new(); // (log depth, log time_ns)

        println!("\n=== benchmark_synthesizer ===");
        println!("{:<8} {:<20} {}", "depth", "completions_ctx time", "# completions");

        while depth < max_depth {
            // Advance one step.
            let cset = synthesizer.completions_ctx(&ctx);
            depth += 1;

            // At each sample point, time a fresh completions_ctx call.
            if sample_depths.contains(&depth) {
                let t0 = std::time::Instant::now();
                let cset_timed = synthesizer.completions_ctx(&ctx);
                let elapsed = t0.elapsed();
                let elapsed_ns = elapsed.as_nanos() as f64;

                println!("{:<8} {:<20?} {}", depth, elapsed, cset_timed.len());

                if elapsed_ns > 0.0 {
                    measurements.push((f64::ln(depth as f64), f64::ln(elapsed_ns)));
                }
            }

            // Extend the synthesizer for the next step.
            let comp = cset.iter().choose(&mut rng);
            match comp {
                Some(comp) => {
                    if let Some(extended) =
                        synthesizer.extend_with_regex(comp, &ctx)
                    {
                        synthesizer = Synthesizer::new(grammar.clone(), extended.1);
                        println!("Extended to '{}'", synthesizer.input());
                    } else {
                        println!("Could not extend at depth {depth}, stopping early.");
                        break;
                    }
                }
                None => {
                    println!("No completions at depth {depth}, stopping early.");
                    break;
                }
            }
        }

        // --- log-log linear regression to find exponent x ---
        // T(n) = C · n^x  =>  ln T = ln C + x · ln n
        // x = (N Σ(ln_n · ln_t) - Σln_n · Σln_t) / (N Σln_n² - (Σln_n)²)
        let n = measurements.len() as f64;
        if n >= 2.0 {
            let sum_x: f64 = measurements.iter().map(|(x, _)| x).sum();
            let sum_y: f64 = measurements.iter().map(|(_, y)| y).sum();
            let sum_xx: f64 = measurements.iter().map(|(x, _)| x * x).sum();
            let sum_xy: f64 = measurements.iter().map(|(x, y)| x * y).sum();
            let denom = n * sum_xx - sum_x * sum_x;
            if denom.abs() > f64::EPSILON {
                let x_exp = (n * sum_xy - sum_x * sum_y) / denom;
                let c_ln = (sum_y - x_exp * sum_x) / n;
                println!(
                    "\nEstimated complexity: T(n) ≈ {:.3e} · n^{:.3}",
                    f64::exp(c_ln),
                    x_exp
                );
                println!("(fitted over {} sample points)", measurements.len());
            }
        } else {
            println!("Not enough sample points to fit a power law.");
        }
    }
}
