# Aufbau

From Carnap's *Der logische Aufbau der Welt*.

A Rust meta-logic engine powering the [**Proposition 7**](https://unsuspicious.org/blog/proposition-7) constrained generation system.

## Developer API

There are two useful API layers:

- `aufbau::logic::partial::Synthesizer`: stateful, incremental generation.
- `aufbau::validation::completability`: validation helpers that match the correctness checks used by the validation suite.

Use the synthesizer for interactive constrained generation.
Use the validation helpers when you need a completed witness or a correctness check that matches the validation suite.

## Synthesis and constrained generation

Typical flow:

1. Load a `Grammar`.
2. Build a `Context` for any variables already in scope.
3. Create a `Synthesizer` from the current prefix.
4. Call `completions_ctx(&ctx)` to get the allowed next-token constraints.
5. Rank or filter those constraints with your decoder or model.
6. Apply the chosen step:
    - `extend(token, &ctx)` for a concrete token.
    - `extend_with_regex(token, &ctx)` for a regex completion.
7. Inspect the returned `TypedAST`, or query the committed synthesizer state with `input()`, `tree()`, or `complete()`.

```rust,no_run
use aufbau::logic::grammar::Grammar;
use aufbau::logic::partial::Synthesizer;
use aufbau::logic::typing::Context;

fn main() -> Result<(), String> {
    let spec = std::fs::read_to_string("examples/fun.auf")
        .map_err(|e| e.to_string())?;
    let grammar = Grammar::load(&spec)?;

    let ctx = Context::new();
    let mut synth = Synthesizer::new(grammar, "let");
    let allowed = synth.completions_ctx(&ctx);

    // `allowed` contains regex constraints for the next valid token.
    // In a real decoder, rank these and pick one.
    if let Some(token) = allowed.iter().next() {
        if let Some((_tree, extended)) = synth.extend_with_regex(token, &ctx) {
            println!("extended to: {}", extended);
        }
    }
    
    Ok(())
}
```

Notes:

- `completions_ctx()` returns admissible next-token regexes, not concrete strings.
- `extend(token, &ctx)` is for a concrete token your ranking layer already chose. It updates `input()` and returns the new `TypedAST`.
- `extend_with_regex(token, &ctx)` asks the synthesizer to find one concrete witness for a regex completion. It commits the new input just like `extend(...)`, and also returns `(TypedAST, String)`.
- `tree()` and `complete()` read the synthesizer's committed cached tree state.
- `Synthesizer::complete()` only checks whether the current input is already complete. It does not search forward.
