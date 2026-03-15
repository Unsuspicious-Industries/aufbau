# Aufbau

From Carnap's *Der logische Aufbau der Welt*.

A Rust meta-logic engine powering the [**Proposition 7**](https://unsuspicious.org/blog/proposition-7) constrained generation system.

You can find techical specification and documentation at the [Aufbau Engine technical spec](https://unsuspicious-industries.github.io/aufbau)

## Synthesis and constrained generation

Typical flow:

1. Load a `Grammar`.
2. Build a `Context` for any variables already in scope.
3. Create a `Synthesizer` from the current prefix.
4. Call `completions_ctx(&ctx)` to get the allowed next-token constraints.
5. Rank or filter those constraints with your decoder or model.
6. Use `extend(token, &ctx)` to check if the chosen token is admissible.
  - If matches, AST and updated input are returned
  - If not, update ranking by removing it and resample [^1]
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

Details :

- `completions_ctx()` returns admissible next-token regexes, not concrete strings.
- `extend(token, &ctx)` checks typing admissibility of a token to complete current input
- `extend_with_regex(re, &ctx)` asks the synthesizer to find one token in the set defined by the regex language that is typedly and syntactically valid.
- `tree()` and `complete()` read the synthesizer's committed cached tree state.
- `Synthesizer::complete()` checks whether the current input is already complete. 

---
 [1^]: In the spec we have proven that it will always have at least one valid compltion in the set. Complexity depends on how bad your ranking engine (LLM?) is. Max is $O(n)$ with $n$ the vocab size, but in practice wont happen.