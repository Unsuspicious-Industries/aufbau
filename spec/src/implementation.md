#[W] Implementation Map

This page maps the specification sections to source code modules.

## Module Structure

| Spec Section | Source Module | Description |
|-------------|--------------|-------------|
| [Grammar Specification](./spec.md) | [`src/logic/grammar/`](~/src/logic/grammar/) | Grammar data structures and `.auf` file loading |
| [Partial Parsing](./parsing.md) | [`src/logic/partial/`](~/src/logic/partial/) | Chart parser, segment handling, partial trees |
| [Span Cache](./parsing/cache.md) | [`src/logic/partial/cache.rs`](~/src/logic/partial/cache.rs) | 4D memoization table for parse results |
| [Meta-Parser](./parsing/meta_parser.md) | [`src/logic/partial/meta.rs`](~/src/logic/partial/meta.rs) | Adaptive depth search, grammar-scope caching |
| [Binding Resolution](./concepts/binding.md) | [`src/logic/binding/`](~/src/logic/binding/) | Grammar path construction (compile-time) |
| [Runtime Binding](./binding.md) | [`src/logic/typing/binding.rs`](~/src/logic/typing/binding.rs) | Tree path resolution (runtime) |
| [Type Inference](./typing.md) | [`src/logic/typing/`](~/src/logic/typing/) | Typing engine, rule application, type cache |
| [Context](./typing/context.md) | [`src/logic/typing/core.rs`](~/src/logic/typing/core.rs) | Typing context, lanes, no-propagate |
| [Premises](./typing/premises.md) | [`src/logic/typing/eval.rs`](~/src/logic/typing/eval.rs) | Premise evaluation |
| [Conclusion](./typing/conclusion.md) | [`src/logic/typing/eval.rs`](~/src/logic/typing/eval.rs) | Conclusion evaluation |
| [Synthesizer](./completion/synthesizer.md) | [`src/logic/partial/synth.rs`](~/src/logic/partial/synth.rs) | Incremental typed extension interface |
| [Search](./completion/search.md) | [`src/logic/search/mod.rs`](~/src/logic/search/mod.rs) | Best-first completion search |
| [Scoring](./completion/scoring.md) | [`src/logic/search/scoring.rs`](~/src/logic/search/scoring.rs) | Heuristic scoring for search states |
| [Completability](./verification/completability.md) | [`src/validation/completability.rs`](~/src/validation/completability.rs) | Prefix soundness, completion checking |
| [Validation](./verification/validation.md) | [`src/validation/`](~/src/validation/) | Test harness, per-grammar test suites |

## Auxiliary Modules

| Module | Description |
|--------|-------------|
| [`src/regex/`](~/src/regex/) | Custom regex engine with Brzozowski derivatives for [two-level completability](https://unsuspicious.org/blog/completing-regex/) |
| [`src/logic/typing/ops.rs`](~/src/logic/typing/ops.rs) | Type operations: unification, subtyping |
| [`src/logic/typing/syntax.rs`](~/src/logic/typing/syntax.rs) | Type parsing from grammar spec strings |
| [`src/logic/typing/rule.rs`](~/src/logic/typing/rule.rs) | Typing rule data structures |
