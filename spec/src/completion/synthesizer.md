#[D] Synthesizer

The **synthesizer** is the stateful interface between the parser and the search engine. It wraps incremental parsing with type-filtered extension operations, providing the search with a clean abstraction: *given the current input, what are the valid next tokens?*

Source: [`src/logic/partial/synth.rs`](~/src/logic/partial/synth.rs)

This chapter is meant to be read before [Search](./search.md). It only covers token generation and input extension; frontier management is described in [Search](./search.md), and numeric ranking is described in [Scoring](./scoring.md).

## Definition

>D Synthesizer
A **synthesizer** is a stateful triple:

$$\Sigma_s = (G, \mathcal{M}, s)$$

where:
- $G$ is a grammar
- $\mathcal{M}$ is a `MetaParser` instance layered on top of the [partial parser](../parsing.md), with cached depth state
- $s \in \Sigma^*$ is the current input string
<

The synthesizer exposes the following operations:

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `partial` | `partial(&mut self) -> Result<PartialAST, String>` | Parse current input into a partial forest |
| `completions` | `completions(&mut self) -> CompletionSet` | Type-filtered completion set under the empty context |
| `completions_ctx` | `completions_ctx(&mut self, ctx: &Context) -> CompletionSet` | Type-filtered completion set in context $\Gamma$ |
| `try_extend` | `try_extend(&mut self, token: &str, ctx: &Context) -> Result<(TypedAST, String), String>` | Parse $s \cdot t$, return the typed result without mutation |
| `extend` | `extend(&mut self, token: &str, ctx: &Context) -> Result<TypedAST, String>` | Parse $s \cdot t$ and commit to the new input |
| `extend_with_regex` | `extend_with_regex(&mut self, token: &DerivativeRegex, ctx: &Context) -> Option<(TypedAST, String)>` | Extend with a regex token, returning the first valid candidate |
| `extend_all_with_regex` | `extend_all_with_regex(&mut self, token: &DerivativeRegex, ctx: &Context, max_examples: usize) -> Vec<(TypedAST, String)>` | Extend with a regex token, returning up to `max_examples` valid candidates |
| `complete` | `complete(&mut self) -> Option<TypedNode>` | Return a complete typed root when one exists |

## Extension Semantics

### Token Extension

>D Parse Extension
Given input $s$ and candidate token $t$, the **parse extension** function attempts three concatenations in order:

1. $s \cdot_\text{auto} t$: with automatic separator insertion (see below)
2. $s \cdot t$: direct concatenation
3. $s \cdot \text{' '} \cdot t$: with explicit space

The first successful parse is returned.
<

>D Automatic Separator
The **automatic separator heuristic** inserts a space between $s$ and $t$ when:

$$\text{last}(s) \in \mathcal{W} \land \text{first}(t) \in \mathcal{W}$$

where $\mathcal{W} = \\{a\text{-}z, A\text{-}Z, 0\text{-}9, \_\\}$ is the set of word characters.
<

The auto-separator is a **syntactic heuristic**, not a grammatical guarantee. It handles the common case where two adjacent identifiers or keywords require whitespace separation. The fallback to direct concatenation and explicit space ensures no valid extension is missed; at worst, the heuristic wastes one parse attempt.

### Regex Extension

>D Regex Extension
When the next token is a `DerivativeRegex` $r$ (a regex that can still match more input), the synthesizer cannot directly concatenate $r$ as a string. Instead, it gathers **concrete candidate strings** and tries each:

1. **Grammar seeds**: word-like special tokens and raw types collected from the grammar
2. **Primary example**: $r.\text{example}()$, the canonical example string of the regex
3. **Tree terminals**: collect terminal text from the current typed tree

Candidates are tried in this order via `try_extend`. `extend_with_regex` returns the first valid extension; `extend_all_with_regex` keeps distinct successful extensions up to the requested bound.
<

The candidate ordering is intentional: reusing grammar-derived literals and already-seen terminals tends to produce more coherent completions than inventing arbitrary new text. This is a heuristic for **quality**, not correctness; any valid candidate would produce a correct completion.

## Step-by-step: building an expression token by token

This section traces exactly what the synthesizer does when the search engine calls `extend` repeatedly to build the expression `let x = 1` from an empty input. The grammar assumed is a simple expression language where a `let`-binding has the form `let <ident> = <expr>`.

>N why this example?
`let x = 1` hits all three extension modes: keyword (no separator needed before `let`), identifier (`x`, chosen from `completions_ctx`), punctuation (`=`), and a literal. Four tokens, four calls to `extend`.
<

**Initial state.** The synthesizer starts with $s = \varepsilon$ (the empty string). `partial()` returns an empty forest; every nonterminal in $G$ is a candidate root. `completions_ctx` with any context will return the set of tokens that can begin a valid expression — including `let`.

**Step 1: extend with `let`.** The search engine calls `extend(\texttt{"let"}, \Gamma)`. The auto-separator check sees $\text{last}(\varepsilon)$ is undefined so no separator is inserted. The MetaParser parses `"let"` and returns a partial forest with one active parse: a `let`-binding with three remaining holes (`<ident>`, `=`, `<expr>`). The synthesizer commits: $s \leftarrow \texttt{"let"}$.

After this step, `completions_ctx` returns only tokens that can extend `"let"` to a syntactically and type-valid string. In a well-designed grammar, that set consists entirely of identifiers matching `[a-z][a-z0-9_]*`.

**Step 2: extend with `x`.** The auto-separator check: $\text{last}(\texttt{"let"}) = \texttt{t} \in \mathcal{W}$ and $\text{first}(\texttt{"x"}) = \texttt{x} \in \mathcal{W}$, so a space is automatically inserted. The MetaParser parses `"let x"`. The partial forest now has the `<ident>` hole filled; the remaining holes are `=` and `<expr>`. The synthesizer commits: $s \leftarrow \texttt{"let x"}$.

`completions_ctx` now returns only `=`, since that is the only token that can advance the `let`-binding parse.

**Step 3: extend with `=`.** $\text{last}(\texttt{"let x"}) = \texttt{x} \in \mathcal{W}$, $\text{first}(\texttt{"="}) = \texttt{=} \notin \mathcal{W}$, so no auto-separator. The MetaParser parses `"let x ="`. The partial forest has one hole remaining: `<expr>`. The synthesizer commits: $s \leftarrow \texttt{"let x ="}$.

`completions_ctx` with a typing context $\Gamma$ now filters: any token that begins a valid expression *and* whose eventual type is compatible with the binding context. Numeric literals (`1`, `2`, ...) and any bound variable in $\Gamma$ are valid.

**Step 4: extend with `1`.** $\text{last}(\texttt{"let x ="}) = \texttt{=} \notin \mathcal{W}$, no auto-separator. The MetaParser parses `"let x = 1"`. The partial forest now contains a complete parse rooted at the `let`-binding nonterminal. `complete()` returns `Some(root)`. The search engine can now stop on this state.



The display in the widget mirrors what the synthesizer's internal state looks like at each step: the accumulated string $s$, which holes in the parse tree have been filled, and what remains. Clicking any offered token fires a Web Audio blip and advances the state exactly as `extend` would.

## Depth Retry

>N bounded retry
retry is intentionally local: nearby states should have nearby depths (e.g. 10 -> 11/12, not 41)
<

>R Depth-Differential Retry
When completion filtering fails on the first pass, the synthesizer retries with a fresh `MetaParser` in a **bounded local window**:

$$d_\text{start} = d + 1, \quad d_\text{max} = d + 2$$

where $d$ is the depth used by the initial parse.

This makes sure the parsing gathers anough trees to produce a typed alternative. It's a heuristics, and is faillible. Bad design but works enough.
<
