#[D] Synthesizer

The **synthesizer** is the stateful interface between the parser and the search engine. It wraps incremental parsing with type-filtered extension operations, providing the search with a clean abstraction: *given the current input, what are the valid next tokens?*

Source: [`src/logic/partial/synth.rs`](~/src/logic/partial/synth.rs)

## Definition

>D Synthesizer
A **synthesizer** is a stateful triple:

$$\Sigma_s = (G, \mathcal{M}, s)$$

where:
- $G$ is a grammar
- $\mathcal{M}$ is a `MetaParser` instance (see [Meta-Parser](../parsing/meta_parser.md)) with cached depth state
- $s \in \Sigma^*$ is the current input string
<

The synthesizer exposes the following operations:

| Operation | Signature | Description |
|-----------|-----------|-------------|
| `partial` | $\Sigma_s \to \mathcal{F}$ | Parse current input into a partial forest |
| `completions` | $\Sigma_s \to \mathcal{C}$ | Grammar-level (untyped) completion set |
| `typed_completions` | $\Sigma_s \times \Gamma \to \mathcal{C}_\tau$ | Type-filtered completion set |
| `try_extend` | $\Sigma_s \times t \times \Gamma \to (\mathcal{F}, s')$ | Parse $s \cdot t$, return result without mutation |
| `extend` | $\Sigma_s \times t \times \Gamma \to (\mathcal{F}, s')$ | Parse $s \cdot t$ and commit to new input |
| `extend_with_regex` | $\Sigma_s \times r \times \Gamma \times n \to (\mathcal{F}, s')$ | Extend with a regex token by trying concrete candidates |
| `complete` | $\Sigma_s \to \text{bool}$ | Check if the current parse is already complete |

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

1. **Tree terminals**: collect all terminal text from the current partial forest via `gather_terminals`
2. **Primary example**: $r.\text{example}()$, the canonical example string of the regex
3. **Additional examples**: $r.\text{examples}(n)$, up to $n$ additional examples

Candidates are tried in this order via `try_extend`. The first valid extension wins.
<

The tree-terminal-first ordering is intentional: reusing identifiers and literals already present in the partial tree produces more coherent completions (e.g., reusing a bound variable name rather than inventing a new one). This is a heuristic for **quality**, not correctness; any valid candidate would produce a correct completion.

## Step-by-step: building an expression token by token

This section traces exactly what the synthesizer does when the search engine calls `extend` repeatedly to build the expression `let x = 1` from an empty input. The grammar assumed is a simple expression language where a `let`-binding has the form `let <ident> = <expr>`.

>N why this example?
`let x = 1` hits all three extension modes: keyword (no separator needed before `let`), identifier (`x`, chosen from typed completions), punctuation (`=`), and a literal. Four tokens, four calls to `extend`.
<

**Initial state.** The synthesizer starts with $s = \varepsilon$ (the empty string). `partial()` returns an empty forest; every nonterminal in $G$ is a candidate root. `typed_completions` with any context will return the set of tokens that can begin a valid expression — including `let`.

**Step 1: extend with `let`.** The search engine calls `extend(s, \texttt{"let"}, \Gamma)`. The auto-separator check sees $\text{last}(\varepsilon)$ is undefined so no separator is inserted. The MetaParser parses `"let"` and returns a partial forest with one active parse: a `let`-binding with three remaining holes (`<ident>`, `=`, `<expr>`). The synthesizer commits: $s \leftarrow \texttt{"let"}$.

After this step, `typed_completions` returns only tokens that can extend `"let"` to a syntactically and type-valid string. In a well-designed grammar, that set consists entirely of identifiers matching `[a-z][a-z0-9_]*`.

**Step 2: extend with `x`.** The auto-separator check: $\text{last}(\texttt{"let"}) = \texttt{t} \in \mathcal{W}$ and $\text{first}(\texttt{"x"}) = \texttt{x} \in \mathcal{W}$, so a space is automatically inserted. The MetaParser parses `"let x"`. The partial forest now has the `<ident>` hole filled; the remaining holes are `=` and `<expr>`. The synthesizer commits: $s \leftarrow \texttt{"let x"}$.

`typed_completions` now returns only `=`, since that is the only token that can advance the `let`-binding parse.

**Step 3: extend with `=`.** $\text{last}(\texttt{"let x"}) = \texttt{x} \in \mathcal{W}$, $\text{first}(\texttt{"="}) = \texttt{=} \notin \mathcal{W}$, so no auto-separator. The MetaParser parses `"let x ="`. The partial forest has one hole remaining: `<expr>`. The synthesizer commits: $s \leftarrow \texttt{"let x ="}$.

`typed_completions` with a typing context $\Gamma$ now filters: any token that begins a valid expression *and* whose eventual type is compatible with the binding context. Numeric literals (`1`, `2`, ...) and any bound variable in $\Gamma$ are valid.

**Step 4: extend with `1`.** $\text{last}(\texttt{"let x ="}) = \texttt{=} \notin \mathcal{W}$, no auto-separator. The MetaParser parses `"let x = 1"`. The partial forest now contains a complete parse rooted at the `let`-binding nonterminal. `complete()` returns true. The search engine accepts this state as a valid completion.

>I step through it
{"label":"extend: let x = 1","input":"","steps":[{"token":"let","tokens":["let"],"display":"s = \"let\"\nactive: let <ident> = <expr>"},{"token":"x","tokens":["x","y","z","n"],"display":"s = \"let x\"\nfilled: <ident> = x\nremaining: = <expr>"},{"token":"=","tokens":["="],"display":"s = \"let x =\"\nfilled: = \nremaining: <expr>"},{"token":"1","tokens":["1","2","42","x"],"display":"s = \"let x = 1\"\ncomplete \u2713\nroot: let-binding"}]}
<

The display in the widget mirrors what the synthesizer's internal state looks like at each step: the accumulated string $s$, which holes in the parse tree have been filled, and what remains. Clicking any offered token fires a Web Audio blip and advances the state exactly as `extend` would.

## Depth Retry

>N bounded retry
retry is intentionally local: nearby states should have nearby depths (e.g. 10 -> 11/12, not 41)
<

>W Depth-Differential Retry
When `typed_completions` fails type filtering on the first pass, the synthesizer retries with a fresh `MetaParser` in a **bounded local window**:

$$d_\text{start} = d + 1, \quad d_\text{max} = d + 2$$

where $d$ is the depth used by the initial parse.

This keeps depth changes smooth across similar synthesizer states and avoids large jumps that harm predictability and cache locality.
<
