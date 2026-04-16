#[D] Synthesizer

The **synthesizer** is the interface between the parser and the completion search engine. It provides two operations: generate candidate next tokens, and extend the input with a chosen token.

**Source:** [`synth.rs`](~/src/logic/fusion/synth.rs)

## Synthesizer State

>D Synthesizer
A **synthesizer** is a stateful tuple:
$$\Sigma_s = (G, P, w)$$

where:
- $G$ is a grammar
- $P$ is the typed parser state (arena, roots, frontier, typing state)
- $w \in \Sigma^*$ is the current input string
<

The synthesizer provides two core operations:

**Candidate generation:** $\mathrm{candidates}(\Sigma_s, \Gamma) \to \mathcal{T}$ computes the set of valid next tokens $\mathcal{T}$ given typing context $\Gamma$.

**Extension:** $\mathrm{extend}(\Sigma_s, t, \Gamma) \to \Sigma_s'$ appends token $t$ to $w$, advances the parser, and returns the new synthesizer state (or rejects if $t$ is invalid).

## Candidate Generation

Given the current parser frontier $F$ and typing context $\Gamma$, candidate generation computes the set of tokens that could extend the parse without type errors.

For each partial branch $B \in F$, the synthesizer:
1. Identifies the next expected symbol $\alpha$ in $B$
2. If $\alpha$ is a terminal, collects it as a candidate
3. If $\alpha$ is a nonterminal, recursively expands to find terminals
4. Filters candidates using the typing runtime (reject if $\Gamma$ forbids the transition)

The result is a set $\mathcal{T} \subseteq S \cup \mathcal{R}$ where $S$ is the set of special tokens and $\mathcal{R}$ is the set of regex patterns.

>E Candidate generation example

**Grammar:**
```
LetExpr = "let" Ident "=" Expr
Ident = #"[a-z]+"
Expr = IntLit | Ident
IntLit = #"[0-9]+"
```

**Input:** `"let x = "`

**Frontier:** Expecting `Expr`

**Candidates:** $\mathcal{T} = \\{\text{#"[0-9]+"}, \text{#"[a-z]+"}\\}$

The synthesizer expands `Expr` to `IntLit | Ident`, collecting their terminal patterns.
<

## Token Extension

Given a candidate token $t \in \mathcal{T}$, the extension operation appends $t$ to the input and advances the parser.

For **literal tokens** (e.g., `"let"`, `"="`), extension is straightforward: concatenate $w \cdot t$.

For **regex patterns** (e.g., `#"[a-z]+"`), the synthesizer must generate a concrete string matching the pattern. It tries candidates in priority order:

1. **Grammar seeds:** special tokens and type names from the grammar
2. **Regex example:** the pattern's canonical example string (e.g., `"a"` for `#"[a-z]+"`)
3. **Tree terminals:** identifiers and literals already present in the partial tree

The first candidate that parses successfully is used.

>E Regex extension example

**Grammar:**
```
Ident = #"[a-z]+"
```

**Input:** `"let "`

**Next token:** `#"[a-z]+"`

**Candidates tried:**
1. Grammar seeds: none
2. Regex example: `"a"` → parses to `"let a"` → **success**

The synthesizer extends with `"a"` and commits $w' = \text{"let a"}$.
<

>E Regex extension with reuse

**Input:** `"let x = "`

**Partial tree terminals:** `{"x"}`

**Next token:** `#"[a-z]+"`

**Candidates tried:**
1. Grammar seeds: none
2. Regex example: `"a"` → parses successfully
3. Tree terminals: `"x"` → parses successfully

Both succeed. The synthesizer may return:
- $\mathrm{extend}(\Sigma_s, \text{"a"}, \Gamma) \to \Sigma_s'$ where $w' = \text{"let x = a"}$
- $\mathrm{extend}(\Sigma_s, \text{"x"}, \Gamma) \to \Sigma_s''$ where $w'' = \text{"let x = x"}$

The search engine explores both paths.
<

## Separator Heuristic

When extending with a token $t$, the synthesizer tries three concatenations in order:

1. **Auto-separator:** $w \cdot_{\text{auto}} t$ (insert space if both boundaries are word characters)
2. **Direct concatenation:** $w \cdot t$
3. **Explicit space:** $w \cdot \text{' '} \cdot t$

The first successful parse is used.

>D Automatic Separator
The **auto-separator** inserts a space between $w$ and $t$ when:
$$\mathrm{last}(w) \in \mathcal{W} \land \mathrm{first}(t) \in \mathcal{W}$$

where $\mathcal{W} = \\{a{-}z, A{-}Z, 0{-}9, \_\\}$ is the set of word characters.
<

This is a heuristic for common cases (e.g., `"let" + "x"` → `"let x"`). The fallback ensures no valid extension is missed.

>E Separator heuristic examples

**Input:** `"let"`, **token:** `"x"`

Tries:
1. Auto-separator: `"let x"` (both `"t"` and `"x"` are word characters) → **success**

**Input:** `"let x"`, **token:** `"="`

Tries:
1. Auto-separator: `"let x="` (no space, since `"="` is not a word character) → fails
2. Direct concatenation: `"let x="` → fails
3. Explicit space: `"let x ="` → **success**

**Input:** `"1"`, **token:** `"+"`

Tries:
1. Auto-separator: `"1+"` (no space, since `"+"` is not a word character) → **success**
<

## Soundness Contract

>L Synthesizer Soundness
If $\mathrm{extend}(\Sigma_s, t, \Gamma)$ succeeds and returns $\Sigma_s' = (G, P', w')$, then:
1. $w' = w \cdot s$ for some separator $s \in \\{\varepsilon, \text{' '}\\}$ followed by $t$ (modulo concrete regex instantiation)
2. $P'$ is a valid parser state for $w'$ under $G$
3. All typing constraints in $\Gamma$ are satisfied in $P'$

If $\mathrm{extend}$ rejects, then no separator strategy or regex instantiation produces a valid typed parse.
<

This contract ensures the synthesizer never produces invalid completions. The search engine trusts that any successful extension is type-correct.

## Depth Retry

When completion filtering fails due to depth limits, the synthesizer retries with a slightly increased depth bound.

>R Depth Retry Heuristic
When parsing rejects due to depth limit $d$, retry with depth bound $d + 2$.

This is a local heuristic: it avoids jumping to arbitrarily high depths while recovering from incremental parsing artifacts.
<

>E Depth retry example

**Initial parse:** $d = 10$, parse fails with `TooDeep`

**Retry:** $d = 12$, parse succeeds

The synthesizer uses the $d = 12$ result and continues. This typically happens with deeply nested left-recursive structures.
<
