#[D] Regex Engine

The regex engine underpins all terminal matching in the parser. It uses **Brzozowski derivatives** to match incrementally, a design that integrates naturally with partial parsing. The theoretical foundations (derivatives, two-level completability) are developed in [Completability and Regular Expressions](https://unsuspicious.org/blog/completing-regex/). This page covers only the implementation-specific choices.

Source: [`src/regex/`](~/src/regex/)

## The `Regex` Type

>D Regex
$$r ::= \emptyset \mid \varepsilon \mid c \mid [a\text{-}b] \mid r_1 \cdot r_2 \mid r_1 \mid r_2 \mid r^*$$

`Empty`, `Epsilon`, `Char`, `Range`, `Concat`, `Union`, `Star`. Grammar terminals written as `/[a-z]+/` are parsed into this representation at grammar load time. Common shorthands (`digit()`, `word()`, `alnum()`) are combinators over these primitives.
<

## Prefix Matching

>D PrefixStatus
`prefix_match(r, s)` computes $d = \text{simplify}(\partial_s r)$ and classifies the result:

| Outcome | Condition | Meaning |
|---------|-----------|---------|
| `NoMatch` | $d = \emptyset$ | $s$ is not a prefix of any string in $\mathcal{L}(r)$ |
| `Extensible(d)` | $\nu(d)$ | $s \in \mathcal{L}(r)$ and $d$ accepts further characters |
| `Prefix(d)` | $\lnot\nu(d)$, $d \neq \emptyset$ | $s$ is a valid prefix but not yet a complete match |
<

The parser maps these outcomes to terminal node status: `Extensible` produces a `Terminal::Complete` with a non-null extension derivative; `Prefix` produces a `Terminal::Partial` with a remainder derivative.

Simplification (standard algebraic identities: $\emptyset \cdot r = \emptyset$, $\varepsilon \cdot r = r$, $r \mid r = r$, etc.) is applied after each derivative step to keep expression size bounded.

## NFA Fallback

For full-string matching (`matches`) and maximum-length prefix matching (`match_len`), the engine compiles to an NFA via Thompson construction rather than using derivatives. This avoids the expression-growth problem derivatives can exhibit on long inputs.

## DFA Intersection

`has_intersection(r_1, r_2)` checks $\mathcal{L}(r_1) \cap \mathcal{L}(r_2) \neq \emptyset$ by product-DFA construction. Used by the typing engine to detect overlapping terminal patterns.

## String Generation

`examples(n)` generates up to $n$ concrete strings in $\mathcal{L}(r)$. The [synthesizer](./completion/synthesizer.md) uses this to find token candidates when extending a partial tree with a regex terminal.
