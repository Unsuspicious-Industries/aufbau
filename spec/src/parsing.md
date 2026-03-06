#[D] Partial Parsing

The partial parser implements the theoretical framework of [partial forests](concepts/theory.md) as a memoized recursive-descent parser with regex-derivative terminal matching. Given an input $s \in \Sigma^*$, it produces a partial forest $\mathcal{F}$ containing all valid parse trees (complete and partial) consistent with the grammar $G$.

Source: [`src/logic/partial/parse.rs`](../src/logic/partial/parse.rs)

## Segmentation

The first stage of parsing tokenizes input into a sequence of **segments**.

>D Segment
A **segment** is a whitespace-delimited token carrying its byte span and a flag indicating whether it is a prefix of a special token. Formally, segmentation is a function:

$$\text{tokenize}: \Sigma^* \to \text{Segment}^*$$

Segments preserve positional information (byte offsets) and are the atomic unit of the span cache.
<

Special tokens (multi-character operators like `->`, `<=`) are matched longest-first. If a segment at the end of input is a strict prefix of a special token (e.g., `-` when `->` exists in the grammar), it is marked as a **partial special token**. This flag disables caching for spans involving that segment, since the token's identity is ambiguous until more input arrives.

## Recursive Descent with Memoization

The parser is a recursive-descent parser over segments, augmented with:
1. A persistent [span cache](parsing/cache.md) for cross-parse memoization
2. Cycle detection for left-recursive grammars
3. Depth-bounded recursion

>D Parse Entry
The main entry point `partial(s)` produces a `PartialParseOutcome`:

1. Tokenize $s$ into segments.
2. Check cache validity: if the new segments extend the previous input's segments (prefix match), reuse the cache; otherwise clear it.
3. Parse from the start symbol $S$ at position 0 with a fresh `ParseState`.
4. Filter roots to those consuming all segments.
5. Return success (with possibly partial trees) or failure.
<

>D Parse State
A **parse state** tracks per-invocation mutable data:

- $\text{visited}: (N \times \mathbb{N}) \to \mathbb{N}$, mapping (nonterminal, position) to visit count for cycle detection.
- $\text{hit\_depth\_limit}: \text{bool}$, a sticky flag set when any branch exceeds the recursion bound. When true, no results from this parse invocation are cached.
<

### Nonterminal Parsing

>D Nonterminal Parse
For nonterminal $A$ at position $i$ and depth $d$:

1. **Depth guard**: if $d > d_{\max}$, abort and set $\text{hit\_depth\_limit} \leftarrow \text{true}$.
2. **Cache lookup**: if the [span cache](parsing/cache.md) can answer for $(d_{\max}, A, i, \ell)$ for all relevant span lengths, return cached trees.
3. **Cycle detection**: track $(A, i)$ in visited. Allow up to $\min(d_{\max}, |\text{segments}| + 2)$ re-entries before aborting. This handles left-recursive productions like `Term ::= Term BaseTerm`.
4. **Try all productions**: iterate over productions for $A$ in rotated order (see below), calling $\text{parse\_production}$ for each.
5. **Cache results**: if no depth limit was hit and segments do not end with a partial special token, store complete subtrees in the span cache grouped by consumed length.
<

### Alternative Ordering

>D Production Rotation
Productions for a nonterminal are tried in a **rotated order** determined by recursion depth:

$$\text{order}(n, d) = [(d \bmod n), (d+1 \bmod n), \ldots, (d-1 \bmod n)]$$

where $n$ is the number of productions and $d$ is the current recursion depth.
<

This rotation distributes search effort across recursion depths, preventing systematic bias toward the first production. It is particularly important for left-recursive grammars where always trying the recursive alternative first would waste effort at shallow depths.

### Symbol Sequence Parsing

A production's right-hand side is a sequence of symbols parsed left-to-right. For each successful parse of the first symbol, the parser attempts to parse the remaining symbols with the unconsumed segments.

A critical invariant governs partial nodes: if a child node is partial (incomplete), it is accepted only if it consumed all remaining segments (i.e., it is at the frontier of the input). Partial nodes cannot appear in non-frontier positions.

## Partial Terminals

Terminals are regex patterns matched via Brzozowski derivatives (see [two-level language completability](https://unsuspicious.org/blog/completing-regex/)). Matching a segment against a terminal regex yields one of four outcomes:

>D Terminal Match Outcomes

| Outcome | Node Produced | Meaning |
| :--- | :--- | :--- |
| Complete | `Terminal::Complete` (no extension) | Segment fully matches the regex |
| Extensible | `Terminal::Complete` (with extension derivative) | Segment is a complete match AND a valid prefix of longer matches |
| Prefix | `Terminal::Partial` (with remainder derivative) | Segment is a valid prefix but not a complete match |
| NoMatch | (none) | Segment does not match |
<

At end-of-input (empty remaining segments), the parser produces a `Terminal::Partial` with the full regex as remainder. This enables downstream completion generation: the remainder derivative describes exactly which tokens could legally appear next.

The distinction between **extension** and **remainder** is central to the completability analysis:
- An **extension** derivative on a complete terminal means the terminal could accept more characters (e.g., identifier `x` could become `xy`).
- A **remainder** derivative on a partial terminal describes what characters are needed to complete the match.

## Epsilon Productions

An epsilon production (empty right-hand side, $\varepsilon$) matches zero segments and produces a nonterminal with no children. Such a nonterminal is considered complete.

## Recursion Limit

>D Default Recursion Depth
The default maximum recursion depth is $d_{\max} = 15$ (`DEFAULT_MAX_RECURSION_DEPTH`). This is overridable per-parser and is distinct from the typing depth limit of 50.
<

When the depth limit is hit, the parser returns a `DepthLimit` error. The `MetaParser` wrapper in `src/logic/partial/meta.rs` uses iterative deepening to find the minimum sufficient depth.

## Incremental Parsing

The parser supports incremental parsing across successive calls. When input $s$ is extended to $s \cdot t$:

1. Normalize both segment sequences to position-independent keys.
2. If the new segments start with the previous segments (prefix match), reuse the [span cache](parsing/cache.md).
3. Otherwise, clear the cache entirely.

This prefix-stability property ensures that typing a single character does not require re-parsing the entire input from scratch.
