#[D] Chart Parser

The chart parser is a memoized recursive-descent parser over segment sequences. It produces a **partial forest** — all parse trees, complete and partial, consistent with the grammar up to the given input prefix.

Source: [`src/logic/partial/parse.rs`](~/src/logic/partial/parse.rs)

## Entry Point

`Parser::partial(input)` is the main entry point. It:

1. Tokenizes `input` into segments via the grammar's tokenizer.
2. Compares the new segment sequence against the previous one. If the new sequence extends the previous (prefix match), the span cache is reused; otherwise it is cleared.
3. Constructs a fresh `ParseState` for cycle tracking.
4. Calls `parse_nonterminal` at the start symbol, position 0, depth 0.
5. Filters results to roots that consumed all segments.
6. Returns `PartialParseOutcome::Success` or `::Failure`.

`Parser::parse(input)` is a convenience wrapper that additionally requires at least one complete root consuming all segments.

## ParseState

`ParseState` is **per-invocation** (not shared across `partial()` calls). It carries:

- `visited: HashMap<(nonterminal, abs_pos), count>` — cycle detection. Tracks how many times `parse_nonterminal` has been entered for a given (nonterminal, position) pair during this invocation.
- `hit_depth_limit: bool` — sticky flag. Set to true if any branch exceeds `max_recursion`. When true, no results from this invocation are stored in the span cache.

## Nonterminal Parsing

>D Nonterminal Parse
`parse_nonterminal(segments, A, binding, abs_pos, level, state)` for nonterminal $A$ at absolute position `abs_pos` and depth `level`:

1. **Depth guard**: if `level > max_recursion`, set `state.hit_depth_limit = true` and return `[]`.
2. **Span cache lookup**: if the cache can answer for $(d_{\max}, A, \text{abs\_pos}, \ell)$ for all relevant $\ell$, return the cached trees immediately.
3. **Cycle detection**: if `(A, abs_pos)` has been visited more than $\min(d_{\max}, |\text{segments}| + 2)$ times, abort that branch (same sticky depth-limit treatment).
4. **Try all productions** in rotated order (see below). For each, call `parse_production`.
5. **Cache store**: if `!state.hit_depth_limit` and the last segment is not a partial special, store complete subtrees grouped by consumed length in the span cache.
<

## Production Rotation

>D Production Rotation (`prng_shuffle`)
Productions for $A$ are tried in **rotated order**: if $A$ has $n$ productions, at depth `level` the order is:

$$[\text{level} \bmod n,\; (\text{level}+1) \bmod n,\; \ldots,\; (\text{level}+n-1) \bmod n]$$

This is `prng_shuffle(n, level)` in the source.
<

The rotation prevents systematic bias toward the first production at every depth. Without it, a left-recursive grammar like `Term ::= Term BaseTerm | Atom` would always try the recursive alternative first at every depth level, wasting work. With rotation, depth 0 starts with production 0, depth 1 starts with production 1, and so on. Different depths explore different alternatives first, distributing the search effort.

<!-- DIAGRAM: showing two depths exploring a left-recursive grammar in different production orders due to rotation -->

## Symbol Sequence Parsing

`parse_symbols(segments, symbols, abs_pos, level, state)` recurses through a production's RHS left to right:

- For each successful parse of the first symbol, it parses the remaining symbols over the unconsumed tail of segments.
- A **partial node** (incomplete match) is accepted only if it consumed all remaining segments — it can only appear at the frontier of the input.
- A local `rest_cache: HashMap<consumed, results>` avoids re-parsing the remaining symbols for identical consumed lengths.

## Terminal Parsing

At each terminal symbol, `parse_regex` calls `re.prefix_match(seg_text)` to classify the segment. See [Regex Engine](../regex.md) for the four outcomes. At end of input (empty segments), a `Terminal::Partial` with the full regex as remainder is produced unconditionally — this feeds the completion engine.

## Left Recursion

The parser handles left-recursive grammars such as:

```
Term(app) ::= Term[f] Term[arg]
Term(var) ::= /[a-z]+/
```

Left recursion is bounded by the cycle-detection counter: `(Term, pos)` is tracked, and after $\min(d_{\max}, |\text{segs}|+2)$ re-entries the branch is cut. The memoization cache then provides earlier results to deeper calls without re-expanding.

>N termination guarantee
The parser terminates because: (1) every complete parse must consume at least one segment, so the segment slice strictly shrinks, and (2) the cycle-detection counter bounds recursion at a fixed position. The combination ensures no infinite expansion.
<

## Incremental Parsing

The span cache persists across `partial()` calls on the same `Parser` instance. When input grows from $s$ to $s \cdot t$:

- Segments of $s$ that fall entirely within the already-parsed prefix remain valid in the cache.
- Segments at the boundary (whose span overlaps the old end of input) are excluded from cache lookup via `allow_cache_lookup` — they may have new parses available with more input.
- If the new segment sequence does not extend the previous one (different input entirely), the cache is cleared.

## Error Types

`ParseError` distinguishes four failure modes:

| Error | Meaning |
|-------|---------|
| `Tokenization(msg)` | Tokenizer rejected a character sequence |
| `NoStartSymbol` | Grammar has no start symbol |
| `NoValidParse` | Grammar rejects the input (permanent) |
| `DepthLimit` | Hit `max_recursion`; may succeed with deeper parse |

`DepthLimit` is the signal the [meta-parser](meta_parser.md) uses to trigger iterative deepening.

## Complexity

The parser is $O(n \times |G| \times d)$ where $n$ is the number of segments, $|G|$ is the grammar size (total production symbols), and $d$ is `max_recursion`. The span cache reduces the constant factor significantly for incremental inputs but does not change the asymptotic bound.
