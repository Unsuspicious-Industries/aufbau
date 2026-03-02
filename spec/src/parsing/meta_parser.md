#[D] Meta-Parser

The **meta-parser** adaptively determines the parse depth required for a grammar.

Source: [`src/logic/partial/meta.rs`](~/src/logic/partial/meta.rs)

## Constants

| Constant | Value | Meaning |
|---|---|---|
| `DEFAULT_START_DEPTH` | 5 | First depth tried |
| `DEFAULT_MAX_DEPTH` | 256 | Hard ceiling; search aborts above this |
| `DEFAULT_DEPTH_FACTOR` | 1.5 | Multiplicative growth on depth-limit failure |
| `DEFAULT_POST_SUCCESS_STEPS` | 1 | Extra increment steps after first success |

## Adaptive Depth Search

>D Depth Search
The meta-parser determines the minimum sufficient parse depth via iterative deepening:

1. Start at $d_0 = 5$ (`DEFAULT_START_DEPTH`).
2. Call `Parser::partial(input)` at depth $d$, yielding a `PartialParseOutcome`.
3. On `Success`: accumulate results. If the parser hit the depth limit on this invocation, enter post-success steps (see below). Otherwise return.
4. On `Failure(DepthLimit)`: set $d \leftarrow \lceil d \cdot 1.5 \rceil$ (minimum increment of 1) and retry.
5. On `Failure(NoValidParse)` in non-incremental mode: stop immediately — higher depth will not help.
6. Abort if $d > 256$.
<

The $1.5\times$ growth factor with ceiling balances aggressive doubling (which overshoots and wastes work on highly ambiguous grammars) against conservative linear increment (which may take many iterations for deeply recursive grammars).

## Post-Success Steps

After the first successful parse, if the underlying parser reports that it hit the recursion limit (`last_hit_depth_limit()`), the meta-parser continues probing at $d+1$ (up to `DEFAULT_POST_SUCCESS_STEPS = 1` extra level). The extra level may reveal parse trees that were truncated at the boundary. Results from all levels are merged, deduplicating roots by structural equality.

This is distinct from the main multiplicative loop: post-success steps use linear `+1` increments and are bounded, not exponential.

## Start Depth Policy

>D Start Depth
The meta-parser always starts from `start_depth` for each input, then applies multiplicative depth growth (`ceil(d * factor)`) when depth limits are hit.
<

## Parse and Partial Methods

`MetaParser` exposes three public parse entry points:

| Method | Returns | Description |
|---|---|---|
| `parse(input)` | `PartialAST` (complete roots only) | Calls `partial`, then filters to `completes()` |
| `partial(input)` | `PartialAST` (all roots) | Full depth search, accumulates all depths |
| `partial_typed(input)` | `PartialAST` (typed roots only) | `partial` then `filter_typed` against grammar |
| `partial_with_depth(input)` | `(PartialAST, usize)` | `partial` plus the last successful depth used |

## Failure Modes

On `Failure(NoValidParse)` in non-incremental mode the search stops immediately — the grammar does not accept the input as a prefix at any depth. On incremental mode, a `NoValidParse` result still applies the $1.5\times$ growth: the extension may have made the prefix temporarily unparseable at the cached depth.

If the search exhausts `max_depth` without any success, `partial` returns `Err("No parse results after trying depths X to Y")`.

## Builder API

`MetaParser` uses a builder pattern for configuration:

```text
MetaParser::new(grammar)
    .with_start_depth(3)
    .with_max_depth(128)
    .with_depth_factor(2.0)
    .with_post_success_steps(2)
```

The depth factor is clamped to a minimum of 1.01 to guarantee progress.
