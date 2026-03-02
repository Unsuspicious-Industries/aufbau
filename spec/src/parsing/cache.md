#[D] Span Cache

The **span cache** is a memoization table for the chart parser, indexed by four dimensions. It enables incremental reuse of parse results across input extensions.

Source: [`src/logic/partial/cache.rs`](~/src/logic/partial/cache.rs)

## Structure

>D Span Cache
The **span cache** is a 4-dimensional table:

$$\mathcal{S}: \text{depth} \times N \times \text{start} \times \text{span} \to \mathcal{P}(\text{NonTerminal})$$

mapping:
- **depth** $d$: the parse depth at which the result was computed
- **nonterminal** $A \in N$: the nonterminal being recognized
- **start segment** $i$: the starting position in the segment array
- **span length** $\ell$: the number of segments consumed

to a set of partial parse trees rooted at $A$.
<

>D Validity Bound
The cache maintains a secondary table:

$$\text{max\_len}: \text{depth} \times N \times \text{start} \to \mathbb{N}$$

recording the maximum span length for which the cache can definitively answer "no results" (the `can_answer` bound). This prevents false negatives: the cache only returns $\emptyset$ if $\ell \leq \text{max\_len}(d, A, i)$.
<

## Depth Sharding

>L Depth Independence
Parse results at depth $d$ are **not reused** at depth $d' > d$. A deeper parse may explore more alternatives (due to the depth bound in the chart parser), so results computed at a shallower depth are potentially incomplete.
<

## Merge Semantics

When two cache entries collide (same key), the result sets are **merged** (set union), not replaced. This is correct because the cache accumulates all valid parse trees for a given span: a new parse path may discover additional trees without invalidating previously found ones.

## Cross-Parse Reuse

When input $s$ is extended to $s \cdot t$, cache entries for segments that are entirely within $s$ remain valid (the prefix is stable). Only entries whose span overlaps the extension boundary may need recomputation.

## Cache Monitor

`CacheMonitor` (`src/logic/partial/monitor.rs`) is an optional instrumentation
layer attached to the span cache.  It is disabled by default and must be
explicitly enabled; all recording methods are no-ops when disabled.

The monitor is split into two snapshot types that can be extracted independently.

### `CacheStatsSnapshot`

Counters that accumulate across the lifetime of a parse session.

| Field | Type | Description |
|-------|------|-------------|
| `enabled` | `bool` | Whether monitoring is active |
| `cache_clears` | `u64` | Number of full cache clears |
| `cache_invalidations` | `u64` | Number of partial invalidations at the prefix boundary |
| `lookups` | `u64` | Total lookup attempts |
| `lookup_hits_exact` | `u64` | Lookups that returned an exact span match |
| `lookup_hits_prefix` | `u64` | Lookups that returned a reusable prefix match |
| `lookup_misses` | `u64` | Lookups that found no usable entry |
| `lookup_scanned_entries` | `u64` | Total cache entries scanned across all lookups |
| `stores` | `u64` | Total store operations (`updates + inserts`) |
| `store_inserts` | `u64` | New cache entries added |
| `store_updates` | `u64` | Existing entries merged (set union) |
| `store_entries_after` | `u64` | Cache size after the last store |
| `depth_limited_parses` | `u64` | Parses that hit the depth limit without completing |

The hit ratio is $(\text{lookup\_hits\_exact} + \text{lookup\_hits\_prefix}) / \text{lookups}$. A low hit ratio under repeated partial parses of similar inputs suggests the invalidation is too aggressive.

### `CacheTimingSnapshot`

Wall-time accumulators for the three hot paths.

| Field | Type | Description |
|-------|------|-------------|
| `partial_calls` | `u64` | Number of `parser.partial()` calls recorded |
| `partial_total` | `Duration` | Cumulative time spent in `partial()` |
| `partial_last` | `Option<Duration>` | Duration of the most recent `partial()` call |
| `lookup_total` | `Duration` | Cumulative time spent in cache lookups |
| `store_total` | `Duration` | Cumulative time spent in cache stores |

Snapshots are cheap clones taken via `CacheMonitor::stats_snapshot()` and
`CacheMonitor::timing_snapshot()`; neither method resets the monitor.
`CacheMonitor::reset()` zeroes all counters and timers while preserving the
`enabled` flag.
