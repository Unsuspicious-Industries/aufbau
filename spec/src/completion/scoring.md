#[D] Scoring

The scoring function $|\sigma|$ assigns a real-valued score to each search state, guiding the [best-first search](./search.md) toward promising completions. The score is a sum of five independent sub-scores, each capturing a different dimension of "how close is this state to a valid completion."

Source: [`src/logic/search/scoring.rs`](~/src/logic/search/scoring.rs)

## Score Tuple

>D State Score
The **state score** is a 5-tuple:

$$\sigma = (c, z, q, y, r) \in \mathbb{R}^5$$

with overall score:

$$|\sigma| = c + z + q + y + r$$

| Component | Name | Range | Direction |
|-----------|------|-------|-----------|
| $c$ | Completeness | $[0, 1]$ | higher = more complete |
| $z$ | Tree size penalty | $[-1, 0]$ | closer to 0 = smaller tree |
| $q$ | Typing quality | $[0, 1]$ | higher = better typed |
| $y$ | Simplicity | $[0, 1]$ | higher = shallower search depth |
| $r$ | Recursion penalty | $[-2.5, 0]$ | closer to 0 = less recursive |
<

The theoretical overall score range is approximately $[-3.5, 3.0]$. In practice, the recursion penalty $r$ and simplicity $y$ dominate: deep, recursive trees are heavily penalized, biasing the search toward shallow, direct completions. This is a design choice; shorter completions are more likely to match user intent.

## Sub-Score Definitions

### Completeness

>D Completeness Score
The **completeness score** $c \in [0, 1]$ estimates how close the partial forest is to being fully parsed. It is computed per-node and averaged:

$$c(v) = \begin{cases}
1.0 & \text{if } v \text{ is a complete terminal} \\\\
\frac{0.5}{|v|+1} & \text{if } v \text{ is a partial terminal with remainder length } |v| \\\\
0.1 & \text{if } v \text{ is an empty non-terminal (no children)} \\\\
\frac{1}{|C_v|}\sum_{u \in C_v} c(u) & \text{if } v \text{ is a non-terminal with children } C_v
\end{cases}$$

The forest-level score is $c = \frac{1}{|N|} \sum_{v} c(v)$ where $N$ is the total node count.
<

### Tree Size Penalty

>D Tree Size Penalty
The **tree size penalty** $z \in [-1, 0]$ penalizes large trees relative to a depth-dependent budget:

$$z = -\frac{|V|}{M(d)}$$

where $|V|$ is the node count and $M(d) = (d + 1) \times 10$ is the expected maximum size at search depth $d$.
<

The budget $M(d) = (d+1) \times 10$ is an empirical heuristic, assuming roughly 10 nodes per extension step. There is no formal derivation; it is a tuning parameter.

### Typing Quality

>D Typing Quality Score
The **typing quality** $q \in [0, 1]$ measures the ratio of complete, well-typed roots in the forest, with an ambiguity penalty:

$$q = \frac{|\\{r \in \text{roots} : r \text{ is complete}\\}|}{|\text{roots}|} - 0.2 \cdot (|\text{roots}| - 1)$$

Clamped to $[0, 1]$.
<

The $0.2 \times (|\text{roots}| - 1)$ term penalizes ambiguous parses. A forest with a single complete root scores optimally. Each additional root reduces the score by $0.2$, reflecting a preference for unambiguous completions.

### Simplicity

>D Simplicity Score
The **simplicity score** $y \in [0, 1]$ rewards shallower search paths:

$$y = 1 - \frac{d}{d_{\max}}$$

where $d$ is the current search depth and $d_{\max}$ is the configured maximum depth.
<

### Recursion Penalty

>D Recursion Penalty
The **recursion penalty** $r \in [-2.5, 0]$ penalizes deeply nested parse trees super-linearly:

$$r = -2.5 \cdot \left(\frac{D(T)}{d_{\max} + 1}\right)^2$$

where $D(T)$ is the maximum depth of any node in the parse tree.
<

The coefficient $2.5$ and the quadratic exponent are empirical. Quadratic scaling means a tree at half the maximum depth incurs $\frac{2.5}{4} = 0.625$ penalty, while a tree at full depth incurs the full $2.5$. This strongly discourages the search from exploring deeply recursive grammar structures (e.g., deeply nested parenthesized expressions).

## Ordering

>W NaN Handling
The `Ord` implementation on `ScoredState` uses `partial_cmp(...).unwrap_or(Equal)`. If a score component evaluates to `NaN` (e.g., division by zero in an edge case), the comparison silently treats the NaN-containing state as equal to any other. This could cause non-deterministic heap ordering. In practice, the score formulas avoid division by zero, but the fallback is a latent robustness concern.
<
