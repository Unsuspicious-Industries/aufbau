#[D] Scoring

The scoring function $|\sigma|$ assigns a real-valued score to each search state, guiding the [best-first search](./search.md) toward promising completions. This chapter documents only the numeric heuristic in `calculate_score()`. Deduplication, grounding preference, greedy completion, and beam truncation are search-policy details described in [Search](./search.md).

Source: [`src/logic/search/scoring.rs`](~/src/logic/search/scoring.rs)

This chapter is meant to be read after [Search](./search.md).

## Score Tuple

>D State Score
The **state score** is a 6-tuple:

$$\sigma = (c, p, \ell, o, s, r) \in \mathbb{R}^6$$

with overall score:

$$|\sigma| = c + p + \ell + o + s + r$$

| Component | Name | Range | Direction |
|-----------|------|-------|-----------|
| $c$ | Completeness | $[0, 2]$ | higher = more matched structure |
| $p$ | Production fullness | $[0, 1]$ | higher = more RHS positions filled |
| $\ell$ | Token length bonus | slow-growing, non-negative | higher = more concrete terminals |
| $o$ | Open slots penalty | $\{0, -0.3, -0.6, \ldots\}$ | closer to 0 = fewer missing children |
| $s$ | Simplicity | $[0, 0.3]$ | higher = shallower search depth |
| $r$ | Recursion penalty | $[-0.5, 0]$ | closer to 0 = shallower tree |
<

Several components choose the best or shallowest root among alternatives rather than averaging over all roots. This is intentional: one promising parse alternative should be able to dominate the score early instead of being drowned out by worse siblings.

## Sub-Score Definitions

### Completeness

>D Completeness Score
For each root $u$, walk all nodes in the subtree. A complete terminal contributes $1$. A partial terminal whose current surface text has length $m$ contributes $\frac{0.5}{m+1}$. An expression node with no children contributes $0$.

Let $S_u$ be the sum of those contributions and $N_u$ the total node count visited. The implementation computes

$$c(u) = \min\left(2,\ 2 \cdot \frac{S_u}{N_u}\right), \qquad c = \max_u c(u).$$
<

Expression nodes with children do not add direct bonus here; they only contribute through their descendants while still increasing the denominator. So the score prefers states with more concrete material and fewer empty placeholders.

### Production Fullness

>D Production Fullness Score
For each expression node $v$ with expected right-hand side length $n_v > 0$ and currently filled child count $k_v > 0$, define its local fill ratio as $\frac{k_v}{n_v}$. For a root $u$, let $E_u$ be the set of such expression nodes.

The implementation computes the RMS fill ratio

$$p(u) = \sqrt{\frac{1}{|E_u|}\sum_{v \in E_u}\left(\frac{k_v}{n_v}\right)^2}, \qquad p = \max_u p(u),$$

with $p = 0$ if $E_u$ is empty.
<

This rewards roots whose productions are already mostly filled, even if the tree is not yet complete.

### Token Length Bonus

>D Token Length Bonus
Let $L_u$ be the number of leaf terminals in root $u$. The implementation computes

$$\ell = 0.25 \cdot \sqrt{\max_u L_u}.$$

This is a small bonus for states that already contain more concrete terminal material. The square root keeps the signal mild.
<

### Open Slots Penalty

>D Open Slots Penalty
For each root $u$, count open slots recursively:

- an expression node with no children contributes $1$
- an expression node with expected RHS length $n_v$ and $k_v$ filled children contributes $n_v - k_v$
- children are then counted recursively as well

Let $O_u$ be the resulting count. The implementation uses the best (smallest) root:

$$o = -0.3 \cdot \min_u O_u.$$
<

This is the dominant negative signal in practice: trees with fewer remaining holes tend to outrank structurally larger but more incomplete alternatives.

### Simplicity

>D Simplicity Score
The **simplicity score** rewards shallower search paths:

$$s = 0.3 \cdot \left(1 - \frac{d}{d_{\max}}\right)$$

where $d$ is the current search depth and $d_{\max}$ is the configured maximum depth.
<

### Recursion Penalty

>D Recursion Penalty
Let $D_u$ be the maximum node depth of root $u$. The implementation takes the shallowest root among alternatives and applies a light quadratic penalty:

$$r = -0.5 \cdot \left(\min\left(1, \frac{\min_u D_u}{d_{\max} + 1}\right)\right)^2.$$

This discourages unnecessarily deep trees, but much more gently than the open-slots penalty discourages incomplete ones.
<

## Ordering

>D Score Use
Search uses the numeric score in two places:

1. heap priority for frontier states
2. secondary ordering among children after expansion

Before score comparison, [Search](./search.md) may prefer children with grounded roots (`ty != Any`) and may truncate to a beam width. So score is important, but it is not the whole search policy.
<
