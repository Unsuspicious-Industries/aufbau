#[D] Search

The search engine explores the space of token-level extensions to find a complete, well-typed expression. It uses a priority queue ordered by a heuristic [scoring function](./scoring.md).

Source: [`src/logic/search/mod.rs`](~/src/logic/search/mod.rs)

This chapter is meant to be read after the [completion overview](../completion.md) and [synthesizer](./synthesizer.md) chapters. It only describes search-side control flow: frontier management, pruning, and stopping conditions. Token generation lives in [Synthesizer](./synthesizer.md); the numeric heuristic itself lives in [Scoring](./scoring.md).

## Operational Shape

>D Search Shape
The current implementation has two phases:

1. A **greedy fast path** (`try_greedy_complete`) that repeatedly takes the locally best child and returns immediately if it reaches a complete root.
2. A **best-first frontier search** (`search_complete`) that uses a `BinaryHeap` once greedy completion stalls.

Both phases use the same synthesizer operations and the same `SearchConfig` budgets.
<

## Search Space

>D Search Space
The **search space** is operationally an exploration over typed states $(T, \pi, d)$ where:

- $Q$ is the set of all such typed states
- $T$ is the current typed parse forest (`TypedAST`)
- $\pi \in \Sigma_t^*$ is the sequence of regex completion tokens chosen so far
- $d \in \mathbb{N}$ is the search depth
- $q_0 = (T_0, [], 0)$ is the initial state, where $T_0$ is the typed parse of the original input
- $F \subseteq Q$ is the set of states whose typed forest contains a complete root

Let $\Sigma_t$ be the set of regex completion tokens produced by `completions_ctx`. For a state $q$ and token $t \in \Sigma_t$, the search asks the [synthesizer](./synthesizer.md) to try up to $n_{\max}$ concrete witnesses for $t$, producing a finite child set $\Delta(q, t) \subseteq Q$.
<

>D Search State
A **search state** $q = (T, \pi, d)$ consists of:

- $T$: a typed parse forest (the current `TypedAST`)
- $\pi$: the regex-token completion path. This records completion tokens, not the concrete witness strings tried for regex expansion.
- $d$: the number of successful extensions applied since $q_0$
<

## Algorithm

>D Search Algorithm
The current implementation uses two phases over the same state representation:

1. Build a `Synthesizer` for $s_0$ and parse the initial typed forest $T_0 = \text{partial\_typed\_ctx}(s_0, \Gamma)$. Reject with `Invalid` if this fails.
2. Seed the visited set with `T_0.text()`.
3. Run a **greedy fast path** (`try_greedy_complete`) for up to $d_{\max}$ steps:
   - ask the synthesizer for `completions_ctx(\Gamma)`
   - expand each regex token using up to $n_{\max}$ concrete witnesses
   - keep the locally best child (grounding first, then open slots, then score)
   - return immediately if a complete root is reached
4. If greedy completion stalls, initialize a max-heap $H$ with $q_0 = (T_0, [], 0)$ scored by [Scoring](./scoring.md).
5. **Loop**: pop the highest-scored state $q = (T, \pi, d)$ from $H$.
   - If $T$ has a complete root, return `Success`.
   - If $d \geq d_{\max}$, skip expansion.
   - If the total explored-state budget $s_{\max}$ has been hit, stop and return `Exhausted`.
   - Otherwise, set the synthesizer input to `T.text()`, compute `completions_ctx(\Gamma)`, and expand each token via `extend_all_with_regex_candidates(..., n_{\max})`.
   - Discard children whose typed forest is empty or whose `text()` has already been visited.
   - If any child has a grounded root (`ty != Any`), discard children whose roots are all `Any`.
   - Sort remaining children by grounded-root count, then score, keep only the top $b_{\max}$, and push them onto $H$.
6. If $H$ becomes empty, return `Exhausted`.
<

The module is documented as "Priority-guided DFS search." This name is historical and relative: the predecessor algorithm was **pure BFS**, expanding every completion at every depth level before proceeding to the next. The current algorithm replaced that with a `BinaryHeap` (max-heap), making it structurally more depth-oriented in practice since high-scoring deep states are preferred over low-scoring shallow ones. This label applies to the heap-driven second phase; the greedy fast path runs before any frontier is built.

However, it is **not true DFS**. True DFS uses a stack (LIFO) and commits to fully exploring one branch before backtracking. This algorithm uses a **priority queue**: the next state popped is always the *globally highest-scored* state, regardless of which branch produced it. It freely interleaves exploration across branches whenever a state from a different branch scores higher than the current branch's frontier. The depth bound $d_{\max}$ is a safety cutoff, not a stack discipline.

Concretely: if branch $A$ at depth 3 scores $2.1$ and branch $B$ at depth 1 scores $2.3$, the algorithm explores $B$ next. True DFS would continue with $A$. The algorithm makes no commitment to a single branch.

Formally, this is **best-first search (BeFS)** with scoring function $|\sigma|$. The "DFS" label reflects the intent (explore depth before breadth, relative to the predecessor) not the mechanism.


## Configuration

>D Search Configuration
The **search configuration** $\kappa = (d_{\max}, n_{\max}, s_{\max}, b_{\max})$ in `SearchConfig::default()` currently consists of:

- $d_{\max} = 10$: maximum search depth (number of token extensions from the initial state)
- $n_{\max} = 1$: maximum number of concrete examples to try per regex token during regex expansion
- $s_{\max} = 96$: total explored-state budget before returning `Exhausted`
- $b_{\max} = 12$: beam width per expanded state after local sorting
<

## Result

>D Search Result
The search returns one of three outcomes:

- $\text{Success}(s', T, \pi, d)$: a complete input $s'$, the first complete typed root $T$, the regex-token completion path $\pi$, and the depth $d$
- $\text{Exhausted}(d_{\max}, n, V)$: the frontier emptied or the state budget was hit after exploring $n$ states; $V$ is the list of deduplicated reconstructed inputs visited so far
- $\text{Invalid}(m)$: the initial input $s_0$ could not be partially typed; $m$ is a diagnostic message
<

## Search-Side Checks

The search code itself performs only lightweight checks. The meaning of the attached types comes from earlier chapters and from `partial_typed_ctx`; this chapter only documents what the search layer does with those typed forests.

>D Expansion Filter
A child produced during expansion is kept only if:

1. regex expansion produced a non-empty `TypedAST`
2. its reconstructed input `text()` has not been visited before
3. after the local grounding preference, it survives the beam-width cutoff

Search does not run a separate `Valid`/`Partial`/`Invalid` predicate here; it relies on the synthesizer to produce typed child forests in the first place.
<

>L Grounding Preference
During one expansion step, if any child has at least one root whose type is not `Any`, the implementation drops children whose roots are all `Any` before score ordering. This is a local search policy, not part of the numeric [scoring](./scoring.md) function.
<

>D Success Condition
A state is accepted as soon as its `TypedAST` contains a complete root (`TypedNode::is_complete()`). In normal use this corresponds to a complete parse from the grammar start symbol, because the parser roots are generated from the grammar entry point.
<

## State Deduplication

The search maintains a `HashSet<String>` keyed by the reconstructed surface input `tree.text()` of each state. Two states with identical reconstructed inputs are considered equivalent, even if they arrived via different token paths. This prevents re-exploration of states that parse the same surface string.

For how the surviving states are numerically ranked, continue to [Scoring](./scoring.md).

>I search trace
{"label":"search: complete \u03bbx:","input":"\u03bbx:","steps":[{"token":"pop","tokens":["pop"],"display":"H = [(\u03bbx:, 2.1)]\npop best: s = \u03bbx:\nroots: Abstraction (incomplete)\nno valid completion, expand"},{"token":"Int","tokens":["Int","Bool","A"],"display":"extend with Int\ns = \u03bbx:Int\nscore(\u03bbx:Int) = 1.8\nH = [(\u03bbx:Int, 1.8), (\u03bbx:Bool, 1.6)]"},{"token":"pop","tokens":["pop"],"display":"pop best: s = \u03bbx:Int\nroots: Abstraction (incomplete)\nno valid completion, expand"},{"token":".","tokens":["."],"display":"extend with .\ns = \u03bbx:Int.\nH = [(\u03bbx:Int., 1.5)]"},{"token":"pop","tokens":["pop"],"display":"pop best: s = \u03bbx:Int.\nexpand with completions_ctx"},{"token":"x","tokens":["x","1","true"],"display":"extend with x\ns = \u03bbx:Int.x\nroot: complete \u2713, Valid(Int)\naccept!"}]}
<
