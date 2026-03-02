#[D] Search

The search engine explores the space of token-level extensions to find a complete, well-typed expression. It uses a priority queue ordered by a heuristic [scoring function](./scoring.md).

Source: [`src/logic/search/mod.rs`](~/src/logic/search/mod.rs)

## Search Space

>D Search Space
The **search space** is a labeled transition system $(Q, \Sigma_t, \delta, q_0, F)$ where:

- $Q$ is the set of **search states**, each a partial forest $\mathcal{F}$ paired with a token path and depth counter
- $\Sigma_t$ is the set of **typed tokens**, `DerivativeRegex` values that pass the type filter
- $\delta: Q \times \Sigma_t \rightharpoonup Q$ is the **transition function**, extending the input by one token via the [synthesizer](./synthesizer.md) and producing a new partial forest. $\delta$ is partial: not every token produces a valid parse
- $q_0 = (\mathcal{F}(s_0), [], 0)$ is the **initial state**, the parse of the original input
- $F \subseteq Q$ is the set of **accepting states**, states containing a complete, well-typed root at the start nonterminal
<

>D Search State
A **search state** $q = (\mathcal{F}, \pi, d)$ consists of:

- $\mathcal{F}$: a partial forest (the current parse result)
- $\pi \in \Sigma_t^*$: the sequence of tokens applied since $q_0$ (the **completion path**)
- $d \in \mathbb{N}$: the depth in the search tree (number of extensions applied)
<

## Algorithm

>D Search Algorithm
The search algorithm is **best-first search** over the search space $(Q, \Sigma_t, \delta, q_0, F)$:

1. Parse input $s_0$ into $\mathcal{F}_0$. Reject if $\mathcal{F}_0$ fails the root validity check.
2. Initialize a max-heap $H$ with $(|\sigma(q_0)|, q_0)$.
3. Initialize a visited set $V = \emptyset$ (keyed by input string).
4. **Loop**: pop the highest-scored state $q = (\mathcal{F}, \pi, d)$ from $H$.
   - If $q \in F$ (has a valid completion), return $\text{Success}(\mathcal{F}, \pi, d)$.
   - If $d \geq d_{\max}$, skip expansion.
   - Otherwise, compute $C = \text{typed\_completions}(\mathcal{F})$.
   - For each $t \in C$: compute $q' = \delta(q, t)$. If $q'$ is defined, its input $\notin V$, and it passes the root validity check, add $q'.\text{input}$ to $V$ and push $(|\sigma(q')|, q')$ onto $H$.
5. If $H$ is empty, return $\text{Exhausted}$.
<

>R On the Name "DFS"
The module is documented as "Priority-guided DFS search." This name is historical and relative: the predecessor algorithm was **pure BFS**, expanding every completion at every depth level before proceeding to the next. The current algorithm replaced that with a `BinaryHeap` (max-heap), making it structurally more depth-oriented in practice since high-scoring deep states are preferred over low-scoring shallow ones.

However, it is **not true DFS**. True DFS uses a stack (LIFO) and commits to fully exploring one branch before backtracking. This algorithm uses a **priority queue**: the next state popped is always the *globally highest-scored* state, regardless of which branch produced it. It freely interleaves exploration across branches whenever a state from a different branch scores higher than the current branch's frontier. The depth bound $d_{\max}$ is a safety cutoff, not a stack discipline.

Concretely: if branch $A$ at depth 3 scores $2.1$ and branch $B$ at depth 1 scores $2.3$, the algorithm explores $B$ next. True DFS would continue with $A$. The algorithm makes no commitment to a single branch.

Formally, this is **best-first search (BeFS)** with scoring function $|\sigma|$. The "DFS" label reflects the intent (explore depth before breadth, relative to the predecessor) not the mechanism.
<

## Configuration

>D Search Configuration
The **search configuration** $\kappa = (d_{\max}, n_{\max})$ consists of:

- $d_{\max} = 10$: maximum search depth (number of token extensions from the initial state)
- $n_{\max} = 10$: maximum number of concrete examples to try per regex token (passed to `extend_with_regex`)
<

## Result

>D Search Result
The search returns one of three outcomes:

- $\text{Success}(s', T, \pi, d)$: a complete input $s'$, its parse tree $T$ (a complete `NonTerminal` at the start symbol), the completion path $\pi$, and the depth $d$
- $\text{Exhausted}(d_{\max}, n, |V|)$: the search explored $n$ states and visited $|V|$ unique inputs without finding a valid completion within depth $d_{\max}$
- $\text{Invalid}(m)$: the initial input $s_0$ cannot be parsed or fails the root validity check; $m$ is a diagnostic message
<

## Acceptance Criteria

The search uses two distinct predicates: one loose (for continuing exploration) and one strict (for accepting a solution).

>D Root Validity (Exploration Filter)
A search state $q$ passes the **root validity check** if **any** root $r$ in the forest satisfies:

$$r \text{ is incomplete} \quad \lor \quad (r \text{ is complete} \land \Gamma \vdash\_\Theta r \in \\{\text{Valid}(\tau), \text{Partial}(\tau)\\})$$

Incomplete roots are **always admitted** because their type status is undetermined.
<

>L Validity Over-Approximation
The root validity check is a **sound over-approximation** of reachability to a valid completion. Incomplete roots are admitted because their type constraints have not yet been fully evaluated; rejecting them would prune states that might lead to valid completions. Only complete roots are subjected to the full type check.

This means the search may explore states that eventually dead-end at invalid completions, but it will never reject a state that could have led to a valid one.
<

>D Valid Completion (Acceptance Criterion)
A forest $\mathcal{F}$ contains a **valid completion** if there exists a root $r \in \mathcal{F}$ such that:

1. $r$ is **complete** (all symbols satisfied)
2. $\lambda(r) = S$ (labeled with the start nonterminal)
3. $\Gamma \vdash\_\Theta r : \text{Valid}(\tau)$ for some type $\tau$

Unlike the exploration filter, $\text{Partial}$ is **not accepted** here. Only fully $\text{Valid}$ trees are accepted as solutions.
<

## State Deduplication

The search maintains a `HashSet<String>` keyed by the **input string** of each state. Two states with identical input strings are considered equivalent, even if they arrived via different token paths. This prevents re-exploration of states that produce the same parse forest (since parsing is deterministic given a fixed input and grammar).

>I search trace
{"label":"search: complete \u03bbx:","input":"\u03bbx:","steps":[{"token":"pop","tokens":["pop"],"display":"H = [(\u03bbx:, 2.1)]\npop best: s = \u03bbx:\nroots: Abstraction (incomplete)\nno valid completion, expand"},{"token":"Int","tokens":["Int","Bool","A"],"display":"extend with Int\ns = \u03bbx:Int\nscore(\u03bbx:Int) = 1.8\nH = [(\u03bbx:Int, 1.8), (\u03bbx:Bool, 1.6)]"},{"token":"pop","tokens":["pop"],"display":"pop best: s = \u03bbx:Int\nroots: Abstraction (incomplete)\nno valid completion, expand"},{"token":".","tokens":["."],"display":"extend with .\ns = \u03bbx:Int.\nH = [(\u03bbx:Int., 1.5)]"},{"token":"pop","tokens":["pop"],"display":"pop best: s = \u03bbx:Int.\nexpand with typed_completions"},{"token":"x","tokens":["x","1","true"],"display":"extend with x\ns = \u03bbx:Int.x\nroot: complete \u2713, Valid(Int)\naccept!"}]}
<
