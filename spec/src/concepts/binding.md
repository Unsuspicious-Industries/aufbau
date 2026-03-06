#[D] Binding Resolution

Given grammar $G = (N, T, P, S, \Theta, A)$, we construct a **binding map** $\beta: \mathcal{B} \times P \to \mathcal{P}^*$ associating binding identifiers with **grammar paths**.

>D Grammar Path
A **grammar path** is a sequence of non-terminals, production indices, and alternative indices:
$$ p = x_1@a_1 \to x_2@a_2 \to \dots \to x_n@a_n $$
where:
- $x_i \in \mathbb{N}$: index in the production.
- $a_i \in \mathbb{N}$: alternative index chosen for non-terminal $x_i$.
<

Grammar paths navigate grammar structure relative to a production. Alternative indices act as constraints: we only follow a path if the tree node's chosen alternative matches the path. This allows grammar paths to convert to tree paths at runtime while ensuring the tree respects the specified structure.

### Regular Grammar Paths

>D Regular Grammar Path
For recursive structures, **Regular Grammar Paths** are regular expressions over $\Sigma = \mathbb{N} \times \mathbb{N}$, where $i@k$ represents traversing the $i$-th child of a node selecting alternative $k$.
<

Example: binding `τ` through nested parentheses:
$$ \beta(\tau, \text{Abstraction}) = 3@0 \cdot (0@1 \cdot 1@0)^* \cdot 0@0 \cdot 0 $$

### Binding Map Construction

For each production $p \in P$ with rule $\Theta(p)$:

1. **Extraction**: Let $\mathcal{B}_\theta$ be bindings mentioned in $\Theta(p)$.
2. **Traversal**: For $b \in \mathcal{B}_\theta$, find $b$ in $p$ at position $k$. If $\alpha_k[b]$ is a non-terminal, recursively explore alternatives in $A(\alpha_k)$ via BFS.
3. **Invariant Extraction**: Compute acyclic grammar paths $\mathcal{P}_b^p$ from rule $\theta$ to binding $b$. Paths must include alternative annotations. Use regular grammar paths for cycles.
4. **Store**: $\beta(b, p) = \mathcal{P}_b^p$.

Grammar paths are static; they are fixed at load time and guide tree path generation during evaluation.

### Binding Resolution

For tree $T$, binding $b$, and node $N$, we resolve nodes $V_b$ by matching tree paths against $R_b = \beta(b, N)$. A node $v$ matches if:
1. **Edge Match**: The edge index sequence matches $R_b$.
2. **Alternative Verification**: For every step $i@k$, the tree node has alternative $k$.

If edge match succeeds but alternative verification fails for all invariants, the parse is rejected.

### Correctness and Constraints

>T Binding Invariance
For any binding $b$ in production $p$, $\beta(b, p)$ is uniquely determined by the grammar structure and independent of input.
<

>A Binding Uniqueness
If $|\beta(b, p)| > 1$, then $b$ must be declared as tuple-typed in $\Theta(p)$ as `(b...)`.
<

>R Limitations
The construction algorithm enforces a limit: `MAX_RECURSION_DEPTH = 16`. Paths deeper than this are omitted. Consequently, $\beta(b, p)$ is an under-approximation for deeply recursive grammars, and bindings at extreme depths may fail to resolve.
<