
# Partial Trees and Forests

>D Partial Tree
A **partial tree** is a structure $t = (V, E,\lambda,\pi,\alpha,\text{root})$ where:
- $V$ is a finite set of nodes
- $E \subseteq V \times \mathbb{N} \times V$ is a finite set of directed, position-indexed edges
- $\lambda: V \to N \cup T$ assigns a label to each node
- $\pi: V \to P$ assigns the production used at each node
- $\alpha: V \to \mathbb{N}$ records the alternative index chosen at each non-terminal node
- $\text{root} \in V$ is the root node

Each partial tree corresponds to either a **complete** parse of input $s$, or a parse that can become complete by extending $s$. A partial tree **must** have consumed all input to be valid.
<

>D Partial Forest
The **partial parse forest** for input $s$ is the finite ordered set of all partial trees for $s$ produced by the parser. Trees in the forest differ by the alternatives chosen during parsing.
<

>R Forest Ordering
Parsing a production $A = \alpha \ |\ \beta$ creates two trees: $A(\alpha)$ and $A(\beta)$. The forest is ordered by the definition order of alternatives in the grammar. The alternative index function $\alpha$ records which choice was made at each non-terminal—this is essential for binding resolution.
<

We use the notation $x[i]$ to designate the node pointed at by the edge indexed at $i$ from node $x$.

### Tree Paths

>D Tree Path
The space of **tree paths** is $\mathcal{P} = \mathbb{N}^*$, where each coordinate is an edge index pointing to a specific child (similar to de Bruijn indexing). Every node $v \in V$ has a unique path $\text{path}(v)$ from the root:

$$\text{path}(v) = \begin{cases}
\varepsilon & \text{if } v \text{ is root} \\
	\text{path}(v') \cdot i & \text{if } v' \text{ is parent of } v \text{ and } v \text{ is its } i\text{-th child}
\end{cases}$$
<

>L Path Injectivity
Paths are injective: $\text{path}_T(x_1) = \text{path}_T(x_2) \implies x_1 = x_2$.
<

We extend the notation with *relative paths* $p = i_0 \cdot i_1 \cdots i_n$ using $x[p] = (x[i_0])[i_1 \cdot i_2 \cdots i_n]$.

### Completeness and Frontiers

>D Satisfied Symbol
For non-terminal $v \in V$ with production $\pi(v) = \alpha_0 \cdots \alpha_n$, symbol $\alpha_s$ is **satisfied** if $v[s]$ exists and is complete.
<

>D Complete Node
A node is **complete** by induction:
- A terminal is complete if it matched the full input (we ignore the non-complete case, handled [here](https://unsuspicious.org/blog/completing-regex/))
- A non-terminal $v$ is complete $\iff$ all symbols $\alpha_0, \ldots, \alpha_n$ in $\pi(v)$ are satisfied
<

>R Completeness and Language Membership
Using a parser $\Psi_L$, saying "node $v$ is complete" is equivalent to saying "the expression node $v$ represents belongs in language $L$".
<

>D Frontier
The **frontier** of a tree is the path to the incomplete node at the end of the tree. In a complete tree, there is no frontier. The frontier is unique.
<

>D Forest Completeness
A partial forest $\mathcal{F} = \{t_1,t_2,\cdots t_n\}$ is **complete** if and only if:
$$\exists k \ |\ t_k \text{ is complete}$$
<


>E Completing a Lambda Abstraction
**Input**: `λx:Int.`

**Partial Tree**:
```
T = Expression
     └─[0]→ Abstraction(abs)  [complete = false]
            ├─[0]→ "λ"
            ├─[1]→ Identifier("x")
            ├─[2]→ ":"
            ├─[3]→ Type
            │      └─[0]→ AtomicType
            │             └─[0]→ BaseType("Int")
            ├─[4]→ "."
            └─[5]→ Expression  [Missing / End of Input]
```

**Frontier**: path `[0, 5]`, where parsing stopped due to end of input.

The tree is **not complete** but is **completable**. The completion set consists of all strings satisfying the `Expression` production, for example:
 - `Expression → AtomicExpression → Variable → Identifier → /[a-z][a-zA-Z0-9]*/`

Choosing `"x"` as a completion yields the complete tree:

```
T' = Expression
      └─[0]→ Abstraction(abs)  [COMPLETE]
             ├─[0]→ "λ"
             ├─[1]→ Identifier("x")
             ├─[2]→ ":"
             ├─[3]→ Type
             │      └─[0]→ AtomicType
             │             └─[0]→ BaseType("Int")
             ├─[4]→ "."
             └─[5]→ Expression
                    └─[0]→ AtomicExpression
                           └─[0]→ Variable(dec)
                                  └─[0]→ Identifier("x")
```
<

>L Frontier Monotonicity
For partial parses $\mathcal{F}(s)$ extending to $\mathcal{F}(s \cdot t)$ with $t \in S'$:
$$\operatorname{front}\_{\mathcal{F}(s \cdot t)}(v) \gt \operatorname{front}\_{\mathcal{F}(s)}(v)$$
<

>P Frontier Monotonicity
Trivial by definition of $S'$: any valid completion must advance the frontier.
<

>R Monotonicity as a Debugging Tool
This property is useful for verifying parser correctness as if the frontier ever decreases, something is wrong.
<