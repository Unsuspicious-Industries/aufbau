#[D] Partial Trees and Forests

>D Partial Tree
A **partial tree** $t = (V, E,\lambda,\pi,\alpha,\text{root})$ is a structure where:
- $V$: finite set of nodes.
- $E \subseteq V \times \mathbb{N} \times V$: position-indexed edges.
- $\lambda: V \to N \cup T$: node labels.
- $\pi: V \to P$: production associated with the node.
- $\alpha: V \to \mathbb{N}$: chosen alternative index.
- $\text{root} \in V$: the start node.
<

### Tree Paths

>D Tree Path
The space of **tree paths** is $\mathcal{P} = \mathbb{N}^*$, where each coordinate is an edge index pointing to a specific child. Every node $v \in V$ has a unique path $\text{path}(v)$ from the root:

$$\text{path}(v) = \begin{cases}
\varepsilon & \text{if } v \text{ is root} \\\\
\text{path}(v') \cdot i & \text{if } v' \text{ is parent of } v \text{ and } v \text{ is its } i\text{-th child}
\end{cases}$$
<

>L Path Injectivity
Paths are injective: $\text{path}_T(x_1) = \text{path}_T(x_2) \implies x_1 = x_2$.
<

We use $x[i]$ for the child at index $i$, and extend this to relative paths $x[p]$.

### Terminal Status and Extensibility

A terminal node matching input $s$ has a status:
- **Complete**: Matched a full token. It may carry an **extension** derivative $D_s(r)$ for further matching.
- **Partial**: Matched only a prefix. It carries a **remainder** derivative for what must follow.

>D Extensibility
A terminal $v$ is **extensible** if its derivative with respect to its matched text is non-empty:
$$\text{extensible}(v) \iff D_{\text{matched}(v)}(r_v) \neq \emptyset$$
<

A terminal can be **both complete and extensible**. For example, `/[a-z]+/` matching `foo` is complete but extensible by `bar`. Conversely, a terminal is incomplete and not extensible only if its remainder regex is $\emptyset$ (a parse error). This distinction is critical for [binding resolution](./binding.md) where bindings to extensible terminals on the rightmost spine are treated as `Partial`.

### Completeness and Frontiers

A node is **complete** by induction:
- A terminal is complete if it matched the full input.
- A non-terminal $v$ is complete $\iff$ all symbols in its production $\pi(v)$ are satisfied.

>D Frontier
The **frontier** is the path to the unique incomplete node where parsing stopped. In a complete tree, there is no frontier.
<

>D Partial Forest
The **partial forest** for input $s$ is the finite ordered set of all partial trees. Trees differ by the alternatives chosen; the forest is ordered by the grammar's definition order.
<

>D Forest Completeness
A forest is **complete** $\iff$ at least one tree in it is complete.
<

### Monotonicity

For partial parses $\mathcal{F}(s)$ extending to $\mathcal{F}(s \cdot t)$, the frontier path length is monotonically non-decreasing.

>L Frontier Monotonicity
$\operatorname{front}(\mathcal{F}(s \cdot t)) \geq \operatorname{front}(\mathcal{F}(s))$
<

This property is a primary invariant for parser correctness; a decreasing frontier indicates an engine failure.

>E Lambda Completion
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

**Frontier**: path `[0, 5]`. Choosing `"x"` as a completion yields a complete tree where the `Abstraction` node is satisfied.
<

>I build partial tree
{"label":"parse: \u03bbx:Int.","input":"","steps":[{"token":"\u03bb","tokens":["\u03bb"],"display":"Expression\n \u2514 Abstraction\n   \u251c [0] \u03bb \u2713\n   \u2514 [1..5] missing\nfrontier: [0,1]"},{"token":"x","tokens":["x"],"display":"Expression\n \u2514 Abstraction\n   \u251c [0] \u03bb \u2713\n   \u251c [1] Ident(x) \u2713\n   \u2514 [2..5] missing\nfrontier: [0,2]"},{"token":":","tokens":[":"],"display":"Expression\n \u2514 Abstraction\n   \u251c [0] \u03bb \u2713\n   \u251c [1] Ident(x) \u2713\n   \u251c [2] : \u2713\n   \u2514 [3..5] missing\nfrontier: [0,3]"},{"token":"Int","tokens":["Int","Bool"],"display":"Expression\n \u2514 Abstraction\n   \u251c [0] \u03bb \u2713\n   \u251c [1] x \u2713\n   \u251c [2] : \u2713\n   \u251c [3] Type(Int) \u2713\n   \u2514 [4..5] missing\nfrontier: [0,4]"},{"token":".","tokens":["."],"display":"Expression\n \u2514 Abstraction\n   \u251c [0] \u03bb \u2713\n   \u251c [1] x \u2713  [2] : \u2713  [3] Int \u2713\n   \u251c [4] . \u2713\n   \u2514 [5] Expression  \u2190 frontier\nfrontier: [0,5]"}]}
<
