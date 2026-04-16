#[D] Binding Resolution

Given grammar $G = (N, T, P, S, \Theta, A)$, **binding resolution** statically
maps each binding name mentioned in a typing rule to the set of grammar paths
that lead to the bound node(s), relative to the named production that owns the rule.

## Grammar Paths

>D Grammar Step
A **grammar step** is a pair $(i, a) \in \mathbb{N} \times (\mathbb{N} \cup \{\bot\})$,
written $i@a$, meaning: descend to the $i$-th child of the current node,
optionally asserting that the current node chose alternative $a$. When $a = \bot$
no alternative constraint is imposed.
<

>D Grammar Path
A **grammar path** $\delta \in \Delta$ is a finite sequence of grammar steps:
$$\delta = i_1@a_1 \cdot i_2@a_2 \cdots i_n@a_n$$
The empty path $\varepsilon$ refers to the node itself.
<


For recursive grammars, finite paths are insufficient: a binding may be reachable
through an unbounded number of transparent intermediate non-terminals.
This is wehy we have a generalisation to **regular grammar paths** 
$$
\hat{\delta} \in \hat{\Delta} =
\text{Reg}(\mathbb{N} \times (\mathbb{N} \cup \{\bot\}))
$$ 
Here, the Kleene star captures repeated descent, is the intended long-term representation. The current
implementation approximates this by truncating recursive paths at depth
$D_{\max}$ and is planned for generalisation.


## Binding Map

>D Rule Name
A **rule name** $\rho \in \mathcal{R}$ is the identifier attached to a named
production $p \in P$. Only named productions carry typing rules; unnamed
productions are transparent to binding resolution.
<

>D Binding Map
The **binding map**
$$\beta : \mathcal{B} \times \mathcal{R} \to \mathcal{P}(\Delta)$$
associates each pair of a binding name $b \in \mathcal{B}$ and a rule name
$\rho \in \mathcal{R}$ with a **set of grammar paths** leading from $\rho$'s
production to nodes tagged with $b$.
<

Each $b$ may have multiple paths because the same binding name can be reachable
via structurally distinct routes through the grammar.

## Construction

The binding map is constructed statically at grammar load time. For each named
production $p$ with rule name $\rho$, a DFS collects all paths from $p$ to
nodes tagged with any binding $b$:

$
\begin{aligned}
&\textbf{Algorithm } \mathrm{ConstructBindingMap}(G)\\\\
&\textbf{Input: } G=(N,T,P,S,\Theta,A)\\\\
&\textbf{Output: } \beta : \mathcal{B}\times \mathcal{R} \to \mathcal{P}(\Delta) \\\\[4pt]
&\textbf{for each named production } p \text{ with rule } \rho:\\\\
&\quad \delta \leftarrow \varepsilon,\quad V \leftarrow \varnothing\\\\
&\quad \mathrm{Collect}(G,\\, p,\\, \rho,\\, \delta,\\, V,\\, \beta)\\\\[6pt]
&\textbf{proc } \mathrm{Collect}(G, p, \rho, \delta, V, \beta):\\\\
&\quad \textbf{if } (p, \mathrm{alt}(p)) \in V \textbf{ or } |\delta| \geq D_{\max} \textbf{ then return}\\\\
&\quad V \leftarrow V \cup \{(p, \mathrm{alt}(p))\}\\\\
&\quad \textbf{for each } (i, s) \in \mathrm{rhs}(p):\\\\
&\quad\quad \textbf{if } s \text{ has binding } b:\\\\
&\quad\quad\quad \beta(b,\, \rho) \mathrel{+}= \delta \cdot (i @ \mathrm{alt}(p))\\\\
&\quad\quad \textbf{if } s \in N \textbf{ and } s \text{ has no rule}:\\\\
&\quad\quad\quad \textbf{for each } p' \in A(s):\\\\
&\quad\quad\quad\quad \mathrm{Collect}(G,\, p',\, \rho,\, \delta \cdot (i @ \mathrm{alt}(p)),\, V,\, \beta)\\\\
&\quad V \leftarrow V \setminus \{(p, \mathrm{alt}(p))\}
\end{aligned}
$

Where:
- $\mathrm{alt}(p)$: the alternative index of $p$ within its non-terminal's production list.
- $\mathrm{rhs}(p)$: the ordered sequence of $(i, s)$ pairs of $p$'s right-hand side.
- $D_{\max}$: maximum recursion depth, bounding path length for cyclic grammars.
- **Rule boundary**: a child non-terminal $s$ whose productions carry their own rule names is **not** traversed. Its binding resolution is handled by its own rule, not the current one.

## Binding Resolution

At runtime, for a node $v$ with rule name $\rho$ and a binding $b \in
\mathcal{B}$, the **resolved nodes** $V_b(v)$ are all descendants of $v$ whose
tree path matches some $\delta \in \beta(b, \rho)$:

$$V_b(v) = \{\, v' \in V \mid \exists\, \delta \in \beta(b,\rho),\; \mathrm{path}(v, v') \in \mathcal{L}(\delta) \,\}$$

where $\mathcal{L}(\delta)$ is the set of concrete tree paths matching $\delta$,
respecting any alternative assertions at each step.

## Properties

>T Binding Invariance
For any $b \in \mathcal{B}$ and $\rho \in \mathcal{R}$, $\beta(b, \rho)$ is
uniquely determined by the grammar structure and independent of input.
<

>A Binding Uniqueness
If $|\beta(b, \rho)| > 1$, then $b$ must be declared as tuple-typed in $\Theta(\rho)$.
<