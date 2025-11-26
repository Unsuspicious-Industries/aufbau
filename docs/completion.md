# Completion

Valid continuations for partial parses.

## FIRST Set

$$\text{FIRST}(\alpha) = \begin{cases}
\{\alpha\} & \alpha \in T \\
\bigcup_{p \in A(\alpha)} \text{FIRST}(p_0) & \alpha \in N, p \neq \varepsilon \\
\end{cases}$$

## Frontier Completions

For partial node $v$ with production $\pi(v) = \alpha_0 \cdots \alpha_n$ and $k$ filled children:

$$\text{COMPLETIONS}(v) = \begin{cases}
\text{extensions}(v[k-1]) & \text{complete}(v) \\
\{\text{remainder}\} & v[k-1] = \text{Partial}(\_,\text{remainder}) \\
\text{COMPLETIONS}(v[k-1]) & \neg\text{complete}(v[k-1]) \land v[k-1] \in N \\
\text{FIRST}(\alpha_k) & \text{otherwise}
\end{cases}$$

## Forest Completions

$$\text{COMPLETIONS}(\mathcal{F}) = \bigcup_{t \in \mathcal{F}} \text{COMPLETIONS}(\text{root}(t))$$

## Type Filtering

Given context $V_{ctx}$ at frontier and constraint flag $c$:

$$C_{filtered} = \begin{cases}
\{v \in V_{ctx} \mid \exists t \in C: t \models v\} & c = \text{true} \\
C \cup \{v \in V_{ctx} \mid \exists t \in C: t \models v\} & c = \text{false}
\end{cases}$$

Context collection traverses to frontier, accumulating bindings from premise extensions $\Gamma[x:\tau]$ and marking membership constraints $x \in \Gamma$.
