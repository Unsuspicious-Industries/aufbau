#[D] Runtime Binding

This page describes the runtime binding resolution pipeline: how static [grammar paths](concepts/binding.md) are matched against a live partial tree during type checking to produce concrete values for binding variables.

Source: [`src/logic/typing/binding.rs`](../src/logic/typing/binding.rs), [`src/logic/typing/eval.rs`](../src/logic/typing/eval.rs)

## Resolution

>D Binding Resolution
Given a non-terminal node $v$ with typing rule $\theta$, **binding resolution** matches the static grammar paths $\beta(b, \theta)$ against the partial tree rooted at $v$. For each binding $b$, the result is classified:

$$\text{resolve}(v, b) = \begin{cases}
\text{Full}(p) & \text{if path } p \text{ is valid and the target node is complete and non-extensible} \\\\
\text{Partial}(p) & \text{if path } p \text{ is valid but the target node is at the frontier or extensible} \\\\
\text{None} & \text{if no grammar path matches the current tree structure}
\end{cases}$$

where $p$ is a tree path (sequence of child indices).
<

The resolution iterates all grammar paths for each binding. A grammar path is **valid** if every step's alternative constraint matches the tree node's chosen alternative. A path is **invalid** (silently skipped) if any alternative constraint fails.

>D Path Validation
Validation of a grammar path against the tree proceeds recursively:

1. Decompose the path into head step $(i, a)$ and tail.
2. If the tree node's alternative index $\neq a$, return Invalid.
3. Look up child $i$:
   - If the child exists and is a non-terminal, recurse with the tail.
   - If the child exists and is a terminal, return Valid only if the tail is empty.
   - If the child does not exist (frontier), return Partial.
<

### Extensibility Classification

A resolved binding is classified as Partial (even if the path fully validates) when the target node lies on the **rightmost spine** of the tree and is extensible. This prevents early bindings (e.g., a function parameter name that is already complete) from flickering between Full and Partial as the user types subsequent tokens.

>D Rightmost Spine Constraint
A binding at tree path $p = [i_1, \ldots, i_k]$ is extensible only if for each $j$, child $i_j$ is the **last child** of its parent, and the leaf node is extensible (has an extension or remainder derivative).
<

>I resolve binding
{"label":"resolve: param in \u03bbx:Int.x","input":"","steps":[{"token":"lookup","tokens":["lookup"],"display":"rule abs needs binding: param\ngrammar paths: \u03b2(param, abs) = [1@0]"},{"token":"validate","tokens":["validate"],"display":"path 1@0:\n  step 1: child[1] of Abstraction\n  alt check: node alt = 0, path alt = 0 \u2713\n  target: Ident(x)"},{"token":"classify","tokens":["classify"],"display":"target Ident(x) is complete\nnot on rightmost spine (child 5 is last)\nresult: Full([1])"},{"token":"substitute","tokens":["substitute"],"display":"solve_binding(Atom(param))\npath [1] \u2192 text \"x\"\nresult: Raw(x)"}]}
<

## Binding Substitution

After resolution, binding variables in type expressions are replaced with concrete values via $\text{solve\_binding}$.

>D Binding Substitution
Given a type expression $\tau$ and resolved bindings $B$, substitution proceeds recursively:

- $\text{Atom}(x)$: look up $x$ in $B$.
  - If $x \in B_{\text{full}}$: read the text at tree path $B(x)$, parse it as a type via the type syntax parser. If strict parsing fails, fall back to partial parsing.
  - If $x \in B_{\text{partial}}$: return $\text{Path}(i_1@a_1 \cdots i_n@a_n)$ where $B(x) = i_1@a_1 \cdots i_n@a_n$, deferring resolution.
  - If $x \notin B$: error (unbound binding variable).
- $\text{Meta}(m)$: pass through (meta-variables are handled by unification, not binding).
- $\tau_1 \to \tau_2$: recurse into both sides.
- $\tau_1 \mid \tau_2 \mid \ldots$: recurse into all members.
- $\lnot \tau$: recurse into inner type.
- All other variants: return unchanged.
<

The fallback from strict to partial type parsing is essential for incremental type checking: a user typing `int ->` has a partial type annotation that cannot be strictly parsed but can be represented as $\text{Partial}(\text{Raw}(\text{int} \to), \text{"int ->"})$.

## Resolution Pipeline

The full binding flow from grammar load to type evaluation:

1. **Load time** ([grammar path construction](concepts/binding.md)): $\beta(b, \theta) = [\text{GrammarPath}]$ for each binding $b$ in each rule $\theta$. Paths are acyclic, depth-bounded at 16, and sorted by length.
2. **Type-check time** (this page): $\text{resolve}(v, b) \to \text{Full}(p) \mid \text{Partial}(p) \mid \text{None}$ by matching grammar paths against the live tree.
3. **Substitution**: $\text{solve\_binding}(\tau, B)$ replaces binding variables with parsed text or deferred path references.

>W Multiple Paths per Binding
When a binding has multiple valid grammar paths, the last valid path (in sorted order) overwrites earlier ones in the resolution map. This means shorter paths take priority (since `GrammarPath` ordering is length-first), but if two paths of equal length are both valid, the lexicographically later one wins. This is a potential source of non-determinism for grammars with ambiguous binding structure.
<
