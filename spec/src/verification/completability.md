#[D] Completability

This chapter formalizes the completability properties that Aufbau guarantees, connecting the theoretical definitions from [Basic Concepts](../basic.md) to the algorithmic implementation.

For the treatment of completability over two-level languages, where the grammar alphabet $\Sigma$ is itself a regular language over a byte alphabet $\Sigma_\text{vocab}$, see [Completability and Regular Expressions](https://unsuspicious.org/blog/completing-regex/).

Source: [`src/validation/completability.rs`](~/src/validation/completability.rs)

## Typed Completability

Recall from [Basic Concepts](../basic.md) that a string $s$ is completable in $L$ if $\mathcal{C}_L(s) \neq \emptyset$. The typing engine (see [Type System](../concepts/typing_system.md)) refines this:

>D Typed Completability
A string $s$ is **typed-completable** in $(L, \Theta, \Gamma)$ if:

$$\mathcal{C}_{L,\Theta}(s) = \\{a \in \mathcal{C}_L(s) : \Gamma \vdash\_\Theta \mathcal{F}(sa) \neq \text{Invalid}\\} \neq \emptyset$$

That is, there exists at least one next token that is both syntactically valid and does not violate any typing constraint.
<

## Completion

>D Completion
$\text{complete}(G, s, d_{\max}, \Gamma)$ performs a depth-bounded [search](../completion/search.md) for a complete, well-typed expression extending input $s$:

$$\text{complete}(G, s, d_{\max}, \Gamma) = \begin{cases}
\text{Success}(s', T) & \text{if finds a valid completion } s' \text{ with tree } T \\\\
\text{Exhausted} & \text{if explores all states within } d_{\max} \text{ without success} \\\\
\text{Invalid} & \text{if } s \text{ cannot be parsed}
\end{cases}$$
<

## Prefix Soundness

>D Prefix Soundness
A string $s$ is **prefix-sound** in $(G, \Theta, \Gamma)$ if every prefix of $s$ is typed-completable:

$$\text{sound}(G, s, \Gamma) \iff \forall i \in [0, |s|] : \text{prefix\_ok}(G, s[0..i], \Gamma)$$

where whitespace-only prefixes (except the empty prefix) are excluded from checking.
<

>D Prefix OK
A prefix $s'$ satisfies $\text{prefix\_ok}(G, s', \Gamma)$ if either:

1. $\mathcal{C}_{L,\Theta}(s') \neq \emptyset$: there exist typed completions (the input is not a dead end), **or**
2. $\exists r \in \mathcal{F}(s') : \Gamma \vdash\_\Theta r \in \\{\text{Valid}(\tau), \text{Partial}(\tau)\\}$: some parse tree root is currently valid or indeterminate
<

Condition 1 alone is insufficient. Consider an input like `let x : int in`. The body expression has not started, so the typed completion set may depend on the body's grammar structure, which is complex to compute from scratch. But the partial tree itself is still consistent: the let-binding is well-formed so far, and the body is simply missing. Condition 2 catches these cases by checking the *current* tree status rather than demanding that forward extensions exist.

>L Prefix Soundness as a Safety Guarantee
If $\text{sound}(G, s, \Gamma)$ holds, then for any prefix $s'$ of $s$, the system is never in a state where no forward progress is possible. The user (or LLM) could have stopped producing tokens at any intermediate point, and the system would still have been able to find a valid completion.

This is the key usability property: **the system never traps the input**.
<

>T Sound Completion
$\text{sound\_complete}(G, s, \Gamma)$ holds if and only if:

1. $\text{sound}(G, s, \Gamma)$: all prefixes are OK
2. $\text{complete}(G, s, d_{\max}, \Gamma) = \text{Success}(s', T)$: the full input has a valid completion

Together, these ensure that the input $s$ can be completed into a valid expression *and* that the system was never stuck at any intermediate point during the production of $s$.
<

## Two-Level Completability

The grammar alphabet $\Sigma$ contains elements that are themselves languages over a finer alphabet. String literals match single strings, but regex patterns like `/[a-z][a-zA-Z0-9]*/` match sets of strings. This creates a **two-level language** structure:

$$L \subseteq \Sigma^* \quad \text{where} \quad \Sigma \subseteq \Sigma_\text{vocab}^*$$

The partial parser $\Phi_L$ operates at the $\Sigma$ level, but the actual input arrives at the $\Sigma_\text{vocab}$ level (bytes, characters, or model vocabulary tokens). A partial input may be in the *middle* of a token: the string `bi` is a prefix of the token `/b(u|o|i)p/` but is not itself a token.

The Brzozowski derivative $D_s(r)$ resolves this: given a regex $r$ and a partial string $s$, the derivative $D_s(r)$ is a new regex matching exactly the valid suffixes of $s$ in $L(r)$. If $D_s(r) \neq \emptyset$, then $s$ is a valid prefix of some string in $L(r)$.

This is formalized in detail in [Completability and Regular Expressions](https://unsuspicious.org/blog/completing-regex/). The implementation lives in the custom regex engine at [`src/regex/`](~/src/regex/).
