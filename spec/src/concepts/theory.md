#[D] Theoretical Foundation

This chapter defines the formal structures underlying the Aufbau system: grammars, parse forests, partial trees, typing judgments, and the completability guarantee.

Sources: [`src/logic/grammar/mod.rs`](~/src/logic/grammar/mod.rs), [`src/logic/partial/structure.rs`](~/src/logic/partial/structure.rs), [`src/logic/typing/mod.rs`](~/src/logic/typing/mod.rs)

## Grammar

>D Grammar
A **grammar** $G$ is a tuple $(N, T, P, S, \Theta, A)$ where:

- $N$: finite set of non-terminals (strings).
- $T$: finite set of terminals, each a derivative regex over the input alphabet.
- $P$: set of productions. Each production is a pair $(\text{lhs} \in N,\ \text{rhs} \in (T \cup N)^*)$ with an optional typing rule name.
- $S \in N$: the distinguished start symbol.
- $\Theta$: a finite map from rule names to typing rules (may be empty).
- $A: N \to P^*$: the alternative map — for each non-terminal, the ordered list of its productions.
<

In the implementation, $N$ and $A$ are represented together as `productions: HashMap<String, Vec<Production>>`. The start symbol is `start: Option<String>`. Typing rules live in `typing_rules: HashMap<String, TypingRule>`.

### Symbol

>D Symbol
A **symbol** is either:

- A **terminal** `Symbol::Terminal { regex, binding }`: matches a segment of input via a derivative regex. Literal strings `'x'` and `"x"` compile to `Regex::literal(x)`; `/pattern/` compiles to a full regex.
- A **non-terminal** `Symbol::Nonterminal { name, binding }`: a reference to another production rule by name.

Both variants carry an optional **binding name** used by the typing system to reference sub-trees.
<

### Production

>D Production
A **production** is a sequence of symbols $\alpha_0[b_0]\,\alpha_1[b_1]\cdots\alpha_n[b_n]$ where each $\alpha_k \in T \cup N$ and each $b_k \in \mathcal{B} \cup \{\varepsilon\}$ is an optional binding name. Productions also carry an optional rule name string used to look up a `TypingRule` in $\Theta$.
<

Productions are represented as `struct Production { rule: Option<String>, rhs: Vec<Symbol> }`.

## Grammar Identity

Grammar equality (used for the meta-parser cache) compares `productions`, `special_tokens`, `delimiters`, and `start`. The `typing_rules` field is deliberately excluded: two grammars differing only in typing annotations produce identical parse forests.

## Parse Forest

>D Partial Parse Forest
For an input $s \in \Sigma^*$ and grammar $G$, the **partial parse forest** $\mathcal{F}(s)$ is the set of all rooted parse trees (both complete and incomplete) that are consistent with a prefix of $s$. Formally, a tree $t \in \mathcal{F}(s)$ is a labelled ordered tree where:

- Internal nodes are labelled by non-terminals.
- Leaves are labelled by terminal segments drawn from $s$.
- Each internal node's children match some production in $A(\text{label}(v))$.
- The yield of $t$ equals some prefix of the tokenisation of $s$.
<

Trees that consume all of $s$ are **complete**; trees that consume a strict prefix are **incomplete** (the partial frontier may be extended).

In the implementation, the forest is `PartialAST { roots: Vec<NonTerminal>, input: String }`. Each `NonTerminal` node carries its production index, consumed segment range, and a child list of `Node` values (either `NonTerminal` or `Terminal`).

### Completeness

>D Complete Tree
A tree $t$ is **complete** if every leaf is a matched terminal and the consumed span covers the full tokenised input. In code: `NonTerminal::is_complete()` returns true when all children are complete and the consumed length equals the segment count of the input.
<

>D Frontier
The **frontier** of an incomplete tree $t$ is the leftmost non-terminal node $v$ whose production has not yet been fully matched. The frontier is the unique point where new input can extend $t$. In code: `NonTerminal::frontier()` returns `Some(index)` for the frontier child, or `None` if the tree is complete.
<

## Partial Parser

>D Partial Parser
A **partial parser** for grammar $G$ is a function:
$$\Psi_G : \Sigma^* \to \mathcal{P}(\text{Tree}(G))$$
mapping each input to a partial parse forest. The parser is *sound* in the sense that every tree in $\Psi_G(s)$ is a consistent partial derivation from $S$, and *complete* modulo recursion depth: every derivation reachable within the configured depth bound appears in the output.
<

## Type System

>D Type
A **type** $\tau$ in the Aufbau system is drawn from the following grammar:

| Variant | Notation | Meaning |
|---|---|---|
| `Atom(x)` | $x$ | Named type variable |
| `Meta(x)` | $?x$ | Inference meta-variable |
| `Raw(s)` | `'s'` | Concrete literal type |
| `Arrow(a, b)` | $\tau_1 \to \tau_2$ | Function type |
| `Union(ts)` | $\tau_1 \mid \tau_2 \mid \cdots$ | Union type |
| `Not(t)` | $\neg\tau$ | Negation type |
| `Any` | $\top$ | Universal type |
| `None` | $\bot$ | Empty type |
| `Partial(t, s)` | $\tilde\tau$ | Indeterminate: type-so-far with continuation |
| `Path(p)` | $\text{Path}(p)$ | Binding location reference |
| `PathOf(t, p)` | $\text{PathOf}(\tau, p)$ | Type at a binding location |
<

The `Partial`, `Path`, and `PathOf` variants are **indeterminacy markers**: they arise when a typing judgment cannot be resolved because the parse tree is incomplete. Their presence in any intermediate result forces the final status to `Indeterminate`.

## Typing Judgment

>D Typing Judgment
The **typing judgment** $\Gamma \vdash\_\Theta t : \mathcal{S}$ reads "under context $\Gamma$ and typing rules $\Theta$, tree $t$ has status $\mathcal{S}$." The status $\mathcal{S}$ is drawn from the **tree status lattice**:

| Status | Meaning |
|---|---|
| $\text{Valid}(\tau)$ | Tree is complete and has type $\tau$ |
| $\text{Partial}(\tau)$ | Tree is incomplete but consistent with type $\tau$ |
| $\text{Invalid}$ | Tree cannot be typed under any extension |
| $\text{TooDeep}$ | Depth limit reached; result is conservative $\text{Partial}(\top)$ |
<

The lattice ordering is $\text{Invalid} \sqsubset \text{Partial}(\tau) \sqsubset \text{Valid}(\tau)$, with $\text{Invalid}$ as bottom. Typing is monotone in input length: if $\Gamma \vdash\_\Theta \mathcal{F}(s) : \text{Invalid}$, then no extension of $s$ can produce a valid tree.

## Completability

>D Completability
A string $s$ is **completable** in $(G, \Theta, \Gamma)$ if there exists an extension $s' \in \Sigma^+$ such that $\Psi_G(s \cdot s')$ contains a tree $t$ with $\Gamma \vdash\_\Theta t : \text{Valid}(\tau)$ for some $\tau$.

The **typed completability set** is:
$$\mathcal{C}\_{L,\Theta}(s) = \\{a \in \Sigma : s \cdot a \text{ is completable in } (G, \Theta, \Gamma)\\}$$
<

>L Soundness of Completions
Every token $a$ returned by the completion pipeline satisfies: parsing $s \cdot a$ with $\Psi_G$ yields at least one tree $t$ such that $\Gamma \vdash\_\Theta t \notin \\{\text{Invalid}\\}$.
<

This is the central correctness invariant of the system. The pipeline never returns a token that would immediately invalidate the expression, but it does not guarantee that a full valid completion exists — only that the expression remains on a non-invalid trajectory.

## Binding Paths

>D Binding Path
For a grammar $G$ and a binding name $b$ in production $p$, the **binding path** $\beta(b, p)$ is a finite sequence of steps $(i_0, a_0)\,(i_1, a_1)\cdots(i_k, a_k)$ where each $(i_j, a_j)$ identifies child index $i_j$ and production alternative $a_j$ at depth $j$. The path describes how to navigate from the root of a tree derived by $p$ to the sub-tree bound to $b$.
<

Binding paths are computed at grammar load time (`rebuild_bindings`) by a DFS over the production graph, truncated at `MAX_RECURSION_DEPTH = 16`. Paths beyond this depth are omitted: $\beta$ is an under-approximation for deeply recursive grammars, and bindings reachable only via long paths may fail to resolve at type-check time without an error.
