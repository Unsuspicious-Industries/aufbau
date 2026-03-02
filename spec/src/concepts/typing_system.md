#[D] Type System

This chapter defines the type language, judgment semantics, and the subtyping relation. Crucially, types in Aufbau are not information carriers in the classical sense: they are **constraints on completability**. A type describes what extensions of a partial expression *may become*.

## Types as Constraints

Usually we envision types as descriptors on values. In Aufbau, the role of types is different. Since we operate on partial trees (expressions still being written), a type is a **constraint on the completability set** $\mathcal{C}_L(s)$.

When the typing engine assigns type $\tau$ to a partial subtree, it asserts: *any completion of this subtree must produce a value of type $\tau$, or the expression will be rejected*. This judgment resolves to:
- $\text{Invalid}$: the constraint is violated (not completable).
- $\text{Partial}(\tau)$: the constraint $\tau$ is consistent so far, but may strengthen or fail as more input arrives.
- $\text{Valid}(\tau)$: the constraint is fully satisfied.

Types are a **refinement** of the completability set. Without typing, $\mathcal{C}_L(s)$ is the set of all syntactically valid extensions. With typing, it becomes:

$$\mathcal{C}_{L,\Theta}(s) = \\{a \in \mathcal{C}_L(s) \ |\ \Gamma \vdash\_\Theta \mathcal{F}(sa) \neq \text{Invalid}\\}$$

where $\vdash\_\Theta$ is the [typing judgment](#the-typing-judgment) defined below. Typing can only narrow the completability set, never widen it.

>L Typed Completability Refinement
$$\mathcal{C}_{L,\Theta}(s) \subseteq \mathcal{C}_L(s)$$
<

## The Typing Judgment

>D Typing Judgment
The **typing judgment** $\Gamma \vdash\_\Theta t : \mathcal{S}$ reads "under context $\Gamma$ and typing rules $\Theta$, tree $t$ has status $\mathcal{S}$," where:

$$
\mathcal{S} \in \{\text{Valid}(\tau),\ \text{Partial}(\tau),\ \text{Invalid}\}
$$

is the [tree status lattice](#tree-status). When no external context is needed, we write $\vdash\_\Theta t : \mathcal{S}$ with an empty context.
<

Source: [`src/logic/typing/eval.rs`](../../src/logic/typing/eval.rs) (`check_tree`, `check_tree_with_context`)


## Type Language

>D Type
A **type** $\tau$ is defined inductively:

| Form | Notation | Description |
| :--- | :--- | :--- |
| Atom | $\tau$ | Named type from binding resolution |
| Meta | $?A$ | Unification variable (unknown type) |
| Raw | $\text{'int'}$ | Concrete type literal |
| Arrow | $\tau_1 \to \tau_2$ | Function type |
| Union | $\tau_1 \mid \tau_2$ | Union type |
| Not | $\lnot\tau$ | Negation type |
| ContextCall | $\Gamma(x)$ | Deferred context lookup |
| Any | $\top$ | Top type (universal supertype) |
| None | $\emptyset$ | Bottom type (empty type) |
| Path | $\text{Path}(i_1@a_1 \cdot i_2@a_2 \cdots i_n@a_n)$ | Internal: unresolved binding at grammar path |
| PathOf | $\text{PathOf}(\tau,\; i_1@a_1 \cdots i_n@a_n)$ | Internal: type $\tau$ at unresolved grammar path |
| Partial | $\widetilde\tau$ | Internal: type inferred from incomplete parse |
<

$\text{Path}(i_1@a_1 \cdots i_n@a_n)$, $\text{PathOf}(\tau, i_1@a_1 \cdots i_n@a_n)$, and $\widetilde\tau$ are markers of **indeterminacy**: places where $\vdash\_\Theta$ cannot yet produce a definitive answer because the parse tree is incomplete. Their presence in a judgment forces the result to $\text{Indeterminate}$, which is how the three-valued logic propagates.

Source: [`src/logic/typing/mod.rs`](../../src/logic/typing/mod.rs), [`src/logic/typing/syntax.rs`](../../src/logic/typing/syntax.rs)

## Tree Status

>D Tree Status
The **tree status** lattice $\mathcal{S}$ classifies the result of $\vdash\_\Theta$:

$$\text{Valid}(\tau) \sqsupset \text{Partial}(\tau) \sqsupset \text{Invalid}$$
<

This lattice defines a **monotonicity** property: as more input arrives, status can move from $\text{Partial}$ to either $\text{Valid}$ or $\text{Invalid}$, but never from $\text{Invalid}$ back to $\text{Partial}$ or $\text{Valid}$.

>L Status Monotonicity
For partial parses $\mathcal{F}(s)$ extending to $\mathcal{F}(s \cdot t)$: if $\Gamma \vdash\_\Theta \mathcal{F}(s) : \text{Invalid}$, then $\Gamma \vdash\_\Theta \mathcal{F}(s \cdot t) : \text{Invalid}$ for all $t$.
<

Monotonicity makes typed search efficient. When a state is $\text{Invalid}$, the entire subtree below it in the search space is pruned.

## Three-Valued Judgments

>D Unification
**Unification** of types $\tau_1$ and $\tau_2$ yields a three-valued result:

$$\text{unify}(\tau_1, \tau_2) \in \\{\text{Ok}(\sigma), \text{Indeterminate}, \text{Fail}\\}$$
<

$\text{Indeterminate}$ means the constraint *might* hold once more information is available. The engine treats this as $\text{Partial}$: the constraint is deferred, not dropped. This is essential for incremental checking; we cannot reject a tree just because a binding has not been typed yet.

## Subtyping

>D Subtype Relation
The **subtype relation** $\subseteq$ on types:

- $\emptyset \subseteq \tau$ for all $\tau$ (bottom is subtype of everything)
- $\tau \subseteq \top$ for all $\tau$ (everything is subtype of top)
- $\tau \subseteq \tau$ (reflexivity)
- $\tau_1 \to \tau_2 \subseteq \tau_1' \to \tau_2'$ iff $\tau_1' \subseteq \tau_1$ and $\tau_2 \subseteq \tau_2'$ (contravariant domain, covariant range)
- $\tau_1 \mid \tau_2 \subseteq \tau$ iff $\tau_1 \subseteq \tau$ and $\tau_2 \subseteq \tau$ (union distributes)
<

Subtyping is also three-valued. If either side contains $\top$, $\text{Path}(\cdots)$, $\text{PathOf}(\cdots)$, or $\Gamma(x)$, the result is $\text{Indeterminate}$ (except $\tau \subseteq \top$, which is always true).

Source: [`src/logic/typing/ops.rs`](../../src/logic/typing/ops.rs)

## Constraint Refinement

The completability set gives syntactically valid next tokens. Typing refines this to $\mathcal{C}_{L,\Theta}(s)$ incrementally:
1. The parser produces a partial forest $\mathcal{F}(s)$.
2. $\vdash\_\Theta$ evaluates each tree, pruning those that are $\text{Invalid}$.
3. The synthesizer computes the grammar-level completion set.
4. Each candidate token is tested by extension: parse $s \cdot t$, apply $\vdash\_\Theta$, keep only those where some root remains non-$\text{Invalid}$.

This is a **generate-and-try** refinement. The type system acts as a filter, not a generator. It cannot propose completions the grammar does not support; it only rejects those violating typing constraints.

Additionally, `Context::shadow` is planned to support rebinding names in contexts for languages with let-bindings.
