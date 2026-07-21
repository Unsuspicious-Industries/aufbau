#[D] Type System

This chapter defines the type language, the typed tree and forest structures,
the typing judgment, and the subtyping relation. Types in Aufbau are not
information carriers in the classical sense: they are **constraints on
completability**. A type describes what extensions of a partial expression
*may become*.

## The Typing Judgment

>D Typing Judgment
The **typing judgment**

$$\Gamma \vdash_\Theta t : \mathcal{S}$$

reads "under context $\Gamma$ and typing rules $\Theta$, tree $t$ has status
$\mathcal{S}$." When no external context is needed we write $\vdash_\Theta t :
\mathcal{S}$ with an empty context.
<

## Tree Status

>D Tree Status
The **tree status** lattice $\mathcal{S}$ classifies the result of typing a tree:

$$\text{Valid}(\tau) \sqsupset \text{Partial}(\tau) \sqsupset \text{Invalid}$$

- $\text{Valid}(\tau)$: the tree is complete and fully satisfies the constraint $\tau$.
- $\text{Partial}(\tau)$: the constraint $\tau$ is consistent with the tree so far,
  but the tree is incomplete; the status may strengthen or fail as more input arrives.
- $\text{Invalid}$: the constraint is violated; the tree cannot be completed
  into any well-typed expression.
<

This lattice is **monotone**: status can only move from $\text{Partial}$ toward
$\text{Valid}$ or $\text{Invalid}$, never in reverse.

>D Forest Status
The **status of a partial forest** $\mathcal{F}$ under $\vdash_\Theta$ is the supremum of the statuses of its trees under the lattice $\mathcal{S}$:

$$\Gamma \vdash_\Theta \mathcal{F} \\;:=\\; \bigsqcup_{t \\,\in\\, \mathcal{F}} \\;\Gamma \vdash_\Theta t$$

Concretely:

$$\Gamma \vdash_\Theta \mathcal{F} = \begin{cases} \text{Valid}(\tau) & \text{if some tree has status } \text{Valid}(\tau) \\\\
 \text{Partial}(\tau) & \text{if no tree is valid but some is } \text{Partial}(\tau) \\\\
  \text{Invalid} & \text{if every tree is } \text{Invalid} \end{cases}$$
<

The forest is $\text{Invalid}$ only when *all* of its trees have been rejected.

>L Status Monotonicity
For partial parses $\mathcal{F}(s)$ extending to $\mathcal{F}(s \cdot a)$:
$$\Gamma \vdash_\Theta \mathcal{F}(s) = \text{Invalid}
\implies
\Gamma \vdash_\Theta \mathcal{F}(s \cdot a) = \text{Invalid}
\quad \forall a$$
<

Monotonicity makes typed search efficient: once a state is $\text{Invalid}$,
the entire subtree below it in the search space can be pruned without
examination.



## Typed Trees and Forests

The typing engine $\vdash_\Theta$ maps a tree to either a status or rejection.
This lifts trees and forests into *typed* counterparts.

>D Typed Tree
A **typed tree** $\hat{t}$ is a pair $(t,\\, \mathcal{T})$ where:
- $t$ is a partial tree, and
- $\mathcal{T} : V \to \tau \cup \{\varepsilon\}$ maps each node to its
  inferred type, or to $\varepsilon$ if not typable

$\hat{t}$ exists whenever $\vdash_\Theta t \neq \text{Invalid}$, i.e. the
tree carries status $\text{Partial}(\tau)$ or $\text{Valid}(\tau)$.
<

The typing engine thus acts as a constructor:

$$
\vdash_\Theta t =
\begin{cases}
\hat{t} & \text{if } \vdash_\Theta t : \text{Valid}(\tau)
           \text{ or } \text{Partial}(\tau) \text{ for some } \tau \\\\
\text{Invalid} & \text{if the typing constraint is violated}
\end{cases}
$$

>D Typed Forest
A **typed forest** $\hat{\mathcal{F}}$ is a forest in which every tree is
either a typed tree $\hat{t}$ or has been individually rejected as
$\text{Invalid}$.
<


Extending the engine to forests:

$$
\vdash_\Theta \mathcal{F} =
\begin{cases}
\hat{\mathcal{F}} & \text{if at least one tree in } \mathcal{F}
                    \text{ produces a typed tree} \\\\
\text{Invalid} & \text{if every tree in } \mathcal{F} \text{ is } \text{Invalid}
\end{cases}
$$

A partial tree that is **typedly invalid** carries an inconsistency in its
typing rules. It is *syntactically valid* but *semantically invalid*


## Types as Constraints on Completability

Without typing, $\mathcal{C}_L(s)$ is the set of all syntactically valid
extensions of a prefix $s$. The typed tree structure gives this a sharper
interpretation: every type $\tau$ assigned to a node in $\hat{t}$ is a
**constraint** asserting that any syntactic completion of that subtree must
produce a value of type $\tau$, or the tree transitions to $\text{Invalid}$.

Types therefore define a refinement of the completability set:

$$
\mathcal{C}_{L,\Theta}(s) = \\{\\, a \in \mathcal{C}_L(s) \mid \Gamma \vdash\_\Theta \mathcal{F}(sa) \neq \text{Invalid} \\,\\}
$$

>L Typed Completability Refinement
$$\mathcal{C}_{L,\Theta}(s) \subseteq \mathcal{C}_L(s)$$
<

Typing can only **narrow** the completability set, never widen it. This
refinement is computed incrementally:

1. The parser produces a partial forest $\mathcal{F}(s)$.
2. $\vdash_\Theta$ evaluates each tree, yielding $\hat{\mathcal{F}}$ or
   $\text{Invalid}$ per tree.
3. The synthesizer computes the grammar-level completion set $\mathcal{C}_L(s)$.
4. Each candidate token $a$ is tested by extension: parse $s \cdot a$, apply
   $\vdash_\Theta$, and retain only those where some root remains
   non-$\text{Invalid}$.

The type system acts as a **filter**, not a generator. It cannot propose
completions the grammar does not support; it only rejects those that violate
typing constraints.


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
| Indeterminate | $\widetilde{\tau}$ | Internal: type inferred from an incomplete parse |

$\text{Path}(\cdots)$, $\text{PathOf}(\cdots)$, and $\widetilde{\tau}$ are markers
of **indeterminacy**: the engine cannot yet produce a definitive type because
the parse tree is incomplete. Their presence in a judgment forces the result
to $\text{Partial}(\tau)$, propagating incompleteness upward without
rejecting the tree.
<


