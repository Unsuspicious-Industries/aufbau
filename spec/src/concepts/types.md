#[D] Type System

This chapter defines the type language, typing rules, and the typing judgment.
Types in Aufbau are not information carriers in the classical sense: they are
**constraints on completability**. A type describes what extensions of a partial
expression *may become*.

## Type Language

A **type** $\tau$ is defined inductively:

| Form | Notation | Description |
| :--- | :--- | :--- |
| Meta | $A$, $?A$ | Type variable (binding reference or unification unknown) |
| Raw | $\text{'t'}$ | Concrete type literal |
| Arrow | $\tau_1 \to \tau_2$ | Function type |
| Union | $\tau_1 \mid \tau_2$ | Union type |
| Not | $\lnot\tau$ | Negation type |
| ContextCall | $\Gamma(x)$ | Deferred context lookup |
| Any | $\top$ | Top type (universal supertype) |
| None | $\emptyset$ | Bottom type (empty type) |
| Indeterminate | $\widetilde{\tau}$ | Type inferred from an incomplete parse |

>R Undeterminate form
$\widetilde{\tau}$ is a marker of **indeterminacy**: the engine cannot yet
produce a definitive type because the parse tree is incomplete. Its presence
in a judgment forces the result to $\text{Partial}(\tau)$, propagating
incompleteness upward without rejecting the tree.
<

## Typing Rules

>D Type Expression
A **type expression** $\hat{\tau}$ is a type $\tau$ that may contain
**meta-variables** $A, B, \ldots \in \mathcal{M}$. In the grammar file syntax,
the `?` prefix (e.g., `?A`) is a convention to signal intent as a unification
unknown, but it has no semantic effect: all meta-variables are resolved
uniformly from the substitution map and obligation values. A meta-variable may
name either a unification unknown or a grammar binding — the obligation
mechanism handles both cases identically.
<

>D Premise
A **premise** $\phi$ is one of:


| Form | Notation | Meaning |
| :--- | :--- | :--- |
| Judgment | $\Gamma \vdash b : \hat{\tau}$ | child $b$ has type $\hat{\tau}$ under $\Gamma$ |
| Membership | $x \in \Gamma$ | name $x$ is bound in context |
| Extension | $\Gamma[x{:}\hat{\tau}] \vdash b : \hat{\tau}'$ | judge $b$ under $\Gamma$ extended with $x : \hat{\tau}$ |
| Threading | $\Gamma \triangleright \bar{b}$ | sequentially thread context through statement list $\bar{b}$ |
<

>D Typing Rule
A **typing rule** $\gamma \in \Theta$ is a tuple $(\text{name}, \mathcal{M}, \mathcal{B}, \bar{\phi}, \hat{\tau}_c)$ where:
- $\text{name} \in N$: the nonterminal this rule is attached to.
- $\mathcal{M}$: a finite set of meta-variables $\{?A, ?B, \ldots\}$.
- $\mathcal{B}$: a finite set of bound names that exists in the grammar
- $\bar{\phi} = \phi_1, \ldots, \phi_n$: an ordered list of premises.
- $\hat{\tau}_c$: the conclusion type expression, written below the line.
<

For more clarity we can express it in natural-deduction style:

$$\frac{\phi_1 \quad \cdots \quad \phi_n}{\hat{\tau}_c} \\; (\text{name})$$


Unification proceeds left-to-right across $\bar{\phi}$: a meta-variable $?A$
bound in an earlier premise may appear in later premises and in $\hat{\tau}_c$.

>E Lambda rule (from `stlc.auf`)
The `lambda` production `'λ' Identifier[a] ':' Type[τ] '.' Expression[e]` has the rule:

$$\frac{\Gamma[a{:}\tau] \vdash e : {?B}}{\tau \to {?B}} \\; (\text{lambda})$$

Here $\mathcal{M} = \{?B\}$, $\mathcal{B} = \{a, \tau, e\}$, the single premise extends the context with the annotated binder, and the conclusion is the arrow type.
<

>E Application rule (from `stlc.auf`)
$$\frac{\Gamma \vdash l : {?A} \to {?B} \quad \Gamma \vdash r : {?A}}{{?B}} \\; (\text{app})$$

$?A$ is unified between the two premises: whatever function type $l$ resolves to determines the required type of $r$.
<

## The Typing Judgment

>D Typing Judgment
The **typing judgment**
$$\Gamma \vdash_\Theta v : \mathcal{S}$$
reads "under context $\Gamma$ and typing rules $\Theta$, node $v$ has status
$\mathcal{S}$." Status $\mathcal{S}$ is one of:

- $\text{Valid}(\tau)$: the node is complete and has type $\tau$.
- $\text{Partial}(\tau)$: the node is incomplete but consistent with type $\tau$.
<
When no external context is needed we write $\vdash_\Theta v : \mathcal{S}$ with an empty context.