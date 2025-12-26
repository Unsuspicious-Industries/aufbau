# Theoretical Foundation

This chapter lays out the formal foundations for our grammar-based parsing system with type annotations and binding resolution.

## Core Definitions

>D Grammar
A **grammar** $G$ is a tuple $(N,T,P,S,\Theta,A)$ where:

- $N$ is the set of non-terminals
- $T$ is the set of terminals
- $P$ is the set of productions with binding annotations
- $S \in N$ is the start symbol
- $\Theta: P \to \theta \cup \{\varepsilon\}$ assigns an optional typing rule to each production
- $A: N \to P^*$ maps each non-terminal to its production alternatives
<

>R Production
A **production** $p \in P$ is a sequence of length $n$:
$$ \alpha_0[b_0] \alpha_1[b_1]\cdots \alpha_n[b_n] $$

where:
- $\forall k \in [0;n], \alpha_k \in T \cup N$ (each element is a terminal or non-terminal)
- $\forall k \in [0;n], b_k \in \mathcal{B} \cup \{\varepsilon\}$ (each element may have a binding)
- $\mathcal{B}$ is the set of bindings and $\varepsilon$ denotes the empty binding
<

>D Non-terminal Alternatives
Each non-terminal $n \in N$ is associated with a set of production alternatives via function $A$. Using BNF notation:
$$ n ::= p_1 \ |\ p_2 \ |\ \cdots \ |\ p_m $$
<

## Goals

With these definitions in place, our goals in the project are to be able to procedurally build a parser $\Psi_L$ for any language $L$ defined by a grammar $G$.

>D Partial Parser
With $\Sigma$ an alphabet and $L$ a language a **partial parser** for $L$ is a function
$$
\Phi_L : \Sigma^* \rightarrow \\{\text{accept},\text{reject}\\}
$$
defined as:
$$
\Phi_L(s) = \begin{cases}
\text{accept} &  \mathcal{C}_L(s) \neq \emptyset \\\\
\text{reject} & \text{otherwise}
\end{cases}
$$
<

Finally, we want to be able to resolve **bindings** in the parse trees produced by $\Psi_L$, and use the **typing rules** $\Theta$ to enforce type correctness during parsing.

>R Completability via Partial Parsing
When $s$ is completable:
- $\Psi_L(s) \neq \text{reject}$
- $\mathcal{F} = \Psi_L(s)$ is **not complete**
- $\mathcal{F}$ becomes complete by extension with some $s' \in S'$
<
