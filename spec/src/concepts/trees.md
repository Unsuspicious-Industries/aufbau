#[D] Parse Forest

## Tokens and Children

>D Token
A **token** $\kappa = (\text{span}, \text{complete})$ is a terminal match over an input span $[\text{start}, \text{end}) \subset \Sigma^*$, where $\text{complete} \in \\{\top, \bot\\}$ indicates whether the matched prefix is a full token.
<

A **child** is either a node reference or a token: 
$$
\chi \in V \cup \mathcal{K}
$$


## Alternatives

>D Alternative
An **alternative** is
$$
a = (p, \chi^\star)
$$ where
 - $p \in P$ 
 - $\chi^\star \in (V \cup \mathcal{K})^*$.
<

## Nodes

>D Node
A **node** $v \in V$ is a tuple $(X, s, \tau, \Gamma_{\text{in}}, \Gamma_{\text{out}}, \beta, A)$ where:
- $X \in N$: the realized non-terminal.
- $s$: the covered input span.
- $\tau$: the synthesized **type**, extracted from an input string.
- $\Gamma_{\text{in}}, \Gamma_{\text{out}}$: input and output typing contexts.
- $\beta$: binding payload.
- $A$: a finite set of alternatives over $v$'s children.
<

>L Node Identity
Two nodes are identical iff they share the same non-terminal and input span:
$$v_1 = v_2 \iff X(v_1) = X(v_2) \land s(v_1) = s(v_2)$$
<

This means any two sub-parses deriving the same non-terminal over the same span share a single node.

## Forest

>D Partial Parse Forest
A **partial parse forest** for input $s \in \Sigma^*$ and grammar $G$ is a tuple $\mathcal{F} = (V, \text{root})$ where:
- $V$: a finite set of nodes, closed under child references.
- $\text{root} \in V$: a node with $X = S$ and span covering $s$.
<