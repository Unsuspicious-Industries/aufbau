#[D] Theoretical Foundation

This chapter defines the formal structures underlying the Aufbau system: grammars, parse forests, partial trees, typing judgments, and the completability guarantee.

## Grammar

>D Grammar
A **grammar** $G$ is a tuple $(N, T, P, \mathcal{B}, S, \Theta, A)$ where:

- $N$: finite set of non-terminals.
- $T: \mathcal{R}$ the finite set of terminals expressed in Regex form.
- $P$ the set of productions
- $\mathcal{B}$ the set of bindings 
- $S \in N$ the distinguished start symbol.
- $\Theta$ the set of typing rules
- $A: N \to P^*$ defining for each non-terminal the ordered list of its productions.
<

The typing rule associated to nonterminal $n$ is expressed as $\Theta(n)$.

### Production

>D Production
A **production** is a sequence of symbols 
$$
p = \alpha_0[b_0]\,\alpha_1[b_1]\cdots\alpha_n[b_n]
$$ 
where each $\alpha_k \in T \cup N$ and each $b_k \in \mathcal{B} \cup \{\varepsilon\}$ is an optional binding name.
<



