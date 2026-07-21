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

### Symbol

>D Symbol
A **symbol** is either:

- A **terminal** `Symbol::Terminal { regex, binding }`: matches a segment of input via a derivative regex. Literal strings `'x'` and `"x"` compile to `Regex::literal(x)`; `/pattern/` compiles to a full regex.
- A **non-terminal** `Symbol::Nonterminal { name, binding }`: a reference to another production rule by name.

Both variants carry an optional **binding name** used by the typing system to reference sub-trees.
<

### Production

>D Production
A **production** is a sequence of symbols $\alpha_0[b_0]\,\alpha_1[b_1]\cdots\alpha_n[b_n]$ where each $\alpha_k \in T \cup N$ and each $b_k \in \mathcal{B} \cup \{\varepsilon\}$ is an optional binding name. Productions also carry an optional rule name string used to look up a typing rule in $\Theta$.
<

>R Grammar Equality
Grammar equality (used for the meta-parser cache) compares `productions`, `special_tokens`, `delimiters`, and `start`. The `typing_rules` field is deliberately excluded because of indeterminate forms like the meta variables `?X` or the context calls.
<
