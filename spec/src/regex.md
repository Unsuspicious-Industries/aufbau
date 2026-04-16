#[D] Regular Expressions

Regular expressoins (RegEx) are tools used to define **regular languages**. In this system, we are going to use them as a parsing cool for subcomponents of the context-dependent languages we ought to study. In the codebase, we integrate a custom engine for matching and analyzing such expressions ([`src/regex/`](~/src/regex/)). The set of regular expressions will be denoted as $\mathcal{R}$.

Our parser uses **Brzozowski derivatives** to match incrementally, a design that integrates naturally with partial parsing. The theoretical foundations (derivatives, two-level completability) are developed in [Completability and Regular Expressions](https://unsuspicious.org/blog/completing-regex/). This page covers only the implementation-specific choices and basic notions
## Regex definitions

>D Regex
$$r ::= \emptyset \mid \varepsilon \mid c \mid [a\text{-}b] \mid r_1 \cdot r_2 \mid r_1 \mid r_2 \mid r^*$$
<

As covered in the blog post mentionned above, the **regex derivative** of a given input is a regular epression defining the full *completability set* of an input in this language. We are not going to redefine it, but use standard notation $v^{-1}$.

For a generic regular language $L$ over an alphabet $\Sigma^*$ : 

$$\forall v \in \Sigma^*, \begin{cases}
v \notin L \quad \text{if } v^{-1} = \emptyset \\\\
v~v{^1} \in L \quad \text{else } 
\end{cases}$$


## Nullability and completion

An string $v\in  \Sigma^*$ is said to be nullable $\nu(v) = \text{ True}$ if its derivatives contains the empty input $\varepsilon$.
>L Nullable
$$
\forall v \in \Sigma^$, \nu(v) \iff v \in L
$$
<
