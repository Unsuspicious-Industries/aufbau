# Binding Resolution

Given our grammar $G = (N, T, P, S, \Theta, A)$ with binding annotations, we construct a **binding map** $\beta: \mathcal{B} \times P \to \mathcal{P}^*$ that associates each binding identifier with a set of canonical **grammar paths** (not tree paths). These grammar paths include alternative annotations that serve as verification constraints during runtime binding resolution.

###[D] Grammar Paths 

>D Grammar Path
A **grammar path** is a sequence of non-terminals, indices in their productions, and alternative indices:
$$
p = x_1@a_1 \to x_2@a_2 \to ... \to x_n@a_n
$$

Where for each step $X_i@A_i$:
- $x_i \in \mathbb{N}$ is an index in the current production
- $a_i \in \mathbb{N}$ is the alternative index chosen for non-terminal $x_i$

The path can be interpreted as navigating the grammar structure from one production to another, following specific alternatives at each non-terminal. A grammar path is always relative to a specific production non-terminal.
<



>E Grammar Path Interpretation
The path $1@0 \to 3@0 \to 2@4$ means:
- Start at the $1$-th element of the $0$-th alternative of our start non-terminal (lets call it "A")
- Go to the third element of the $0$-th alternative of "A", which is "B"
- Go to the $4$-th element of the second production alternative for "C".

The produciton index is first, followed by the alternative index after the `@` symbol.
<

>R Converting Grammar Paths to Tree Paths
Grammar paths can be converted to tree paths if the tree respects the choice of alternatives specified in the path.

We can intepret the alternative indices as constraints on the trees we might apply the grammar path to. When traversing a tree, we only follow a grammar path if at each non-terminal node, the alternative chosen matches the one specified in the grammar path.
<

###[W] Regular Grammar Paths

>D Regular Grammar Path
To handle recursive structures (like `Type ::= AtomicType | '(' Type ')'`), we define **Regular Grammar Paths** as regular expressions over $\Sigma = \mathbb{N} \times \mathbb{N}$. A symbol $i@k \in \Sigma$ represents traversing the $i$-th child of a node that selected alternative $k$.
<

>E Binding Through Recursive Structure
To bind `τ` at the `Abstraction` node through potentially nested parentheses:
$$ \beta(\tau, \text{Abstraction}) = 3@0 \cdot (0@1 \cdot 1@0)^* \cdot 0@0 \cdot 0 $$
This matches paths starting with edge 3 (alt 0), followed by any number of parenthesis wrappings, ending at `BaseType`.
<

### Binding Map Construction

For each production $p \in P$ with associated typing rule $\Theta(p)$:

1. **Binding extraction**: Let $\mathcal{B}_\theta = \{b_i \mid b_i \text{ mentioned in } \Theta(p)\}$ be bindings referenced in its typing rule.

2. **Grammar graph traversal**: For each $b \in \mathcal{B}_\theta$, identify the first occurrence of $b$ in $p$ at position $k$. If $\alpha_k[b]$ is a non-terminal, recursively explore **all** of its production alternatives from $A(\alpha_k)$ until $b$ is found. Use breadth-first search. Graph traversal tracks:
   - the index in the production (child position)
   - the index in the alternative structure (which alternative of a non-terminal)

3. **Path invariant extraction**: Compute all acyclic **grammar paths** $\mathcal{P}_b^p = \{p_1, p_2, \ldots, p_m\}$ from the node associated with typing rule $\theta$ to nodes carrying binding $b$. Each path **must** include alternative annotations to avoid ambiguity. For cycles, use *regular grammar paths*.

4. **Store**: $\beta(b, p) = \mathcal{P}_b^p$

>R Grammar Paths are Static
These are **grammar paths**, not tree paths. They are computed once at grammar-load time and remain fixed. They guide the generation of tree paths at runtime.
<

>E Binding Map Construction

We can construct a subset of the binding map $\beta$ for our STLC grammar step by step.

Let's choose a production we'll call $p$:

```ebnf
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type '.' Expression[e]
```

with rule $\theta$:

```
Γ[x:τ₁] ⊢ e : τ₂
----------------------- (abs)
τ₁ → τ₂
```

- $\mathcal{B}_\theta = \{x, \tau_1, e, \tau_2\}$

We can then get our paths for each binding:

- $\beta(x, \text{Abstraction}) = \{1\}$
- $\beta(\tau_1, \text{Abstraction}) = \{3@1 \cdot 0\}$ (Abstraction@0:3 $\to$ Type@1:0 $\to$ FunctionType@0:0)
- $\beta(\tau_2, \text{Abstraction}) = \{3@1 \cdot 2\}$ (Abstraction@0:3 $\to$ Type@1:0 $\to$ FunctionType@0:2)
- $\beta(e, \text{Abstraction}) = \{5\}$

**Note**: Here we assume the typing rule binds to `Type` when it chooses the `FunctionType` alternative (alt 1). The paths $3@1 \cdot 0$ and $3@1 \cdot 2$ explicitly specify that we're taking alternative 1 of `Type` (which is `FunctionType`), then accessing children 0 and 2 of that production respectively.
<

#### Binding Resolution

For a partial tree $T$,  binding $b$, and node $N$ we resolve nodes $V_b$ by matching tree paths against every grammar paths in $R_b = \beta(b, N)$.

A tree node $v$ is in $V_b$ if and only if its path $p = e_1 \dots e_n$ matches $p' \in R_b$. A match requires:
1.  **Edge Match**: The sequence of edge indices from $N$ to $v$ matches the indices in $R_b$.
2.  **Alternative Verification**: For every step $i@k$ in $R_b$, the corresponding tree node $u$ must have the alternative $k$. 
In practice, the **edge match** is ensured by following the *tree path* defined by the *grammar path* stripped of it's alternative indications. Alternative consistency is checked in parralel.

>R Alternative Verification
If edge match but alternative verification fails for every tree invariant the binding has, we assume something is wrong and reject the parse.
<

### Correctness

In order for all of this to work we need to state a few theorems that shall remain invariant. Our main goal will be to prove them.

>T Binding Invariance
For any binding $b$ in production $p$, the set $\beta(b, p)$ is uniquely determined by the grammar structure and independent of input
<

>A Binding Uniqueness
If $|\beta(b, p)| > 1$, then $b$ must be declared as tuple-typed in $\Theta(p)$ with the syntax `(b...)`
<

>R Completeness
This approach separates grammar analysis (compile-time invariants) from tree traversal (runtime binding), reducing parsing complexity while maintaining formal guarantees. Obviously regular tree paths create a lot more trouble, because we can't determine $|\beta(b, p)|$ at grammar loading time.
<
