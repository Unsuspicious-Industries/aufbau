# Proposition 7: Implementation Challenges and Formalization Notes


**Main challenges**: 
- Binding resolution across recursive structures (solved via regular tree paths but a bit janky) 
- Type constraint propagation (partial solution dependent on bindings)
- Proving correctness guarantees (ongoing)

## Partial Parsing and Completability

In order to provide constraints for generation tasks, we develop an approach based on **completability**. A string $s$ is said to be **completable** in a given language if there exists a string $s'$ such that the concatenation $s s'$ is a valid string in that language.

Our goal is to check for completability, but also provide all possible $s'$ that would make $s s'$ valid. Several approaches can be considered, from naive algorithmic implementation that leverages a **partial parser** integrating a **typing core** defining a set of valid type operations implementable in grammars, to higher order logical systems that could allow for a provable implementation of the constraint mechanism.

### Partial Parser

In this codebase I've started implementing a partial parsing system, inspired from **Earley parsing** that collects all possible valid partial trees over a given string $s$, and then uses the typing core to filter out invalid branches based on defined type rules.

Currently, we define a specific grammar format adapted to this approach, with a combination of BNF-like production with _bindings_ and typing rules.

```
Identifier ::= /[a-z][a-zA-Z0-9]*/
Variable(dec) ::= Identifier[x]
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type '.' Expression[e]

AtomicExpression ::= Variable | '(' Expression ')'
Application(app) ::= AtomicExpression[e₁] AtomicExpression[e₂]
BaseType ::= Identifier[τ]
AtomicType ::= BaseType | '(' Type ')'

FunctionType ::= AtomicType[τ₁] '→' Type[τ₂]
Type ::= AtomicType | FunctionType

Expression ::= AtomicExpression | Abstraction | Application

// Variable lookup rule
x ∈ Γ
----------- (dec)
Γ(x)

// Lambda abstraction rule  
Γ[x:τ₁] ⊢ e : τ₂
----------------------- (abs)
τ₁ → τ₂

// Function application rule
Γ ⊢ e₁ : ?A, Γ ⊢ e₂ : ?A → ?B
-------------------------------- (app)
?B
```

> Notice how here, every symbol in a typing rule is treated as a metavariable, that will be bound to a node except the `?x` elements which are silent variables restricted to the typing rule that we use for pattern matching, like in the function application. Here, we are basically saying "check if e₁ is a function type, and if so, bind ?A to the LHS and ?B to RHS"
> I might need to expand on the pattern matching implementation, as it's non-trivial.

### Binding

In order to express context dependent constraint in our partial parsing system, that could be applied by the typing core, we need to find a way to attach type information, as some kind of _meta variables_ to elements of our AST. In order to do this we use **bindings** which are identifiers, defined in the grammar and optionally attached to elements in a production, that constitute an anchor for the typing system.

#### Repetitions Operators and Binding Levels

Another problem arises when we need to establish properties about a variable length list of variably-typed objects, like function arguments in languages like _C_ or _Python_. Indeed, such semantics are not expressible through the simple **function type** $X \rightarrow Y$. We need a type that encapsulates several typed parameters.

A simple solution, and the one I have tried to implement are **tuple types** a type that is essentially a generic "array of other types" bound to an array of objects.

Quick example:

```c
int func(int a, char* b, float c);
```

Can have type $(\text{int},\text{char*},\text{float}) \rightarrow \text{int}$

The grammatical definition of the production rule for function declaration would then look like:

```ebnf
FuncDec ::= Type[τ₁] Identifier '(' FuncArgs ')' ';'
FuncArgs ::= ε | ArgList
ArgList ::= TypedParam | TypedParam ',' ArgList
TypedParam ::= Type[τ₂] Identifier[x]
```

And would have the following typing rule

```
----------------------- (FuncDef)
(τ₂...) → τ₁
```

With `(τ₂...)` being bound to all produced `τ₂` which follow a specific tree scheme below the `FuncDec` node.

The next logical question here is the binding system. How do we formalize the idea of attaching some elements in a production to the same type, and in which order, and above all prevent conflicts and ambiguous heuristics.

#### Over Complicated Parser

In a previous attempt, I tried to implement this by using repetition operators like `*` or `+`, allowing for inline definitions of tuple types, and same-level binding, looking for nodes only on the level directly below the rule carrying node. This naive approach added a lot of complexity to the parsing step, which I haven't managed to figure out within reasonable complexity bounds.

Another drawback of this highly complex parsing step was that it made any formalization attempt very hard.

> *Note*: This is the approach I failed to implement for most of October.  

#### A Simpler More Rigorous Approach

A new proposed approach, that I've started to implement uses grammar defined in a simpler format, similar to the code block above, and leverages _tree invariants_, created when parsing the grammar to match and bind the nodes in the **AST**.

This method has several crucial properties:

- ASTs should have the same shape as the grammar BNF-like trees
- Use tree paths / _invariants_ defined when loading the grammar for binding (input independence).

## Formal Framework

Given a grammar $G = (N,T,P,S,\Theta,A)$ with

- $N$ the set of non-terminals
- $T$ the set of terminals
- $P$ the set of productions with binding annotation
- $S$ the start symbol
- $\Theta: P \to \theta \cup \{\varepsilon\}$ the optional typing rule associated with each production
- $A: N \to P^*$ the production alternatives attached to each non-terminal.

We then have each production $p \in P$ of form $$ \alpha_0[b_0] \alpha_1[b_1]\cdots \alpha_n[b_n] $$

- $\forall k \in [0;n], \alpha_k \in T \cup N$
- $\forall k \in [0;n], b_k \in \mathcal{B} \cup \{\varepsilon\}$
- $\mathcal{B}$ the set of bindings and $\varepsilon$ the empty binding

Each non-terminal can have multiple production alternatives to which it's mapped by $A$:
$$ n ::= p_1 ~|~ p_2 ~| \cdots |~ p_m $$

A typing rule is a set of conditions applied to types, represented here by bindings. 

### Partial Tree and Forest

The partial tree is a tree $t = (V, E,\lambda,\pi,\alpha,\text{root})$ where:
- $V$ is a finite set of nodes
- $E \subseteq V \times \mathbb{N} \times V$ is a finite set of directed, position-indexed edges
- $\lambda: V \to N \cup T$ the label of the node
- $\pi: V \to P$ the production associated with each node
- $\alpha: V \to \mathbb{N}$ the alternative index chosen at each non-terminal node (which production from $A(\lambda(v))$ was selected)
- $\text{root} \in V$ is the root node

Each partial tree corresponds to either a **complete** parse of $s$ or a parse that can become complete by extending $s$. A partial tree **must** have consumed all input to be valid.

The **partial parse forest for $s$** is then defined as the finite ordered set of all partial trees for $s$ produced by the parser. These trees differ by the alternative they chose when parsing.

Parsing the production $A = \alpha ~|~ \beta$ creates two trees: $A(\alpha)$ and $A(\beta)$. A forest is ordered by the order of definition of each alternative in the grammar. The alternative index function $\alpha$ records which choice was made at each non-terminal, which is essential for binding resolution.

We will use the notation $x[i]$ to designate the node pointed at by the edge indexed at $i$ from node $x$.

### Tree Paths

The space of **tree paths** is $\mathcal{P} = \mathbb{N}^*$, where each coordinate is an edge index, pointing to a specific child. It's in a way similar to de Bruijn indexing. Every node $v \in V$ has a unique path $\text{path}(v)$ from the root, defined recursively from $E$:

$$\text{path}(v) = \begin{cases}
\varepsilon & \text{if } v \text{ is root} \\
	\text{path}(v') \cdot i & \text{if } v' \text{ is parent of } v \text{ and } v \text{ is its } i\text{-th child}
\end{cases}$$

**Paths are injective**: $\text{path}_T(x_1) = \text{path}_T(x_2) \implies x_1 = x_2$.

We can recursively extend our notation with *relative path* $p = i_0 \cdot i_1 \cdots i_n$ by using $x[p] = (x[i_0])[i_1 \cdot i_2 \cdots i_n]$.

#### Completeness and Frontiers

For non-terminal $v \in V$ with production $\pi(v) = \alpha_0 \cdots \alpha_n$, symbol $\alpha_s$ is **satisfied** if $$v[s] \text{ exists and is complete}$$

A node is **complete** by induction:
- Terminals are complete if they matched full input (we won't care about the non-complete case, handled [here](https://unsuspicious.org/blog/completing-regex/))
- Non-terminal $v$ is complete $\iff$ all symbols $\alpha_0, \ldots, \alpha_n$ in $\pi(v)$ are satisfied.

Notice that using a parser $\Psi_L$ and saying "node $v$ is complete" is the same as saying "the expression the node $v$ represents belongs in language $L$"

We define the **frontier** of a tree as the path to the incomplete node at the end of a tree. In a complete tree, there is no frontier. We can assert that the frontier is unique.

We define completeness for a **partial forest** with 
$$
\mathcal{F} = \{t_1,t_2,\cdots t_n\} \text{ complete} \iff \exists k ~|~ t_k \text{ complete}
$$ 

### Completability

Given a string $s \notin L$, $s$ is **completable** in $L$ if we can define the completion set $S' \neq \emptyset$ such that $\forall s' \in S': ss' \in L$.

In this case:

- $\Psi_L(s) \neq \text{reject}$
- $\mathcal{F} = \Psi_L(s)$ is **not complete**
- $\mathcal{F}$ becomes complete by extension with $s' \in S'$

#### Completion Examples

**Input**: `λx:Int.`

**Partial Tree**:
```
T = Expression
     └─[0]→ Abstraction(abs)  [complete = false]
            ├─[0]→ "λ"
            ├─[1]→ Identifier("x")
            ├─[2]→ ":"
            ├─[3]→ Type
            │      └─[0]→ AtomicType
            │             └─[0]→ BaseType("Int")
            ├─[4]→ "."
            └─[5]→ Expression  [Missing / End of Input]
```

**Frontier**: path `[0, 5]`, where we are being stopped by end of input

- The tree is **not complete**
- The tree is **completable**

The completion set in this case is defined by all the strings that would satisfy the `Expression` production like for examples:
 - `Expression → AtomicExpression → Variable → Identifier → /[a-z][a-zA-Z0-9]*/`
 - ...

In our scenario we can choose any string matching the identifier regex. Here we decide to pick `"x"`.

**New Partial Tree**:
```
T' = Expression
      └─[0]→ Abstraction(abs)  [COMPLETE]
             ├─[0]→ "λ"
             ├─[1]→ Identifier("x")
             ├─[2]→ ":"
             ├─[3]→ Type
             │      └─[0]→ AtomicType
             │             └─[0]→ BaseType("Int")
             ├─[4]→ "."
             └─[5]→ Expression
                    └─[0]→ AtomicExpression
                           └─[0]→ Variable(dec)
                                  └─[0]→ Identifier("x")
```

#### Frontier Monotonicity

For partial parses $\mathcal{F}(s)$ extending to $\mathcal{F}(s \cdot t)$ with $t \in S'$:

$$\text{front}_{\mathcal{F}(s \cdot t)}(v) \gt \text{front}_{\mathcal{F}(s)}(v)$$

The proof is trivial by definition of $S'$ but it's a very useful tool if we want to check that our parser works as expected.

### Binding Resolution

Given our grammar $G = (N, T, P, S, \Theta, A)$ with binding annotations, we construct a **binding map** $\beta: \mathcal{B} \times P \to \mathcal{P}^*$ that associates each binding identifier with a set of canonical **grammar paths** (not tree paths). These grammar paths include alternative annotations that serve as verification constraints during runtime binding resolution.

#### Grammar paths

A path in the grammar is defined as a sequence of non-terminals, indices in their production and alternative index like so:
$$
p = A@1:0 \to B:3 \to C@2:4
$$

> This path goes from the $0$-th element of the first alternative of non-terminal $A$ which is $B$, to the third element of the $0$-th alternative of $B$ which is $C$ to the $4$-th element of the second production alternative for $C$.

The alternative index is defined after the '$@$' symbol, and the index in the production is after '$:$' In the absence of the $@$ we assume $@0$.

> They can be converted to tree paths if the tree we are working on respects the choice of alternatives.

#### Regular Grammar Paths

To handle recursive structures (like `Type ::= AtomicType | '(' Type ')'`), we define **Regular Grammar Paths** as regular expressions over $\Sigma = \mathbb{N} \times \mathbb{N}$. A symbol $i@k \in \Sigma$ represents traversing the $i$-th child of a node that selected alternative $k$.

Example if we wanted to bind `τ` at the `Abstraction` node:
$$ \beta(\tau, \text{Abstraction}) = 3@0 \cdot (0@1 \cdot 1@0)^* \cdot 0@0 \cdot 0 $$
This matches paths starting with edge 3 (alt 0), followed by any number of parenthesis wrappings, ending at `BaseType`.
#### Construction 

For each production $p \in P$ with associated typing rule $\Theta(p)$:

1. **Binding extraction**: Let $\mathcal{B}_\theta = \{b_i \mid b_i \text{ mentioned in } \Theta(p)\}$ be bindings referenced in its typing rule.
2. **Grammar graph traversal**: For each $b \in \mathcal{B}_\theta$, identify the first occurrence of $b$ in $p$ at position $k$. If $\alpha_k[b]$ is a non-terminal, recursively explore **all** of its production alternatives from $A(\alpha_k)$ until $b$ is found. Use breadth-first search. Graph traversal in the grammar tracks two sets of indices:
     - the index in the production (child position)
     - the index in the alternative structure (which alternative of a non-terminal)
3. **Path invariant extraction**: Compute all acyclic **grammar paths** $\mathcal{P}_b^p = \{p_1, p_2, \ldots, p_m\}$ from the node associated with our typing rule $\theta$ to nodes carrying binding $b$. Each path **must** include alternative annotations to avoid ambiguity. They act as constraints that will verify the parse tree structure at runtime. 
	 - In the case of cycles when creating the **grammar paths**, use the *regular grammar paths*. Same rules but should be more generalizable.
4. Store: $\beta(b, p) = \mathcal{P}_b^p$

> These are **grammar paths**, not tree paths. They are computed once at grammar-load time and remain fixed. They are used in the generation of the tree paths. 

#### Example

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

#### Binding Resolution

For a partial tree $T$,  binding $b$, and node $N$ we resolve nodes $V_b$ by matching tree paths against every grammar paths in $R_b = \beta(b, N)$.

A tree node $v$ is in $V_b$ if and only if its path $p = e_1 \dots e_n$ matches $p' \in R_b$. A match requires:
1.  **Edge Match**: The sequence of edge indices from $N$ to $v$ matches the indices in $R_b$.
2.  **Alternative Verification**: For every step $i@k$ in $R_b$, the corresponding tree node $u$ must have the alternative $k$. 
In practice, the **edge match** is ensured by following the *tree path* defined by the *grammar path* stripped of it's alternative indications. Alternative consistency is checked in parralel.

> If edge match and alternative verification fails for every tree invariant the binding has, we assume something is wrong and reject the parse.

#### Correctness

In order for all of this to work we need to state a few theorems that shall remain invariant. Our main goal will be to prove them.

**Theorem**: For any binding $b$ in production $p$, the set $\beta(b, p)$ is uniquely determined by the grammar structure and independent of input.

**Theorem**: If $|\beta(b, p)| > 1$, then $b$ must be declared as tuple-typed in $\Theta(p)$ with the syntax `(b...)`

> This approach separates grammar analysis (compile-time invariants) from tree traversal (runtime binding), reducing parsing complexity while maintaining formal guarantees. Obviously regular tree paths create a lot more trouble, because we can't determine $|\beta(b, p)|$ at grammar loading time.

### Typing Evaluation

For each tree in a forest, we bind our rules, and then evaluate and check for non-contradiction. 

**Type-constrained completability**: The completion set is filtered by typing rules, ensuring only well-typed completions are generated.

#### Example

Using the abstraction example I proposed for the binding resolution, we can demonstrate simple type-checking behavior. 

Take an input $s = \text{`f(λx:X→Y.`}$ that parses to:

```
T = Expression
     └─[0]→ Application(app)
            ├─[0]→ AtomicExpression
            │      └─[0]→ Variable(dec)
            │             └─[0]→ Identifier("f")
            └─[1]→ AtomicExpression
                   └─[1]→ "(" Expression ")"
                          ├─[0]→ "("
                          ├─[1]→ Expression
                          │      └─[0]→ Abstraction(abs)
                          │             ├─[0]→ "λ"
                          │             ├─[1]→ Identifier("x")
                          │             ├─[2]→ ":"
                          │             ├─[3]→ Type
                          │             │      └─[0]→ FunctionType
                          │             │             ├─[0]→ BaseType("X")
                          │             │             ├─[1]→ '→'        
                          │             │             └─[2]→ BaseType("Y")
                          │             ├─[4]→ "."                   
                          │             └─[INCOMPLETE]
                          └─[INCOMPLETE]
```

**Binding Resolution** for the `Abstraction` node  using production 
```
Abstraction(abs) ::= 'λ' Identifier[x] ':' Type[τ] '.' Expression[e]
```

Note: The typing rule references `τ₁` and `τ₂` but the grammar only binds `τ`. The typing rule pattern-matches against the structure of `τ` to extract these components when `τ` is a `FunctionType`.

Here, we search through the tree using grammar path, and we obtain the following bound results: 
- $\beta(x, \text{Abstraction}) = \{1\}$ → `Identifier` ("x")
- $\beta(\tau_1, \text{Abstraction}) = \{3\cdot 1 \cdot 0 \}$ → `AtomicType` ("X")
- $\beta(\tau_2, \text{Abstraction}) = \{3\cdot 1 \cdot 2\}$ → `Type` ("Y")
- $\beta(e, \text{Abstraction}) = \{5\}$ → `Expression` [INCOMPLETE]

Applying typing rule `Γ[x:τ₁] ⊢ e : τ₂` yields `τ₁ → τ₂` we get constraints:
- The node that will be created as the body of the function ($e$) should be of return type $\tau_2$ 

> **Note**: In the example, the body is incomplete. This would mean that we would create a typing constraint of type $\tau_2$ on next input. In case of a complete body, we would recurse into $e$, find its type, and in case of contradiction, dump the tree.

Now for the **Application node** with production:

```
Application(app) ::= AtomicExpression[e₁] AtomicExpression[e₂]
```

- **`e₁`**: $\beta(e_1, \text{Application}) = \{0\}$ → `Variable` ("f") 
- **`e₂`**: $\beta(e_2, \text{Application}) = \{1\}$ → The `Abstraction` node below, which we evaluated to be of type "X→Y".

Applying the typing rule for `Application`:
```
Γ ⊢ e₁ : ?A, Γ ⊢ e₂ : ?A → ?B
-------------------------------- (app)
?B
```

With the pattern matching engine and silent types, we check that `e₂`'s type can be unified with `?A → ?B` and have `e₁` (which is `f`) have type `?A` for `?A = X`

This produces the typing constraint that `f` must something of type $?A$ which is in this context a wrapper for $X$.


## Typing Core

The typing core is the heart of the constraint system. It evaluates typing rules against bound AST nodes to determine type validity and propagate constraints. It's basically an intepreter for the logic in the type rules.

### Type Language

The type language $\mathcal{T}$ is defined inductively:

$$\tau ::= \text{atom} ~|~ \tau_1 \to \tau_2 ~|~ \tau_1 \land \tau_2 ~|~ \tau_1 \lor \tau_2 ~|~ \neg\tau ~|~ \top ~|~ \bot ~|~ \Gamma(x) ~|~ \text{'raw'}$$

Where:
- **Atom** ($\text{atom}$): Type variables like `τ`, `σ`, `Int`, `Bool`
- **Arrow** ($\tau_1 \to \tau_2$): Function types
- **Intersection** ($\tau_1 \land \tau_2$): Values that are both types
- **Union** ($\tau_1 \lor \tau_2$): Values that are either type  
- **Negation** ($\neg\tau$): Complement types
- **Universe** ($\top$): Top type (all values)
- **Empty** ($\bot$): Bottom type (no values)
- **Context Call** ($\Gamma(x)$): Lookup type of $x$ in context $\Gamma$
- **Raw** ($\text{'raw'}$): Concrete literal types

### Inference Variables

Variables prefixed with `?` (e.g., `?A`, `?B`) are **inference variables**—placeholders that get unified during type checking. They enable pattern matching in rules like:

$$\frac{\Gamma \vdash e_1 : ?A \to ?B \quad \Gamma \vdash e_2 : ?A}{\Gamma \vdash e_1~e_2 : ?B}$$

They are central to the logic by allowing complex operations to be expressed.

### Typing Context

A typing context $\Gamma$ is a partial map from variable names to types:

$$\Gamma: \text{String} \rightharpoonup \mathcal{T}$$

### Context Extension Scoping

**Critical distinction** between extension locations:

| Location | Syntax | Scope |
|----------|--------|-------|
| Premise | $\Gamma[x:\tau] \vdash e : \sigma$ | Local to that premise |
| Conclusion | $\Gamma \to \Gamma[x:\tau] \vdash \sigma$ | Propagates to parent |

**Premise extensions** create a temporary child context:
$$\frac{\Gamma[x:\tau_1] \vdash e : \tau_2}{\Gamma \vdash \lambda x:\tau_1.e : \tau_1 \to \tau_2}$$
Here $x:\tau_1$ is only visible when checking $e$.

**Conclusion extensions** modify the ambient context upward:
$$\frac{}{\Gamma \to \Gamma[x:\tau] \vdash \tau} \text{ (let)}$$
Here $x:\tau$ becomes visible to subsequent sibling nodes.

### Typing Rules Structure

A typing rule $\theta = (\text{name}, \text{premises}, \text{conclusion})$

**Premises**:
- $\Gamma \vdash t : \tau$ — ascription
- $x \in \Gamma$ — membership  
- $\Gamma[x:\tau]$ — local extension (setting)

**Conclusions**:
- $\tau$ — bare type
- $\Gamma(x)$ — context lookup
- $\Gamma \to \Gamma'[x:\tau] \vdash \sigma$ — context transform

### Type Evaluation Algorithm

Given a partial tree $T$ with bound typing rules, we evaluate types bottom-up:

```
INFER-TYPE(node, grammar, context):
    if node is Terminal:
        return context.lookup(node.value) or None
    
    if node.production has rule θ:
        return APPLY-RULE(node, θ, grammar, context)
    
    // No rule: propagate from children
    for child in node.children:
        if type ← INFER-TYPE(child, grammar, context):
            return type
    return Universe
```

```
APPLY-RULE(node, θ, grammar, context):
    // 1. Resolve bindings
    bound_nodes ← {}
    for (binding, paths) in grammar.binding_map.for_rule(θ.name):
        for path in paths:
            if result ← resolve_path(node, path):
                bound_nodes[binding] ← result
    
    // 2. Initialize inference variables
    inference_vars ← {}
    for (name, node) in bound_nodes:
        if text ← get_node_text(node):
            if type ← parse_type(text):
                inference_vars[name] ← type
    
    // 3. Evaluate premises
    for premise in θ.premises:
        premise_ctx ← context
        if premise.setting:
            for (var, type) in premise.setting.extensions:
                var_value ← get_node_text(bound_nodes[var])
                premise_ctx.add(var_value, substitute(type, inference_vars))
        
        if premise.judgment is Ascription(term, expected):
            actual ← INFER-TYPE(bound_nodes[term], grammar, premise_ctx)
            if not UNIFY(actual, expected, inference_vars):
                return FAIL
        
        if premise.judgment is Membership(var, ctx):
            var_value ← get_node_text(bound_nodes[var])
            if var_value not in premise_ctx:
                return FAIL
    
    // 4. Evaluate conclusion
    return substitute(θ.conclusion, inference_vars)
```

### Unification

Unification matches types and binds inference variables:

$$\text{UNIFY}(\tau_1, \tau_2, \sigma) = \begin{cases}
\text{true} & \text{if } \tau_1 = \tau_2 \\
\sigma[?X := \tau_1]; \text{true} & \text{if } \tau_2 = ?X \text{ and } ?X \notin \text{FV}(\tau_1) \\
\sigma[?X := \tau_2]; \text{true} & \text{if } \tau_1 = ?X \text{ and } ?X \notin \text{FV}(\tau_2) \\
\text{UNIFY}(l_1, l_2, \sigma) \land \text{UNIFY}(r_1, r_2, \sigma) & \text{if } \tau_1 = l_1 \to r_1, \tau_2 = l_2 \to r_2 \\
\text{true} & \text{if } \tau_1 = \top \text{ or } \tau_2 = \top \\
\text{false} & \text{otherwise}
\end{cases}$$

The occurs check ($?X \notin \text{FV}(\tau)$) prevents infinite types.

### Type-Constrained Completion

When computing completions, the typing core filters out invalid branches:

$$S'_{typed} = \{s' \in S' ~|~ \text{well-typed}(\Psi_L(s \cdot s'))\}$$

A forest is **well-typed** if at least one tree in it passes type evaluation without contradictions.

#### Context Collection at Hole

For type-aware completion, we collect the available context at the frontier (hole):

```
COLLECT-CONTEXT-AT-HOLE(node, grammar, ctx, constrained):
    if node is partial Terminal:
        return (ctx, constrained)
    
    if node is complete:
        return None
    
    // Extend context from current node's rule
    if node.production.rule:
        for premise in rule.premises:
            if premise.setting:
                for (var, type) in premise.setting.extensions:
                    var_value ← resolve_binding(node, var)
                    ctx.add(var_value)
    
    // Check if any child is membership-constrained
    for (i, child) in node.children:
        child_constrained ← False
        if binding ← child.binding:
            for premise in rule.premises:
                if premise is Membership(binding, _):
                    child_constrained ← True
        
        if result ← COLLECT-CONTEXT-AT-HOLE(child, grammar, ctx, constrained or child_constrained):
            return result
    
    return None
```

When `constrained = true`, only variables from `ctx` are valid completions.

---

## Completion Engine

The completion engine computes valid next tokens using FIRST sets and type filtering.

### FIRST Set Computation

For any symbol $\alpha$, the FIRST set $\text{FIRST}(\alpha)$ contains all tokens that can begin strings derivable from $\alpha$:

$$\text{FIRST}(\alpha) = \begin{cases}
\{\alpha\} & \text{if } \alpha \in T \\
\bigcup_{p \in A(\alpha)} \text{FIRST}(\text{first}(p)) & \text{if } \alpha \in N
\end{cases}$$

Where $\text{first}(p)$ is the first symbol of production $p$.

### Completion Algorithm

```
COMPLETIONS(partial_ast, grammar):
    tokens ← {}
    for root in partial_ast.roots:
        tokens ← tokens ∪ COLLECT-VALID-TOKENS(root, grammar)
    return deduplicate(tokens)

COLLECT-VALID-TOKENS(node, grammar):
    if node.is_complete():
        // Only extensible tokens (regex that can match more)
        return node.last_child.extensions
    
    // Find frontier and compute FIRST sets
    if last_child ← node.children.last():
        if last_child is partial Terminal with remainder:
            return {remainder}
        if last_child is incomplete NonTerminal:
            return COLLECT-VALID-TOKENS(last_child, grammar)
        // Last child complete, need next symbol
        next_symbol ← node.production.rhs[node.children.len()]
        return FIRST(next_symbol, grammar)
    else:
        // No children yet
        return FIRST(node.production.rhs[0], grammar)
```

---

## Implementation Correspondence

The following table maps formal concepts to implementation files:

| Formal Concept | Implementation |
|----------------|----------------|
| Grammar $G$ | `src/logic/grammar/mod.rs` |
| Partial Tree $T$ | `src/logic/partial/structure.rs` |
| Parser $\Psi_L$ | `src/logic/partial/parse.rs` |
| Binding Map $\beta$ | `src/logic/grammar/binding.rs` |
| Binding Resolution | `src/logic/partial/binding.rs` |
| Type Language $\mathcal{T}$ | `src/logic/typing/mod.rs` |
| Typing Rules $\theta$ | `src/logic/typing/rule.rs` |
| Type Evaluation | `src/logic/typing/eval.rs` |
| FIRST Sets | `src/logic/partial/completion.rs` |
| Completability | `src/validation/completability.rs` |

---

## Conclusion

This formalization covers:
- **Partial parsing**: Forest representation with alternatives and frontiers
- **Binding resolution**: Grammar paths and tree path matching
- **Typing core**: Type language, unification, and evaluation algorithm
- **Completion**: FIRST sets and type-constrained filtering

Remaining challenges:
- Formally ensuring no conflicts for binding with regular tree paths
- Proving soundness of the type system composition
- Verifying correctness of structure-preserving BNF parser
- Optimizing completion exploration for large search spaces