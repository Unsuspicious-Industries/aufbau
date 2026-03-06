#[W] Premises

Premises are conditions above the line in a typing rule, evaluated left-to-right to thread the typing [context](./context.md).

### Premise Result

Evaluating a premise yields a **three-valued result**:
- $\text{Ok}(\Gamma')$: the premise holds. $\Gamma'$ is the modified context.
- $\text{Fail}$: the premise is definitively violated.
- $\text{Partial}$: the premise is undecided due to incomplete information.

This mirrors the [tree status lattice](../concepts/typing_system.md). Typing constraints are deferred when information is missing.

## Premise Types

>D Ascription (Inference)
$\Gamma \vdash e : \tau$ infers the type of $e$ in context $\Gamma$ and unifies with $\tau$.
1. Resolve binding $e$ to a tree node.
2. Recursively type-check the subtree.
3. Unify the inferred type with $\tau$.
<

>D Check
$\Gamma \triangleright e$ asserts that $e$ is well-typed in $\Gamma$ without constraining its type.
1. Resolve binding $e$.
2. Type-check the subtree.
3. Succeed if status is $\text{Valid}$ or $\text{Partial}$; fail if $\text{Invalid}$.
<

>D Membership
$x \in \Gamma$ asserts that $x$ is bound in context $\Gamma$.
1. Resolve binding $x$ to its text value.
2. Look up text in $\Gamma$.
3. Succeed if found; fail if not; return $\text{Partial}$ if $x$ is incomplete.
<

>D Context Extension
$\Gamma[x:\tau]$ extends the context with binding $x:\tau$.
1. Resolve $x$ and $\tau$.
2. Call $\Gamma.\text{extend}(x, \tau)$.
3. Succeed with $\Gamma' = \Gamma[x:\tau]$; fail if $x$ exists in $\Gamma$.
<

### Operation (Equality / Subtyping)

$\tau_1\ \text{op}\ \tau_2$ where $\text{op} \in \\{=, \subseteq\\}$.
- $\tau_1 = \tau_2$: unify the two types.
- $\tau_1 \subseteq \tau_2$: check the subtype relation.

Both use three-valued logic, returning $\text{Ok}$, $\text{Fail}$, or $\text{Partial}$.

### Evaluation Order

Premises evaluate strictly left-to-right. The context from premise $i$ is input to $i+1$, unless in [no-propagate mode](./context.md). Premise ordering is semantically significant: reordering changes binding availability.
