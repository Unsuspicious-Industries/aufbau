#[D] Syntax-Directed Typing

Type inference in Aufbau operates concurrently with Earley parsing. Rather than
building a complete parse tree and type-checking it post-hoc, the parser invokes
the typing engine at two well-defined points during item processing:
**descent** (when entering a child nonterminal) and **finalization** (when a
production completes). This architecture enables early rejection of ill-typed
branches, reducing the effective search space of the parser.

## Obligations

The central data structure linking parsing and typing is the **obligation**.

>D Obligation
An **obligation** $o = (b, \Delta_b, v, \alpha)$ consists of:
- A binding name $b \in \mathcal{B}$ drawn from the production's typing rule.
- A set of grammar paths $\Delta_b \subseteq \Delta$ indicating where in the derivation $b$ is resolved (from the binding map $\beta$).
- An optional textual value $v \in \text{String} \cup \{\bot\}$, filled when the corresponding terminal or nonterminal child is consumed.
- An optional actual type $\alpha \in \mathcal{T} \cup \{\bot\}$, filled from the child node's inferred type at completion.
<

Obligations are created when a production is seeded and progressively filled as
the Earley item advances through the production's right-hand side. They carry
the information that the typing rule needs to verify its premises and compute its
conclusion.

>D Obligation Creation
Given a production $p$ with rule name $\rho$ and the binding map $\beta$, the
initial obligation set is:
$$O_0(p) = \bigl\{\, (b,\; \beta(b, \rho),\; \bot,\; \bot) \;\bigm|\; b \in \text{used}(\rho),\; \beta(b, \rho) \neq \varnothing \,\bigr\}$$
where $\text{used}(\rho)$ is the set of binding names referenced in rule $\rho$'s
premises, conclusion type, and context output extensions.
<

## Obligation Propagation

When the parser descends into a child nonterminal at dot position $i$ in
alternative $a$, obligations are **stepped** to produce the child's obligation
set. This filters and shortens paths so the child knows which of the parent's
bindings it is responsible for resolving.

>D Stepping
For parent obligations $O$ at dot $i$, alternative $a$:
$$\text{step}(O, i, a) = \bigl\{\, (b,\; \delta',\; v,\; \alpha) \;\bigm|\; (b, \Delta_b, v, \alpha) \in O,\;\delta' = \text{tail}(\Delta_b, i, a) \neq \varnothing \,\bigr\}$$
where $\text{tail}(\Delta_b, i, a)$ removes the first step from each path whose
first step matches $(i, a)$, discarding non-matching paths.
<

Stepped obligations whose remaining path has length 1 correspond to direct
children of the current production. These are filled upon child consumption:

>D Terminal Filling
When a terminal at dot $i$ in alternative $a$ matches text $t$, any obligation
$o$ with a single-step path matching $(i, a)$ has $o.v \leftarrow t$.
<

>D Nonterminal Filling
When a nonterminal child at dot $i$ completes with arena node $n$, any
obligation $o$ with a single-step path matching $(i, a)$ has $o.v \leftarrow
\text{text}(n)$ and $o.\alpha \leftarrow \text{type}(n)$.
<

## The Typing Runtime Interface

The parser communicates with the typing engine through a two-method trait.

>D TypingRuntime Trait
The **TypingRuntime** trait exposes exactly two operations:

- $\text{descend}(p, i, b, \Gamma, O) \to \Gamma' \mid \bot$
- $\text{finalize}(p, \Gamma_{\text{in}}, O, \sigma) \to (\tau, \Gamma_{\text{out}}) \mid \bot$

where $p$ is a production, $i$ the dot position, $b$ the optional binding name,
$\Gamma$ a context, $O$ the current obligations, $\sigma$ the node status
(Complete or Partial), $\tau$ the inferred type, and $\bot$ denotes rejection.
<

### Descent

Descent is invoked each time the parser processes a nonterminal symbol in a
production's right-hand side. Its purpose is **context extension**: if the
typing rule requires extending $\Gamma$ before judging a particular child (as
in $\Gamma[x{:}\tau] \vdash e : \hat{\tau}$), descent performs the extension
and returns the enriched context for the child.

>D Descent Semantics
Given rule $\rho$ for production $p$, and a premise of the form
$\Gamma[x_1{:}\hat{\tau}_1, \ldots, x_k{:}\hat{\tau}_k] \vdash b :
\hat{\tau}$ where $b$ matches the current binding name, descent:

1. For each extension $(x_j, \hat{\tau}_j)$:
   - Resolves $x_j$ to its textual value via $O$: $v_j = O(x_j).v$
   - Resolves $\hat{\tau}_j$ to a concrete type via $O$ and the substitution context
   - Extends $\Gamma$: $\Gamma \leftarrow \Gamma[v_j \mapsto \text{resolve}(\hat{\tau}_j)]$
2. Returns the extended context $\Gamma'$.

If any required value is missing (the obligation is unfilled), descent returns
$\Gamma$ unchanged, deferring the check to finalization.
<

### Finalization

Finalization is invoked when all children of a production have been consumed
(or the input frontier is reached). It verifies the typing rule's premises
against the now-populated obligations and computes the conclusion.

>D Finalization Semantics
Given rule $\rho$ with premises $\bar{\phi}$ and conclusion $\hat{\tau}_c$,
finalization of a completed item with obligations $O$ and initial context
$\Gamma_{\text{in}}$:

1. Initialize substitution $\theta = \varnothing$ and working context $\Gamma_w = \Gamma_{\text{in}}$.
2. For each premise $\phi_j$, verify against $O$, $\theta$, and $\Gamma_w$:
   - **Ascription** $\Gamma' \vdash b : \hat{\tau}$: resolve $\hat{\tau}$ via $\theta$ and $O$,
     then unify with $O(b).\alpha$. Successful unification may extend $\theta$.
   - **Membership** $x \in \Gamma'$: check that $O(x).v$ is bound in $\Gamma_w$.
   - **Check** $\Gamma' \triangleright b$: verify $O(b).v$ is present (context threading without type assertion).
   - **Operation** $\hat{\tau}_1 \sim \hat{\tau}_2$: resolve both sides and check structural equality or unify.
3. If all premises hold, resolve $\hat{\tau}_c$ via $\theta$ and $O$ to produce $\tau$.
4. Compute $\Gamma_{\text{out}}$ by applying any conclusion context transforms.
5. Return $(\text{intern}(\tau), \text{intern}(\Gamma_{\text{out}}))$.

If any premise fails and $\sigma = \text{Complete}$, finalization returns $\bot$
(the branch is rejected). If $\sigma = \text{Partial}$, the engine falls back
to a lenient mode that permits unresolved obligations.
<

## Context Management

Contexts are interned: structurally identical contexts share the same identifier.
This is critical for the deduplication invariant of the parser, which keys on
context identity.

>D Context Interning
The runtime maintains a bijective mapping $\mathcal{C} : \text{Context} \to
\text{CtxId}$. Calls to $\text{intern}(\Gamma)$ return the existing identifier
if $\Gamma$ is structurally equal to a previously interned context, or allocate
a fresh one otherwise.
<

>D Context Threading
For a production $P \to s_1 \dots s_n$, the output context of child $s_i$
provides the input context for child $s_{i+1}$:
$$\Gamma_{\text{in}} \xrightarrow{s_1} \Gamma_1 \xrightarrow{s_2} \dots \xrightarrow{s_n} \Gamma_{\text{out}}$$
This guarantees that declarations in earlier children (e.g., `let x = 5;`) are
visible to later children in the same production.
<

### Initial vs. Accumulated Context

The parser item tracks two context values:

- $\Gamma_{\text{in}}$: the context at seed time (before any child processing).
- $\Gamma_{\text{cur}}$: the accumulated context after child completions.

Finalization receives $\Gamma_{\text{in}}$ for productions with typing rules
(so the rule can re-derive extensions from obligations) and $\Gamma_{\text{cur}}$
for transparent productions (so child-propagated contexts flow through).

>L Finalization Context Selection
Let production $p$ have rule $\rho$. The context passed to finalization is:
$$\Gamma_{\text{final}} = \begin{cases} \Gamma_{\text{in}} & \text{if } \rho \neq \bot \\ \Gamma_{\text{cur}} & \text{if } \rho = \bot \end{cases}$$
<

## Type Resolution

Type expressions in rules may contain meta-variables ($?A$, $?B$) that refer to
unification unknowns, or binding references ($\tau$, $x$) that must be resolved
from obligations.

>D Type Resolution
Resolution of a type expression $\hat{\tau}$ under substitution $\theta$ and
obligations $O$:

$$\text{resolve}(\hat{\tau}, \theta, O) = \begin{cases}
\theta(m) & \text{if } \hat{\tau} = m \in \text{dom}(\theta) \\
O(m).\alpha_{\text{resolved}} & \text{if } \hat{\tau} = m,\; m \notin \text{dom}(\theta),\; O(m).\alpha \neq \bot \\
\hat{\tau} & \text{if } \hat{\tau} = m,\; m \notin \text{dom}(\theta),\; O(m) = \bot \\
\text{resolve}(\hat{\tau}_1) \to \text{resolve}(\hat{\tau}_2) & \text{if } \hat{\tau} = \hat{\tau}_1 \to \hat{\tau}_2 \\
\Gamma(O(x).v) & \text{if } \hat{\tau} = \Gamma(x)
\end{cases}$$

When resolving from an obligation, if the interned type is $\top$ (indicating
no specific type was inferred for the child), the engine falls back to parsing
the obligation's textual value as a raw type literal.
<

## Deduplication

The parser deduplicates items by the key $(p, \text{dot}, \text{start},
\text{pos}, \Gamma)$. Including the context identifier in the dedup key is
essential: the same syntactic item under different typing contexts may lead to
different parse outcomes (e.g., a variable lookup succeeding in one context and
failing in another). Context interning ensures this does not cause unbounded
state explosion.

>T Dedup Finiteness
If the context space is finite (bounded by the number of distinct extensions
reachable from the grammar's typing rules), then the dedup key space is finite
for any fixed input length.
<

## Transparent Nonterminals and Type Propagation

A nonterminal without a typing rule is **transparent**: it imposes no type
constraints and simply propagates its child's type and context.

>D Transparent Type Inference
If production $p$ has no rule ($\rho = \bot$) and finalization yields
$\tau = \top$, the parser examines the item's children. If exactly one
nonterminal child has type $\tau_c \neq \top$, the node inherits $\tau_c$.
Otherwise the node retains $\top$.
<

This ensures that types inferred deep in the derivation (e.g., `int_lit`
producing `'Int'`) propagate through wrapper nonterminals (e.g., `Expression`)
without requiring explicit typing rules at every level.

## Partial Nodes

When a branch reaches the input frontier, it may be syntactically valid but
structurally incomplete. Partial nodes receive lenient treatment.

>L Partial Fallback
If a node has status $\text{Partial}$ and premise verification fails,
finalization does not reject the branch. Instead, it infers a type from
whatever obligations are resolved and returns $\top$ if no meaningful type
can be determined.
<

This allows the completion engine to continue exploring the frontier without
being blocked by temporary type indeterminacy.
