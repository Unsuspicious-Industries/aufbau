#[D] Type Inference

The type inference engine evaluates partial trees against the typing rules $\Theta$ attached to grammar productions. The central judgment is $\Gamma \vdash\_\Theta t : \mathcal{S}$, where $\mathcal{S}$ is a [tree status](concepts/typing_system.md) in the lattice $\\{\text{Valid}(\tau), \text{Partial}(\tau), \text{TooDeep}, \text{Malformed}\\}$.

Source: [`src/logic/typing/eval.rs`](../src/logic/typing/eval.rs)

## Dispatch

Type checking is dispatched top-down from the root of the partial tree.

>D Evaluation Dispatch
Given a tree reference $t$ at depth $d$ in context $\Gamma$:

1. **Depth guard**: if $d > 50$, return $\text{TooDeep}$.
2. **Cache lookup**: if the type cache contains an entry for $t$'s path, return $\text{Valid}(\text{cached})$.
3. **Terminal**: if $t$ is a terminal node:
   - Complete terminal: $\text{Valid}(\top)$.
   - Partial terminal: $\text{Partial}(\top)$.
4. **Non-terminal with rule**: if $t$'s production has a typing rule $\theta \in \Theta$, apply the rule (see below).
5. **Non-terminal without rule**: apply the transparent wrapper (see below).
<

### Completeness Promotion

>L Partial-to-Malformed Promotion
If a non-terminal $t$ is complete (all children present and complete) and not extensible, but the typing result is $\text{Partial}$, the result is promoted to $\text{Malformed}$.
<

This prevents permanently indeterminate states on finished subtrees. A complete, non-extensible subtree that cannot be assigned a definite type has definitively failed type checking.

## Rule Application

When a non-terminal has an associated typing rule $\theta = (\text{premises}, \text{conclusion})$:

>D Rule Application
1. **Resolve bindings**: compute $B = \text{resolve}(t, \theta)$ via [runtime binding](binding.md). If the binding target is at the frontier, return $\text{Partial}$. If the binding structure is malformed, return $\text{Malformed}$.
2. **Create unifier**: initialize a fresh Hindley-Milner unifier $U$ for meta-variable resolution.
3. **Initialize context lanes**: create named context maps. The primary lane is determined by the conclusion's context input, or the first premise's setting, or defaults to $\Gamma$.
4. **Evaluate premises** (in order): for each premise $p_i$, compute $\text{check\_premise}(p_i)$:
   - $\text{Ok}(\Gamma')$: update the relevant context lane.
   - $\text{Fail}$: immediately return $\text{Malformed}$.
   - $\text{Partial}$: set a flag but **continue evaluating remaining premises** (a later premise may Fail).
5. **If any premise was Partial**: return $\text{Partial}(\top)$.
6. **Evaluate conclusion**: resolve the output type via the unifier and bindings, return $\text{Valid}(\tau)$.
7. **Extract context transform**: if the conclusion specifies $\Gamma \to \Gamma[x:\tau]$, build the extended context for sibling propagation.
<

The non-short-circuiting behavior on Partial premises (step 4) is deliberate: a Partial followed by a Fail should yield Malformed, not Partial.

>I type check
{"label":"typecheck: \u03bbx:Int.x","input":"","steps":[{"token":"resolve","tokens":["resolve"],"display":"node: Abstraction(\u03bbx:Int.x)\nrule: abs\nbindings: {param=x, ty=Int, body=x}"},{"token":"premise 1","tokens":["premise 1"],"display":"\u0393 \u22a2 ty : ?A\nresolve ty \u2192 Int\ninfer: Valid(Raw(Int))\nunify ?A := Raw(Int) \u2713"},{"token":"premise 2","tokens":["premise 2"],"display":"\u0393[x:Int] \u22a2 body : ?B\nextend context: x \u2192 Int\nresolve body \u2192 x\nlookup x in \u0393' \u2192 Int\nunify ?B := Raw(Int) \u2713"},{"token":"conclusion","tokens":["conclusion"],"display":"conclusion: ?A \u2192 ?B\nsubstitute: Int \u2192 Int\nresult: Valid(Int \u2192 Int)"}]}
<

## Transparent Wrapper

Productions without typing rules are "transparent": they do not constrain types but propagate results from their children.

>D Transparent Wrapper
For a non-terminal without a typing rule:

- **1 non-terminal child**: recurse into that child. The parent inherits the child's type and output context unchanged.
- **0 non-terminal children** (terminals only): if at the frontier, $\text{Partial}(\top)$; otherwise $\text{Valid}(\top)$.
- **2+ non-terminal children**: evaluate children left-to-right, threading context updates through successive siblings. If any child is $\text{Malformed}$, short-circuit. If any is $\text{Partial}$, the result is $\text{Partial}(\top)$. Otherwise $\text{Valid}(\top)$.
<

The left-to-right context threading in the multi-child case is what enables statement sequences: a `let x = 5` declaration extends the context so that subsequent statements see the binding for `x`.

## Type Cache

>D Type Cache
The type cache is a map $\text{cache}: \mathcal{P} \to \tau$ from tree paths to types, created fresh per `check_tree` invocation and threaded through all recursive calls. Only $\text{Valid}$ results are cached. $\text{Partial}$, $\text{Malformed}$, and $\text{TooDeep}$ results are not cached because they may change as the tree evolves.
<

The cache is controlled by a thread-local flag (`TYPE_CACHE_ENABLED`, default true). Disabling it is useful for debugging but has significant performance cost on deep trees.

## Meta-Variable Resolution

Meta-variables ($?A$, $?B$, etc.) are resolved via Hindley-Milner unification. The unifier maintains a substitution $\sigma: \text{MetaVar} \to \tau$ with the following invariants:

- **Occurs check**: $?X$ must not appear in $\tau$ before binding $?X := \tau$.
- **Idempotent substitution**: $\sigma(\sigma(\tau)) = \sigma(\tau)$.

>D Unification
Unification of types $\tau_1$ and $\tau_2$ produces a three-valued result:

- **Ok**: types are structurally compatible; meta-variable bindings are recorded.
- **Fail**: types are structurally incompatible (e.g., $\text{Raw}(\text{int})$ vs $\text{Raw}(\text{bool})$).
- **Indeterminate**: one or both sides contain unresolved components ($\top$, $\text{Path}(i_1@a_1 \cdots i_n@a_n)$, $\Gamma(x)$); cannot determine compatibility yet.
<

$\top$ is treated as indeterminate in unification (not unified with concrete types), which is consistent with its role as "not yet determined" rather than "accepts everything."

## Resolution Pipeline

Type expressions undergo a two-stage resolution before use:

1. **Meta resolution** ($\text{solve\_meta}$): substitute all bound meta-variables from the unifier's substitution map.
2. **Binding resolution** ($\text{solve\_binding}$): substitute all $\text{Atom}(x)$ variables with values read from the tree via the [runtime binding](binding.md) system.

## Subtyping

>D Subtyping Rules
The subtyping relation $\tau_1 \subseteq \tau_2$ is defined by:

- $\bot \subseteq \tau$ for all $\tau$ (bottom is subtype of everything).
- $\tau \subseteq \top$ for all $\tau$ (everything is subtype of top).
- $\tau \subseteq \tau$ (reflexivity, via structural equality).
- $(\tau_1 \to \tau_2) \subseteq (\tau_3 \to \tau_4)$ iff $\tau_3 \subseteq \tau_1$ and $\tau_2 \subseteq \tau_4$ (contravariant in domain, covariant in codomain).
- $(\tau_1 \mid \ldots \mid \tau_n) \subseteq \tau$ iff $\tau_i \subseteq \tau$ for all $i$.
- $\tau \subseteq (\tau_1 \mid \ldots \mid \tau_n)$ iff $\tau \subseteq \tau_i$ for some $i$.
<

Equality is three-valued: $\text{equal}(\tau_1, \tau_2) \in \\{\text{true}, \text{false}, \text{indeterminate}\\}$. Indeterminate arises when either side contains $\top$, $\text{Path}(\cdots)$, or $\Gamma(x)$.

## Depth Limit

The typing depth limit of 50 prevents infinite recursion on grammars with cyclic typing dependencies. At depth 50, the checker returns $\text{TooDeep}$, which is treated as "ok" by `evaluate_typing` (i.e., $\text{TooDeep}$ trees are not rejected). This is a conservative choice: a tree that exceeds the depth limit is assumed to be potentially valid rather than definitively malformed.

>W TooDeep Is Not Malformed
$\text{TooDeep}$ returns `is_ok() = true`, meaning `evaluate_typing` accepts forests containing only TooDeep trees. This could cause false positives: a tree that would be $\text{Malformed}$ at depth 51 is instead accepted as $\text{TooDeep}$ at depth 50. The depth limit of 50 is empirically sufficient for all current grammars, but it is a soundness approximation, not a guarantee.
<
