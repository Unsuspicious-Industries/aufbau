#[W] Context

The typing context $\Gamma$ maps a name to a type. It is threaded through a
rule by its [premises](./premises.md) and modified by its
[conclusion](./conclusion.md).

A name is addressed one of two ways, and the two live in **separate
namespaces**.

Source: [`src/typing/context.rs`](../../../src/typing/context.rs),
[`src/typing/ir.rs`](../../../src/typing/ir.rs),
[`src/typing/domain.rs`](../../../src/typing/domain.rs)

## Operations

>D Context Operations
| Operation | Syntax | Meaning |
| :--- | :--- | :--- |
| **Lookup** | $\Gamma(x)$ | The type bound to $x$'s value, if any |
| **Membership** | $x \in \Gamma$ | $x$'s value is bound |
| **Shadow** | $\Gamma[x:\tau]$ | $\Gamma$ with $x:\tau$, hiding any earlier $x$ |
| **Ambient lookup** | $\Gamma(\texttt{'k'})$ | The type at the fixed name `k` |
| **Ambient shadow** | $\Gamma[\texttt{'k'}:\tau]$ | $\Gamma$ with `k`$:\tau$ ambient |
<

A **binding key** is the *value* of the binding, not the binding name: in
$\texttt{Variable(var) ::= Identifier[x]}$, the premise $x \in \Gamma$ asks
whether the identifier the parser actually read is bound.

An **ambient key** is quoted, and is the name itself. It is what lets two rules
that share no token communicate: a rule can only address a binding key if it
contains a token holding that text, so a constraint that must span a subtree —
a function's return type reaching each `return` inside it — has no binding key
available to either side. The name has to come from the grammar.

Ambient keys are stored apart from binding keys because binding keys are *user
data*: a program names its variables whatever it likes. With one namespace, an
ambient entry `'return'` is reachable by a program that declares a variable
called `return`, and C's `return;` then type-checks as an expression statement.
Separation makes an ambient key unspellable from the object language.

An ambient key that no rule sets is rejected at grammar load (`typing::check`),
the same way a binding no production declares is: both are reads that can never
resolve, and at runtime an unresolvable read is reported as "not known yet",
which is indistinguishable from one that simply has not resolved so far.

An incomplete binding gives $\text{Partial}$ rather than failure. A prefix that
is not yet a known name may still become one, so membership on an open lexeme
succeeds when some binding *starts with* the text read so far.

## Two Scopes

>D Setting vs Effect
A **setting** $\Gamma[x:\tau] \vdash e : \tau'$ is premise-local: $x$ is in scope
for that premise's subtree only, and is gone for its siblings.

An **effect** $\Gamma \to \Gamma[x:\tau] \vdash \tau'$ is a conclusion's export:
it applies to the node's right-hand siblings.
<

These are the only two ways context changes. A setting cannot leak rightward and
an effect cannot be premise-local; conflating them is what makes a `let` in a
sequence differ from a `λ` over a body.

## Compile Time vs Runtime

Context handling is split across the [IR cut](../../../docs/architecture.md).

At compile time `ir::compile` turns each setting into `PushScope` /
`Extend` / `PopScope` instructions and records, per binding, the instruction
range that builds that binding's context. That range is the binding's
**splice**. Settings are premise-local by construction: the splice for one
premise never covers another's extensions.

At runtime `domain::descend` produces the context to enter a child under. It
replays the instructions *before* the child's splice — for the substitution the
earlier premises fix — then applies the splice's `Extend`s under that
substitution. The replay is what makes a setting sound: in a `match` arm,
$\Gamma[\text{head}:?A]$ must bind $\text{head}$ to the scrutinee's element
type, so $?A$ has to be resolved before the extension is applied. Without the
replay $\text{head}$ would bind to a free variable and the arm would typecheck
against any type.

`domain::run` folds the whole stream to a verdict, maintaining a stack of
contexts so a `PopScope` restores exactly what preceded its `PushScope`.

Neither `descend` nor `run` may introduce a context constraint that the compiled
program does not already carry. New checks belong in the grammar rules, before
the cut.
