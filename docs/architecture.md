# Architecture

## Layout

`ls src/` is the pipeline, in order:

```text
regex/    lexical level: derivatives, prefix status
grammar/  the SPG: productions, symbols, tokenizer, load/save, binding maps
parse/    Earley over the grammar, incremental, arena-backed
ast/      the derivation itself (FusionAST)
typing/   the constraint domain: surface rules -> IR -> execution
synth/    what a caller drives: feed, mask, verify, explain
```

with `semantics/` interning values for the parser, `ffi/` (python, ocaml),
`validation/` (corpora and properties), and `cli/`.

There is no `engine/` wrapper module. It held no code — nine `pub mod` lines —
while adding a level to every path below it and hiding the pipeline behind a
name that means "the whole crate". Nothing nests more than three deep now.

`typing/` stays flat rather than splitting into per-stage directories: its files
reference each other densely (`domain` → `ir` → `rule` → `types`), so a
directory per stage would lengthen every import without isolating anything. The
stages are marked in `typing/mod.rs` instead.

## What a type is

A *type* is any term the active grammar can derive. Nothing in the engine
enumerates type constructors, and nothing branches on a particular type's name.

That is the whole point, not an implementation detail. Because a type is an
arbitrary term, a "type" can be a data structure, and the constraint engine
becomes a way to enforce any structured scheme: a database schema, a record
shape, a config format, an API payload. Constraining a programming language is
one instance, not the purpose.

Two rules follow, and they are load-bearing:

- **No hardcoded behavior.** A constructor-specific branch, a known type name,
  or a language-specific assumption in engine code narrows what aufbau can
  constrain. Generality here comes from less code, not more.
- **Test over every grammar, discovered not listed.** Invariants are checked
  against all of `examples/*.auf` read at run time, so a grammar nobody wrote
  the test for is still covered.

## Compile-time vs runtime

Aufbau has one internal cut: `typing::ir::compile`.

```text
grammar (.auf) --parse--> TypingRule --compile--> Program --run/descend--> Verdict
                          |                       |
                          introduces constraints  only discharges them
```

**Before the cut** (`src/typing/rule.rs`, the `.auf` source): constraints are
introduced. Every obligation the engine checks originates in a surface rule.
Safety features belong here. A new check is a new premise or a new rule, not a
new case in the executor.

**At the cut** (`src/typing/ir.rs`): `compile` lowers a `TypingRule` to a `Program`,
a flat `Vec<Instr>` plus `splices`, a per-binding map into instruction ranges. Each
`Instr` is one `domain` primitive call. Compilation fixes the schedule of those
calls and adds no logic. Premise-local scoping, implicit in the old tree-walk, is
explicit as balanced `PushScope`/`PopScope`.

**After the cut** (`src/typing/domain.rs`): the executor is fixed.

- `run` folds the stream, threading a substitution, a stack of premise-local
  contexts, and a three-valued status, into a `RuleResult`.
- `descend` replays the instructions before a binding's splice for the substitution
  they fix, then applies that splice's `Extend`s to build the child context.
- `finalize` maps `RuleResult` to `Verdict` (`Satisfied` / `Live` / `Lost`).

`descend` adds semantics of its own: prefix replay and splice application are not in
the `Program`, they are how it is consumed mid-parse. But neither `run` nor `descend`
can invent an obligation the `Program` does not carry. The executor satisfies,
leaves `Unknown`, or prunes. It never extends.

`src/semantics/runtime.rs` compiles every rule once in `TypingRuntime::new`, keyed by
rule name, so the parser's hot path looks up a `Program` instead of recompiling per
node. This is the only compile step at runtime and it happens eagerly at construction.
Nothing recompiles lazily behind the caller's back.

### Addressing the context

The context maps names to types, and a rule addresses a name two ways.

A **binding key** (`Γ[a:τ]`, `Γ(a)`) is the runtime *text* of the token bound to
`a`. This only connects two rules that can both see the same token — a binder
and its uses.

An **ambient key** (`Γ['k':τ]`, `Γ('k')`) is a name the grammar fixes. It exists
because the binding form cannot express a constraint that spans a subtree: a C
function's return type must reach every `return` in its body, and the two rules
share no token, so neither side can name a key the other would produce. What was
missing was a fixed name for the mailbox, not the value.

The two are stored in separate namespaces. Binding keys are user data — a
program names its variables whatever it likes — so a shared namespace would let
a program reach an ambient entry by declaring a variable with that name. That is
not hypothetical: sharing one namespace makes C's `return;` type-check as an
expression statement reading a variable called `return`.

Nothing here is language-specific. An ambient key is how a grammar hands data to
rules that cannot see where it came from — a return type, the table a query is
scoped to, the schema version a record must satisfy.

### Checking and observing the IR

Two modules sit on either side of the cut, and the split is deliberate.

`typing::check` runs **before** execution, once per rule at grammar load. It
rejects programs that are malformed as schedules: unbalanced scopes, a register
read before it is written, overlapping splices, a missing or non-final `emit`,
and any binding an instruction names that no production declares. That last one
matters most — at runtime an unresolvable name is reported as "not known yet",
which is indistinguishable from "not known *yet*", so a rule that can never
resolve otherwise just stays `live` forever with no diagnostic.

`typing::trace` observes execution, and is compiled out unless the `trace`
feature is on. `SPG.ir(rule)` shows the static program; the trace shows the run:
each instruction with the value it produced or why it could not, the scope
depth, the verdict, and every descent into a child.

```
cargo test --features trace
```

```text
lambda <Extensible>
  +1  0| push_scope                     scope opened
  +1  1| r0 = τ                         = A
  +1  2| extend a := r0                 Γ[x : A]
  +1  4| ascribe e : r1                 Satisfied (expected ?B, actual A)
      5| pop_scope                      scope closed
      7| emit r2                        -> FunctionType(A, ?B)
lambda => Satisfied
```

Indentation is tree depth; `+N` is premise-local scope depth, so opening a scope
and descending into a child stay visibly different. Recording goes through the
`trace!` macro, whose whole body — including building the event and formatting
its strings — is inside the `cfg`, so the default build evaluates nothing.

### Rules

- **`run`/`descend` are the hot path.** They execute per node, per parse step, in
  deployment. Spend cost in `compile`, which runs once, never in the fold.
- **Caching is explicit.** A `Program` is a value the caller can hold, compare, and
  reuse. No transparent memoization anywhere below the API. If a result is cached,
  the caller can see where and can build the cache itself.
- **No hidden behavior.** Anything that can be made explicit to the caller is.
  No silent fallbacks, no implicit recompilation, no work that does not appear in
  the API.

### Why here

1. **Soundness stays upstream.** What is derivable is a statement about the grammar
   and `compile`. The executor adds no constraint, so it cannot make an underivable
   program typecheck. The one runtime-side safety mechanism is the three-valued
   prune, and it only ever rejects; it cannot admit.
2. **`Program` is the extension point.** Behaviour is a function of the compiled
   `Program`, not the rule text. A new backend (a different sampler, an external
   solver, a lowering to another runtime) is a new consumer of `Program`, not a
   change to the engine. It must not retype the rule, or point 1 stops holding.
