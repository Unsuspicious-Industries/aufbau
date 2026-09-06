# Changelog

## 0.5.2 — Demand-driven prediction

First release published to PyPI since 0.3.1. 0.5.0 and 0.5.1 were intermediate
versions that were never uploaded, so a consumer moving from 0.3.1 gets
everything in all three sections below.

### Added

- **Demand-driven prediction.** The type a parent premise ascribes to a child is
  now carried as an inherited attribute, and a production whose conclusion
  pattern cannot meet it is refused at prediction rather than after it consumes
  input. A production with no rule of its own passes the demand through
  unchanged, so a chain like `Expression -> AtomicExpression -> Integer` keeps
  it. The demand is part of the item key, and is canonicalised first, or the
  chart would grow without bound as each rule evaluation minted fresh holes.
- **Freshness premises** are enforced during descent as well as at finalisation,
  so a rebinding is refused where it is written.

### Changed

- **Ascription refutes as soon as the failure is stable**, rather than waiting
  for the production to complete. Unification failure survives instantiation, so
  only a node that can be replaced wholesale has to wait. `let x : Bool = 1` is
  now dead at the `1`.
- `Completeness::Sound` carries `blockers`, not `uninhabited`. The FFI tuple
  shape is unchanged; the list can now name a reason (`"freshness"`) as well as
  a sort.
- `examples/c.auf` gains `*` and `%`; `examples/ml.auf` gains `*`, `/` and
  `mod`. This changes what those grammars accept, so benchmark numbers taken
  against 0.3.1 grammars are not comparable.

### Known failing

`validation::parseable::verdicts::no_false_prunes` fails on four ml prefixes
that are completable and are rejected, which contradicts the unconditional
soundness of `dead`. The bug predates this release: the same four fail at
0.5.1. It is documented in `PLAN.md` W1a with the failing list as its
specification. A consumer should know that a live prefix can be pruned in a
grammar with type-directed list construction.

## 0.5.0 — Python term evidence and freshness-aware completion

### Breaking

- **Python `Ast.type_of()` now returns `Term`, not rendered text.** The old
  0.4.0 wheel returned `str`; the current FFI returns the structured term used
  by the engine. This incompatible correction is why the release is 0.5.0,
  rather than an indistinguishable rebuild of 0.4.0. Consumers must publish and
  install 0.5 wheels together with `proposition7>=0.3`.

### Added

- **Freshness premises** (`x ∉ Γ`) reject completed rebinding while retaining an
  extensible identifier prefix that can still become fresh. Freshness is
  binding-only and validated at grammar load.
- The completeness classifier now accounts for freshness: an infinite binder
  preserves `inhabited`; a finite, exhaustible binder reports the explicit
  `freshness` blocker under `sound`.
- `TypingSynth.mask(candidates)` evaluates a candidate set without mutating the
  synthesizer, matching constrained-decoder use.
- `engine_perf` emits the canonical `aufbau.engine-perf/v2` raw-sample report.

## 0.4.0 — Engine API v1

Adds `aufbau.engine/v1`: a stable identifier naming the whole contract below, so
a consumer checks one string at startup rather than probing for methods.

```python
import aufbau
assert aufbau.ENGINE_API == "aufbau.engine/v1"
```

### Breaking

- **`SPG.build()` removed.** The structural constructor took nested tuples
  (`("nt", "Type", "τ")`) that re-encoded a syntax the grammar parser already
  handles. Build grammars from `.auf` source with `SPG(source)`, or
  programmatically with `aufbau.dsl`, which renders `.auf` and hands it to the
  same parser. See *Migration* below.
- **`aufbau_dsl` is now `aufbau.dsl`.** One package, one namespace. The DSL was
  never actually shipped in a wheel before this release (see *Packaging*), so
  in practice this only affects checkouts that added `python/` to `PYTHONPATH`.
- **`aufbau.pyi` corrected to match the shipped module.** The stub disagreed
  with the extension on several members; the module was right in every case and
  did not change. Callers who wrote code against the *stub* rather than against
  observed behaviour may need to adjust:

  | Member | Stub said | Actually is |
  |---|---|---|
  | `Ast.node_count`, `Ast.is_complete` | attribute | method |
  | `Node.is_complete`, `Node.nt_name`, `Node.child_count` | attribute | method |
  | `Child.terminal_text`, `Child.terminal_complete` | attribute | method |
  | `Node.children` | method returning `list[Node]` | attribute of `list[Child]` |
  | `Node.rhs` | `list[Child]` | `int` (production arity) |
  | `Production.rhs` | method | attribute |

  `scripts/check_wheel_api.py` now enforces stub/module agreement, so this class
  of drift cannot recur.

### Added

- **Ambient context keys.** A setting or effect can name a context entry with a
  quoted key the grammar fixes, rather than the runtime text of a bound token:

  ```
  Γ['return': ret] ⊢ body : ⊤        // a definition publishes its return type
  Γ ⊢ e : Γ('return')                // every `return` in the body reads it
  ```

  A binding key (`Γ[a:τ]`) can only connect rules that both see the same token,
  so a constraint spanning a subtree had no way to express itself. `'return' ∈ Γ`
  asks whether the entry exists at all.

  Ambient and binding keys are **separate namespaces**. Binding keys are user
  data — a program names its variables whatever it likes — so one namespace
  would let a program reach an ambient entry by declaring a variable with that
  name. Concretely, sharing them makes C's `return;` type-check as an expression
  statement reading a variable called `return`.

  Reading an ambient key no rule sets is rejected at grammar load, the same way
  a binding no production declares already was.

- `Synthesizer.set_context(bindings)` — replace the whole typing context. Every
  type is parsed before anything is mutated, so the replacement happens whole or
  not at all; an invalid binding leaves the previous context in place.
  `add_to_ctx()` and `clear_ctx()` remain as compatibility wrappers.
- `Synthesizer.verify(expected_type=None) -> Verification` — reports **every**
  distinct complete-root type, not just the first one found. More than one entry
  in `root_types` means the roots genuinely disagree. With a goal,
  `goal_satisfied` is true only when at least one complete root exists, all
  complete roots unify with the goal modulo the grammar's rewrites, and the roots
  agree on a single type. State-free.
- `Production.__len__`, so `len(production)` works as the stub always promised.
- `aufbau.__version__` and `aufbau.ENGINE_API`.

### Fixed

- **Failed `feed()` no longer corrupts state.** It installed the extended input
  *before* parsing, so a rejected token stayed in `input` with the parse tree
  dropped, and every later call worked off broken state. `feed()` is now
  transactional: input, tree and parser advance together or not at all.
- **The context is authoritative in one place.** The Python wrapper kept its own
  copy and passed it back on every call, so a mutation could be visible to one
  operation and not another. It also invalidated the cached parse tree on every
  call, which meant the cache never hit — a hot-path cost for `mask()`.
- **Epsilon productions survived `source()` round-trips.** An empty right-hand
  side rendered as a blank alternative, which the loader discards, so
  `A ::= 'a' B | ε` silently lost its ε branch on reload.
- **Quoted literal types survived `source()` round-trips.** `'Int'` rendered
  bare and re-parsed as a binding reference, so a reloaded grammar failed with
  "type pattern 'Int' has no complete parse". Separators still render bare, so
  `τ -> ?B` stays readable.
- **Nonterminals carrying a typing rule were registered twice.** `add_production`
  guarded on `productions` while `bind_nt_rule` also registers a name, so every
  rule-bearing nonterminal appeared twice in `nonterminals`, doubling
  `nt_count()`/`nt_index()` and duplicating its rendered productions.
- **`aufbau.dsl` emitted syntax the parser rejects.** Premises were joined with
  spaces instead of commas, settings rendered `Γ[a=?A] ▸ …` instead of
  `Γ[a:τ] ⊢ …`, and `inst(x)` rendered as `Γ[x]`.
- **The C corpus classified three programs wrongly.** Two were mislabelled and
  one relied on a check the fragment did not then perform; it does now, and
  `corpora/c/invalid.txt` asserts it.
- **`return` is checked against the declared return type** in `examples/c.auf`.
  `int f(int x) { return (int*)x; }` was typed and is now rejected — at the end
  of the offending statement, not at the closing brace. It cannot be rejected at
  the offending expression, because `(int*)x + 1` is still a legal continuation
  at that point.
- **An effect no longer falls back to the binder's own name.** `Γ → Γ[x:τ]`
  whose binding had not resolved silently keyed the context by the literal
  string `x`, exporting an entry nobody asked for. Say it with a quoted key or
  say nothing.

### Packaging

- The wheel now ships `aufbau.dsl`, `aufbau.pyi` and `py.typed`. Previously
  `aufbau.pyi` told users to `from aufbau_dsl import …` and the wheel contained
  no such module.
- Release wheels are built per interpreter with an explicit `--interpreter`, on a
  pinned Rust toolchain identical across x86_64 and aarch64. The published 0.2.1
  aarch64 wheel was missing `Synthesizer.from_grammar`, `mask`, `status`,
  `root_type` and `in_scope`; nothing compared the built artifact to its stub.
  CI now installs each wheel into a clean venv and runs
  `scripts/check_wheel_api.py` against it before upload.

### Debuggability

- `typing::check` — static analysis of every compiled rule, run at grammar load.
  Rejects unbalanced scopes, registers read before written, overlapping splices,
  a missing or non-final `emit`, and any binding an instruction names that no
  production declares. That last check turns a class of silent failure into a
  load error: at runtime an unresolvable name is reported as "not known yet",
  so a rule that can *never* resolve was indistinguishable from one still
  waiting for input.
- `typing::trace` + `Synthesizer::explain()` — an execution trace of the IR:
  each instruction with the value it produced or why it could not, scope depth,
  verdict, and each descent. Behind the `trace` feature, and the `trace!` macro
  puts the whole call (arguments included) inside the `cfg`, so the default
  build is unchanged.

### Layout

- **`src/engine/` is dissolved.** It contained no code — nine `pub mod` lines —
  while adding a level to every path beneath it and hiding the pipeline behind a
  name meaning "the whole crate". Its children are now top-level siblings of the
  stages that already were: `grammar/`, `parse/`, `ast/` (was `structure/`),
  `synth/`, plus `error.rs`, `path.rs`, `debug.rs`. `ls src/` is the pipeline.
  Nothing nests more than three deep; 16 files were at four.
- `include_str!` of an example grammar is now manifest-relative rather than a
  `../../../..` chain, so moving a test file cannot break it.
- `typing/` stays flat deliberately, with its stages marked in `mod.rs`. Its
  files reference each other densely, so a directory per stage would lengthen
  every import without isolating anything.

### Verification

- `make check` now runs `rocqchk` over the Rocq development and fails if a `.v`
  file grows an `Admitted` not declared in `verification/OBLIGATIONS.md`. Both
  run in CI, along with the OCaml differential certification.
- `gen_sound` and `gen_complete` remain admitted and are declared as such.
- The Rust suite and the OCaml certification now run the **same**
  `corpora/<lang>/*.txt` files, rather than two similar-looking sets.

## Migration: 0.3 → 0.4

**`SPG.build(...)` → `aufbau.dsl`**

```python
# 0.3
g = aufbau.SPG.build(
    productions=[
        ("Identifier", None, [[("re", "[a-z]+", None)]]),
        ("Variable", "var", [[("nt", "Identifier", "x")]]),
        ("Expr", None, [[("nt", "Variable", None)]]),
    ],
    rules=[("var", "x ∈ Γ", "Γ(x)")],
    start="Expr",
)

# 0.4
from aufbau.dsl import G, nt, re_

g = (G("Expr")
     .prod("Identifier", re_("[a-z]+"))
     .prod("Variable", nt("Identifier", bind="x"), rule="var")
     .prod("Expr", nt("Variable"))
     .rule("var", "x ∈ Γ", "Γ(x)")
     .build())
```

`G.source()` renders the `.auf` it builds, so a grammar that misbehaves can be
inspected as text.

**`import aufbau_dsl` → `from aufbau import dsl`**

**Context updates** — prefer one atomic replacement over repeated single adds:

```python
s.set_context({"x": "int", "f": "int -> int"})   # 0.4
```

**Verification** — `root_type()` still works, but it returns whichever complete
root comes first and cannot show a conflict:

```python
v = s.verify("int -> int")
if v.is_ambiguous():
    raise ValueError(f"conflicting root types: {v.root_types}")
assert v.goal_satisfied
```
