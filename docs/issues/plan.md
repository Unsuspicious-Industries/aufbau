# Typed Earley Parser — Refactor Plan

## Problem statement

The parser interleaves Earley parsing with syntax-directed type checking.
The current typing integration has structural problems:

1. `prepare_child` / `descend` split artificially. Always called in sequence.
2. Binding collection is fully bottom-up. Children accumulate `BindingValue`
   in `TypingState`, parent merges them at completion, `finish_production`
   re-scans all children to check premises. Fragile and wasteful.
3. `child_sigmas: Vec<TypingState>` on every `Item`. Full sigma clone per
   child, only used at finalization. Duplicates what the arena stores.
4. No early type mismatch. Errors found only at `finish_production`.
5. `BindingMap` / `GrammarPath` are precomputed at grammar load but never
   used at runtime.
6. Pruning is disabled (old version was wrong).
7. `Type::Atom` is a vestigial type variant. It was used as a symbolic
   placeholder for binding names (e.g. `τ` from `Type[τ]`) and resolved
   via ad-hoc `binding_type` lookups. With explicit binding handling in
   the parser, `Atom` has no purpose. It fails unification unconditionally
   and causes silent breakage when it leaks into expected types.

---

## Core idea

**Top-down obligation passing, bottom-up resolution.**

When entering a production `A(r) → s₁ … sₙ`, evaluate rule `r`'s premises
to produce **obligations**: expected (possibly symbolic) types for each
binding, using `Path` / `PathOf` for unresolved references. Push these
obligations down to children as expected types. Children, upon finalizing,
resolve obligations from below. Mismatches reject immediately.

`BindingMap` provides the grammar-space paths from rule to binding sites.
These paths are **stepped** at each descent: when descending into child `i`
at alt `a`, strip the first step `(i, a)` from each path and pass the
suffix to the child. This keeps paths always relative to the current node.

---

## Formal model

### Objects

```
G = (N, T, P, S)           grammar
A(r) → s₁ … sₙ            production with optional rule name r
Γ : Name ⇀ Type            typing context
E : Name → Obligation       obligation map (production-local)
β : BindingMap              precomputed: rule → binding → [GrammarPath]
```

No substitution θ on items. Meta-variable resolution is done once at
FINALIZE by re-running unification over the filled obligations.

### Obligation

```
Obligation = {
    name:     String,           -- binding name from grammar (e.g. "x", "τ", "body")
    paths:    Vec<GrammarPath>, -- grammar paths to binding site, RELATIVE to current node
    expected: Type,             -- expected type (may contain Meta, Path, PathOf, Raw)
    value:    Option<String>,   -- resolved text (filled on scan/complete)
    actual:   Option<TypeId>,   -- resolved type (filled on complete/finalize)
}
```

`expected` can be:
- `Raw("Int")` — concrete, enables immediate rejection
- `Meta("R")` — unresolved meta-variable, resolved at finalize
- `Path([2, 0])` — "the type of the node at grammar path [2, 0] from here"
- `PathOf(Arrow(Path([1]), Meta("R")), [3])` — compound symbolic type
- `Any` — no constraint

**No `Atom` type.** Where the current system uses `Atom("τ")` as a
symbolic reference to a binding value, the new system uses `Path(β(τ))`
— the grammar path to the binding site from BindingMap. This makes the
dependency explicit and machine-checkable. `Path` and `PathOf` already
exist in the `Type` enum, and the unifier returns `Indeterminate` for
them, which means "don't reject yet, defer to finalize".

### Typed Earley item

```
⟨ A(r) → α • β,  i, j,  Γ, E ⟩
```

- `[i, j)` — consumed span
- `Γ` — effective typing context (extended by parent before prediction)
- `E` — obligations for this production's bindings, filled incrementally

`E` is **production-local**. It never propagates into children.
Children receive only `Γ` (possibly extended) and an expected type.

No θ (substitution) on items. Rebuilt at FINALIZE from the completed E.

### Path stepping invariant

> Grammar paths in obligations are always relative to the node they live in.

When the parser descends from production `B` into child `i` at alt `a`,
every obligation path `p = [(i, a), rest…]` whose first step matches
`(i, a)` is **stepped**: the suffix `rest` becomes the new path, relative
to the child. Obligations whose first step doesn't match `(i, a)` are
irrelevant to this child and stay in the parent.

This means:
- At the production root, paths are as computed by `BindingMap`
- At each descent level, paths shrink by one step
- A path of length 0 means "this node IS the binding target"
- Pruning: at prediction time, the set of first-step alts in the remaining
  paths tells us which child alternatives are reachable by obligations

### Start symbol

The start symbol has no parent and no rule above it. Its items begin with
an empty obligation set `E = ∅`. Its own rule (if any) creates obligations
internally at production entry, not from a parent's descent.

---

## Four rules

### DESCEND (predict + enter child)

Merged `prepare_child` + `descend`. Single function.

From `⟨ B(r) → α • A[b] β, i, j, Γ, E ⟩`:

1. **Context extension:** if rule `r` has premise `Δ ⊢ b : τ` where `Δ`
   extends Γ, resolve extensions using `E`:
   ```
   Γ_child = extend(Γ, Δ.extensions, E)
   ```
   Extensions use already-resolved obligations (e.g. `name` is scanned,
   `τ` is typed). If a needed value is still pending → `Γ_child = Γ`.

2. **Expected type:** resolve `τ_exp` for binding `b` from rule `r`:
   ```
   τ_exp = resolve(premise_type_for(b), E, Γ_child)
   ```
   May contain `Meta(...)`, `Path(...)`, `PathOf(...)` if dependencies
   aren't resolved yet. That's fine — unification with `Indeterminate`
   defers the check.

3. **Step obligations:** for each obligation `o` in `E` whose path starts
   with step `(child_idx, _)`, extract the set of matching alt indices.
   These are the only alternatives worth seeding for the child nonterminal.
   If no obligations target this child → seed all alternatives.

4. **Seed:** for each surviving alt `a` of `A`:
   ```
   emit ⟨ A(r') → • γ, j, j, Γ_child, E_child ⟩
   ```
   where `E_child` contains:
   - Obligations stepped from parent (suffix after removing `(child_idx, a)`)
   - New obligations from rule `r'`'s own premises (created from BindingMap)

### SCAN (terminal)

From `⟨ A(r) → α • t[b] β, i, j, Γ, E ⟩` where `input[j]` matches `t`:

- Find obligation for `b` in `E`, set `value = input[j]`
- If obligation has a concrete `expected` and the terminal text doesn't
  match (e.g. membership check) → reject early
- Emit `⟨ A(r) → α t • β, i, j+1, Γ, E' ⟩`

### COMPLETE (child finishes)

`⟨ A(r') → γ •, k, j, Γ_A, E' ⟩` finalizes → arena node with `ty = τ_A`.

For each waiter `⟨ B(r) → α • A[b] β, i, k, Γ, E ⟩` with `τ_exp`:

1. **Early type check:** if `τ_exp` is fully concrete (no Meta, Path,
   PathOf), unify with `τ_A`:
   - `Ok` → continue
   - `Fail` → discard this continuation
   If `τ_exp` has symbolic parts → skip check (deferred to FINALIZE)

2. **Fill obligation:** set `E[b].value = span_text(k, j)`,
   `E[b].actual = τ_A`

3. **Thread context:** `Γ_new = arena_node.env_out`

4. Emit `⟨ B(r) → α A • β, i, j, Γ_new, E_new ⟩`

### FINALIZE (production completes)

From `⟨ A(r) → γ •, i, j, Γ, E ⟩`:

1. **Build θ from scratch:** run unification over all premises using `Γ, E`:
   - Membership `x ∈ Γ`: look up `E[x].value` in `Γ`
   - Ascription `Δ ⊢ b : τ`: resolve `τ` using `E` (Path → obligation
     actual, Meta → unify), check against `E[b].actual`
   - Equality `τ₁ = τ₂`: check after resolution

2. **Resolve conclusion:** `τ_inferred = resolve(conclusion, θ, E, Γ)`

3. **Output context:** `Γ_out = apply_conclusion_context(Γ, θ, E)`

4. Record `ArenaNode { ty, env_in = Γ, env_out = Γ_out }`

For **partial** status: skip premise checks, infer type heuristically
from whatever `E` entries have `actual` set.

---

## Data structures

### `Item` (replaces current)

```rust
pub struct Item {
    pub prod: ProdId,
    pub dot: usize,
    pub start: usize,
    pub pos: usize,
    pub ctx: CtxId,
    pub obligations: Vec<Obligation>,  // E
    pub children: Vec<ChildRef>,
}
```

No `sigma`, `sigma_in`, `child_sigmas`, `node_path`, `theta`.

### `Obligation`

```rust
pub struct Obligation {
    pub name: String,
    pub paths: Vec<GrammarPath>,     // relative to current node, stepped on descent
    pub expected: Type,              // may be symbolic (Meta, Path, PathOf)
    pub value: Option<String>,       // text, filled on scan/complete
    pub actual: Option<TypeId>,      // type, filled on complete
}
```

### `Waiter`

```rust
pub struct Waiter {
    pub item: Item,
    pub expected: Option<Type>,      // τ_exp for early rejection at complete
}
```

### `TypingRuntime` trait (new)

```rust
pub trait TypingRuntime {
    /// Compute (child_ctx, expected_type, stepped_obligations) for
    /// entering child symbol at `dot` position.
    fn descend(
        &self,
        prod: ProdId,
        dot: usize,
        binding: Option<&str>,
        ctx: CtxId,
        obligations: &[Obligation],
    ) -> TransitionResult<DescendResult>;

    /// Evaluate rule premises and resolve conclusion type.
    fn finalize(
        &self,
        prod: ProdId,
        ctx: CtxId,
        obligations: &[Obligation],
        status: NodeStatus,
    ) -> TransitionResult<FinalizeResult>;
}

pub struct DescendResult {
    pub ctx: CtxId,
    pub expected: Option<Type>,
    pub child_obligations: Vec<Obligation>,
}

pub struct FinalizeResult {
    pub inferred: TypeId,
    pub ctx_out: CtxId,
}
```

No `consume_terminal`, `finish_node_child`, `finish_terminal_child`.
Terminal scanning and child completion are handled in the parser directly
by filling obligation fields.

---

## What could go wrong

### 1. Cross-sibling dependencies

`Γ[x:τ] ⊢ body : ?R` — `τ` depends on sibling `Type[τ]` not yet parsed.

**Resolution:** At obligation creation time, the expected type for `body`
uses `Path(β(τ))` — the grammar path to the `τ` binding site. This is
symbolic: the unifier returns `Indeterminate`. When `Type[τ]` completes,
its obligation gets `actual = Raw("Int")`. But the `body` obligation's
`expected` still contains the symbolic `Path(...)`.

Full resolution happens at FINALIZE: `Path(β(τ))` resolves to
`E[τ].actual`, which is now filled. θ is rebuilt, premises are checked.
The early check at COMPLETE only fires for fully concrete expected types,
so it doesn't over-reject.

### 2. Earley dedup vs. expected types

Dedup key `(prod, dot, start, pos)` ignores obligations. Two items at
the same position with different expected types: first wins.

**Resolution:** Correct under the syntax-directedness assumption: for a
given derivation path, there's one valid typing. Two different parents
seeding the same child with different expected types means the child
nonterminal is shared — but only one typing can be correct for the input
at that position. The other fails at FINALIZE of its parent.

If this becomes a problem (polymorphic grammars), include expected type
in the dedup key. Not needed for current grammars.

### 3. Multi-level paths through transparent wrappers

`β(x, abs) = [(2, 0), (0, 0)]` — binding `x` is at child 0 of child 2's
alt 0. Child 2 might be a transparent wrapper (no rule).

**Resolution:** Path stepping handles this naturally. When descending into
child 2 at alt 0, step to `[(0, 0)]`. When the wrapper descends into its
child 0, step to `[]` (empty = "you are the target"). The obligation
propagates through transparent wrappers via `child_obligations`.

For pruning: at each level, the set of alt indices from paths' first steps
restricts which alternatives to seed.

### 4. Partial parse handling

Partial prefixes have incomplete obligations.

**Resolution:** FINALIZE with `status = Partial` skips premise checks.
Infer type from whatever obligations have `actual` set (best-effort).
Preserves prefix-completability.

### 5. Obligations for rule-less productions

Productions without a typing rule have no premises → no self-generated
obligations.

**Resolution:** Transparent wrappers receive stepped obligations from
their parent. They pass them along to their children via `descend`.
Their `finalize` propagates the single child's type (heuristic: if one
non-Any child type exists, use it; otherwise Any).

### 6. Rules belong to nonterminals, not alternatives

A typing rule name is attached to a nonterminal (e.g. `Bind(bind)`),
not to a specific alternative. In practice, every alternative of a
nonterminal with a rule SHOULD define the bindings that the rule
references — otherwise those alternatives are dead branches that will
always fail at FINALIZE.

BindingMap correctly enumerates paths through all alternatives, so if
alt 0 and alt 1 both define binding `x` at different positions, both
paths are recorded. At production entry for alt `a`, the paths whose
first step has `a != current_alt` are irrelevant and get filtered out
during stepping.

Dead alternatives (that don't define required bindings) are not an error
at the grammar level — they're simply rejected at FINALIZE when their
obligations are unfulfilled. Worth a diagnostic comment in the code.

### 7. Obligation storage cost

Obligations are cloned on every item fork (scan/complete/predict).

**Resolution:** Obligations are small: one per binding in the production
(typically 1–5). Much smaller than the current `child_sigmas` which
clones full `TypingState` per parsed child. Net reduction in clone cost.

---

## Atom elimination

`Type::Atom` is removed. Its uses are replaced:

| Current usage | Replacement |
|---|---|
| `Atom("τ")` in premise `⊢ body : τ` | `Path(β(τ))` — grammar path to τ binding |
| `resolve_type(Atom(name))` → `binding_type` lookup | `resolve_obligation(Path(p))` → `E[p].actual` |
| `Atom` in unification → hard fail | `Path` in unification → `Indeterminate` |

The `binding_type` helper (which parsed binding text as `Type::parse_raw`)
is also removed. Type resolution goes through obligation actuals, not
through text re-parsing.

---

## Implementation phases

### Phase 1: Merge `prepare_child` + `descend`

- Single `descend(prod, dot, binding, ctx, obligations) → DescendResult`
- Remove `prepare_child` from trait
- Thread `expected` into `Waiter`
- Behavioral equivalence: all existing tests pass

### Phase 2: Introduce obligations, remove `child_sigmas`

- New `Obligation` struct
- `Item` carries `obligations: Vec<Obligation>` instead of `child_sigmas`
- Remove `sigma`, `sigma_in` from Item
- SCAN: fill obligation value
- COMPLETE: fill obligation actual + value, early unify check
- FINALIZE: use obligations instead of scanning child_sigmas
- Remove `finish_node_child`, `finish_terminal_child`, `consume_terminal`
- Remove old `BindingValue` (path-based), `bind_terminal`, `bind_node`

### Phase 3: Obligation creation from BindingMap + Atom elimination

- At production entry, consult `BindingMap` to create obligations for each
  binding referenced by the rule
- Obligations carry `GrammarPath` (relative to production root)
- `descend` steps paths and computes `child_obligations`
- Replace all `Atom(name)` in expected types with `Path(β(name))`
- Remove `Type::Atom` variant and `binding_type` helper

### Phase 4: Pruning via stepped paths

- At PREDICT, extract alt set from obligation paths' first steps
- Seed only matching alternatives
- Fall back to all-alts when no obligations target the child

---

## What does NOT change

- `ParseArena`, `ArenaNode` — unchanged
- `Grammar`, `Production`, `Symbol` — unchanged
- `BindingMap`, `GrammarPath` (precomputed) — unchanged, now actually used
- `Type` enum (`Path`, `PathOf` stay; `Atom` removed) — minor change
- `Unifier` / `UnifyResult` — unchanged (Indeterminate = deferred)
- Earley chart structure (`Tables`, agenda, waiters, results) — unchanged
- `FusionAST` output — unchanged
- External API (`TypedParser::parse`) — unchanged
