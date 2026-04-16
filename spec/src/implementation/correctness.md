#[W] Parser Correctness Lemmas

This chapter lemmaifies the parser core. Everything here is intended to be reusable: properties are stated as lemmas over individual functions, then assembled into larger system claims.

**Source:** [`parser.rs`](~/src/logic/parse/parser.rs)

## Dependency Spine

- `L1`-`L6`: path lemmas
- `L7`-`L11`: prune lemmas
- `L12`-`L16`: agenda and completion lemmas
- `L17`-`L22`: process/finalize lemmas
- `G1`-`G6`: global parser properties built from the local lemmas

---

## Path Lemmas

### L1 PathExtendDefinition

For all `parent`, `child`, `alt`:

$$
\mathrm{extend\_path}(parent, child, alt) = parent \mathbin{++} [(child, alt)]
$$

Depends on: function definition only.

### L2 PathExtendLength

$$
|\mathrm{extend\_path}(parent, child, alt)| = |parent| + 1
$$

Uses: `L1`.

### L3 DescendPrefixSound

If

$$
\mathrm{descend\_binding\_path}(current, binding) = Some(rest)
$$

then `current` is a prefix of `binding` and:

$$
binding = current \mathbin{++} rest
$$

Uses: function definition only.

### L4 DescendExactNode

If `binding = current`, then:

$$
\mathrm{descend\_binding\_path}(current, binding) = Some([])
$$

Uses: `L3`.

### L5 DescendStrictSuffix

If `binding = current ++ suffix` and `suffix != []`, then:

$$
\mathrm{descend\_binding\_path}(current, binding) = Some(suffix)
$$

Uses: `L3`.

### L6 DescendRejectsNonPrefix

If `current` is not a prefix of `binding`, then:

$$
\mathrm{descend\_binding\_path}(current, binding) = None
$$

Uses: function definition only.

---

## Prune Lemmas

### L7 PruneRelevantBindings

`prune(sigma, nt)` only considers bindings `b` satisfying:

$$
b.nt = nt
$$

Depends on: filter in `prune`.

### L8 PruneDescendedPaths

Every path used by `prune` is a descended suffix relative to `sigma.path`.

Formally, every `p` in the local `paths` vector satisfies:

$$
\exists b \in sigma.bindings.\; b.nt = nt \land p = \mathrm{descend\_binding\_path}(sigma.path, b.path)
$$

Uses: `L7`.

### L9 PruneFallbackCharacterization

If no relevant descended path exists, `prune` returns all productions of `nt`.

Formally, if `paths = []`, then:

$$
\mathrm{prune}(sigma, nt) = \mathrm{productions\_at}(nt)
$$

Depends on: `prune` implementation.

### L10 PruneAltWitness

If a descended path has the form:

$$
[(child, alt)] ++ tail
$$

then `prune` may extract witness `alt` from that path.

Depends on: `path.first().map(|step| step.1)`.

### L11 PruneExactNodePanic

If:

- `paths != []`, and
- every path in `paths` is `[]`

then `prune` panics.

This is the current repro-class lemma.

Uses: `L4`, `L8`, `L10`.

---

## Agenda and Completion Lemmas

### L12 SeenProcessMonotone

`seen_process` only grows.

Depends on: `HashSet::insert` in `enqueue_process`.

### L13 EnqueueProcessIdempotent

Enqueueing the same syntactic key twice adds at most one `Process` task.

Uses: `L12`.

### L14 SeenCompleteMonotone

`seen_complete` only grows.

Depends on: `HashSet::insert` in `complete`.

### L15 CompletedNodesMonotone

`completed_nodes[(nt,start,end)]` only grows.

Depends on: unconditional `push` in `complete`.

### L16 CompletionWakeIdempotent

Waiters at `(nt,start)` are woken at most once for each `(nt,start,end)` span.

Uses: `L14`.

---

## Process and Finalize Lemmas

### L17 SeedInitialShape

Every item created by `seed(nt, pos, sigma)` satisfies:

- `dot = 0`
- `start = pos`
- `pos = pos`
- `sigma_in = sigma`
- `sigma = sigma`
- `node_path = sigma.path`

Depends on: `seed` implementation.

### L18 TerminalStepPathShape

Every terminal child transition computes child path:

$$
path = \mathrm{extend\_path}(item.node\_path, item.dot, item.prod.alt)
$$

Uses: `L1`.

### L19 NonterminalWaiterPathSound

Every waiter stored by the nonterminal branch carries the same child path used for `typing.descend`.

Uses: `L18`.

### L20 CompletionResumptionAdvancesParent

When a waiter resumes from a completed node, the resumed parent has:

- `dot' = dot + 1`
- `pos' = completion.end`
- one additional child node reference

Depends on: `complete` implementation.

### L21 FinalizeTypedBranchOnly

`finalize` emits a node only if `finish_production` succeeds and returns `out.inferred = Some(ty)`.

Depends on: `finalize` implementation.

### L22 FrontierLiftReusesFinalize

Frontier lifting does not invent a second notion of partial node. It reuses `finish` and `finalize` on blocked items.

Depends on: `parse` post-loop frontier lifting phase.

---

## Global Lemmas

### G1 PathCoherence

All child-entry paths are produced only by repeated applications of `extend_path`.

Uses: `L1`, `L2`, `L18`, `L19`.

### G2 PruneIsTotalOnStrictDescendants

If every relevant binding for `nt` descends from `sigma.path` to a non-empty suffix, then `prune` returns a non-empty alt set and does not panic.

Uses: `L5`, `L8`, `L10`.

### G3 PruneIsNotTotalOnExactNodeBindings

If relevant bindings target the current node exactly, `prune` is not total.

Uses: `L4`, `L11`.

### G4 ProcessDedupPreservesSingle-StepReachability

Process dedup may discard duplicates, but it never prevents the first instance of a syntactic state from being explored.

Uses: `L12`, `L13`.

### G5 CompletionDedupPreservesNodeReachability

Completion dedup suppresses duplicate wakeups but not node recording.

Uses: `L15`, `L16`, `L20`.

### G6 PartialPrefixClosure

A branch blocked only by end-of-input can still become a materialized partial node through frontier lifting.

Uses: `L21`, `L22`.

---

## Current Known Failure Lemma

### KF1 ReproFailureShape

The repro failures are instances of `G3`.

Observed shape:

- a binding is created for target `nt`
- the parser later seeds exactly that same node
- descent strips the whole current path
- the resulting descended path is `[]`
- `paths` is non-empty but yields no alt witness
- `prune` panics

This explains why broad parser mechanics can still look healthy while concrete typed repros fail.

---

## Next Proof Boundary

The next missing semantic choice is a lemma schema for exact-node bindings.

One of the following must become true:

- exact-node bindings are ignored by prune
- exact-node bindings carry alt information elsewhere
- exact-node bindings denote all alternatives
- exact-node bindings are forbidden by construction

Until one of those becomes a real lemma, `G2` cannot be strengthened to a total correctness statement for all well-formed runtime states.
