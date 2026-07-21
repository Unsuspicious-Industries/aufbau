# Parser Rewrite Flaws Report

This file records the remaining issues after moving `src/logic/parse/` onto the agenda/waiter/result model from `plan.md` and `plan.amend.md`.

Only issues that are still structurally hard are listed here. The easy fixes were applied directly in code.

## Fixed Without Structural Changes

- Nested partial prefixes now propagate upward by saturating frontier items into partial completions and re-running the normal `Complete -> waiter resume -> Process` loop.
- Repetition-backed parser tests were removed from the parser rewrite scope.
- The live parser entrypoint now uses agenda processing instead of recursive descent.
- Frontier state now stores parser `Item`s directly instead of an extra adapter type.

## Remaining Hard Issues

## 1. Ambiguous Complete Roots Still Collapse

### Symptom

- Grammars like `Start ::= A | B`, `A ::= 'x'`, `B ::= 'x'` still produce one returned state instead of two.

### Root Cause

- The agenda control model is correct: different start productions are seeded independently.
- The collapse happens after control reaches completion/finalization.
- The current forest/finalization path still effectively normalizes some successful end states together.
- The most likely collapse points are:
  - typed node finalization in `TypedParser::finalize`
  - state projection at the parse root boundary
  - assumptions elsewhere that one completed start span implies one externally visible state

### Why This Is Hard

- The parser control loop is no longer the main problem.
- The remaining problem lives at the interface between:
  - typed finalization
  - arena node creation
  - external state projection

### Recommendation

- Audit every place where a completed start-span node can be collapsed into a single representative.
- Treat `CompletedNodes[(start_nt, 0, end)]` as the source of truth for externally visible roots.
- Verify that `finalize` and root projection do not discard distinct successful nodes that share span and type.

## 2. Legacy Depth Infrastructure Still Exists Outside The Core Parser

### Symptom

- The new parser no longer needs depth for correctness.
- But the codebase still contains:
  - `with_max_depth`
  - `DepthConfig`
  - `MetaTypedParser`
  - synthesizer/search code that escalates parse depth
  - tests that still assume depth-limited parsing exists

### Root Cause

- The old system relied on recursive descent plus escalating depth caps.
- The agenda rewrite replaced that only in the core parser.
- Higher-level orchestration still assumes parse failure may mean "try again at a larger depth".

### Why This Is Hard

- Removing it cleanly is a cross-module change, not a local parser edit.
- `MetaTypedParser` is wired into synthesis and many tests.

### Recommendation

- Replace `MetaTypedParser` with plain `TypedParser` in all call sites.
- Delete `DepthConfig` and the escalation loop in `src/logic/fusion/meta.rs`.
- Turn `with_max_depth` into a no-op temporarily if needed during migration, then remove it.
- Remove all tests whose only purpose is exercising depth escalation.

## 3. `State` Still Carries Legacy Fields That Are Not Part Of The Formal Model

### Symptom

- `State` still contains legacy fields like `next` and depth metadata.
- The formal parser model only needs root/span/frontier plus whatever metadata the surrounding UI truly uses.

### Root Cause

- The old prefix/advance API shaped `State` around a single path-oriented continuation.
- The agenda parser uses a frontier set instead.

### Why This Is Hard

- `State` is used outside the parser module.
- Removing legacy fields requires touching display, fusion helpers, synthesis, and tests.

### Recommendation

- Redefine `State` around the agenda parser model.
- Remove `next` entirely once all external users switch to frontier-based continuation.
- Downgrade depth metadata to optional diagnostics or remove it once meta parser deletion is complete.

## 4. `advance()` Is Still Legacy-Model Code

### Symptom

- `parse()` is now agenda-based, but `advance()` still extends old-style partially materialized roots.

### Root Cause

- `advance()` was not yet rewritten to restart from stored frontier items.

### Why This Is Hard

- It needs a frontier-first continuation model, not the old root-walking heuristic.
- That means the public prefix API needs to trust frontier items as the real continuation state.

### Recommendation

- Reimplement `advance()` from `State.frontier` only.
- Seed the agenda from those items on the extended input.
- Delete the existing root-extension logic afterward.

## Suggested Cleanup Order

1. Finish the root projection audit for ambiguous complete roots.
2. Rewrite `advance()` from frontier items.
3. Remove `next` from `State`.
4. Remove `MetaTypedParser` and depth escalation from fusion/synth.
5. Remove `with_max_depth`, depth-only tests, and remaining legacy parser code.
