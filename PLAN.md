# aufbau — plan

**Charter: aufbau constrains. Nothing else.** It owns what a type is, what
unifies, and the three-valued prefix verdict. It knows nothing about models,
tokenizers, tools, sessions, or files.

`../ARCHITECTURE.md` is canonical. This file is what aufbau does about it.

**aufbau mostly stays as it is.** This revision restructures the Python above
it; the engine's charter is unchanged. What follows is therefore maintenance and
one piece of real research, not a rewrite.

## 1. What aufbau owns

```
types, unification, rewrites        what a type means
semantic prefix grammars            .auf loading, elaboration, diagnostics
the three-valued verdict            typed / live / dead, dead unconditionally sound
Synthesizer                         mask (state-free), feed (state-forward),
                                    add_to_ctx, in_scope, status, completeness
FFI                                 Python (PyO3) and OCaml
```

`dead` being **unconditionally sound** is the project's central claim: if the
engine says a prefix is dead, no completion of it can typecheck. Everything
above depends on it. Any change that could weaken it is a correctness change,
not a feature.

## 2. Status

- **372 passed, 5 failed** (+16 ignored), re-measured 2026-09-06 with
  `cargo test --release --lib`. The earlier "376 green" here does not hold, and
  the failures are not new: four of them fail identically at the commit before
  the freshness work, verified in a throwaway worktree.
  - `typing::ir::tests::ir_golden`
  - `validation::corpus::tests::valid_programs_type`
  - `validation::parseable::ml::valid_expressions_ml`, `::valid_programs_ml`
  - `validation::parseable::verdicts::no_false_prunes`, which is new only
    because the test is new; what it named was never a bug. See W1a.
- `SPG.diagnostics()` landed: five checks, so `A ::= B` with `B` undefined is no
  longer accepted in silence.
- **~79% of the stack's measured complexity is aufbau**, and it has never had a
  reduction pass. Two attempts stalled. It remains the largest single target.
- Pre-existing line-count budget violations in `src/grammar`, `src/typing`,
  `src/typing/domain` — **`CI=1` is required for every build**, which is a
  standing papered-over failure, not a convention.

## 3. Workstreams

### W1 — the freshness premise: reviewed and committed (2026-09-06)

**Done.** The work is no longer uncommitted: it was read, measured against the
previous HEAD, and landed as three commits (housekeeping, the engine work, and
the prefix-verdict tests). It is not green, and the four failures it inherits
are recorded in §2 rather than attributed to it.

The review below stands and is kept, because the anti-monotonicity concern is
about the design and not about whether the code was committed.

The concern is precise and it is not stylistic:

- `∈` (membership) is **monotone** in Γ: extending the context cannot falsify a
  premise that held.
- `∉` (freshness, `x ∉ Γ`) is **anti-monotone**: extending the context *can*
  falsify a premise that held.

A prefix judged `dead` under a non-membership premise may become live under a
larger Γ, or a prefix judged live may stop being completable. That is exactly
the shape of thing that undermines "`dead` is unconditionally sound".

That `complete.rs` is in the diff makes it worse, not better: `completeness()`
is what I5 relies on, and it is the check that tells us the mask cannot trap a
small model. A change to the inhabitation calculation must not ride in unread
alongside a soundness-relevant premise.

**First review returned 2026-08-25** (ox-alpha; the transcript tree was removed
in the 2026-08-30 cleanup). Its answer, recorded because it is the sharpest
statement of the problem so far — **and not yet accepted**:

> `dead` remains sound under a `∉` premise **iff the freshness binder ranges
> over an infinite language.** With an infinite binder (`Identifier ::=
> /[a-z]+/`) a `Contradiction` fires only when the name is *already fully* in Γ;
> since Γ only grows, that verdict is irreversible, so `dead` never reverts to
> `live`. With a *finite* binder, `dead` stays sound but the **inhabited
> certificate is lost** — `completeness()` correctly degrades to
> `Sound { blockers: ["freshness"] }`.

That reframes the anti-monotonicity worry precisely: the danger is not `dead`
becoming `live`, it is **I5's certificate silently weakening**. The `complete.rs`
+171 lines exist to detect exactly that, via a `has_finite_freshness_binder`
guard and an `infinite_sorts` fixpoint. Recommendation was **keep**.

⚠️ **The review's verification claim was unsupported — and wrong in detail.** It
stated "the existing 372 passing tests still pass", but its transcript shows only
file reads before a `504 Upstream idle timeout`: **it never ran `cargo test`.**

**Re-run here, 2026-08-25: `376 passed; 0 failed; 16 ignored` in 431s.** The
review's *reasoning* survives; the point about 372 being stale stands.

⚠️ **That green count no longer reproduces.** Measured 2026-09-06 with
`nix develop --command cargo test --release --lib`: **372 passed, 5 failed**,
listed in §2. Four of the five also fail at the commit before this work, so
they are not caused by it, but the suite has not been green here at any point
this file could have observed. Whatever the 08-25 run measured, it is not what
the tree does now, and the number should not be quoted again without a re-run.

- [x] Suite re-run and counted: **372/5/16** on 2026-09-06. Note the documented
      invocation is incomplete — `cargo` is **not** in `nix shell nixpkgs#python3`;
      use this repo's own `nix develop`, which carries the toolchain.
- [ ] Check the claim that the diff's own tests cover the anti-monotone
      direction (`duplicate_fresh_names_are_dead`,
      `freshness_accepts_new_name_and_rejects_rebinding`,
      `freshness_keeps_partial_bound_name_live`,
      `prop_extensible_duplicate_can_grow_fresh`). If they do, the brief's
      "write a property test" item is already satisfied — verify rather than
      duplicate.
- [ ] Decide whether a **finite** freshness binder should be a hard error at
      grammar load rather than a silent certificate downgrade. Under I5 a
      grammar that cannot promise inhabitation is one that can trap a small
      model; `Sound { blockers }` is honest but nothing enforces it yet.
- [ ] Read the diff in full yourself before accepting.
- [ ] Answer explicitly: **under what quantification is `dead` still sound when
      a rule carries `∉`?** State the invariant, do not gesture at it.
- [ ] Property-test the anti-monotone direction specifically: generate a Γ, a
      prefix and an extension, and assert the verdict does not move in a
      direction that breaks soundness.
- [ ] Justify the `complete.rs` change on its own terms.
- [ ] Then keep it or drop it. **Do not leave it sitting green and unread** —
      that is the worst of the three states, because it looks verified.

Motivation is real: without it, a session binding can silently shadow an earlier
one. That is a genuine gap. It does not license landing the fix unexamined.

### W1a — the false prune: **there was none.** Closed 2026-09-25.

`no_false_prunes` failed from the day it was written, and was read as four
completable `ml` prefixes being rejected — a violation of section 1's central
claim that `dead` is unconditionally sound. **That reading was wrong, and the
claim holds.**

The four prefixes were written against a start symbol the grammar no longer
has. On 2026-08-31 `ml.auf` gained a `Program`/`Define` top level, so a program
is a list of structure items `let name (param : T) : T = body`. Every recorded
prefix is a bare expression:

```
let f : int = (             a program must begin `let <name> (`; dies at `:`
1 + (                       a program must begin `let`
let xs : int list = 1       as the first
let rec inc : ...           `Define` has no `rec`
```

All four are therefore **correctly** dead. Rewritten at the real start symbol,
every property the list meant to assert holds and every verdict is `live`:

```
let solve (xs : int list) : int = (             -> live
let solve (xs : int list) : int = 1 + (         -> live
let solve (xs : int list) : int list = 1        -> live
let inc (xs : int list) : int list =
    match xs with [ ] -> [ ] | h :: t -> (h +   -> live
```

The reasoning in the old entry was sound in the abstract — refuting a demand
against a still-open node *is* unsound, and `MUST_STAY_LIVE` remains the
specification protecting against a future eager-refutation optimisation. What
was never checked is whether the engine actually did that. It did not.

**What changed so this cannot recur.** Every `MUST_STAY_LIVE` case now carries a
witness, and the witness is checked: it must extend its prefix and must type.
"Genuinely completable" was an assertion nobody verified, which is exactly how
uncompletable data spent three weeks impersonating unsoundness. A case whose
witness does not type now fails saying *fix the case, not the engine*.

Four sibling failures had the same cause and are also closed: the `ml`
expression suites now parse at `Expression`, `corpora/ml` declares its
nonterminal in a `start` file, and `ir.golden` had predated the `define` rule.
The suite is 384 passed, 0 failed.

Still open, and genuinely: `ocaml/lang_ml.ml` cannot read the corpus `start`
file, because the OCaml FFI exposes no start override on a loaded grammar. Its
ML certification carries the breakage the Rust side has shed.

### W2 — D4: vocabulary-wide masking

**The real performance work in the whole stack**, and it is aufbau-side.

Today's decode loop is rejection sampling: sample a token, screen 2–3 spellings
via `Synthesizer.mask`, exclude and resample, bounded by `max_retries` (2048).
Sound, and it handles tokenizer spacing well. But:

- cost is O(retries) FFI round-trips per emitted token;
- it cannot become a vLLM logits processor, which wants one vectorized mask over
  the whole vocabulary per step;
- when the model's distribution is far from the grammar it exhausts the ceiling
  and returns `no_valid` — honest, and how the flagship demo example failed.

- [ ] Build a cached vocabulary-wide mask: a trie or DFA over the tokenizer
      vocabulary, keyed by synthesizer state.
- [ ] Keep it engine-side. This is §1's "CPU load belongs in the core": the
      retry loop is Python driving the engine one token at a time across the
      FFI. **Push the loop down; do not optimize it where it stands.**
- [ ] Independent of everything above it. Not needed for the demo; needed before
      serving at scale.

This is also the answer to "should inference be rewritten in Rust": the
bottleneck is here, not in the forward pass.

### W3 — reduction pass

- [ ] aufbau is ~79% of measured complexity and has never had one. Two attempts
      stalled — treat that as evidence the target needs decomposition into
      independently landable pieces, not another sweeping attempt.
- [ ] Retire the pre-existing line-budget violations so `CI=1` stops being
      mandatory. A build flag that must always be set is a disabled check.

### W4 — the FFI contract p7 depends on

Everything above aufbau derives structure and types **only** from the FFI —
`node.nt_name()`, `node.children`, `ast.type_of(evidence)`, `spg.tokenize()`,
`spg.unify()`. A second implementation in Python is a silent drift surface.

- [ ] `ENGINE_API == "aufbau.engine/v1"` stays the compatibility gate. p7 raises
      at first import on a mismatch, because a stale wheel has bitten this
      project before.
- [ ] `pyo3/extension-module` means FFI test binaries never link — one of the
      two silent killers already paid for. Keep the build honest.
- [ ] **The missing `type_of` render round-trip property test still does not
      exist**, despite once being claimed in a commit message. `Ast.type_of`
      returning a spelling the engine's own parser rejects was the other silent
      killer. Write it: render a type, re-parse it, assert it unifies with the
      original.

### W5 — I6 strong form (research, not plumbing)

Recorded so it is not mistaken for scheduled work. Checked against `rule.rs`,
`context.rs`, `domain.rs` rather than assumed.

**Can express today:** exact finite capabilities as Γ-membership premises (a
`CanWrite<Foo>` token that must be in scope); typestate growing monotonically
across a statement sequence; literal resource names when those names are context
keys.

**Cannot express:** membership of an arbitrary path expression in a glob;
effects derived from runtime values or from a primitive's result; union or
subset constraints over a whole program's effect set; linear consumption,
quotas, or revocation — any non-monotone policy.

Closing the gap needs path-refinement or dependent types carrying the argument
value, permission premises indexed by it, and either effect-row aggregation or
an end-of-program policy judgment.

- [ ] Schedule as engine research if wanted. The cheapest real step short of it
      is the finite-capability-token form above.
- [ ] Until then: **effects are metadata, enforcement is I7 plus preflight.** Do
      not let a document or a commit message claim otherwise.

## 4. Benchmark boundary

Aufbau is a library/engine under test, not a benchmark runner. Benchmark
admission, timing/provenance artifacts, grading, and canonical summaries belong
to the external [`../benchmarks`](../benchmarks) harness. The stack-wide backend
acceptance definition is [`../ARCHITECTURE.md` §8](../ARCHITECTURE.md); aufbau
contributes engine tests and diagnostics only.

## 5. Required properties

- `dead` is unconditionally sound, under every premise form the engine admits.
- `completeness()` correctly identifies sorts with no closed term.
- A type rendered by `type_of` re-parses and unifies with itself.
- Grammar diagnostics catch undefined nonterminals and unmatched rule labels.
- The engine branches on no primitive name, tool, or model concept.
- Tests build without `CI=1` once W3 lands.

## 6. Done when

The freshness premise is resolved on the record, `type_of` round-trips under a
property test, and the vocabulary-wide mask exists — at which point the decode
loop stops being the stack's bottleneck.
