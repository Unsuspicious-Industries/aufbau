#[D] Coq Verification Oracle

The Aufbau engine includes mechanized soundness proofs in Coq for core type systems. These proofs establish a one-way soundness contract: **if the Coq checker accepts a string, then a well-typed AST exists for that string under the formal typing rules**.

## Oracle Contract

The verification pipeline (`verification/check.sh`) operates as follows:

1. **Extraction**: Coq definitions for parsers and type checkers are computationally extracted to OCaml via `Extract.v`.
2. **Orchestration**: The OCaml orchestrator (`verification/orchestrator.ml`) loads the extracted checkers and feeds them prefix inputs from `verification/prefixes/*.txt`.
3. **Witness**: For each accepted prefix, the orchestrator verifies that the Rust engine also parses and types the input successfully.
4. **Soundness**: The Coq proofs guarantee that acceptance implies well-typedness under the formal semantics.

## Guarantees and Non-Goals

>T Coq Checker Soundness
For each verified language $L$ (STLC, Fun, Imp, Typescript), there exists a Coq theorem `L_checker_soundness` stating:
$$\forall \text{input}, \tau.\; \text{typecheck}(\text{input}) = \text{Some}(\tau) \implies \exists e.\; \text{parse}(\text{input}) = \text{Some}(e) \land \text{has\_type}(\varnothing, e, \tau)$$
<

The Coq proofs are **one-way**: they establish that the Coq checker is sound, but they do not mechanically prove that the Rust implementation refines the Coq specification.

>R Explicit Non-Goal
There is currently **no mechanized Rust ↔ Coq refinement proof**. The Rust engine is validated empirically via the fuzzing harness in `src/validation/` and cross-checked against the Coq oracle on representative prefixes.
<

## Theorem Alignment Matrix

| Spec Theorem | Rust Implementation | Coq Theorem | Validation Locus |
|:-------------|:--------------------|:------------|:-----------------|
| Span Bounding | `parse/arena.rs` span construction | *(not mechanized)* | `validation/parseable/` |
| Completeness Soundness | `parse/parser.rs` status propagation | *(not mechanized)* | `validation/parseable/` |
| Monotonic Extension | `parse/advance.rs` | *(not mechanized)* | `validation/parseable/` |
| Context Threading | `typing/runtime.rs` | `L_checker_soundness` (implicit) | `validation/completable/` |
| Binding Invariance | `fusion/binding.rs` | *(not mechanized)* | `validation/completable/` |

The Coq oracle provides a **formal upper bound** on soundness: the Rust engine is tested to agree with the oracle on thousands of representative inputs, but full equivalence is not mechanically proven.
