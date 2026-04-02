# Verification Theory

This document explains the theoretical approach used by the Coq verification
layer in `verification/coq/`.

It is not a proof script manual. It is the semantic map behind the current Coq
artifacts, why they are structured the way they are, and what they do and do not
 establish.

## 1. Goal Of The Coq Layer

The Coq side is a verified reference implementation for small language-specific
subsets.

Its purpose is to give us:

- a trusted parser/checker for representative languages
- executable proofs of soundness for those verified checkers
- an external oracle against which Rust-generated completions can be checked

The Coq development is therefore used in two ways:

1. as a formal semantics for accepted programs
2. as a validation backend for completion/prefix checking

The second point matters: the Rust implementation is not extracted from Coq, so
the current verification story is relational rather than by construction.

## 2. Overall Method

For each verified language, the Coq code defines:

- a syntax type for terms/programs
- a parser from token lists into that syntax
- an executable typechecker or well-formedness checker
- a soundness theorem for the executable checker

The general shape is:

- `parse : string -> option ast`
- `typecheck : string -> option ty` or `typecheck_program : string -> option unit`
- `soundness : checker input = Some r -> exists ast, parse input = Some ast /\ P ast r`

This is a classic verified-checker architecture:

- parsing is executable
- checking is executable
- proofs connect checker success to a semantic judgment

## 3. Why Executable Checkers?

The Coq development uses executable functions rather than purely inductive
relations as the main operational artifact.

Reasons:

- we want to run them from scripts (`check.sh`, `check_prefixes.sh`)
- we want concrete acceptance/rejection on example programs
- we want proof terms that talk directly about the output of the executable
  checker

This means the semantic relation is often encoded in a lightweight way, for
example:

- `has_type Γ e τ := infer Γ e = Some τ`
- `block_wf Γ body := check_block Γ body = Some tt`

This is less abstract than a traditional declarative type system, but it is
highly usable as a checked reference.

## 4. Common Substrate

`verification/coq/Common.v` provides the shared foundation:

- string equality and lookup
- environments as association lists
- token splitting on whitespace
- a tiny parser-combinator-style substrate

This is intentionally simple.

The verified parsers are not trying to model the entire Rust tokenizer or the
entire grammar language. They model a smaller, controlled token language that is
sufficient for verified examples and completion checking.

## 5. Language-Specific Modules

There are currently four verified language modules:

- `STLC.v`
- `Fun.v`
- `Imp.v`
- `Typescript.v`

Each module follows the same pattern, but with different notions of syntax and
typing.

### 5.1 STLC

The STLC module defines:

- simple types
- lambda/application syntax
- an executable inference procedure

Its key theorem says:

- if `typecheck input = Some τ`
- then there exists a parsed STLC term `e`
- such that `parse input = Some e`
- and `e` has type `τ`

### 5.2 FUN

The FUN module extends the pattern with:

- integers, floats, booleans
- let-bindings
- integer and float operators
- applications

The important point for current project goals is that FUN gives us a verified
oracle for the float/int distinction. This is especially useful for catching
soundness bugs in Rust partial typing and typed completion.

### 5.3 IMP

The IMP module focuses on statement programs and environment threading.

Its judgment is not phrased as “returns a type” for the whole program, but as a
block well-formedness predicate.

That is why the soundness theorem concludes with:

- existence of a parsed body
- and `block_wf nil body`

### 5.4 TypeScript Subset

The TypeScript module is intentionally a subset.

It includes:

- declarations
- arrays
- unions
- function declarations
- calls
- returns
- if/else blocks

The current TypeScript checker is still an executable, syntax-directed
statement/block checker rather than a rich declarative metatheory of TS.

That is acceptable for its current role: a verified oracle for representative
typed programs and generated completions.

## 6. Meaning Of The Soundness Lemmas

The soundness lemmas are one-way acceptance theorems.

Their logical content is:

> whenever the verified checker accepts, there exists a parsed witness program
> or term returned by the verified parser, and that witness satisfies the
> verified typing/well-formedness predicate.

This proves no false positives.

It does **not** prove:

- completeness of the checker
- equivalence with the Rust implementation
- that every Rust completion is valid unless separately checked against Coq

### 6.1 Witness Form

The existential form

```coq
exists p, parse_program input = Some p /\ has_type p
```

is deliberate.

It gives a concrete witness for acceptance:

- the parser really produced a syntax tree
- the checker acceptance corresponds to that exact tree

This is stronger than merely saying “the string is good” in an informal sense.

## 7. Example Theorems

Besides the general soundness lemmas, the Coq files now include explicit example
theorems for concrete programs.

These serve two roles:

- regression-proof executable examples
- documentation of what the verified checker is actually intended to accept

For example, in the TypeScript module we have explicit theorems about:

- array declarations
- array-typed function calls
- rejection of bad initializers
- rejection of bad argument types

These are much easier to inspect than the general soundness theorem alone.

## 8. Prefix Verification Pipeline

The shell script `verification/check_prefixes.sh` uses the Coq layer as an
external oracle for Rust completions.

Pipeline:

1. Rust generates a completion for a prefix
2. the completed program is passed to the verified Coq checker
3. if Coq rejects, the Rust completion is treated as unsound

This is not a proof that Rust is correct.

It is a checked counterexample generator for unsoundness:

- if Rust proposes something Coq rejects, that is immediate evidence of a bug or
  mismatch

So the Coq layer currently acts as a semantic firewall around completion soundness.

## 9. Relationship To Rust

At present, the Rust and Coq systems are related by testing and validation, not
by formal refinement.

That means:

- Coq proves facts about Coq parsers/checkers
- Rust is tested against those facts on representative programs and completions

This is useful, but it is not yet a mechanized equivalence proof.

To get full end-to-end verification, we would need one of:

- a proof that Rust implements the same semantics as Coq
- extraction of the verified checker into the runtime path
- or a formal correspondence theorem between the Rust grammar/parsing/typing
  invariants and the Coq ones

## 10. Current Limits

Theoretical limitations of the current Coq approach:

- tokenization is whitespace-based and simplified
- grammar support is language-specific, not generic over the full Rust grammar DSL
- the TypeScript fragment is intentionally incomplete
- some properties are encoded with executable predicates rather than more
  abstract declarative judgments

These are acceptable for a reference-verifier architecture, but they should be
kept in mind when interpreting proof results.

## 11. What Is Already Strong

Despite those limitations, the current Coq layer already gives us something
substantial:

- verified executable acceptance criteria for multiple languages
- soundness proofs for the acceptance procedures
- concrete example theorems for representative programs
- a practical oracle for catching Rust completion unsoundness

That is enough to justify using Coq as the highest-trust checker in the current
workflow.

## 12. Next Theoretical Steps

The natural next upgrades are:

1. define explicit declarative typing relations for the verified subsets, then
   prove equivalence with the executable checkers
2. formalize a generic statement of completion soundness against the Coq oracles
3. formalize correspondence between the Rust parser invariants and the Coq
   parsers for representative fragments
4. extend the TypeScript subset with more realistic control-flow/type forms while
   keeping proofs manageable

In short:

- today: verified checker + soundness + oracle pipeline
- later: refinement/correspondence between Rust and Coq semantics
