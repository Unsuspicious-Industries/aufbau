# Coq verification workflow (Dune)

This directory now uses Dune as the single build entrypoint for Coq.
## Commands

- Build all Coq modules:

  ```bash
  dune build @coq
  ```

  If `dune` is not installed in the current environment, the helper scripts
  automatically fall back to direct `coqc` builds of
  `Common.v`, `STLC.v`, `Fun.v`, `Imp.v`, and `Typescript.v`.

- Run the existing verifier helper:

  ```bash
  ./check.sh fun "let x : Int ="
  ./check.sh --program imp "let x : Int = 0 ; x = x + 1"
  ```

- Check prefixes with Coq checker pipeline:

  ```bash
  ./check_prefixes.sh verification/prefixes.txt 3
  ./check_prefixes.sh verification/prefixes/stlc.txt 3
  ./check_prefixes.sh verification/prefixes/fun.txt 3
  ./check_prefixes.sh verification/prefixes/imp.txt 3
  ./check_prefixes.sh verification/prefixes/typescript.txt 3
  ```

## Core Soundness Lemmas

The main proof obligations exposed by the verified checkers are:

- `STLC.typecheck_sound` in `verification/coq/STLC.v`
- `FunLang.typecheck_sound` in `verification/coq/Fun.v`
- `ImpLang.typecheck_program_sound` in `verification/coq/Imp.v`
- `TypescriptLang.typecheck_program_sound` in `verification/coq/Typescript.v`

In addition, each language module now exposes explicit named soundness lemmas and
concrete example-program proofs:

- `STLC.stlc_checker_soundness`
- `FunLang.fun_checker_soundness`
- `ImpLang.imp_checker_soundness`
- `TypescriptLang.typescript_checker_soundness`

And concrete executable examples such as:

- `STLC.stlc_example_program_parses`
- `FunLang.fun_example_program_parses`
- `TypescriptLang.typescript_example_program_parses`

Important caveat: these proofs establish soundness of the verified Coq parser
and checker implementations, plus concrete example acceptance. They do **not**
yet prove full semantic equivalence with the Rust parser/typer on all inputs.

## What The Soundness Lemmas Actually Say

All of the `*_sound` lemmas are one-way “no false positives” theorems.

They have the general shape:

```coq
checker input = Some result
-> exists ast, parser input = Some ast /\ well_typed ast result
```

Read operationally, they prove:

- if the verified checker accepts, then the verified parser really parsed the input
- and the parsed term/program satisfies the verified typing or well-formedness judgment

What they do **not** prove:

- that every well-typed program is accepted (that would be completeness)
- that the Rust implementation is extensionally identical to the Coq one
- that every completion produced by Rust is covered automatically unless it is
  separately checked against the Coq checker pipeline

Per language:

- `STLC.typecheck_sound` / `STLC.stlc_checker_soundness`
  - acceptance implies existence of a parsed STLC term with the returned type
- `FunLang.typecheck_sound` / `FunLang.fun_checker_soundness`
  - acceptance implies existence of a parsed FUN expression with the returned type
- `ImpLang.typecheck_program_sound` / `ImpLang.imp_checker_soundness`
  - acceptance implies existence of a parsed IMP block that satisfies `block_wf`
- `TypescriptLang.typecheck_program_sound` /
  `TypescriptLang.typescript_checker_soundness`
  - acceptance implies existence of a parsed TypeScript-subset program that
    passes the verified statement/block checker (`has_type`)

Run:

```bash
coqtop -quiet -Q verification/_build/default/coq verification.coq <<'EOF'
Require Import verification.coq.STLC.
Require Import verification.coq.Fun.
Require Import verification.coq.Imp.
Require Import verification.coq.Typescript.
Check STLC.typecheck_sound.
Check FunLang.typecheck_sound.
Check ImpLang.typecheck_program_sound.
Check TypescriptLang.typecheck_program_sound.
EOF
```
