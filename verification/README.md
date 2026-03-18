# Coq verification workflow (Dune)

This directory now uses Dune as the single build entrypoint for Coq.
## Commands

- Build all Coq modules:

  ```bash
  dune build @coq
  ```

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
  ```

## Core Soundness Lemmas

The main proof obligations exposed by the verified checkers are:

- `STLC.typecheck_sound` in `verification/coq/STLC.v`
- `FunLang.typecheck_sound` in `verification/coq/Fun.v`
- `ImpLang.typecheck_program_sound` in `verification/coq/Imp.v`

Run:

```bash
coqtop -quiet -Q verification/_build/default/coq verification.coq <<'EOF'
Require Import verification.coq.STLC.
Require Import verification.coq.Fun.
Require Import verification.coq.Imp.
Check STLC.typecheck_sound.
Check FunLang.typecheck_sound.
Check ImpLang.typecheck_program_sound.
EOF
```
