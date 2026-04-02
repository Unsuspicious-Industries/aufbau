Require Import Corelib.extraction.Extraction.
Require Import verification.coq.STLC.
Require Import verification.coq.Fun.
Require Import verification.coq.Imp.
Require Import verification.coq.Typescript.

Extraction Language OCaml.
Set Extraction Output Directory "extracted".

Extraction
  "verifiers.ml"
  STLC.parse
  STLC.typecheck
  FunLang.parse
  FunLang.typecheck
  ImpLang.parse_program
  ImpLang.typecheck_program
  TypescriptLang.parse_program
  TypescriptLang.typecheck_program.
