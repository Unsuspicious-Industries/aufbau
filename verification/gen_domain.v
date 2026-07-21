(** gen_domain.v — Generator-side domain interface (Definition 4.1 of §4).

    Extends [ConstraintDomain] with the projection machinery the generic
    enumerator needs:
      - per-position [Restriction]s (possibly infinite, opaque to the core)
      - [project]      : restrictions from rule + ambient ctx + partial prefix
      - [step]         : enumerate ≤ k terminal values satisfying a restriction
      - [check_tuple]  : final n-ary joint check after all children are resolved
*)

From Stdlib Require Import List String.
From AufbauVerif Require Import core domain.

Module Type GenDomain.

  Declare Module D : ConstraintDomain.

  (** Specification handed to the regex / value enumerator at a terminal slot. *)
  Parameter TerminalSpec : Type.

  (** Per-position restriction on acceptable evidence values. *)
  Parameter Restriction         : Type.
  Parameter trivial_restriction : Restriction.

  (** A partial prefix of resolved children: yield-text paired with evidence. *)
  Definition partial := list (string * D.Evidence).

  (** Project per-position restrictions from a rule, context, and partial prefix. *)
  Parameter project : option D.Rule -> D.Ctx -> partial -> list Restriction.

  (** Enumerate up to [k] (value, evidence) pairs satisfying [r]. *)
  Parameter step    : TerminalSpec -> Restriction -> nat ->
                      list (string * D.Evidence).

  (** Final joint check across all resolved children — covers n-ary
      constraints not factoring through left-to-right resolution. *)
  Parameter check_tuple : option D.Rule -> D.Ctx -> partial -> bool.

End GenDomain.
