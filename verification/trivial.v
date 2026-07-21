(** trivial.v — Smoke-test instantiation of the generator framework.

    Provides the most trivial possible [ConstraintDomain] and [GenDomain]
    implementations, then applies the [Generation] functor.  The point is
    purely to validate that the module-type signatures are satisfiable and
    that the entire pipeline elaborates without surprises. *)

From Stdlib Require Import List String.
Import ListNotations.

From AufbauVerif Require Import core domain gen_domain generation.

(** ** A trivial constraint domain — all types collapsed to [unit]. *)

Module TrivialDomain <: ConstraintDomain.

  Definition Rule     : Type := unit.
  Definition Ctx      : Type := unit.
  Definition Evidence : Type := unit.
  Definition Effect   : Type := unit.

  Definition empty_ctx    : Ctx      := tt.
  Definition top_evidence : Evidence := tt.

  Definition eval_rule (_ : Rule) (_ : Ctx) (_ : list Evidence)
    : verdict * Evidence * option Effect :=
    (Satisfied, tt, None).

  Definition apply_effect (_ : Effect) (g : Ctx) : Ctx := g.

End TrivialDomain.

(** ** A trivial generator domain on top of [TrivialDomain]. *)

Module TrivialGen <: GenDomain.

  Module D := TrivialDomain.

  Definition TerminalSpec       : Type := unit.
  Definition Restriction        : Type := unit.
  Definition trivial_restriction : Restriction := tt.

  Definition partial := list (string * D.Evidence).

  Definition project (_ : option D.Rule) (_ : D.Ctx) (_ : partial)
    : list Restriction := [].

  Definition step (_ : TerminalSpec) (_ : Restriction) (_ : nat)
    : list (string * D.Evidence) := [].

  Definition check_tuple (_ : option D.Rule) (_ : D.Ctx) (_ : partial)
    : bool := true.

End TrivialGen.

(** ** Apply the generator functor. *)

Module TrivialGeneration := Generation TrivialGen.

(** Smoke test: the depth-zero generator returns the empty list. *)
Lemma trivial_depth_zero :
  forall g n k,
    TrivialGeneration.gen g n tt k 0 = [].
Proof. reflexivity. Qed.
