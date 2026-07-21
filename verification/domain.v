(** domain.v — Abstract constraint domain interface (Definition 2.8 of §2).

    A constraint domain bundles the rule language, contexts, evidence and
    right-bound effects of a semantic algebra.  The generic generator (in
    [generation.v]) treats this interface as opaque: only [eval_rule] and
    [apply_effect] are consulted at build time. *)

From Stdlib Require Import List.
From AufbauVerif Require Import core.

Module Type ConstraintDomain.

  Parameter Rule     : Type.
  Parameter Ctx      : Type.
  Parameter Evidence : Type.
  Parameter Effect   : Type.

  (** Initial/empty context.  Used as the root [Gamma] in the generator. *)
  Parameter empty_ctx    : Ctx.

  (** Evidence assigned to non-terminals that have no associated rule
      (the [⊤] cell of the domain).  This is what propagates upward when a
      production carries no semantic premise. *)
  Parameter top_evidence : Evidence.

  (** Evaluate a rule against a context and a vector of child evidences.
      Returns a verdict, the exported evidence value, and an optional
      right-bound effect on the ambient context. *)
  Parameter eval_rule : Rule -> Ctx -> list Evidence ->
                        verdict * Evidence * option Effect.

  (** Apply a right-bound effect to the ambient context. *)
  Parameter apply_effect : Effect -> Ctx -> Ctx.

End ConstraintDomain.
