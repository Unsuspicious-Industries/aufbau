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

(** ** The IR cut, and what would ground it

    This interface is the formal shadow of the implementation's compile/execute
    boundary (see [docs/architecture.md]).  The correspondence:

      - [Rule]        ~  [typing::ir::Program], the compiled instruction stream
      - [eval_rule]   ~  [typing::domain::run], the fold over that stream
      - [apply_effect] ~ [typing::domain::apply_effect]
      - [verdict]     ~  [semantics::domain::Verdict]

    The design claim is that constraints are introduced only by the source
    grammar and its typing rules.  [compile] fixes a schedule and [run]/[descend]
    discharge it; nothing after IR construction can invent a constraint.  Here
    that shows up as [eval_rule] being a parameter: the generator in
    [generation.v] consults it opaquely and never inspects a [Rule], so no
    constraint can enter between [Rule] and [verdict].

    Grounding the claim about the *concrete* domain — rather than assuming it, as
    this signature does — would need roughly:

      1. [eval_rule]'s ascription step agrees with [unify_modulo]: the verdict for
         an [Ascribe] instruction is [Satisfied] exactly when the evaluated
         register and the child's evidence unify modulo the grammar's rewrites.
      2. [Lost] is not merely "unproven" but unreachable: [eval_rule r G evs =
         (Lost, _, _)] implies no [derives] witness extends this node, so pruning
         on [Lost] loses no derivation.  This is the half that makes constrained
         decoding sound rather than merely conservative.
      3. [Live] is exactly the undetermined case: every [Live] node has both a
         completion that reaches [Satisfied] and one that reaches [Lost], so the
         three-valued prune is not hiding a decidable answer.
      4. Compilation preserves meaning: evaluating a [TypingRule] directly and
         evaluating [compile rule] agree on every context and evidence vector.

    None of these are proven or admitted here.  They are the obligations a future
    cut would discharge; [gen_sound] / [gen_complete] in [generation.v] are the
    only [Admitted] statements in this development, and this comment deliberately
    adds no more. *)
