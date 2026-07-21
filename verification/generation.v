(** generation.v — Constraint-aware input generator for Semantic Prefix Grammars.

    Reference implementation of the model documented in
    [draft/sections/04-generation.tex].  The eventual Rust generator will be
    differentially tested against the OCaml extraction of this development.

    Contents of the [Generation] functor:
      - [grammar]                — generator inputs (productions, rules, terminals)
      - [evidence_tree]          — Definition 4.4 of the draft
      - [tree_yield / depth / leaves] and small helpers
      - [gen]                    — the core enumerator (Definition 4.5)
      - [derives]                — syntactic derivation predicate
      - [gen_sound / gen_complete] — Theorems 4.6 / 4.7 (Admitted)
*)

From Stdlib Require Import List String Arith.
Import ListNotations.

From AufbauVerif Require Import core domain gen_domain.

Module Generation (G : GenDomain).

  (** ** Grammar parameters *)

  Record grammar : Type := mkGrammar {
    g_start    : nt_id;
    g_prods    : nt_id -> list production;
    g_rule     : nt_id -> option G.D.Rule;
    g_terminal : tm_id -> option G.TerminalSpec
  }.

  (** ** Evidence trees (Definition 4.4) *)

  Inductive evidence_tree : Type :=
    | ETLeaf : string -> G.D.Evidence -> evidence_tree
    | ETNode : nt_id -> nat -> G.D.Evidence ->
               option G.D.Effect -> list evidence_tree -> evidence_tree.

  Definition tree_evidence (t : evidence_tree) : G.D.Evidence :=
    match t with
    | ETLeaf _ ev       => ev
    | ETNode _ _ ev _ _ => ev
    end.

  Definition tree_effect (t : evidence_tree) : option G.D.Effect :=
    match t with
    | ETLeaf _ _        => None
    | ETNode _ _ _ ef _ => ef
    end.

  Definition concat_strs (xs : list string) : string :=
    fold_right String.append EmptyString xs.

  Fixpoint tree_yield (t : evidence_tree) : string :=
    match t with
    | ETLeaf s _          => s
    | ETNode _ _ _ _ kids => concat_strs (map tree_yield kids)
    end.

  Fixpoint tree_depth (t : evidence_tree) : nat :=
    match t with
    | ETLeaf _ _          => 0
    | ETNode _ _ _ _ kids => S (fold_right Nat.max 0 (map tree_depth kids))
    end.

  Fixpoint tree_leaves (t : evidence_tree) : list string :=
    match t with
    | ETLeaf s _          => [s]
    | ETNode _ _ _ _ kids => flat_map tree_leaves kids
    end.

  Definition apply_opt_effect (oe : option G.D.Effect) (Gamma : G.D.Ctx)
    : G.D.Ctx :=
    match oe with
    | None    => Gamma
    | Some ef => G.D.apply_effect ef Gamma
    end.

  (** ** Core generator (Definition 4.5)

      [gen g n Gamma k d] enumerates [(yield, evidence_tree)] pairs for
      derivations from [n] under context [Gamma], with terminal value budget
      [k] per slot and tree depth bound [d].

      For each production [p] of [n]:
        1. Walk [p] left-to-right, projecting per-position restrictions from
           the partial prefix accumulated so far.
        2. At each position, enumerate terminal values via [G.step], or
           recurse into the child NT via [gen ... d'].
        3. After [p] is fully resolved, run the final [G.check_tuple]; on
           success, build an [ETNode] by evaluating the local rule
           ([G.D.eval_rule]) — verdict [Lost] drops the candidate.

      Structural recursion: outer [gen] decreases on [d].  The inner
      [let fix walk] decreases on [syms] and may call outer [gen] with
      [d'] (which is a recognized subterm of [d]). *)

  Fixpoint gen (g : grammar) (n : nt_id) (Gamma : G.D.Ctx) (k : nat) (d : nat)
           {struct d} : list (string * evidence_tree) :=
    match d with
    | 0    => []
    | S d' =>
        flat_map (fun ip =>
          let alt := fst ip in
          let p   := snd ip in
          let fix walk (syms : list symbol)
                       (acc  : list (string * evidence_tree))
                       (ctx  : G.D.Ctx)
                       {struct syms}
                  : list (list (string * evidence_tree)) :=
            match syms with
            | []         => [List.rev acc]
            | s :: rest  =>
                let prefix : G.partial :=
                  map (fun st => (tree_yield (snd st), tree_evidence (snd st)))
                      (List.rev acc) in
                let restrictions := G.project (g.(g_rule) n) Gamma prefix in
                let r := nth (List.length acc) restrictions G.trivial_restriction in
                let candidates : list (string * evidence_tree) :=
                  match s with
                  | SymTm t =>
                      match g.(g_terminal) t with
                      | None    => []
                      | Some ts =>
                          map (fun se => (fst se, ETLeaf (fst se) (snd se)))
                              (G.step ts r k)
                      end
                  | SymNT m =>
                      gen g m ctx k d'
                  end in
                flat_map (fun cand =>
                  let ctx' := apply_opt_effect (tree_effect (snd cand)) ctx in
                  walk rest (cand :: acc) ctx'
                ) candidates
            end in
          let completions := walk p [] Gamma in
          flat_map (fun children =>
            let prefix_final : G.partial :=
              map (fun st => (tree_yield (snd st), tree_evidence (snd st)))
                  children in
            if G.check_tuple (g.(g_rule) n) Gamma prefix_final then
              let yield_s   := concat_strs (map fst children) in
              let kids      := map snd children in
              let child_evs := map tree_evidence kids in
              match g.(g_rule) n with
              | None    =>
                  [(yield_s, ETNode n alt G.D.top_evidence None kids)]
              | Some rl =>
                  let result := G.D.eval_rule rl Gamma child_evs in
                  let v  := fst (fst result) in
                  let ev := snd (fst result) in
                  let oe := snd result in
                  match v with
                  | Lost => []
                  | _    => [(yield_s, ETNode n alt ev oe kids)]
                  end
              end
            else []
          ) completions
        ) (enum_with_index (g.(g_prods) n))
    end.

  (** ** Syntactic derivation predicate

      [derives g n t] means [t] is a well-formed derivation of [n] in [g]:
        - the alternative [alt] indexes a production [p] of [n];
        - the children align positionally with [p];
        - terminal positions hold [ETLeaf] nodes, NT positions recursively
          derive their child NT.

      Semantic invariants (evidence/effect agreement) live in [gen_sound]. *)

  Inductive sym_derives (g : grammar) : symbol -> evidence_tree -> Prop :=
    | sym_term :
        forall t txt ev,
          sym_derives g (SymTm t) (ETLeaf txt ev)
    | sym_nt :
        forall m c,
          derives g m c ->
          sym_derives g (SymNT m) c

  with derives (g : grammar) : nt_id -> evidence_tree -> Prop :=
    | derives_intro :
        forall n alt p ev oe children,
          nth_error (g.(g_prods) n) alt = Some p ->
          List.length children = List.length p ->
          (forall i s c,
              nth_error p i = Some s ->
              nth_error children i = Some c ->
              sym_derives g s c) ->
          derives g n (ETNode n alt ev oe children).

  (** ** Correctness theorems — Theorems 4.6 / 4.7 of the draft

      Stated, not proven.  The proofs depend on per-domain [step_sound]
      obligations (Definition 4.8) discharged inside each concrete
      [GenDomain] instantiation. *)

  (** Soundness: every enumerated pair has a valid derivation, its yield
      matches the string, and its depth respects the bound. *)
  Theorem gen_sound :
    forall (g : grammar) (n : nt_id) (Gamma : G.D.Ctx) (k d : nat)
           (s : string) (t : evidence_tree),
      In (s, t) (gen g n Gamma k d) ->
      derives g n t /\ tree_yield t = s /\ tree_depth t <= d.
  Proof. Admitted.

  (** Completeness: every well-formed derivation [t] of depth ≤ [d] whose
      terminal yields all fit within the value budget [k] is enumerated. *)
  Theorem gen_complete :
    forall (g : grammar) (n : nt_id) (Gamma : G.D.Ctx) (k d : nat)
           (t : evidence_tree),
      derives g n t ->
      tree_depth t <= d ->
      (forall s, In s (tree_leaves t) -> String.length s <= k) ->
      In (tree_yield t, t) (gen g n Gamma k d).
  Proof. Admitted.

  (** ** Sanity facts (actually proven) *)

  Lemma gen_depth_zero :
    forall g n Gamma k, gen g n Gamma k 0 = [].
  Proof. reflexivity. Qed.

  Lemma tree_yield_leaf :
    forall s ev, tree_yield (ETLeaf s ev) = s.
  Proof. reflexivity. Qed.

  Lemma tree_depth_leaf :
    forall s ev, tree_depth (ETLeaf s ev) = 0.
  Proof. reflexivity. Qed.

  Lemma tree_leaves_leaf :
    forall s ev, tree_leaves (ETLeaf s ev) = [s].
  Proof. reflexivity. Qed.

End Generation.
