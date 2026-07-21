(** core.v — Shared syntactic core for the SPG model.

    Everything that is independent of the constraint domain lives here:
    identifier aliases, symbol/production types, the [verdict] trichotomy,
    and small list utilities used downstream by the generator. *)

From Stdlib Require Import List.
Import ListNotations.

(** ** Identifier aliases (kept abstract — backed by [nat] for now) *)

Definition nt_id := nat.
Definition tm_id := nat.

(** ** Grammar symbols and productions *)

Inductive symbol : Type :=
  | SymTm : tm_id -> symbol
  | SymNT : nt_id -> symbol.

Definition production := list symbol.

(** ** Verdict trichotomy (Definition 2.8 of the draft) *)

Inductive verdict : Type :=
  | Satisfied : verdict
  | Live      : verdict
  | Lost      : verdict.

(** ** Small utilities *)

(** Pair each element of [xs] with its 0-based index. *)
Fixpoint enum_with_index_aux {A : Type} (xs : list A) (i : nat)
  : list (nat * A) :=
  match xs with
  | []      => []
  | x :: ys => (i, x) :: enum_with_index_aux ys (S i)
  end.

Definition enum_with_index {A : Type} (xs : list A) : list (nat * A) :=
  enum_with_index_aux xs 0.

(** Trivial fact: indexing the empty list yields the empty list. *)
Lemma enum_with_index_nil : forall A, @enum_with_index A [] = [].
Proof. reflexivity. Qed.
