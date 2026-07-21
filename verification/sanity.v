(** sanity.v — proves the pipeline is wired up correctly.
    Delete or extend once real verification sources land. *)

From Stdlib Require Import List Arith.
Import ListNotations.

Lemma sanity_ok : 1 + 1 = 2.
Proof. reflexivity. Qed.
