Require Import Corelib.Init.Prelude.
Require Import Corelib.Lists.ListDef.
Require Import Corelib.Strings.PrimStringAxioms.
Open Scope list_scope.
Open Scope pstring_scope.

Module Common.

Definition env (A : Type) := list (string * A).

Definition char_eqb (c1 c2 : char63) : bool := eqb c1 c2.

Fixpoint list_eqb {A : Type} (eqbA : A -> A -> bool) (xs ys : list A) : bool :=
  match xs, ys with
  | nil, nil => true
  | x :: xs', y :: ys' => eqbA x y && list_eqb eqbA xs' ys'
  | _, _ => false
  end.

Definition string_eqb (s1 s2 : string) : bool :=
  list_eqb char_eqb (to_list s1) (to_list s2).

Lemma list_eqb_refl :
  forall (A : Type) (eqbA : A -> A -> bool),
    (forall x, eqbA x x = true) ->
    forall xs, list_eqb eqbA xs xs = true.
Proof.
  intros A eqbA Heqb.
  induction xs as [|x xs IH]; simpl; auto.
  rewrite Heqb, IH. reflexivity.
Qed.

Lemma list_eqb_eq :
  forall (A : Type) (eqbA : A -> A -> bool),
    (forall x y, eqbA x y = true -> x = y) ->
    forall xs ys, list_eqb eqbA xs ys = true -> xs = ys.
Proof.
  intros A eqbA Heqb.
  induction xs as [|x xs IH]; destruct ys as [|y ys]; simpl; try discriminate; auto.
  intro H.
  apply andb_prop in H as [Hxy Hrest].
  apply Heqb in Hxy.
  specialize (IH _ Hrest).
  subst. reflexivity.
Qed.

Lemma list_char_eqb_eq :
  forall xs ys,
    list_eqb char_eqb xs ys = true ->
    xs = ys.
Proof.
  induction xs as [|x xs IH]; destruct ys as [|y ys]; simpl; try discriminate; auto.
  intro H.
  apply andb_prop in H as [Hxy Hrest].
  apply eqb_correct in Hxy.
  specialize (IH _ Hrest).
  subst. reflexivity.
Qed.

Lemma string_eqb_refl :
  forall s, string_eqb s s = true.
Proof.
  intro s. unfold string_eqb. apply list_eqb_refl. apply eqb_refl.
Qed.

Lemma string_eqb_eq :
  forall s1 s2,
    string_eqb s1 s2 = true ->
    s1 = s2.
Proof.
  intros s1 s2 Heq.
  unfold string_eqb in Heq.
  apply list_char_eqb_eq in Heq.
  rewrite <- (of_to_list s1), <- (of_to_list s2).
  now f_equal.
Qed.

Lemma string_eqb_neq :
  forall s1 s2,
    string_eqb s1 s2 = false ->
    s1 <> s2.
Proof.
  intros s1 s2 Heq Heq'.
  subst. rewrite string_eqb_refl in Heq. discriminate.
Qed.

Fixpoint lookup {A : Type} (x : string) (Γ : env A) : option A :=
  match Γ with
  | nil => None
  | (y, v) :: Γ' => if string_eqb x y then Some v else lookup x Γ'
  end.

Definition extend {A : Type} (Γ : env A) (x : string) (v : A) : env A :=
  (x, v) :: Γ.

Lemma lookup_extend_eq :
  forall (A : Type) (Γ : env A) x v,
    lookup x (extend Γ x v) = Some v.
Proof.
  intros. unfold lookup, extend. now rewrite string_eqb_refl.
Qed.

Lemma lookup_extend_neq :
  forall (A : Type) (Γ : env A) x y v,
    x <> y ->
    lookup x (extend Γ y v) = lookup x Γ.
Proof.
  intros A Γ x y v Hxy.
  unfold extend, lookup.
  destruct (string_eqb x y) eqn:Heq.
  - apply string_eqb_eq in Heq. contradiction.
  - reflexivity.
Qed.

Lemma lookup_extend_cases :
  forall (A : Type) (Γ : env A) x y v,
    x = y /\ lookup x (extend Γ y v) = Some v \/
    x <> y /\ lookup x (extend Γ y v) = lookup x Γ.
Proof.
  intros.
  destruct (string_eqb x y) eqn:Heq.
  - left. split.
    + apply string_eqb_eq in Heq. exact Heq.
    + unfold extend, lookup. rewrite Heq. reflexivity.
  - right. split.
    + intro H. subst. rewrite string_eqb_refl in Heq. discriminate.
    + unfold extend, lookup. rewrite Heq. reflexivity.
Qed.

Definition char_between (lo hi c : char63) : bool :=
  leb lo c && leb c hi.

Definition char_is_lower (c : char63) : bool := char_between 97%uint63 122%uint63 c.
Definition char_is_upper (c : char63) : bool := char_between 65%uint63 90%uint63 c.
Definition char_is_digit (c : char63) : bool := char_between 48%uint63 57%uint63 c.
Definition char_is_alnum (c : char63) : bool :=
  char_is_lower c || char_is_upper c || char_is_digit c.

Definition char_is_space (c : char63) : bool :=
  char_eqb c 32%uint63
    || char_eqb c 9%uint63
    || char_eqb c 10%uint63
    || char_eqb c 13%uint63.

Fixpoint list_forall {A : Type} (p : A -> bool) (xs : list A) : bool :=
  match xs with
  | nil => true
  | x :: xs' => p x && list_forall p xs'
  end.

Fixpoint list_exists {A : Type} (p : A -> bool) (xs : list A) : bool :=
  match xs with
  | nil => false
  | x :: xs' => p x || list_exists p xs'
  end.

Definition string_forall (p : char63 -> bool) (s : string) : bool :=
  list_forall p (to_list s).

Definition string_exists (p : char63 -> bool) (s : string) : bool :=
  list_exists p (to_list s).

Lemma orb_true_elim : forall a b : bool, (a || b)%bool = true -> a = true \/ b = true.
Proof. destruct a, b; simpl; auto. Qed.

Lemma list_forall_sound :
  forall A (p : A -> bool) xs,
    list_forall p xs = true -> Forall (fun x => p x = true) xs.
Proof.
  intros A p xs. induction xs as [|x xs IH]; simpl; intro H.
  - constructor.
  - apply andb_prop in H as [Hx Hxs].
    constructor; [exact Hx | apply IH; exact Hxs].
Qed.

Lemma list_forall_complete :
  forall A (p : A -> bool) xs,
    Forall (fun x => p x = true) xs -> list_forall p xs = true.
Proof.
  intros A p xs. induction xs as [|x xs IH]; simpl; intro H.
  - reflexivity.
  - inversion H; subst.
    rewrite H2. simpl. apply IH in H3. rewrite H3. reflexivity.
Qed.

Fixpoint rev_append {A : Type} (xs ys : list A) : list A :=
  match xs with
  | nil => ys
  | x :: xs' => rev_append xs' (x :: ys)
  end.

Definition rev {A : Type} (xs : list A) : list A := rev_append xs nil.

Definition rev_chars_to_string (rev_chars : list char63) : string :=
  of_list (rev rev_chars).

Fixpoint split_ws_chars_aux
    (input : list char63)
    (current_rev : list char63)
    (acc_rev : list string) : list string :=
  match input with
  | nil =>
      let acc_rev' :=
        match current_rev with
        | nil => acc_rev
        | _ => rev_chars_to_string current_rev :: acc_rev
        end in
      rev acc_rev'
  | c :: rest =>
      if char_is_space c then
        let acc_rev' :=
          match current_rev with
          | nil => acc_rev
          | _ => rev_chars_to_string current_rev :: acc_rev
          end in
        split_ws_chars_aux rest nil acc_rev'
      else
        split_ws_chars_aux rest (c :: current_rev) acc_rev
  end.

Definition split_ws (input : string) : list string := split_ws_chars_aux (to_list input) nil nil.

Definition parser (A : Type) := list string -> option (A * list string).

Definition parse_fail {A : Type} : parser A := fun _ => None.

Definition parse_ret {A : Type} (x : A) : parser A := fun toks => Some (x, toks).

Definition parse_bind {A B : Type} (p : parser A) (f : A -> parser B) : parser B :=
  fun toks =>
    match p toks with
    | Some (x, rest) => f x rest
    | None => None
    end.

Definition parse_map {A B : Type} (p : parser A) (f : A -> B) : parser B :=
  parse_bind p (fun x => parse_ret (f x)).

Definition expect_token (tok : string) : parser unit :=
  fun toks =>
    match toks with
    | t :: rest => if string_eqb t tok then Some (tt, rest) else None
    | nil => None
    end.

Definition satisfy_token (p : string -> bool) : parser string :=
  fun toks =>
    match toks with
    | t :: rest => if p t then Some (t, rest) else None
    | nil => None
    end.

Definition run_parser {A : Type} (p : parser A) (toks : list string) : option A :=
  match p toks with
  | Some (x, nil) => Some x
  | _ => None
  end.

End Common.
