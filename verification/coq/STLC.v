Require Import Corelib.Init.Prelude.
Require Import Corelib.Lists.ListDef.
Require Import Corelib.Strings.PrimStringAxioms.
Require Import verification.coq.Common.
Open Scope list_scope.
Open Scope pstring_scope.

Module STLC.
Import Common.Common.

Inductive ty : Type :=
| TyBase : string -> ty
| TyArrow : ty -> ty -> ty. 

Inductive expr : Type :=
| EVar : string -> expr
| ELam : string -> ty -> expr -> expr
| EApp : expr -> expr -> expr.

Fixpoint ty_eqb (τ1 τ2 : ty) : bool :=
  match τ1, τ2 with
  | TyBase a, TyBase b => string_eqb a b
  | TyArrow a1 b1, TyArrow a2 b2 => ty_eqb a1 a2 && ty_eqb b1 b2
  | _, _ => false
  end.

Lemma ty_eqb_eq : forall τ1 τ2, ty_eqb τ1 τ2 = true -> τ1 = τ2.
Proof.
  induction τ1; destruct τ2; simpl; try discriminate.
  - intro H. apply string_eqb_eq in H. subst. reflexivity.
  - intros H.
    destruct (ty_eqb τ1_1 τ2_1) eqn:Ha; try discriminate.
    destruct (ty_eqb τ1_2 τ2_2) eqn:Hb; try discriminate.
    specialize (IHτ1_1 _ Ha). specialize (IHτ1_2 _ Hb). subst. reflexivity.
Qed.

Definition char_is_ident_tail (c : char63) : bool :=
  char_is_alnum c || char_eqb c 95%uint63.

Definition is_identifier (s : string) : bool :=
  match to_list s with
  | nil => false
  | c :: rest =>
      (char_is_lower c || char_is_upper c || char_eqb c 95%uint63)
        && list_forall char_is_ident_tail rest
  end.

Definition is_type_name (s : string) : bool :=
  match to_list s with
  | nil => false
  | _ => string_forall char_is_ident_tail s
  end.

Definition fuel_of_tokens (toks : list string) : nat := Datatypes.length toks * 4 + 16.

Fixpoint parse_type (fuel : nat) (toks : list string) {struct fuel}
  : option (ty * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_atomic_type fuel' toks with
      | Some (τ1, tok :: rest2) =>
          if string_eqb tok "->" then
            match parse_type fuel' rest2 with
            | Some (τ2, rest3) => Some (TyArrow τ1 τ2, rest3)
            | None => None
            end
          else
            Some (τ1, tok :: rest2)
      | Some (τ1, nil) => Some (τ1, nil)
      | None => None
      end
  end
with parse_atomic_type (fuel : nat) (toks : list string) {struct fuel}
  : option (ty * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | nil => None
      | tok :: rest =>
          if string_eqb tok "(" then
            match parse_type fuel' rest with
            | Some (τ, tok' :: rest2) =>
                if string_eqb tok' ")" then Some (τ, rest2) else None
            | _ => None
            end
          else if is_type_name tok then
            Some (TyBase tok, rest)
          else
            None
      end
  end.

Fixpoint parse_expr (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_atom fuel' toks with
      | Some (e, rest) => parse_app_tail fuel' e rest
      | None => None
      end
  end
with parse_atom (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | nil => None
      | tok :: rest =>
          if string_eqb tok "(" then
            match parse_expr fuel' rest with
            | Some (e, tok' :: rest2) =>
                if string_eqb tok' ")" then Some (e, rest2) else None
            | _ => None
            end
          else if string_eqb tok "λ" then
            match rest with
            | x :: colon :: rest2 =>
                if is_identifier x then
                  if string_eqb colon ":" then
                    match parse_type fuel' rest2 with
                    | Some (τ, dot :: rest3) =>
                        if string_eqb dot "." then
                          match parse_expr fuel' rest3 with
                          | Some (body, rest4) => Some (ELam x τ body, rest4)
                          | None => None
                          end
                        else None
                    | _ => None
                    end
                  else None
                else None
            | _ => None
            end
          else if is_identifier tok then
            Some (EVar tok, rest)
          else
            None
      end
  end
with parse_app_tail (fuel : nat) (lhs : expr) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => Some (lhs, toks)
  | S fuel' =>
      match toks with
      | nil => Some (lhs, nil)
      | tok :: rest =>
          if orb (string_eqb tok "(") (orb (string_eqb tok "λ") (is_identifier tok)) then
            match parse_atom fuel' (tok :: rest) with
            | Some (arg, rest1) => parse_app_tail fuel' (EApp lhs arg) rest1
            | None => None
            end
          else
            Some (lhs, tok :: rest)
      end
  end.

Definition parse (input : string) : option expr :=
  let toks := split_ws input in
  run_parser (parse_expr (fuel_of_tokens toks)) toks.

Fixpoint infer (Γ : env ty) (e : expr) : option ty :=
  match e with
  | EVar x => lookup x Γ
  | ELam x τ body =>
      match infer (extend Γ x τ) body with
      | Some τb => Some (TyArrow τ τb)
      | None => None
      end
  | EApp f a =>
      match infer Γ f, infer Γ a with
      | Some (TyArrow τarg τres), Some τa =>
          if ty_eqb τarg τa then Some τres else None
      | _, _ => None
      end
  end.

Inductive has_type : env ty -> expr -> ty -> Prop :=
| T_Var : forall Γ x τ,
    lookup x Γ = Some τ ->
    has_type Γ (EVar x) τ
| T_Lam : forall Γ x τ body τb,
    has_type (extend Γ x τ) body τb ->
    has_type Γ (ELam x τ body) (TyArrow τ τb)
| T_App : forall Γ f a τarg τres,
    has_type Γ f (TyArrow τarg τres) ->
    has_type Γ a τarg ->
    has_type Γ (EApp f a) τres.

Theorem infer_sound :
  forall Γ e τ,
    infer Γ e = Some τ ->
    has_type Γ e τ.
Proof.
  intros Γ e τ Hinfer.
  revert Γ τ Hinfer.
  induction e; intros Γ τ Hinfer; simpl in *.
  - constructor. exact Hinfer.
  - destruct (infer (extend Γ s t) e) eqn:Hbody; try discriminate.
    inversion Hinfer; subst. constructor. now apply IHe.
  - destruct (infer Γ e1) as [τf|] eqn:Hf; try discriminate.
    destruct τf as [basef | τarg τres]; try discriminate.
    destruct (infer Γ e2) as [τa|] eqn:Ha; try discriminate.
    destruct (ty_eqb τarg τa) eqn:Heq; inversion Hinfer; subst.
    apply ty_eqb_eq in Heq. subst.
    apply T_App with (τarg := τa).
    + apply IHe1. exact Hf.
    + apply IHe2. exact Ha.
Qed.

Definition typecheck (input : string) : option ty :=
  match parse input with
  | Some e => infer nil e
  | None => None
  end.

Theorem typecheck_sound :
  forall input τ,
    typecheck input = Some τ ->
    exists e, parse input = Some e /\ has_type nil e τ.
Proof.
  intros input τ Htc.
  unfold typecheck in Htc.
  destruct (parse input) as [e|] eqn:Hparse; try discriminate.
  exists e. split; [reflexivity|].
  now apply infer_sound in Htc.
Qed.

End STLC.
