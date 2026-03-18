Require Import Corelib.Init.Prelude.
Require Import Corelib.Lists.ListDef.
Require Import Corelib.Strings.PrimStringAxioms.
Require Import verification.coq.Common.
Open Scope list_scope.
Open Scope pstring_scope.

Module FunLang.
Import Common.Common.

Inductive ty : Type :=
| TyInt
| TyFloat
| TyBool
| TyName : string -> ty
| TyArrow : ty -> ty -> ty.

Inductive int_op : Type := IAdd | ISub | IMul | IDiv.
Inductive float_op : Type := FAdd | FSub | FMul | FDiv.

Inductive expr : Type :=
| EVar : string -> expr
| EInt : string -> expr
| EFloat : string -> expr
| EBool : bool -> expr
| ELam : string -> ty -> expr -> expr
| ELet : string -> ty -> expr -> expr -> expr
| EIntBin : int_op -> expr -> expr -> expr
| EFloatBin : float_op -> expr -> expr -> expr
| EApp : expr -> expr -> expr.

Fixpoint ty_eqb (τ1 τ2 : ty) : bool :=
  match τ1, τ2 with
  | TyInt, TyInt => true
  | TyFloat, TyFloat => true
  | TyBool, TyBool => true
  | TyName a, TyName b => string_eqb a b
  | TyArrow a1 b1, TyArrow a2 b2 => ty_eqb a1 a2 && ty_eqb b1 b2
  | _, _ => false
  end.

Lemma ty_eqb_eq : forall τ1 τ2, ty_eqb τ1 τ2 = true -> τ1 = τ2.
Proof.
  induction τ1; destruct τ2; simpl; try discriminate; auto.
  - intro H. apply string_eqb_eq in H. subst. reflexivity.
  - intro H.
    destruct (ty_eqb τ1_1 τ2_1) eqn:Ha; try discriminate.
    destruct (ty_eqb τ1_2 τ2_2) eqn:Hb; try discriminate.
    specialize (IHτ1_1 _ Ha). specialize (IHτ1_2 _ Hb). subst. reflexivity.
Qed.

Definition char_is_ident_tail (c : char63) : bool := char_is_lower c || char_is_digit c.

Definition is_identifier (s : string) : bool :=
  match to_list s with
  | nil => false
  | c :: rest => char_is_lower c && list_forall char_is_ident_tail rest
  end.

Definition is_type_name (s : string) : bool :=
  match to_list s with
  | nil => false
  | c :: rest => char_is_upper c && list_forall char_is_ident_tail rest
  end.

Definition is_integer_token (s : string) : bool :=
  match to_list s with
  | nil => false
  | _ => string_forall char_is_digit s
  end.

Fixpoint chars_have_single_dot (seen_dot : bool) (cs : list char63) : bool :=
  match cs with
  | nil => false
  | c :: rest =>
      if char_eqb c 46%uint63 then
        if seen_dot then false
        else match rest with
             | nil => false
             | _ => chars_have_single_dot true rest
             end
      else if char_is_digit c then
        match rest with
        | nil => seen_dot
        | _ => chars_have_single_dot seen_dot rest
        end
      else false
  end.

Definition is_float_token (s : string) : bool :=
  chars_have_single_dot false (to_list s).

Definition is_bool_token (s : string) : bool :=
  orb (string_eqb s "true") (string_eqb s "false").

Definition parse_int_op (tok : string) : option int_op :=
  if string_eqb tok "+" then Some IAdd
  else if string_eqb tok "-" then Some ISub
  else if string_eqb tok "*" then Some IMul
  else if string_eqb tok "/" then Some IDiv
  else None.

Definition parse_float_op (tok : string) : option float_op :=
  if string_eqb tok "+." then Some FAdd
  else if string_eqb tok "-." then Some FSub
  else if string_eqb tok "*." then Some FMul
  else if string_eqb tok "/." then Some FDiv
  else None.

Definition fuel_of_tokens (toks : list string) : nat := Datatypes.length toks * 6 + 24.

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
          else if string_eqb tok "Int" then Some (TyInt, rest)
          else if string_eqb tok "Float" then Some (TyFloat, rest)
          else if string_eqb tok "Bool" then Some (TyBool, rest)
          else if is_type_name tok then Some (TyName tok, rest)
          else None
      end
  end.

Fixpoint parse_expr (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: name :: colon :: rest =>
          if string_eqb tok "let" then
            if andb (is_identifier name) (string_eqb colon ":") then
              match parse_type fuel' rest with
              | Some (τ, eqtok :: rest1) =>
                  if string_eqb eqtok "=" then
                    match parse_expr fuel' rest1 with
                    | Some (value, semi :: rest2) =>
                        if string_eqb semi ";" then
                          match parse_expr fuel' rest2 with
                          | Some (body, rest3) => Some (ELet name τ value body, rest3)
                          | None => None
                          end
                        else None
                    | _ => None
                    end
                  else None
              | _ => None
              end
            else parse_bin fuel' toks
          else parse_bin fuel' toks
      | _ => parse_bin fuel' toks
      end
  end
with parse_bin (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_postfix fuel' toks with
      | Some (e, rest) => parse_bin_tail fuel' e rest
      | None => None
      end
  end
with parse_bin_tail (fuel : nat) (lhs : expr) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => Some (lhs, toks)
  | S fuel' =>
      match toks with
      | nil => Some (lhs, nil)
      | tok :: rest =>
          match parse_int_op tok with
          | Some op =>
              match parse_postfix fuel' rest with
              | Some (rhs, rest1) => parse_bin_tail fuel' (EIntBin op lhs rhs) rest1
              | None => None
              end
          | None =>
              match parse_float_op tok with
              | Some op =>
                  match parse_postfix fuel' rest with
                  | Some (rhs, rest1) => parse_bin_tail fuel' (EFloatBin op lhs rhs) rest1
                  | None => None
                  end
              | None => Some (lhs, tok :: rest)
              end
          end
      end
  end
with parse_postfix (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_atom fuel' toks with
      | Some (e, rest) => parse_postfix_tail fuel' e rest
      | None => None
      end
  end
with parse_postfix_tail (fuel : nat) (func : expr) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => Some (func, toks)
  | S fuel' =>
      match toks with
      | tok :: rest =>
          if string_eqb tok "(" then
            match parse_expr fuel' rest with
            | Some (arg, close :: rest1) =>
                if string_eqb close ")" then
                  parse_postfix_tail fuel' (EApp func arg) rest1
                else None
            | _ => None
            end
          else Some (func, toks)
      | _ => Some (func, toks)
      end
  end
with parse_atom (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: rest =>
          if string_eqb tok "(" then
            match rest with
            | name :: colon :: rest1 =>
                if andb (is_identifier name) (string_eqb colon ":") then
                  match parse_type fuel' rest1 with
                  | Some (τ, close :: arrow :: rest2) =>
                      if andb (string_eqb close ")") (string_eqb arrow "=>") then
                        match parse_expr fuel' rest2 with
                        | Some (body, rest3) => Some (ELam name τ body, rest3)
                        | None => None
                        end
                      else
                        match parse_expr fuel' rest with
                        | Some (e, close' :: rest3) =>
                            if string_eqb close' ")" then Some (e, rest3) else None
                        | _ => None
                        end
                  | _ =>
                      match parse_expr fuel' rest with
                      | Some (e, close' :: rest3) =>
                          if string_eqb close' ")" then Some (e, rest3) else None
                      | _ => None
                      end
                  end
                else
                  match parse_expr fuel' rest with
                  | Some (e, close' :: rest3) =>
                      if string_eqb close' ")" then Some (e, rest3) else None
                  | _ => None
                  end
            | _ =>
                match parse_expr fuel' rest with
                | Some (e, close' :: rest3) =>
                    if string_eqb close' ")" then Some (e, rest3) else None
                | _ => None
                end
            end
          else if is_float_token tok then Some (EFloat tok, rest)
          else if is_integer_token tok then Some (EInt tok, rest)
          else if string_eqb tok "true" then Some (EBool true, rest)
          else if string_eqb tok "false" then Some (EBool false, rest)
          else if is_identifier tok then Some (EVar tok, rest)
          else None
      | nil => None
      end
  end.

Definition parse (input : string) : option expr :=
  let toks := split_ws input in
  run_parser (parse_expr (fuel_of_tokens toks)) toks.

Fixpoint infer (Γ : env ty) (e : expr) : option ty :=
  match e with
  | EVar x => lookup x Γ
  | EInt _ => Some TyInt
  | EFloat _ => Some TyFloat
  | EBool _ => Some TyBool
  | ELam x τ body =>
      match infer (extend Γ x τ) body with
      | Some τb => Some (TyArrow τ τb)
      | None => None
      end
  | ELet x τ value body =>
      match infer Γ value with
      | Some τv =>
          if ty_eqb τ τv then infer (extend Γ x τ) body else None
      | None => None
      end
  | EIntBin _ l r =>
      match infer Γ l, infer Γ r with
      | Some TyInt, Some TyInt => Some TyInt
      | _, _ => None
      end
  | EFloatBin _ l r =>
      match infer Γ l, infer Γ r with
      | Some TyFloat, Some TyFloat => Some TyFloat
      | _, _ => None
      end
  | EApp f a =>
      match infer Γ f, infer Γ a with
      | Some (TyArrow τarg τres), Some τa =>
          if ty_eqb τarg τa then Some τres else None
      | _, _ => None
      end
  end.

Definition has_type (Γ : env ty) (e : expr) (τ : ty) : Prop :=
  infer Γ e = Some τ.

Theorem infer_sound :
  forall Γ e τ,
    infer Γ e = Some τ ->
    has_type Γ e τ.
Proof.
  intros Γ e τ Hinfer. exact Hinfer.
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

End FunLang.
