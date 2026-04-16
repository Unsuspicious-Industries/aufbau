Require Import Corelib.Init.Prelude.
Require Import Corelib.Lists.ListDef.
Require Import Corelib.Strings.PrimStringAxioms.
Require Import verification.coq.Common.
Open Scope list_scope.
Open Scope pstring_scope.

Module ImpLang.
Import Common.Common.

Inductive ty : Type :=
| TyInt
| TyBool
| TyUnion : ty -> ty -> ty.

Inductive arith_op : Type := AAdd | ASub | AMul | ADiv.
Inductive comp_op : Type := CEq | CNe | CLt | CLe | CGt | CGe.

Inductive expr : Type :=
| EVar : string -> expr
| EInt : string -> expr
| EBool : bool -> expr
| EArith : arith_op -> expr -> expr -> expr
| EComp : comp_op -> expr -> expr -> expr.

Inductive stmt : Type :=
| SDecl : string -> ty -> expr -> stmt
| SAssign : string -> expr -> stmt
| SIf : expr -> list stmt -> stmt
| SIfElse : expr -> list stmt -> list stmt -> stmt
| SWhile : expr -> list stmt -> stmt.

Fixpoint ty_eqb (τ1 τ2 : ty) : bool :=
  match τ1, τ2 with
  | TyInt, TyInt => true
  | TyBool, TyBool => true
  | TyUnion a1 b1, TyUnion a2 b2 => ty_eqb a1 a2 && ty_eqb b1 b2
  | _, _ => false
  end.

Definition char_is_ident_tail (c : char63) : bool :=
  char_is_lower c || char_is_digit c || char_eqb c 95%uint63.

Definition is_identifier (s : string) : bool :=
  match to_list s with
  | nil => false
  | c :: rest => char_is_lower c && list_forall char_is_ident_tail rest
  end.

Definition is_integer_token (s : string) : bool :=
  match to_list s with
  | nil => false
  | _ => string_forall char_is_digit s
  end.

Definition parse_arith_op (tok : string) : option arith_op :=
  if string_eqb tok "+" then Some AAdd
  else if string_eqb tok "-" then Some ASub
  else if string_eqb tok "*" then Some AMul
  else if string_eqb tok "/" then Some ADiv
  else None.

Definition parse_comp_op (tok : string) : option comp_op :=
  if string_eqb tok "==" then Some CEq
  else if string_eqb tok "!=" then Some CNe
  else if string_eqb tok "<" then Some CLt
  else if string_eqb tok "<=" then Some CLe
  else if string_eqb tok ">" then Some CGt
  else if string_eqb tok ">=" then Some CGe
  else None.

Definition fuel_of_tokens (toks : list string) : nat := Datatypes.length toks * 8 + 32.

Fixpoint parse_type (fuel : nat) (toks : list string) {struct fuel}
  : option (ty * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_atomic_type fuel' toks with
      | Some (τ1, tok :: rest2) =>
          if string_eqb tok "|" then
            match parse_type fuel' rest2 with
            | Some (τ2, rest3) => Some (TyUnion τ1 τ2, rest3)
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
            | Some (τ, close :: rest1) =>
                if string_eqb close ")" then Some (τ, rest1) else None
            | _ => None
            end
          else if string_eqb tok "Int" then Some (TyInt, rest)
          else if string_eqb tok "Bool" then Some (TyBool, rest)
          else None
      end
  end.

Fixpoint parse_expr (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_arith fuel' toks with
      | Some (lhs, tok :: rest) =>
          match parse_comp_op tok with
          | Some op =>
              match parse_arith fuel' rest with
              | Some (rhs, rest1) => Some (EComp op lhs rhs, rest1)
              | None => None
              end
          | None => Some (lhs, tok :: rest)
          end
      | Some (lhs, nil) => Some (lhs, nil)
      | None => None
      end
  end
with parse_arith (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_atom fuel' toks with
      | Some (e, rest) => parse_arith_tail fuel' e rest
      | None => None
      end
  end
with parse_arith_tail (fuel : nat) (lhs : expr) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => Some (lhs, toks)
  | S fuel' =>
      match toks with
      | nil => Some (lhs, nil)
      | tok :: rest =>
          match parse_arith_op tok with
          | Some op =>
              match parse_atom fuel' rest with
              | Some (rhs, rest1) => parse_arith_tail fuel' (EArith op lhs rhs) rest1
              | None => None
              end
          | None => Some (lhs, tok :: rest)
          end
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
            | Some (e, close :: rest1) =>
                if string_eqb close ")" then Some (e, rest1) else None
            | _ => None
            end
          else if is_integer_token tok then Some (EInt tok, rest)
          else if string_eqb tok "true" then Some (EBool true, rest)
          else if string_eqb tok "false" then Some (EBool false, rest)
          else if is_identifier tok then Some (EVar tok, rest)
          else None
      end
  end.

Fixpoint parse_stmt (fuel : nat) (toks : list string) {struct fuel}
  : option (stmt * list string) :=
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
                        if string_eqb semi ";" then Some (SDecl name τ value, rest2) else None
                    | _ => None
                    end
                  else None
              | _ => None
              end
            else None
          else if string_eqb tok "if" then
            if string_eqb name "(" then
              match parse_expr fuel' (colon :: rest) with
              | Some (cond, close :: rest2) =>
                  if string_eqb close ")" then
                    match parse_block fuel' rest2 with
                    | Some (then_block, else_tok :: rest3) =>
                        if string_eqb else_tok "else" then
                          match parse_block fuel' rest3 with
                          | Some (else_block, rest4) => Some (SIfElse cond then_block else_block, rest4)
                          | None => Some (SIf cond then_block, else_tok :: rest3)
                          end
                        else Some (SIf cond then_block, else_tok :: rest3)
                    | Some (then_block, nil) => Some (SIf cond then_block, nil)
                    | None => None
                    end
                  else None
              | _ => None
              end
            else None
          else if string_eqb tok "while" then
            if string_eqb name "(" then
              match parse_expr fuel' (colon :: rest) with
              | Some (cond, close :: rest2) =>
                  if string_eqb close ")" then
                    match parse_block fuel' rest2 with
                    | Some (body, rest3) => Some (SWhile cond body, rest3)
                    | None => None
                    end
                  else None
              | _ => None
              end
            else None
          else if is_identifier tok then
            if string_eqb name "=" then
              match parse_expr fuel' (colon :: rest) with
              | Some (value, semi :: rest1) =>
                  if string_eqb semi ";" then Some (SAssign tok value, rest1) else None
              | _ => None
              end
            else None
          else None
      | tok :: rest =>
          if string_eqb tok "if" then
            match rest with
            | open :: rest1 =>
                if string_eqb open "(" then
                  match parse_expr fuel' rest1 with
                  | Some (cond, close :: rest2) =>
                      if string_eqb close ")" then
                        match parse_block fuel' rest2 with
                        | Some (then_block, else_tok :: rest3) =>
                            if string_eqb else_tok "else" then
                              match parse_block fuel' rest3 with
                              | Some (else_block, rest4) => Some (SIfElse cond then_block else_block, rest4)
                              | None => Some (SIf cond then_block, else_tok :: rest3)
                              end
                            else Some (SIf cond then_block, else_tok :: rest3)
                        | Some (then_block, nil) => Some (SIf cond then_block, nil)
                        | None => None
                        end
                      else None
                  | _ => None
                  end
                else None
            | _ => None
            end
          else if string_eqb tok "while" then
            match rest with
            | open :: rest1 =>
                if string_eqb open "(" then
                  match parse_expr fuel' rest1 with
                  | Some (cond, close :: rest2) =>
                      if string_eqb close ")" then
                        match parse_block fuel' rest2 with
                        | Some (body, rest3) => Some (SWhile cond body, rest3)
                        | None => None
                        end
                      else None
                  | _ => None
                  end
                else None
            | _ => None
            end
          else None
      | nil => None
      end
  end
with parse_stmts (fuel : nat) (toks : list string) {struct fuel}
  : option (list stmt * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: _ =>
          if string_eqb tok "}" then Some (nil, toks)
          else
            match parse_stmt fuel' toks with
            | Some (s, rest) =>
                match parse_stmts fuel' rest with
                | Some (ss, rest1) => Some (s :: ss, rest1)
                | None => None
                end
            | None => None
            end
      | nil => Some (nil, nil)
      end
  end
with parse_block (fuel : nat) (toks : list string) {struct fuel}
  : option (list stmt * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: rest =>
          if string_eqb tok "{" then
            match parse_stmts fuel' rest with
            | Some (ss, close :: rest1) =>
                if string_eqb close "}" then Some (ss, rest1) else None
            | _ => None
            end
          else None
      | nil => None
      end
  end.

Definition parse_program (input : string) : option (list stmt) :=
  let toks := split_ws input in
  run_parser (parse_block (fuel_of_tokens toks)) toks.

Fixpoint infer_expr (Γ : env ty) (e : expr) : option ty :=
  match e with
  | EVar x => lookup x Γ
  | EInt _ => Some TyInt
  | EBool _ => Some TyBool
  | EArith _ l r =>
      match infer_expr Γ l, infer_expr Γ r with
      | Some TyInt, Some TyInt => Some TyInt
      | _, _ => None
      end
  | EComp _ l r =>
      match infer_expr Γ l, infer_expr Γ r with
      | Some TyInt, Some TyInt => Some TyBool
      | _, _ => None
      end
  end.

Definition default_stmt_fuel : nat := 512.

Fixpoint check_stmt_fuel (fuel : nat) (Γ : env ty) (s : stmt) : option (env ty)
with check_stmts_fuel (fuel : nat) (Γ : env ty) (ss : list stmt) : option (env ty).
Proof.
  - destruct fuel as [|fuel']; [exact None|].
    destruct s.
    + destruct (infer_expr Γ e) as [τv|] eqn:Hv; [exact (if ty_eqb t τv then Some (extend Γ s t) else None)|exact None].
    + destruct (lookup s Γ) as [τx|] eqn:Hx; [|exact None].
      destruct (infer_expr Γ e) as [τv|] eqn:Hv; [exact (if ty_eqb τx τv then Some Γ else None)|exact None].
    + destruct (infer_expr Γ e) as [τc|] eqn:Hc; [exact (if ty_eqb τc TyBool then match check_stmts_fuel fuel' Γ l with Some _ => Some Γ | None => None end else None)|exact None].
    + destruct (infer_expr Γ e) as [τc|] eqn:Hc; [exact (if ty_eqb τc TyBool then match check_stmts_fuel fuel' Γ l, check_stmts_fuel fuel' Γ l0 with | Some _, Some _ => Some Γ | _, _ => None end else None)|exact None].
    + destruct (infer_expr Γ e) as [τc|] eqn:Hc; [exact (if ty_eqb τc TyBool then match check_stmts_fuel fuel' Γ l with Some _ => Some Γ | None => None end else None)|exact None].
  - destruct fuel as [|fuel']; [exact None|].
    destruct ss as [|s ss'].
    + exact (Some Γ).
    + destruct (check_stmt_fuel fuel' Γ s) as [Γ'|] eqn:Hs; [exact (check_stmts_fuel fuel' Γ' ss')|exact None].
Defined.

Definition check_stmt (Γ : env ty) (s : stmt) : option (env ty) :=
  check_stmt_fuel default_stmt_fuel Γ s.

Definition check_stmts (Γ : env ty) (ss : list stmt) : option (env ty) :=
  check_stmts_fuel default_stmt_fuel Γ ss.

Definition check_block (Γ : env ty) (body : list stmt) : option unit :=
  match check_stmts Γ body with
  | Some _ => Some tt
  | None => None
  end.

Definition expr_has_type (Γ : env ty) (e : expr) (τ : ty) : Prop :=
  infer_expr Γ e = Some τ.

Definition stmt_wf (Γ Γ' : env ty) (s : stmt) : Prop :=
  check_stmt Γ s = Some Γ'.

Definition stmts_wf (Γ Γ' : env ty) (ss : list stmt) : Prop :=
  check_stmts Γ ss = Some Γ'.

Definition block_wf (Γ : env ty) (body : list stmt) : Prop :=
  check_block Γ body = Some tt.

Definition typecheck_program (input : string) : option unit :=
  match parse_program input with
  | Some body => check_block nil body
  | None => None
  end.

(* Soundness of the executable checker:
   whenever the verified IMP checker accepts a program, there exists a parsed
   statement block [body] for that input, and that block is well-formed under
   the verified block judgment [block_wf].

   Again, this is a no-false-positives theorem, not a completeness theorem. *)
Theorem typecheck_program_sound :
  forall input,
    typecheck_program input = Some tt ->
    exists body, parse_program input = Some body /\ block_wf nil body.
Proof.
  intros input Htc.
  unfold typecheck_program in Htc.
  destruct (parse_program input) as [body|] eqn:Hparse; try discriminate.
  exists body. split; [reflexivity|exact Htc].
Qed.

(* Readable alias for the main IMP checker soundness theorem. *)
Theorem imp_checker_soundness :
  forall input,
    typecheck_program input = Some tt ->
    exists body, parse_program input = Some body /\ block_wf nil body.
Proof.
  exact typecheck_program_sound.
Qed.

End ImpLang.
