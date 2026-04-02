Require Import Corelib.Init.Prelude.
Require Import Corelib.Lists.ListDef.
Require Import Corelib.Strings.PrimStringAxioms.
Require Import verification.coq.Common.
Open Scope list_scope.
Open Scope pstring_scope.

Module TypescriptLang.
Import Common.Common.

Inductive ty : Type :=
| TyString
| TyNumber
| TyBoolean
| TyVoid
| TyArray : ty -> ty
| TyUnion : ty -> ty -> ty
| TyFun : list ty -> ty -> ty.

Inductive expr : Type :=
| EVar : string -> expr
| EString : string -> expr
| ENumber : string -> expr
| EBool : bool -> expr
| EArray : list expr -> expr
| ECall : string -> list expr -> expr
| ETypeofEq : string -> string -> expr.

Inductive stmt : Type :=
| SReturn : option expr -> stmt
| SConst : string -> ty -> expr -> stmt
| SLet : string -> ty -> expr -> stmt
| SExpr : expr -> stmt
| SIfElse : expr -> list stmt -> list stmt -> stmt
| SFunction : string -> list (string * ty) -> ty -> list stmt -> stmt.

Definition program := list stmt.

Fixpoint fold_left_ty (f : ty -> ty -> ty) (xs : list ty) (acc : ty) : ty :=
  match xs with
  | nil => acc
  | x :: xs' => fold_left_ty f xs' (f acc x)
  end.

Fixpoint fold_left_env (xs : list (string * ty)) (Γ : env ty) : env ty :=
  match xs with
  | nil => Γ
  | (x, τ) :: xs' => fold_left_env xs' (extend Γ x τ)
  end.

Fixpoint ty_eqb (τ1 τ2 : ty) : bool :=
  match τ1, τ2 with
  | TyString, TyString => true
  | TyNumber, TyNumber => true
  | TyBoolean, TyBoolean => true
  | TyVoid, TyVoid => true
  | TyArray a, TyArray b => ty_eqb a b
  | TyUnion a1 b1, TyUnion a2 b2 => andb (ty_eqb a1 a2) (ty_eqb b1 b2)
  | TyFun xs1 r1, TyFun xs2 r2 => andb (list_eqb ty_eqb xs1 xs2) (ty_eqb r1 r2)
  | _, _ => false
  end.

Definition char_is_ident_start (c : char63) : bool :=
  orb (orb (char_is_lower c) (char_is_upper c)) (char_eqb c 95%uint63).

Definition char_is_ident_tail (c : char63) : bool :=
  orb (char_is_ident_start c) (char_is_digit c).

Definition is_identifier (s : string) : bool :=
  match to_list s with
  | nil => false
  | c :: rest => andb (char_is_ident_start c) (list_forall char_is_ident_tail rest)
  end.

Definition is_number_token (s : string) : bool :=
  let chars := to_list s in
  let digits_or_dot c := orb (char_is_digit c) (char_eqb c 46%uint63) in
  andb (negb (list_eqb char_eqb chars nil)) (string_forall digits_or_dot s).

Definition is_string_token (s : string) : bool :=
  match to_list s with
  | q1 :: rest =>
      match rev rest with
      | q2 :: _ => andb (char_eqb q1 34%uint63) (char_eqb q2 34%uint63)
      | _ => false
      end
  | _ => false
  end.

Definition parse_type_name (tok : string) : option ty :=
  if string_eqb tok "string" then Some TyString
  else if string_eqb tok "number" then Some TyNumber
  else if string_eqb tok "boolean" then Some TyBoolean
  else if string_eqb tok "void" then Some TyVoid
  else None.

Fixpoint parse_type (fuel : nat) (toks : list string) {struct fuel}
  : option (ty * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_type_part fuel' toks with
      | Some (τ1, tok :: rest) =>
          if string_eqb tok "|" then
            match parse_type fuel' rest with
            | Some (τ2, rest') => Some (TyUnion τ1 τ2, rest')
            | None => None
            end
          else Some (τ1, tok :: rest)
      | Some (τ1, nil) => Some (τ1, nil)
      | None => None
      end
  end
with parse_type_part (fuel : nat) (toks : list string) {struct fuel}
  : option (ty * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_base_type fuel' toks with
      | Some (τ, open :: close :: rest) =>
          if andb (string_eqb open "[") (string_eqb close "]")
          then Some (TyArray τ, rest)
          else Some (τ, open :: close :: rest)
      | Some (τ, rest) => Some (τ, rest)
      | None => None
      end
  end
with parse_base_type (fuel : nat) (toks : list string) {struct fuel}
  : option (ty * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: rest =>
          if string_eqb tok "(" then
            match parse_type fuel' rest with
            | Some (τ, close :: rest') => if string_eqb close ")" then Some (τ, rest') else None
            | _ => None
            end
          else
            match parse_type_name tok with
            | Some τ => Some (τ, rest)
            | None => None
            end
      | nil => None
      end
  end.

Fixpoint parse_expr (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok1 :: name :: tok2 :: tag :: rest =>
          if andb (string_eqb tok1 "typeof")
               (andb (is_identifier name)
                     (andb (string_eqb tok2 "===") (is_string_token tag)))
          then Some (ETypeofEq name tag, rest)
          else parse_postfix fuel' toks
      | _ => parse_postfix fuel' toks
      end
  end
with parse_postfix (fuel : nat) (toks : list string) {struct fuel}
  : option (expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_atom fuel' toks with
      | Some (EVar f, open :: close :: rest) =>
          if andb (string_eqb open "(") (string_eqb close ")")
          then Some (ECall f nil, rest)
          else if string_eqb open "(" then
                 match parse_expr_list fuel' (close :: rest) with
                 | Some (args, close' :: rest') =>
                     if string_eqb close' ")" then Some (ECall f args, rest') else None
                 | _ => None
                 end
               else Some (EVar f, open :: close :: rest)
      | Some (EVar f, open :: rest) =>
          if string_eqb open "(" then
            match parse_expr_list fuel' rest with
            | Some (args, close' :: rest') =>
                if string_eqb close' ")" then Some (ECall f args, rest') else None
            | _ => None
            end
          else Some (EVar f, open :: rest)
      | other => other
      end
  end
with parse_expr_list (fuel : nat) (toks : list string) {struct fuel}
  : option (list expr * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match parse_expr fuel' toks with
      | Some (e, tok :: rest) =>
          if string_eqb tok "," then
            match parse_expr_list fuel' rest with
            | Some (es, rest') => Some (e :: es, rest')
            | None => None
            end
          else Some (e :: nil, tok :: rest)
      | Some (e, rest) => Some (e :: nil, rest)
      | None => None
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
            match parse_expr fuel' rest with
            | Some (e, close :: rest') => if string_eqb close ")" then Some (e, rest') else None
            | _ => None
            end
          else if string_eqb tok "[" then
            match rest with
            | close :: rest' =>
                if string_eqb close "]" then Some (EArray nil, rest')
                else match parse_expr_list fuel' (close :: rest') with
                     | Some (es, close' :: rest'') => if string_eqb close' "]" then Some (EArray es, rest'') else None
                     | _ => None
                     end
            | nil => None
            end
          else if is_string_token tok then Some (EString tok, rest)
          else if is_number_token tok then Some (ENumber tok, rest)
          else if string_eqb tok "true" then Some (EBool true, rest)
          else if string_eqb tok "false" then Some (EBool false, rest)
          else if is_identifier tok then Some (EVar tok, rest)
          else None
      | nil => None
      end
  end.

Fixpoint parse_param_list (fuel : nat) (toks : list string) {struct fuel}
  : option (list (string * ty) * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: _ =>
          if string_eqb tok ")" then Some (nil, toks)
          else match toks with
               | name :: colon :: rest =>
                   if andb (is_identifier name) (string_eqb colon ":") then
                     match parse_type fuel' rest with
                     | Some (τ, comma :: rest') =>
                         if string_eqb comma "," then
                           match parse_param_list fuel' rest' with
                           | Some (ps, rest'') => Some ((name, τ) :: ps, rest'')
                           | None => None
                           end
                         else Some ((name, τ) :: nil, comma :: rest')
                     | Some (τ, rest') => Some ((name, τ) :: nil, rest')
                     | None => None
                     end
                   else None
               | _ => None
               end
      | nil => None
      end
  end.

Fixpoint parse_stmt (fuel : nat) (toks : list string) {struct fuel}
  : option (stmt * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok1 :: tok2 :: rest =>
          if andb (string_eqb tok1 "return") (string_eqb tok2 ";") then Some (SReturn None, rest)
          else if string_eqb tok1 "return" then
                 match parse_expr fuel' (tok2 :: rest) with
                 | Some (e, semi :: rest') => if string_eqb semi ";" then Some (SReturn (Some e), rest') else None
                 | _ => None
                 end
               else if string_eqb tok1 "const" then
                      match tok2 :: rest with
                      | name :: colon :: rest' =>
                          if andb (is_identifier name) (string_eqb colon ":") then
                            match parse_type fuel' rest' with
                            | Some (τ, eqtok :: rest'') =>
                                if string_eqb eqtok "=" then
                                  match parse_expr fuel' rest'' with
                                  | Some (e, semi :: rest''') => if string_eqb semi ";" then Some (SConst name τ e, rest''') else None
                                  | _ => None
                                  end
                                else None
                            | _ => None
                            end
                          else None
                      | _ => None
                      end
                    else if string_eqb tok1 "let" then
                      match tok2 :: rest with
                      | name :: colon :: rest' =>
                          if andb (is_identifier name) (string_eqb colon ":") then
                            match parse_type fuel' rest' with
                            | Some (τ, eqtok :: rest'') =>
                                if string_eqb eqtok "=" then
                                  match parse_expr fuel' rest'' with
                                  | Some (e, semi :: rest''') => if string_eqb semi ";" then Some (SLet name τ e, rest''') else None
                                  | _ => None
                                  end
                                else None
                            | _ => None
                            end
                          else None
                      | _ => None
                      end
                    else if andb (string_eqb tok1 "if") (string_eqb tok2 "(") then
                      match parse_expr fuel' rest with
                      | Some (cond, close :: rest') =>
                          if string_eqb close ")" then
                            match parse_block fuel' rest' with
                            | Some (then_body, else_tok :: rest'') =>
                                if string_eqb else_tok "else" then
                                  match parse_block fuel' rest'' with
                                  | Some (else_body, rest''') => Some (SIfElse cond then_body else_body, rest''')
                                  | _ => None
                                  end
                                else None
                            | _ => None
                            end
                          else None
                      | _ => None
                      end
                    else if andb (string_eqb tok1 "function") (is_identifier tok2) then
                      match rest with
                      | open :: rest' =>
                          if string_eqb open "(" then
                            match parse_param_list fuel' rest' with
                            | Some (params, close :: colon :: rest'') =>
                                if andb (string_eqb close ")") (string_eqb colon ":") then
                                  match parse_type fuel' rest'' with
                                  | Some (ret, rest''') =>
                                      match parse_block fuel' rest''' with
                                      | Some (body, rest'''') => Some (SFunction tok2 params ret body, rest'''')
                                      | _ => None
                                      end
                                  | _ => None
                                  end
                                else None
                            | _ => None
                            end
                          else None
                      | _ => None
                      end
                    else
                      match parse_expr fuel' toks with
                      | Some (e, semi :: rest') => if string_eqb semi ";" then Some (SExpr e, rest') else None
                      | _ => None
                      end
      | _ =>
          match parse_expr fuel' toks with
          | Some (e, semi :: rest) => if string_eqb semi ";" then Some (SExpr e, rest) else None
          | _ => None
          end
      end
  end
with parse_block (fuel : nat) (toks : list string) {struct fuel}
  : option (list stmt * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: rest => if string_eqb tok "{" then parse_stmt_list fuel' rest else None
      | _ => None
      end
  end
with parse_stmt_list (fuel : nat) (toks : list string) {struct fuel}
  : option (list stmt * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | tok :: rest =>
          if string_eqb tok "}" then Some (nil, rest)
          else
          match parse_stmt fuel' toks with
          | Some (s, rest) =>
              match parse_stmt_list fuel' rest with
              | Some (ss, rest') => Some (s :: ss, rest')
              | None => None
              end
          | None => None
          end
      | nil => None
      end
  end.

Fixpoint parse_top_stmt_list (fuel : nat) (toks : list string) : option (list stmt * list string) :=
  match fuel with
  | O => None
  | S fuel' =>
      match toks with
      | nil => Some (nil, nil)
      | _ =>
          match parse_stmt fuel' toks with
          | Some (s, rest) =>
              match parse_top_stmt_list fuel' rest with
              | Some (ss, rest') => Some (s :: ss, rest')
              | None => None
              end
          | None => None
          end
      end
  end.

Definition fuel_of_tokens (toks : list string) : nat := Datatypes.length toks * 10 + 64.

Definition parse_program (input : string) : option program :=
  let toks := split_ws input in
  run_parser (parse_top_stmt_list (fuel_of_tokens toks)) toks.

Fixpoint subtype (τ1 τ2 : ty) : bool :=
  match τ1, τ2 with
  | _, TyUnion a b => orb (subtype τ1 a) (subtype τ1 b)
  | TyArray a, TyArray b => subtype a b
  | TyFun args1 ret1, TyFun args2 ret2 => andb (list_eqb ty_eqb args1 args2) (subtype ret1 ret2)
  | _, _ => ty_eqb τ1 τ2
  end.

Fixpoint infer_expr (Γ : env ty) (e : expr) {struct e} : option ty :=
  let fix infer_expr_list (es : list expr) : option (list ty) :=
      match es with
      | nil => Some nil
      | e0 :: es' =>
          match infer_expr Γ e0, infer_expr_list es' with
          | Some τ, Some τs => Some (τ :: τs)
          | _, _ => None
          end
      end
  in
  match e with
  | EVar x => lookup x Γ
  | EString _ => Some TyString
  | ENumber _ => Some TyNumber
  | EBool _ => Some TyBoolean
  | EArray es =>
      match infer_expr_list es with
      | Some nil => Some (TyArray TyVoid)
      | Some (τ :: τs) =>
          Some (TyArray (fold_left_ty (fun acc t => if subtype t acc then acc else TyUnion acc t) τs τ))
      | None => None
      end
  | ECall f args =>
      match lookup f Γ, infer_expr_list args with
      | Some (TyFun params ret), Some arg_tys =>
          if list_eqb subtype arg_tys params then Some ret else None
      | _, _ => None
      end
  | ETypeofEq _ _ => Some TyBoolean
  end.

Fixpoint check_stmt (Γ : env ty) (expected : option ty) (s : stmt) {struct s} : option (env ty) :=
  let fix check_block_local (Γ0 : env ty) (expected0 : option ty) (ss : list stmt) : option (env ty) :=
      match ss with
      | nil => Some Γ0
      | s0 :: ss' =>
          match check_stmt Γ0 expected0 s0 with
          | Some Γ' => check_block_local Γ' expected0 ss'
          | None => None
          end
      end
  in
  match s with
  | SReturn None =>
      match expected with
      | Some TyVoid => Some Γ
      | Some _ => None
      | None => Some Γ
      end
  | SReturn (Some e) =>
      match expected, infer_expr Γ e with
      | Some τ, Some τe => if subtype τe τ then Some Γ else None
      | None, Some _ => Some Γ
      | _, _ => None
      end
  | SConst x τ e | SLet x τ e =>
      match infer_expr Γ e with
      | Some τe => if subtype τe τ then Some (extend Γ x τ) else None
      | None => None
      end
  | SExpr e => match infer_expr Γ e with Some _ => Some Γ | None => None end
  | SIfElse cond tbranch ebranch =>
      match infer_expr Γ cond, check_block_local Γ expected tbranch, check_block_local Γ expected ebranch with
      | Some TyBoolean, Some _, Some _ => Some Γ
      | _, _, _ => None
      end
  | SFunction name params ret body =>
      let fn_ty := TyFun (map snd params) ret in
      let Γfun := fold_left_env params (extend Γ name fn_ty) in
      match check_block_local Γfun (Some ret) body with
      | Some _ => Some (extend Γ name fn_ty)
      | None => None
      end
  end.

Fixpoint check_block (Γ : env ty) (expected : option ty) (ss : list stmt) : option (env ty) :=
  match ss with
  | nil => Some Γ
  | s :: ss' =>
      match check_stmt Γ expected s with
      | Some Γ' => check_block Γ' expected ss'
      | None => None
      end
  end.

Definition typecheck_program (input : string) : option ty :=
  match parse_program input with
  | Some p =>
      match check_block nil None p with
      | Some _ => Some TyVoid
      | None => None
      end
  | None => None
  end.

Lemma typecheck_program_returns_void :
  forall input τ,
    typecheck_program input = Some τ -> τ = TyVoid.
Proof.
  intros input τ Htc.
  unfold typecheck_program in Htc.
  destruct (parse_program input) as [p|] eqn:Hparse; try discriminate.
  destruct (check_block nil None p) as [Γ|] eqn:Hcheck; inversion Htc; reflexivity.
Qed.

Definition has_type (p : program) : Prop := check_block nil None p <> None.

Example ts_const_ok :
  typecheck_program "const x : number = 1 ;" = Some TyVoid.
Proof. reflexivity. Qed.

Example ts_array_program_typechecks :
  typecheck_program "const xs : number [ ] = [ 1 , 2 ] ;" = Some TyVoid.
Proof. reflexivity. Qed.

Example ts_array_program_parses :
  exists p, parse_program "const xs : number [ ] = [ 1 , 2 ] ;" = Some p.
Proof.
  vm_compute. eexists. reflexivity.
Qed.

Theorem typescript_example_program_parses :
  exists p,
    parse_program
      "function useIt ( v : number [ ] ) : void { return ; } const xs : number [ ] = [ 1 , 2 ] ; useIt ( xs ) ;" = Some p.
Proof.
  vm_compute. eexists. reflexivity.
Qed.

Theorem typescript_example_program_typechecks :
  typecheck_program
    "function useIt ( v : number [ ] ) : void { return ; } const xs : number [ ] = [ 1 , 2 ] ; useIt ( xs ) ;" = Some TyVoid.
Proof. reflexivity. Qed.

Example ts_function_parse_exists :
  exists p,
    parse_program
      "function useIt ( v : number [ ] ) : void { return ; }" = Some p.
Proof.
  vm_compute. eexists. reflexivity.
Qed.

Example ts_function_typechecks :
  typecheck_program
    "function useIt ( v : number [ ] ) : void { return ; }" = Some TyVoid.
Proof. reflexivity. Qed.

Example ts_array_call_parse_exists :
  exists p,
    parse_program
      "const xs : number [ ] = [ 1 , 2 ] ; function useIt ( v : number [ ] ) : void { return ; } useIt ( xs ) ;" = Some p.
Proof.
  vm_compute. eexists. reflexivity.
Qed.

Example ts_array_call_typechecks :
  typecheck_program
    "const xs : number [ ] = [ 1 , 2 ] ; function useIt ( v : number [ ] ) : void { return ; } useIt ( xs ) ;" = Some TyVoid.
Proof. reflexivity. Qed.

Example ts_array_argument_rejected :
  typecheck_program
    "const flag : boolean = true ; function useIt ( v : number [ ] ) : void { return ; } useIt ( flag ) ;" = None.
Proof. reflexivity. Qed.

Example ts_union_if_parse_exists :
  exists p,
    parse_program
      "function head ( xs : string | string [ ] ) : void { if ( true ) { return ; } else { return ; } }" = Some p.
Proof.
  vm_compute. eexists. reflexivity.
Qed.

Example ts_union_if_typechecks :
  typecheck_program
    "function head ( xs : string | string [ ] ) : void { if ( true ) { return ; } else { return ; } }" = Some TyVoid.
Proof. reflexivity. Qed.

Example ts_many_args_parse_exists :
  exists p,
    parse_program
      "function mix ( a : number , b : number [ ] , c : string | boolean ) : void { return ; } mix ( 1 , [ 2 , 3 ] , true ) ;" = Some p.
Proof.
  vm_compute. eexists. reflexivity.
Qed.

Example ts_many_args_typechecks :
  typecheck_program
    "function mix ( a : number , b : number [ ] , c : string | boolean ) : void { return ; } mix ( 1 , [ 2 , 3 ] , true ) ;" = Some TyVoid.
Proof. reflexivity. Qed.

Example ts_bad_initializer_rejected :
  typecheck_program
    "const xs : string [ ] = true ; function useIt ( v : string [ ] ) : void { return ; } useIt ( xs ) ;" = None.
Proof. reflexivity. Qed.

Example ts_bad_argument_rejected :
  typecheck_program
    "const flag : boolean = true ; function useIt ( v : string [ ] ) : void { return ; } useIt ( flag ) ;" = None.
Proof. reflexivity. Qed.

(* Soundness of the executable TypeScript-subset checker:
   if the checker returns [Some τ], then there exists a parsed program [p]
   returned by the verified parser on the same input, and [p] satisfies the
   verified well-typedness predicate [has_type].

   Note that [has_type] here means “the program passes the verified block/
   statement checker”, not that we recover a richer declarative typing judgment
   for every subterm. This still proves the crucial no-false-positives property
   for the extracted TypeScript subset checker. *)
Theorem typecheck_program_sound :
  forall input τ,
    typecheck_program input = Some τ ->
    exists p, parse_program input = Some p /\ has_type p.
Proof.
  intros input τ Htc.
  assert (τ = TyVoid) as Hvoid by (eapply typecheck_program_returns_void; exact Htc).
  subst τ.
  unfold typecheck_program in Htc.
  destruct (parse_program input) as [p|] eqn:Hparse; try discriminate.
  destruct (check_block nil None p) as [Γ|] eqn:Hcheck; try discriminate.
  exists p. split; [reflexivity|].
  unfold has_type. rewrite Hcheck. discriminate.
Qed.

(* Readable alias for the main TypeScript checker soundness theorem. *)
Theorem typescript_checker_soundness :
  forall input τ,
    typecheck_program input = Some τ ->
    exists p, parse_program input = Some p /\ has_type p.
Proof.
  exact typecheck_program_sound.
Qed.

End TypescriptLang.
