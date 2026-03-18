
type unit0 =
| Tt

type bool =
| True
| False

type nat =
| O
| S of nat

type 'a option =
| Some of 'a
| None

type ('a, 'b) prod =
| Pair of 'a * 'b

type 'a list =
| Nil
| Cons of 'a * 'a list

(** val length : 'a1 list -> nat **)

let rec length = function
| Nil -> O
| Cons (_, l') -> S (length l')

(** val add : nat -> nat -> nat **)

let rec add n m =
  match n with
  | O -> m
  | S p -> S (add p m)

(** val mul : nat -> nat -> nat **)

let rec mul n m =
  match n with
  | O -> O
  | S p -> add m (mul p m)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| Nil -> Nil
| Cons (a, l0) -> Cons ((f a), (map f l0))

(** val seq : nat -> nat -> nat list **)

let rec seq start = function
| O -> Nil
| S len0 -> Cons (start, (seq (S start) len0))

type positive =
| XI of positive
| XO of positive
| XH

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a

  (** val to_nat : positive -> nat **)

  let to_nat x =
    iter_op add x (S O)

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function
  | O -> XH
  | S x -> succ (of_succ_nat x)
 end

module Z =
 struct
  (** val double : z -> z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Pos.pred_double p)

  (** val to_nat : z -> nat **)

  let to_nat = function
  | Zpos p -> Pos.to_nat p
  | _ -> O

  (** val of_nat : nat -> z **)

  let of_nat = function
  | O -> Z0
  | S n0 -> Zpos (Pos.of_succ_nat n0)
 end

type int (* AXIOM TO BE REALIZED *)

(** val lsl0 : int -> int -> int **)

let lsl0 =
  failwith "AXIOM TO BE REALIZED (Corelib.Numbers.Cyclic.Int63.PrimInt63.lsl)"

(** val lsr0 : int -> int -> int **)

let lsr0 =
  failwith "AXIOM TO BE REALIZED (Corelib.Numbers.Cyclic.Int63.PrimInt63.lsr)"

(** val land0 : int -> int -> int **)

let land0 =
  failwith "AXIOM TO BE REALIZED (Corelib.Numbers.Cyclic.Int63.PrimInt63.land)"

(** val lor0 : int -> int -> int **)

let lor0 =
  failwith "AXIOM TO BE REALIZED (Corelib.Numbers.Cyclic.Int63.PrimInt63.lor)"

(** val sub : int -> int -> int **)

let sub =
  failwith "AXIOM TO BE REALIZED (Corelib.Numbers.Cyclic.Int63.PrimInt63.sub)"

(** val eqb : int -> int -> bool **)

let eqb =
  failwith "AXIOM TO BE REALIZED (Corelib.Numbers.Cyclic.Int63.PrimInt63.eqb)"

(** val leb : int -> int -> bool **)

let leb =
  failwith "AXIOM TO BE REALIZED (Corelib.Numbers.Cyclic.Int63.PrimInt63.leb)"

(** val size : nat **)

let size =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    O))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val is_zero : int -> bool **)

let is_zero i =
  eqb i (Uint63.of_int (0))

(** val is_even : int -> bool **)

let is_even i =
  is_zero (land0 i (Uint63.of_int (1)))

(** val to_Z_rec : nat -> int -> z **)

let rec to_Z_rec n i =
  match n with
  | O -> Z0
  | S n0 ->
    (match is_even i with
     | True -> Z.double (to_Z_rec n0 (lsr0 i (Uint63.of_int (1))))
     | False -> Z.succ_double (to_Z_rec n0 (lsr0 i (Uint63.of_int (1)))))

(** val to_Z : int -> z **)

let to_Z =
  to_Z_rec size

(** val of_pos_rec : nat -> positive -> int **)

let rec of_pos_rec n p =
  match n with
  | O -> (Uint63.of_int (0))
  | S n0 ->
    (match p with
     | XI p0 ->
       lor0 (lsl0 (of_pos_rec n0 p0) (Uint63.of_int (1))) (Uint63.of_int (1))
     | XO p0 -> lsl0 (of_pos_rec n0 p0) (Uint63.of_int (1))
     | XH -> (Uint63.of_int (1)))

(** val of_pos : positive -> int **)

let of_pos =
  of_pos_rec size

(** val of_Z : z -> int **)

let of_Z = function
| Z0 -> (Uint63.of_int (0))
| Zpos p -> of_pos p
| Zneg p -> sub (Uint63.of_int (0)) (of_pos p)

type char63 = int

type string (* AXIOM TO BE REALIZED *)

(** val make : int -> char63 -> string **)

let make =
  failwith "AXIOM TO BE REALIZED (Corelib.Strings.PrimString.make)"

(** val length0 : string -> int **)

let length0 =
  failwith "AXIOM TO BE REALIZED (Corelib.Strings.PrimString.length)"

(** val get : string -> int -> char63 **)

let get =
  failwith "AXIOM TO BE REALIZED (Corelib.Strings.PrimString.get)"

(** val cat : string -> string -> string **)

let cat =
  failwith "AXIOM TO BE REALIZED (Corelib.Strings.PrimString.cat)"

(** val to_list : string -> char63 list **)

let to_list s =
  map (fun i -> get s (of_Z (Z.of_nat i)))
    (seq O (Z.to_nat (to_Z (length0 s))))

(** val of_list : char63 list -> string **)

let rec of_list = function
| Nil -> (Pstring.unsafe_of_string "")
| Cons (c, cs0) -> cat (make (Uint63.of_int (1)) c) (of_list cs0)

module Common =
 struct
  type 'a env = (string, 'a) prod list

  (** val char_eqb : char63 -> char63 -> bool **)

  let char_eqb =
    eqb

  (** val list_eqb : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

  let rec list_eqb eqbA xs ys =
    match xs with
    | Nil -> (match ys with
              | Nil -> True
              | Cons (_, _) -> False)
    | Cons (x, xs') ->
      (match ys with
       | Nil -> False
       | Cons (y, ys') ->
         (match eqbA x y with
          | True -> list_eqb eqbA xs' ys'
          | False -> False))

  (** val string_eqb : string -> string -> bool **)

  let string_eqb s1 s2 =
    list_eqb char_eqb (to_list s1) (to_list s2)

  (** val lookup : string -> 'a1 env -> 'a1 option **)

  let rec lookup x = function
  | Nil -> None
  | Cons (p, _UU0393_') ->
    let Pair (y, v) = p in
    (match string_eqb x y with
     | True -> Some v
     | False -> lookup x _UU0393_')

  (** val extend : 'a1 env -> string -> 'a1 -> 'a1 env **)

  let extend _UU0393_ x v =
    Cons ((Pair (x, v)), _UU0393_)

  (** val char_between : char63 -> char63 -> char63 -> bool **)

  let char_between lo hi c =
    match leb lo c with
    | True -> leb c hi
    | False -> False

  (** val char_is_lower : char63 -> bool **)

  let char_is_lower c =
    char_between (Uint63.of_int (97)) (Uint63.of_int (122)) c

  (** val char_is_upper : char63 -> bool **)

  let char_is_upper c =
    char_between (Uint63.of_int (65)) (Uint63.of_int (90)) c

  (** val char_is_digit : char63 -> bool **)

  let char_is_digit c =
    char_between (Uint63.of_int (48)) (Uint63.of_int (57)) c

  (** val char_is_alnum : char63 -> bool **)

  let char_is_alnum c =
    match match char_is_lower c with
          | True -> True
          | False -> char_is_upper c with
    | True -> True
    | False -> char_is_digit c

  (** val char_is_space : char63 -> bool **)

  let char_is_space c =
    match match match char_eqb c (Uint63.of_int (32)) with
                | True -> True
                | False -> char_eqb c (Uint63.of_int (9)) with
          | True -> True
          | False -> char_eqb c (Uint63.of_int (10)) with
    | True -> True
    | False -> char_eqb c (Uint63.of_int (13))

  (** val list_forall : ('a1 -> bool) -> 'a1 list -> bool **)

  let rec list_forall p = function
  | Nil -> True
  | Cons (x, xs') ->
    (match p x with
     | True -> list_forall p xs'
     | False -> False)

  (** val string_forall : (char63 -> bool) -> string -> bool **)

  let string_forall p s =
    list_forall p (to_list s)

  (** val rev_append : 'a1 list -> 'a1 list -> 'a1 list **)

  let rec rev_append xs ys =
    match xs with
    | Nil -> ys
    | Cons (x, xs') -> rev_append xs' (Cons (x, ys))

  (** val rev : 'a1 list -> 'a1 list **)

  let rev xs =
    rev_append xs Nil

  (** val rev_chars_to_string : char63 list -> string **)

  let rev_chars_to_string rev_chars =
    of_list (rev rev_chars)

  (** val split_ws_chars_aux :
      char63 list -> char63 list -> string list -> string list **)

  let rec split_ws_chars_aux input current_rev acc_rev =
    match input with
    | Nil ->
      let acc_rev' =
        match current_rev with
        | Nil -> acc_rev
        | Cons (_, _) -> Cons ((rev_chars_to_string current_rev), acc_rev)
      in
      rev acc_rev'
    | Cons (c, rest) ->
      (match char_is_space c with
       | True ->
         let acc_rev' =
           match current_rev with
           | Nil -> acc_rev
           | Cons (_, _) -> Cons ((rev_chars_to_string current_rev), acc_rev)
         in
         split_ws_chars_aux rest Nil acc_rev'
       | False -> split_ws_chars_aux rest (Cons (c, current_rev)) acc_rev)

  (** val split_ws : string -> string list **)

  let split_ws input =
    split_ws_chars_aux (to_list input) Nil Nil

  type 'a coq_parser = string list -> ('a, string list) prod option

  (** val run_parser : 'a1 coq_parser -> string list -> 'a1 option **)

  let run_parser p toks =
    match p toks with
    | Some p0 ->
      let Pair (x, l) = p0 in
      (match l with
       | Nil -> Some x
       | Cons (_, _) -> None)
    | None -> None
 end

module STLC =
 struct
  type ty =
  | TyBase of string
  | TyArrow of ty * ty

  type expr =
  | EVar of string
  | ELam of string * ty * expr
  | EApp of expr * expr

  (** val ty_eqb : ty -> ty -> bool **)

  let rec ty_eqb _UU03c4_1 _UU03c4_2 =
    match _UU03c4_1 with
    | TyBase a ->
      (match _UU03c4_2 with
       | TyBase b -> Common.string_eqb a b
       | TyArrow (_, _) -> False)
    | TyArrow (a1, b1) ->
      (match _UU03c4_2 with
       | TyBase _ -> False
       | TyArrow (a2, b2) ->
         (match ty_eqb a1 a2 with
          | True -> ty_eqb b1 b2
          | False -> False))

  (** val char_is_ident_tail : char63 -> bool **)

  let char_is_ident_tail c =
    match Common.char_is_alnum c with
    | True -> True
    | False -> Common.char_eqb c (Uint63.of_int (95))

  (** val is_identifier : string -> bool **)

  let is_identifier s =
    match to_list s with
    | Nil -> False
    | Cons (c, rest) ->
      (match match match Common.char_is_lower c with
                   | True -> True
                   | False -> Common.char_is_upper c with
             | True -> True
             | False -> Common.char_eqb c (Uint63.of_int (95)) with
       | True -> Common.list_forall char_is_ident_tail rest
       | False -> False)

  (** val is_type_name : string -> bool **)

  let is_type_name s =
    match to_list s with
    | Nil -> False
    | Cons (_, _) -> Common.string_forall char_is_ident_tail s

  (** val fuel_of_tokens : string list -> nat **)

  let fuel_of_tokens toks =
    add (mul (length toks) (S (S (S (S O))))) (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S O))))))))))))))))

  (** val parse_type : nat -> string list -> (ty, string list) prod option **)

  let rec parse_type fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_atomic_type fuel' toks with
       | Some p ->
         let Pair (_UU03c4_1, l) = p in
         (match l with
          | Nil -> Some (Pair (_UU03c4_1, Nil))
          | Cons (tok, rest2) ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "->") with
             | True ->
               (match parse_type fuel' rest2 with
                | Some p0 ->
                  let Pair (_UU03c4_2, rest3) = p0 in
                  Some (Pair ((TyArrow (_UU03c4_1, _UU03c4_2)), rest3))
                | None -> None)
             | False -> Some (Pair (_UU03c4_1, (Cons (tok, rest2))))))
       | None -> None)

  (** val parse_atomic_type :
      nat -> string list -> (ty, string list) prod option **)

  and parse_atomic_type fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
          | True ->
            (match parse_type fuel' rest with
             | Some p ->
               let Pair (_UU03c4_, l) = p in
               (match l with
                | Nil -> None
                | Cons (tok', rest2) ->
                  (match Common.string_eqb tok' (Pstring.unsafe_of_string ")") with
                   | True -> Some (Pair (_UU03c4_, rest2))
                   | False -> None))
             | None -> None)
          | False ->
            (match is_type_name tok with
             | True -> Some (Pair ((TyBase tok), rest))
             | False -> None)))

  (** val parse_expr :
      nat -> string list -> (expr, string list) prod option **)

  let rec parse_expr fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_atom fuel' toks with
       | Some p -> let Pair (e, rest) = p in parse_app_tail fuel' e rest
       | None -> None)

  (** val parse_atom :
      nat -> string list -> (expr, string list) prod option **)

  and parse_atom fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
          | True ->
            (match parse_expr fuel' rest with
             | Some p ->
               let Pair (e, l) = p in
               (match l with
                | Nil -> None
                | Cons (tok', rest2) ->
                  (match Common.string_eqb tok' (Pstring.unsafe_of_string ")") with
                   | True -> Some (Pair (e, rest2))
                   | False -> None))
             | None -> None)
          | False ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "\206\187") with
             | True ->
               (match rest with
                | Nil -> None
                | Cons (x, l) ->
                  (match l with
                   | Nil -> None
                   | Cons (colon, rest2) ->
                     (match is_identifier x with
                      | True ->
                        (match Common.string_eqb colon
                                 (Pstring.unsafe_of_string ":") with
                         | True ->
                           (match parse_type fuel' rest2 with
                            | Some p ->
                              let Pair (_UU03c4_, l0) = p in
                              (match l0 with
                               | Nil -> None
                               | Cons (dot, rest3) ->
                                 (match Common.string_eqb dot
                                          (Pstring.unsafe_of_string ".") with
                                  | True ->
                                    (match parse_expr fuel' rest3 with
                                     | Some p0 ->
                                       let Pair (body, rest4) = p0 in
                                       Some (Pair ((ELam (x, _UU03c4_,
                                       body)), rest4))
                                     | None -> None)
                                  | False -> None))
                            | None -> None)
                         | False -> None)
                      | False -> None)))
             | False ->
               (match is_identifier tok with
                | True -> Some (Pair ((EVar tok), rest))
                | False -> None))))

  (** val parse_app_tail :
      nat -> expr -> string list -> (expr, string list) prod option **)

  and parse_app_tail fuel lhs toks =
    match fuel with
    | O -> Some (Pair (lhs, toks))
    | S fuel' ->
      (match toks with
       | Nil -> Some (Pair (lhs, Nil))
       | Cons (tok, rest) ->
         (match match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
                | True -> True
                | False ->
                  (match Common.string_eqb tok
                           (Pstring.unsafe_of_string "\206\187") with
                   | True -> True
                   | False -> is_identifier tok) with
          | True ->
            (match parse_atom fuel' (Cons (tok, rest)) with
             | Some p ->
               let Pair (arg, rest1) = p in
               parse_app_tail fuel' (EApp (lhs, arg)) rest1
             | None -> None)
          | False -> Some (Pair (lhs, (Cons (tok, rest))))))

  (** val parse : string -> expr option **)

  let parse input =
    let toks = Common.split_ws input in
    Common.run_parser (parse_expr (fuel_of_tokens toks)) toks

  (** val infer : ty Common.env -> expr -> ty option **)

  let rec infer _UU0393_ = function
  | EVar x -> Common.lookup x _UU0393_
  | ELam (x, _UU03c4_, body) ->
    (match infer (Common.extend _UU0393_ x _UU03c4_) body with
     | Some _UU03c4_b -> Some (TyArrow (_UU03c4_, _UU03c4_b))
     | None -> None)
  | EApp (f, a) ->
    (match infer _UU0393_ f with
     | Some t ->
       (match t with
        | TyBase _ -> None
        | TyArrow (_UU03c4_arg, _UU03c4_res) ->
          (match infer _UU0393_ a with
           | Some _UU03c4_a ->
             (match ty_eqb _UU03c4_arg _UU03c4_a with
              | True -> Some _UU03c4_res
              | False -> None)
           | None -> None))
     | None -> None)

  (** val typecheck : string -> ty option **)

  let typecheck input =
    match parse input with
    | Some e -> infer Nil e
    | None -> None
 end

module FunLang =
 struct
  type ty =
  | TyInt
  | TyFloat
  | TyBool
  | TyName of string
  | TyArrow of ty * ty

  type int_op =
  | IAdd
  | ISub
  | IMul
  | IDiv

  type float_op =
  | FAdd
  | FSub
  | FMul
  | FDiv

  type expr =
  | EVar of string
  | EInt of string
  | EFloat of string
  | EBool of bool
  | ELam of string * ty * expr
  | ELet of string * ty * expr * expr
  | EIntBin of int_op * expr * expr
  | EFloatBin of float_op * expr * expr
  | EApp of expr * expr

  (** val ty_eqb : ty -> ty -> bool **)

  let rec ty_eqb _UU03c4_1 _UU03c4_2 =
    match _UU03c4_1 with
    | TyInt -> (match _UU03c4_2 with
                | TyInt -> True
                | _ -> False)
    | TyFloat -> (match _UU03c4_2 with
                  | TyFloat -> True
                  | _ -> False)
    | TyBool -> (match _UU03c4_2 with
                 | TyBool -> True
                 | _ -> False)
    | TyName a ->
      (match _UU03c4_2 with
       | TyName b -> Common.string_eqb a b
       | _ -> False)
    | TyArrow (a1, b1) ->
      (match _UU03c4_2 with
       | TyArrow (a2, b2) ->
         (match ty_eqb a1 a2 with
          | True -> ty_eqb b1 b2
          | False -> False)
       | _ -> False)

  (** val char_is_ident_tail : char63 -> bool **)

  let char_is_ident_tail c =
    match Common.char_is_lower c with
    | True -> True
    | False -> Common.char_is_digit c

  (** val is_identifier : string -> bool **)

  let is_identifier s =
    match to_list s with
    | Nil -> False
    | Cons (c, rest) ->
      (match Common.char_is_lower c with
       | True -> Common.list_forall char_is_ident_tail rest
       | False -> False)

  (** val is_type_name : string -> bool **)

  let is_type_name s =
    match to_list s with
    | Nil -> False
    | Cons (c, rest) ->
      (match Common.char_is_upper c with
       | True -> Common.list_forall char_is_ident_tail rest
       | False -> False)

  (** val is_integer_token : string -> bool **)

  let is_integer_token s =
    match to_list s with
    | Nil -> False
    | Cons (_, _) -> Common.string_forall Common.char_is_digit s

  (** val chars_have_single_dot : bool -> char63 list -> bool **)

  let rec chars_have_single_dot seen_dot = function
  | Nil -> False
  | Cons (c, rest) ->
    (match Common.char_eqb c (Uint63.of_int (46)) with
     | True ->
       (match seen_dot with
        | True -> False
        | False ->
          (match rest with
           | Nil -> False
           | Cons (_, _) -> chars_have_single_dot True rest))
     | False ->
       (match Common.char_is_digit c with
        | True ->
          (match rest with
           | Nil -> seen_dot
           | Cons (_, _) -> chars_have_single_dot seen_dot rest)
        | False -> False))

  (** val is_float_token : string -> bool **)

  let is_float_token s =
    chars_have_single_dot False (to_list s)

  (** val parse_int_op : string -> int_op option **)

  let parse_int_op tok =
    match Common.string_eqb tok (Pstring.unsafe_of_string "+") with
    | True -> Some IAdd
    | False ->
      (match Common.string_eqb tok (Pstring.unsafe_of_string "-") with
       | True -> Some ISub
       | False ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "*") with
          | True -> Some IMul
          | False ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "/") with
             | True -> Some IDiv
             | False -> None)))

  (** val parse_float_op : string -> float_op option **)

  let parse_float_op tok =
    match Common.string_eqb tok (Pstring.unsafe_of_string "+.") with
    | True -> Some FAdd
    | False ->
      (match Common.string_eqb tok (Pstring.unsafe_of_string "-.") with
       | True -> Some FSub
       | False ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "*.") with
          | True -> Some FMul
          | False ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "/.") with
             | True -> Some FDiv
             | False -> None)))

  (** val fuel_of_tokens : string list -> nat **)

  let fuel_of_tokens toks =
    add (mul (length toks) (S (S (S (S (S (S O))))))) (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      O))))))))))))))))))))))))

  (** val parse_type : nat -> string list -> (ty, string list) prod option **)

  let rec parse_type fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_atomic_type fuel' toks with
       | Some p ->
         let Pair (_UU03c4_1, l) = p in
         (match l with
          | Nil -> Some (Pair (_UU03c4_1, Nil))
          | Cons (tok, rest2) ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "->") with
             | True ->
               (match parse_type fuel' rest2 with
                | Some p0 ->
                  let Pair (_UU03c4_2, rest3) = p0 in
                  Some (Pair ((TyArrow (_UU03c4_1, _UU03c4_2)), rest3))
                | None -> None)
             | False -> Some (Pair (_UU03c4_1, (Cons (tok, rest2))))))
       | None -> None)

  (** val parse_atomic_type :
      nat -> string list -> (ty, string list) prod option **)

  and parse_atomic_type fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
          | True ->
            (match parse_type fuel' rest with
             | Some p ->
               let Pair (_UU03c4_, l) = p in
               (match l with
                | Nil -> None
                | Cons (tok', rest2) ->
                  (match Common.string_eqb tok' (Pstring.unsafe_of_string ")") with
                   | True -> Some (Pair (_UU03c4_, rest2))
                   | False -> None))
             | None -> None)
          | False ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "Int") with
             | True -> Some (Pair (TyInt, rest))
             | False ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string "Float") with
                | True -> Some (Pair (TyFloat, rest))
                | False ->
                  (match Common.string_eqb tok
                           (Pstring.unsafe_of_string "Bool") with
                   | True -> Some (Pair (TyBool, rest))
                   | False ->
                     (match is_type_name tok with
                      | True -> Some (Pair ((TyName tok), rest))
                      | False -> None))))))

  (** val parse_expr :
      nat -> string list -> (expr, string list) prod option **)

  let rec parse_expr fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> parse_bin fuel' toks
       | Cons (tok, l) ->
         (match l with
          | Nil -> parse_bin fuel' toks
          | Cons (name, l0) ->
            (match l0 with
             | Nil -> parse_bin fuel' toks
             | Cons (colon, rest) ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string "let") with
                | True ->
                  (match match is_identifier name with
                         | True ->
                           Common.string_eqb colon
                             (Pstring.unsafe_of_string ":")
                         | False -> False with
                   | True ->
                     (match parse_type fuel' rest with
                      | Some p ->
                        let Pair (_UU03c4_, l1) = p in
                        (match l1 with
                         | Nil -> None
                         | Cons (eqtok, rest1) ->
                           (match Common.string_eqb eqtok
                                    (Pstring.unsafe_of_string "=") with
                            | True ->
                              (match parse_expr fuel' rest1 with
                               | Some p0 ->
                                 let Pair (value, l2) = p0 in
                                 (match l2 with
                                  | Nil -> None
                                  | Cons (semi, rest2) ->
                                    (match Common.string_eqb semi
                                             (Pstring.unsafe_of_string ";") with
                                     | True ->
                                       (match parse_expr fuel' rest2 with
                                        | Some p1 ->
                                          let Pair (body, rest3) = p1 in
                                          Some (Pair ((ELet (name, _UU03c4_,
                                          value, body)), rest3))
                                        | None -> None)
                                     | False -> None))
                               | None -> None)
                            | False -> None))
                      | None -> None)
                   | False -> parse_bin fuel' toks)
                | False -> parse_bin fuel' toks))))

  (** val parse_bin :
      nat -> string list -> (expr, string list) prod option **)

  and parse_bin fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_postfix fuel' toks with
       | Some p -> let Pair (e, rest) = p in parse_bin_tail fuel' e rest
       | None -> None)

  (** val parse_bin_tail :
      nat -> expr -> string list -> (expr, string list) prod option **)

  and parse_bin_tail fuel lhs toks =
    match fuel with
    | O -> Some (Pair (lhs, toks))
    | S fuel' ->
      (match toks with
       | Nil -> Some (Pair (lhs, Nil))
       | Cons (tok, rest) ->
         (match parse_int_op tok with
          | Some op ->
            (match parse_postfix fuel' rest with
             | Some p ->
               let Pair (rhs, rest1) = p in
               parse_bin_tail fuel' (EIntBin (op, lhs, rhs)) rest1
             | None -> None)
          | None ->
            (match parse_float_op tok with
             | Some op ->
               (match parse_postfix fuel' rest with
                | Some p ->
                  let Pair (rhs, rest1) = p in
                  parse_bin_tail fuel' (EFloatBin (op, lhs, rhs)) rest1
                | None -> None)
             | None -> Some (Pair (lhs, (Cons (tok, rest)))))))

  (** val parse_postfix :
      nat -> string list -> (expr, string list) prod option **)

  and parse_postfix fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_atom fuel' toks with
       | Some p -> let Pair (e, rest) = p in parse_postfix_tail fuel' e rest
       | None -> None)

  (** val parse_postfix_tail :
      nat -> expr -> string list -> (expr, string list) prod option **)

  and parse_postfix_tail fuel func toks =
    match fuel with
    | O -> Some (Pair (func, toks))
    | S fuel' ->
      (match toks with
       | Nil -> Some (Pair (func, toks))
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
          | True ->
            (match parse_expr fuel' rest with
             | Some p ->
               let Pair (arg, l) = p in
               (match l with
                | Nil -> None
                | Cons (close, rest1) ->
                  (match Common.string_eqb close
                           (Pstring.unsafe_of_string ")") with
                   | True -> parse_postfix_tail fuel' (EApp (func, arg)) rest1
                   | False -> None))
             | None -> None)
          | False -> Some (Pair (func, toks))))

  (** val parse_atom :
      nat -> string list -> (expr, string list) prod option **)

  and parse_atom fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
          | True ->
            (match rest with
             | Nil ->
               (match parse_expr fuel' rest with
                | Some p ->
                  let Pair (e, l) = p in
                  (match l with
                   | Nil -> None
                   | Cons (close', rest3) ->
                     (match Common.string_eqb close'
                              (Pstring.unsafe_of_string ")") with
                      | True -> Some (Pair (e, rest3))
                      | False -> None))
                | None -> None)
             | Cons (name, l) ->
               (match l with
                | Nil ->
                  (match parse_expr fuel' rest with
                   | Some p ->
                     let Pair (e, l0) = p in
                     (match l0 with
                      | Nil -> None
                      | Cons (close', rest3) ->
                        (match Common.string_eqb close'
                                 (Pstring.unsafe_of_string ")") with
                         | True -> Some (Pair (e, rest3))
                         | False -> None))
                   | None -> None)
                | Cons (colon, rest1) ->
                  (match match is_identifier name with
                         | True ->
                           Common.string_eqb colon
                             (Pstring.unsafe_of_string ":")
                         | False -> False with
                   | True ->
                     (match parse_type fuel' rest1 with
                      | Some p ->
                        let Pair (_UU03c4_, l0) = p in
                        (match l0 with
                         | Nil ->
                           (match parse_expr fuel' rest with
                            | Some p0 ->
                              let Pair (e, l1) = p0 in
                              (match l1 with
                               | Nil -> None
                               | Cons (close', rest3) ->
                                 (match Common.string_eqb close'
                                          (Pstring.unsafe_of_string ")") with
                                  | True -> Some (Pair (e, rest3))
                                  | False -> None))
                            | None -> None)
                         | Cons (close, l1) ->
                           (match l1 with
                            | Nil ->
                              (match parse_expr fuel' rest with
                               | Some p0 ->
                                 let Pair (e, l2) = p0 in
                                 (match l2 with
                                  | Nil -> None
                                  | Cons (close', rest3) ->
                                    (match Common.string_eqb close'
                                             (Pstring.unsafe_of_string ")") with
                                     | True -> Some (Pair (e, rest3))
                                     | False -> None))
                               | None -> None)
                            | Cons (arrow, rest2) ->
                              (match match Common.string_eqb close
                                             (Pstring.unsafe_of_string ")") with
                                     | True ->
                                       Common.string_eqb arrow
                                         (Pstring.unsafe_of_string "=>")
                                     | False -> False with
                               | True ->
                                 (match parse_expr fuel' rest2 with
                                  | Some p0 ->
                                    let Pair (body, rest3) = p0 in
                                    Some (Pair ((ELam (name, _UU03c4_,
                                    body)), rest3))
                                  | None -> None)
                               | False ->
                                 (match parse_expr fuel' rest with
                                  | Some p0 ->
                                    let Pair (e, l2) = p0 in
                                    (match l2 with
                                     | Nil -> None
                                     | Cons (close', rest3) ->
                                       (match Common.string_eqb close'
                                                (Pstring.unsafe_of_string ")") with
                                        | True -> Some (Pair (e, rest3))
                                        | False -> None))
                                  | None -> None))))
                      | None ->
                        (match parse_expr fuel' rest with
                         | Some p ->
                           let Pair (e, l0) = p in
                           (match l0 with
                            | Nil -> None
                            | Cons (close', rest3) ->
                              (match Common.string_eqb close'
                                       (Pstring.unsafe_of_string ")") with
                               | True -> Some (Pair (e, rest3))
                               | False -> None))
                         | None -> None))
                   | False ->
                     (match parse_expr fuel' rest with
                      | Some p ->
                        let Pair (e, l0) = p in
                        (match l0 with
                         | Nil -> None
                         | Cons (close', rest3) ->
                           (match Common.string_eqb close'
                                    (Pstring.unsafe_of_string ")") with
                            | True -> Some (Pair (e, rest3))
                            | False -> None))
                      | None -> None))))
          | False ->
            (match is_float_token tok with
             | True -> Some (Pair ((EFloat tok), rest))
             | False ->
               (match is_integer_token tok with
                | True -> Some (Pair ((EInt tok), rest))
                | False ->
                  (match Common.string_eqb tok
                           (Pstring.unsafe_of_string "true") with
                   | True -> Some (Pair ((EBool True), rest))
                   | False ->
                     (match Common.string_eqb tok
                              (Pstring.unsafe_of_string "false") with
                      | True -> Some (Pair ((EBool False), rest))
                      | False ->
                        (match is_identifier tok with
                         | True -> Some (Pair ((EVar tok), rest))
                         | False -> None)))))))

  (** val parse : string -> expr option **)

  let parse input =
    let toks = Common.split_ws input in
    Common.run_parser (parse_expr (fuel_of_tokens toks)) toks

  (** val infer : ty Common.env -> expr -> ty option **)

  let rec infer _UU0393_ = function
  | EVar x -> Common.lookup x _UU0393_
  | EInt _ -> Some TyInt
  | EFloat _ -> Some TyFloat
  | EBool _ -> Some TyBool
  | ELam (x, _UU03c4_, body) ->
    (match infer (Common.extend _UU0393_ x _UU03c4_) body with
     | Some _UU03c4_b -> Some (TyArrow (_UU03c4_, _UU03c4_b))
     | None -> None)
  | ELet (x, _UU03c4_, value, body) ->
    (match infer _UU0393_ value with
     | Some _UU03c4_v ->
       (match ty_eqb _UU03c4_ _UU03c4_v with
        | True -> infer (Common.extend _UU0393_ x _UU03c4_) body
        | False -> None)
     | None -> None)
  | EIntBin (_, l, r) ->
    (match infer _UU0393_ l with
     | Some t ->
       (match t with
        | TyInt ->
          (match infer _UU0393_ r with
           | Some t0 -> (match t0 with
                         | TyInt -> Some TyInt
                         | _ -> None)
           | None -> None)
        | _ -> None)
     | None -> None)
  | EFloatBin (_, l, r) ->
    (match infer _UU0393_ l with
     | Some t ->
       (match t with
        | TyFloat ->
          (match infer _UU0393_ r with
           | Some t0 -> (match t0 with
                         | TyFloat -> Some TyFloat
                         | _ -> None)
           | None -> None)
        | _ -> None)
     | None -> None)
  | EApp (f, a) ->
    (match infer _UU0393_ f with
     | Some t ->
       (match t with
        | TyArrow (_UU03c4_arg, _UU03c4_res) ->
          (match infer _UU0393_ a with
           | Some _UU03c4_a ->
             (match ty_eqb _UU03c4_arg _UU03c4_a with
              | True -> Some _UU03c4_res
              | False -> None)
           | None -> None)
        | _ -> None)
     | None -> None)

  (** val typecheck : string -> ty option **)

  let typecheck input =
    match parse input with
    | Some e -> infer Nil e
    | None -> None
 end

module ImpLang =
 struct
  type ty =
  | TyInt
  | TyBool
  | TyUnion of ty * ty

  type arith_op =
  | AAdd
  | ASub
  | AMul
  | ADiv

  type comp_op =
  | CEq
  | CNe
  | CLt
  | CLe
  | CGt
  | CGe

  type expr =
  | EVar of string
  | EInt of string
  | EBool of bool
  | EArith of arith_op * expr * expr
  | EComp of comp_op * expr * expr

  type stmt =
  | SDecl of string * ty * expr
  | SAssign of string * expr
  | SIf of expr * stmt list
  | SIfElse of expr * stmt list * stmt list
  | SWhile of expr * stmt list

  (** val ty_eqb : ty -> ty -> bool **)

  let rec ty_eqb _UU03c4_1 _UU03c4_2 =
    match _UU03c4_1 with
    | TyInt -> (match _UU03c4_2 with
                | TyInt -> True
                | _ -> False)
    | TyBool -> (match _UU03c4_2 with
                 | TyBool -> True
                 | _ -> False)
    | TyUnion (a1, b1) ->
      (match _UU03c4_2 with
       | TyUnion (a2, b2) ->
         (match ty_eqb a1 a2 with
          | True -> ty_eqb b1 b2
          | False -> False)
       | _ -> False)

  (** val char_is_ident_tail : char63 -> bool **)

  let char_is_ident_tail c =
    match match Common.char_is_lower c with
          | True -> True
          | False -> Common.char_is_digit c with
    | True -> True
    | False -> Common.char_eqb c (Uint63.of_int (95))

  (** val is_identifier : string -> bool **)

  let is_identifier s =
    match to_list s with
    | Nil -> False
    | Cons (c, rest) ->
      (match Common.char_is_lower c with
       | True -> Common.list_forall char_is_ident_tail rest
       | False -> False)

  (** val is_integer_token : string -> bool **)

  let is_integer_token s =
    match to_list s with
    | Nil -> False
    | Cons (_, _) -> Common.string_forall Common.char_is_digit s

  (** val parse_arith_op : string -> arith_op option **)

  let parse_arith_op tok =
    match Common.string_eqb tok (Pstring.unsafe_of_string "+") with
    | True -> Some AAdd
    | False ->
      (match Common.string_eqb tok (Pstring.unsafe_of_string "-") with
       | True -> Some ASub
       | False ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "*") with
          | True -> Some AMul
          | False ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "/") with
             | True -> Some ADiv
             | False -> None)))

  (** val parse_comp_op : string -> comp_op option **)

  let parse_comp_op tok =
    match Common.string_eqb tok (Pstring.unsafe_of_string "==") with
    | True -> Some CEq
    | False ->
      (match Common.string_eqb tok (Pstring.unsafe_of_string "!=") with
       | True -> Some CNe
       | False ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "<") with
          | True -> Some CLt
          | False ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "<=") with
             | True -> Some CLe
             | False ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string ">") with
                | True -> Some CGt
                | False ->
                  (match Common.string_eqb tok (Pstring.unsafe_of_string ">=") with
                   | True -> Some CGe
                   | False -> None)))))

  (** val fuel_of_tokens : string list -> nat **)

  let fuel_of_tokens toks =
    add (mul (length toks) (S (S (S (S (S (S (S (S O))))))))) (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S O))))))))))))))))))))))))))))))))

  (** val parse_type : nat -> string list -> (ty, string list) prod option **)

  let rec parse_type fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_atomic_type fuel' toks with
       | Some p ->
         let Pair (_UU03c4_1, l) = p in
         (match l with
          | Nil -> Some (Pair (_UU03c4_1, Nil))
          | Cons (tok, rest2) ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "|") with
             | True ->
               (match parse_type fuel' rest2 with
                | Some p0 ->
                  let Pair (_UU03c4_2, rest3) = p0 in
                  Some (Pair ((TyUnion (_UU03c4_1, _UU03c4_2)), rest3))
                | None -> None)
             | False -> Some (Pair (_UU03c4_1, (Cons (tok, rest2))))))
       | None -> None)

  (** val parse_atomic_type :
      nat -> string list -> (ty, string list) prod option **)

  and parse_atomic_type fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
          | True ->
            (match parse_type fuel' rest with
             | Some p ->
               let Pair (_UU03c4_, l) = p in
               (match l with
                | Nil -> None
                | Cons (close, rest1) ->
                  (match Common.string_eqb close
                           (Pstring.unsafe_of_string ")") with
                   | True -> Some (Pair (_UU03c4_, rest1))
                   | False -> None))
             | None -> None)
          | False ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "Int") with
             | True -> Some (Pair (TyInt, rest))
             | False ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string "Bool") with
                | True -> Some (Pair (TyBool, rest))
                | False -> None))))

  (** val parse_expr :
      nat -> string list -> (expr, string list) prod option **)

  let rec parse_expr fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_arith fuel' toks with
       | Some p ->
         let Pair (lhs, l) = p in
         (match l with
          | Nil -> Some (Pair (lhs, Nil))
          | Cons (tok, rest) ->
            (match parse_comp_op tok with
             | Some op ->
               (match parse_arith fuel' rest with
                | Some p0 ->
                  let Pair (rhs, rest1) = p0 in
                  Some (Pair ((EComp (op, lhs, rhs)), rest1))
                | None -> None)
             | None -> Some (Pair (lhs, (Cons (tok, rest))))))
       | None -> None)

  (** val parse_arith :
      nat -> string list -> (expr, string list) prod option **)

  and parse_arith fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match parse_atom fuel' toks with
       | Some p -> let Pair (e, rest) = p in parse_arith_tail fuel' e rest
       | None -> None)

  (** val parse_arith_tail :
      nat -> expr -> string list -> (expr, string list) prod option **)

  and parse_arith_tail fuel lhs toks =
    match fuel with
    | O -> Some (Pair (lhs, toks))
    | S fuel' ->
      (match toks with
       | Nil -> Some (Pair (lhs, Nil))
       | Cons (tok, rest) ->
         (match parse_arith_op tok with
          | Some op ->
            (match parse_atom fuel' rest with
             | Some p ->
               let Pair (rhs, rest1) = p in
               parse_arith_tail fuel' (EArith (op, lhs, rhs)) rest1
             | None -> None)
          | None -> Some (Pair (lhs, (Cons (tok, rest))))))

  (** val parse_atom :
      nat -> string list -> (expr, string list) prod option **)

  and parse_atom fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "(") with
          | True ->
            (match parse_expr fuel' rest with
             | Some p ->
               let Pair (e, l) = p in
               (match l with
                | Nil -> None
                | Cons (close, rest1) ->
                  (match Common.string_eqb close
                           (Pstring.unsafe_of_string ")") with
                   | True -> Some (Pair (e, rest1))
                   | False -> None))
             | None -> None)
          | False ->
            (match is_integer_token tok with
             | True -> Some (Pair ((EInt tok), rest))
             | False ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string "true") with
                | True -> Some (Pair ((EBool True), rest))
                | False ->
                  (match Common.string_eqb tok
                           (Pstring.unsafe_of_string "false") with
                   | True -> Some (Pair ((EBool False), rest))
                   | False ->
                     (match is_identifier tok with
                      | True -> Some (Pair ((EVar tok), rest))
                      | False -> None))))))

  (** val parse_stmt :
      nat -> string list -> (stmt, string list) prod option **)

  let rec parse_stmt fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match rest with
          | Nil ->
            (match Common.string_eqb tok (Pstring.unsafe_of_string "if") with
             | True ->
               (match rest with
                | Nil -> None
                | Cons (open0, rest1) ->
                  (match Common.string_eqb open0
                           (Pstring.unsafe_of_string "(") with
                   | True ->
                     (match parse_expr fuel' rest1 with
                      | Some p ->
                        let Pair (cond, l) = p in
                        (match l with
                         | Nil -> None
                         | Cons (close, rest2) ->
                           (match Common.string_eqb close
                                    (Pstring.unsafe_of_string ")") with
                            | True ->
                              (match parse_block fuel' rest2 with
                               | Some p0 ->
                                 let Pair (then_block, l0) = p0 in
                                 (match l0 with
                                  | Nil ->
                                    Some (Pair ((SIf (cond, then_block)),
                                      Nil))
                                  | Cons (else_tok, rest3) ->
                                    (match Common.string_eqb else_tok
                                             (Pstring.unsafe_of_string "else") with
                                     | True ->
                                       (match parse_block fuel' rest3 with
                                        | Some p1 ->
                                          let Pair (else_block, rest4) = p1 in
                                          Some (Pair ((SIfElse (cond,
                                          then_block, else_block)), rest4))
                                        | None ->
                                          Some (Pair ((SIf (cond,
                                            then_block)), (Cons (else_tok,
                                            rest3)))))
                                     | False ->
                                       Some (Pair ((SIf (cond, then_block)),
                                         (Cons (else_tok, rest3))))))
                               | None -> None)
                            | False -> None))
                      | None -> None)
                   | False -> None))
             | False ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string "while") with
                | True ->
                  (match rest with
                   | Nil -> None
                   | Cons (open0, rest1) ->
                     (match Common.string_eqb open0
                              (Pstring.unsafe_of_string "(") with
                      | True ->
                        (match parse_expr fuel' rest1 with
                         | Some p ->
                           let Pair (cond, l) = p in
                           (match l with
                            | Nil -> None
                            | Cons (close, rest2) ->
                              (match Common.string_eqb close
                                       (Pstring.unsafe_of_string ")") with
                               | True ->
                                 (match parse_block fuel' rest2 with
                                  | Some p0 ->
                                    let Pair (body, rest3) = p0 in
                                    Some (Pair ((SWhile (cond, body)), rest3))
                                  | None -> None)
                               | False -> None))
                         | None -> None)
                      | False -> None))
                | False -> None))
          | Cons (name, l) ->
            (match l with
             | Nil ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string "if") with
                | True ->
                  (match rest with
                   | Nil -> None
                   | Cons (open0, rest1) ->
                     (match Common.string_eqb open0
                              (Pstring.unsafe_of_string "(") with
                      | True ->
                        (match parse_expr fuel' rest1 with
                         | Some p ->
                           let Pair (cond, l0) = p in
                           (match l0 with
                            | Nil -> None
                            | Cons (close, rest2) ->
                              (match Common.string_eqb close
                                       (Pstring.unsafe_of_string ")") with
                               | True ->
                                 (match parse_block fuel' rest2 with
                                  | Some p0 ->
                                    let Pair (then_block, l1) = p0 in
                                    (match l1 with
                                     | Nil ->
                                       Some (Pair ((SIf (cond, then_block)),
                                         Nil))
                                     | Cons (else_tok, rest3) ->
                                       (match Common.string_eqb else_tok
                                                (Pstring.unsafe_of_string "else") with
                                        | True ->
                                          (match parse_block fuel' rest3 with
                                           | Some p1 ->
                                             let Pair (else_block, rest4) = p1
                                             in
                                             Some (Pair ((SIfElse (cond,
                                             then_block, else_block)), rest4))
                                           | None ->
                                             Some (Pair ((SIf (cond,
                                               then_block)), (Cons (else_tok,
                                               rest3)))))
                                        | False ->
                                          Some (Pair ((SIf (cond,
                                            then_block)), (Cons (else_tok,
                                            rest3))))))
                                  | None -> None)
                               | False -> None))
                         | None -> None)
                      | False -> None))
                | False ->
                  (match Common.string_eqb tok
                           (Pstring.unsafe_of_string "while") with
                   | True ->
                     (match rest with
                      | Nil -> None
                      | Cons (open0, rest1) ->
                        (match Common.string_eqb open0
                                 (Pstring.unsafe_of_string "(") with
                         | True ->
                           (match parse_expr fuel' rest1 with
                            | Some p ->
                              let Pair (cond, l0) = p in
                              (match l0 with
                               | Nil -> None
                               | Cons (close, rest2) ->
                                 (match Common.string_eqb close
                                          (Pstring.unsafe_of_string ")") with
                                  | True ->
                                    (match parse_block fuel' rest2 with
                                     | Some p0 ->
                                       let Pair (body, rest3) = p0 in
                                       Some (Pair ((SWhile (cond, body)),
                                       rest3))
                                     | None -> None)
                                  | False -> None))
                            | None -> None)
                         | False -> None))
                   | False -> None))
             | Cons (colon, rest0) ->
               (match Common.string_eqb tok (Pstring.unsafe_of_string "let") with
                | True ->
                  (match match is_identifier name with
                         | True ->
                           Common.string_eqb colon
                             (Pstring.unsafe_of_string ":")
                         | False -> False with
                   | True ->
                     (match parse_type fuel' rest0 with
                      | Some p ->
                        let Pair (_UU03c4_, l0) = p in
                        (match l0 with
                         | Nil -> None
                         | Cons (eqtok, rest1) ->
                           (match Common.string_eqb eqtok
                                    (Pstring.unsafe_of_string "=") with
                            | True ->
                              (match parse_expr fuel' rest1 with
                               | Some p0 ->
                                 let Pair (value, l1) = p0 in
                                 (match l1 with
                                  | Nil -> None
                                  | Cons (semi, rest2) ->
                                    (match Common.string_eqb semi
                                             (Pstring.unsafe_of_string ";") with
                                     | True ->
                                       Some (Pair ((SDecl (name, _UU03c4_,
                                         value)), rest2))
                                     | False -> None))
                               | None -> None)
                            | False -> None))
                      | None -> None)
                   | False -> None)
                | False ->
                  (match Common.string_eqb tok (Pstring.unsafe_of_string "if") with
                   | True ->
                     (match Common.string_eqb name
                              (Pstring.unsafe_of_string "(") with
                      | True ->
                        (match parse_expr fuel' (Cons (colon, rest0)) with
                         | Some p ->
                           let Pair (cond, l0) = p in
                           (match l0 with
                            | Nil -> None
                            | Cons (close, rest2) ->
                              (match Common.string_eqb close
                                       (Pstring.unsafe_of_string ")") with
                               | True ->
                                 (match parse_block fuel' rest2 with
                                  | Some p0 ->
                                    let Pair (then_block, l1) = p0 in
                                    (match l1 with
                                     | Nil ->
                                       Some (Pair ((SIf (cond, then_block)),
                                         Nil))
                                     | Cons (else_tok, rest3) ->
                                       (match Common.string_eqb else_tok
                                                (Pstring.unsafe_of_string "else") with
                                        | True ->
                                          (match parse_block fuel' rest3 with
                                           | Some p1 ->
                                             let Pair (else_block, rest4) = p1
                                             in
                                             Some (Pair ((SIfElse (cond,
                                             then_block, else_block)), rest4))
                                           | None ->
                                             Some (Pair ((SIf (cond,
                                               then_block)), (Cons (else_tok,
                                               rest3)))))
                                        | False ->
                                          Some (Pair ((SIf (cond,
                                            then_block)), (Cons (else_tok,
                                            rest3))))))
                                  | None -> None)
                               | False -> None))
                         | None -> None)
                      | False -> None)
                   | False ->
                     (match Common.string_eqb tok
                              (Pstring.unsafe_of_string "while") with
                      | True ->
                        (match Common.string_eqb name
                                 (Pstring.unsafe_of_string "(") with
                         | True ->
                           (match parse_expr fuel' (Cons (colon, rest0)) with
                            | Some p ->
                              let Pair (cond, l0) = p in
                              (match l0 with
                               | Nil -> None
                               | Cons (close, rest2) ->
                                 (match Common.string_eqb close
                                          (Pstring.unsafe_of_string ")") with
                                  | True ->
                                    (match parse_block fuel' rest2 with
                                     | Some p0 ->
                                       let Pair (body, rest3) = p0 in
                                       Some (Pair ((SWhile (cond, body)),
                                       rest3))
                                     | None -> None)
                                  | False -> None))
                            | None -> None)
                         | False -> None)
                      | False ->
                        (match is_identifier tok with
                         | True ->
                           (match Common.string_eqb name
                                    (Pstring.unsafe_of_string "=") with
                            | True ->
                              (match parse_expr fuel' (Cons (colon, rest0)) with
                               | Some p ->
                                 let Pair (value, l0) = p in
                                 (match l0 with
                                  | Nil -> None
                                  | Cons (semi, rest1) ->
                                    (match Common.string_eqb semi
                                             (Pstring.unsafe_of_string ";") with
                                     | True ->
                                       Some (Pair ((SAssign (tok, value)),
                                         rest1))
                                     | False -> None))
                               | None -> None)
                            | False -> None)
                         | False -> None)))))))

  (** val parse_stmts :
      nat -> string list -> (stmt list, string list) prod option **)

  and parse_stmts fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> Some (Pair (Nil, Nil))
       | Cons (tok, _) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "}") with
          | True -> Some (Pair (Nil, toks))
          | False ->
            (match parse_stmt fuel' toks with
             | Some p ->
               let Pair (s, rest) = p in
               (match parse_stmts fuel' rest with
                | Some p0 ->
                  let Pair (ss, rest1) = p0 in
                  Some (Pair ((Cons (s, ss)), rest1))
                | None -> None)
             | None -> None)))

  (** val parse_block :
      nat -> string list -> (stmt list, string list) prod option **)

  and parse_block fuel toks =
    match fuel with
    | O -> None
    | S fuel' ->
      (match toks with
       | Nil -> None
       | Cons (tok, rest) ->
         (match Common.string_eqb tok (Pstring.unsafe_of_string "{") with
          | True ->
            (match parse_stmts fuel' rest with
             | Some p ->
               let Pair (ss, l) = p in
               (match l with
                | Nil -> None
                | Cons (close, rest1) ->
                  (match Common.string_eqb close
                           (Pstring.unsafe_of_string "}") with
                   | True -> Some (Pair (ss, rest1))
                   | False -> None))
             | None -> None)
          | False -> None))

  (** val parse_program : string -> stmt list option **)

  let parse_program input =
    let toks = Common.split_ws input in
    Common.run_parser (parse_block (fuel_of_tokens toks)) toks

  (** val infer_expr : ty Common.env -> expr -> ty option **)

  let rec infer_expr _UU0393_ = function
  | EVar x -> Common.lookup x _UU0393_
  | EInt _ -> Some TyInt
  | EBool _ -> Some TyBool
  | EArith (_, l, r) ->
    (match infer_expr _UU0393_ l with
     | Some t ->
       (match t with
        | TyInt ->
          (match infer_expr _UU0393_ r with
           | Some t0 -> (match t0 with
                         | TyInt -> Some TyInt
                         | _ -> None)
           | None -> None)
        | _ -> None)
     | None -> None)
  | EComp (_, l, r) ->
    (match infer_expr _UU0393_ l with
     | Some t ->
       (match t with
        | TyInt ->
          (match infer_expr _UU0393_ r with
           | Some t0 -> (match t0 with
                         | TyInt -> Some TyBool
                         | _ -> None)
           | None -> None)
        | _ -> None)
     | None -> None)

  (** val default_stmt_fuel : nat **)

  let default_stmt_fuel =
    S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
      (S (S (S (S (S (S (S
      O)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

  (** val check_stmt_fuel :
      nat -> ty Common.env -> stmt -> ty Common.env option **)

  let rec check_stmt_fuel fuel _UU0393_ s =
    match fuel with
    | O -> None
    | S n ->
      (match s with
       | SDecl (s0, t, e) ->
         let o = infer_expr _UU0393_ e in
         (match o with
          | Some t0 ->
            (match ty_eqb t t0 with
             | True -> Some (Common.extend _UU0393_ s0 t)
             | False -> None)
          | None -> None)
       | SAssign (s0, e) ->
         let o = Common.lookup s0 _UU0393_ in
         (match o with
          | Some t ->
            let o0 = infer_expr _UU0393_ e in
            (match o0 with
             | Some t0 ->
               (match ty_eqb t t0 with
                | True -> Some _UU0393_
                | False -> None)
             | None -> None)
          | None -> None)
       | SIf (e, l) ->
         let o = infer_expr _UU0393_ e in
         (match o with
          | Some t ->
            (match ty_eqb t TyBool with
             | True ->
               (match check_stmts_fuel n _UU0393_ l with
                | Some _ -> Some _UU0393_
                | None -> None)
             | False -> None)
          | None -> None)
       | SIfElse (e, l, l0) ->
         let o = infer_expr _UU0393_ e in
         (match o with
          | Some t ->
            (match ty_eqb t TyBool with
             | True ->
               (match check_stmts_fuel n _UU0393_ l with
                | Some _ ->
                  (match check_stmts_fuel n _UU0393_ l0 with
                   | Some _ -> Some _UU0393_
                   | None -> None)
                | None -> None)
             | False -> None)
          | None -> None)
       | SWhile (e, l) ->
         let o = infer_expr _UU0393_ e in
         (match o with
          | Some t ->
            (match ty_eqb t TyBool with
             | True ->
               (match check_stmts_fuel n _UU0393_ l with
                | Some _ -> Some _UU0393_
                | None -> None)
             | False -> None)
          | None -> None))

  (** val check_stmts_fuel :
      nat -> ty Common.env -> stmt list -> ty Common.env option **)

  and check_stmts_fuel fuel _UU0393_ ss =
    match fuel with
    | O -> None
    | S n ->
      (match ss with
       | Nil -> Some _UU0393_
       | Cons (s, l) ->
         let o = check_stmt_fuel n _UU0393_ s in
         (match o with
          | Some e -> check_stmts_fuel n e l
          | None -> None))

  (** val check_stmts : ty Common.env -> stmt list -> ty Common.env option **)

  let check_stmts _UU0393_ ss =
    check_stmts_fuel default_stmt_fuel _UU0393_ ss

  (** val check_block : ty Common.env -> stmt list -> unit0 option **)

  let check_block _UU0393_ body =
    match check_stmts _UU0393_ body with
    | Some _ -> Some Tt
    | None -> None

  (** val typecheck_program : string -> unit0 option **)

  let typecheck_program input =
    match parse_program input with
    | Some body -> check_block Nil body
    | None -> None
 end
