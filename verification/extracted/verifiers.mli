
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

val length : 'a1 list -> nat

val add : nat -> nat -> nat

val mul : nat -> nat -> nat

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val seq : nat -> nat -> nat list

type positive =
| XI of positive
| XO of positive
| XH

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Pos :
 sig
  val succ : positive -> positive

  val pred_double : positive -> positive

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> nat

  val of_succ_nat : nat -> positive
 end

module Z :
 sig
  val double : z -> z

  val succ_double : z -> z

  val to_nat : z -> nat

  val of_nat : nat -> z
 end

type int (* AXIOM TO BE REALIZED *)

val lsl0 : int -> int -> int

val lsr0 : int -> int -> int

val land0 : int -> int -> int

val lor0 : int -> int -> int

val sub : int -> int -> int

val eqb : int -> int -> bool

val leb : int -> int -> bool

val size : nat

val is_zero : int -> bool

val is_even : int -> bool

val to_Z_rec : nat -> int -> z

val to_Z : int -> z

val of_pos_rec : nat -> positive -> int

val of_pos : positive -> int

val of_Z : z -> int

type char63 = int

type string (* AXIOM TO BE REALIZED *)

val make : int -> char63 -> string

val length0 : string -> int

val get : string -> int -> char63

val cat : string -> string -> string

val to_list : string -> char63 list

val of_list : char63 list -> string

module Common :
 sig
  type 'a env = (string, 'a) prod list

  val char_eqb : char63 -> char63 -> bool

  val list_eqb : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool

  val string_eqb : string -> string -> bool

  val lookup : string -> 'a1 env -> 'a1 option

  val extend : 'a1 env -> string -> 'a1 -> 'a1 env

  val char_between : char63 -> char63 -> char63 -> bool

  val char_is_lower : char63 -> bool

  val char_is_upper : char63 -> bool

  val char_is_digit : char63 -> bool

  val char_is_alnum : char63 -> bool

  val char_is_space : char63 -> bool

  val list_forall : ('a1 -> bool) -> 'a1 list -> bool

  val string_forall : (char63 -> bool) -> string -> bool

  val rev_append : 'a1 list -> 'a1 list -> 'a1 list

  val rev : 'a1 list -> 'a1 list

  val rev_chars_to_string : char63 list -> string

  val split_ws_chars_aux :
    char63 list -> char63 list -> string list -> string list

  val split_ws : string -> string list

  type 'a coq_parser = string list -> ('a, string list) prod option

  val run_parser : 'a1 coq_parser -> string list -> 'a1 option
 end

module STLC :
 sig
  type ty =
  | TyBase of string
  | TyArrow of ty * ty

  type expr =
  | EVar of string
  | ELam of string * ty * expr
  | EApp of expr * expr

  val ty_eqb : ty -> ty -> bool

  val char_is_ident_tail : char63 -> bool

  val is_identifier : string -> bool

  val is_type_name : string -> bool

  val fuel_of_tokens : string list -> nat

  val parse_type : nat -> string list -> (ty, string list) prod option

  val parse_atomic_type : nat -> string list -> (ty, string list) prod option

  val parse_expr : nat -> string list -> (expr, string list) prod option

  val parse_atom : nat -> string list -> (expr, string list) prod option

  val parse_app_tail :
    nat -> expr -> string list -> (expr, string list) prod option

  val parse : string -> expr option

  val infer : ty Common.env -> expr -> ty option

  val typecheck : string -> ty option
 end

module FunLang :
 sig
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

  val ty_eqb : ty -> ty -> bool

  val char_is_ident_tail : char63 -> bool

  val is_identifier : string -> bool

  val is_type_name : string -> bool

  val is_integer_token : string -> bool

  val chars_have_single_dot : bool -> char63 list -> bool

  val is_float_token : string -> bool

  val parse_int_op : string -> int_op option

  val parse_float_op : string -> float_op option

  val fuel_of_tokens : string list -> nat

  val parse_type : nat -> string list -> (ty, string list) prod option

  val parse_atomic_type : nat -> string list -> (ty, string list) prod option

  val parse_expr : nat -> string list -> (expr, string list) prod option

  val parse_bin : nat -> string list -> (expr, string list) prod option

  val parse_bin_tail :
    nat -> expr -> string list -> (expr, string list) prod option

  val parse_postfix : nat -> string list -> (expr, string list) prod option

  val parse_postfix_tail :
    nat -> expr -> string list -> (expr, string list) prod option

  val parse_atom : nat -> string list -> (expr, string list) prod option

  val parse : string -> expr option

  val infer : ty Common.env -> expr -> ty option

  val typecheck : string -> ty option
 end

module ImpLang :
 sig
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

  val ty_eqb : ty -> ty -> bool

  val char_is_ident_tail : char63 -> bool

  val is_identifier : string -> bool

  val is_integer_token : string -> bool

  val parse_arith_op : string -> arith_op option

  val parse_comp_op : string -> comp_op option

  val fuel_of_tokens : string list -> nat

  val parse_type : nat -> string list -> (ty, string list) prod option

  val parse_atomic_type : nat -> string list -> (ty, string list) prod option

  val parse_expr : nat -> string list -> (expr, string list) prod option

  val parse_arith : nat -> string list -> (expr, string list) prod option

  val parse_arith_tail :
    nat -> expr -> string list -> (expr, string list) prod option

  val parse_atom : nat -> string list -> (expr, string list) prod option

  val parse_stmt : nat -> string list -> (stmt, string list) prod option

  val parse_stmts : nat -> string list -> (stmt list, string list) prod option

  val parse_block : nat -> string list -> (stmt list, string list) prod option

  val parse_program : string -> stmt list option

  val infer_expr : ty Common.env -> expr -> ty option

  val default_stmt_fuel : nat

  val check_stmt_fuel : nat -> ty Common.env -> stmt -> ty Common.env option

  val check_stmts_fuel :
    nat -> ty Common.env -> stmt list -> ty Common.env option

  val check_stmts : ty Common.env -> stmt list -> ty Common.env option

  val check_block : ty Common.env -> stmt list -> unit0 option

  val typecheck_program : string -> unit0 option
 end
