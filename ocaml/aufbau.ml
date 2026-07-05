(* Internal representations shared with the Rust engine across the FFI.
   Variant order must match the Rust declarations in src/ffi/ocaml/. *)
type term = Var of string | Con of string * term array | Leaf of string

type gsym =
  | Nt  of string * string option
  | Lit of string * string option
  | Re  of string * string option

type gdef = string * string option * gsym array array

module Texpr = struct
  type atom =
    | Lit  of string
    | Hole of string
    | Ref  of string
    | Ctx  of string
    | Inst of string
    | Top
    | Bot

  type t = atom list

  let lit s = Lit s
  let hole s = Hole s
  let ref_ s = Ref s
  let ctx s = Ctx s
  let inst s = Inst s
  let top = Top
  let bot = Bot
end

type gpremise =
  | PAscribe of (string * Texpr.atom array) array * string * Texpr.atom array
  | PMember  of string
  | PEquate  of Texpr.atom array * Texpr.atom array

type grule =
  | RParsed of string * string * string
  | RMade   of string * gpremise array * (string * Texpr.atom array) array * Texpr.atom array

type gram

(* built only by the FFI *)
type built = Built of gram | Invalid of string [@@warning "-37"]

type verdict = Typed of term | Live | Dead of string [@@warning "-37"]

(* The raw boundary: the engine speaks arrays; the modules below speak lists. *)
module Ffi = struct
  external unify : term -> term -> (string * term) array option = "aufbau_unify"

  external modulo :
    (term * term) array -> term -> term -> (string * term) array option
    = "aufbau_unify_modulo"

  external normalize : (term * term) array -> term -> term = "aufbau_normalize"

  external make :
    gdef array -> grule array -> (string * string) array
    -> string option -> string option -> built
    = "aufbau_grammar"

  external load : string -> built = "aufbau_load"
  external show : gram -> string = "aufbau_show"
  external check : gram -> string -> verdict = "aufbau_check"
  external completeness : gram -> string * string array = "aufbau_completeness"
end

module Term = struct
  type t = term = Var of string | Con of string * t array | Leaf of string

  let var name = Var name
  let leaf name = Leaf name
  let con head kids = Con (head, Array.of_list kids)

  let rec show = function
    | Var x  -> "?" ^ x
    | Leaf x -> x
    | Con (f, ks) ->
      f ^ "(" ^ String.concat ", " (List.map show (Array.to_list ks)) ^ ")"
end

module Rewrite = struct
  type rule = term * term
  type theory = rule list

  let ( => ) lhs rhs : rule = (lhs, rhs)
  let normalize theory t = Ffi.normalize (Array.of_list theory) t
end

module Unify = struct
  type subst = (string * term) list

  let unify ?(modulo = []) a b =
    let solved =
      match modulo with
      | [] -> Ffi.unify a b
      | theory -> Ffi.modulo (Array.of_list theory) a b
    in
    Option.map Array.to_list solved

  let show s =
    "{" ^ String.concat "; " (List.map (fun (x, t) -> x ^ " = " ^ Term.show t) s) ^ "}"
end

module Rule = struct
  type premise = gpremise

  let texpr (t : Texpr.t) = Array.of_list t
  let pairs ps = Array.of_list (List.map (fun (x, tx) -> (x, texpr tx)) ps)

  let ascribe ?(under = []) b t = PAscribe (pairs under, b, texpr t)
  let member x = PMember x
  let equate l r = PEquate (texpr l, texpr r)

  type t = grule

  let make name ?(effects = []) premises conclusion =
    RMade (name, Array.of_list premises, pairs effects, texpr conclusion)

  let parse name ~premises ~conclusion = RParsed (name, premises, conclusion)
end

module Grammar = struct
  type symbol = gsym

  let nt ?bind name = Nt (name, bind)
  let lit token = Lit (token, None)
  let re ?bind pat = Re (pat, bind)

  type def = gdef

  let def ?rule name alts : def =
    (name, rule, Array.of_list (List.map Array.of_list alts))

  type t = gram

  let result = function Built g -> Ok g | Invalid e -> Error e

  let make ?start ?ty ?(rules = []) ?(rewrites = []) defs =
    result
      (Ffi.make (Array.of_list defs) (Array.of_list rules)
         (Array.of_list rewrites) start ty)

  let load src = result (Ffi.load src)
  let show = Ffi.show

  type completeness =
    | Syntactic
    | Inhabited
    | Sound of string list

  let completeness g =
    match Ffi.completeness g with
    | "syntactic", _ -> Syntactic
    | "inhabited", _ -> Inhabited
    | _, sorts -> Sound (Array.to_list sorts)

  let complete g = match completeness g with Sound _ -> false | _ -> true
end

module Check = struct
  type t = verdict = Typed of term | Live | Dead of string

  let run = Ffi.check

  let typed g program =
    match run g program with Typed t -> Some t | Live | Dead _ -> None

  let accepts g program =
    match run g program with Typed _ -> true | Live | Dead _ -> false

  let live g prefix =
    match run g prefix with Typed _ | Live -> true | Dead _ -> false

  let show = function
    | Typed t -> "typed: " ^ Term.show t
    | Live -> "live"
    | Dead e -> "dead: " ^ e
end
