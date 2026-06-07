(* Internal representations shared with the Rust engine across the FFI. *)
type term = Var of string | Con of string * term array | Leaf of string

type gsym =
  | Nt  of string * string option
  | Lit of string * string option
  | Re  of string * string option

type gdef = string * string option * gsym array array
type grule = string * string * string

type ggram = {
  defs     : gdef array;
  rules    : grule array;
  rewrites : (string * string) array;
  start    : string option;
}

(* The raw boundary: the engine speaks arrays; the modules below speak lists. *)
module Ffi = struct
  external unify : term -> term -> (string * term) array option = "aufbau_unify"

  external modulo :
    (term * term) array -> term -> term -> (string * term) array option
    = "aufbau_unify_modulo"

  external normalize : (term * term) array -> term -> term = "aufbau_normalize"

  external check :
    gdef array -> grule array -> (string * string) array -> string option -> string -> string
    = "aufbau_check"
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

module Grammar = struct
  type symbol = gsym

  let nt ?bind name = Nt (name, bind)
  let lit token = Lit (token, None)
  let re ?bind pat = Re (pat, bind)

  type def = gdef

  let def ?rule name alts : def =
    (name, rule, Array.of_list (List.map Array.of_list alts))

  type rule = grule

  let rule name ~premises ~conclusion : rule = (name, premises, conclusion)

  type t = ggram

  let make ?start ?(rules = []) ?(rewrites = []) defs =
    { defs = Array.of_list defs;
      rules = Array.of_list rules;
      rewrites = Array.of_list rewrites;
      start }

  let check g program = Ffi.check g.defs g.rules g.rewrites g.start program
end
