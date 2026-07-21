(** Convenience combinators for building SPGs structurally. *)

open Aufbau

(* ── Symbol constructors ────────────────────────────────────────────── *)

let nt ?bind name = Grammar.nt ?bind name
let lit s = Grammar.lit s
let re ?bind pat = Grammar.re ?bind pat
let seq syms = syms

(* ── Type-expression atoms ─────────────────────────────────────────── *)

type atom = Texpr.t  (* alias *)

let ( ^ ) a b = a @ b

let hole s = [Texpr.hole s]
let ref_ s = [Texpr.ref_ s]
let ctx s = [Texpr.ctx s]
let inst s = [Texpr.inst s]
let top = [Texpr.top]
let bot = [Texpr.bot]
let lit_atom s = [Texpr.lit s]

(* ── Premises ──────────────────────────────────────────────────────── *)

type premise = Rule.premise

let ascribe ?(under = []) b t = Rule.ascribe ~under b t
let member x = Rule.member x
let equate a b = Rule.equate a b

(* ── Builder ───────────────────────────────────────────────────────── *)

type builder = {
  mutable start : string option;
  mutable ty : string option;
  mutable prods : Grammar.def list;
  mutable rules : Rule.t list;
}

let grammar ?start ?ty f =
  let b = { start; ty; prods = []; rules = [] } in
  let _ = f b in
  build b

let prod ?rule name alts b =
  let def = Grammar.def ?rule name (List.map (fun alt -> alt) alts) in
  b.prods <- b.prods @ [def];
  b

let rule name premises conclusion b =
  let r = Rule.make name premises conclusion in
  b.rules <- b.rules @ [r];
  b

let build b =
  Grammar.make ?start:b.start ?ty:b.ty ~rules:b.rules b.prods

let run ?start ?ty f =
  match grammar ?start ?ty f with
  | Ok g -> g
  | Error e -> failwith e
