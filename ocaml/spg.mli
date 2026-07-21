(** Convenience combinators for building SPGs structurally.

    Provides an alternative to {!Grammar.make}/{!Rule.make} with
    a lighter syntax using combinators and a builder pattern.

    Example::

        open Spg

        let stlc =
          grammar ~start:"Expr" ~ty:"Type" (fun d ->
            d
            (* productions *)
            |> prod "Identifier" [re "[a-z]+"]
            |> prod "TypeName"   [re "[A-Za-z0-9]+"]
            |> prod "Atom"       [nt "TypeName"; seq [lit "("; nt "Type"; lit ")"]]
            |> prod "Type"       [nt "Atom"; seq [nt "Atom"; lit "->"; nt "Type"]]
            |> prod ~rule:"var" "Variable" [nt ~bind:"x" "Identifier"]
            |> prod ~rule:"app" "Application" [nt ~bind:"l" "Expr"; nt ~bind:"r" "AtomE"]
            |> prod "AtomE"     [nt "Variable"; seq [lit "("; nt "Expr"; lit ")"]]
            |> prod "Expr"      [nt "AtomE"; nt "Variable"; nt "Application"]
            (* rules *)
            |> rule "var" [member "x"] @@ ctx "x"
            |> rule "app" [ascribe "l" (hole "A" ^ lit "->" ^ hole "B");
                           ascribe "r" (hole "A")] @@ hole "B")

    Use {!build} to finalize; {!run} to finalize and check.
*)

(** {2 Symbol constructors} *)

val nt : ?bind:string -> string -> Aufbau.Grammar.symbol
(** [nt name] is a reference to nonterminal [name] with optional binding. *)

val lit : string -> Aufbau.Grammar.symbol
(** [lit text] is a literal token. *)

val re : ?bind:string -> string -> Aufbau.Grammar.symbol
(** [re pattern] is a regex terminal. *)

val seq : Aufbau.Grammar.symbol list -> Aufbau.Grammar.symbol list
(** [seq syms] groups symbols into an alternative (identity, for readability). *)

(** {2 Type expression atoms} *)

type atom
val ( ^ ) : atom -> atom -> atom
(** [a ^ b] concatenates two type-expression atoms. *)

val hole : string -> atom
val ref_ : string -> atom
val ctx : string -> atom
val inst : string -> atom
val top : atom
val bot : atom
val lit_atom : string -> atom

(** {2 Premise constructors} *)

type premise
val ascribe : ?under:(string * atom) list -> string -> atom -> premise
val member : string -> premise
val equate : atom -> atom -> premise

(** {2 Builder} *)

type builder
(** A mutable SPG builder.  Use {!grammar} to create one, then chain
    {!prod} and {!rule} calls, then call {!build} or {!run}. *)

val grammar : ?start:string -> ?ty:string -> (builder -> builder) ->
  (Aufbau.Grammar.t, string) result
(** [grammar ~start ~ty f] creates a builder, passes it to [f], and
    finalizes.  Returns [Error _] if the grammar is invalid. *)

val prod : ?rule:string -> string -> Aufbau.Grammar.symbol list list -> builder -> builder
(** [prod ~rule name alts] adds a production definition. *)

val rule : string -> premise list -> atom -> builder -> builder
(** [rule name premises conclusion] adds a typing rule. *)

val build : builder -> (Aufbau.Grammar.t, string) result
(** Finalize the builder into a {!Aufbau.Grammar.t}. *)

val run : ?start:string -> ?ty:string -> (builder -> builder) -> Aufbau.Grammar.t
(** [run f] is [grammar f] with [Ok g -> g | Error e -> failwith e]. *)
