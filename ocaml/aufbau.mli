(** Aufbau — the type algebra as native OCaml values.

    A type is a term over a grammar signature, built inductively. Unification is
    first-order; a rewrite theory gives equality beyond syntax; a grammar is
    constructed structurally and checks programs. No strings as the medium of
    construction, no surface `.auf`. *)

(** Type terms. *)
module Term : sig
  (** A metavariable, a constructor over children, or a base leaf. *)
  type t =
    | Var  of string
    | Con  of string * t array
    | Leaf of string

  (** a metavariable [?x]. *)
  val var : string -> t

  (** a base type. *)
  val leaf : string -> t

  (** a constructor applied to children. *)
  val con : string -> t list -> t

  val show : t -> string
end

(** Rewrite theories — equality beyond syntax. *)
module Rewrite : sig
  (** An oriented equation [lhs ⇝ rhs]. *)
  type rule

  (** A theory: the equations defining type equality. *)
  type theory = rule list

  val ( => ) : Term.t -> Term.t -> rule    (** an oriented rewrite. *)

  (** [normalize theory t] is the normal form of [t]. *)
  val normalize : theory -> Term.t -> Term.t
end

(** Unification. *)
module Unify : sig
  (** A solved substitution. *)
  type subst = (string * Term.t) list

  (** [unify a b] is the most general unifier, or [None] on clash. With [~modulo],
      equality is taken up to the theory. *)
  val unify : ?modulo:Rewrite.theory -> Term.t -> Term.t -> subst option

  val show : subst -> string
end

(** Grammars — built structurally, then used to check programs. *)
module Grammar : sig
  (** A grammar symbol. *)
  type symbol

  (** a nonterminal, optionally binding a name. *)
  val nt : ?bind:string -> string -> symbol

  (** a literal token. *)
  val lit : string -> symbol

  (** a regex terminal, optionally binding a name. *)
  val re : ?bind:string -> string -> symbol

  (** A nonterminal definition. *)
  type def

  (** [def ?rule name alts] defines [name] by its alternatives, optionally
      attaching a typing [rule]. *)
  val def : ?rule:string -> string -> symbol list list -> def

  (** A typing rule. *)
  type rule

  (** [rule name ~premises ~conclusion] in the inference notation. *)
  val rule : string -> premises:string -> conclusion:string -> rule

  (** A semantic prefix grammar. *)
  type t

  (** Assemble a grammar from its definitions, rules, rewrites, and start symbol. *)
  val make :
    ?start:string ->
    ?rules:rule list ->
    ?rewrites:(string * string) list ->
    def list ->
    t

  (** [check g program] is [program : type], or [error: …]. *)
  val check : t -> string -> string
end
