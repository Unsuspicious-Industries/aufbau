(** Aufbau — the engine as native OCaml values.

    Everything is built inductively: type terms, rule type expressions,
    premises, rules, productions, whole grammars. Strings appear only as
    leaves (names, literal tokens, regexes) or through the optional [.auf]
    sugar ([Rule.parse], [Grammar.load]). A grammar is a persistent handle:
    assembled and validated once, checked many times. *)

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

  (** an oriented rewrite. *)
  val ( => ) : Term.t -> Term.t -> rule

  (** [normalize theory t] is the normal form of [t]. *)
  val normalize : theory -> Term.t -> Term.t
end

(** Unification. *)
module Unify : sig
  (** A solved substitution. *)
  type subst = (string * Term.t) list

  (** [unify a b] is the most general unifier, or [None] on clash. With
      [~modulo], equality is taken up to the theory. *)
  val unify : ?modulo:Rewrite.theory -> Term.t -> Term.t -> subst option

  val show : subst -> string
end

(** Rule-level type expressions: a sequence of atoms. Everything between
    meta-atoms is literal type syntax, parsed by the grammar's own type
    fragment; there is no built-in arrow or product. *)
module Texpr : sig
  type atom
  type t = atom list

  (** literal type text: a name or a separator. *)
  val lit : string -> atom

  (** [?A], a unification variable. *)
  val hole : string -> atom

  (** [typeof b]: the type of the child bound to [b]. *)
  val ref_ : string -> atom

  (** [Γ(x)]: the type bound to [x]'s text. *)
  val ctx : string -> atom

  (** [Γ(x)] with fresh variables (a scheme use). *)
  val inst : string -> atom

  (** [⊤], the unconstrained type. *)
  val top : atom

  (** [∅], the contradictory type. *)
  val bot : atom
end

(** Typing rules, built structurally or parsed from inference notation. *)
module Rule : sig
  type premise

  (** [ascribe ~under b t]: [Γ[under] ⊢ b : t]. The setting [under] is
      premise-local. *)
  val ascribe : ?under:(string * Texpr.t) list -> string -> Texpr.t -> premise

  (** [x ∈ Γ]. *)
  val member : string -> premise

  (** [τ₁ = τ₂]. *)
  val equate : Texpr.t -> Texpr.t -> premise

  type t

  (** [make name ?effects premises conclusion]. [effects] are right-bound
      context extensions ([Γ → Γ[x:τ]]), exported to later siblings. *)
  val make : string -> ?effects:(string * Texpr.t) list -> premise list -> Texpr.t -> t

  (** The inference-notation sugar, exactly the [.auf] surface. *)
  val parse : string -> premises:string -> conclusion:string -> t
end

(** Grammars — built structurally, validated on assembly. *)
module Grammar : sig
  (** A grammar symbol. *)
  type symbol

  (** a nonterminal. *)
  val nt : ?bind:string -> string -> symbol

  (** a literal token. *)
  val lit : string -> symbol

  (** a regex terminal. *)
  val re : ?bind:string -> string -> symbol

  (** A nonterminal definition. *)
  type def

  (** [def ?rule name alts] defines [name] by its alternatives, optionally
      attaching a typing [rule]. *)
  val def : ?rule:string -> string -> symbol list list -> def

  (** A semantic prefix grammar: a persistent, validated handle. *)
  type t

  (** Assemble a grammar. [ty] designates the type fragment (the [Ty*] of the
      surface syntax); [start] defaults to the last definition. Fails when a
      rule pattern has no unique parse at the type sort, or a rewrite is
      ill-formed: the same validation as loading [.auf] source. *)
  val make :
    ?start:string ->
    ?ty:string ->
    ?rules:Rule.t list ->
    ?rewrites:(string * string) list ->
    def list ->
    (t, string) result

  (** Load from [.auf] source. *)
  val load : string -> (t, string) result

  (** Render the handle back to [.auf] source. *)
  val show : t -> string

  (** The realizability class of a grammar. Pruning ([Check.run] returning
      [Dead]) is sound unconditionally; this classifies the converse, whether
      every [Live] prefix has a continuation. [Syntactic]: no typing rules,
      live ⇔ realizable. [Inhabited]: every sort an ascription can constrain
      has a universal inhabitant (a term typed at a bare hole, OCaml's
      [assert false]), so live ⇒ realizable. [Sound sorts]: a live prefix may
      demand an uninhabited type at the listed sorts. The certificate is
      sufficient, not necessary. *)
  type completeness =
    | Syntactic
    | Inhabited
    | Sound of string list

  val completeness : t -> completeness

  (** [complete g] iff the certificate guarantees live ⇒ realizable. *)
  val complete : t -> bool
end

(** Checking programs and prefixes. *)
module Check : sig
  (** [Typed t]: a complete parse with type [t]. [Live]: a completable
      prefix. [Dead e]: rejected, syntactically or semantically. *)
  type t = Typed of Term.t | Live | Dead of string

  val run : Grammar.t -> string -> t

  (** the type of a complete, well-typed parse. *)
  val typed : Grammar.t -> string -> Term.t option

  (** complete and typed. *)
  val accepts : Grammar.t -> string -> bool

  (** typed or still completable. *)
  val live : Grammar.t -> string -> bool

  val show : t -> string
end
