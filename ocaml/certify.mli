(** Generic differential certification harness.

    A certification language L defines how to load the grammar, oracle,
    and corpora.  [Certify.Make(L)] produces a runnable checker that:

    1. Loads the grammar via [L.grammar].
    2. Loads corpora (valid, invalid, beyond) via [L.corpora].
    3. For each program, checks that aufbau and [L.oracle] agree.

    The beyond list documents known fragment boundaries (sound
    incompleteness: aufbau rejects, oracle accepts). *)

(** The interface a language must provide. *)
module type LANGUAGE = sig
  val name : string
  (** Human-readable language name (for diagnostics). *)

  val grammar : Aufbau.Grammar.t
  (** The aufbau grammar for this language. *)

  val oracle : string -> bool
  (** The trusted reference checker.  [true] means the program is
      well-typed (or compilable).  For fragment boundaries where aufbau
      rejects but the oracle accepts, use {!beyond}. *)

  val corpora :
    valid : string list ->
    invalid : string list ->
    beyond : string list ->
    unit
  (** Load corpora: feeds the program lists back, so the implementer
      can load them from files or embed them inline.  The three
      categories are:

      - {e valid}: programs aufbau and the oracle agree are well-typed.
      - {e invalid}: both agree are rejected.
      - {e beyond}: oracle accepts but aufbau rejects — the documented
        boundary between the monomorphic fragment and the full
        language's type system. *)
end

(** Certificate of a complete run. *)
type report = {
  language : string;
  valid_total : int;
  valid_agree : int;
  invalid_total : int;
  invalid_agree : int;
  beyond_total : int;
  beyond_agree : int;
  complete : bool;
  failures : string list;
}

val show_report : report -> string
(** Human-readable summary. *)

module Make (L : LANGUAGE) : sig
  val run : unit -> report
  (** Run the full certification suite.  Prints progress and returns
      the report.  Exit code is [1] on any disagreement. *)
end

(** Convenience: load a corpus from newline-separated text files.
    Lines that are empty or start with [#] are ignored. *)
val load_corpus : string -> string list

(** Build a simple oracle that shells out to an external command.
    [shell_oracle cmd program] writes [program] to a temp file, runs
    [cmd tempfile], and returns [true] iff the command exits 0. *)
val shell_oracle : string -> string -> bool
