val parse_grammar : string -> (unit, string) result
val complete_k : string -> string -> int -> int -> string array
val complete : string -> string -> int -> string option
val check_prefix : string -> string -> bool
val version : unit -> string
val call_ocaml_callback : (string -> string) -> string -> (string, string) result
