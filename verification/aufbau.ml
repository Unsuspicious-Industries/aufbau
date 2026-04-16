(* OCaml stubs for Rust FFI functions *)
(* These are implemented in the linked Rust staticlib *)

external parse_grammar : string -> (unit, string) result = "aufbau_parse_grammar"
external complete_k : string -> string -> int -> int -> string array = "aufbau_complete_k"
external complete : string -> string -> int -> string option = "aufbau_complete"
external check_prefix : string -> string -> bool = "aufbau_check_prefix"
external version : unit -> string = "aufbau_version"
external call_ocaml_callback : (string -> string) -> string -> (string, string) result = "aufbau_call_ocaml_callback"
