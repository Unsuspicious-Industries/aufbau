(* The reference typechecker: OCaml's own, over expressions, via compiler-libs.
   This is the trusted side of the differential harness; aufbau's ML instance
   is certified against it. *)

let env =
  lazy
    (Compmisc.init_path ();
     Compmisc.initial_env ())

(* Does [src] parse and typecheck as an OCaml expression? *)
let typechecks src =
  match Parse.expression (Lexing.from_string src) with
  | exception _ -> false
  | parsed -> (
    try
      ignore (Typecore.type_expression (Lazy.force env) parsed);
      true
    with _ -> false)
