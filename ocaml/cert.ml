(* Differential certification of aufbau's ML instance (examples/ml.auf)
   against OCaml's own typechecker (Oracle, via compiler-libs).

   Two claims, over the corpus:
     soundness:  every program aufbau accepts, the oracle accepts;
     agreement:  on the monomorphic corpus, acceptance coincides exactly.
   The corpus mirrors src/validation/parseable/ml.rs, so the Rust suite and
   this harness pin the same programs from both sides of the boundary.

   ml.auf uses a strict OCaml-subset syntax (fun/(x:t)->e, lowercase types,
   = for equality, -> in match arms), so programs are fed directly to the
   oracle with no translation.

   The [beyond] list is the fragment boundary, found by this harness: OCaml
   generalizes the type of a nonexpansive match scrutinee, so a dead branch
   may use a pattern variable at a generic type; monomorphic ML rejects.
   There aufbau must reject while the oracle accepts, soundly inside. *)

open Aufbau

let failed = ref 0

let case name cond =
  if cond then Printf.printf "ok   %s\n" name
  else (incr failed; Printf.printf "FAIL %s\n" name)

(* examples/ml.auf, found by walking up from the dune sandbox. *)
let ml_grammar () =
  let rec up dir =
    let candidate = Filename.concat dir "examples/ml.auf" in
    if Sys.file_exists candidate then candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith "examples/ml.auf not found" else up parent
  in
  let path = up (Sys.getcwd ()) in
  let ic = open_in_bin path in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match Grammar.load src with Ok g -> g | Error e -> failwith e

let valid =
  [ "fun (x : int) -> x";
    "fun (x : int) -> fun (y : bool) -> x";
    "(fun (x : int) -> x)(5)";
    "let a : int = 5 in a";
    "let a : int = 5 in a + 1";
    "1 < 2";
    "if true then 1 else 2";
    "if 1 < 2 then 1 else 0";
    "(1, true)";
    "fst (1, true)";
    "snd (1, true)";
    "1 :: []";
    "1 :: 2 :: 3 :: []";
    "(1, true) :: []";
    "let xs : int list = 1 :: [] in xs";
    "let rec f : int -> int = fun (n : int) -> f(n) in f(0)";
    "assert false";
    "let a : int = assert false in a";
    "if true then 1 else assert false";
    "let f : int -> bool = assert false in f(0)";
    "match [] with [] -> 0 | h :: t -> h + 1";
    "let rec length : int list -> int = fun (xs : int list) -> match xs with [] -> 0 | h :: t -> 1 + length(t) in length(1 :: 2 :: 3 :: [])";
    "let rec sum : int list -> int = fun (xs : int list) -> match xs with [] -> 0 | h :: t -> h + sum(t) in sum(1 :: 2 :: [])";
    "let rec member : int list -> bool = fun (xs : int list) -> match xs with [] -> false | h :: t -> if h = 0 then true else member(t) in member(0 :: 1 :: [])";
    "let rec copy : int list -> int list = fun (xs : int list) -> match xs with [] -> [] | h :: t -> h :: copy(t) in copy(1 :: 2 :: [])" ]

let invalid =
  [ "fun (x : int) -> y";
    "1 + true";
    "if 1 then 2 else 3";
    "if true then 1 else false";
    "fst 5";
    "let a : bool = 5 in a";
    "true < 2";
    "1 :: true :: []";
    "1 :: 2";
    "let xs : bool list = 1 :: [] in xs";
    "match 1 :: [] with [] -> 0 | h :: t -> true";
    "match 5 with [] -> 0 | h :: t -> 1";
    "let rec f : int list -> int = fun (xs : int list) -> match xs with [] -> 0 | h :: t -> f(h) in f(1 :: [])" ]

(* Rejected by monomorphic ML, accepted by OCaml's generalization. *)
let beyond =
  [ "match [] with [] -> 0 | h :: t -> if h then 1 else h + 1" ]

let () =
  let g = ml_grammar () in
  (* `assert false` is the universal inhabitant, so the grammar certifies
     completeness: every live prefix is realizable, matching the oracle's
     own inhabitedness (divergence at every type). *)
  case "complete (inhabited via assert false)" (Grammar.complete g);
  List.iter
    (fun p ->
      let aufbau = Check.accepts g p in
      let oracle = Oracle.typechecks p in
      case ("agree+ " ^ p) (aufbau && oracle))
    valid;
  List.iter
    (fun p ->
      let aufbau = Check.accepts g p in
      let oracle = Oracle.typechecks p in
      (* soundness is the aufbau => oracle direction; agreement is both. *)
      case ("agree- " ^ p) ((not aufbau) && not oracle))
    invalid;
  List.iter
    (fun p ->
      let aufbau = Check.accepts g p in
      let oracle = Oracle.typechecks p in
      case ("bound  " ^ p) ((not aufbau) && oracle))
    beyond;
  if !failed > 0 then (Printf.printf "\n%d disagreement(s)\n" !failed; exit 1)
  else print_endline "\naufbau ≡ OCaml typechecker on the monomorphic ML corpus"
