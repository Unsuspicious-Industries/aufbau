(** ML certification: aufbau vs OCaml's typechecker. *)

open Aufbau

(** Find [examples/ml.auf] by walking up from CWD. *)
let find_grammar () =
  let rec up dir =
    let candidate = Filename.concat dir "corpora/ml/invalid.txt" in
    (* walk up until corpora/ is found, then resolve from repo root *)
    let repo_candidate = Filename.concat dir "examples/ml.auf" in
    if Sys.file_exists repo_candidate then repo_candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith "examples/ml.auf not found" else up parent
  in
  let path = up (Sys.getcwd ()) in
  let ic = open_in_bin path in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match Grammar.load src with Ok g -> g | Error e -> failwith e

let grammar = find_grammar ()

let oracle program =
  match Oracle.typechecks program with
  | true -> true
  | false -> false

let corpora ~valid ~invalid ~beyond =
  let dir =
    let rec up dir =
      let candidate = Filename.concat dir "corpora/ml" in
      if Sys.file_exists candidate then candidate
      else
        let parent = Filename.dirname dir in
        if parent = dir then failwith "corpora/ml not found" else up parent
    in
    up (Sys.getcwd ())
  in
  valid (Certify.load_corpus (Filename.concat dir "valid.txt"));
  invalid (Certify.load_corpus (Filename.concat dir "invalid.txt"));
  beyond (Certify.load_corpus (Filename.concat dir "beyond.txt"))
