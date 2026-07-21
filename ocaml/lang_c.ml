(** C certification: aufbau vs [cc -fsyntax-only]. *)

open Aufbau

(** Find [examples/c.auf] by walking up from CWD. *)
let find_grammar () =
  let rec up dir =
    let candidate = Filename.concat dir "examples/c.auf" in
    if Sys.file_exists candidate then candidate
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith "examples/c.auf not found" else up parent
  in
  let path = up (Sys.getcwd ()) in
  let ic = open_in_bin path in
  let src = really_input_string ic (in_channel_length ic) in
  close_in ic;
  match Grammar.load src with Ok g -> g | Error e -> failwith e

let grammar = find_grammar ()

(** The oracle: compile with [cc -fsyntax-only]. *)
let oracle program =
  Certify.shell_oracle "cc -fsyntax-only -Wall -Werror -xc -" program

let corpora ~valid ~invalid ~beyond =
  let dir =
    let rec up dir =
      let candidate = Filename.concat dir "corpora/c" in
      if Sys.file_exists candidate then candidate
      else
        let parent = Filename.dirname dir in
        if parent = dir then failwith "corpora/c not found" else up parent
    in
    up (Sys.getcwd ())
  in
  valid (Certify.load_corpus (Filename.concat dir "valid.txt"));
  invalid (Certify.load_corpus (Filename.concat dir "invalid.txt"));
  beyond []  (* no known boundary programs for C yet *)
