(** Generic differential certification harness. *)

open Aufbau

module type LANGUAGE = sig
  val name : string
  val grammar : Grammar.t
  val oracle : string -> bool
  val corpora :
    valid : string list ->
    invalid : string list ->
    beyond : string list ->
    unit
end

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

let show_report r =
  Printf.sprintf
    "\n\
     ─── %s certification ───\n\
     completeness: %s\n\
     valid:        %d/%d agree\n\
     invalid:      %d/%d agree\n\
     beyond:       %d/%d agree\n\
     failures:     %d\n"
    r.language
    (if r.complete then "certified (inhabited)" else "sound (uninhabited sorts)")
    r.valid_agree r.valid_total
    r.invalid_agree r.invalid_total
    r.beyond_agree r.beyond_total
    (List.length r.failures)

let check oracle grammar program =
  let aufbau_ok =
    match Check.run grammar program with Typed _ -> true | _ -> false
  in
  let oracle_ok = oracle program in
  (aufbau_ok, oracle_ok)

let rec run_cases oracle grammar programs kind =
  let failed = ref [] in
  let ok = ref 0 in
  List.iter
    (fun program ->
      let aufbau_ok, oracle_ok = check oracle grammar program in
      let label = kind ^ " " ^ program in
      let agree =
        match kind with
        | "agree+" | "valid" -> aufbau_ok && oracle_ok
        | "agree-" | "invalid" -> (not aufbau_ok) && not oracle_ok
        | "bound" | "beyond" -> (not aufbau_ok) && oracle_ok
        | _ -> aufbau_ok = oracle_ok
      in
      if agree then (
        incr ok;
        Printf.printf "  ok   %s\n" label)
      else (
        failed := label :: !failed;
        Printf.printf "  FAIL %s  (aufbau=%b oracle=%b)\n" label aufbau_ok
          oracle_ok))
    programs;
  (!ok, !failed)

module Make (L : LANGUAGE) = struct
  let run () =
    let g = L.grammar in
    let complete = Grammar.complete g in
    let valid_progs = ref [] in
    let invalid_progs = ref [] in
    let beyond_progs = ref [] in
    L.corpora ~valid:(fun l -> valid_progs := l)
      ~invalid:(fun l -> invalid_progs := l)
      ~beyond:(fun l -> beyond_progs := l);
    let valid_list = !valid_progs in
    let invalid_list = !invalid_progs in
    let beyond_list = !beyond_progs in
    print_endline ("\n─ " ^ L.name ^ " certification ─");
    print_endline (if complete then "completeness: inhabited"
                   else "completeness: sound (uninhabited sorts)");
    let valid_ok, valid_fail =
      run_cases L.oracle g valid_list "agree+"
    in
    let invalid_ok, invalid_fail =
      run_cases L.oracle g invalid_list "agree-"
    in
    let beyond_ok, beyond_fail =
      run_cases L.oracle g beyond_list "bound"
    in
    let failures = valid_fail @ invalid_fail @ beyond_fail in
    let report : report = {
      language = L.name;
      valid_total = List.length valid_list;
      valid_agree = valid_ok;
      invalid_total = List.length invalid_list;
      invalid_agree = invalid_ok;
      beyond_total = List.length beyond_list;
      beyond_agree = beyond_ok;
      complete;
      failures;
    } in
    List.iter (fun f -> Printf.eprintf "FAIL: %s\n" f) failures;
    print_endline (show_report report);
    report
end

(** Load corpus files: one program per line; [#] comments and blank
    lines are ignored. *)
let load_corpus path =
  let ic = open_in path in
  let lines = ref [] in
  (try
     while true do
       let line = input_line ic in
       let trimmed = String.trim line in
       if trimmed <> "" && trimmed.[0] <> '#' then lines := trimmed :: !lines
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !lines

(** Shell-out oracle: write program to a temp file, run [cmd tempfile],
    return [true] iff exit code is 0. *)
let shell_oracle cmd program =
  let tmp = Filename.temp_file "certify" ".tmp" in
  let oc = open_out tmp in
  output_string oc program;
  close_out oc;
  let status =
    Sys.command (Printf.sprintf "%s %s 2>/dev/null" cmd (Filename.quote tmp))
  in
  Sys.remove tmp;
  status = 0
