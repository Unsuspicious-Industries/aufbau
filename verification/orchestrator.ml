type mode =
  | Prefix
  | Program
  | PrefixFile of string

type language_info = {
  spec_path : string;
  coq_import : string;
  coq_check_term : string;
}

type config = {
  mode : mode;
  language : string option;
  input_text : string option;
  count : int;
  depth : int option;
  jobs : int;
  verbose : bool;
  trace_search : bool;
}

type process_result = {
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

type coq_result = {
  accepted : bool;
  summary : string;
}

type completion_outcome = {
  completion : string;
  result : coq_result;
}

type batch_report = {
  index : int;
  language : string;
  prefix : string;
  outcomes : completion_outcome list;
  completion_error : string option;
  fatal_error : string option;
  log_lines : string list;
}

let trim = String.trim

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let split_lines text =
  let rec loop i j acc =
    if j = String.length text then
      List.rev (String.sub text i (j - i) :: acc)
    else if text.[j] = '\n' then
      loop (j + 1) (j + 1) (String.sub text i (j - i) :: acc)
    else
      loop i (j + 1) acc
  in
  if text = "" then [] else loop 0 0 []

let read_all path =
  let ic = open_in_bin path in
  Fun.protect
    ~finally:(fun () -> close_in ic)
    (fun () ->
      let len = in_channel_length ic in
      really_input_string ic len)

let read_stdin () =
  let buf = Buffer.create 256 in
  (try
     while true do
       Buffer.add_channel buf stdin 1024
     done
   with End_of_file -> ());
  Buffer.contents buf

let env_or_fail name =
  match Sys.getenv_opt name with
  | Some value -> value
  | None -> failwith ("missing required environment variable: " ^ name)

let root_dir () = env_or_fail "AUFBAU_ROOT"
let verification_dir () = env_or_fail "AUFBAU_VERIFICATION_DIR"

let coq_build_dir () = Filename.concat (verification_dir ()) "_build/default/coq"

let run_process ?(cwd = ".") ?stdin_data prog args =
  let stdout_path = Filename.temp_file "aufbau_stdout_" ".log" in
  let stderr_path = Filename.temp_file "aufbau_stderr_" ".log" in
  let stdin_r, stdin_w = Unix.pipe () in
  let pid = Unix.fork () in
  if pid = 0 then (
    let stdout_fd = Unix.openfile stdout_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o644 in
    let stderr_fd = Unix.openfile stderr_path [ Unix.O_WRONLY; Unix.O_TRUNC ] 0o644 in
    Unix.chdir cwd;
    Unix.dup2 stdin_r Unix.stdin;
    Unix.dup2 stdout_fd Unix.stdout;
    Unix.dup2 stderr_fd Unix.stderr;
    Unix.close stdin_w;
    Unix.close stdin_r;
    Unix.close stdout_fd;
    Unix.close stderr_fd;
    Unix.execvp prog (Array.of_list (prog :: args))
  );
  Unix.close stdin_r;
  (match stdin_data with
  | Some input ->
      let bytes = Bytes.of_string input in
      let rec write_loop off remaining =
        if remaining > 0 then
          let written = Unix.write stdin_w bytes off remaining in
          write_loop (off + written) (remaining - written)
      in
      write_loop 0 (Bytes.length bytes)
  | None -> ());
  Unix.close stdin_w;
  let _, status = Unix.waitpid [] pid in
  let stdout = read_all stdout_path in
  let stderr = read_all stderr_path in
  Sys.remove stdout_path;
  Sys.remove stderr_path;
  { status; stdout; stderr }

let run_process_streaming ?(cwd = ".") ?stdin_data prog args =
  let stdin_r, stdin_w = Unix.pipe () in
  let stdout_r, stdout_w = Unix.pipe () in
  let stderr_r, stderr_w = Unix.pipe () in
  let pid = Unix.fork () in
  if pid = 0 then (
    Unix.chdir cwd;
    Unix.dup2 stdin_r Unix.stdin;
    Unix.dup2 stdout_w Unix.stdout;
    Unix.dup2 stderr_w Unix.stderr;
    Unix.close stdin_w;
    Unix.close stdin_r;
    Unix.close stdout_r;
    Unix.close stdout_w;
    Unix.close stderr_r;
    Unix.close stderr_w;
    Unix.execvp prog (Array.of_list (prog :: args))
  );
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  (match stdin_data with
  | Some input ->
      let bytes = Bytes.of_string input in
      let rec write_loop off remaining =
        if remaining > 0 then
          let written = Unix.write stdin_w bytes off remaining in
          write_loop (off + written) (remaining - written)
      in
      write_loop 0 (Bytes.length bytes)
  | None -> ());
  Unix.close stdin_w;
  let stdout_buffer = Buffer.create 4096 in
  let stderr_buffer = Buffer.create 4096 in
  let temp = Bytes.create 4096 in
  let rec pump stdout_open stderr_open =
    if (not stdout_open) && not stderr_open then ()
    else
      let fds =
        (if stdout_open then [ stdout_r ] else []) @ if stderr_open then [ stderr_r ] else []
      in
      let ready, _, _ = Unix.select fds [] [] (-1.0) in
      let stdout_open =
        if stdout_open && List.mem stdout_r ready then
          let read = Unix.read stdout_r temp 0 (Bytes.length temp) in
          if read = 0 then (
            Unix.close stdout_r;
            false
          ) else (
            let chunk = Bytes.sub_string temp 0 read in
            Buffer.add_string stdout_buffer chunk;
            output_string stdout chunk;
            flush stdout;
            true
          )
        else
          stdout_open
      in
      let stderr_open =
        if stderr_open && List.mem stderr_r ready then
          let read = Unix.read stderr_r temp 0 (Bytes.length temp) in
          if read = 0 then (
            Unix.close stderr_r;
            false
          ) else (
            let chunk = Bytes.sub_string temp 0 read in
            Buffer.add_string stderr_buffer chunk;
            output_string stderr chunk;
            flush stderr;
            true
          )
        else
          stderr_open
      in
      pump stdout_open stderr_open
  in
  pump true true;
  let _, status = Unix.waitpid [] pid in
  { status; stdout = Buffer.contents stdout_buffer; stderr = Buffer.contents stderr_buffer }

let process_ok = function
  | Unix.WEXITED 0 -> true
  | _ -> false

let fail_process label result =
  let details =
    [
      Some ("error: " ^ label);
      (if trim result.stderr <> "" then Some ("stderr:\n" ^ result.stderr) else None);
      (if trim result.stdout <> "" then Some ("stdout:\n" ^ result.stdout) else None);
    ]
    |> List.filter_map (fun x -> x)
    |> String.concat "\n"
  in
  failwith details

let command_exists name =
  let result = run_process "which" [ name ] in
  process_ok result.status

let language_info = function
  | "stlc" ->
      {
        spec_path = Filename.concat (root_dir ()) "examples/stlc.auf";
        coq_import = "verification.coq.STLC";
        coq_check_term = "STLC.typecheck";
      }
  | "fun" ->
      {
        spec_path = Filename.concat (root_dir ()) "examples/fun.auf";
        coq_import = "verification.coq.Fun";
        coq_check_term = "FunLang.typecheck";
      }
  | "imp" ->
      {
        spec_path = Filename.concat (root_dir ()) "examples/imp.auf";
        coq_import = "verification.coq.Imp";
        coq_check_term = "ImpLang.typecheck_program";
      }
  | "typescript" ->
      {
        spec_path = Filename.concat (root_dir ()) "examples/typescript.auf";
        coq_import = "verification.coq.Typescript";
        coq_check_term = "TypescriptLang.typecheck_program";
      }
  | language -> failwith ("unknown language: " ^ language)

let coq_escape s =
  let buffer = Buffer.create (String.length s + 16) in
  String.iter
    (function
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '"' -> Buffer.add_string buffer "\\\""
      | '\n' -> Buffer.add_string buffer "\\n"
      | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

let log ~verbose module_name message =
  if verbose then prerr_endline (Printf.sprintf "log> %s: %s" module_name message)

let reverify_coq ~verbose =
  if not (command_exists "dune") then (
    prerr_endline "error: dune is required to build Rocq artifacts";
    exit 1
  );
  prerr_endline "Re-verifying Rocq modules...";
  (* reverify log *)
  log ~verbose "reverify" "streaming Rocq build output";
  let runner = if verbose then run_process_streaming else run_process in
  let result = runner ~cwd:(verification_dir ()) "dune" [ "build"; "@coq" ] in
  if not (process_ok result.status) then fail_process "failed to build Rocq artifacts" result

let default_search_depth = 20

let target_depth config =
  match config.depth with
  | Some value -> max 1 value
  | None -> default_search_depth

let rec depth_schedule acc current target =
  if current >= target then List.rev (target :: acc)
  else
    let next = min target (max (current + 1) (current * 2)) in
    depth_schedule (current :: acc) next target

let depth_attempts target = depth_schedule [] 2 target

(* New FFI-based complete_k - direct call to Rust, no subprocess *)
let complete_k_ffi ~verbose ~language ~prefix ~count ~depth =
  let info = language_info language in
  let spec_source = read_all info.spec_path in
  let attempts = depth_attempts depth in
  let rec try_depths = function
    | [] -> []
    | current_depth :: rest ->
        log ~verbose "complete-k-ffi"
          (Printf.sprintf "running Rust FFI complete-k for %s at depth=%d count=%d"
             language current_depth count);
        let completions =
          try Aufbau.complete_k spec_source prefix current_depth count |> Array.to_list
          with exn ->
            failwith
              ("Rust FFI complete_k crashed: " ^ Printexc.to_string exn
             ^ " language=" ^ language ^ " depth=" ^ string_of_int current_depth
             ^ " count=" ^ string_of_int count)
        in
        if completions <> [] then (
          log ~verbose "complete-k-ffi"
            (Printf.sprintf "Rust FFI returned %d completions" (List.length completions));
          completions
        ) else if rest <> [] then (
          log ~verbose "complete-k-ffi"
            (Printf.sprintf "depth=%d returned no completions, trying deeper budget"
               current_depth);
          try_depths rest
        ) else
          []
  in
  let completions = try_depths attempts in
  log ~verbose "complete-k-ffi"
    (Printf.sprintf "final completion count=%d" (List.length completions));
  completions

let summarize_chunk chunk =
  split_lines chunk
  |> List.map trim
  |> List.filter (fun line -> starts_with ~prefix:"=" line)
  |> String.concat " "

let accepted_chunk chunk =
  let re = Str.regexp "= Some\\b" in
  try
    ignore (Str.search_forward re chunk 0);
    true
  with Not_found -> false

let run_coq_batch ~language programs =
  let info = language_info language in
  let script_lines =
    [
      "Require Import Corelib.Strings.PrimStringAxioms.";
      "Require Import " ^ info.coq_import ^ ".";
      "Open Scope pstring_scope.";
    ]
    @ List.mapi
        (fun idx program ->
          let escaped = coq_escape program in
          Printf.sprintf
            "Goal True. idtac \"BEGIN%d\". Eval vm_compute in (%s \"%s\"). idtac \"END%d\". exact I. Qed."
            idx info.coq_check_term escaped idx)
        programs
  in
  let temp_path = Filename.temp_file "aufbau_coq_" ".v" in
  let oc = open_out_bin temp_path in
  output_string oc (String.concat "\n" script_lines);
  close_out oc;
  let result =
    run_process ~cwd:(verification_dir ()) "coqtop"
      [
        "-quiet";
        "-batch";
        "-Q";
        coq_build_dir ();
        "verification.coq";
        "-load-vernac-source";
        temp_path;
      ]
  in
  Sys.remove temp_path;
  if not (process_ok result.status) then fail_process "coqtop batch check failed" result;
  List.mapi
    (fun idx _program ->
      let begin_marker = Printf.sprintf "BEGIN%d" idx in
      let end_marker = Printf.sprintf "END%d" idx in
      let begin_pos = try Some (Str.search_forward (Str.regexp_string begin_marker) result.stdout 0) with Not_found -> None in
      let end_pos =
        match begin_pos with
        | None -> None
        | Some pos ->
            (try Some (Str.search_forward (Str.regexp_string end_marker) result.stdout pos) with Not_found -> None)
      in
      let chunk =
        match begin_pos, end_pos with
        | Some b, Some e -> String.sub result.stdout b (e - b)
        | _ -> ""
      in
      { accepted = accepted_chunk chunk; summary = summarize_chunk chunk })
    programs

let parse_prefix_entry line =
  match String.index_opt line '|' with
  | None -> None
  | Some idx ->
      let language = String.sub line 0 idx |> trim in
      let prefix = String.sub line (idx + 1) (String.length line - idx - 1) in
      Some (language, prefix)

let lines_of_file path = read_all path |> split_lines

let batch_report_of_prefix ~index ~language ~prefix ~count ~depth ~verbose ~trace_search:_trace_search =
  try
    let completions = complete_k_ffi ~verbose ~language ~prefix ~count ~depth in
    if completions = [] then
      {
        index;
        language;
        prefix;
        outcomes = [];
        completion_error = Some "no completions returned";
        fatal_error = None;
        log_lines = [ "generated 0 completions" ];
      }
    else
      let results = run_coq_batch ~language completions in
      let outcomes = List.map2 (fun completion result -> { completion; result }) completions results in
      {
        index;
        language;
        prefix;
        outcomes;
        completion_error = None;
        fatal_error = None;
        log_lines =
          [
            Printf.sprintf "generated %d completions" (List.length completions);
            Printf.sprintf "checked %d completions with Rocq" (List.length outcomes);
          ];
      }
  with
  | Failure message ->
      { index; language; prefix; outcomes = []; completion_error = None; fatal_error = Some message; log_lines = [] }
  | Sys_error message ->
      { index; language; prefix; outcomes = []; completion_error = None; fatal_error = Some message; log_lines = [] }

let default_jobs () =
  match Sys.getenv_opt "AUFBAU_VERIFY_JOBS" with
  | Some value -> max 1 (int_of_string value)
  | None -> 4

let run_parallel jobs entries worker =
  let jobs = max 1 jobs in
  if jobs > 1 then
    prerr_endline
      "warning: parallel verify workers are temporarily disabled due unstable fork+FFI behavior; running sequentially";
  List.map worker entries

let print_batch_report ~verbose report =
  print_endline "============================================================";
  Printf.printf "language: %s\n" report.language;
  Printf.printf "prefix: %s\n" report.prefix;
  if verbose then
    List.iter (fun line -> Printf.printf "log: %s\n" line) report.log_lines;
  (match report.fatal_error with
  | Some message -> Printf.printf "fatal-error: %s\n" message
  | None -> ());
  (match report.completion_error with
  | Some message -> Printf.printf "status: %s\n" message
  | None ->
      List.iteri
        (fun idx outcome ->
          Printf.printf "completion[%d]: %s\n" (idx + 1) outcome.completion;
          Printf.printf "coq-result[%d]: %s\n" (idx + 1) outcome.result.summary;
          if outcome.result.accepted then
            Printf.printf "status[%d]: accepted by Rocq verifier\n" (idx + 1)
          else
            Printf.printf "status[%d]: rejected by Rocq verifier\n" (idx + 1))
        report.outcomes)

let summarize_reports reports =
  let queued = List.length reports in
  let accepted = ref 0 in
  let rejected = ref 0 in
  let completion_errors = ref 0 in
  let fatal_errors = ref 0 in
  List.iter
    (fun report ->
      if report.fatal_error <> None then incr fatal_errors
      else if report.completion_error <> None then incr completion_errors
      else
        List.iter
          (fun outcome -> if outcome.result.accepted then incr accepted else incr rejected)
          report.outcomes)
    reports;
  (queued, !accepted, !rejected, !completion_errors, !fatal_errors)

let run_single (config : config) =
  let language =
    match config.language with
    | Some value -> value
    | None -> failwith "single-check mode requires a language"
  in
  let input_text =
    match config.input_text with
    | Some value -> value
    | None -> failwith "missing input text"
  in
  let completion, label =
    match config.mode with
    | Program ->
        prerr_endline "Checking provided complete program with Rocq...";
        (input_text, "program")
    | Prefix ->
        prerr_endline "Completing prefix with aufbau...";
        let completions =
          complete_k_ffi ~verbose:config.verbose ~language ~prefix:input_text
            ~count:config.count ~depth:(target_depth config)
        in
        (match completions with
        | completion :: _ -> (completion, "prefix")
        | [] ->
            Printf.printf "language: %s\n" language;
            Printf.printf "prefix: %s\n" input_text;
            log ~verbose:config.verbose "verify-single" "no completion found; skipping Rocq check";
            print_endline "status: no completions returned";
            exit 1)
    | PrefixFile _ -> failwith "unexpected prefix-file mode in run_single_check"
  in
  if config.verbose then (
    Printf.printf "log: mode=%s\n" (if config.mode = Program then "program" else "prefix");
    Printf.printf "log: language=%s\n" language
  );
  prerr_endline "Running verified Rocq checker...";
  let result = List.hd (run_coq_batch ~language [ completion ]) in
  Printf.printf "language: %s\n" language;
  Printf.printf "%s: %s\n" label input_text;
  Printf.printf "completion: %s\n" completion;
  Printf.printf "coq-result: %s\n" result.summary;
  if config.verbose then Printf.printf "log: accepted=%b\n" result.accepted;
  if result.accepted then (
    print_endline "status: accepted by Rocq verifier";
    exit 0
  ) else (
    prerr_endline "status: rejected by Rocq verifier";
    exit 1
  )

let run_prefix_file (config : config) path =
  let input_errors = ref 0 in
  let work = ref [] in
  let next_index = ref 0 in
  List.iter
    (fun raw_line ->
      let line = trim raw_line in
      if line <> "" && not (starts_with ~prefix:"#" line) then
        match parse_prefix_entry line with
        | None ->
            incr input_errors;
            prerr_endline ("error: invalid prefix entry '" ^ line ^ "' (expected language|prefix)")
        | Some (language, prefix) ->
            let index = !next_index in
            incr next_index;
            work := (index, language, prefix) :: !work)
    (lines_of_file path);
  let reports =
    run_parallel config.jobs (List.rev !work) (fun (index, language, prefix) ->
        batch_report_of_prefix ~index ~language ~prefix ~count:config.count
          ~depth:(target_depth config) ~verbose:config.verbose
          ~trace_search:config.trace_search)
    |> List.sort (fun left right -> Int.compare left.index right.index)
  in
  List.iter (print_batch_report ~verbose:config.verbose) reports;
  let queued, accepted, rejected, completion_errors, fatal_errors = summarize_reports reports in
  print_endline "============================================================";
  Printf.printf "queued prefixes: %d\n" queued;
  Printf.printf "accepted completions: %d\n" accepted;
  Printf.printf "rejected completions: %d\n" rejected;
  Printf.printf "completion errors: %d\n" completion_errors;
  Printf.printf "fatal errors: %d\n" fatal_errors;
  Printf.printf "input errors: %d\n" !input_errors;
  if fatal_errors > 0 || !input_errors > 0 then (
    prerr_endline "status: verification failed (fatal/input errors)";
    exit 1
  ) else if rejected > 0 then (
    prerr_endline "status: verification failed (rejected completions by Rocq)";
    exit 1
  ) else if completion_errors > 0 then (
    prerr_endline "status: verification incomplete (completion errors only)";
    exit 1
  ) else (
    print_endline "status: all prefix completions accepted by the Rocq verifier";
    exit 0
  )

let usage () =
  prerr_endline "Usage:";
  prerr_endline "  verification/check.sh <language> [prefix]";
  prerr_endline "  printf '%s' '<prefix>' | verification/check.sh <language>";
  prerr_endline "  verification/check.sh --program <language> [program]";
  prerr_endline "  verification/check.sh -f <file> [--count N] [--depth N] [--jobs N] [--verbose] [--trace-search]";
  prerr_endline "";
  prerr_endline "Languages: stlc, fun, imp, typescript";
  exit 2

let parse_args () =
  let program_mode = ref false in
  let prefix_file = ref None in
  let count = ref 3 in
  let depth = ref None in
  let jobs = ref (default_jobs ()) in
  let verbose = ref false in
  let trace_search = ref false in
  let positionals = ref [] in
  let rec loop i =
    if i >= Array.length Sys.argv then ()
    else
      match Sys.argv.(i) with
      | "--program" ->
          program_mode := true;
          loop (i + 1)
      | "-f" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          prefix_file := Some Sys.argv.(i + 1);
          loop (i + 2)
      | "--count" | "-k" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          count := int_of_string Sys.argv.(i + 1);
          loop (i + 2)
      | "--depth" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          depth := Some (int_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | "--jobs" | "-j" ->
          if i + 1 >= Array.length Sys.argv then usage ();
          jobs := max 1 (int_of_string Sys.argv.(i + 1));
          loop (i + 2)
      | "--verbose" ->
          verbose := true;
          loop (i + 1)
      | "--trace-search" ->
          trace_search := true;
          verbose := true;
          loop (i + 1)
      | "-h" | "--help" -> usage ()
      | arg ->
          positionals := !positionals @ [ arg ];
          loop (i + 1)
  in
  loop 1;
  match !prefix_file with
  | Some file ->
      {
        mode = PrefixFile file;
        language = None;
        input_text = None;
        count = !count;
        depth = !depth;
        jobs = !jobs;
        verbose = !verbose;
        trace_search = !trace_search;
      }
  | None ->
      let positionals = !positionals in
      let language, text =
        match positionals with
        | [] -> usage ()
        | language :: rest ->
            let text = if rest = [] then trim (read_stdin ()) else String.concat " " rest in
            (language, text)
      in
      if text = "" then usage ();
      {
        mode = if !program_mode then Program else Prefix;
        language = Some language;
        input_text = Some text;
        count = !count;
        depth = !depth;
        jobs = !jobs;
        verbose = !verbose;
        trace_search = !trace_search;
      }
let () =
  try
    let config = parse_args () in
    reverify_coq ~verbose:config.verbose;
    match config.mode with
    | Prefix | Program -> run_single config
    | PrefixFile path -> run_prefix_file config path
  with
  | Failure message ->
      prerr_endline ("error: " ^ message);
      exit 2
  | Sys_error message ->
      prerr_endline ("error: " ^ message);
      exit 2
