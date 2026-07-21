(** Differential certification runner.

    Instantiates the generic {!Certify} framework for each supported
    language and runs all certification suites.  A language is
    certified when its grammar's completeness class guarantees that
    aufbau's live/dead pruning agrees with the external oracle's
    accept/reject on the corpora. *)

module ML = Certify.Make (Lang_ml)
module C = Certify.Make (Lang_c)

let () =
  let r_ml = ML.run () in
  let r_c = C.run () in
  let total_failures =
    List.length r_ml.failures + List.length r_c.failures
  in
  print_endline "\n─── certification summary ───";
  print_endline (Certify.show_report r_ml);
  print_endline (Certify.show_report r_c);
  if total_failures > 0 then (
    Printf.printf "\n%d disagreement(s)\n" total_failures;
    exit 1)
  else print_endline "\nAll certifications passed."
