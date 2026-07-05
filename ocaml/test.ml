(* OCaml FFI tests — run by `make test-ocaml` (and `dune runtest --root ocaml`). *)
open Aufbau
open Term
open Rewrite

let failed = ref 0

let case name cond =
  if cond then Printf.printf "ok   %s\n" name
  else (incr failed; Printf.printf "FAIL %s\n" name)

let arrow a b = con "Fun" [ a; b ]
let sum a b = con "Sum" [ a; b ]

let stlc_defs =
  Grammar.[
    def "Identifier" [ [ re "[a-z]+" ] ];
    def "TypeName"   [ [ re "[A-Za-z0-9]+" ] ];
    def "Atom"       [ [ nt "TypeName" ]; [ lit "("; nt "Type"; lit ")" ] ];
    def "Type"       [ [ nt "Atom" ]; [ nt "Atom"; lit "->"; nt "Type" ] ];
    def ~rule:"var" "Variable" [ [ nt ~bind:"x" "Identifier" ] ];
    def ~rule:"lambda" "Lambda"
      [ [ lit "λ"; nt ~bind:"a" "Identifier"; lit ":"; nt ~bind:"τ" "Type";
          lit "."; nt ~bind:"e" "Expr" ] ];
    def "AtomE" [ [ nt "Variable" ]; [ lit "("; nt "Expr"; lit ")" ] ];
    def ~rule:"app" "Application" [ [ nt ~bind:"l" "Expr"; nt ~bind:"r" "AtomE" ] ];
    def "Expr" [ [ nt "AtomE" ]; [ nt "Lambda" ]; [ nt "Application" ] ] ]

(* The same rules twice: parsed from the inference notation, and built
   structurally. The two grammars must agree everywhere. *)
let parsed_rules =
  Rule.[
    parse "var"    ~premises:"x ∈ Γ"                        ~conclusion:"Γ(x)";
    parse "lambda" ~premises:"Γ[a:τ] ⊢ e : ?B"              ~conclusion:"τ -> ?B";
    parse "app"    ~premises:"Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A" ~conclusion:"?B" ]

let made_rules =
  let open Rule in
  let open Texpr in
  [ make "var" [ member "x" ] [ ctx "x" ];
    make "lambda"
      [ ascribe ~under:[ ("a", [ ref_ "τ" ]) ] "e" [ hole "B" ] ]
      [ ref_ "τ"; lit " -> "; hole "B" ];
    make "app"
      [ ascribe "l" [ hole "A"; lit " -> "; hole "B" ]; ascribe "r" [ hole "A" ] ]
      [ hole "B" ] ]

let grammar rules =
  match Grammar.make ~start:"Expr" ~ty:"Type" ~rules stlc_defs with
  | Ok g -> g
  | Error e -> failwith e

let stlc = grammar parsed_rules
let stlc_made = grammar made_rules

let shows g p =
  match Check.run g p with
  | Typed t -> "typed " ^ Term.show t
  | Live -> "live"
  | Dead _ -> "dead"

let () =
  let a, b = (var "A", var "B") in
  let int, bool, unit = (leaf "Int", leaf "Bool", leaf "Unit") in

  (* unification *)
  case "unify arrow"
    (Unify.unify (arrow a b) (arrow int bool) = Some [ ("A", int); ("B", bool) ]);
  case "unify clash" (Unify.unify (arrow int bool) (arrow int unit) = None);
  case "unify captures subtree"
    (Unify.unify (arrow a b) (arrow int (arrow int bool))
     = Some [ ("A", int); ("B", arrow int bool) ]);
  case "unify identical vars" (Unify.unify a a = Some []);

  (* normalization / rewrite theory *)
  let theory = [ bool => sum unit unit ] in
  case "normalize synonym" (normalize theory bool = sum unit unit);
  case "normalize under constructor"
    (normalize theory (arrow bool a) = arrow (sum unit unit) a);
  case "free unify clashes" (Unify.unify bool (sum unit unit) = None);
  case "modulo unify succeeds" (Unify.unify ~modulo:theory bool (sum unit unit) = Some []);

  (* grammar handle + structured verdicts *)
  case "typed identity"
    (match Check.typed stlc "λx:A.x" with
     | Some t -> Term.show t = "Type(A, A)"
     | None -> false);
  case "typed curried const"
    (match Check.typed stlc "λx:A.λy:B.x" with
     | Some t -> Term.show t = "Type(A, Type(B, A))"
     | None -> false);
  case "live prefix" (Check.run stlc "λx:A." = Live);
  case "dead unbound"
    (match Check.run stlc "x" with Dead _ -> true | _ -> false);

  (* structural rules ≡ parsed rules *)
  List.iter
    (fun p -> case ("made = parsed on " ^ p) (shows stlc p = shows stlc_made p))
    [ "λx:A.x"; "λx:A.λy:B.x"; "λx:A."; "x"; "λf:A->B.λx:A.f(x)" ];

  (* round-trip: the structural grammar renders to loadable .auf *)
  case "show then load"
    (match Grammar.load (Grammar.show stlc) with
     | Ok g -> shows g "λx:A.x" = shows stlc "λx:A.x"
     | Error _ -> false);

  if !failed > 0 then (Printf.printf "\n%d test(s) failed\n" !failed; exit 1)
  else print_endline "\nall OCaml FFI tests passed"
