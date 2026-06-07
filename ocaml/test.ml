(* OCaml FFI tests — run by `make test` (and `dune runtest --root ocaml`). *)
open Aufbau
open Term
open Rewrite

let failed = ref 0

let case name cond =
  if cond then Printf.printf "ok   %s\n" name
  else (incr failed; Printf.printf "FAIL %s\n" name)

let arrow a b = con "Fun" [ a; b ]
let sum a b = con "Sum" [ a; b ]

let stlc =
  Grammar.make ~start:"Expr"
    ~rules:
      Grammar.[ rule "var"    ~premises:"x ∈ Γ"                        ~conclusion:"Γ(x)";
                rule "lambda" ~premises:"Γ[a:τ] ⊢ e : ?B"              ~conclusion:"τ -> ?B";
                rule "app"    ~premises:"Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A" ~conclusion:"?B" ]
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

  (* grammar construction + checking *)
  case "check identity" (Grammar.check stlc "λx:A.x" = "λx:A.x : A -> A");
  case "check curried const"
    (Grammar.check stlc "λx:A.λy:B.x" = "λx:A.λy:B.x : A -> B -> A");
  case "check rejects unbound"
    (let r = Grammar.check stlc "x" in
     String.length r >= 6 && String.sub r 0 6 = "error:");

  if !failed > 0 then (Printf.printf "\n%d test(s) failed\n" !failed; exit 1)
  else print_endline "\nall OCaml FFI tests passed"
