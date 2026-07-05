open Aufbau
open Term
open Rewrite

(* domain syntax, defined where it belongs — in the type system, not the engine *)
let ( @-> ) a b = con "Fun" [ a; b ]
let ( + ) a b = con "Sum" [ a; b ]

let a, b = (var "A", var "B")
let int, bool, unit = (leaf "Int", leaf "Bool", leaf "Unit")

(* STLC, fully structural: productions, rules, and type expressions as values.
   No .auf source, no inference-notation strings. *)
let stlc =
  let rules =
    let open Rule in
    let open Texpr in
    [ make "var" [ member "x" ] [ ctx "x" ];
      make "lambda"
        [ ascribe ~under:[ ("a", [ ref_ "τ" ]) ] "e" [ hole "B" ] ]
        [ ref_ "τ"; lit " -> "; hole "B" ];
      make "app"
        [ ascribe "l" [ hole "A"; lit " -> "; hole "B" ];
          ascribe "r" [ hole "A" ] ]
        [ hole "B" ] ]
  in
  let defs =
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
      def ~rule:"app" "Application"
        [ [ nt ~bind:"l" "Expr"; nt ~bind:"r" "AtomE" ] ];
      def "Expr" [ [ nt "AtomE" ]; [ nt "Lambda" ]; [ nt "Application" ] ] ]
  in
  match Grammar.make ~start:"Expr" ~ty:"Type" ~rules defs with
  | Ok g -> g
  | Error e -> failwith e

let () =
  (* application: ?A -> ?B  against  Int -> Bool *)
  (match Unify.unify (a @-> b) (int @-> bool) with
   | Some s -> Printf.printf "app    : %s\n" (Unify.show s)
   | None   -> print_endline "app    : clash");

  (* a whole subtree is captured: ?B = Int -> Bool *)
  (match Unify.unify (a @-> b) (int @-> (int @-> bool)) with
   | Some s -> Printf.printf "curry  : %s\n" (Unify.show s)
   | None   -> ());

  (* a theory: Bool ≡ Unit + Unit *)
  let theory = [ bool => (unit + unit) ] in
  Printf.printf "normal : %s\n" (Term.show (normalize theory bool));
  (match Unify.unify ~modulo:theory bool (unit + unit) with
   | Some _ -> print_endline "modulo : Bool ≡ Unit + Unit"
   | None   -> print_endline "modulo : clash");

  (* checks: a typed whole, a live prefix, a dead one *)
  Printf.printf "stlc   : %s\n" (Check.show (Check.run stlc "λx:A.x"));
  Printf.printf "prefix : %s\n" (Check.show (Check.run stlc "λx:A."));
  Printf.printf "unbound: %s\n" (Check.show (Check.run stlc "x"));

  (* the structural grammar rendered back to .auf *)
  print_newline ();
  print_string (Grammar.show stlc)
