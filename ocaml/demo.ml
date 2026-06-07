open Aufbau
open Term
open Rewrite

(* domain syntax, defined where it belongs — in the type system, not the engine *)
let ( @-> ) a b = con "Fun" [ a; b ]
let ( + ) a b = con "Sum" [ a; b ]

let a, b = (var "A", var "B")
let int, bool, unit = (leaf "Int", leaf "Bool", leaf "Unit")

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

  (* STLC, defined structurally in OCaml — no .auf source. *)
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
        def ~rule:"app" "Application"
          [ [ nt ~bind:"l" "Expr"; nt ~bind:"r" "AtomE" ] ];
        def "Expr" [ [ nt "AtomE" ]; [ nt "Lambda" ]; [ nt "Application" ] ] ]
  in
  Printf.printf "stlc   : %s\n" (Grammar.check stlc "λx:A.x")
