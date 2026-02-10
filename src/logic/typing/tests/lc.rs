use crate::logic::grammar::Grammar;
use crate::logic::typing::eval::check_tree;
use crate::logic::Parser;
use crate::set_debug_level;
use crate::validation::completable::load_example_grammar;

fn lc() -> Grammar {
    load_example_grammar("stlc")
}

#[test]
fn test_identity() {
    // P => P
    let g = lc();
    let mut p = Parser::new(g.clone());
    set_debug_level(crate::DebugLevel::Trace);
    let ast = p
        .partial(
            r#"
            λf : A -> B -> C. (λg : A -> B. (λx : A. f g x))

    "#,
        )
        .unwrap();
    assert!(ast.is_complete(), "Identity should be provable");
    let complete = ast.completes();
    for c in complete {
        println!("{}", &c);
        let typed = c.typed(&g);
        if let Some(typed) = typed {
            println!("{}", typed);
        } else {
            // check tree
            set_debug_level(crate::DebugLevel::Trace);
            let status = check_tree(&c, &g);
            print!("{:#?}", status);
        }
    }
}
/*
Term [alt 2] ✓
  Lambda [alt 0] ✓
    "λ"<>
    Variable [alt 0] ✓
      Identifier [alt 0] ✓
        "f"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
    ":"<>
    Type [alt 0] ✓
      BaseType [alt 0] ✓
        TypeName [alt 0] ✓
          Identifier [alt 0] ✓
            "A"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
      "->"<>
      Type [alt 0] ✓
        BaseType [alt 0] ✓
          TypeName [alt 0] ✓
            Identifier [alt 0] ✓
              "B"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
        "->"<>
        Type [alt 1] ✓
          BaseType [alt 0] ✓
            TypeName [alt 0] ✓
              Identifier [alt 0] ✓
                "C"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
    "."<>
    Term [alt 2] ✓
      Lambda [alt 0] ✓
        "λ"<>
        Variable [alt 0] ✓
          Identifier [alt 0] ✓
            "g"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
        ":"<>
        Type [alt 0] ✓
          BaseType [alt 0] ✓
            TypeName [alt 0] ✓
              Identifier [alt 0] ✓
                "A"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
          "->"<>
          Type [alt 1] ✓
            BaseType [alt 0] ✓
              TypeName [alt 0] ✓
                Identifier [alt 0] ✓
                  "B"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
        "."<>
        Term [alt 2] ✓
          Lambda [alt 0] ✓
            "λ"<>
            Variable [alt 0] ✓
              Identifier [alt 0] ✓
                "x"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
            ":"<>
            Type [alt 1] ✓
              BaseType [alt 0] ✓
                TypeName [alt 0] ✓
                  Identifier [alt 0] ✓
                    "A"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
            "."<>
            Term [alt 0] ✓
              Application [alt 0] ✓
                BaseTerm [alt 0] ✓
                  Variable [alt 0] ✓
                    Identifier [alt 0] ✓
                      "f"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
                BaseTerm [alt 1] ✓
                  "("<>
                  Term [alt 0] ✓
                    Application [alt 0] ✓
                      BaseTerm [alt 0] ✓
                        Variable [alt 0] ✓
                          Identifier [alt 0] ✓
                            "g"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
                      BaseTerm [alt 0] ✓
                        Variable [alt 0] ✓
                          Identifier [alt 0] ✓
                            "x"<(((((([0-9]|[A-Z])|_)|[a-z])|τ)|[₀-₉]))*>
                  ")"<>
 */
