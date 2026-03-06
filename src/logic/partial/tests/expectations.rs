use crate::testing::{
    assert_parse_structurally_matches, assert_partial_structurally_matches, grammars,
    load_inline_grammar,
};

#[test]
fn stlc_var() {
    assert_parse_structurally_matches(
        grammars::stlc(),
        "x",
        r#"(Expression @0 #1
  (AtomicExpression @0 #1
    (Variable @0 #1
      (Identifier @0 $x #1
        (T "x")))))"#,
    );
}

#[test]
fn stlc_app() {
    assert_parse_structurally_matches(
        grammars::stlc(),
        "f x",
        r#"(Expression @1 #2
  (Application @0 #2
    (Expression @0 $l #1
      (AtomicExpression @0 #1
        (Variable @0 #1
          (Identifier @0 $x #1
            (T "f")))))
    (AtomicExpression @0 $r #1
      (Variable @0 #1
        (Identifier @0 $x #1
          (T "x"))))))"#,
    );
}

#[test]
fn fun_int() {
    assert_parse_structurally_matches(
        grammars::fun(),
        "42",
        r#"(Expression @0 #1
  (IntBinary @0 #1
    (Expression @2 $left #1
      (Application @0 #1
        (AtomicExpression @1 $func #1
          (Integer @0 #1
            (T "42")))
        (T~ "")))
    (IntOp @0 $op #0
      (T~ ""))))"#,
    );
}

#[test]
fn fun_lambda() {
    assert_parse_structurally_matches(
        grammars::fun(),
        "(x: Int) => x",
        r#"(Expression @0 #7
  (IntBinary @0 #7
    (Expression @2 $left #7
      (Application @0 #7
        (AtomicExpression @4 $func #7
          (Lambda @0 #7
            (T "(")
            (Identifier @0 $param #1
              (T "x"))
            (T ":")
            (Type @0 $τ #1
              (BaseType @0 #1
                (TypeName @0 #1
                  (T "Int"))))
            (T ")")
            (T "=>")
            (Expression @1 $body #1
              (FloatBinary @0 #1
                (Expression @4 $left #1
                  (AtomicExpression @0 #1
                    (Variable @0 #1
                      (Identifier @0 $x #1
                        (T "x")))))
                (FloatOp @0 $op #0
                  (T~ ""))))))
        (T~ "")))
    (IntOp @2 $op #0
      (T~ ""))))"#,
    );
}

#[test]
fn imp_block() {
    assert_parse_structurally_matches(
        grammars::imp(),
        "{ let x: Int = 1; }",
        r#"(Program @0 #9
  (Block @0 $main #9
    (T "{")
    (Statements @0 $stmts #7
      (Statement @0 $head #7
        (Declaration @0 #7
          (T "let")
          (Identifier @0 $name #1
            (T "x"))
          (T ":")
          (Type @0 $τ #1
            (BaseType @0 #1
              (TypeName @0 #1
                (T "Int"))))
          (T "=")
          (Expression @2 $value #1
            (AtomicExpr @1 #1
              (Integer @0 #1
                (T "1"))))
          (T ";")))
      (Statements @1 $tail #0))
    (T "}")))"#,
    );
}

#[test]
fn custom_left_rec() {
    let grammar = load_inline_grammar(
        r#"
        Num ::= /[0-9]+/
        Expr ::= Expr '+' Num | Num
        start ::= Expr
        "#,
    );

    assert_parse_structurally_matches(
        &grammar,
        "1 + 2",
        r#"(start @0 #3
  (Expr @0 #3
    (Expr @1 #1
      (Num @0 #1
        (T "1")))
    (T "+")
    (Num @0 #1
      (T "2"))))"#,
    );
}

#[test]
fn custom_partial_arrow() {
    let grammar = load_inline_grammar(
        r#"
        Identifier ::= /[A-Za-z]+/
        BaseType ::= Identifier
        Type ::= BaseType '->' Type | BaseType
        start ::= Type
        "#,
    );

    assert_partial_structurally_matches(
        &grammar,
        "A-",
        r#"(start @0 #2
  (Type @0 #2
    (BaseType @0 #1
      (Identifier @0 #1
        (T "A")))
    (T~ "-")))"#,
    );
}
