use crate::testing::{
    assert_parse_structurally_matches, assert_partial_structurally_matches, grammars,
    load_inline_grammar,
};

#[test]
fn stlc_var() {
    assert_parse_structurally_matches(
        grammars::stlc(),
        "x",
        r#"(Expression @0 #0
  (AtomicExpression @0 #0
    (Variable @0 #0
      (Identifier @0 $x #0
        (T "x" + "((([0-9]|[A-Z])|[a-z]))*")))))"#,
    );
}

#[test]
fn stlc_app() {
    assert_parse_structurally_matches(
        grammars::stlc(),
        "f x",
        r#"(Expression @1 #0
  (Application @0 #0
    (Expression @0 $l #0
      (AtomicExpression @0 #0
        (Variable @0 #0
          (Identifier @0 $x #0
            (T "f" + "((([0-9]|[A-Z])|[a-z]))*")))))
    (AtomicExpression @0 $r #0
      (Variable @0 #0
        (Identifier @0 $x #0
          (T "x" + "((([0-9]|[A-Z])|[a-z]))*"))))))"#,
    );
}

#[test]
fn fun_int() {
    assert_parse_structurally_matches(
        grammars::fun(),
        "42",
        r#"(Expression @4 #0
  (AtomicExpression @1 #0
    (Integer @0 #0
      (T "42" + "[0-9]*"))))"#,
    );
}

#[test]
fn fun_lambda() {
    assert_parse_structurally_matches(
        grammars::fun(),
        "(x: Int) => x",
        r#"(Expression @4 #0
  (AtomicExpression @4 #0
    (Lambda @0 #0
      (T "(" + "")
      (Identifier @0 $param #0
        (T "x" + "(([0-9]|[a-z]))*"))
      (T ":" + "")
      (Type @0 $τ #0
        (BaseType @0 #0
          (TypeName @0 #0
            (T "Int" + "(([0-9]|[a-z]))*"))))
      (T ")" + "")
      (T "=>" + "")
      (Expression @4 $body #0
        (AtomicExpression @0 #0
          (Variable @0 #0
            (Identifier @0 $x #0
              (T "x" + "(([0-9]|[a-z]))*"))))))))"#,
    );
}

#[test]
fn imp_block() {
    assert_parse_structurally_matches(
        grammars::imp(),
        "{ let x: Int = 1; }",
        r#"(Program @0 #0
  (Block @0 $main #0
    (T "{" + "")
    (Statements @0 $stmts #0
      (Statement @0 $head #0
        (Declaration @0 #0
          (T "let" + "")
          (Identifier @0 $name #0
            (T "x" + "((([0-9]|_)|[a-z]))*"))
          (T ":" + "")
          (Type @0 $τ #0
            (BaseType @0 #0
              (TypeName @0 #0
                (T "Int" + ""))))
          (T "=" + "")
          (Expression @2 $value #0
            (AtomicExpr @1 #0
              (Integer @0 #0
                (T "1" + "[0-9]*"))))
          (T ";" + "")))
      (Statements @1 $tail #0))
    (T "}" + "")))"#,
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
        r#"(start @0 #0
  (Expr @0 #0
    (Expr @1 #0
      (Num @0 #0
        (T "1" + "[0-9]*")))
    (T "+" + "")
    (Num @0 #0
      (T "2" + "[0-9]*"))))"#,
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
        r#"(start @0 #0
  (Type @0 #0
    (BaseType @0 #0
      (Identifier @0 #0
        (T "A" + "(([A-Z]|[a-z]))*")))
    (T~ "-" ~ ">")))"#,
    );
}
