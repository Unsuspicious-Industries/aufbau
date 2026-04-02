//! Grammar-DSL-driven feature tests for Fusion.
//!
//! These tests aim to cover Fusion capabilities without depending on any
//! particular example language (Fun/STLC/IMP). Each test provides an inline
//! grammar+typing-rule spec and asserts behavior via the shared harness.

use super::harness::{FusionTestCase, run_cases};

#[test]
fn accepts_prefix_and_completes_when_extended() {
    // start ::= 'x' 'y' and we should accept prefix "x" as partial, then "x y" as complete.
    let spec = r#"
start ::= 'x' 'y'

----------- (start)
⊤
"#;
    run_cases(&[
        FusionTestCase::pass_spec("prefix x is partial", spec, "x").require_partial_only(),
        FusionTestCase::pass_spec("x y is complete", spec, "x y").require_complete(),
    ]);
}

#[test]
fn context_extension_applies_before_body() {
    // A minimal "let" that extends Γ with name's type *before* typing the body.
    let spec = r#"
Identifier ::= /[a-z]+/
IntLit(int) ::= /[0-9]+/
Var(var) ::= Identifier[x]
Let(let) ::= 'let' Identifier[name] '=' IntLit[value] ';' Var[body]
start ::= Let

----------- (int)
'Int'

x ∈ Γ
----------- (var)
Γ(x)

Γ ⊢ value : 'Int', Γ[name:'Int'] ⊢ body : ?R
------------------------------------------ (let)
?R
"#;
    run_cases(&[FusionTestCase::pass_spec(
        "let extends ctx for body lookup",
        spec,
        "let foo = 1 ; foo",
    )
    .require_complete()]);
}

#[test]
fn rejects_by_membership_premise() {
    // A DSL where variables require membership in Γ; with empty Γ, "x" must be rejected.
    let spec = r#"
Identifier ::= /[a-z]+/
Var(var) ::= Identifier[x]
One(one) ::= '1'
Expr ::= Var | One
start ::= Expr

----------- (one)
'Int'

// variable lookup
x ∈ Γ
----------- (var)
Γ(x)
"#;
    run_cases(&[
        FusionTestCase::pass_spec("literal ok", spec, "1").require_complete(),
        FusionTestCase::xfail_spec("unbound var rejected", spec, "x"),
    ]);
}
