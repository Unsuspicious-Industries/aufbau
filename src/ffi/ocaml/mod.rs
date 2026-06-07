//! OCaml FFI — the type algebra (terms, unification, rewriting) as native OCaml
//! values, built inductively rather than through the surface string. The idiomatic
//! OCaml surface lives in `ocaml/`.

pub mod grammar;
pub mod rewrite;
pub mod term;
pub mod theory;
pub mod unify;
