//! The type term as it crosses the OCaml boundary.
//!
//! Mirrors the OCaml `type term = Var of string | Con of string * term array |
//! Leaf of string`. The boundary speaks structure, never the surface string: a
//! caller builds terms inductively and the engine receives the real [`Term`].

use crate::typing::{Pattern, Term};

/// The OCaml-facing type term. Variant order matches the OCaml declaration.
#[derive(ocaml::ToValue, ocaml::FromValue)]
pub enum OTerm {
    Var(String),
    Con(String, Vec<OTerm>),
    Leaf(String),
}

impl OTerm {
    /// Into the engine's term. A leaf is taken literally as a regular singleton.
    #[must_use]
    pub fn lower(self) -> Term {
        match self {
            OTerm::Var(n) => Term::Var(n),
            OTerm::Con(f, ks) => Term::Con(f, ks.into_iter().map(OTerm::lower).collect()),
            OTerm::Leaf(s) => Term::Leaf(Pattern::raw(&s)),
        }
    }

    /// Out of the engine's term; a leaf is rendered as its regular-set surface.
    #[must_use]
    pub fn lift(t: &Term) -> Self {
        match t {
            Term::Var(n) => OTerm::Var(n.clone()),
            Term::Con(f, ks) => OTerm::Con(f.clone(), ks.iter().map(OTerm::lift).collect()),
            Term::Leaf(p) => OTerm::Leaf(p.to_string()),
        }
    }
}
