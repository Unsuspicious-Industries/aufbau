//! Normalization, exposed to OCaml.

use super::term::OTerm;
use super::theory;

/// The normal form of a term under a rewrite theory.
#[ocaml::func]
#[must_use]
pub fn aufbau_normalize(rules: Vec<(OTerm, OTerm)>, t: OTerm) -> OTerm {
    let norm = theory::normalizer(rules);
    OTerm::lift(&norm.normalize(&t.lower()))
}
