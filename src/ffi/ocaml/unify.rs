//! Unification, exposed to OCaml.

use super::term::OTerm;
use super::theory;
use crate::typing::{Subst, term, unify_modulo};

/// Unify two terms in the free theory. `Some subst` (a solved assoc array) on
/// success, `None` on clash.
#[ocaml::func]
#[must_use]
pub fn aufbau_unify(a: OTerm, b: OTerm) -> Option<Vec<(String, OTerm)>> {
    let mut s = Subst::new();
    term::unify(&a.lower(), &b.lower(), &mut s, true).then(|| theory::solution(&s))
}

/// Unify two terms modulo a rewrite theory (normalize, then unify).
#[ocaml::func]
#[must_use]
pub fn aufbau_unify_modulo(
    rules: Vec<(OTerm, OTerm)>,
    a: OTerm,
    b: OTerm,
) -> Option<Vec<(String, OTerm)>> {
    let norm = theory::normalizer(rules);
    let mut s = Subst::new();
    unify_modulo(&norm, &a.lower(), &b.lower(), &mut s, true).then(|| theory::solution(&s))
}
