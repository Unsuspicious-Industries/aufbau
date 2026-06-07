//! Rewrite theories and substitutions across the boundary.

use super::term::OTerm;
use crate::typing::{Normalizer, RewriteRule, Subst, term};

/// Build a normalizer from a list of `(lhs, rhs)` rewrite pairs.
#[must_use]
pub fn normalizer(rules: Vec<(OTerm, OTerm)>) -> Normalizer {
    Normalizer::from_rules(
        rules
            .into_iter()
            .map(|(l, r)| RewriteRule {
                lhs: l.lower(),
                rhs: r.lower(),
            })
            .collect(),
    )
}

/// A solved substitution as a sorted `(name, term)` assoc array, each binding
/// fully applied.
#[must_use]
pub fn solution(subst: &Subst) -> Vec<(String, OTerm)> {
    let mut out: Vec<(String, OTerm)> = subst
        .iter()
        .map(|(k, v)| (k.clone(), OTerm::lift(&term::apply(v, subst))))
        .collect();
    out.sort_by(|a, b| a.0.cmp(&b.0));
    out
}
