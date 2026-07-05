//! Completeness delineation — for which grammars is every live prefix realizable?
//!
//! Safe pruning is unconditional: `dead` means no continuation exists. The
//! converse — `live` means some continuation exists — is *semantic
//! inhabitedness* (`def:inhabited`), PSPACE-complete in general. This module
//! decides a sound, conservative certificate for it, so a grammar carries its
//! own classification:
//!
//! - [`Completeness::Syntactic`]: no typing rules. Liveness is Earley
//!   liveness, which is exact: live ⇔ realizable.
//! - [`Completeness::Inhabited`]: every sort an ascription can constrain has a
//!   *universal inhabitant* — a closed derivation whose type is a fresh hole,
//!   inhabiting every demanded type (OCaml's `assert false`). Then any pending
//!   ascription is dischargeable, so live ⇒ realizable.
//! - [`Completeness::Sound`]: neither certificate applies. Pruning is still
//!   sound, but a live prefix may demand a type the language cannot inhabit
//!   (the STLC prefix `λf:A→B. f(` with `A`, `B` distinct atoms).
//!
//! The certificate is sufficient, not necessary: a `Sound` grammar may in fact
//! be complete; an `Inhabited` or `Syntactic` one is guaranteed complete.
//!
//! ## The universal-inhabitant fixpoint
//!
//! `universal(N)` is the least set closed under:
//! - a rule-less alternative of `N` with exactly one nonterminal child `M`
//!   and no bound terminal (a transparent wrapper) where `universal(M)`;
//! - an alternative of `N` carrying rule `R` where the conclusion is a bare
//!   hole (`?A`) or `⊤` with no exported effects, every premise is an
//!   ascription whose subject is bound in this alternative to a universal
//!   nonterminal, and every other nonterminal child is productive.
//!
//! The base case is an axiom concluding `?A` over derivable syntax. Membership
//! and equation premises never certify (they reach outside the node); a
//! conclusion mentioning structure (`τ -> ?B`) pins the shape and never
//! inhabits an atom.

use crate::engine::grammar::{Production, SPG, Symbol};
use crate::typing::rule::Judgment;
use crate::typing::{Atom, TypingRule};
use std::collections::HashSet;

/// The realizability class of a grammar: for which inputs does `live`
/// guarantee a continuation?
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Completeness {
    /// No typing rules: live ⇔ realizable (Earley liveness is exact).
    Syntactic,
    /// Every ascribed sort has a universal inhabitant: live ⇒ realizable.
    Inhabited,
    /// Pruning is sound, but a live prefix may be uninhabited at the listed
    /// sorts (ascription positions with no universal inhabitant).
    Sound { uninhabited: Vec<String> },
}

impl Completeness {
    /// Does this class guarantee that every live prefix is realizable?
    #[must_use]
    pub fn is_complete(&self) -> bool {
        !matches!(self, Completeness::Sound { .. })
    }
}

impl std::fmt::Display for Completeness {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Completeness::Syntactic => write!(f, "syntactic"),
            Completeness::Inhabited => write!(f, "inhabited"),
            Completeness::Sound { uninhabited } => {
                write!(
                    f,
                    "sound (live may be uninhabited at: {})",
                    uninhabited.join(", ")
                )
            }
        }
    }
}

/// Classify a grammar. See the module docs for the certificate.
#[must_use]
pub fn completeness(g: &SPG) -> Completeness {
    if g.rules.is_empty() {
        return Completeness::Syntactic;
    }
    let universal = universal_sorts(g);
    let mut uninhabited: Vec<String> = ascribed_sorts(g)
        .into_iter()
        .filter(|s| !universal.contains(s))
        .collect();
    uninhabited.sort();
    if uninhabited.is_empty() {
        Completeness::Inhabited
    } else {
        Completeness::Sound { uninhabited }
    }
}

/// Nonterminals that derive at least one complete string (standard CFG
/// productivity, ignoring types). Least fixpoint.
#[must_use]
pub fn productive_sorts(g: &SPG) -> HashSet<String> {
    fixpoint(g, |prod, done| {
        prod.rhs.iter().all(|s| match s {
            Symbol::Terminal { .. } => true,
            Symbol::Nonterminal { name, .. } => done.contains(name),
        })
    })
}

/// Nonterminals with a universal inhabitant: a closed derivation whose type
/// unifies with every demanded type. Least fixpoint over the rules in the
/// module docs.
#[must_use]
pub fn universal_sorts(g: &SPG) -> HashSet<String> {
    let productive = productive_sorts(g);
    let mut universal = HashSet::new();
    loop {
        let mut grew = false;
        for (nt, prods) in &g.productions {
            if universal.contains(nt) {
                continue;
            }
            let rule = g.nt_rule(nt).and_then(|r| g.rules.get(r.as_str()));
            let ok = prods.iter().enumerate().any(|(alt, p)| match rule {
                None => {
                    let idx = g.nt_index(nt).unwrap_or(usize::MAX);
                    g.is_transparent((idx, alt))
                        && p.rhs.iter().any(
                            |s| matches!(s, Symbol::Nonterminal { name, .. } if universal.contains(name)),
                        )
                }
                Some(r) => universal_via(r, p, &universal, &productive),
            });
            if ok {
                universal.insert(nt.clone());
                grew = true;
            }
        }
        if !grew {
            return universal;
        }
    }
}

/// Can this alternative, under rule `r`, produce a term of any demanded type?
fn universal_via(
    r: &TypingRule,
    p: &Production,
    universal: &HashSet<String>,
    productive: &HashSet<String>,
) -> bool {
    // The conclusion must be a bare hole or ⊤ — anything structural pins the
    // shape — and must export nothing.
    let bare = matches!(
        r.conclusion.ty.0.as_slice(),
        [Atom::Hole(_)] | [Atom::Top] | []
    );
    if !bare || !r.conclusion.effects.is_empty() {
        return false;
    }
    // Every premise must be an ascription whose subject this alternative
    // binds to a universal nonterminal.
    let subject_universal = |b: &str| {
        p.rhs.iter().any(|s| match s {
            Symbol::Nonterminal {
                name,
                binding: Some(bind),
            } => bind == b && universal.contains(name),
            _ => false,
        })
    };
    let premises_ok = r.premises.iter().all(|pr| match &pr.judgment {
        Judgment::Ascription { binding, .. } => subject_universal(binding),
        Judgment::Membership { .. } | Judgment::Equation { .. } => false,
    });
    if !premises_ok {
        return false;
    }
    // Every remaining child must at least derive something.
    p.rhs.iter().all(|s| match s {
        Symbol::Terminal { .. } => true,
        Symbol::Nonterminal { name, .. } => universal.contains(name) || productive.contains(name),
    })
}

/// Sorts that can appear as the subject of an ascription premise — the
/// positions where a demanded type may have no inhabitant. A subject bound to
/// a terminal is reported as `Nt[binding]` (its text leaf is never universal).
fn ascribed_sorts(g: &SPG) -> HashSet<String> {
    let mut out = HashSet::new();
    for (nt, rule_name) in &g.nonterminal_rules {
        let Some(rule) = g.rules.get(rule_name.as_str()) else {
            continue;
        };
        let subjects: HashSet<&str> = rule
            .premises
            .iter()
            .filter_map(|p| match &p.judgment {
                Judgment::Ascription { binding, .. } => Some(binding.as_str()),
                _ => None,
            })
            .collect();
        if subjects.is_empty() {
            continue;
        }
        for p in g.productions.get(nt).into_iter().flatten() {
            for s in &p.rhs {
                let Some(b) = s.binding() else { continue };
                if !subjects.contains(b.as_str()) {
                    continue;
                }
                match s {
                    Symbol::Nonterminal { name, .. } => {
                        out.insert(name.clone());
                    }
                    Symbol::Terminal { .. } => {
                        out.insert(format!("{nt}[{b}]"));
                    }
                }
            }
        }
    }
    out
}

/// Generic least fixpoint over nonterminals: `N` enters the set when some
/// alternative of `N` satisfies `admit` given the current set.
fn fixpoint(g: &SPG, admit: impl Fn(&Production, &HashSet<String>) -> bool) -> HashSet<String> {
    let mut done: HashSet<String> = HashSet::new();
    loop {
        let mut grew = false;
        for (nt, prods) in &g.productions {
            if !done.contains(nt) && prods.iter().any(|p| admit(p, &done)) {
                done.insert(nt.clone());
                grew = true;
            }
        }
        if !grew {
            return done;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn load(name: &str) -> SPG {
        let src = std::fs::read_to_string(format!(
            "{}/examples/{name}.auf",
            env!("CARGO_MANIFEST_DIR")
        ))
        .unwrap();
        SPG::load(&src).unwrap()
    }

    #[test]
    fn no_rules_is_syntactic() {
        let g = SPG::load("A ::= 'a' | 'a' A").unwrap();
        assert_eq!(completeness(&g), Completeness::Syntactic);
    }

    #[test]
    fn stlc_is_sound_only() {
        // The draft's counterexample: `λf:A→B. f(` is live but uninhabited.
        let g = load("stlc");
        let c = completeness(&g);
        assert!(!c.is_complete(), "STLC must not certify completeness: {c}");
    }

    #[test]
    fn ml_is_inhabited() {
        // `assert false : ?A` is the universal inhabitant, so every demanded
        // type at an ascribed position is realizable.
        let g = load("ml");
        assert_eq!(completeness(&g), Completeness::Inhabited);
    }

    #[test]
    fn ml_without_diverge_is_sound_only() {
        // Pinning the inhabitant's type drops the certificate: `?A` is what
        // makes `assert false` universal, not the production's existence.
        let src =
            std::fs::read_to_string(format!("{}/examples/ml.auf", env!("CARGO_MANIFEST_DIR")))
                .unwrap();
        let pinned = src.replace(
            "------------------ (diverge)\n?A",
            "------------------ (diverge)\n'int'",
        );
        assert_ne!(src, pinned, "expected the diverge axiom in ml.auf");
        let g = SPG::load(&pinned).unwrap();
        assert!(!completeness(&g).is_complete());
    }

    #[test]
    fn universal_propagates_through_wrappers() {
        // An axiom at a hole certifies its sort; transparent alternation and
        // a rule whose premises are all universal ascriptions propagate it.
        let g = SPG::load(
            "TypeAtom ::= /[A-Z][a-z]*/\n\
             Type* ::= TypeAtom | TypeAtom '->' Type\n\
             Diverge(diverge) ::= 'loop'\n\
             Atom ::= Diverge | '(' Expression ')'\n\
             Application(app) ::= Atom[l] Atom[r]\n\
             Expression ::= Atom | Application\n\
             \n\
             ----------- (diverge)\n\
             ?A\n\
             \n\
             Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A\n\
             ----------- (app)\n\
             ?B\n",
        )
        .unwrap();
        let u = universal_sorts(&g);
        for nt in ["Diverge", "Atom", "Application", "Expression"] {
            assert!(u.contains(nt), "{nt} should be universal: {u:?}");
        }
        assert_eq!(completeness(&g), Completeness::Inhabited);
    }
}
