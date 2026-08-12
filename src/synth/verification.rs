//! All-root verification — the Engine API v1 `verify()`.
//!
//! `root_type()` returns *a* complete-root type. When a prefix has more than
//! one complete derivation, which one it returns depends on production order,
//! so a grammar that types the same input two different ways looks unambiguous
//! from the outside. That hides exactly the case a caller most needs to see.
//!
//! [`verify`] reports every complete root instead. Roots whose normalized types
//! are equal collapse to one entry, so more than one entry in
//! [`Verification::root_types`] means the input genuinely has conflicting
//! complete-root types.
//!
//! Verification is state-free: it reads the current parse and changes nothing.

use crate::synth::Synthesizer;
use crate::typing::{Normalizer, Subst, Term, unify_modulo};

/// The outcome of verifying the current input.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Verification {
    /// `"typed"` (a complete root), `"live"` (parses, none complete yet), or
    /// `"dead"` (does not parse).
    pub status: &'static str,
    /// Every distinct complete-root type, normalized and rendered, sorted so
    /// the result does not depend on traversal order. More than one entry means
    /// the roots disagree.
    pub root_types: Vec<String>,
    /// Whether the goal is met, or `None` when no goal was given.
    pub goal_satisfied: Option<bool>,
}

impl Verification {
    /// Whether the complete roots disagree on the type.
    #[must_use]
    pub fn is_ambiguous(&self) -> bool {
        self.root_types.len() > 1
    }
}

/// Verify the current input, optionally against a goal type.
///
/// `expected` is parsed with the active grammar, so a goal is any term that
/// grammar derives — nothing here enumerates type shapes.
///
/// The goal holds only when there is at least one complete root, *every*
/// complete root unifies with the goal modulo the grammar's rewrites, and the
/// roots agree on a single type. Requiring all of them is the point: a prefix
/// with one satisfying root and one conflicting root has not been verified.
pub fn verify(synth: &mut Synthesizer, expected: Option<&str>) -> Result<Verification, String> {
    let norm = crate::typing::loader::normalizer(synth.grammar());

    let Ok(ast) = synth.ast() else {
        return Ok(Verification {
            status: "dead",
            root_types: Vec::new(),
            goal_satisfied: expected.map(|_| false),
        });
    };

    let rt = synth.runtime().clone();
    let mut terms: Vec<Term> = ast
        .roots()
        .filter(crate::ast::FusionNode::is_complete)
        .filter_map(|r| rt.evidence_of(r.evidence()))
        .map(|t| norm.normalize(&t))
        .collect();

    // Distinct *up to the rewrite theory*: normalize first, then dedup, so two
    // roots that differ only by a rewrite are one entry rather than a spurious
    // ambiguity report.
    let mut root_types: Vec<String> = Vec::new();
    for t in &terms {
        let rendered = crate::typing::syntax::render(synth.grammar(), t);
        if !root_types.contains(&rendered) {
            root_types.push(rendered);
        }
    }
    root_types.sort();

    let status = if terms.is_empty() { "live" } else { "typed" };

    let goal_satisfied = match expected {
        None => None,
        Some(goal) => {
            let goal = Term::parse(synth.grammar(), goal)
                .map_err(|e| format!("goal type '{goal}': {e}"))?;
            Some(satisfies(&norm, &mut terms, &goal, root_types.len()))
        }
    };

    Ok(Verification {
        status,
        root_types,
        goal_satisfied,
    })
}

/// Every complete root unifies with `goal`, there is at least one, and they
/// agree on a single type.
fn satisfies(norm: &Normalizer, roots: &mut [Term], goal: &Term, distinct: usize) -> bool {
    if roots.is_empty() || distinct != 1 {
        return false;
    }
    roots.iter().all(|t| {
        let mut subst = Subst::new();
        unify_modulo(norm, t, goal, &mut subst, true)
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::grammar::SPG;
    use crate::typing::TypingSynth;

    fn stlc() -> SPG {
        SPG::load(include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/stlc.auf"
        )))
        .unwrap()
    }

    fn verify_input(g: &SPG, input: &str, goal: Option<&str>) -> Verification {
        let mut s = TypingSynth::new(g.clone(), input);
        verify(&mut s, goal).unwrap()
    }

    #[test]
    fn complete_input_is_typed_with_one_root_type() {
        let v = verify_input(&stlc(), "λx:A.x", None);
        assert_eq!(v.status, "typed");
        assert_eq!(v.root_types.len(), 1, "{:?}", v.root_types);
        assert!(!v.is_ambiguous());
        assert_eq!(v.goal_satisfied, None);
    }

    #[test]
    fn incomplete_input_is_live_with_no_root_types() {
        let v = verify_input(&stlc(), "λx:A.", None);
        assert_eq!(v.status, "live");
        assert!(v.root_types.is_empty());
    }

    #[test]
    fn unparseable_input_is_dead() {
        let v = verify_input(&stlc(), "!!!", None);
        assert_eq!(v.status, "dead");
        assert!(v.root_types.is_empty());
        assert_eq!(v.goal_satisfied, None);
    }

    #[test]
    fn goal_is_checked_against_the_root_type() {
        let g = stlc();
        let hit = verify_input(&g, "λx:A.x", Some("A -> A"));
        assert_eq!(hit.goal_satisfied, Some(true), "{:?}", hit.root_types);

        let miss = verify_input(&g, "λx:A.x", Some("B -> B"));
        assert_eq!(miss.goal_satisfied, Some(false), "{:?}", miss.root_types);
    }

    /// A hole in the goal unifies with anything of the right shape, and nothing
    /// in `verify` special-cases what a type looks like.
    #[test]
    fn goal_may_contain_holes() {
        let v = verify_input(&stlc(), "λx:A.x", Some("?T -> ?T"));
        assert_eq!(v.goal_satisfied, Some(true), "{:?}", v.root_types);
    }

    /// A goal cannot be satisfied by an input that has no complete root: there
    /// is nothing to have verified.
    #[test]
    fn goal_fails_without_a_complete_root() {
        let g = stlc();
        assert_eq!(
            verify_input(&g, "λx:A.", Some("?T")).goal_satisfied,
            Some(false)
        );
        assert_eq!(
            verify_input(&g, "!!!", Some("?T")).goal_satisfied,
            Some(false)
        );
    }

    #[test]
    fn a_bad_goal_type_is_an_error_not_a_false() {
        let mut s = TypingSynth::new(stlc(), "λx:A.x");
        assert!(verify(&mut s, Some("!!!")).is_err());
    }

    /// Verification reads state and changes none of it.
    #[test]
    fn verify_is_state_free() {
        let g = stlc();
        let mut s = TypingSynth::new(g, "λx:A.x");
        let before = s.input().to_string();
        let first = verify(&mut s, Some("A -> A")).unwrap();
        let second = verify(&mut s, Some("A -> A")).unwrap();
        assert_eq!(first, second);
        assert_eq!(s.input(), before);
    }
}
