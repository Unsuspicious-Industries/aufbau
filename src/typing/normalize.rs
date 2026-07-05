//! Type normalization — §2. A type's meaning is its normal form.
//!
//! Unification on trees is the *free* theory: two types are equal iff their trees
//! are. Anything coarser (type-synonym unfolding, currying, projection laws) is an
//! equational theory, and the general way to add one without a bespoke unifier per
//! type is term rewriting: a set of oriented rules `lhs ⇝ rhs` over the type
//! signature, applied to a fixpoint. The whole type relation is then
//!
//! ```text
//! unify_modulo(a, b)  =  unify(normalize(a), normalize(b))
//! ```
//!
//! and [`unify_modulo`] is the single seam every ascription routes through, so
//! enriching the theory never touches the domain or the rule IR. Today the theory
//! is whatever rules a grammar declares; with none, `normalize` is the identity and
//! `unify_modulo` is bare [`unify`](term::unify).
//!
//! ## Tiers (room for upgrade)
//! - **now**: oriented rewrites, applied left-to-right, checking direction.
//! - **AC**: commutativity/associativity cannot be oriented as terminating rules;
//!   they extend [`Normalizer::reduce_once`] with per-constructor canonicalization
//!   (flatten/sort/drop-unit), not new call sites.
//! - **narrowing**: unification *modulo* a theory with variables on both sides needs
//!   narrowing (= search); that is the kanren layer, behind this same seam.

use crate::typing::term::{self, Subst, Term};
use std::fmt;

/// An oriented rewrite rule `lhs ⇝ rhs`. Both sides are type terms whose
/// [`Term::Var`]s are the rule's metavariables; a match binds them to subtrees.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RewriteRule {
    pub lhs: Term,
    pub rhs: Term,
}

impl fmt::Display for RewriteRule {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ⇝ {}", self.lhs, self.rhs)
    }
}

/// A rewrite theory: the rules that define type equivalence beyond tree identity.
/// Empty (the default) means the free theory, where `normalize` is the identity.
#[derive(Clone, Debug, Default)]
pub struct Normalizer {
    rules: Vec<RewriteRule>,
}

/// Cap on rewrite steps at one node. A confluent terminating theory never reaches
/// it; an ill-formed (looping) one is stopped rather than hung. Termination is the
/// theory author's obligation
const MAX_STEPS: usize = 1024;

impl Normalizer {
    /// The free theory: no rewrites, `normalize` is the identity.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// A theory from oriented rules.
    #[must_use]
    pub fn from_rules(rules: Vec<RewriteRule>) -> Self {
        Self { rules }
    }

    /// The declared rewrite rules, in order.
    #[must_use]
    pub fn rules(&self) -> &[RewriteRule] {
        &self.rules
    }

    /// Reduce `t` to its normal form: normalize children, then rewrite at the root
    /// to a fixpoint. Innermost (bottom-up), so a root rewrite sees already-normal
    /// children.
    #[must_use]
    pub fn normalize(&self, t: &Term) -> Term {
        // The free theory: nothing to rewrite, so normal form is the term itself.
        if self.rules.is_empty() {
            return t.clone();
        }
        let normalized = match t {
            Term::Con(label, kids) => {
                Term::Con(label.clone(), kids.iter().map(|k| self.normalize(k)).collect())
            }
            other => other.clone(),
        };
        let mut t = normalized;
        for _ in 0..MAX_STEPS {
            match self.reduce_once(&t) {
                // The rewritten term may expose new redexes; re-normalize it.
                Some(next) => t = self.normalize(&next),
                None => return t,
            }
        }
        t
    }

    /// One root rewrite: the result of the first rule whose `lhs` matches `t`, with
    /// the match substitution applied to its `rhs`. `None` if `t` is a redex of no
    /// rule. AC canonicalization will extend here.
    fn reduce_once(&self, t: &Term) -> Option<Term> {
        for rule in &self.rules {
            if let Some(subst) = matches(&rule.lhs, t) {
                return Some(term::apply(&rule.rhs, &subst));
            }
        }
        None
    }
}

/// One-sided match: find a substitution `σ` over `pat`'s variables with
/// `apply(σ, pat) = t`, treating `t` as rigid (its variables are constants). This
/// is unification with one side frozen — what rewriting needs, where only the rule
/// pattern is flexible.
#[must_use]
pub fn matches(pat: &Term, t: &Term) -> Option<Subst> {
    let mut subst = Subst::new();
    match_into(pat, t, &mut subst).then_some(subst)
}

fn match_into(pat: &Term, t: &Term, subst: &mut Subst) -> bool {
    match (pat, t) {
        (Term::Var(x), _) => match subst.get(x) {
            // A repeated metavariable must bind consistently.
            Some(bound) => bound == t,
            None => {
                subst.insert(x.clone(), t.clone());
                true
            }
        },
        (Term::Con(f, ps), Term::Con(g, ts)) => {
            f == g && ps.len() == ts.len() && ps.iter().zip(ts).all(|(p, t)| match_into(p, t, subst))
        }
        // A concrete pattern leaf matches only the same leaf; set-valued rewrite
        // LHS leaves are a later refinement.
        (Term::Leaf(p), Term::Leaf(q)) => p == q,
        _ => false,
    }
}

/// Is a unification failure between `a` and `b` stable under instantiation
/// i.e. may the caller prune (`Lost`) on it? In the free theory, yes: the only
/// failures are rigid-rigid clashes and occurs-check violations, and both
/// survive any substitution. With rewrite rules, only if both sides are ground:
/// a hole can block a redex, so instantiating it may enable a rewrite that
/// repairs the clash (`nf ∘ σ ≠ σ ∘ nf` on open terms). A non-stable failure
/// must read as `Unknown` (a postponed constraint), never `Lost`.
#[must_use]
pub fn failure_is_stable(norm: &Normalizer, a: &Term, b: &Term) -> bool {
    norm.rules().is_empty() || (a.is_ground() && b.is_ground())
}

/// Unification modulo the theory: normalize both sides, then unify on the free
/// theory. Complete for the checking direction (one ground side); see the module
/// note on narrowing for variables on both sides, and [`failure_is_stable`] for
/// when a failure licenses pruning.
#[must_use]
pub fn unify_modulo(
    norm: &Normalizer,
    a: &Term,
    b: &Term,
    subst: &mut Subst,
    occurs_check: bool,
) -> bool {
    // Free theory: unify the terms directly, no normal-form clones.
    if norm.rules().is_empty() {
        return term::unify(a, b, subst, occurs_check);
    }
    let a = norm.normalize(a);
    let b = norm.normalize(b);
    term::unify(&a, &b, subst, occurs_check)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn con(label: &str, kids: Vec<Term>) -> Term {
        Term::con(label, kids)
    }

    #[test]
    fn empty_theory_is_identity() {
        let norm = Normalizer::new();
        let t = con("Fun", vec![Term::leaf("A"), Term::var("B")]);
        assert_eq!(norm.normalize(&t), t);
    }

    #[test]
    fn matches_binds_subtree() {
        // `?A -> ?B` matches `a -> (b -> c)` with ?B the whole right subtree.
        let pat = con("Fun", vec![Term::var("A"), Term::var("B")]);
        let inner = con("Fun", vec![Term::leaf("b"), Term::leaf("c")]);
        let t = con("Fun", vec![Term::leaf("a"), inner.clone()]);
        let subst = matches(&pat, &t).expect("should match");
        assert_eq!(subst.get("A"), Some(&Term::leaf("a")));
        assert_eq!(subst.get("B"), Some(&inner));
    }

    #[test]
    fn matches_rejects_label_clash() {
        let pat = con("Fun", vec![Term::var("A")]);
        let t = con("Prod", vec![Term::leaf("a")]);
        assert!(matches(&pat, &t).is_none());
    }

    #[test]
    fn repeated_metavar_must_agree() {
        // `Pair(?A, ?A)` matches `Pair(x, x)` but not `Pair(x, y)`.
        let pat = con("Pair", vec![Term::var("A"), Term::var("A")]);
        let same = con("Pair", vec![Term::leaf("x"), Term::leaf("x")]);
        let diff = con("Pair", vec![Term::leaf("x"), Term::leaf("y")]);
        assert!(matches(&pat, &same).is_some());
        assert!(matches(&pat, &diff).is_none());
    }

    #[test]
    fn rewrite_unfolds_synonym() {
        // Bool ⇝ Sum(Unit, Unit). A synonym is unfolded to its normal form.
        let rule = RewriteRule {
            lhs: Term::leaf("Bool"),
            rhs: con("Sum", vec![Term::leaf("Unit"), Term::leaf("Unit")]),
        };
        let norm = Normalizer::from_rules(vec![rule]);
        assert_eq!(
            norm.normalize(&Term::leaf("Bool")),
            con("Sum", vec![Term::leaf("Unit"), Term::leaf("Unit")])
        );
        // And under a constructor: Fun(Bool, A) normalizes its child.
        assert_eq!(
            norm.normalize(&con("Fun", vec![Term::leaf("Bool"), Term::leaf("A")])),
            con(
                "Fun",
                vec![
                    con("Sum", vec![Term::leaf("Unit"), Term::leaf("Unit")]),
                    Term::leaf("A")
                ]
            )
        );
    }

    #[test]
    fn rewrite_projection_with_metavars() {
        // fst(Pair(?A, ?B)) ⇝ ?A
        let rule = RewriteRule {
            lhs: con("fst", vec![con("Pair", vec![Term::var("A"), Term::var("B")])]),
            rhs: Term::var("A"),
        };
        let norm = Normalizer::from_rules(vec![rule]);
        let t = con("fst", vec![con("Pair", vec![Term::leaf("X"), Term::leaf("Y")])]);
        assert_eq!(norm.normalize(&t), Term::leaf("X"));
    }

    #[test]
    fn unify_modulo_sees_through_synonym() {
        // With Bool ⇝ Sum(Unit,Unit), the leaf Bool unifies with the expanded form.
        let rule = RewriteRule {
            lhs: Term::leaf("Bool"),
            rhs: con("Sum", vec![Term::leaf("Unit"), Term::leaf("Unit")]),
        };
        let norm = Normalizer::from_rules(vec![rule]);
        let expanded = con("Sum", vec![Term::leaf("Unit"), Term::leaf("Unit")]);
        let mut s = Subst::new();
        assert!(unify_modulo(&norm, &Term::leaf("Bool"), &expanded, &mut s, true));
        // Free unification alone would not.
        let mut s2 = Subst::new();
        assert!(!term::unify(&Term::leaf("Bool"), &expanded, &mut s2, true));
    }

    #[test]
    fn empty_theory_modulo_equals_unify() {
        let norm = Normalizer::new();
        let a = con("Fun", vec![Term::var("A"), Term::leaf("X")]);
        let b = con("Fun", vec![Term::leaf("Y"), Term::var("B")]);
        let (mut s1, mut s2) = (Subst::new(), Subst::new());
        assert_eq!(
            unify_modulo(&norm, &a, &b, &mut s1, true),
            term::unify(&a, &b, &mut s2, true)
        );
    }
}
