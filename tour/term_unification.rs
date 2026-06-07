//! A tour of the type-term unifier (§2). Types are trees over the grammar
//! signature; unification is first-order Robinson; a rewrite theory gives equality
//! beyond syntax. Run: `cargo run --example term_unification`.

use aufbau::typing::{Normalizer, RewriteRule, Subst, Term, term, unify_modulo};

fn arrow(a: Term, b: Term) -> Term {
    Term::con("Fun", vec![a, b])
}
fn sum(a: Term, b: Term) -> Term {
    Term::con("Sum", vec![a, b])
}

/// Print the most general unifier, or a clash.
fn unify(label: &str, a: &Term, b: &Term) {
    let mut s = Subst::new();
    if term::unify(a, b, &mut s, true) {
        let mut binds: Vec<_> = s
            .iter()
            .map(|(k, v)| format!("{k} = {}", term::apply(v, &s)))
            .collect();
        binds.sort();
        println!("{label}: {{{}}}", binds.join(", "));
    } else {
        println!("{label}: clash");
    }
}

fn main() {
    let (a, b) = (Term::var("A"), Term::var("B"));
    let (int, bool_, unit) = (Term::leaf("Int"), Term::leaf("Bool"), Term::leaf("Unit"));

    // Application: ?A -> ?B against Int -> Bool.
    unify(
        "app  ",
        &arrow(a.clone(), b.clone()),
        &arrow(int.clone(), bool_.clone()),
    );

    // A variable captures a whole subtree — what leftmost string-splitting could not.
    unify(
        "curry",
        &arrow(a.clone(), b.clone()),
        &arrow(int.clone(), arrow(int.clone(), bool_.clone())),
    );

    // A genuine mismatch is rejected.
    unify("clash", &arrow(int.clone(), bool_.clone()), &arrow(int, unit.clone()));

    // Unification modulo a theory: Bool ⇝ Unit + Unit makes the two equal.
    let theory = Normalizer::from_rules(vec![RewriteRule {
        lhs: bool_.clone(),
        rhs: sum(unit.clone(), unit.clone()),
    }]);
    let mut s = Subst::new();
    let ok = unify_modulo(&theory, &bool_, &sum(unit.clone(), unit), &mut s, true);
    println!("modulo: Bool ≡ Unit + Unit  ->  {ok}");
}
