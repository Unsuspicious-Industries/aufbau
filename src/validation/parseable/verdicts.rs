//! Prefix verdicts pinned by example: where the oracle is precise, and where it
//! is not.
//!
//! [`check_all_prefixes_parseable`] cannot express either half of this. It asks
//! whether a prefix is *accepted*, so it cannot tell a prefix that is accepted
//! and completable from one that is accepted and dead, and it never asserts
//! that a prefix is rejected. Those are the two decoder contracts, so each gets
//! its own list here.
//!
//! [`check_all_prefixes_parseable`]: super::check_all_prefixes_parseable

use crate::grammar::SPG;
use crate::typing::{Context, TypingSynth};

/// `(grammar, prefix, why)`.
type Case = (&'static str, &'static str, &'static str);

/// `(grammar, prefix, witness, why)` -- a completable prefix and something that
/// completes it. The witness must extend the prefix and must type, and both are
/// asserted; see [`MUST_STAY_LIVE`].
type LiveCase = (&'static str, &'static str, &'static str, &'static str);

/// Accepted today, and genuinely completable: rejecting one is a false prune,
/// which breaks safe pruning — the contract that is supposed to hold for every
/// grammar the front end accepts.
///
/// These are not decoration. Every entry was produced by an implementation of
/// eager refutation that read correctly and broke here. Refuting a demand
/// against an open child node is unsound, because a *longer* node can still
/// fill the same obligation at a different type: at `(h +` the arm body is a
/// parenthesised `int`, and the obligation ends up filled by `(h + 1) :: t` at
/// `int list`. Any future attempt to refute earlier has to survive this list.
///
/// **Every case carries a witness, and the witness is checked.** A prefix is
/// only completable if something completes it, and asserting that on faith is
/// how this list came to report a soundness violation that did not exist: the
/// four `ml` cases were written as bare expressions (`let f : int = (`) and
/// copied in after the grammar had grown a `Program`/`Define` top level, where
/// a program must begin `let <name> (`. They were therefore *correctly* dead,
/// and `no_false_prunes` failed from the day it was written -- recorded as a
/// false prune in the engine rather than as stale test data. The witness column
/// makes that failure mode impossible to misread: a case whose witness does not
/// type is a broken case, and the test says so in those words.
const MUST_STAY_LIVE: &[LiveCase] = &[
    (
        "fun",
        "let x : Bool = tru",
        "let x : Bool = true ; x",
        "tru extends to the keyword true, which has the demanded type",
    ),
    (
        "fun",
        "let n : Int = 1 ; let nb : Bool = true ; let x : Bool = n",
        "let n : Int = 1 ; let nb : Bool = true ; let x : Bool = nb ; x",
        "n can still grow into nb, which is the demanded Bool",
    ),
    (
        "ml",
        "let solve (xs : int list) : int = (",
        "let solve (xs : int list) : int = ( 1 )",
        "a paren can still open an expression of the demanded type",
    ),
    (
        "ml",
        "let solve (xs : int list) : int = 1 + (",
        "let solve (xs : int list) : int = 1 + ( 2 )",
        "same, with the demand coming from an arithmetic premise",
    ),
    (
        "ml",
        "let solve (xs : int list) : int list = 1",
        "let solve (xs : int list) : int list = 1 :: [ ]",
        "1 concludes int, but 1 :: [] concludes int list",
    ),
    (
        "ml",
        "let inc (xs : int list) : int list = \
         match xs with [ ] -> [ ] | h :: t -> (h +",
        "let inc (xs : int list) : int list = \
         match xs with [ ] -> [ ] | h :: t -> (h + 1) :: t",
        "the arm body is int here and int list once the cons arrives",
    ),
    (
        "fun",
        "let x : Bool = (y : Int) =>",
        "let x : Bool = (y : Int) => true (1) ; x",
        "an abstraction cannot be Bool, but it can be the function of an \
         application that is: (y : Int) => true (1) has type Bool",
    ),
];

/// Rejected today, and genuinely uncompletable: accepting one is lost
/// precision. The dual guard — it catches a change that buys back the false
/// prunes above by refuting nothing at all.
const MUST_STAY_DEAD: &[Case] = &[
    (
        "fun",
        "let x : Bool = 1",
        "constant conclusion: every digit-initial expression concludes Int or \
         Float, and neither is Bool — refuted at prediction",
    ),
    (
        "fun",
        "let x : Int = tru",
        "tru extends only to the keyword true, and the context is empty, so no \
         variable can supply an Int — refuted at prediction",
    ),
    (
        "fun",
        "let x : Int = true",
        "a closed keyword clashes with the annotation at once",
    ),
    (
        "fun",
        "let x : Bool = 1 ;",
        "the separator closes the literal, so Int vs Bool is exact",
    ),
    (
        "fun",
        "let n : Int = 1 ; let nb : Bool = true ; let x : Bool = n ;",
        "the separator closes the name, and n is the Int one",
    ),
];

/// Accepted today with **no** completion at all: the honest record of the
/// oracle's imprecision, and the gap between the shipped evaluator and the one
/// the dead-end-freedom theorem describes.
///
/// What remains is the context-lookup conclusion. Prediction filtering compares
/// a demand against the rule's conclusion *pattern*, and `var` concludes
/// `Γ(x)`, whose pattern is a bare hole — so it admits everything. Deciding it
/// means enumerating the context entries whose key extends the lexeme read so
/// far, which the pattern abstraction does not do.
///
/// **If an entry here starts being rejected, that is progress.** Move it to
/// `MUST_STAY_DEAD` rather than restoring the old behaviour.
const KNOWN_DEAD_END: &[Case] = &[(
    "fun",
    "let n : Int = 1 ; let x : Bool = n",
    "a variable's conclusion is a context lookup, so its prediction-time \
     pattern is a bare hole and admits every demand; deciding it needs the \
     finitely many context entries whose key extends the lexeme read so far",
)];

/// Is `input` retained by the oracle — accepted as a prefix, with at least one
/// surviving reading?
fn live(grammar: &SPG, input: &str) -> bool {
    let mut synth = TypingSynth::new(grammar.clone(), input);
    synth
        .parse_with(&Context::new())
        .is_ok_and(|roots| !roots.is_empty())
}

/// Does `input` have a complete, well-typed root? The `"typed"` of `status()`.
fn typed(grammar: &SPG, input: &str) -> bool {
    let mut synth = TypingSynth::new(grammar.clone(), input);
    let Ok(ast) = synth.parse_with(&Context::new()) else {
        return false;
    };
    let rt = synth.runtime().clone();
    ast.roots()
        .filter(crate::ast::FusionNode::is_complete)
        .any(|r| rt.evidence_of(r.evidence()).is_some())
}

/// Whitespace-insensitive prefix test, so a witness written with the line
/// continuations this file needs still counts as extending its prefix.
fn extends(witness: &str, prefix: &str) -> bool {
    let squash = |s: &str| s.split_whitespace().collect::<Vec<_>>().join(" ");
    squash(witness).starts_with(&squash(prefix))
}

/// Every live case's witness must extend its prefix and must type.
///
/// This is the check whose absence let stale test data read as an engine bug.
/// A prefix is completable only if something completes it; without a witness,
/// "genuinely completable" is an assertion nobody verified, and four `ml` cases
/// sat here for weeks describing a soundness violation that did not exist.
fn broken_witnesses(cases: &[LiveCase]) -> Vec<String> {
    cases
        .iter()
        .filter_map(|(g, input, witness, _)| {
            let grammar = super::load_example_grammar(g);
            if !extends(witness, input) {
                return Some(format!(
                    "  [{g}] witness does not extend its prefix\n      prefix:  {input:?}\n      witness: {witness:?}"
                ));
            }
            if !typed(&grammar, witness) {
                return Some(format!(
                    "  [{g}] witness does not type, so the prefix is not known to be completable\n      prefix:  {input:?}\n      witness: {witness:?}"
                ));
            }
            None
        })
        .collect()
}

fn verdicts(cases: &[Case], want_live: bool) -> Vec<String> {
    cases
        .iter()
        .filter(|(g, input, _)| live(&super::load_example_grammar(g), input) != want_live)
        .map(|(g, input, why)| format!("  [{g}] {input:?}\n      {why}"))
        .collect()
}

fn live_verdicts(cases: &[LiveCase]) -> Vec<String> {
    cases
        .iter()
        .filter(|(g, input, _, _)| !live(&super::load_example_grammar(g), input))
        .map(|(g, input, _, why)| format!("  [{g}] {input:?}\n      {why}"))
        .collect()
}

#[test]
fn every_live_case_has_a_working_witness() {
    let broken = broken_witnesses(MUST_STAY_LIVE);
    assert!(
        broken.is_empty(),
        "{} live case(s) are not backed by a witness that types. Fix the case, \
         not the engine — an uncompletable prefix is *correctly* dead, and \
         reading one as a false prune is what this check exists to prevent:\n{}",
        broken.len(),
        broken.join("\n")
    );
}

#[test]
fn no_false_prunes() {
    // Witnesses first: if a case is not completable at all, "rejected" is the
    // right answer and the failure below would be misleading.
    let broken = broken_witnesses(MUST_STAY_LIVE);
    assert!(
        broken.is_empty(),
        "cannot judge false prunes while {} case(s) have no working witness; \
         see every_live_case_has_a_working_witness:\n{}",
        broken.len(),
        broken.join("\n")
    );
    let bad = live_verdicts(MUST_STAY_LIVE);
    assert!(
        bad.is_empty(),
        "{} completable prefix(es) rejected — this is a false prune, and safe \
         pruning is supposed to hold for every grammar:\n{}",
        bad.len(),
        bad.join("\n")
    );
}

#[test]
fn refutations_are_kept() {
    let bad = verdicts(MUST_STAY_DEAD, false);
    assert!(
        bad.is_empty(),
        "{} uncompletable prefix(es) now accepted — precision lost:\n{}",
        bad.len(),
        bad.join("\n")
    );
}

#[test]
fn known_dead_ends_are_unchanged() {
    let fixed = verdicts(KNOWN_DEAD_END, true);
    assert!(
        fixed.is_empty(),
        "{} known dead end(s) are now refuted. This is progress: move them to \
         MUST_STAY_DEAD and check `no_false_prunes` still passes.\n{}",
        fixed.len(),
        fixed.join("\n")
    );
}
