//! Execution tracing for the IR — compiled out unless the `trace` feature is on.
//!
//! `SPG.ir(rule)` shows the *static* program: which instructions, in what order.
//! It cannot show why a run went wrong — which register failed to resolve, which
//! ascription contradicted, how deep the scope stack was, or which binding the
//! parser descended into. Without this, that has to be reconstructed by
//! black-box probing of `status()`.
//!
//! # Cost
//!
//! Nothing, when the feature is off. Recording goes through [`trace!`], whose
//! whole body — including building the [`Step`] and formatting its strings —
//! sits inside `#[cfg(feature = "trace")]`. There is no branch to predict and no
//! argument to evaluate, so `run` is byte-identical to the untraced build.
//!
//! ```ignore
//! cargo test --features trace
//! ```

/// What one step of execution did.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Step {
    /// A rule program started running for a node.
    Enter { rule: String, node: String },
    /// One instruction ran.
    Instr {
        /// Open `PushScope`s after this instruction: premise-local scope depth.
        scope: usize,
        /// Index into the program's instruction stream.
        pc: usize,
        /// The instruction, rendered.
        instr: String,
        /// What it produced, or why it could not.
        outcome: String,
    },
    /// The verdict the program reached.
    Leave { rule: String, verdict: String },
    /// The parser descended into a child, building its context from a splice.
    /// This is where *tree* depth changes, as opposed to scope depth.
    Descend {
        rule: String,
        binding: String,
        outcome: String,
    },
}

/// Record a [`Step`]. Expands to nothing without the `trace` feature, so the
/// step is never built.
#[macro_export]
macro_rules! trace {
    ($trace:expr, $step:expr) => {
        #[cfg(feature = "trace")]
        {
            $trace.push($step);
        }
    };
}

#[cfg(feature = "trace")]
mod imp {
    use super::Step;
    use std::cell::RefCell;
    use std::rc::Rc;

    /// A shared trace buffer. Cloning shares it, because a `TypingDomain` is
    /// cloned into both the parser and the runtime and they must record to the
    /// same place.
    #[derive(Clone, Debug, Default)]
    pub struct Trace {
        steps: Rc<RefCell<Vec<Step>>>,
    }

    impl Trace {
        pub fn push(&self, step: Step) {
            self.steps.borrow_mut().push(step);
        }

        /// Drop everything recorded so far. Call before a run to be explained.
        pub fn clear(&self) {
            self.steps.borrow_mut().clear();
        }

        #[must_use]
        pub fn steps(&self) -> Vec<Step> {
            self.steps.borrow().clone()
        }
    }
}

#[cfg(not(feature = "trace"))]
mod imp {
    use super::Step;

    /// Zero-sized stand-in. Every method is a no-op; `trace!` never calls
    /// `push`, so nothing here is reachable in a normal build.
    #[derive(Clone, Debug, Default)]
    pub struct Trace;

    impl Trace {
        #[inline(always)]
        pub fn push(&self, _step: Step) {}
        #[inline(always)]
        pub fn clear(&self) {}
        #[must_use]
        #[inline(always)]
        pub fn steps(&self) -> Vec<Step> {
            Vec::new()
        }
    }
}

pub use imp::Trace;

impl Trace {
    /// The trace as an indented log.
    ///
    /// Indentation is *tree* depth (one level per `Enter`); the `+N` column is
    /// *scope* depth inside a program. A premise-local setting shows as `+1`
    /// without indenting, so opening a scope and descending into a child are
    /// visibly different things.
    ///
    /// Empty without the `trace` feature.
    #[must_use]
    pub fn render(&self) -> String {
        let mut out = String::new();
        let mut depth = 0usize;
        for step in self.steps() {
            let pad = "  ".repeat(depth);
            match step {
                Step::Enter { rule, node } => {
                    out.push_str(&format!("{pad}{rule} <{node}>\n"));
                    depth += 1;
                }
                Step::Leave { rule, verdict } => {
                    depth = depth.saturating_sub(1);
                    out.push_str(&format!("{}{rule} => {verdict}\n", "  ".repeat(depth)));
                }
                Step::Instr {
                    scope,
                    pc,
                    instr,
                    outcome,
                } => {
                    let s = if scope > 0 {
                        format!("+{scope}")
                    } else {
                        "  ".into()
                    };
                    out.push_str(&format!("{pad}{s} {pc:>2}| {instr:<30} {outcome}\n"));
                }
                Step::Descend {
                    rule,
                    binding,
                    outcome,
                } => out.push_str(&format!("{pad}↳ descend {rule}/{binding}: {outcome}\n")),
            }
        }
        out
    }
}

#[cfg(all(test, feature = "trace"))]
mod tests {
    use crate::grammar::SPG;
    use crate::typing::TypingSynth;

    /// A traced run names every instruction, what it produced, the scope depth,
    /// and each descent. This is what makes an "it just stays live" failure
    /// diagnosable without black-box probing.
    #[test]
    fn trace_shows_instructions_and_scope() {
        let g = SPG::load(include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/examples/stlc.auf"
        )))
        .unwrap();
        let mut s = TypingSynth::new(g, "λx:A.x");
        let log = s.explain();

        assert!(log.contains("lambda"), "no rule entered:\n{log}");
        assert!(log.contains("ascribe"), "no ascription step:\n{log}");
        assert!(log.contains("push_scope"), "no scope step:\n{log}");
        assert!(log.contains("descend"), "no descent:\n{log}");
        assert!(log.contains("+1"), "scope depth not shown:\n{log}");
    }

    /// The trace names the *unresolved* register, which is the case that is
    /// otherwise invisible: the node just stays `live` forever.
    #[test]
    fn trace_names_unresolved_lookups() {
        let g = SPG::load(
            "Identifier ::= /[a-z]+/\nMarked(marked) ::= 'return'[r] Identifier[x]\nExpr ::= Marked\n\n----------- (marked)\nΓ(r)\n",
        )
        .unwrap();
        let mut s = TypingSynth::new(g, "return foo");
        let log = s.explain();
        assert!(
            log.contains("UNRESOLVED"),
            "unresolved not reported:\n{log}"
        );
    }
}
