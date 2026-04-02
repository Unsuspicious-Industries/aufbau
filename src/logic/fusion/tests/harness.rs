use crate::logic::fusion::{CtxId, MetaTypedParser, RuleRuntime, TypedParser};
use crate::logic::grammar::Grammar;
use crate::logic::typing::Context;

#[derive(Clone, Debug)]
pub enum Expectation {
    Pass,
    XFail,
}

#[derive(Clone, Debug)]
pub enum Completeness {
    /// At least one complete root must exist.
    Complete,
    /// No complete roots; at least one root exists (i.e. prefix is accepted).
    PartialOnly,
    /// No constraint (default).
    Any,
}

#[derive(Clone, Debug)]
pub struct FusionTestCase {
    pub name: &'static str,
    pub grammar: Grammar,
    pub input: &'static str,
    pub ctx: Context,
    pub max_depth: u16,
    pub expect: Expectation,
    pub completeness: Completeness,
}

impl FusionTestCase {
    pub fn pass(name: &'static str, grammar: Grammar, input: &'static str) -> Self {
        Self {
            name,
            grammar,
            input,
            ctx: Context::new(),
            max_depth: 62,
            expect: Expectation::Pass,
            completeness: Completeness::Any,
        }
    }

    pub fn xfail(name: &'static str, grammar: Grammar, input: &'static str) -> Self {
        Self {
            name,
            grammar,
            input,
            ctx: Context::new(),
            max_depth: 62,
            expect: Expectation::XFail,
            completeness: Completeness::Any,
        }
    }

    pub fn pass_spec(name: &'static str, spec: &'static str, input: &'static str) -> Self {
        let grammar = Grammar::load(spec).unwrap_or_else(|e| panic!("bad grammar spec: {}", e));
        Self::pass(name, grammar, input)
    }

    pub fn xfail_spec(name: &'static str, spec: &'static str, input: &'static str) -> Self {
        let grammar = Grammar::load(spec).unwrap_or_else(|e| panic!("bad grammar spec: {}", e));
        Self::xfail(name, grammar, input)
    }

    #[allow(dead_code)]
    pub fn with_ctx(mut self, ctx: Context) -> Self {
        self.ctx = ctx;
        self
    }

    pub fn with_max_depth(mut self, depth: u16) -> Self {
        self.max_depth = depth;
        self
    }

    pub fn require_complete(mut self) -> Self {
        self.completeness = Completeness::Complete;
        self
    }

    pub fn require_partial_only(mut self) -> Self {
        self.completeness = Completeness::PartialOnly;
        self
    }
}

#[derive(Clone, Debug)]
pub struct FusionCaseFailure {
    pub name: &'static str,
    pub input: &'static str,
    pub expected: Expectation,
    pub error: String,
}

fn run_one(case: &FusionTestCase) -> Result<(), FusionCaseFailure> {
    let runtime = RuleRuntime::new(case.grammar.clone());
    let ctx_id: CtxId = runtime.intern_context(case.ctx.clone());

    let parser =
        TypedParser::new(case.grammar.clone(), runtime.clone()).with_max_depth(case.max_depth);
    let meta = MetaTypedParser::new(parser)
        .with_start_depth(case.max_depth.min(8))
        .with_max_depth(case.max_depth)
        .with_depth_factor(1.5);

    let res = meta
        .parse_with_arena(case.input, ctx_id)
        .map(|(state, _depth, arena)| (state, arena));
    match (&case.expect, res) {
        (Expectation::Pass, Ok((state, arena))) => {
            if state.roots.is_empty() {
                Err(FusionCaseFailure {
                    name: case.name,
                    input: case.input,
                    expected: case.expect.clone(),
                    error: "pass case produced zero roots".to_string(),
                })
            } else {
                let has_complete = state.roots.iter().any(|id| {
                    arena.node(*id).is_some_and(|n| {
                        matches!(n.status, crate::logic::fusion::NodeStatus::Complete)
                    })
                });
                match case.completeness {
                    Completeness::Any => Ok(()),
                    Completeness::Complete => {
                        if has_complete {
                            Ok(())
                        } else {
                            Err(FusionCaseFailure {
                                name: case.name,
                                input: case.input,
                                expected: case.expect.clone(),
                                error: "expected a complete root, found only partial roots"
                                    .to_string(),
                            })
                        }
                    }
                    Completeness::PartialOnly => {
                        if has_complete {
                            Err(FusionCaseFailure {
                                name: case.name,
                                input: case.input,
                                expected: case.expect.clone(),
                                error: "expected only partial roots, but a complete root exists"
                                    .to_string(),
                            })
                        } else {
                            Ok(())
                        }
                    }
                }
            }
        }
        (Expectation::Pass, Err(err)) => Err(FusionCaseFailure {
            name: case.name,
            input: case.input,
            expected: case.expect.clone(),
            error: err.to_string(),
        }),
        (Expectation::XFail, Ok(_)) => Err(FusionCaseFailure {
            name: case.name,
            input: case.input,
            expected: case.expect.clone(),
            error: "xfail unexpectedly passed".to_string(),
        }),
        (Expectation::XFail, Err(_)) => Ok(()),
    }
}

pub fn run_cases(cases: &[FusionTestCase]) {
    let mut failures: Vec<FusionCaseFailure> = Vec::new();
    for case in cases {
        if let Err(f) = run_one(case) {
            failures.push(f);
        }
    }

    if failures.is_empty() {
        return;
    }

    let mut msg = String::new();
    msg.push_str(&format!("\n{} fusion case(s) failed:\n", failures.len()));
    msg.push_str(&"=".repeat(72));
    msg.push('\n');
    for (i, f) in failures.iter().enumerate() {
        msg.push_str(&format!(
            "\n[{}] {} expect={:?}\n  input: {}\n  error: {}\n",
            i + 1,
            f.name,
            f.expected,
            f.input,
            f.error
        ));
    }
    msg.push('\n');
    msg.push_str(&"=".repeat(72));
    msg.push('\n');

    panic!("{}", msg);
}
