use crate::cli::validate::{RunnableCase, RunnableSuite, ValidateCmd};
use aufbau::validation::completable;

pub fn run(args: &ValidateCmd) {
    let all = completable::all_suites();

    let suites: Vec<RunnableSuite> = all
        .into_iter()
        .map(|(name, grammar, cases)| {
            let runnable_cases: Vec<RunnableCase> = cases
                .into_iter()
                .map(|mut case| {
                    if let Some(timeout_secs) = args.completable_timeout_secs {
                        case.timeout_secs = timeout_secs;
                    }
                    let g = grammar.clone();
                    let desc = case.description.to_string();
                    let input = case.input.to_string();
                    RunnableCase {
                        desc,
                        input,
                        run: Box::new(move || {
                            let (result, _duration, _meta) =
                                completable::run_test_timed_meta(&g, &case);
                            match result {
                                completable::TestResult::Pass(_) => (true, String::new()),
                                completable::TestResult::Fail(msg) => (false, msg),
                            }
                        }),
                    }
                })
                .collect();

            RunnableSuite {
                name,
                cases: runnable_cases,
            }
        })
        .collect();

    super::run_suites("completable", suites, args);
}
