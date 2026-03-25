use crate::cli::validate::{RunnableCase, RunnableSuite, ValidateCmd};
use aufbau::validation::parseable;

pub fn run(args: &ValidateCmd) {
    let all = parseable::all_suites();

    let suites: Vec<RunnableSuite> = all
        .into_iter()
        .map(|(name, grammar, valids, invalids)| {
            let mut cases: Vec<RunnableCase> = Vec::with_capacity(valids.len() + invalids.len());

            for case in valids.into_iter().chain(invalids.into_iter()) {
                let g = grammar.clone();
                let desc = case.description.to_string();
                let input = case.input.to_string();
                cases.push(RunnableCase {
                    desc,
                    input,
                    run: Box::new(move || {
                        let result = parseable::run_parse_test(&g, &case);
                        match result {
                            parseable::ParseResult::Pass { .. } => (true, String::new()),
                            parseable::ParseResult::Fail {
                                failing_prefix,
                                error,
                                ..
                            } => (
                                false,
                                format!("Failing prefix: '{}'\nError: {}", failing_prefix, error),
                            ),
                        }
                    }),
                });
            }

            RunnableSuite { name, cases }
        })
        .collect();

    super::run_suites("parseable", suites, args);
}
