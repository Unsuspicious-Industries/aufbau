use crate::logic::grammar::Grammar;
use crate::logic::partial::MetaParser;
use crate::logic::Parser;

#[test]
fn test_metaparser_multiplicative_growth() {
    // Verify MetaParser grows depth multiplicatively (ceil(d * factor)), and
    // that it returns the first depth in the growth sequence that can parse.
    let spec = r#"
    Chain ::= Chain 'a' | 'a'
    start ::= Chain
    "#;
    let g = Grammar::load(spec).unwrap();

    // choose an input that requires a non-trivial recursion depth
    let chain_len = 12usize;
    let input = (0..chain_len).map(|_| "a").collect::<Vec<_>>().join(" ");

    // find the minimal depth that makes a raw Parser succeed
    let mut baseline = Parser::new(g.clone());
    let mut min_depth = None;
    for d in 1..=100usize {
        baseline.set_max_recursion(d);
        match baseline.partial(&input) {
            crate::logic::partial::PartialParseOutcome::Success { .. } => {
                min_depth = Some(d);
                break;
            }
            _ => {}
        }
    }
    let min_depth = min_depth.expect("should find a depth that succeeds");

    // Configure MetaParser with multiplicative growth starting low
    let start = 2usize;
    let factor = 1.5f64;
    let mut mp = MetaParser::new(g.clone())
        .with_start_depth(start)
        .with_max_depth(200)
        .with_post_success_steps(0)
        .with_depth_factor(factor);

    // compute expected depth produced by multiplicative growth
    let mut expected = start;
    while expected < min_depth {
        let mut next = ((expected as f64) * factor).ceil() as usize;
        if next <= expected {
            next = expected + 1;
        }
        expected = next;
    }

    // run partial and confirm it returns our expected growth value
    let (_ast, used_depth) = mp
        .partial_with_depth(&input)
        .expect("meta parse should succeed");
    assert_eq!(used_depth, expected);
    assert!(used_depth >= min_depth);
}

#[test]
fn test_metaparser_does_not_carry_depth_between_inputs() {
    // Ensure each input starts from configured start_depth.
    // A previous deep parse must not bias the next unrelated parse.
    let spec = r#"
    Chain ::= Chain 'a' | 'a'
    start ::= Chain
    "#;
    let g = Grammar::load(spec).unwrap();

    let mut mp = MetaParser::new(g)
        .with_start_depth(2)
        .with_max_depth(128)
        .with_post_success_steps(0)
        .with_depth_factor(2.0);

    let deep_input = (0..14).map(|_| "a").collect::<Vec<_>>().join(" ");
    let (_deep_ast, deep_depth) = mp.partial_with_depth(&deep_input).unwrap();
    assert!(deep_depth > 2, "expected deep parse to need growth");

    let (_short_ast, short_depth) = mp.partial_with_depth("a").unwrap();
    assert_eq!(
        short_depth, 2,
        "MetaParser should restart from start_depth for each input"
    );
}
