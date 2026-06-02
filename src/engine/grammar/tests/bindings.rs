use crate::engine::binding;
use crate::engine::grammar::SPG;

fn steps(path: &binding::GrammarPath) -> Vec<(usize, usize)> {
    path.steps().iter().map(|s| (s.i, s.a)).collect()
}

#[test]
fn stlc_abs_binding_paths_match_spec() {
    let spec = include_str!("../../../../examples/stlc.auf");
    let grammar = SPG::load(spec).expect("load stlc");

    let assert_path = |binding: &str, rule: &str, expected: Vec<Vec<(usize, usize)>>| {
        let paths = grammar
            .bindings
            .as_ref()
            .unwrap()
            .get(binding, rule)
            .unwrap_or_else(|| panic!("missing paths for {}:{}", binding, rule));
        assert_eq!(
            paths.len(),
            expected.len(),
            "path count mismatch for {}:{}",
            binding,
            rule
        );
        for (path, expected_steps) in paths.iter().zip(expected.iter()) {
            assert_eq!(
                steps(path),
                *expected_steps,
                "unexpected path for {}:{}",
                binding,
                rule
            );
        }
    };

    assert_path("a", "lambda", vec![vec![(1, 0)]]);
    assert_path("e", "lambda", vec![vec![(5, 0)]]);
    assert_path("τ", "lambda", vec![vec![(3, 0)]]);

    assert_path("l", "app", vec![vec![(0, 0)]]);
    assert_path("r", "app", vec![vec![(1, 0)]]);
}

#[test]
fn repeated_binding_produces_multiple_paths() {
    let spec = r#"
    Number(num) ::= /[0-9]+/
    Pair(pair) ::= Number[x] ',' Number[x]

    Γ ⊢ x : 'number'
    ----------------- (pair)
    'number'
    "#;

    let grammar = SPG::load(spec).expect("load pair grammar");
    let paths = grammar
        .bindings
        .as_ref()
        .unwrap()
        .get("x", "pair")
        .expect("binding paths for repeated x");

    assert_eq!(paths.len(), 2, "repeated binding should keep both paths");
    assert_eq!(steps(&paths[0]), vec![(0, 0)]);
    assert_eq!(steps(&paths[1]), vec![(2, 0)]);
}

#[test]
fn repetition_helpers_preserve_inner_binding_paths() {
    let spec = r#"
    Item(item) ::= Number[x]
    Seq(seq) ::= Item*

    Γ ⊢ x : 'number'
    ----------------- (item)
    'number'
    "#;

    let grammar = SPG::load(spec).expect("load repetition grammar");
    let paths = grammar
        .bindings
        .as_ref()
        .unwrap()
        .get("x", "item")
        .expect("binding paths for repeated item");

    assert_eq!(paths.len(), 1);
    assert_eq!(steps(&paths[0]), vec![(0, 0)]);
}
