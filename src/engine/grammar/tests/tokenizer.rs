use crate::engine::grammar::tokenizer::{DEFAULT_DELIMITERS, Segment, Tokenizer};

#[test]
fn test_tokenize_with_special_tokens() {
    let input = "x=r+4;print(x)";
    let special_tokens = vec!["+".to_string(), "=".to_string()];
    let delimiters = vec![';', '(', ')'];
    let mut tokenizer = Tokenizer::with_specials_and_delimiters(special_tokens, delimiters);

    let segments = tokenizer.tokenize(input).unwrap();
    let token_strs: Vec<_> = segments.iter().map(|seg| seg.text()).collect();

    assert_eq!(token_strs, vec!["x", "=", "r", "+", "4", "print", "x"]);
}

#[test]
fn test_tokenize_with_spans_positions() {
    let input = "int x = 5;";
    let special_tokens = vec!["int".to_string(), "=".to_string(), ";".to_string()];
    let delimiters = vec![' ', '\t', '\n'];
    let mut tokenizer = Tokenizer::with_specials_and_delimiters(special_tokens, delimiters);
    let segments = tokenizer.tokenize(input).unwrap();

    let strs: Vec<_> = segments.iter().map(|seg| seg.text()).collect();
    assert_eq!(strs, vec!["int", "x", "=", "5", ";"]);

    assert_eq!(segments[0].start, 0);
    assert_eq!(segments[0].end, 3);
    assert_eq!(segments[1].start, 4);
    assert_eq!(segments[1].end, 5);
}

#[test]
fn test_partial_special_token_at_end() {
    let input = "foo-";
    let special_tokens = vec!["->".to_string()];
    let delimiters = vec![' '];
    let mut tokenizer = Tokenizer::with_specials_and_delimiters(special_tokens, delimiters);

    let segments = tokenizer.tokenize(input).unwrap();
    assert_eq!(segments.len(), 2);
    assert_eq!(segments[0].text(), "foo");
    assert!(!segments[0].is_partial_special);
    assert_eq!(segments[1].text(), "-");
    assert!(segments[1].is_partial_special);
}

#[test]
fn test_complete_special_token_not_partial() {
    let input = "foo->";
    let special_tokens = vec!["->".to_string()];
    let delimiters = vec![' '];
    let mut tokenizer = Tokenizer::with_specials_and_delimiters(special_tokens, delimiters);

    let segments = tokenizer.tokenize(input).unwrap();
    assert_eq!(segments.len(), 2);
    assert_eq!(segments[0].text(), "foo");
    assert_eq!(segments[1].text(), "->");
    assert!(!segments[1].is_partial_special);
}

#[test]
fn test_partial_special_token_in_lambda_type() {
    let input = "λf:(A-";
    let special_tokens = vec!["->".to_string(), "λ".to_string(), ":".to_string()];
    let delimiters = vec![' ', '(', ')'];
    let mut tokenizer = Tokenizer::with_specials_and_delimiters(special_tokens, delimiters);

    let segments = tokenizer.tokenize(input).unwrap();
    let tokens: Vec<_> = segments.iter().map(|s| s.text()).collect();
    assert_eq!(tokens, vec!["λ", "f", ":", "A", "-"]);
    assert!(segments[4].is_partial_special);
}

fn fun_tokenizer() -> Tokenizer {
    let special_tokens = vec![
        "->".into(),
        "λ".into(),
        ":".into(),
        ".".into(),
        "=".into(),
        "let".into(),
        "in".into(),
        "if".into(),
        "then".into(),
        "else".into(),
        "true".into(),
        "false".into(),
        "(".into(),
        ")".into(),
    ];
    Tokenizer::with_specials_and_delimiters(special_tokens, vec![' ', '\n', '\t'])
}

fn tok(tokenizer: &mut Tokenizer, input: &str) -> Vec<(String, bool)> {
    tokenizer
        .tokenize(input)
        .unwrap()
        .iter()
        .map(|s| (s.text(), s.is_partial_special))
        .collect()
}

#[test]
fn fun_typename_int_standalone() {
    let mut t = fun_tokenizer();
    assert_eq!(tok(&mut t, "Int"), vec![("Int".into(), false)]);
}

#[test]
fn fun_typename_bool_standalone() {
    let mut t = fun_tokenizer();
    assert_eq!(tok(&mut t, "Bool"), vec![("Bool".into(), false)]);
}

#[test]
fn fun_typename_int_in_lambda() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "λx:Int");
    assert_eq!(
        result,
        vec![
            ("λ".into(), false),
            ("x".into(), false),
            (":".into(), false),
            ("Int".into(), false),
        ]
    );
}

#[test]
fn fun_typename_int_before_dot() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "λx:Int.x");
    assert_eq!(
        result,
        vec![
            ("λ".into(), false),
            ("x".into(), false),
            (":".into(), false),
            ("Int".into(), false),
            (".".into(), false),
            ("x".into(), false),
        ]
    );
}

#[test]
fn fun_typename_int_before_arrow() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "Int->Bool");
    assert_eq!(
        result,
        vec![
            ("Int".into(), false),
            ("->".into(), false),
            ("Bool".into(), false),
        ]
    );
}

#[test]
fn fun_typename_int_before_space() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "Int Bool");
    assert_eq!(result, vec![("Int".into(), false), ("Bool".into(), false),]);
}

#[test]
fn fun_typename_ending_in_t() {
    let mut t = fun_tokenizer();
    for name in &["Int", "Nat", "Set", "Abst"] {
        let result = tok(&mut t, name);
        assert_eq!(
            result,
            vec![(name.to_string(), false)],
            "TypeName '{}' should tokenize as a single token",
            name
        );
    }
}

#[test]
fn fun_typename_ending_in_e() {
    let mut t = fun_tokenizer();
    for name in &["Type", "Name", "Base"] {
        let result = tok(&mut t, name);
        assert_eq!(
            result,
            vec![(name.to_string(), false)],
            "TypeName '{}' should tokenize as a single token",
            name
        );
    }
}

#[test]
fn fun_typename_ending_in_f() {
    let mut t = fun_tokenizer();
    for name in &["Ref", "Def"] {
        let result = tok(&mut t, name);
        assert_eq!(
            result,
            vec![(name.to_string(), false)],
            "TypeName '{}' should tokenize as a single token",
            name
        );
    }
}

#[test]
fn fun_typename_ending_in_i() {
    let mut t = fun_tokenizer();
    for name in &["Fi", "Pi"] {
        let result = tok(&mut t, name);
        assert_eq!(
            result,
            vec![(name.to_string(), false)],
            "TypeName '{}' should tokenize as a single token",
            name
        );
    }
}

#[test]
fn fun_identifier_ending_in_keyword_prefix() {
    let mut t = fun_tokenizer();
    for name in &["nat", "set", "ref", "pi"] {
        let result = tok(&mut t, name);
        assert_eq!(
            result,
            vec![(name.to_string(), false)],
            "identifier '{}' should tokenize as a single token",
            name
        );
    }
}

#[test]
fn fun_identifier_containing_keyword() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "int");
    assert_eq!(
        result,
        vec![
            ("in".into(), false),
            ("t".into(), true),
        ]
    );
    let result = tok(&mut t, "iff");
    assert_eq!(
        result,
        vec![
            ("if".into(), false),
            ("f".into(), true),
        ]
    );
}

#[test]
fn fun_keywords_match_exactly() {
    let mut t = fun_tokenizer();
    for kw in &["let", "in", "if", "then", "else", "true", "false"] {
        let result = tok(&mut t, kw);
        assert_eq!(
            result,
            vec![(kw.to_string(), false)],
            "keyword '{}' should tokenize as a single special token",
            kw
        );
    }
}

#[test]
fn fun_keyword_followed_by_space_and_identifier() {
    let mut t = fun_tokenizer();
    assert_eq!(
        tok(&mut t, "let x"),
        vec![("let".into(), false), ("x".into(), false),]
    );
    assert_eq!(
        tok(&mut t, "if true then 1 else 2"),
        vec![
            ("if".into(), false),
            ("true".into(), false),
            ("then".into(), false),
            ("1".into(), false),
            ("else".into(), false),
            ("2".into(), false),
        ]
    );
}

#[test]
fn fun_partial_arrow_after_typename() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "Int-");
    assert_eq!(
        result,
        vec![
            ("Int".into(), false),
            ("-".into(), true),
        ]
    );
}

#[test]
fn fun_partial_arrow_after_identifier() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "foo-");
    assert_eq!(result, vec![("foo".into(), false), ("-".into(), true),]);
}

#[test]
fn fun_partial_keyword_at_end() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "le");
    assert_eq!(result, vec![("le".into(), true)]);
    let result = tok(&mut t, "tru");
    assert_eq!(result, vec![("tru".into(), true)]);
    let result = tok(&mut t, "th");
    assert_eq!(result, vec![("th".into(), true)]);
}

#[test]
fn fun_full_lambda_expression() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "λf:Int->Bool.f");
    assert_eq!(
        result,
        vec![
            ("λ".into(), false),
            ("f".into(), false),
            (":".into(), false),
            ("Int".into(), false),
            ("->".into(), false),
            ("Bool".into(), false),
            (".".into(), false),
            ("f".into(), true),
        ]
    );
}

#[test]
fn fun_full_let_expression() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "let x:Int=42 in x");
    assert_eq!(
        result,
        vec![
            ("let".into(), false),
            ("x".into(), false),
            (":".into(), false),
            ("Int".into(), false),
            ("=".into(), false),
            ("42".into(), false),
            ("in".into(), false),
            ("x".into(), false),
        ]
    );
}

#[test]
fn fun_full_if_expression() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "if true then 1 else 2");
    assert_eq!(
        result,
        vec![
            ("if".into(), false),
            ("true".into(), false),
            ("then".into(), false),
            ("1".into(), false),
            ("else".into(), false),
            ("2".into(), false),
        ]
    );
}

#[test]
fn fun_nested_lambda_with_arrow_types() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "λf:(Int->Int).λx:Int.f x");
    assert_eq!(
        result,
        vec![
            ("λ".into(), false),
            ("f".into(), false),
            (":".into(), false),
            ("(".into(), false),
            ("Int".into(), false),
            ("->".into(), false),
            ("Int".into(), false),
            (")".into(), false),
            (".".into(), false),
            ("λ".into(), false),
            ("x".into(), false),
            (":".into(), false),
            ("Int".into(), false),
            (".".into(), false),
            ("f".into(), false),
            ("x".into(), false),
        ]
    );
}

#[test]
fn fun_partial_lambda_prefix_int() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "λx:Int");
    assert_eq!(
        result,
        vec![
            ("λ".into(), false),
            ("x".into(), false),
            (":".into(), false),
            ("Int".into(), false),
        ]
    );
    let tokenizer_result = t.tokenize("λx:Int").unwrap();
    assert!(tokenizer_result.iter().all(|s| !s.is_partial_special));
}

#[test]
fn fun_identifier_is_keyword_prefix() {
    let mut t = fun_tokenizer();
    let result = tok(&mut t, "le");
    assert_eq!(result[0].0, "le");
    assert!(result[0].1);
}

#[test]
fn fun_number_token_not_split() {
    let mut t = fun_tokenizer();
    assert_eq!(tok(&mut t, "42"), vec![("42".into(), false)]);
    assert_eq!(tok(&mut t, "100"), vec![("100".into(), false)]);
}
