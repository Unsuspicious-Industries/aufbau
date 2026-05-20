use super::python::*;
use pyo3::prelude::*;

const SPEC: &str = "start ::= 'x' 'y'";
const TYPED_SPEC: &str = r#"
        Identifier ::= /[a-z]+/
        Variable(var) ::= Identifier[x]
        Expr ::= Variable

        x ∈ Γ
        ----------- (var)
        Γ(x)
    "#;

#[test]
fn grammar_load_and_inspect() {
    let g = PyGrammar::new(TYPED_SPEC).unwrap();
    assert_eq!(g.start(), Some("Expr"));
    assert_eq!(g.nonterminals().len(), 3);
    assert!(!g.nonterminals().is_empty());
    // "var" rule should be present
    assert!(g.rule_names().contains(&"var".to_string()));
}

#[test]
fn grammar_productions_inspect() {
    let g = PyGrammar::new(TYPED_SPEC).unwrap();
    let prods = g.productions("Variable").unwrap();
    assert_eq!(prods.len(), 1);
    let rhs = prods[0].rhs();
    assert_eq!(rhs.len(), 1);
    assert_eq!(rhs[0].kind(), "terminal");
    assert_eq!(rhs[0].binding(), Some("x"));
}

#[test]
fn grammar_tokenize() {
    let g = PyGrammar::new(TYPED_SPEC).unwrap();
    let segs = g.tokenize("hello world").unwrap();
    assert_eq!(segs.len(), 2);
    assert_eq!(segs[0].text(), "hello");
    assert_eq!(segs[1].text(), "world");
}

#[test]
fn synthesis_grammar_access() {
    let mut s = PySynthesizer::new(TYPED_SPEC.to_string(), "x").unwrap();
    let g = s.grammar();
    assert_eq!(g.start(), Some("Expr"));
    // Should be able to get a rule
    let rule = s.get_rule("var");
    assert!(rule.is_some());
    assert_eq!(rule.unwrap().name(), "var");
}

#[test]
fn python_synth_tokens_and_feed() {
    let mut s = PySynthesizer::new(SPEC.to_string(), "").unwrap();
    s.feed("x").unwrap();
    assert_eq!(s.input(), "x");
}

#[test]
fn python_synth_set_input_and_complete() {
    let mut s = PySynthesizer::new(SPEC.to_string(), "").unwrap();
    s.set_input("x y").unwrap();
    assert!(s.is_complete());
}

#[test]
fn python_synth_exported_as_module_class() {
    pyo3::prepare_freethreaded_python();
    Python::with_gil(|py| {
        let module = PyModule::new(py, "aufbau").unwrap();
        super::python::aufbau(py, &module).unwrap();

        let synth_class = module.getattr("Synthesizer").unwrap();
        let instance = synth_class.call1((SPEC, "")).unwrap();

        let input = instance.call_method0("input").unwrap();
        assert_eq!(input.extract::<String>().unwrap(), "");
    });
}

#[test]
fn python_regex_helpers_work() {
    let regex = PyRegex::new("a*b").unwrap();
    assert!(regex.matches("ab"));

    let prefix_status = regex.prefix_match("a");
    assert!(prefix_status.is_prefix());
    assert!(!prefix_status.is_no_match());

    let derived = regex.derivative("ab");
    assert!(derived.is_nullable());
}
