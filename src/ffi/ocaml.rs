use crate::logic::grammar::Grammar;
use crate::validation::completability;

fn parse_grammar_impl(spec_source: &str) -> Result<(), String> {
    Grammar::load(spec_source)
        .map(|_| ())
        .map_err(|e| format!("Grammar parse error: {}", e))
}

fn complete_k_impl(spec_source: &str, prefix: &str, depth: isize, count: isize) -> Vec<String> {
    let grammar = match Grammar::load(spec_source) {
        Ok(g) => g,
        Err(_) => return vec![],
    };
    let depth = depth.max(1) as usize;
    let count = count.max(1) as usize;
    completability::complete_k(&grammar, prefix, depth, count, None)
}

fn complete_impl(spec_source: &str, prefix: &str, depth: isize) -> Option<String> {
    complete_k_impl(spec_source, prefix, depth, 1)
        .into_iter()
        .next()
}

fn check_prefix_impl(spec_source: &str, prefix: &str) -> bool {
    let grammar = match Grammar::load(spec_source) {
        Ok(g) => g,
        Err(_) => return false,
    };

    grammar.tokenize(prefix).is_ok()
}

fn version_impl() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[ocaml::func]
#[cfg_attr(feature = "ocaml-ffi", ocaml::sig("string -> (unit, string) result"))]
pub fn aufbau_parse_grammar(spec_source: String) -> Result<(), String> {
    parse_grammar_impl(&spec_source)
}

#[ocaml::func]
#[cfg_attr(
    feature = "ocaml-ffi",
    ocaml::sig("string -> string -> int -> int -> string array")
)]
pub fn aufbau_complete_k(
    spec_source: String,
    prefix: String,
    depth: ocaml::Int,
    count: ocaml::Int,
) -> Vec<String> {
    complete_k_impl(&spec_source, &prefix, depth, count)
}

#[ocaml::func]
#[cfg_attr(
    feature = "ocaml-ffi",
    ocaml::sig("string -> string -> int -> string option")
)]
pub fn aufbau_complete(spec_source: String, prefix: String, depth: ocaml::Int) -> Option<String> {
    complete_impl(&spec_source, &prefix, depth)
}

#[ocaml::func]
#[cfg_attr(feature = "ocaml-ffi", ocaml::sig("string -> string -> bool"))]
pub fn aufbau_check_prefix(spec_source: String, prefix: String) -> bool {
    check_prefix_impl(&spec_source, &prefix)
}

#[ocaml::func]
#[cfg_attr(feature = "ocaml-ffi", ocaml::sig("unit -> string"))]
pub fn aufbau_version() -> String {
    version_impl()
}

#[ocaml::func]
#[cfg_attr(
    feature = "ocaml-ffi",
    ocaml::sig("(string -> string) -> string -> (string, string) result")
)]
pub fn aufbau_call_ocaml_callback(callback: ocaml::Value, input: String) -> Result<String, String> {
    let callback = ocaml::function!(callback, (arg: String) -> String);
    callback(gc, &input).map_err(|err| format!("OCaml callback failed: {err:?}"))
}

#[cfg(test)]
mod tests {
    use super::*;

    const STLC_SPEC: &str = include_str!("../../examples/stlc.auf");

    #[test]
    fn ffi_parse_grammar_accepts_valid_spec() {
        let parsed = parse_grammar_impl(STLC_SPEC);
        assert!(parsed.is_ok());
    }

    #[test]
    fn ffi_parse_grammar_rejects_invalid_spec() {
        let parsed = parse_grammar_impl("this is not a grammar");
        assert!(parsed.is_err());
    }

    #[test]
    fn ffi_complete_k_returns_bounded_results() {
        let results = complete_k_impl(STLC_SPEC, "let", 10, 3);
        assert!(results.len() <= 3);
    }

    #[test]
    fn ffi_complete_returns_none_for_invalid_spec() {
        let result = complete_impl("not a grammar", "let", 8);
        assert!(result.is_none());
    }

    #[test]
    fn ffi_check_prefix_handles_valid_and_invalid_specs() {
        let ok = check_prefix_impl(STLC_SPEC, "let");
        let bad = check_prefix_impl("not a grammar", "let");
        assert!(ok);
        assert!(!bad);
    }

    #[test]
    fn ffi_version_is_non_empty() {
        let v = version_impl();
        assert!(!v.trim().is_empty());
    }
}
