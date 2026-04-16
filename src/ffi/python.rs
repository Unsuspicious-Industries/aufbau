use crate::logic::grammar::Grammar;
use crate::logic::synth::Synthesizer;
use crate::logic::typing::{Context, Type};
use crate::regex::{PrefixStatus, Regex};
use pyo3::exceptions::{PyRuntimeError, PyValueError};
use pyo3::prelude::*;

#[pyclass(unsendable, name = "Synthesizer")]
pub struct PySynthesizer {
    spec_source: String,
    synth: Synthesizer,
    ctx: Context,
}

#[pymethods]
impl PySynthesizer {
    #[new]
    #[pyo3(signature = (spec_source, input = "", max_depth = None))]
    fn new(spec_source: String, input: &str, max_depth: Option<usize>) -> PyResult<Self> {
        let grammar = Grammar::load(&spec_source)
            .map_err(|e| PyValueError::new_err(format!("failed to load grammar: {}", e)))?;
        let _ = max_depth;
        let synth = Synthesizer::new(grammar, input);
        Ok(Self {
            spec_source,
            synth,
            ctx: Context::new(),
        })
    }

    fn set_input(&mut self, input: &str) -> PyResult<()> {
        let grammar = Grammar::load(&self.spec_source)
            .map_err(|e| PyValueError::new_err(format!("failed to load grammar: {}", e)))?;
        self.synth = Synthesizer::new(grammar, input);
        Ok(())
    }

    fn input(&self) -> String {
        self.synth.input().to_string()
    }

    fn parse(&mut self) -> PyResult<String> {
        self.synth
            .parse_with(&self.ctx)
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }

    fn tokens(&mut self) -> Vec<String> {
        self.synth
            .completions_with(&self.ctx)
            .iter()
            .map(|t| t.to_pattern())
            .collect()
    }

    fn token_examples(&mut self) -> Vec<String> {
        self.synth
            .completions_with(&self.ctx)
            .iter()
            .filter_map(|t| t.example())
            .collect()
    }

    fn feed(&mut self, token: &str) -> PyResult<String> {
        self.synth
            .feed(token, &self.ctx)
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }

    fn add_binding(&mut self, name: &str, ty: &str) -> PyResult<()> {
        let ty = Type::parse_raw(ty)
            .map_err(|e| PyValueError::new_err(format!("invalid type '{}': {}", ty, e)))?;
        self.ctx.add(name.to_string(), ty);
        Ok(())
    }

    fn clear_bindings(&mut self) {
        self.ctx = Context::new();
    }

    fn ast(&self) -> Option<String> {
        self.synth.ast().map(|a| a.to_string())
    }

    fn is_complete(&mut self) -> bool {
        match self.synth.parse_with(&self.ctx) {
            Ok(ast) => ast.is_complete(),
            Err(_) => false,
        }
    }
}

#[pyfunction]
fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[pyfunction]
fn regex_matches(pattern: &str, text: &str) -> PyResult<bool> {
    let regex = Regex::from_str(pattern)
        .map_err(|e| PyValueError::new_err(format!("invalid regex: {}", e)))?;
    Ok(regex.matches(text))
}

#[pyfunction]
fn regex_prefix_valid(pattern: &str, prefix: &str) -> PyResult<bool> {
    let regex = Regex::from_str(pattern)
        .map_err(|e| PyValueError::new_err(format!("invalid regex: {}", e)))?;
    Ok(!matches!(regex.prefix_match(prefix), PrefixStatus::NoMatch))
}

#[pymodule]
fn aufbau_python(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PySynthesizer>()?;
    m.add_function(wrap_pyfunction!(version, m)?)?;
    m.add_function(wrap_pyfunction!(regex_matches, m)?)?;
    m.add_function(wrap_pyfunction!(regex_prefix_valid, m)?)?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    const SPEC: &str = "start ::= 'x' 'y'";

    #[test]
    fn python_synth_tokens_and_feed() {
        let mut s = PySynthesizer::new(SPEC.to_string(), "", Some(8)).unwrap();
        s.feed("x").unwrap();
        assert_eq!(s.input(), "x");
    }

    #[test]
    fn python_synth_set_input_and_complete() {
        let mut s = PySynthesizer::new(SPEC.to_string(), "", Some(8)).unwrap();
        s.set_input("x y").unwrap();
        assert!(s.is_complete());
    }

    #[test]
    fn python_regex_helpers_work() {
        assert!(regex_matches("x", "x").unwrap());
        assert!(regex_prefix_valid("xy", "x").unwrap());
    }
}
