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
    #[pyo3(signature = (spec_source, input = ""))]
    fn new(spec_source: String, input: &str) -> PyResult<Self> {
        let grammar = Grammar::load(&spec_source)
            .map_err(|e| PyValueError::new_err(format!("failed to load grammar: {}", e)))?;
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
        match self.synth.allowed_tokens_with(&self.ctx) {
            Ok(tokens) => tokens.iter().map(|t| t.to_pattern()).collect(),
            Err(_) => Vec::new(),
        }
    }

    fn token_examples(&mut self) -> Vec<String> {
        match self.synth.allowed_tokens_with(&self.ctx) {
            Ok(tokens) => tokens.iter().filter_map(|t| t.example()).collect(),
            Err(_) => Vec::new(),
        }
    }
    // state altering
    fn feed(&mut self, token: &str) -> PyResult<String> {
        self.synth
            .feed_with(token, &self.ctx)
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }
    // not state altering
    fn try_feed(&mut self, token: &str) -> PyResult<String> {
        self.synth
            .try_feed(token)
            .map(|ast| ast.to_string())
            .map_err(PyRuntimeError::new_err)
    }

    // context utils

    fn add_to_ctx(&mut self, name: &str, ty: &str) -> PyResult<()> {
        let ty = Type::parse_raw(ty)
            .map_err(|e| PyValueError::new_err(format!("invalid type '{}': {}", ty, e)))?;
        self.ctx.add(name.to_string(), ty);
        Ok(())
    }

    fn clear_ctx(&mut self) {
        self.ctx = Context::new();
    }

    // getting an AST string representaton
    fn ast_str(&mut self) -> Option<String> {
        self.synth.ast().ok().map(|a| a.to_string())
    }

    fn is_complete(&mut self) -> bool {
        match self.synth.parse_with(&self.ctx) {
            Ok(ast) => ast.is_complete(),
            Err(_) => false,
        }
    }
}

#[pyclass(unsendable, name = "Regex")]
#[derive(Clone)]
pub struct PyRegex {
    regex: Regex,
}

#[pymethods]
impl PyRegex {
    #[new]
    fn new(pattern: &str) -> PyResult<Self> {
        let regex = Regex::from_str(pattern)
            .map_err(|e| PyValueError::new_err(format!("invalid regex: {}", e)))?;
        Ok(Self { regex })
    }

    fn __repr__(&self) -> String {
        format!("Regex({})", self.regex.to_pattern())
    }

    fn __str__(&self) -> String {
        self.regex.to_pattern()
    }

    fn matches(&self, text: &str) -> bool {
        self.regex.matches(text)
    }

    fn prefix_match(&self, prefix: &str) -> PyPrefixStatus {
        PyPrefixStatus::from(self.regex.prefix_match(prefix))
    }

    fn derivative(&self, text: &str) -> Self {
        Self {
            regex: self.regex.derivative(text),
        }
    }

    fn deriv(&self, character: &str) -> PyResult<Self> {
        let mut chars = character.chars();
        let c = chars
            .next()
            .ok_or_else(|| PyValueError::new_err("character must be a non-empty string"))?;
        if chars.next().is_some() {
            return Err(PyValueError::new_err(
                "character must be a single Unicode character",
            ));
        }
        Ok(Self {
            regex: self.regex.deriv(c),
        })
    }

    fn is_empty(&self) -> bool {
        self.regex.is_empty()
    }

    fn is_nullable(&self) -> bool {
        self.regex.is_nullable()
    }

    fn match_len(&self, text: &str) -> Option<usize> {
        self.regex.match_len(text)
    }

    fn to_pattern(&self) -> String {
        self.regex.to_pattern()
    }
}

#[pyclass(unsendable, name = "PrefixStatus")]
#[derive(Clone)]
pub struct PyPrefixStatus {
    kind: String,
    regex: Option<PyRegex>,
}

#[pymethods]
impl PyPrefixStatus {
    #[getter]
    fn kind(&self) -> &str {
        &self.kind
    }

    #[getter]
    fn regex(&self) -> Option<PyRegex> {
        self.regex.clone()
    }

    fn __repr__(&self) -> String {
        match &self.regex {
            Some(regex) => format!("PrefixStatus.{}({})", self.kind, regex.to_pattern()),
            None => format!("PrefixStatus.{}", self.kind),
        }
    }

    fn is_complete(&self) -> bool {
        matches!(self.kind.as_str(), "complete" | "extensible")
    }

    fn is_prefix(&self) -> bool {
        self.kind == "prefix"
    }

    fn is_extensible(&self) -> bool {
        self.kind == "extensible"
    }

    fn is_no_match(&self) -> bool {
        self.kind == "no_match"
    }
}

impl From<PrefixStatus> for PyPrefixStatus {
    fn from(status: PrefixStatus) -> Self {
        match status {
            PrefixStatus::Extensible(regex) => Self {
                kind: "extensible".to_string(),
                regex: Some(PyRegex { regex }),
            },
            PrefixStatus::Complete => Self {
                kind: "complete".to_string(),
                regex: None,
            },
            PrefixStatus::Prefix(regex) => Self {
                kind: "prefix".to_string(),
                regex: Some(PyRegex { regex }),
            },
            PrefixStatus::NoMatch => Self {
                kind: "no_match".to_string(),
                regex: None,
            },
        }
    }
}

#[pyfunction]
fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[pymodule]
fn aufbau(_py: Python<'_>, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PySynthesizer>()?;
    m.add_class::<PyRegex>()?;
    m.add_class::<PyPrefixStatus>()?;
    m.add_function(wrap_pyfunction!(version, m)?)?;
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
    fn python_synth_exported_as_module_class() {
        pyo3::prepare_freethreaded_python();
        Python::with_gil(|py| {
            let module = PyModule::new(py, "aufbau").unwrap();
            super::aufbau(py, &module).unwrap();

            let synth_class = module.getattr("Synthesizer").unwrap();
            let instance = synth_class.call1((SPEC, "", None::<usize>)).unwrap();

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
}
