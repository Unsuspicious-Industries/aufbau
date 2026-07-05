//! Regex and prefix-status objects: the leaf layer of the constraint model,
//! exposed for token-level experiments.

use pyo3::exceptions::PyValueError;
use pyo3::prelude::*;

use crate::regex::{PrefixStatus, Regex};

// ═══════════════════════════════════════════════════════════════════════════════
// PyRegex
// ═══════════════════════════════════════════════════════════════════════════════

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
            .map_err(|e| PyValueError::new_err(format!("invalid regex: {e}")))?;
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

// ═══════════════════════════════════════════════════════════════════════════════
// PyPrefixStatus
// ═══════════════════════════════════════════════════════════════════════════════

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
