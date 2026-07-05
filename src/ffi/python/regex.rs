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

    /// Whether `text` is a complete match.
    fn matches(&self, text: &str) -> bool {
        self.regex.matches(text)
    }

    /// Classify `prefix` as a match, a partial prefix, or no match.
    fn prefix_match(&self, prefix: &str) -> PyPrefixStatus {
        PyPrefixStatus::from(self.regex.prefix_match(prefix))
    }

    /// The regex remaining after consuming `text`.
    fn derivative(&self, text: &str) -> Self {
        Self {
            regex: self.regex.derivative(text),
        }
    }

    /// The regex remaining after consuming one character.
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

    /// Whether this regex matches nothing at all.
    fn is_empty(&self) -> bool {
        self.regex.is_empty()
    }

    /// Whether the empty string is a match.
    fn is_nullable(&self) -> bool {
        self.regex.is_nullable()
    }

    /// Length of the longest prefix of `text` that is a complete match.
    fn match_len(&self, text: &str) -> Option<usize> {
        self.regex.match_len(text)
    }

    /// Render back to source pattern syntax.
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

    /// Whether the prefix is itself a complete or extensible match.
    fn is_complete(&self) -> bool {
        matches!(self.kind.as_str(), "complete" | "extensible")
    }

    /// Whether the prefix could still be completed, but isn't a match yet.
    fn is_prefix(&self) -> bool {
        self.kind == "prefix"
    }

    /// Whether the prefix is a complete match that could also be extended.
    fn is_extensible(&self) -> bool {
        self.kind == "extensible"
    }

    /// Whether the prefix cannot lead to any match.
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
