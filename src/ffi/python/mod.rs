#[cfg(feature = "python-ffi")]
pub mod grammar;
#[cfg(feature = "python-ffi")]
pub mod parse;
#[cfg(feature = "python-ffi")]
pub mod tests;
#[cfg(feature = "python-ffi")]
pub mod typing;

use pyo3::prelude::*;
use pyo3::Bound;

use self::grammar::{PyGrammar, PyProduction, PySegment, PySymbol};
use self::parse::{PyAst, PyChild, PyNode};
use self::typing::{PyPrefixStatus, PyRegex, PySynthesizer, PyTypingRule};

#[pymodule]
fn aufbau(m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_class::<PyGrammar>()?;
    m.add_class::<PyProduction>()?;
    m.add_class::<PySymbol>()?;
    m.add_class::<PySegment>()?;
    m.add_class::<PyAst>()?;
    m.add_class::<PyNode>()?;
    m.add_class::<PyChild>()?;
    m.add_class::<PySynthesizer>()?;
    m.add_class::<PyTypingRule>()?;
    m.add_class::<PyRegex>()?;
    m.add_class::<PyPrefixStatus>()?;
    Ok(())
}
