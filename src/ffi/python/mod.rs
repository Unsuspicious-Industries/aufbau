#[cfg(feature = "python-ffi")]
pub mod grammar;
#[cfg(feature = "python-ffi")]
pub mod parse;
#[cfg(feature = "python-ffi")]
pub mod regex;
#[cfg(feature = "python-ffi")]
pub mod typing;

use pyo3::Bound;
use pyo3::prelude::*;

use self::grammar::{PyGrammar, PyProduction, PySegment, PySymbol};
use self::parse::{PyAst, PyChild, PyNode};
use self::regex::{PyPrefixStatus, PyRegex};
use self::typing::{PySynthesizer, PyTerm, PyTypingRule, PyVerification};

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
    m.add_class::<PyVerification>()?;
    m.add_class::<PyTerm>()?;
    m.add_class::<PyTypingRule>()?;
    m.add_class::<PyRegex>()?;
    m.add_class::<PyPrefixStatus>()?;

    // Module identity. `ENGINE_API` names the whole v1 contract, so a consumer
    // checks one string at startup instead of probing for methods.
    m.add("__version__", env!("CARGO_PKG_VERSION"))?;
    m.add("ENGINE_API", "aufbau.engine/v1")?;
    Ok(())
}
