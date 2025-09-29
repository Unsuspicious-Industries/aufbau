use pyo3::prelude::*;
use pyo3::types::{PyList, PyModule};
use numpy::{PyArray1, PyArrayMethods};

pub struct LLMWrapper {
    py_module: Py<PyModule>,
    model_id: String,
}

impl LLMWrapper {
    pub fn new(script_path: &str, model_name: &str) -> PyResult<Self> {
        // Minimal: ensure Python can import our module and call load_model(model_name)
        Python::attach(|py| {
            let sys = py.import("sys")?;
            // sys.path is a list; push our script directory to the front
            let path_any = sys.getattr("path")?;
            let path: &pyo3::Bound<PyList> = path_any.downcast()?;
            path.insert(0, script_path)?;

            let py_module = PyModule::import(py, "main")?;
            let model_id_obj = py_module.getattr("load_model")?.call1((model_name,))?;
            let model_id: String = model_id_obj.extract()?;

            Ok(LLMWrapper {
                py_module: py_module.into(),
                model_id,
            })
        })
    }

    pub fn get_logits(&self, prompt: &str) -> PyResult<Vec<f32>> {
        Python::attach(|py| {
            let py_module = self.py_module.bind(py);
            let any = py_module.getattr("get_logits")?.call1((&self.model_id, prompt))?;
            let np: &pyo3::Bound<PyArray1<f32>> = any.downcast()?;
            let readonly = np.readonly();
            let slice = readonly.as_slice()?;
            Ok(slice.to_vec())
        })
    }
}