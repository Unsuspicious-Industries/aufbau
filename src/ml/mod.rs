// run ML inference
// 
// Environment variables:
// - HF_HOME: Sets the cache directory for Hugging Face models (default: ~/.cache/huggingface)
// - HUGGINGFACE_TOKEN: Required for accessing gated models like Mixtral
//
// Example usage:
// export HF_HOME="/path/to/your/cache"
// export HUGGINGFACE_TOKEN="your_token_here"
use anyhow::{Context, Error as E, Result};
use candle_nn::VarBuilder;
use hf_hub::{Repo, RepoType, api::sync::ApiBuilder};
use once_cell::sync::OnceCell;
use serde_json;
use std::any::Any;
use std::collections::HashMap;
use std::env;
use std::sync::{Arc, Mutex};
use tokenizers::Tokenizer;

use candle_core::{DType, Device, Tensor};
pub use candle_transformers::models::mixtral::{Config as MixtralConfig, Model as MixtralModel};
pub use candle_transformers::models::phi3::{Config as Phi3Config, Model as Phi3Model};

// Thread-safe model cache
static MODEL_CACHE: OnceCell<Arc<Mutex<HashMap<String, Box<dyn Any + Send + Sync>>>>> =
    OnceCell::new();

/// Enum to handle different model types
pub enum Model {
    Mixtral(MixtralModel),
    Phi3(Phi3Model),
}

impl Model {
    pub fn forward(&mut self, input: &Tensor, pos: usize) -> Result<Tensor> {
        match self {
            Model::Mixtral(model) => Ok(model.forward(input, pos)?),
            Model::Phi3(model) => Ok(model.forward(input, pos)?),
        }
    }

    pub fn clear_kv_cache(&mut self) {
        match self {
            Model::Mixtral(_model) => {
                // Mixtral doesn't have clear_kv_cache, but we can implement if needed
            },
            Model::Phi3(model) => model.clear_kv_cache(),
        }
    }
}

#[derive(Clone)]
pub struct LogitsProvider {
    pub model: Arc<Mutex<Model>>,
    pub tokenizer: Arc<Tokenizer>,
    pub device: Device,
}

impl LogitsProvider {
    pub fn get_logits(&self, token_ids: &[u32]) -> Result<Vec<f32>> {
        let input = Tensor::new(token_ids, &self.device)?.unsqueeze(0)?;
        let mut model = self.model.lock().unwrap();
        let logits = model.forward(&input, 0)?;
        
        // Handle different output shapes for different models
        let logits = match logits.dims().len() {
            3 => logits.squeeze(0)?.squeeze(0)?,
            2 => logits.squeeze(0)?,
            _ => logits,
        };
        
        let logits = logits.to_dtype(DType::F32)?;
        let logits_vec = logits.to_vec1::<f32>()?;
        Ok(logits_vec)
    }

    pub fn tokenize(&self, text: &str) -> Result<Vec<u32>> {
        let encoding = self.tokenizer.encode(text, true).map_err(E::msg)?;
        Ok(encoding.get_ids().to_vec())
    }
}

fn load_model(name: &str) -> Result<LogitsProvider> {
    let cache = MODEL_CACHE.get_or_init(|| Arc::new(Mutex::new(HashMap::new())));

    // Try to get from cache first
    {
        let cache_guard = cache.lock().unwrap();
        if let Some(provider) = cache_guard.get(name) {
            if let Some(provider) = provider.downcast_ref::<LogitsProvider>() {
                return Ok(provider.clone());
            }
        }
    }

    match name {
        "mixtral" => {
            let token = env::var("HUGGINGFACE_TOKEN")
                .context("HUGGINGFACE_TOKEN must be set to access Mixtral weights")?;
            let api = ApiBuilder::from_env().with_token(Some(token)).build()?;
            let repo = api.repo(Repo::with_revision(
                "mistralai/Mixtral-8x7B-v0.1".to_string(),
                RepoType::Model,
                "main".to_string(),
            ));
            let tokenizer_filename = repo.get("tokenizer.json")?;
            let filenames =
                candle_examples::hub_load_safetensors(&repo, "model.safetensors.index.json")?;
            let tokenizer = Tokenizer::from_file(tokenizer_filename).map_err(E::msg)?;
            let config = MixtralConfig::v0_1_8x7b(true);
            let device = candle_examples::device(false)?;
            let dtype = device.bf16_default_to_f32();
            let vb = unsafe { VarBuilder::from_mmaped_safetensors(&filenames, dtype, &device)? };
            let model = MixtralModel::new(&config, vb)?;

            let provider = LogitsProvider {
                model: Arc::new(Mutex::new(Model::Mixtral(model))),
                tokenizer: Arc::new(tokenizer),
                device,
            };

            // Store in cache
            {
                let mut cache_guard = cache.lock().unwrap();
                cache_guard.insert(name.to_string(), Box::new(provider.clone()));
            }

            Ok(provider)
        }
        "phi3" | "phi3-mini" => {
            let api = ApiBuilder::from_env().build()?;
            let repo = api.repo(Repo::with_revision(
                "microsoft/Phi-3-mini-4k-instruct".to_string(),
                RepoType::Model,
                "main".to_string(),
            ));
            
            let tokenizer_filename = repo.get("tokenizer.json")?;
            let filenames = candle_examples::hub_load_safetensors(&repo, "model.safetensors.index.json")?;
            let tokenizer = Tokenizer::from_file(tokenizer_filename).map_err(E::msg)?;
            
            let config_filename = repo.get("config.json")?;
            let config_str = std::fs::read_to_string(config_filename)?;
            let config: Phi3Config = serde_json::from_str(&config_str)?;
            
            let device = candle_examples::device(false)?;
            let dtype = device.bf16_default_to_f32();
            let vb = unsafe { VarBuilder::from_mmaped_safetensors(&filenames, dtype, &device)? };
            let model = Phi3Model::new(&config, vb)?;

            let provider = LogitsProvider {
                model: Arc::new(Mutex::new(Model::Phi3(model))),
                tokenizer: Arc::new(tokenizer),
                device,
            };

            // Store in cache
            {
                let mut cache_guard = cache.lock().unwrap();
                cache_guard.insert(name.to_string(), Box::new(provider.clone()));
            }

            Ok(provider)
        }
        "phi3-medium" => {
            let api = ApiBuilder::from_env().build()?;
            let repo = api.repo(Repo::with_revision(
                "microsoft/Phi-3-medium-4k-instruct".to_string(),
                RepoType::Model,
                "main".to_string(),
            ));
            
            let tokenizer_filename = repo.get("tokenizer.json")?;
            let filenames = candle_examples::hub_load_safetensors(&repo, "model.safetensors.index.json")?;
            let tokenizer = Tokenizer::from_file(tokenizer_filename).map_err(E::msg)?;
            
            let config_filename = repo.get("config.json")?;
            let config_str = std::fs::read_to_string(config_filename)?;
            let config: Phi3Config = serde_json::from_str(&config_str)?;
            
            let device = candle_examples::device(false)?;
            let dtype = device.bf16_default_to_f32();
            let vb = unsafe { VarBuilder::from_mmaped_safetensors(&filenames, dtype, &device)? };
            let model = Phi3Model::new(&config, vb)?;

            let provider = LogitsProvider {
                model: Arc::new(Mutex::new(Model::Phi3(model))),
                tokenizer: Arc::new(tokenizer),
                device,
            };

            // Store in cache
            {
                let mut cache_guard = cache.lock().unwrap();
                cache_guard.insert(name.to_string(), Box::new(provider.clone()));
            }

            Ok(provider)
        }
        _ => Err(E::msg(format!("Unknown model: {}. Supported models: mixtral, phi3, phi3-mini, phi3-medium", name))),
    }
}

pub fn get_logits_provider(model_name: &str) -> Result<LogitsProvider> {
    load_model(model_name)
}

pub fn get_logits(model_name: &str, input: &str) -> Result<Vec<f32>> {
    let provider = get_logits_provider(model_name)?;
    let token_ids = provider.tokenize(input)?;
    provider.get_logits(&token_ids)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_get_logits_mixtral() -> Result<()> {
        let logits = get_logits("mixtral", "Hello, world!")?;
        println!("Mixtral logits length: {}", logits.len());
        assert!(!logits.is_empty());
        Ok(())
    }

    #[test]
    fn test_get_logits_phi3() -> Result<()> {
        let logits = get_logits("phi3", "Hello, world!")?;
        println!("Phi-3 logits length: {}", logits.len());
        assert!(!logits.is_empty());
        Ok(())
    }

    #[test]
    fn test_phi3_tokenization() -> Result<()> {
        let provider = get_logits_provider("phi3")?;
        let tokens = provider.tokenize("The quick brown fox")?;
        println!("Phi-3 tokens: {:?}", tokens);
        assert!(!tokens.is_empty());
        Ok(())
    }

    #[test]
    fn test_model_caching() -> Result<()> {
        // Load the same model twice - should use cache on second call
        let provider1 = get_logits_provider("phi3")?;
        let provider2 = get_logits_provider("phi3")?;
        
        // Both should have the same tokenizer (Arc should be the same)
        assert!(Arc::ptr_eq(&provider1.tokenizer, &provider2.tokenizer));
        Ok(())
    }
}
