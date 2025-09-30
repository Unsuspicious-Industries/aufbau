// run ML inference
use candle_nn::VarBuilder;
use hf_hub::{Repo, api::sync::Api, RepoType,api::sync::ApiBuilder};
use once_cell::sync::OnceCell;
use tokenizers::Tokenizer;
use std::any::Any;
use std::collections::HashMap;
use std::env;
use std::sync::{Arc, Mutex};
use anyhow::{Error as E, Result};

use candle_core::{Device, Tensor, DType};
pub use candle_transformers::models::mixtral::{Config as MixtralConfig, Model as MixtralModel};

// Thread-safe model cache
static MODEL_CACHE: OnceCell<Arc<Mutex<HashMap<String, Box<dyn Any + Send + Sync>>>>> = OnceCell::new();

#[derive(Clone)]
pub struct LogitsProvider {
    pub model: Arc<Mutex<MixtralModel>>,
    pub tokenizer: Arc<Tokenizer>,
    pub device: Device,
}

impl LogitsProvider {
    pub fn get_logits(&self, token_ids: &[u32]) -> Result<Vec<f32>> {
        let input = Tensor::new(token_ids, &self.device)?.unsqueeze(0)?;
        let mut model = self.model.lock().unwrap();
        let logits = model.forward(&input, 0)?;
        let logits = logits.squeeze(0)?.squeeze(0)?.to_dtype(DType::F32)?;
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
            let api = ApiBuilder::new().with_token(env::var("HUGGINGFACE_TOKEN").ok()).build()?;
            let repo = api.repo(Repo::with_revision(
                "mistralai/Mixtral-8x7B-v0.1".to_string(),
                RepoType::Model,
                "main".to_string(),
            ));
            let tokenizer_filename = repo.get("tokenizer.json")?;
            let filenames = candle_examples::hub_load_safetensors(&repo, "model.safetensors.index.json")?;
            let tokenizer = Tokenizer::from_file(tokenizer_filename).map_err(E::msg)?;
            let config = MixtralConfig::v0_1_8x7b(true);
            let device = candle_examples::device(false)?;
            let dtype = device.bf16_default_to_f32();
            let vb = unsafe { VarBuilder::from_mmaped_safetensors(&filenames, dtype, &device)? };
            let model = MixtralModel::new(&config, vb)?;

            let provider = LogitsProvider {
                model: Arc::new(Mutex::new(model)),
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
        _ => Err(E::msg(format!("Unknown model: {}", name))),
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

#[test]
fn test_get_logits() -> Result<()> {
    let logits = get_logits("mixtral", "Hello, world!")?;
    println!("Logits length: {}", logits.len());
    Ok(())
}