import numpy as np

# Try to import ML libraries, but don't fail if they're not available
try:
    import torch
    from transformers import AutoModelForCausalLM, AutoTokenizer
    ML_AVAILABLE = True
except ImportError:
    print("Warning: ML libraries not available. Using mock implementation.")
    ML_AVAILABLE = False

# Global model cache
_model_cache = {}

def load_model(model_name: str):
    """Load and cache an LLM model"""
    if not ML_AVAILABLE:
        print(f"Mock loading model: {model_name}")
        return model_name
    
    if model_name not in _model_cache:
        try:
            tokenizer = AutoTokenizer.from_pretrained(model_name)
            model = AutoModelForCausalLM.from_pretrained(
                model_name,
                torch_dtype=torch.float16,
                device_map="auto"
            )
            _model_cache[model_name] = (model, tokenizer)
        except Exception as e:
            print(f"Error loading model {model_name}: {e}")
            print("Falling back to mock implementation")
            return model_name
    
    return model_name  # Return identifier, not the object itself

def get_logits(model_id: str, prompt: str) -> np.ndarray:
    """Get logits for a prompt (non-streaming)"""
    if not ML_AVAILABLE or model_id not in _model_cache:
        # Return mock logits
        vocab_size = 50257  # GPT-2 vocab size
        return np.random.randn(vocab_size).astype(np.float32)
    
    model, tokenizer = _model_cache[model_id]
    
    inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
    
    with torch.no_grad():
        outputs = model(**inputs)
        logits = outputs.logits[0, -1, :]  # Last token logits
    
    return logits.cpu().numpy().astype(np.float32)

def stream_logits(model_id: str, prompt: str):
    """Stream tokens and their logits in real-time"""
    if not ML_AVAILABLE or model_id not in _model_cache:
        # Return mock streaming
        mock_tokens = ["Hello", " world", "!", " This", " is", " a", " test", "."]
        vocab_size = 50257
        for token in mock_tokens:
            logits = np.random.randn(vocab_size).astype(np.float32)
            yield (token, logits)
        return
    
    model, tokenizer = _model_cache[model_id]
    
    inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
    input_ids = inputs.input_ids
    
    max_length = 100
    
    for _ in range(max_length):
        with torch.no_grad():
            outputs = model(input_ids)
            logits = outputs.logits[0, -1, :]  # Shape: [vocab_size]
            
            # Get next token
            next_token_id = torch.argmax(logits, dim=-1, keepdim=True)
            
            # Decode token
            token = tokenizer.decode(next_token_id[0])
            
            # Yield token and its logits as tuple
            yield (token, logits.cpu().numpy().astype(np.float32))
            
            # Stop on EOS
            if next_token_id.item() == tokenizer.eos_token_id:
                break
            
            # Append for next iteration
            input_ids = torch.cat([input_ids, next_token_id.unsqueeze(0)], dim=-1)

def get_logits_batch(model_id: str, prompts: list[str]) -> list[np.ndarray]:
    """Batch process multiple prompts for efficiency"""
    if not ML_AVAILABLE or model_id not in _model_cache:
        # Return mock batch logits
        vocab_size = 50257
        return [np.random.randn(vocab_size).astype(np.float32) for _ in prompts]
    
    model, tokenizer = _model_cache[model_id]
    
    inputs = tokenizer(prompts, return_tensors="pt", padding=True).to(model.device)
    
    with torch.no_grad():
        outputs = model(**inputs)
        # Get last token logits for each sequence
        logits_list = []
        for i in range(len(prompts)):
            seq_len = inputs.attention_mask[i].sum().item()
            logits = outputs.logits[i, seq_len-1, :]
            logits_list.append(logits.cpu().numpy().astype(np.float32))
    
    return logits_list

def get_top_k_tokens(model_id: str, prompt: str, k: int = 10):
    """Get top-k tokens and their probabilities"""
    if not ML_AVAILABLE or model_id not in _model_cache:
        # Return mock top-k tokens
        mock_tokens = ["the", "a", "an", "and", "or", "but", "in", "on", "at", "to"]
        results = []
        for i in range(min(k, len(mock_tokens))):
            token = mock_tokens[i]
            prob = 1.0 / (i + 1)  # Decreasing probabilities
            idx = i
            results.append((token, prob, idx))
        return results
    
    model, tokenizer = _model_cache[model_id]
    
    inputs = tokenizer(prompt, return_tensors="pt").to(model.device)
    
    with torch.no_grad():
        outputs = model(**inputs)
        logits = outputs.logits[0, -1, :]
        probs = torch.softmax(logits, dim=-1)
        
        top_k_probs, top_k_indices = torch.topk(probs, k)
        
        results = []
        for prob, idx in zip(top_k_probs, top_k_indices):
            token = tokenizer.decode([idx.item()])
            results.append((token, prob.item(), idx.item()))
    
    return results