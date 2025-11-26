"""LLM intergration for typed constrained generation."""

from __future__ import annotations

from typing import List, Optional, Callable

from .inference import GenerationResult, generate, until_complete


class ConstrainedModel:
    """HuggingFace model wrapped with typed constrainted decoding."""
    
    def __init__(
        self,
        model,
        tokenizer,
        grammar: str,
        device: str = "cpu",
    ):
        self.model = model
        self.tokenizer = tokenizer
        self.grammar = grammar
        self.device = device
        self.vocab = [tokenizer.decode([i]) for i in range(tokenizer.vocab_size)]
        self._input_ids = None
    
    @classmethod
    def from_pretrained(
        cls,
        model_name: str,
        grammar: str,
        device: str = "cpu",
        **model_kwargs,
    ) -> "ConstrainedModel":
        try:
            import torch
            from transformers import AutoModelForCausalLM, AutoTokenizer
        except ImportError:
            raise ImportError(
                "transformers and torch required. "
                "pip install transformers torch"
            )
        
        # Pass trust_remote_code to tokenizer if specified
        tokenizer_kwargs = {}
        if "trust_remote_code" in model_kwargs:
            tokenizer_kwargs["trust_remote_code"] = model_kwargs["trust_remote_code"]
        
        tokenizer = AutoTokenizer.from_pretrained(model_name, **tokenizer_kwargs)
        model = AutoModelForCausalLM.from_pretrained(model_name, **model_kwargs)
        
        # Only move to device if not using device_map
        if "device_map" not in model_kwargs:
            model.to(device)
        else:
            # When using device_map, get actual device from model
            device = str(next(model.parameters()).device)
        
        model.eval()
        
        return cls(model, tokenizer, grammar, device)
    
    def _get_logits(self) -> List[float]:
        import torch
        
        with torch.no_grad():
            outputs = self.model(self._input_ids.to(self.device), use_cache=False)
            # Convert to float32 before tolist() for float16 models
            logits = outputs.logits[0, -1, :].float().cpu().tolist()
        return logits
    
    def _update_input_ids(self, token: str):
        import torch
        
        token_ids = self.tokenizer.encode(token, add_special_tokens=False)
        if token_ids:
            new_ids = torch.tensor([token_ids])
            self._input_ids = torch.cat([self._input_ids, new_ids], dim=1)
    
    def generate(
        self,
        initial: str = "",
        prompt: str = "",
        max_tokens: int = 50,
        greedy_k: int = 1,
        pre_top_k: Optional[int] = 100,
        on_token: Optional[Callable[[str, int], None]] = None,
    ) -> GenerationResult:
        import torch
        
        full_prompt = prompt + initial
        self._input_ids = self.tokenizer.encode(full_prompt, return_tensors="pt")
        
        generated_tokens = []
        
        def logit_fn():
            return self._get_logits()
        
        def on_token_wrapper(token: str, step: int):
            self._update_input_ids(token)
            generated_tokens.append(token)
            if on_token:
                on_token(token, step)
        
        result, sampler = generate(
            grammar=self.grammar,
            vocab=self.vocab,
            logit_fn=logit_fn,
            initial=initial,
            max_tokens=max_tokens,
            greedy_k=greedy_k,
            pre_top_k=pre_top_k,
            on_token=on_token_wrapper,
        )
        
        return result
    
    def until_complete(
        self,
        initial: str = "",
        prompt: str = "",
        max_tokens: int = 100,
        greedy_k: int = 1,
        pre_top_k: Optional[int] = 100,
        on_token: Optional[Callable[[str, int], None]] = None,
    ) -> GenerationResult:
        import torch
        
        full_prompt = prompt + initial
        self._input_ids = self.tokenizer.encode(full_prompt, return_tensors="pt")
        
        def logit_fn():
            return self._get_logits()
        
        def on_token_wrapper(token: str, step: int):
            self._update_input_ids(token)
            if on_token:
                on_token(token, step)
        
        result, sampler = until_complete(
            grammar=self.grammar,
            vocab=self.vocab,
            logit_fn=logit_fn,
            initial=initial,
            max_tokens=max_tokens,
            greedy_k=greedy_k,
            pre_top_k=pre_top_k,
            on_token=on_token_wrapper,
        )
        
        return result
    
    def generate_unconstrained(
        self,
        initial: str = "",
        prompt: str = "",
        max_tokens: int = 50,
        on_token: Optional[Callable[[str, int], None]] = None,
        stop_tokens: Optional[List[str]] = None,
    ) -> GenerationResult:
        """Generate tokens without any grammar/type constraints (raw model output)."""
        import torch
        
        if stop_tokens is None:
            stop_tokens = ["\n", "<|end|>", "<|endoftext|>"]
        
        full_prompt = prompt + initial
        self._input_ids = self.tokenizer.encode(full_prompt, return_tensors="pt")
        
        generated_text = initial
        tokens_generated = 0
        stopped_reason = "max_tokens"
        
        for step in range(max_tokens):
            logits = self._get_logits()
            
            # Get top token
            top_idx = max(range(len(logits)), key=lambda i: logits[i])
            token = self.vocab[top_idx]
            
            # Check stop conditions
            if token in stop_tokens or any(stop in token for stop in stop_tokens):
                stopped_reason = "stop_token"
                break
            
            generated_text += token
            tokens_generated += 1
            self._update_input_ids(token)
            
            if on_token:
                on_token(token, step)
        
        return GenerationResult(
            text=generated_text,
            is_complete=False,  # unconstrained doesn't track completeness
            tokens_generated=tokens_generated,
            stopped_reason=stopped_reason,
        )