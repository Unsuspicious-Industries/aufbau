"""
Proposition 7 Sampler - HuggingFace Transformers integration.

Usage:
    import p7_constrained as p7
    
    gen = p7.Generator("gpt2", grammar=p7.GRAMMARS["stlc"])
    result = gen("Generate a lambda term:")
"""

from __future__ import annotations

from dataclasses import dataclass
from enum import Enum
from typing import TYPE_CHECKING, Any, Callable, List, Optional, Union
import sys

# Import Rust bindings
from p7_constrained import Grammar, ConstrainedGenerator

if TYPE_CHECKING:
    import torch
    from transformers import PreTrainedModel, PreTrainedTokenizer


class SamplingMode(Enum):
    """Sampling mode for generation."""
    CONSTRAINED = "constrained"  # Only valid grammar tokens
    FREE = "free"                # Unrestricted generation
    BIMODAL = "bimodal"          # Switch between modes based on entropy


@dataclass
class SamplerConfig:
    """Configuration for the sampler."""
    # Sampling parameters
    temperature: float = 0.8
    top_k: int = 50
    top_p: float = 0.95
    max_tokens: int = 100
    
    # Mode settings
    mode: SamplingMode = SamplingMode.CONSTRAINED
    
    # Bimodal settings (only used when mode=BIMODAL)
    entropy_threshold: float = 4.0   # Switch to FREE if entropy exceeds this
    free_token_limit: int = 10       # Max tokens in FREE mode before switching back
    
    # Device
    device: Optional[str] = None  # None = auto-detect


class Generator:
    """
    Proposition 7 constrained generator.
    
    Integrates HuggingFace Transformers with the p7 constraint engine
    for type-aware constrained decoding.
    
    Example:
        gen = Generator("gpt2", grammar=p7.GRAMMARS["stlc"])
        result = gen("Generate a well-typed term:")
        print(result)  # e.g., "λ x : Int . x"
    """
    
    def __init__(
        self,
        model: str,
        grammar: Union[str, Grammar],
        config: Optional[SamplerConfig] = None,
        **model_kwargs: Any,
    ) -> None:
        """
        Initialize the generator.
        
        Args:
            model: HuggingFace model name or path
            grammar: Grammar spec string or Grammar object
            config: Sampler configuration
            **model_kwargs: Additional arguments for model loading
        """
        self.config: SamplerConfig = config or SamplerConfig()
        self.model_name: str = model
        
        # Parse grammar
        if isinstance(grammar, str):
            self.grammar: Grammar = Grammar(grammar)
        else:
            self.grammar = grammar
        
        # Lazy loading
        self._model: Optional[PreTrainedModel] = None
        self._tokenizer: Optional[PreTrainedTokenizer] = None
        self._vocab: Optional[List[str]] = None
        self._model_kwargs: dict[str, Any] = model_kwargs
        self._torch: Any = None
        self._device: str = "cpu"
    
    def _ensure_loaded(self) -> None:
        """Lazy load model and tokenizer."""
        if self._model is not None:
            return
        
        try:
            import torch
            from transformers import AutoModelForCausalLM, AutoTokenizer
        except ImportError:
            raise ImportError(
                "Please install transformers and torch:\n"
                "  pip install transformers torch"
            )
        
        self._torch = torch
        
        print(f"Loading {self.model_name}...", file=sys.stderr)
        
        self._tokenizer = AutoTokenizer.from_pretrained(self.model_name)
        self._model = AutoModelForCausalLM.from_pretrained(
            self.model_name,
            **self._model_kwargs
        )
        
        # Set pad token
        if self._tokenizer.pad_token is None:
            self._tokenizer.pad_token = self._tokenizer.eos_token
        
        # Device
        device = self.config.device
        if device is None:
            device = "cuda" if torch.cuda.is_available() else "cpu"
        self._model = self._model.to(device)
        self._device = device
        
        # Build vocabulary cache
        print("Building vocabulary cache...", file=sys.stderr)
        self._vocab = []
        for i in range(self._tokenizer.vocab_size):
            try:
                token_str = self._tokenizer.decode([i])
            except Exception:
                token_str = ""
            self._vocab.append(token_str)
        
        print(f"Ready! ({len(self._vocab)} tokens)", file=sys.stderr)
    
    def __call__(
        self,
        prompt: str = "",
        **kwargs: Any,
    ) -> str:
        """
        Generate constrained text.
        
        Args:
            prompt: Input prompt
            **kwargs: Override config options
            
        Returns:
            Generated text conforming to the grammar
        """
        return self.generate(prompt=prompt, **kwargs)
    
    def generate(
        self,
        prompt: str = "",
        max_tokens: Optional[int] = None,
        temperature: Optional[float] = None,
        mode: Optional[SamplingMode] = None,
        stream: bool = False,
        callback: Optional[Callable[[str, SamplingMode], None]] = None,
    ) -> str:
        """
        Generate constrained text with full options.
        
        Args:
            prompt: Input prompt
            max_tokens: Maximum tokens to generate
            temperature: Sampling temperature
            mode: Sampling mode (CONSTRAINED, FREE, BIMODAL)
            stream: If True, print tokens as generated
            callback: Optional callback(token, mode) for each token
            
        Returns:
            Generated text conforming to the grammar (and typing rules)
        """
        self._ensure_loaded()
        torch = self._torch
        
        assert self._model is not None
        assert self._tokenizer is not None
        assert self._vocab is not None
        
        # Config overrides
        max_tokens = max_tokens or self.config.max_tokens
        temperature = temperature or self.config.temperature
        mode = mode or self.config.mode
        
        # Initialize constraint engine
        gen: ConstrainedGenerator = ConstrainedGenerator(self.grammar)
        
        # Encode prompt
        if prompt:
            input_ids: torch.Tensor = self._tokenizer.encode(prompt, return_tensors="pt")
        else:
            bos = self._tokenizer.bos_token_id
            input_ids = torch.tensor([[bos]]) if bos else torch.tensor([[0]])
        
        input_ids = input_ids.to(self._device)
        
        # State for bimodal
        current_mode: SamplingMode = SamplingMode.CONSTRAINED if mode != SamplingMode.FREE else SamplingMode.FREE
        free_count: int = 0
        valid: List[int] = []
        
        generated: List[str] = []
        
        for step in range(max_tokens):
            # Forward pass
            with torch.no_grad():
                logits: torch.Tensor = self._model(input_ids).logits[:, -1, :]
            
            # Get valid tokens (constrained mode)
            if current_mode == SamplingMode.CONSTRAINED:
                valid = gen.get_valid_token_indices(self._vocab)
                
                if not valid:
                    # No valid tokens
                    if mode == SamplingMode.BIMODAL:
                        current_mode = SamplingMode.FREE
                        free_count = 0
                    else:
                        break
                else:
                    # Check entropy for bimodal
                    if mode == SamplingMode.BIMODAL:
                        entropy = self._compute_entropy(logits[0, valid])
                        if entropy > self.config.entropy_threshold:
                            current_mode = SamplingMode.FREE
                            free_count = 0
            
            # Apply constraints
            if current_mode == SamplingMode.CONSTRAINED and valid:
                mask = torch.full_like(logits, float('-inf'))
                mask[0, valid] = 0
                logits = logits + mask
            
            # Temperature
            if temperature != 1.0:
                logits = logits / temperature
            
            # Top-k
            if self.config.top_k > 0:
                top_k_vals = torch.topk(logits, self.config.top_k)[0][..., -1, None]
                logits[logits < top_k_vals] = float('-inf')
            
            # Top-p
            if self.config.top_p < 1.0:
                sorted_logits, sorted_idx = torch.sort(logits, descending=True)
                cumsum = torch.cumsum(torch.softmax(sorted_logits, dim=-1), dim=-1)
                remove = cumsum > self.config.top_p
                remove[..., 1:] = remove[..., :-1].clone()
                remove[..., 0] = False
                indices_to_remove = remove.scatter(1, sorted_idx, remove)
                logits[indices_to_remove] = float('-inf')
            
            # Sample
            probs = torch.softmax(logits, dim=-1)
            next_id: int = torch.multinomial(probs, 1).item()  # type: ignore
            
            # Decode
            token: str = self._vocab[next_id]
            
            # Track
            if current_mode == SamplingMode.CONSTRAINED:
                gen.feed_raw(token)
                generated.append(token)
            else:
                free_count += 1
                if free_count >= self.config.free_token_limit:
                    current_mode = SamplingMode.CONSTRAINED
            
            # Callbacks
            if stream:
                marker = "🔒" if current_mode == SamplingMode.CONSTRAINED else "🔓"
                print(f"{marker}{token}", end="", flush=True)
            if callback:
                callback(token, current_mode)
            
            # Update input
            input_ids = torch.cat([
                input_ids,
                torch.tensor([[next_id]], device=self._device)
            ], dim=-1)
            
            # Check completion
            if current_mode == SamplingMode.CONSTRAINED and gen.is_complete():
                break
            
            # Check EOS
            if next_id == self._tokenizer.eos_token_id:
                break
        
        if stream:
            print()
        
        return "".join(generated)
    
    def _compute_entropy(self, logits: torch.Tensor) -> float:
        """Compute entropy of logits."""
        torch = self._torch
        probs = torch.softmax(logits, dim=-1)
        log_probs = torch.log(probs + 1e-10)
        return float(-torch.sum(probs * log_probs).item())
    
    def batch_generate(
        self,
        prompts: List[str],
        **kwargs: Any,
    ) -> List[str]:
        """Generate for multiple prompts (sequential for now)."""
        return [self.generate(prompt=p, **kwargs) for p in prompts]


def generate(
    model: str,
    grammar: Union[str, Grammar],
    prompt: str = "",
    **kwargs: Any,
) -> str:
    """
    One-shot constrained generation.
    
    Example:
        result = p7.generate("gpt2", p7.GRAMMARS["stlc"], "λ")
    """
    gen = Generator(model, grammar)
    return gen.generate(prompt=prompt, **kwargs)
