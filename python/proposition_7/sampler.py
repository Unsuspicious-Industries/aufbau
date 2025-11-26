"""Typed sampler for constrainted LLM generation."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Callable, List, Optional, Union
import math
import random

from proposition_7.proposition_7 import Grammar, ConstrainedGenerator


class CompletionEngine:
    """Low-level completoin engine wrapper."""
    
    def __init__(self, grammar_str: str):
        self.grammar = Grammar(grammar_str)
        self.generator = ConstrainedGenerator(self.grammar)
    
    def feed(self, prompt: str) -> None:
        self.generator.feed_raw(prompt)

    def get_completions(self) -> List[str]:
        return self.generator.get_completions()
    
    def debug_completions(self) -> Any:
        return self.generator.debug_completions()


class TypedSampler:
    """
    Typed sampler for constrainted LLM generation.
    
    Filters LLM outputs to only well-typed completions.
    """
    
    def __init__(
        self,
        grammar: str,
        vocab: List[str],
        logit_fn: Callable[[], List[float]],
    ):
        self.grammar = Grammar(grammar)
        self.generator = ConstrainedGenerator(self.grammar)
        self.vocab = vocab
        self.logit_fn = logit_fn
        self._neg_inf = -float('inf')
    
    def reset(self) -> None:
        self.generator.reset()
    
    def feed(self, text: str) -> None:
        """Feed text. Raises TypeError if ill-typed."""
        self.generator.feed_raw(text)
    
    def current_text(self) -> str:
        return self.generator.current_text()
    
    def infer(self, pre_top_k: Optional[int] = None) -> List[float]:
        """Get masked logits (invalid tokens set to -inf)."""
        logits = list(self.logit_fn())
        
        if pre_top_k is not None and pre_top_k < len(logits):
            indexed = [(i, l) for i, l in enumerate(logits)]
            indexed.sort(key=lambda x: x[1], reverse=True)
            top_k_indices = [i for i, _ in indexed[:pre_top_k]]
            
            top_k_tokens = [self.vocab[i] for i in top_k_indices]
            valid_tokens = self.generator.filter_completions(top_k_tokens)
            valid_set = set(valid_tokens)
            
            valid_indices = {i for i in top_k_indices if self.vocab[i] in valid_set}
        else:
            valid_indices = set(self.generator.filter_completion_indices(self.vocab))
        
        for i in range(len(logits)):
            if i not in valid_indices:
                logits[i] = self._neg_inf
        
        return logits
    
    def infer_text(self, k: int = 10, pre_top_k: Optional[int] = None) -> List[str]:
        """Get top-k valid token strings sorted by logit."""
        logits = self.logit_fn()
        
        if pre_top_k is not None:
            indexed = [(i, l) for i, l in enumerate(logits)]
            indexed.sort(key=lambda x: x[1], reverse=True)
            candidates = indexed[:pre_top_k]
            
            candidate_tokens = [self.vocab[i] for i, _ in candidates]
            valid_tokens = set(self.generator.filter_completions(candidate_tokens))
            
            valid_pairs = [(i, l) for i, l in candidates if self.vocab[i] in valid_tokens]
        else:
            valid_indices = self.generator.filter_completion_indices(self.vocab)
            if not valid_indices:
                return []
            valid_pairs = [(i, logits[i]) for i in valid_indices]
            valid_pairs.sort(key=lambda x: x[1], reverse=True)
        
        top_k = valid_pairs[:k]
        return [self.vocab[i] for i, _ in top_k]
    
    def infer_greedy(
        self, 
        k: int = 1, 
        pre_top_k: Optional[int] = None
    ) -> Optional[str]:
        """Pick one token from top-k valid. k=1 is pure greedy."""
        valid = self.infer_text(k=k, pre_top_k=pre_top_k)
        
        if not valid:
            return None
        
        if k == 1 or len(valid) == 1:
            return valid[0]
        
        return random.choice(valid)
    
    def infer_unconstrained(self, k: int = 1) -> Optional[str]:
        """Pick from top-k ignoring grammar. For comparision."""
        logits = self.logit_fn()
        
        indexed = [(i, l) for i, l in enumerate(logits)]
        indexed.sort(key=lambda x: x[1], reverse=True)
        top_k = indexed[:k]
        
        if not top_k:
            return None
        
        if k == 1:
            return self.vocab[top_k[0][0]]
        
        idx, _ = random.choice(top_k)
        return self.vocab[idx]
    
    def is_complete(self) -> bool:
        return self.generator.is_complete()
    
    def check_completion(self, token: str) -> bool:
        return self.generator.check_completion(token)
