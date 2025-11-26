"""High-level inference for typed constrained generation."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Callable, List, Optional

from .sampler import TypedSampler


@dataclass
class GenerationResult:
    text: str
    is_complete: bool
    tokens_generated: int
    stopped_reason: str  # max_tokens | complete | no_valid | type_error
    
    def to_sexpr(self, sampler: TypedSampler) -> Optional[str]:
        if not self.is_complete:
            return None
        try:
            return sampler.generator.to_sexpr()
        except:
            return None


def generate(
    grammar: str,
    vocab: List[str],
    logit_fn: Callable[[], List[float]],
    initial: str = "",
    max_tokens: int = 50,
    greedy_k: int = 1,
    pre_top_k: Optional[int] = 100,
    on_token: Optional[Callable[[str, int], None]] = None,
) -> tuple[GenerationResult, TypedSampler]:
    """Generate tokens using typed constrained decoding."""
    sampler = TypedSampler(grammar=grammar, vocab=vocab, logit_fn=logit_fn)
    
    if initial:
        try:
            sampler.feed(initial)
        except TypeError as e:
            return GenerationResult(
                text=initial,
                is_complete=False,
                tokens_generated=0,
                stopped_reason=f"type_error: {e}"
            ), sampler
    
    tokens_generated = 0
    stopped_reason = "max_tokens"
    
    for step in range(max_tokens):
        next_token = sampler.infer_greedy(k=greedy_k, pre_top_k=pre_top_k)
        
        if next_token is None:
            stopped_reason = "no_valid"
            break
        
        try:
            sampler.feed(next_token)
            tokens_generated += 1
            
            if on_token:
                on_token(next_token, step)
                
        except TypeError as e:
            stopped_reason = f"type_error: {e}"
            break
    
    return GenerationResult(
        text=sampler.current_text(),
        is_complete=sampler.is_complete(),
        tokens_generated=tokens_generated,
        stopped_reason=stopped_reason
    ), sampler


def until_complete(
    grammar: str,
    vocab: List[str],
    logit_fn: Callable[[], List[float]],
    initial: str = "",
    max_tokens: int = 100,
    greedy_k: int = 1,
    pre_top_k: Optional[int] = 100,
    on_token: Optional[Callable[[str, int], None]] = None,
) -> tuple[GenerationResult, TypedSampler]:
    """Generate until the parse is complete or max_tokens reached."""
    sampler = TypedSampler(grammar=grammar, vocab=vocab, logit_fn=logit_fn)
    
    if initial:
        try:
            sampler.feed(initial)
        except TypeError as e:
            return GenerationResult(
                text=initial,
                is_complete=False,
                tokens_generated=0,
                stopped_reason=f"type_error: {e}"
            ), sampler
    
    if sampler.is_complete():
        return GenerationResult(
            text=sampler.current_text(),
            is_complete=True,
            tokens_generated=0,
            stopped_reason="complete"
        ), sampler
    
    tokens_generated = 0
    stopped_reason = "max_tokens"
    
    for step in range(max_tokens):
        next_token = sampler.infer_greedy(k=greedy_k, pre_top_k=pre_top_k)
        
        if next_token is None:
            stopped_reason = "no_valid"
            break
        
        try:
            sampler.feed(next_token)
            tokens_generated += 1
            
            if on_token:
                on_token(next_token, step)
            
            if sampler.is_complete():
                stopped_reason = "complete"
                break
                
        except TypeError as e:
            stopped_reason = f"type_error: {e}"
            break
    
    return GenerationResult(
        text=sampler.current_text(),
        is_complete=sampler.is_complete(),
        tokens_generated=tokens_generated,
        stopped_reason=stopped_reason
    ), sampler
