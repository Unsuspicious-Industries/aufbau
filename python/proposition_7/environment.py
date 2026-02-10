"""
Environment for structured reasoning with LLMs.

Allows LLMs to switch between:
- <think>...</think>: Unconstrained chain-of-thought reasoning
- <{grammar}>...</{grammar}>: Grammar-constrained guaranteed output

This enables CoT reasoning while ensuring the final output is well-typed.
Grammar-independent: tags are derived from grammar name.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from enum import Enum
from typing import Callable, List, Optional, Tuple, Dict, Any

from .grammars import GRAMMARS, get_grammar, get_grammar_info


class Mode(Enum):
    """Current generation mode."""
    THINK = "think"
    GRAMMAR = "grammar"  # Grammar-constrained (tag varies by grammar)


@dataclass
class ThinkBlock:
    """A block of unconstrained reasoning."""
    content: str
    
    def __str__(self) -> str:
        return f"<think>{self.content}</think>"


@dataclass 
class GrammarBlock:
    """A block of grammar-constrained output."""
    content: str
    grammar_name: str
    is_complete: bool
    
    def __str__(self) -> str:
        return f"<{self.grammar_name}>{self.content}</{self.grammar_name}>"


@dataclass
class EnvironmentResult:
    """Result from environment generation."""
    blocks: List[ThinkBlock | GrammarBlock] = field(default_factory=list)
    total_tokens: int = 0
    stopped_reason: str = "max_tokens"
    grammar_name: str = ""
    
    @property
    def think_blocks(self) -> List[ThinkBlock]:
        return [b for b in self.blocks if isinstance(b, ThinkBlock)]
    
    @property
    def grammar_blocks(self) -> List[GrammarBlock]:
        return [b for b in self.blocks if isinstance(b, GrammarBlock)]
    
    @property
    def final_output(self) -> Optional[GrammarBlock]:
        """Get the last grammar block (the final output)."""
        blocks = self.grammar_blocks
        return blocks[-1] if blocks else None
    
    @property
    def all_thoughts(self) -> str:
        """Concatenate all thinking."""
        return "\n".join(b.content for b in self.think_blocks)
    
    @property
    def is_complete(self) -> bool:
        """Check if we have a complete grammar output."""
        final = self.final_output
        return final is not None and final.is_complete
    
    def __str__(self) -> str:
        return "".join(str(b) for b in self.blocks)


def build_system_prompt(
    grammar_name: str,
    task_description: Optional[str] = None,
    include_examples: bool = True,
) -> str:
    """
    Procedurally generate a system prompt for the given grammar.
    
    Args:
        grammar_name: Name of the grammar (e.g., "stlc", "fun", "imp")
        task_description: Optional task-specific description
        include_examples: Whether to include syntax examples
        
    Returns:
        System prompt string
    """
    info = get_grammar_info(grammar_name)
    
    lines = [
        f"You are a reasoning assistant that produces well-typed {info['short']}.",
        "",
        "You can use two modes:",
        "- <think>...</think>: Free-form reasoning. Think step by step.",
        f"- <{grammar_name}>...</{grammar_name}>: Produce the final well-typed output. This is grammar-constrained.",
        "",
        "Process:",
        "1. Use <think> to reason about the problem",
        f"2. When ready, use <{grammar_name}> to produce typed output",
        "3. The output must be syntactically and type-correct",
    ]
    
    if info["syntax_hints"]:
        lines.extend(["", "Syntax:"])
        for hint in info["syntax_hints"]:
            lines.append(f"  - {hint}")
    
    if include_examples and info["examples"]:
        lines.extend(["", "Examples:"])
        for name, code in info["examples"]:
            lines.append(f"  {name}: {code}")
    
    if task_description:
        lines.extend(["", f"Task: {task_description}"])
    
    return "\n".join(lines)


class ReasoningEnvironment:
    """
    Environment that allows LLMs to reason with CoT then produce typed output.
    
    Grammar-independent: uses grammar name for tags (e.g., <stlc>, <fun>).
    
    Usage:
        from proposition_7 import ConstrainedModel, GRAMMARS
        
        model = ConstrainedModel.from_pretrained("...", grammar=get_grammar("stlc"))
        env = ReasoningEnvironment(model, grammar_name="stlc")
        
        result = env.generate(
            prompt="Create a function that takes an Int and returns it",
            initial="λx:",
        )
        
        print(result.all_thoughts)  # CoT reasoning
        print(result.final_output)  # Well-typed output
    """
    
    THINK_OPEN = "<think>"
    THINK_CLOSE = "</think>"
    
    def __init__(
        self,
        model,  # ConstrainedModel
        grammar_name: str,
        think_budget: int = 200,
        grammar_budget: int = 100,
        system_prompt: Optional[str] = None,
    ):
        """
        Initialize the reasoning environment.
        
        Args:
            model: A ConstrainedModel with grammar loaded
            grammar_name: Name of the grammar (for tags and prompts)
            think_budget: Max tokens per think block
            grammar_budget: Max tokens per grammar block
            system_prompt: Custom system prompt (auto-generated if None)
        """
        self.model = model
        self.grammar_name = grammar_name
        self.think_budget = think_budget
        self.grammar_budget = grammar_budget
        
        # Grammar-specific tags
        self.grammar_open = f"<{grammar_name}>"
        self.grammar_close = f"</{grammar_name}>"
        
        # System prompt (procedurally generated or custom)
        self.system_prompt = system_prompt or build_system_prompt(grammar_name)
        
        # Stop tokens for think mode
        self._think_stop = [
            self.THINK_CLOSE,
            self.grammar_open,
            self.grammar_close,  # Also stop on closing tag
            "<|end|>",
            "<|endoftext|>",
            "<|eot_id|>",
        ]
    
    def _generate_think(
        self,
        prompt: str,
        on_token: Optional[Callable[[str, int], None]] = None,
    ) -> Tuple[str, str, int]:
        """
        Generate unconstrained thinking until </think> or <grammar>.
        
        Returns: (content, stop_tag, tokens_generated)
        """
        result = self.model.generate_unconstrained(
            prompt=prompt,
            max_tokens=self.think_budget,
            on_token=on_token,
            stop_tokens=self._think_stop,
        )
        
        content = result.text
        stop_tag = ""
        
        # Check what stopped us - check all possible tags
        for tag in [self.THINK_CLOSE, self.grammar_open, self.grammar_close]:
            if tag in content:
                idx = content.find(tag)
                stop_tag = tag
                content = content[:idx]
                break
        
        return content, stop_tag, result.tokens_generated
    
    def _generate_grammar(
        self,
        prompt: str,
        initial: str = "",
        on_token: Optional[Callable[[str, int], None]] = None,
    ) -> Tuple[str, bool, int]:
        """
        Generate grammar-constrained output.
        
        Returns: (content, is_complete, tokens_generated)
        """
        result = self.model.until_complete(
            prompt=prompt,
            initial=initial,
            max_tokens=self.grammar_budget,
            on_token=on_token,
        )
        
        return result.text, result.is_complete, result.tokens_generated
    
    def generate(
        self,
        prompt: str,
        initial: str = "",
        max_blocks: int = 10,
        start_thinking: bool = True,
        on_think_token: Optional[Callable[[str, int], None]] = None,
        on_grammar_token: Optional[Callable[[str, int], None]] = None,
        on_mode_switch: Optional[Callable[[Mode, str], None]] = None,
    ) -> EnvironmentResult:
        """
        Generate with alternating think/grammar blocks.
        
        Args:
            prompt: Initial prompt/question
            initial: Initial text for grammar blocks (partial expression)
            max_blocks: Maximum number of blocks to generate
            start_thinking: Whether to start with <think> mode
            on_think_token: Callback for each token in think mode
            on_grammar_token: Callback for each token in grammar mode
            on_mode_switch: Callback when switching modes (mode, tag)
            
        Returns:
            EnvironmentResult with all blocks and metadata
        """
        result = EnvironmentResult(grammar_name=self.grammar_name)
        
        # Build full prompt with system
        full_prompt = self.system_prompt + "\n\n" + prompt
        
        current_mode = Mode.THINK if start_thinking else Mode.GRAMMAR
        accumulated = full_prompt
        
        if start_thinking:
            accumulated += f"\n{self.THINK_OPEN}"
        else:
            accumulated += f"\n{self.grammar_open}"
        
        for block_idx in range(max_blocks):
            if on_mode_switch:
                tag = self.THINK_OPEN if current_mode == Mode.THINK else self.grammar_open
                on_mode_switch(current_mode, tag)
            
            if current_mode == Mode.THINK:
                content, stop_tag, tokens = self._generate_think(
                    prompt=accumulated,
                    on_token=on_think_token,
                )
                
                result.blocks.append(ThinkBlock(content=content))
                result.total_tokens += tokens
                accumulated += content
                
                if stop_tag == self.THINK_CLOSE:
                    accumulated += self.THINK_CLOSE
                elif stop_tag == self.grammar_open:
                    accumulated += self.grammar_open
                else:
                    # No clear transition, close think and start grammar
                    accumulated += self.THINK_CLOSE
                    accumulated += f"\n{self.grammar_open}"
                
                current_mode = Mode.GRAMMAR
                if stop_tag != self.grammar_open:
                    accumulated += f"\n{self.grammar_open}"
            
            else:  # GRAMMAR mode
                # Only use initial on first grammar block
                use_initial = initial if not result.grammar_blocks else ""
                
                try:
                    content, is_complete, tokens = self._generate_grammar(
                        prompt=accumulated,
                        initial=use_initial,
                        on_token=on_grammar_token,
                    )
                except Exception as e:
                    # Grammar generation failed - stop here
                    result.stopped_reason = f"error: {e}"
                    break
                
                result.blocks.append(GrammarBlock(
                    content=content,
                    grammar_name=self.grammar_name,
                    is_complete=is_complete,
                ))
                result.total_tokens += tokens
                accumulated += content + self.grammar_close
                
                if is_complete:
                    result.stopped_reason = "complete"
                    break
                else:
                    # Not complete, need more thinking
                    current_mode = Mode.THINK
                    accumulated += f"\n{self.THINK_OPEN}"
        
        if result.stopped_reason != "complete":
            result.stopped_reason = "max_blocks"
        
        return result
    
    def generate_simple(
        self,
        task: str,
        initial: str = "",
        cot_prompt: Optional[str] = None,
        on_think: Optional[Callable[[str], None]] = None,
        on_output: Optional[Callable[[str], None]] = None,
    ) -> EnvironmentResult:
        """
        Simplified generation: one think block, then grammar output.
        
        Args:
            task: The task to accomplish
            initial: Starting point for grammar output
            cot_prompt: Custom CoT instruction
            on_think: Callback with full think text
            on_output: Callback with full grammar output
            
        Returns:
            EnvironmentResult with one think and one grammar block
        """
        if not cot_prompt:
            info = get_grammar_info(self.grammar_name)
            cot_prompt = f"Think step by step about how to complete this {info['short']}."
        
        full_prompt = f"{task}\n\n{cot_prompt}\nInitial: {initial}"
        
        # Collect outputs for callbacks
        think_tokens = []
        grammar_tokens = []
        
        def on_think_token(tok, step):
            think_tokens.append(tok)
            
        def on_grammar_token(tok, step):
            grammar_tokens.append(tok)
        
        result = self.generate(
            prompt=full_prompt,
            initial=initial,
            max_blocks=2,
            start_thinking=True,
            on_think_token=on_think_token,
            on_grammar_token=on_grammar_token,
        )
        
        if on_think and result.think_blocks:
            on_think(result.think_blocks[0].content)
        if on_output and result.grammar_blocks:
            on_output(result.grammar_blocks[0].content)
        
        return result


class SimpleEnvironment:
    """
    Simplified environment for single-shot CoT + grammar generation.
    
    For cases where you just want: think once, then generate.
    """
    
    def __init__(
        self,
        model,  # ConstrainedModel
        grammar_name: str,
        cot_tokens: int = 150,
        grammar_tokens: int = 100,
    ):
        self.model = model
        self.grammar_name = grammar_name
        self.cot_tokens = cot_tokens
        self.grammar_tokens = grammar_tokens
        self.grammar_open = f"<{grammar_name}>"
        self.grammar_close = f"</{grammar_name}>"
    
    def generate(
        self,
        task: str,
        initial: str = "",
        on_cot: Optional[Callable[[str], None]] = None,
        on_output: Optional[Callable[[str], None]] = None,
    ) -> Tuple[str, str, bool]:
        """
        Generate with CoT reasoning then grammar output.
        
        Args:
            task: Description of what to generate
            initial: Initial partial expression
            on_cot: Callback with CoT text
            on_output: Callback with grammar text
            
        Returns:
            (cot_text, grammar_text, is_complete)
        """
        info = get_grammar_info(self.grammar_name)
        
        # Phase 1: Chain of Thought
        cot_prompt = f"""{task}

Think step by step about how to complete this {info['short']}: {initial}
Consider the types and constraints.

<think>"""
        
        cot_result = self.model.generate_unconstrained(
            prompt=cot_prompt,
            max_tokens=self.cot_tokens,
            stop_tokens=["</think>", self.grammar_open, "\n\n\n"],
        )
        cot_text = cot_result.text
        
        if on_cot:
            on_cot(cot_text)
        
        # Phase 2: Grammar-constrained Generation
        grammar_prompt = f"""{task}

Reasoning:
{cot_text}

Now produce the well-typed {info['short']} starting with: {initial}

{self.grammar_open}"""
        
        grammar_result = self.model.until_complete(
            prompt=grammar_prompt,
            initial=initial,
            max_tokens=self.grammar_tokens,
        )
        
        if on_output:
            on_output(grammar_result.text)
        
        return cot_text, grammar_result.text, grammar_result.is_complete


def create_environment(
    model_name: str,
    grammar_name: str,
    device: str = "auto",
    think_budget: int = 200,
    grammar_budget: int = 100,
    **model_kwargs,
) -> ReasoningEnvironment:
    """
    Convenience function to create a reasoning environment.
    
    Args:
        model_name: HuggingFace model name
        grammar_name: Name of grammar from GRAMMARS dict
        device: Device to use
        think_budget: Max tokens per think block
        grammar_budget: Max tokens per grammar block
        **model_kwargs: Additional args for model loading
        
    Returns:
        Configured ReasoningEnvironment
    """
    from .llm import ConstrainedModel
    
    grammar = get_grammar(grammar_name)
    
    model = ConstrainedModel.from_pretrained(
        model_name,
        grammar=grammar,
        device=device,
        **model_kwargs,
    )
    
    return ReasoningEnvironment(
        model=model,
        grammar_name=grammar_name,
        think_budget=think_budget,
        grammar_budget=grammar_budget,
    )
