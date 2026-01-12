# proposition_7: Type-aware constrainted decoding for LLMs
#
# Unlike CFG-only approaches, P7 supports context-dependant grammars
# with typing rules - enabling generation of well-typed code.

from __future__ import annotations

from typing import TYPE_CHECKING

from proposition_7.proposition_7 import (
    Grammar,
    ConstrainedGenerator,
    ConstrainedLogitsProcessor,
    regex_matches,
    regex_prefix_valid,
)

from .sampler import CompletionEngine, TypedSampler
from .grammars import GRAMMARS, list_grammars, get_grammar, get_grammar_info
from .inference import generate, until_complete, GenerationResult
from .llm import ConstrainedModel
from .environment import (
    ReasoningEnvironment,
    SimpleEnvironment,
    EnvironmentResult,
    ThinkBlock,
    GrammarBlock,
    Mode,
    create_environment,
    build_system_prompt,
)

__all__ = [
    # rust bindings
    "Grammar",
    "ConstrainedGenerator", 
    "ConstrainedLogitsProcessor",
    "regex_matches",
    "regex_prefix_valid",
    # python stuff
    "CompletionEngine",
    "TypedSampler",
    "generate",
    "until_complete",
    "GenerationResult",
    "ConstrainedModel",
    # environment
    "ReasoningEnvironment",
    "SimpleEnvironment", 
    "EnvironmentResult",
    "ThinkBlock",
    "GrammarBlock",
    "Mode",
    "create_environment",
    "build_system_prompt",
    "get_grammar_info",
    # grammars
    "GRAMMARS",
    "list_grammars",
    "get_grammar",
]

__version__: str = "0.1.0"
