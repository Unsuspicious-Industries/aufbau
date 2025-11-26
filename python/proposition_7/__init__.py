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
from .grammars import GRAMMARS, list_grammars, get_grammar
from .inference import generate, until_complete, GenerationResult
from .llm import ConstrainedModel

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
    # grammars
    "GRAMMARS",
    "list_grammars",
    "get_grammar",
]

__version__: str = "0.1.0"
