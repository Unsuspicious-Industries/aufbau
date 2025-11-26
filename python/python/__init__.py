# proposition-7: Type-aware constrained decoding for LLMs
#
# Unlike CFG-only approaches (Outlines, etc.), Proposition 7 supports
# context-dependent grammars with typing rules - enabling generation
# of well-typed code, not just syntactically valid code.
#
# Usage:
#   import p7_constrained as p7
#   
#   gen = p7.Generator("gpt2", grammar=p7.GRAMMARS["stlc"])
#   result = gen("Generate a well-typed lambda term:")

from __future__ import annotations

from typing import TYPE_CHECKING

# Re-export Rust bindings
from p7_constrained import (
    Grammar,
    ConstrainedGenerator,
    ConstrainedLogitsProcessor,
    regex_matches,
    regex_prefix_valid,
)

# Python-side integration
from .sampler import Generator, SamplingMode, SamplerConfig, generate
from .grammars import GRAMMARS, list_grammars, get_grammar

__all__ = [
    # Rust bindings (low-level)
    "Grammar",
    "ConstrainedGenerator", 
    "ConstrainedLogitsProcessor",
    "regex_matches",
    "regex_prefix_valid",
    # Python integration (high-level)
    "Generator",
    "SamplingMode",
    "SamplerConfig",
    "generate",
    # Built-in grammars (all with typing rules)
    "GRAMMARS",
    "list_grammars",
    "get_grammar",
]

__version__: str = "0.1.0"
