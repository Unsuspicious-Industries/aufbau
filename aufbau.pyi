"""Type stubs for the aufbau native module (pyo3).

Usage::

    from aufbau import SPG, Synthesizer, Term, Regex, PrefixStatus
    from aufbau_dsl import G, nt, lit, re_, hole, ctx, ascribe, member

See :mod:`aufbau_dsl` for the idiomatic grammar construction DSL.
"""

from typing import Optional, Union

# ── Type aliases for SPG.build() input ───────────────────────────────────

SymbolSpec = tuple[str, str, Optional[str]]
"""A grammar symbol spec: (kind, value, binding) with kind one of "nt" | "lit" | "re".

This is the *input* format for :meth:`SPG.build`. The :mod:`aufbau_dsl`
module provides :func:`~aufbau_dsl.nt`, :func:`~aufbau_dsl.lit`, and
:func:`~aufbau_dsl.re_` constructors.

At runtime, production symbols are returned as :class:`Symbol` objects.
"""

ProductionSpec = tuple[str, Optional[str], list[list[SymbolSpec]]]
"""A nonterminal definition specification: (name, rule, alternatives)."""

RuleSpec = tuple[str, str, str]
"""A typing rule in inference notation: (name, premises, conclusion)."""


# ── Runtime types ────────────────────────────────────────────────────────

class Term:
    """A type as a tree: a metavariable, a constructor over children, or a base leaf."""
    def label(self) -> Optional[str]: ...
    def children(self) -> list[Term]: ...
    def is_var(self) -> bool: ...
    def is_leaf(self) -> bool: ...
    def is_con(self) -> bool: ...
    def is_ground(self) -> bool: ...


class Symbol:
    """A grammar symbol as returned by :meth:`Production.rhs`.

    Attributes:
        kind: ``"terminal"`` or ``"nonterminal"``.
        name: The symbol's text (literal, nonterminal name, or regex pattern).
        binding: Optional binding name, or None.
    """
    kind: str
    name: str
    binding: Optional[str]

    def is_terminal(self) -> bool: ...
    def has_binding(self) -> bool: ...


class Production:
    """A nonterminal production (alternative)."""
    def rhs(self) -> list[Symbol]: ...
    def __len__(self) -> int: ...


class Segment:
    """A lexical segment from :meth:`SPG.tokenize`."""
    text: str
    start: int
    end: int
    index: int
    len: int


class TypingRule:
    """A typing rule, accessible via :meth:`Synthesizer.get_rule`."""
    name: str
    def premise_count(self) -> int: ...
    def pretty(self, indent: int = 0) -> str: ...
    def bindings(self) -> list[str]: ...


class Ast:
    """A parse AST, returned by :meth:`Synthesizer.ast`."""
    roots: list[Node]
    node_count: int
    is_complete: bool
    input: str
    def type_of(self, evidence) -> Optional[Term]: ...


class Node:
    """A node in the parse AST."""
    nodeid: int
    evidence: str
    is_complete: bool
    text: str
    nt_name: str
    start: int
    end: int
    child_count: int
    rhs: list[Child]
    def children(self) -> list[Node]: ...


class Child:
    """A child position in a production."""
    kind: str
    node: Optional[Node]
    terminal_text: Optional[str]
    terminal_complete: bool


# ── Core engine classes ──────────────────────────────────────────────────

class SPG:
    """A semantic prefix grammar: syntax plus typing rules, validated on load."""

    def __init__(self, source: str) -> None:
        """Load a grammar from ``.auf`` source."""
        ...

    @staticmethod
    def build(
        productions: list[ProductionSpec],
        rules: list[RuleSpec] = ...,
        rewrites: list[tuple[str, str]] = ...,
        start: Optional[str] = ...,
        ty: Optional[str] = ...,
    ) -> SPG:
        """Assemble a grammar structurally, without ``.auf`` source."""
        ...

    def source(self) -> str:
        """Render the grammar back to ``.auf`` source."""
        ...

    start: Optional[str]

    def nonterminals(self) -> list[str]: ...

    def productions(self, nt: str) -> list[Production]: ...

    def nt_rule(self, nt: str) -> Optional[str]: ...

    def rule_names(self) -> list[str]: ...

    def is_transparent(self, nt: str) -> bool: ...

    def specials(self) -> list[str]: ...

    def ir(self, rule: str) -> str:
        """Internal representation of a typing rule (debug/diagnostics)."""
        ...

    def tokenize(self, input: str) -> list[Segment]: ...

    def parse_type(self, s: str) -> Term: ...

    def show(self, t: Term) -> str: ...

    def normalize(self, s: str) -> Term: ...

    def unify(self, a: str, b: str) -> Optional[dict[str, str]]: ...

    def unify_modulo(self, a: str, b: str) -> Optional[dict[str, str]]: ...

    def rewrites(self) -> list[tuple[str, str]]: ...

    def signature(self) -> list[tuple[str, int]]: ...

    def completeness(self) -> tuple[str, list[str]]: ...


class Synthesizer:
    """Incremental parser / type checker over one grammar and input."""

    def __init__(self, spec_source: str, input: str = "") -> None: ...

    @staticmethod
    def from_grammar(grammar: SPG, input: str = "") -> Synthesizer: ...

    def set_input(self, input: str) -> None: ...

    def input(self) -> str: ...

    def parse(self) -> str: ...

    def feed(self, token: str) -> str: ...

    def try_feed(self, token: str) -> str: ...

    def mask(self, candidates: list[str]) -> list[bool]: ...

    def in_scope(self, expected: Optional[str] = ...) -> list[str]: ...

    def status(self) -> str: ...

    def root_type(self) -> Optional[Term]: ...

    def add_to_ctx(self, name: str, ty: str) -> None: ...

    def clear_ctx(self) -> None: ...

    def is_complete(self) -> bool: ...

    def grammar(self) -> SPG: ...

    def get_rule(self, name: str) -> TypingRule: ...

    def ast(self) -> Ast: ...


class Regex:
    """A regular expression over Brzozowski derivatives."""

    def __init__(self, pattern: str) -> None: ...

    def matches(self, text: str) -> bool: ...

    def prefix_match(self, prefix: str) -> PrefixStatus: ...

    def derivative(self, text: str) -> Regex: ...

    def deriv(self, character: str) -> Regex: ...

    def is_empty(self) -> bool: ...

    def is_nullable(self) -> bool: ...

    def match_len(self, text: str) -> Optional[int]: ...

    def to_pattern(self) -> str: ...


class PrefixStatus:
    """Result of matching a prefix against a Regex."""

    kind: str
    regex: Optional[Regex]

    def is_complete(self) -> bool: ...
    def is_prefix(self) -> bool: ...
    def is_extensible(self) -> bool: ...
    def is_no_match(self) -> bool: ...
