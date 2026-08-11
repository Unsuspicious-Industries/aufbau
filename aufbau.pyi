"""Type stubs for the aufbau native module (pyo3).

Usage::

    from aufbau import SPG, Synthesizer, Term, Regex, PrefixStatus
    from aufbau.dsl import G, nt, lit, re_, hole, ctx, ascribe, member

A grammar is built either from ``.auf`` source with ``SPG(source)``, or
programmatically with :mod:`aufbau.dsl`. There is no structural
constructor: the DSL renders ``.auf`` and hands it to the same parser, so
the surface syntax has exactly one definition.
"""

from typing import Mapping, Optional, Union

#: Semantic version of the native module.
__version__: str

#: The Engine API this build implements. A consumer checks this one string at
#: startup: it names the whole v1 contract (atomic `set_context`, transactional
#: `feed`, all-root `verify`), so there is nothing else to probe for.
ENGINE_API: str

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
    rhs: list[Symbol]
    #: Legacy spelling of ``len(production)``.
    len: int
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
    input: str
    def node_count(self) -> int: ...
    def is_complete(self) -> bool: ...
    def type_of(self, evidence) -> Optional[Term]: ...


class Node:
    """A node in the parse AST."""
    nodeid: int
    evidence: str
    text: str
    start: int
    end: int
    #: Arity of the production this node matched, which for an incomplete node
    #: exceeds ``len(children)``.
    rhs: int
    children: list[Child]
    def is_complete(self) -> bool: ...
    def nt_name(self) -> str: ...
    def child_count(self) -> int: ...


class Child:
    """A child position in a production."""
    kind: str
    node: Optional[Node]
    def terminal_text(self) -> Optional[str]: ...
    def terminal_complete(self) -> Optional[bool]: ...


# ── Core engine classes ──────────────────────────────────────────────────

class SPG:
    """A semantic prefix grammar: syntax plus typing rules, validated on load."""

    def __init__(self, source: str) -> None:
        """Load a grammar from ``.auf`` source."""
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

    def root_type(self) -> Optional[Term]:
        """The type of *a* complete root. Prefer :meth:`verify`, which reports
        every complete root and so cannot hide a conflict."""
        ...

    def set_context(self, bindings: Mapping[str, str]) -> None:
        """Replace the entire typing context.

        Every type is parsed before anything is mutated: the replacement happens
        whole or not at all, and an invalid binding leaves the old context in
        place. All later calls see the new bindings immediately.
        """
        ...

    def context(self) -> list[tuple[str, str]]:
        """The accumulated typing context, rendered by the active grammar.

        This round-trips through :meth:`set_context` so callers carry the
        engine's authoritative context forward without reimplementing effect
        application.
        """
        ...

    def verify(self, expected_type: Optional[str] = ...) -> Verification:
        """Verify the current input, optionally against a goal type. State-free."""
        ...

    def add_to_ctx(self, name: str, ty: str) -> None:
        """Add one binding. Compatibility wrapper over :meth:`set_context`."""
        ...

    def clear_ctx(self) -> None:
        """Clear the context. Compatibility wrapper over :meth:`set_context`."""
        ...

    def is_complete(self) -> bool: ...

    def grammar(self) -> SPG: ...

    def get_rule(self, name: str) -> TypingRule: ...

    def ast(self) -> Ast: ...


class Verification:
    """The result of :meth:`Synthesizer.verify`."""

    #: ``"typed"`` | ``"live"`` | ``"dead"``.
    status: str
    #: Every distinct complete-root type, normalized and rendered. More than one
    #: entry means the complete roots disagree.
    root_types: list[str]
    #: Whether the goal was met, or ``None`` when no goal was given.
    goal_satisfied: Optional[bool]

    def is_ambiguous(self) -> bool: ...


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
