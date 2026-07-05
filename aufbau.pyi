"""Type stubs for the aufbau native module (pyo3)."""

from typing import Optional

Symbol = tuple[str, str, Optional[str]]
"""A grammar symbol: (kind, value, binding) with kind one of "nt" | "lit" | "re"."""

Production = tuple[str, Optional[str], list[list[Symbol]]]
"""A nonterminal definition: (name, rule, alternatives)."""

Rule = tuple[str, str, str]
"""A typing rule in inference notation: (name, premises, conclusion)."""

class Term:
    """A type as a tree: a metavariable, a constructor over children, or a base leaf."""

    def label(self) -> Optional[str]:
        """Constructor label (the nonterminal), or None for a hole or leaf."""
        ...
    def children(self) -> list[Term]:
        """Child terms of a constructor (empty for a hole or leaf)."""
        ...
    def is_var(self) -> bool:
        """Whether this term is an unbound metavariable (a hole)."""
        ...
    def is_leaf(self) -> bool:
        """Whether this term is a base leaf (an atomic type name)."""
        ...
    def is_con(self) -> bool:
        """Whether this term is a constructor applied to children."""
        ...
    def is_ground(self) -> bool:
        """No unification variables: a fully determined type."""
        ...

class SPG:
    """A semantic prefix grammar: syntax plus typing rules, validated on load."""

    def __init__(self, source: str) -> None:
        """Load a grammar from `.auf` source."""
        ...
    @staticmethod
    def build(
        productions: list[Production],
        rules: list[Rule] = ...,
        rewrites: list[tuple[str, str]] = ...,
        start: Optional[str] = ...,
        ty: Optional[str] = ...,
    ) -> SPG:
        """Assemble a grammar structurally, without `.auf` source. `ty` names the
        type fragment (the `Ty*` of the surface syntax); `start` defaults to the
        last production."""
        ...
    def source(self) -> str:
        """Render the grammar back to `.auf` source."""
        ...
    start: Optional[str]
    """The start nonterminal."""
    def nonterminals(self) -> list[str]:
        """All nonterminal names."""
        ...
    def productions(self, nt: str) -> list:
        """The alternatives defining nonterminal `nt`."""
        ...
    def nt_rule(self, nt: str) -> Optional[str]:
        """Rule name attached to a nonterminal, if any."""
        ...
    def rule_names(self) -> list[str]:
        """Names of all typing rules."""
        ...
    def is_transparent(self, nt: str) -> bool:
        """Is every production of `nt` a transparent wrapper?"""
        ...
    def specials(self) -> list[str]:
        """Literal tokens requiring no separating whitespace from neighbors."""
        ...
    def tokenize(self, input: str) -> list:
        """Split `input` into lexical segments using the grammar's own tokenizer."""
        ...
    def parse_type(self, s: str) -> Term:
        """Parse a type string into its term (tree), using the grammar's structure."""
        ...
    def show(self, t: Term) -> str:
        """Render a term back to the grammar's surface syntax."""
        ...
    def normalize(self, s: str) -> Term:
        """Normal form of a type under the grammar's rewrite theory."""
        ...
    def unify(self, a: str, b: str) -> Optional[dict[str, str]]:
        """Free (syntactic) unification of two types, or None on clash."""
        ...
    def unify_modulo(self, a: str, b: str) -> Optional[dict[str, str]]:
        """Unify two types modulo the rewrite theory (normalize, then unify)."""
        ...
    def rewrites(self) -> list[tuple[str, str]]:
        """The declared rewrite theory, as (lhs, rhs) source pairs."""
        ...
    def signature(self) -> list[tuple[str, int]]:
        """Constructor names with the arity they appear at, sorted."""
        ...
    def completeness(self) -> tuple[str, list[str]]:
        """The realizability certificate as (kind, sorts): "syntactic" (no rules:
        live <=> realizable), "inhabited" (universal inhabitants everywhere: live
        => realizable), or "sound" with the sorts where a live prefix may be
        uninhabited."""
        ...

class Synthesizer:
    """Incremental parser / type checker over one grammar and input."""

    def __init__(self, spec_source: str, input: str = "") -> None:
        """Load a grammar from `.auf` source and start it on `input`."""
        ...
    @staticmethod
    def from_grammar(grammar: SPG, input: str = "") -> Synthesizer:
        """A synthesizer over an already-built grammar (no `.auf` re-parse)."""
        ...
    def set_input(self, input: str) -> None:
        """Reset the input, keeping the loaded grammar."""
        ...
    def input(self) -> str:
        """Current accumulated input."""
        ...
    def parse(self) -> str:
        """Parse, returning an AST string."""
        ...
    def feed(self, token: str) -> str:
        """Feed one token (state-altering)."""
        ...
    def try_feed(self, token: str) -> str:
        """Try feeding one token without altering state."""
        ...
    def mask(self, candidates: list[str]) -> list[bool]:
        """The constrained-generation mask: for each candidate continuation, can
        the current input still be extended by it? One pass over the whole
        candidate set, no state change."""
        ...
    def in_scope(self, expected: Optional[str] = ...) -> list[str]:
        """In-scope names whose type unifies with `expected` (every name when
        `expected` is None) -- the var rule's membership constraint intersected
        with a type."""
        ...
    def status(self) -> str:
        """The three-valued verdict on the current input: "typed" (a complete,
        well-typed parse), "live" (a completable prefix), or "dead"."""
        ...
    def root_type(self) -> Optional[Term]:
        """The type of a complete root, as a term."""
        ...
    def add_to_ctx(self, name: str, ty: str) -> None:
        """Add a variable to the typing context. The type is parsed with the
        grammar into its tree, so a structured type becomes a constructor, not a
        flat leaf."""
        ...
    def clear_ctx(self) -> None:
        """Clear the typing context."""
        ...
    def is_complete(self) -> bool:
        """Whether the parsed tree is complete."""
        ...
    def grammar(self) -> SPG:
        """Expose the grammar for inspection."""
        ...
    def get_rule(self, name: str):
        """Get a specific typing rule by name."""
        ...
    def ast(self):
        """Return the current AST as a structured object."""
        ...

class Regex:
    """A regular expression over the derivative construction (Brzozowski), used
    for terminal matching and prefix-status queries."""

    def __init__(self, pattern: str) -> None:
        """Parse a regex pattern."""
        ...
    def matches(self, text: str) -> bool:
        """Whether `text` is a complete match."""
        ...
    def prefix_match(self, prefix: str) -> PrefixStatus:
        """Classify `prefix` as a match, a partial prefix, or no match."""
        ...
    def derivative(self, text: str) -> Regex:
        """The regex remaining after consuming `text`."""
        ...
    def deriv(self, character: str) -> Regex:
        """The regex remaining after consuming one character."""
        ...
    def is_empty(self) -> bool:
        """Whether this regex matches nothing at all."""
        ...
    def is_nullable(self) -> bool:
        """Whether the empty string is a match."""
        ...
    def match_len(self, text: str) -> Optional[int]:
        """Length of the longest prefix of `text` that is a complete match."""
        ...
    def to_pattern(self) -> str:
        """Render back to source pattern syntax."""
        ...

class PrefixStatus:
    """The result of matching a prefix against a Regex: complete, extensible,
    a bare prefix, or no match at all."""

    kind: str
    regex: Optional[Regex]
    def is_complete(self) -> bool:
        """Whether the prefix is itself a complete or extensible match."""
        ...
    def is_prefix(self) -> bool:
        """Whether the prefix could still be completed, but isn't a match yet."""
        ...
    def is_extensible(self) -> bool:
        """Whether the prefix is a complete match that could also be extended."""
        ...
    def is_no_match(self) -> bool:
        """Whether the prefix cannot lead to any match."""
        ...
