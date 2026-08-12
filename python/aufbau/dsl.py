"""Build aufbau grammars from Python, without writing ``.auf`` source.

Usage::

    from aufbau.dsl import G, nt, lit, re_, hole, ctx, ref_, member, ascribe

    g = (G("Expr", ty="Type")
         .prod("Identifier", re_("[a-z]+"))
         .prod("Type", nt("Atom") | nt("Atom") ^ lit("->") ^ nt("Type"))
         .prod("Atom", nt("TypeName") | lit("(") ^ nt("Type") ^ lit(")"))
         .prod("Variable", nt("Identifier", bind="x"), rule="var")
         .prod("Expr", nt("Variable") | nt("Application"))
         .rule("var", [member("x")], ctx("x"))
         .rule("app", [ascribe("l", hole("A") ^ "->" ^ hole("B")),
                       ascribe("r", hole("A"))], hole("B"))
         .build())

`G.source()` renders ``.auf`` and `G.build()` hands it to the same parser
``SPG(source)`` uses, so the surface syntax has one definition and a grammar
built here is inspectable as text when it misbehaves.

Nothing here is specific to programming languages. A *type* is any term the
grammar can derive, so the same machinery constrains any structured output:
a schema, a record shape, a config. See ``docs/architecture.md``.

Requires the ``aufbau`` native module (pyo3).
"""

from __future__ import annotations

import os
from typing import Optional

import aufbau


# ── Symbols ─────────────────────────────────────────────────────────────

class _Sym:
    """A grammar symbol: nonterminal, literal, or regex terminal."""

    __slots__ = ("kind", "val", "bind")

    def __init__(self, kind: str, val: str, bind: Optional[str] = None):
        self.kind = kind   # "nt" | "lit" | "re"
        self.val = val
        self.bind = bind

    def __xor__(self, other: _Sym) -> _SymSeq:
        """``sym1 ^ sym2`` concatenates two symbols into a sequence."""
        if not isinstance(other, _Sym):
            raise TypeError(f"cannot concatenate _Sym and {type(other).__name__}")
        return _SymSeq([self, other])

    def __or__(self, other) -> _Alts:
        """``alt1 | alt2`` starts an alternation list."""
        return _Alts([_as_seq(self)]).__or__(other)

    def to_auf(self) -> str:
        if self.kind == "nt":
            base = self.val
        elif self.kind == "lit":
            base = f"'{self.val}'"
        elif self.kind == "re":
            base = f"/{self.val}/"
        else:
            raise ValueError(f"unknown symbol kind {self.kind!r}")
        return f"{base}[{self.bind}]" if self.bind else base

    def __repr__(self) -> str:
        if self.bind:
            return f"{self.kind}({self.val!r}, bind={self.bind!r})"
        return f"{self.kind}({self.val!r})"


class _SymSeq:
    """A sequence of symbols forming one production alternative."""

    __slots__ = ("syms",)

    def __init__(self, syms: list[_Sym]):
        self.syms = list(syms)

    def __xor__(self, other: _Sym) -> _SymSeq:
        if not isinstance(other, _Sym):
            raise TypeError(f"cannot concatenate _SymSeq and {type(other).__name__}")
        return _SymSeq(self.syms + [other])

    def __or__(self, other) -> _Alts:
        return _Alts([self]).__or__(other)

    def to_auf(self) -> str:
        return " ".join(s.to_auf() for s in self.syms) or "ε"

    def __repr__(self) -> str:
        return " ^ ".join(repr(s) for s in self.syms)


class _Alts:
    """A list of alternatives for a production (``a | b | c``)."""

    __slots__ = ("alts",)

    def __init__(self, alts: list[_SymSeq]):
        self.alts = list(alts)

    def __or__(self, other) -> _Alts:
        return _Alts(self.alts + [_as_seq(other)])

    def to_auf(self) -> str:
        return " | ".join(a.to_auf() for a in self.alts)

    def __repr__(self) -> str:
        return " | ".join(repr(a) for a in self.alts)


def _as_seq(item) -> _SymSeq:
    """Coerce a _Sym or _SymSeq into a _SymSeq."""
    if isinstance(item, _Sym):
        return _SymSeq([item])
    if isinstance(item, _SymSeq):
        return item
    raise TypeError(f"expected _Sym or _SymSeq, got {type(item).__name__}")


def _as_alts(item) -> _Alts:
    """Coerce a _Sym, _SymSeq, or _Alts into an _Alts."""
    if isinstance(item, _Alts):
        return item
    return _Alts([_as_seq(item)])


def nt(name: str, *, bind: Optional[str] = None) -> _Sym:
    """A nonterminal symbol reference."""
    return _Sym("nt", name, bind)


def lit(text: str) -> _Sym:
    """A literal token symbol."""
    return _Sym("lit", text)


def re_(pattern: str, *, bind: Optional[str] = None) -> _Sym:
    """A regex terminal symbol."""
    return _Sym("re", pattern, bind)


# ── Texpr (type expression) atoms ────────────────────────────────────────

def _needs_quotes(s: str) -> bool:
    """Does this literal have to be written ``'like this'``?

    A ``lit`` atom is both a type name (``Int``) and the separator between
    atoms (``->``). Written bare, a separator re-parses as itself, but a type
    name comes back as a *binding reference* and the rule fails to build with
    "type pattern 'Int' has no complete parse".

    Mirrors ``needs_quotes`` in ``src/typing/types.rs``. Quoting is the safe
    direction, so this only decides readability: ``?A -> ?B`` stays legible
    instead of becoming ``?A '->' ?B``.
    """
    return any(c.isalnum() or c in "_?" for c in s)


class _Texpr:
    """A type expression: a sequence of atoms joined by ``^``."""

    __slots__ = ("_atoms",)

    def __init__(self, atoms: list[tuple[str, str]]):
        self._atoms = atoms

    def __xor__(self, other: _Texpr | str) -> _Texpr:
        return _Texpr(self._atoms + _coerce(other)._atoms)

    def __rxor__(self, other: _Texpr | str) -> _Texpr:
        return _Texpr(_coerce(other)._atoms + self._atoms)

    def __repr__(self) -> str:
        return " ".join(f"[{k} {v}]" for k, v in self._atoms)

    def _to_auf(self) -> str:
        """Render as the .auf surface syntax for type expressions."""
        parts = []
        for kind, val in self._atoms:
            if kind == "raw":
                parts.append(val)
            elif kind == "lit":
                parts.append(f"'{val}'" if _needs_quotes(val) else val)
            elif kind == "hole":
                parts.append(f"?{val}")
            elif kind == "ref":
                parts.append(val)
            elif kind == "ctx":
                parts.append(f"Γ({val})")
            elif kind == "inst":
                parts.append(f"inst({val})")
            elif kind == "top":
                parts.append("⊤")
            elif kind == "bot":
                parts.append("∅")
        return " ".join(parts)


def _coerce(x: _Texpr | str) -> _Texpr:
    """Accept a plain ``str`` anywhere a type is expected, as *raw* ``.auf``.

    A bare string is source text, not a quoted literal: ``"?A -> ?B"`` has to
    reach the parser verbatim, and ``hole("A") ^ "->" ^ hole("B")`` has to give
    ``?A -> ?B``. Use :func:`lit_atom` for the literal type ``'Int'``, which is
    the case that must be quoted.
    """
    if isinstance(x, _Texpr):
        return x
    if isinstance(x, str):
        return _Texpr([("raw", x)])
    raise TypeError(f"expected a type expression or str, got {type(x).__name__}")


def lit_atom(s: str) -> _Texpr:
    """Literal type text."""
    return _Texpr([("lit", s)])


def hole(name: str = "") -> _Texpr:
    """Type hole (unification variable) ``?name``."""
    return _Texpr([("hole", name)])


def ref_(binding: str) -> _Texpr:
    """Reference to a binding's type."""
    return _Texpr([("ref", binding)])


def ctx(name: str) -> _Texpr:
    """``Γ(name)`` — context lookup."""
    return _Texpr([("ctx", name)])


def inst(name: str) -> _Texpr:
    """``inst(name)`` — context lookup with its variables freshened."""
    return _Texpr([("inst", name)])


def top() -> _Texpr:
    """``⊤`` — the unconstrained type."""
    return _Texpr([("top", "")])


def bot() -> _Texpr:
    """``∅`` — the contradictory type."""
    return _Texpr([("bot", "")])


# ── Premise helpers ──────────────────────────────────────────────────────

def ascribe(binding: str, ty: _Texpr | str, *, under: Optional[list[tuple[str, _Texpr | str]]] = None) -> dict:
    """Ascription premise: ``binding : ty`` (optionally under local bindings)."""
    rule: dict = {"kind": "ascribe", "binding": binding, "type": _coerce(ty)._to_auf()}
    if under:
        rule["under"] = [(n, _coerce(t)._to_auf()) for n, t in under]
    return rule


def member(name: str) -> dict:
    """Membership premise: ``name ∈ Γ``."""
    return {"kind": "member", "name": name}


def equate(a: _Texpr | str, b: _Texpr | str) -> dict:
    """Equality premise: ``a = b``."""
    return {
        "kind": "equate",
        "left": _coerce(a)._to_auf(),
        "right": _coerce(b)._to_auf(),
    }


# ── Grammar builder ─────────────────────────────────────────────────────

class G:
    """Fluent builder for aufbau grammars.

    Example::

        g = (G("Expr", ty="Ty")
             .prod("Identifier", re_("[a-z]+"))
             .prod("Type", nt("Atom") | nt("Atom") ^ lit("->") ^ nt("Type"))
             .rule("var", [member("x")], ctx("x"))
             .build())
    """

    def __init__(self, start: str, *, ty: Optional[str] = None):
        self._start = start
        self._ty = ty
        self._productions: list[dict] = []
        self._rewrites: list[tuple[str, str]] = []
        self._rules: list[tuple[str, str, str]] = []

    def prod(self, name: str, alts, *, rule: Optional[str] = None) -> "G":
        """Add a nonterminal definition.

        *alts* is a :class:`_Sym`, :class:`_SymSeq`, or :class:`_Alts`
        built with ``nt``, ``lit``, ``re_``, ``^``, and ``|``.
        """
        self._productions.append((name, rule, _as_alts(alts).to_auf()))
        return self

    def rewrite(self, lhs: str, rhs: str) -> "G":
        """Add a rewrite rule (``lhs`` normalizes to ``rhs``)."""
        self._rewrites.append((lhs, rhs))
        return self

    def rule(
        self, name: str, premises: list[dict] | str, conclusion: _Texpr | str
    ) -> "G":
        """Add a typing rule.

        *premises* is a list of dicts from :func:`ascribe`, :func:`member`, or
        :func:`equate` — or the raw premise line, if writing it out is clearer
        than assembling it. *conclusion* is a type expression or raw string.
        """
        if isinstance(premises, str):
            self._rules.append((name, premises, _coerce(conclusion)._to_auf()))
            return self
        self._rules.append(
            (name, self._premises_to_auf(premises), _coerce(conclusion)._to_auf())
        )
        return self

    def _premises_to_auf(self, premises: list[dict]) -> str:
        """Render premises as `.auf` source.

        This has to match `src/typing/rule.rs`, not merely look like it: the
        output is fed straight back to aufbau's own parser. Premises are comma
        separated, and a setting is one bracket per extension directly on the
        turnstile's context — `Γ[a:τ][b:σ] ⊢ e : ?A`.
        """
        parts = []
        for p in premises:
            kind = p["kind"]
            if kind == "ascribe":
                setting = "".join(f"[{n}:{t}]" for n, t in p.get("under", ()))
                parts.append(f"Γ{setting} ⊢ {p['binding']} : {p['type']}")
            elif kind == "member":
                parts.append(f"{p['name']} ∈ Γ")
            elif kind == "equate":
                parts.append(f"{p['left']} = {p['right']}")
        return ", ".join(parts)

    def source(self) -> str:
        """Render the grammar as ``.auf`` source.

        The DSL builds text and hands it to the one grammar parser, rather than
        pushing a second structural encoding through the FFI. That keeps exactly
        one definition of the surface syntax, and makes what the DSL produces
        directly inspectable when a grammar misbehaves.
        """
        # The loader reads the *last* declared nonterminal as the start symbol,
        # so the start production is emitted last regardless of when it was
        # declared. Everything else keeps its declaration order.
        prods = sorted(self._productions, key=lambda p: p[0] == self._start)

        def lhs(name: str, rule: Optional[str]) -> str:
            if name == self._ty:
                return f"{name}*"
            return f"{name}({rule})" if rule is not None else name

        # Blocks are separated by blank lines, and a block containing `::=` is
        # read as productions only. Rewrites and each rule therefore have to be
        # their own block, or they are silently ignored.
        blocks = ["\n".join(f"{lhs(n, r)} ::= {alts}" for n, r, alts in prods)]

        if self._rewrites:
            blocks.append("\n".join(f"{a} ~> {b}" for a, b in self._rewrites))

        for name, premises, conclusion in self._rules:
            bar = "-" * max(20, len(conclusion) + 5)
            blocks.append(f"{premises}\n{bar} ({name})\n{conclusion}")

        return "\n\n".join(blocks) + "\n"

    def build(self) -> "aufbau.SPG":
        """Assemble the grammar into an :class:`aufbau.SPG` handle."""
        return aufbau.SPG(self.source())


# ── Convenience: load from corpora ───────────────────────────────────────

def load_corpus(language: str, category: str) -> list[str]:
    """Load a corpus file from the ``corpora/`` directory.

    Walks up from the current file (and from CWD) looking for
    ``corpora/<language>/<category>.txt``.  Blank lines and ``#``
    comments are skipped.
    """
    search_dirs = [os.path.dirname(__file__), os.getcwd()]
    for d in search_dirs:
        while d and d != "/":
            candidate = os.path.join(d, "corpora", language, f"{category}.txt")
            if os.path.exists(candidate):
                programs = []
                with open(candidate, encoding="utf-8") as f:
                    for line in f:
                        line = line.strip()
                        if line and not line.startswith("#"):
                            programs.append(line)
                return programs
            parent = os.path.dirname(d)
            if parent == d:
                break
            d = parent
    raise FileNotFoundError(f"corpora/{language}/{category}.txt not found")
