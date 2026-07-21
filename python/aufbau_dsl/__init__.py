"""Idiomatic Python DSL for building aufbau grammars without writing .auf source.

Usage:

    from aufbau import SPG
    from aufbau_dsl import G, nt, lit, re_

    g = (G("Expr", ty="Ty")
         .rule("var", [member("x")], ctx("x"))
         .rule("app", [ascribe("l", hole("A") ^ "->" ^ hole("B")),
                       ascribe("r", hole("A"))], hole("B"))
         .prod("Identifier", [re_("[a-z]+")])
         .prod("Type", [nt("Atom"), nt("Atom") ^ lit("->") ^ nt("Type")])
         .prod("Atom", [nt("TypeName"),
                        lit("(") ^ nt("Type") ^ lit(")")])
         .prod("Variable", @rule("var")(nt(bind="x", "Identifier")))
         .prod("Application", @rule("app")(nt(bind="l", "Expr") ^ nt(bind="r", "AtomE")))
         .prod("Expr", [nt("Variable"), nt("Lambda"), nt("Application")])
         .build())

Requires the `aufbau` native module (pyo3).
"""

from __future__ import annotations

from typing import Optional

import aufbau


# ── Symbol constructors ─────────────────────────────────────────────────

def nt(name: str, *, bind: Optional[str] = None) -> tuple[str, str, Optional[str]]:
    """A nonterminal symbol."""
    return ("nt", name, bind)


def lit(text: str) -> tuple[str, str, None]:
    """A literal token symbol."""
    return ("lit", text, None)


def re_(pattern: str, *, bind: Optional[str] = None) -> tuple[str, str, Optional[str]]:
    """A regex terminal symbol."""
    return ("re", pattern, bind)


# ── Texpr (type expression) atom helpers ─────────────────────────────────

class _Texpr:
    """A type expression: a sequence of atoms with a `^` concatenation operator."""

    def __init__(self, atoms: list[tuple[str, str]]):
        self._atoms = atoms

    def __xor__(self, other: _Texpr | str) -> _Texpr:
        """`^` concatenates type-expression atoms."""
        if isinstance(other, str):
            other = lit_atom(other)
        return _Texpr(self._atoms + other._atoms)

    def __repr__(self) -> str:
        return " ".join(f"[{k} {v}]" for k, v in self._atoms)

    def _to_auf(self) -> str:
        """Render as the .auf surface syntax for type expressions."""
        parts = []
        for kind, val in self._atoms:
            if kind == "lit":
                parts.append(val)
            elif kind == "hole":
                parts.append(f"?{val}")
            elif kind == "ref":
                parts.append(val)
            elif kind == "ctx":
                parts.append(f"Γ({val})")
            elif kind == "inst":
                parts.append(f"Γ[{val}]")
            elif kind == "top":
                parts.append("⊤")
            elif kind == "bot":
                parts.append("∅")
        return " ".join(parts)


def lit_atom(s: str) -> _Texpr:
    """Literal type text."""
    return _Texpr([("lit", s)])


def hole(name: str = "") -> _Texpr:
    """Type hole (unification variable) ?name."""
    return _Texpr([("hole", name)])


def ref_(binding: str) -> _Texpr:
    """Reference to a binding's type."""
    return _Texpr([("ref", binding)])


def ctx(name: str) -> _Texpr:
    """Γ(name) — context lookup."""
    return _Texpr([("ctx", name)])


def inst(name: str) -> _Texpr:
    """Γ[name] — instantiated scheme."""
    return _Texpr([("inst", name)])


def top() -> _Texpr:
    """⊤ — the unconstrained type."""
    return _Texpr([("top", "")])


def bot() -> _Texpr:
    """∅ — the contradictory type."""
    return _Texpr([("bot", "")])


# ── Premise helpers ──────────────────────────────────────────────────────

def ascribe(binding: str, ty: _Texpr | str, *, under: Optional[list[tuple[str, _Texpr | str]]] = None) -> dict:
    """Ascription premise: `binding : ty` (optionally under local bindings)."""
    if isinstance(ty, str):
        ty = lit_atom(ty)
    rule: dict = {"kind": "ascribe", "binding": binding, "type": ty._to_auf()}
    if under:
        rule["under"] = [(n, (t._to_auf() if isinstance(t, _Texpr) else t)) for n, t in under]
    return rule


def member(name: str) -> dict:
    """Membership premise: `name ∈ Γ`."""
    return {"kind": "member", "name": name}


def equate(a: _Texpr | str, b: _Texpr | str) -> dict:
    """Equality premise: `a = b`."""
    return {
        "kind": "equate",
        "left": a._to_auf() if isinstance(a, _Texpr) else a,
        "right": b._to_auf() if isinstance(b, _Texpr) else b,
    }


# ── Pending rule descriptor ──────────────────────────────────────────────

class _RuleBuilder:
    """A half-built typing rule: name, premises, conclusion."""

    def __init__(self, name: str):
        self.name = name
        self.premises: list[dict] = []
        self.conclusion: _Texpr | None = None
        self._closed = False

    def premise(self, *rules: dict) -> _RuleBuilder:
        if self._closed:
            raise RuntimeError("rule already built")
        self.premises.extend(rules)
        return self

    def conclude(self, ty: _Texpr | str) -> _RuleBuilder:
        if self._closed:
            raise RuntimeError("rule already built")
        if isinstance(ty, str):
            ty = lit_atom(ty)
        self.conclusion = ty
        self._closed = True
        return self

    def _to_auf(self) -> str:
        """Render as inference notation."""
        prems = "  ".join(self._premise_text(p) for p in self.premises)
        conc = self.conclusion._to_auf() if self.conclusion else "?"
        return f"rule {self.name}:\n  premises: {prems}\n  conclusion: {conc}"

    @staticmethod
    def _premise_text(p: dict) -> str:
        kind = p["kind"]
        if kind == "ascribe":
            txt = f"{p['binding']} : {p['type']}"
            if "under" in p:
                txt = f"(Γ[{', '.join(f'{n}={t}' for n, t in p['under'])}] ⊢ {txt})"
            return txt
        elif kind == "member":
            return f"{p['name']} ∈ Γ"
        elif kind == "equate":
            return f"{p['left']} = {p['right']}"
        return str(p)


# ── Grammar builder ─────────────────────────────────────────────────────

class G:
    """Fluent builder for aufbau grammars.

    Example::

        g = (G("Expr", ty="Ty")
             .prod("Identifier", [re_("[a-z]+")])
             .prod("Expr", [nt("Variable"), nt("Lambda")])
             .rule("var", [member("x")], ctx("x"))
             .build())
    """

    def __init__(self, start: str, *, ty: Optional[str] = None):
        self._start = start
        self._ty = ty
        self._productions: list[dict] = []
        self._rewrites: list[tuple[str, str]] = []
        self._rule_builders: dict[str, _RuleBuilder] = {}
        self._rules: list[tuple[str, str, str]] = []

    def prod(self, name: str, alternatives: list[list[tuple]], *, rule: Optional[str] = None) -> "G":
        """Add a nonterminal definition.

        Args:
            name: Nonterminal name.
            alternatives: List of alternatives; each alternative is a list of
                symbols (created via :func:`nt`, :func:`lit`, :func:`re_`).
            rule: Optional typing rule name.
        """
        if rule is not None:
            self._productions.append({"name": name, "alts": [alt for alt in alternatives], "rule": rule})
        else:
            self._productions.append({"name": name, "alts": [alt for alt in alternatives]})
        return self

    def __getitem__(self, alts: list[list]) -> "G":
        return self

    def rewrite(self, lhs: str, rhs: str) -> "G":
        """Add a rewrite rule (`lhs` normalizes to `rhs`)."""
        self._rewrites.append((lhs, rhs))
        return self

    def rule(self, name: str, premises: list[dict], conclusion: _Texpr | str) -> "G":
        """Add a typing rule in structured form.

        Args:
            name: Rule name (must match the name used in .prod(rule=...)).
            premises: List of premise dicts (from :func:`ascribe`, :func:`member`, :func:`equate`).
            conclusion: Conclusion type expression.
        """
        if isinstance(conclusion, str):
            conclusion = lit_atom(conclusion)
        self._rules.append((name, self._premises_to_auf(premises), conclusion._to_auf()))
        return self

    def _premises_to_auf(self, premises: list[dict]) -> str:
        """Convert structured premise list to inference-notation string."""
        parts = []
        for p in premises:
            kind = p["kind"]
            if kind == "ascribe":
                txt = f"⊢ {p['binding']} : {p['type']}"
                if "under" in p:
                    txt = f"(Γ[{', '.join(f'{n}={t}' for n, t in p['under'])}] ▸ {txt})"
                parts.append(txt)
            elif kind == "member":
                parts.append(p["name"])
            elif kind == "equate":
                parts.append(f"{p['left']} = {p['right']}")
        return "  ".join(parts)

    def build(self) -> "aufbau.SPG":
        """Assemble the grammar into an :class:`aufbau.SPG` handle.

        Raises ValueError if the grammar is invalid.
        """
        raw_prods = []
        for p in self._productions:
            rule_name = p.get("rule")
            prods = []
            for alt in p["alts"]:
                prods.append(list(alt))
            raw_prods.append((p["name"], rule_name, prods))

        rules = [list(r) for r in self._rules]
        rewrites = [(lhs, rhs) for lhs, rhs in self._rewrites]

        result = aufbau.SPG.build(
            productions=raw_prods,
            rules=rules,
            rewrites=rewrites if rewrites else None,
            start=self._start,
            ty=self._ty,
        )
        if result is None:
            raise ValueError("grammar build returned None (likely invalid)")
        return result


# ── Convenience: load from corpora ───────────────────────────────────────

import os


def load_corpus(language: str, category: str) -> list[str]:
    """Load a corpus file from the corpora directory.

    Args:
        language: Language name (e.g. "ml", "c").
        category: Corpus category ("valid", "invalid", "beyond").

    Returns:
        List of program texts.
    """
    # Find corpora/ by walking up from the current file or CWD
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
