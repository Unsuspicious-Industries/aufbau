"""Tests for the aufbau Python FFI bindings.

Run with:
    maturin develop
    python -m pytest src/ffi/test.py -v
"""

import os
import pytest
import aufbau


def _ml_grammar():
    """examples/ml.auf, found by walking up from this file (mirrors cert.ml)."""
    d = os.path.dirname(os.path.abspath(__file__))
    while d != "/":
        p = os.path.join(d, "examples", "ml.auf")
        if os.path.exists(p):
            with open(p) as f:
                return aufbau.SPG(f.read())
        d = os.path.dirname(d)
    raise FileNotFoundError("examples/ml.auf")


def _token_prefixes(g, src):
    """Cumulative prefixes ending at each token boundary: what a generator sees
    mid-stream, with tokens kept intact."""
    return [src[: s.end] for s in g.tokenize(src)]


STLC = r"""
    Identifier ::= /[a-z]+/
    TypeName ::= 'A' | 'B' | 'C' | /[A-Z][a-zA-Z0-9]*/
    TAtom ::= TypeName | '(' Type ')'
    Type* ::= TAtom | TAtom '→' Type
    Variable(var) ::= Identifier[x]
    Lambda(lambda) ::= 'λ' Identifier[param] ':' Type[τ] '.' Expr[body]
    Application(app) ::= Expr[func] Expr[arg]
    Expr ::= Variable | Lambda | Application | '(' Expr ')'

    x ∈ Γ
    ----------- (var)
    Γ(x)

    Γ[param:τ] ⊢ body : ?T
    ----------- (lambda)
    τ → ?T

    Γ ⊢ func : ?A → ?T, Γ ⊢ arg : ?A
    ----------- (app)
    ?T
"""

ARITH = r"""
    Number ::= /[0-9]+/
    Identifier ::= /[a-z][a-zA-Z0-9]*/
    Literal ::= Number
    Variable ::= Identifier
    Operator ::= '+' | '-' | '*' | '/'
    Primary ::= Literal | Variable | '(' Expression ')'
    Expression ::= Primary | Primary Operator Expression
"""


class TestGrammar:
    def test_load(self):
        g = aufbau.SPG("start ::= 'x' 'y'")
        assert g.start == "start"
        assert len(g.nonterminals()) == 1

    def test_nonterminals(self):
        g = aufbau.SPG(ARITH)
        nts = g.nonterminals()
        assert "Expression" in nts
        assert "Number" in nts

    def test_productions(self):
        g = aufbau.SPG(ARITH)
        prods = g.productions("Primary")
        assert len(prods) == 3
        rhs = prods[0].rhs
        assert rhs[0].kind == "nonterminal"
        assert rhs[0].name == "Literal"

    def test_all_productions(self):
        g = aufbau.SPG(ARITH)
        # Verify every nonterminal can be queried
        for nt in g.nonterminals():
            assert isinstance(g.productions(nt), list)

    def test_tokenize(self):
        g = aufbau.SPG(ARITH)
        segs = g.tokenize("1 + 2 * 3")
        assert len(segs) == 5
        assert segs[0].text == "1"
        assert segs[2].text == "2"

    def test_tokenize_empty(self):
        g = aufbau.SPG("start ::= 'a'")
        segs = g.tokenize("")
        assert segs == []

    def test_specials(self):
        g = aufbau.SPG(ARITH)
        assert "+" in g.specials()
        assert "*" in g.specials()

    def test_rule_names(self):
        g = aufbau.SPG(STLC)
        assert "var" in g.rule_names()
        assert "lambda" in g.rule_names()
        assert "app" in g.rule_names()

    def test_nt_rule(self):
        g = aufbau.SPG(STLC)
        assert g.nt_rule("Variable") == "var"
        assert g.nt_rule("Lambda") == "lambda"

    def test_transparent(self):
        g = aufbau.SPG(ARITH)
        # Primary is transparent: every production has exactly one
        # nonterminal child and no bound terminals
        assert g.is_transparent("Primary")
        # Expression has a production with 3 children (Primary Operator Expression)
        assert not g.is_transparent("Expression")


class TestSynthesizer:
    def test_parse_complete(self):
        s = aufbau.Synthesizer("start ::= 'x' 'y' 'z'", "x y z")
        result = s.parse()
        assert "nt0" in result

    def test_is_complete(self):
        s = aufbau.Synthesizer("start ::= 'a' 'b'", "a")
        assert not s.is_complete()
        s.feed(" b")
        assert s.is_complete()

    def test_feed(self):
        """feed extends the raw text; a fragment carries its own separator."""
        s = aufbau.Synthesizer("start ::= 'x' 'y'", "")
        s.feed("x")
        assert s.input() == "x"
        s.feed(" y")
        assert s.is_complete()

    def test_set_input(self):
        s = aufbau.Synthesizer("start ::= 'a' 'b'", "a")
        s.set_input("a b")
        assert s.input() == "a b"
        assert s.is_complete()

    def test_try_feed(self):
        s = aufbau.Synthesizer("start ::= 'x' 'y'", "x")
        result = s.try_feed(" y")
        assert "nt0" in result
        assert s.input() == "x"

    def test_add_to_ctx(self):
        s = aufbau.Synthesizer(STLC, "x")
        s.add_to_ctx("x", "A")
        result = s.parse()
        assert "nt" in result

    def test_clear_ctx(self):
        s = aufbau.Synthesizer(STLC, "x")
        s.add_to_ctx("x", "A")
        result_with_ctx = s.parse()
        assert "nt" in result_with_ctx
        s.clear_ctx()
        # Without context, parsing fails for typed grammar
        with pytest.raises(Exception):
            s.parse()

    def test_ast(self):
        s = aufbau.Synthesizer(STLC, "λx:A.x")
        ast = s.ast()
        assert ast.input == "λx:A.x"
        assert ast.node_count() > 0

    def test_ast_roots(self):
        s = aufbau.Synthesizer(ARITH, "1 + 2")
        ast = s.ast()
        roots = ast.roots
        assert len(roots) > 0

    def test_ast_type_of(self):
        s = aufbau.Synthesizer(STLC, "λx:A.x")
        ast = s.ast()
        for root in ast.roots:
            ty = ast.type_of(root.evidence)
            assert ty is not None

    def test_invalid_input(self):
        s = aufbau.Synthesizer("start ::= 'a'", "b")
        with pytest.raises(Exception):
            s.parse()

    def test_get_rule(self):
        s = aufbau.Synthesizer(STLC, "x")
        s.add_to_ctx("x", "A")
        rule = s.get_rule("var")
        assert rule is not None
        assert rule.name == "var"
        assert rule.bindings() == ["x"]

    def test_grammar_access(self):
        s = aufbau.Synthesizer(STLC, "x")
        g = s.grammar()
        assert g.start == "Expr"
        assert g.nt_rule("Variable") == "var"


class TestRegex:
    def test_match(self):
        r = aufbau.Regex("[a-z]+")
        assert r.matches("hello")
        assert not r.matches("123")

    def test_prefix(self):
        r = aufbau.Regex("abc")
        status = r.prefix_match("ab")
        assert status.is_prefix()
        assert not status.is_complete()

    def test_derivative(self):
        r = aufbau.Regex("abc")
        d = r.derivative("a")
        assert d.matches("bc")

    def test_nullable(self):
        r = aufbau.Regex("a*")
        assert r.is_nullable()


class TestSymbolProduction:
    def test_symbol_terminal(self):
        g = aufbau.SPG("start ::= 'x' 'y'")
        prods = g.productions("start")
        rhs = prods[0].rhs
        assert rhs[0].kind == "terminal"
        assert rhs[0].name == "x"
        assert not rhs[0].has_binding()

    def test_symbol_nonterminal_binding(self):
        g = aufbau.SPG(STLC)
        prods = g.productions("Lambda")
        rhs = prods[0].rhs
        assert any(s.has_binding() for s in rhs)


class TestStructuralBuild:
    """SPG.build: grammars as values, no .auf source."""

    def stlc(self):
        return aufbau.SPG.build(
            productions=[
                ("Identifier", None, [[("re", "[a-z]+", None)]]),
                ("TypeName", None, [[("re", "[A-Z][a-zA-Z0-9]*", None)]]),
                ("TAtom", None, [[("nt", "TypeName", None)],
                                 [("lit", "(", None), ("nt", "Type", None), ("lit", ")", None)]]),
                ("Type", None, [[("nt", "TAtom", None)],
                                [("nt", "TAtom", None), ("lit", "->", None), ("nt", "Type", None)]]),
                ("Variable", "var", [[("nt", "Identifier", "x")]]),
                ("Lambda", "lambda", [[("lit", "λ", None), ("nt", "Identifier", "a"),
                                       ("lit", ":", None), ("nt", "Type", "τ"),
                                       ("lit", ".", None), ("nt", "Expr", "e")]]),
                ("AtomE", None, [[("nt", "Variable", None)],
                                 [("lit", "(", None), ("nt", "Expr", None), ("lit", ")", None)]]),
                ("Application", "app", [[("nt", "Expr", "l"), ("nt", "AtomE", "r")]]),
                ("Expr", None, [[("nt", "AtomE", None)], [("nt", "Lambda", None)],
                                [("nt", "Application", None)]]),
            ],
            rules=[
                ("var", "x ∈ Γ", "Γ(x)"),
                ("lambda", "Γ[a:τ] ⊢ e : ?B", "τ -> ?B"),
                ("app", "Γ ⊢ l : ?A -> ?B, Γ ⊢ r : ?A", "?B"),
            ],
            start="Expr",
            ty="Type",
        )

    def test_build_and_check(self):
        g = self.stlc()
        s = aufbau.Synthesizer.from_grammar(g, "λx:A.x")
        assert s.status() == "typed"
        assert str(s.root_type()) == "Type(A, A)"

    def test_build_rejects_bad_rule_pattern(self):
        with pytest.raises(ValueError):
            aufbau.SPG.build(
                productions=[("W", "w", [[("re", "[a-z]+", "x")]])],
                rules=[("w", "Γ ⊢ x : ?A | ?B", "?A")],
            )

    def test_source_round_trip(self):
        g = self.stlc()
        g2 = aufbau.SPG(g.source())
        s = aufbau.Synthesizer.from_grammar(g2, "λx:A.x")
        assert s.status() == "typed"


class TestGeneration:
    """The constrained-generation surface: status, mask, input reuse."""

    def test_status_three_values(self):
        s = aufbau.Synthesizer("start ::= 'a' 'b'", "a b")
        assert s.status() == "typed"
        s.set_input("a")
        assert s.status() == "live"
        s.set_input("c")
        assert s.status() == "dead"

    def test_mask(self):
        s = aufbau.Synthesizer("start ::= 'a' 'b'", "a")
        assert s.mask([" b", " a", " c"]) == [True, False, False]
        # masking does not move the state
        assert s.input() == "a"
        assert s.status() == "live"

    def test_mask_typed_pruning(self):
        s = aufbau.Synthesizer(STLC, "λx:A.")
        # the body may open with the bound variable or a parenthesis, never `#`
        assert s.mask(["x", "(", "#"]) == [True, True, False]

    def test_set_input_reuses_grammar(self):
        s = aufbau.Synthesizer(STLC, "x")
        s.add_to_ctx("x", "A")
        assert s.status() == "typed"
        s.set_input("λy:B.y")
        assert s.status() == "typed"
        assert str(s.root_type()) == "Type(B, B)"


class TestDifferential:
    """The two intrinsic measurements of the pruning oracle, over examples/ml.auf:
    false-prune rate (soundness) and prune lead time (value). One synthesizer is
    reused across every prefix, so the cost is one parse per prefix, not a
    grammar rebuild per case."""

    VALID = [
        "(fun (x : int) -> x)(5)",
        "let a : int = 5 in a + 1",
        "if 1 < 2 then 1 else 0",
        "1 :: 2 :: 3 :: []",
        "fst (1, true)",
    ]
    # Ill-typed programs whose error term is sealed before the last token, so a
    # rigid clash fires at an exact node. Each is syntactically valid (a
    # syntax-only checker accepts the whole string), so the lead is pure value.
    INVALID = [
        "true < 2",                                   # left of < is bool
        "true + 1",                                   # left of + is bool
        "if 1 then 2 else 3",                         # condition is int
        "let a : bool = 5 in a",                      # value is int, declared bool
        "match 5 with [] -> 0 | h :: t -> 1",         # scrutinee is not a list
    ]

    def test_false_prune_rate_is_zero(self):
        """No prefix of a well-typed program is ever Dead: safe pruning never
        discards a completable prefix."""
        g = _ml_grammar()
        s = aufbau.Synthesizer.from_grammar(g)
        for p in self.VALID:
            for q in _token_prefixes(g, p):
                s.set_input(q)
                assert s.status() != "dead", f"false prune at {q!r} of {p!r}"

    def test_prune_lead_time(self):
        """Every ill-typed program is pruned strictly before its last token: the
        rigid clash fires when the offending sub-term seals, ahead of the
        syntax-only baseline that (the program being syntactically valid) would
        only reject at the end. Lead is the tokens between."""
        g = _ml_grammar()
        s = aufbau.Synthesizer.from_grammar(g)
        leads = {}
        for p in self.INVALID:
            pres = _token_prefixes(g, p)
            first_dead = None
            for i, q in enumerate(pres):
                s.set_input(q)
                if s.status() == "dead":
                    first_dead = i
                    break
            assert first_dead is not None, f"never pruned: {p!r}"
            leads[p] = len(pres) - 1 - first_dead
        # every program leads the end-of-input baseline by at least one token.
        assert all(lead >= 1 for lead in leads.values()), leads
        # the non-list scrutinee is caught ten tokens before the end.
        assert leads["match 5 with [] -> 0 | h :: t -> 1"] >= 10, leads


class TestCompleteness:
    """The realizability class: when does `live` guarantee a continuation?
    Safe pruning is unconditional; the classifier certifies the converse."""

    def test_no_rules_is_syntactic(self):
        g = aufbau.SPG("A ::= 'a' | 'a' A")
        assert g.completeness() == ("syntactic", [])

    def test_ml_is_inhabited(self):
        # `assert false : ?A` is the universal inhabitant: every ascribed
        # position can take any demanded type, so live prefixes realize.
        assert _ml_grammar().completeness() == ("inhabited", [])

    def test_stlc_is_sound_only(self):
        # The classic gap: `λf:A→B. f(` is live but uninhabited.
        kind, uninhabited = aufbau.SPG(STLC).completeness()
        assert kind == "sound"
        assert "Expr" in uninhabited


class TestInScope:
    """The var rule's membership constraint as a masking signal: the in-scope
    names filtered by the expected type (Γ-as-trie)."""

    def test_type_filtered_names(self):
        g = _ml_grammar()
        s = aufbau.Synthesizer.from_grammar(g)
        s.add_to_ctx("n", "int")
        s.add_to_ctx("b", "bool")
        s.add_to_ctx("f", "int -> int")
        assert s.in_scope() == ["b", "f", "n"]
        assert s.in_scope("int") == ["n"]
        assert s.in_scope("bool") == ["b"]
        assert s.in_scope("int -> int") == ["f"]
        # a hole expectation admits every name (everything unifies with a var)
        assert s.in_scope("?T") == ["b", "f", "n"]
