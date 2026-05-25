"""Tests for the aufbau Python FFI bindings.

Run with:
    maturin develop
    python -m pytest src/ffi/test.py -v
"""

import pytest
import aufbau


STLC = r"""
    Identifier ::= /[a-z]+/
    Type ::= 'A' | 'B' | 'C' | /[A-Z][a-zA-Z0-9]*/
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

    Γ ⊢ func : τ₂ → ?T, Γ ⊢ arg : τ₂
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
        s.feed("b")
        assert s.is_complete()

    def test_feed(self):
        s = aufbau.Synthesizer("start ::= 'x' 'y'", "")
        s.feed("x")
        assert s.input() == "x"
        s.feed("y")
        assert s.is_complete()

    def test_set_input(self):
        s = aufbau.Synthesizer("start ::= 'a' 'b'", "a")
        s.set_input("a b")
        assert s.input() == "a b"
        assert s.is_complete()

    def test_try_feed(self):
        s = aufbau.Synthesizer("start ::= 'x' 'y'", "x")
        result = s.try_feed("y")
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
