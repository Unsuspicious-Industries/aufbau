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

    def test_ir_renders_every_rule(self):
        """The IR debug view must work for every rule of a real grammar, not
        just the well-shaped ones. `var` is Member-only; `Program` also carries
        its splices, so they are part of the view."""
        g = _ml_grammar()
        for name in g.rule_names():
            text = g.ir(name)
            assert text.startswith(f"{name}:\n"), f"{name}: bad header\n{text}"

        g = aufbau.SPG(STLC)
        assert "member x" in g.ir("var")
        lam = g.ir("lambda")
        assert "push_scope" in lam and "pop_scope" in lam
        assert "splice body = [" in lam

    def test_ir_unknown_rule(self):
        with pytest.raises(ValueError):
            aufbau.SPG(STLC).ir("no_such_rule")

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


class TestDslBuild:
    """`aufbau.dsl` is the programmatic builder: grammars as values, no .auf
    source written by hand."""

    def stlc(self):
        from aufbau.dsl import G, nt, lit, re_

        return (
            G("Expr", ty="Type")
            .prod("Identifier", re_("[a-z]+"))
            .prod("TypeName", re_("[A-Z][a-zA-Z0-9]*"))
            .prod("TAtom", nt("TypeName") | lit("(") ^ nt("Type") ^ lit(")"))
            .prod("Type", nt("TAtom") | nt("TAtom") ^ lit("->") ^ nt("Type"))
            .prod("Variable", nt("Identifier", bind="x"), rule="var")
            .prod(
                "Lambda",
                lit("\u03bb") ^ nt("Identifier", bind="a") ^ lit(":")
                ^ nt("Type", bind="\u03c4") ^ lit(".") ^ nt("Expr", bind="e"),
                rule="lambda",
            )
            .prod("AtomE", nt("Variable") | lit("(") ^ nt("Expr") ^ lit(")"))
            .prod("Application", nt("Expr", bind="l") ^ nt("AtomE", bind="r"), rule="app")
            .prod("Expr", nt("AtomE") | nt("Lambda") | nt("Application"))
            .rule("var", "x \u2208 \u0393", "\u0393(x)")
            .rule("lambda", "\u0393[a:\u03c4] \u22a2 e : ?B", "\u03c4 -> ?B")
            .rule("app", "\u0393 \u22a2 l : ?A -> ?B, \u0393 \u22a2 r : ?A", "?B")
            .build()
        )

    def test_build_and_check(self):
        g = self.stlc()
        s = aufbau.Synthesizer.from_grammar(g, "\u03bbx:A.x")
        assert s.status() == "typed"
        assert g.show(s.root_type()) == "A -> A"

    def test_build_rejects_bad_rule_pattern(self):
        from aufbau.dsl import G, re_

        with pytest.raises(ValueError):
            (G("W")
             .prod("W", re_("[a-z]+", bind="x"), rule="w")
             .rule("w", "\u0393 \u22a2 x : ?A | ?B", "?A")
             .build())

    def test_source_round_trip(self):
        g = self.stlc()
        g2 = aufbau.SPG(g.source())
        s = aufbau.Synthesizer.from_grammar(g2, "\u03bbx:A.x")
        assert s.status() == "typed"

    def test_start_is_emitted_last(self):
        """The loader reads the last declared nonterminal as the start symbol,
        so `source()` must place it there whatever order prod() was called in."""
        from aufbau.dsl import G, nt, re_

        g = (G("Top")
             .prod("Top", nt("Leaf"))
             .prod("Leaf", re_("[a-z]+"))
             .build())
        assert g.start == "Top"


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
        assert s.grammar().show(s.root_type()) == "B -> B"


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


class TestDslRoundTrip:
    """`aufbau.dsl` emits `.auf` source, so its output has to satisfy aufbau's
    own parser — not merely resemble it.

    The published DSL rendered premises space-separated, settings as
    `Γ[a=?A] ▸ …`, and `inst(x)` as `Γ[x]`; none of those parse. Building a
    grammar, rendering it with `source()`, and reloading is what holds the DSL
    and `src/typing/rule.rs` to the same syntax.
    """

    def stlc(self):
        from aufbau.dsl import G, nt, lit, re_, hole, ctx, ref_, member, ascribe

        return (
            G("Expr", ty="Type")
            .prod("Identifier", re_("[a-z]+"))
            .prod("TypeName", re_("[A-Z][a-zA-Z0-9]*"))
            .prod("TAtom", nt("TypeName") | lit("(") ^ nt("Type") ^ lit(")"))
            .prod("Type", nt("TAtom") | nt("TAtom") ^ lit("->") ^ nt("Type"))
            .prod("Variable", nt("Identifier", bind="x"), rule="var")
            .prod(
                "Lambda",
                lit("λ") ^ nt("Identifier", bind="a") ^ lit(":")
                ^ nt("Type", bind="τ") ^ lit(".") ^ nt("Expr", bind="e"),
                rule="lambda",
            )
            .prod("AtomE", nt("Variable") | lit("(") ^ nt("Expr") ^ lit(")"))
            .prod("Application", nt("Expr", bind="l") ^ nt("AtomE", bind="r"), rule="app")
            .prod("Expr", nt("AtomE") | nt("Lambda") | nt("Application"))
            .rule("var", [member("x")], ctx("x"))
            .rule("lambda", [ascribe("e", hole("B"), under=[("a", ref_("τ"))])],
                  ref_("τ") ^ "->" ^ hole("B"))
            .rule("app", [ascribe("l", hole("A") ^ "->" ^ hole("B")),
                          ascribe("r", hole("A"))], hole("B"))
            .build()
        )

    def test_builds(self):
        g = self.stlc()
        assert set(g.rule_names()) == {"var", "lambda", "app"}

    def test_source_reparses(self):
        """The whole point: what the DSL renders must load again."""
        g = self.stlc()
        src = g.source()
        again = aufbau.SPG(src)
        assert set(again.rule_names()) == set(g.rule_names())

    def test_rules_survive_round_trip(self):
        """Reloading must preserve each rule's compiled IR, not just its name."""
        g = self.stlc()
        again = aufbau.SPG(g.source())
        for name in sorted(g.rule_names()):
            assert again.ir(name) == g.ir(name), f"{name} changed:\n{g.source()}"

    def test_premises_are_comma_separated(self):
        g = self.stlc()
        # `app` has two premises; space-joined they parse as one malformed premise.
        assert g.ir("app").count("ascribe") == 2

    def test_setting_renders_as_scoped_extension(self):
        g = self.stlc()
        lam = g.ir("lambda")
        assert "push_scope" in lam and "extend a" in lam

    def test_inst_renders_as_call(self):
        from aufbau.dsl import inst

        assert inst("x")._to_auf() == "inst(x)"

    def test_literal_type_is_quoted(self):
        """A bare `Int` re-parses as a binding reference, and the rule then
        fails to build with "type pattern 'Int' has no complete parse".
        Separators must stay bare or `?A -> ?B` becomes unreadable."""
        from aufbau.dsl import lit_atom, hole, ref_

        assert lit_atom("Int")._to_auf() == "'Int'"
        assert (ref_("τ") ^ "->" ^ hole("B"))._to_auf() == "τ -> ?B"

    def test_literal_type_builds_and_round_trips(self):
        from aufbau.dsl import G, nt, re_, lit_atom

        g = (G("E")
             .prod("N", re_("[0-9]+"))
             .prod("Num", nt("N", bind="d"), rule="num")
             .prod("E", nt("Num"))
             .rule("num", [], lit_atom("Int"))
             .build())
        assert aufbau.SPG(g.source()).ir("num") == g.ir("num")

    def test_typed_parse_still_works(self):
        """A round-tripped grammar must still type-check input."""
        g = self.stlc()
        again = aufbau.SPG(g.source())
        for grammar in (g, again):
            s = aufbau.Synthesizer.from_grammar(grammar)
            s.add_to_ctx("y", "A")
            assert s.feed("y") is not None
            assert s.is_complete()


class TestEngineApiV1:
    """The Engine API v1 contract: one identifier, one authoritative context,
    a transactional feed, and all-root verification."""

    def test_module_identity(self):
        assert aufbau.ENGINE_API == "aufbau.engine/v1"
        assert isinstance(aufbau.__version__, str) and aufbau.__version__

    def test_set_context_replaces_wholesale(self):
        s = aufbau.Synthesizer(STLC, "x")
        s.set_context({"x": "A", "y": "B"})
        assert s.in_scope() == ["x", "y"]
        s.set_context({"z": "C"})
        assert s.in_scope() == ["z"]

    def test_set_context_is_atomic_on_failure(self):
        """An invalid binding must leave the previous context untouched, not a
        half-applied one."""
        s = aufbau.Synthesizer(STLC, "x")
        s.set_context({"x": "A"})
        with pytest.raises(ValueError):
            s.set_context({"good": "A", "bad": "!!!"})
        assert s.in_scope() == ["x"]

    def test_context_change_is_visible_immediately(self):
        """The regression behind `add_to_ctx` then `mask`: a second copy of the
        context meant one operation could see a mutation another did not."""
        s = aufbau.Synthesizer(STLC, "")
        assert s.mask(["x"]) == [False]
        s.set_context({"x": "A"})
        assert s.mask(["x"]) == [True]
        assert s.status() != "dead"

    def test_failed_feed_leaves_state_unchanged(self):
        """feed() used to install the input before parsing it, so a rejected
        token stayed in `input` and poisoned every later call."""
        s = aufbau.Synthesizer("start ::= 'a' 'b'", "a")
        before = s.input()
        with pytest.raises(Exception):
            s.feed("ZZZ")
        assert s.input() == before
        assert s.status() != "dead"
        s.feed(" b")
        assert s.is_complete()

    def test_masked_candidate_feeds(self):
        """A candidate accepted by mask must be accepted by feed from the same
        state; otherwise the generation mask is not a usable signal."""
        s = aufbau.Synthesizer(STLC, "")
        s.set_context({"x": "A"})
        candidates = ["x", "λ", "!!!"]
        allowed = s.mask(candidates)
        for cand, ok in zip(candidates, allowed):
            probe = aufbau.Synthesizer(STLC, "")
            probe.set_context({"x": "A"})
            fed = True
            try:
                probe.feed(cand)
            except Exception:
                fed = False
            assert fed == ok, f"mask said {ok} for {cand!r}, feed said {fed}"

    def test_verify_reports_root_types(self):
        s = aufbau.Synthesizer(STLC, "λx:A.x")
        v = s.verify()
        assert v.status == "typed"
        assert len(v.root_types) == 1
        assert not v.is_ambiguous()
        assert v.goal_satisfied is None

    def test_verify_checks_goal(self):
        s = aufbau.Synthesizer(STLC, "λx:A.x")
        assert s.verify("A → A").goal_satisfied is True
        assert s.verify("B → B").goal_satisfied is False

    def test_verify_on_incomplete_and_dead(self):
        live = aufbau.Synthesizer(STLC, "λx:A.")
        assert live.verify().status == "live"
        assert live.verify().root_types == []
        assert live.verify("?T").goal_satisfied is False

        dead = aufbau.Synthesizer(STLC, "!!!")
        assert dead.verify().status == "dead"
        assert dead.verify("?T").goal_satisfied is False

    def test_verify_is_state_free(self):
        s = aufbau.Synthesizer(STLC, "λx:A.x")
        before = s.input()
        first = s.verify("A → A")
        second = s.verify("A → A")
        assert (first.status, first.root_types) == (second.status, second.root_types)
        assert s.input() == before

    def test_compat_wrappers_still_work(self):
        s = aufbau.Synthesizer(STLC, "x")
        s.add_to_ctx("x", "A")
        assert s.in_scope() == ["x"]
        s.clear_ctx()
        assert s.in_scope() == []
