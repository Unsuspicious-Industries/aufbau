# aufbau

Typed constrained decoding engine.

This repository provides the Rust core and Python bindings (`aufbau`) used by downstream projects like `p7`.

## Quick Start

```bash
make test
```

## Makefile Usage

```bash
make            # Build everything (debug mode)
make test       # Run all tests (Rust + Python FFI)
make test-rust  # Run only Rust tests
make test-py    # Run only Python FFI tests
make check      # Verify all targets compile (including python-ffi)
make dev        # Fast development build (debug mode)
make build      # Release build
make run        # Run aufbau binary
make clean      # Remove all build artifacts
make help       # Show all available targets
```

## Python FFI

Exported API (stubs in `aufbau.pyi`):

- `SPG(source)` — load a grammar from `.auf` source
  - `SPG.build(productions, rules=[], rewrites=[], start=None, ty=None)` —
    assemble structurally: a production is `(name, rule, alternatives)`, a
    symbol `("nt"|"lit"|"re", value, binding)`, a rule
    `(name, premises, conclusion)` in inference notation; `ty` names the type
    fragment (the `Ty*` of the surface syntax)
  - `source()` — render back to `.auf`
  - `start`, `nonterminals()`, `productions(nt)`, `nt_rule(nt)`, `rule_names()`
  - `tokenize(input)`, `specials()`, `is_transparent(nt)`
  - `parse_type(s)`, `show(term)`, `normalize(s)`, `unify(a, b)`,
    `unify_modulo(a, b)`, `rewrites()`, `signature()`
  - `completeness()` — the realizability certificate as `(kind, sorts)`:
    `"syntactic"` (no rules: live ⇔ realizable), `"inhabited"` (universal
    inhabitants everywhere: live ⇒ realizable), or `"sound"` with the sorts
    where a live prefix may be uninhabited
- `Synthesizer(spec_source, input="")`
  - `Synthesizer.from_grammar(spg, input="")` — reuse a built grammar
  - `parse()`, `feed(token)`, `try_feed(token)`, `set_input(input)`, `input()`
  - `mask(candidates)` — the constrained-generation primitive: one bool per
    candidate continuation, no state change
  - `in_scope(expected=None)` — in-scope names whose type unifies with
    `expected` (the var rule's membership constraint as a type-filtered mask)
  - `status()` — `"typed" | "live" | "dead"`; `root_type()` — the type `Term`
  - `add_to_ctx(name, type)`, `clear_ctx()`, `is_complete()`, `ast()`
  - `get_rule(name)`, `grammar()`
- `Term` — a type as a tree: `label()`, `children()`, `is_var()`, `is_leaf()`,
  `is_con()`, `is_ground()`
- `Ast` — parse result
  - `roots`, `node_count()`, `is_complete()`, `input`, `type_of(evidence)`
- `Regex(pattern)`
  - `matches(text)`, `prefix_match(prefix)`, `derivative(text)`
  - `is_empty()`, `is_nullable()`, `match_len(text)`, `to_pattern()`
- `PrefixStatus`
  - `kind`, `regex`, `is_complete()`, `is_prefix()`, `is_extensible()`, `is_no_match()`

### Development

```bash
maturin develop      # install editable Python module
pip install pytest   # for running Python tests
```

## OCaml FFI

The engine is also exposed as native OCaml values (`ocaml/aufbau.mli`), built
inductively rather than through `.auf` source: type terms, rewrite theories,
unification, rule-level type expressions, typing rules, grammars, and the
`Check` module for running programs and prefixes against a grammar. Every
module and function in `aufbau.mli` carries its own doc comment; that file is
the authoritative reference.

```bash
make ocaml       # build the Rust staticlib + OCaml bindings (cargo build --features ocaml-ffi)
make ocaml-run   # run ocaml/demo.ml
make test-ocaml  # run ocaml/test.ml + the differential certification below
```

### Differential certification against OCaml

`ocaml/cert.ml` (with `ocaml/oracle.ml` as the trusted reference, built on
`compiler-libs`) checks aufbau's `ml` instance (`examples/ml.auf`) against
OCaml's own typechecker over a corpus that mirrors
`src/validation/parseable/ml.rs`, so the Rust suite and this harness pin the
same programs from both sides of the boundary. It certifies two claims:

- **soundness** — every program aufbau accepts, OCaml's compiler also accepts;
- **agreement** — on the monomorphic corpus, acceptance coincides exactly,
  except at a documented fragment boundary (OCaml's `let`-generalization of
  dead match branches) where aufbau is soundly stricter.

Runs as part of `make test-ocaml` / `dune runtest --root ocaml`.
