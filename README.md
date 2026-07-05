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
