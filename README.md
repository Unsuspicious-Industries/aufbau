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

Exported API:

- `SPG(source)` — load and inspect a grammar
  - `start`, `nonterminals()`, `productions(nt)`, `nt_rule(nt)`, `rule_names()`
  - `tokenize(input)`, `specials()`, `is_transparent(nt)`
- `Synthesizer(spec_source, input="")`
  - `parse()`, `feed(token)`, `try_feed(token)`, `set_input(input)`, `input()`
  - `add_to_ctx(name, type)`, `clear_ctx()`, `is_complete()`, `ast()`
  - `get_rule(name)`, `grammar()`
- `Ast` — parse result
  - `roots`, `node_count()`, `is_complete()`, `input`, `type_of(evidence)`
- `Regex(pattern)`
  - `matches(text)`, `prefix_match(prefix)`, `derivative(text)`
  - `is_empty()`, `is_nullable()`, `match_len(text)`, `to_pattern()`
- `PrefixStatus`
  - `kind`, `regex`, `is_complete()`, `is_prefix()`, `is_extensible()`, `is_no_match()`
- `version()` → string

### Development

```bash
maturin develop      # install editable Python module
pip install pytest   # for running Python tests
```
