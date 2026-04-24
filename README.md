# aufbau

Typed constrained decoding engine.

This repository provides the Rust core and Python bindings (`aufbau`) used by downstream projects like `p7`.

## Python FFI

Python bindings are exposed as module `aufbau` via PyO3 in `src/ffi/python.rs`.

Exported API:

- Class `Synthesizer(spec_source, input="", max_depth=None)`
  - `parse()`, `tokens()`, `token_examples()`, `feed(token)`, `try_feed(token)`
  - `set_input(input)`, `input()`, `ast()`, `is_complete()`
- Class `Regex(pattern)`
  - `matches(text)`, `prefix_match(prefix)`, `derivative(text)`, `deriv(character)`
  - `is_empty()`, `is_nullable()`, `match_len(text)`, `to_pattern()`
- Class `PrefixStatus`
  - `kind`, `regex`, `is_complete()`, `is_prefix()`, `is_extensible()`, `is_no_match()`
- Function: `version()`

Build:

```bash
cargo build --features python-ffi
```

Package (maturin config in `pyproject.toml`):

```bash
maturin develop
```
