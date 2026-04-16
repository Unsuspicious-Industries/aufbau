# aufbau

Typed constrained decoding and verification engine.

This repository provides the Rust core, OCaml verification integration, and
Python bindings (`aufbau_python`) used by downstream projects like `p7`.

## OCaml FFI (bidirectional)

With `--features ocaml-ffi`, the project supports both directions:

- OCaml -> Rust: OCaml externals in `verification/aufbau.ml` call Rust exports in `src/ffi/ocaml.rs` (`aufbau_complete_k`, `aufbau_complete`, `aufbau_check_prefix`, ...).
- Rust -> OCaml: Rust can invoke an OCaml closure through `aufbau_call_ocaml_callback` (OCaml passes `(string -> string)`, Rust calls it via `ocaml::function!`).

Build:

```bash
cargo build --features ocaml-ffi
```

Or use the existing verification build flow:

```bash
make verification
```

## Python FFI

Python bindings are exposed as module `aufbau_python` via PyO3 in `src/ffi/python.rs`.

Exported API:

- Class `Synthesizer(spec_source, input="", max_depth=None)`
  - `parse()`, `tokens()`, `token_examples()`, `feed(token)`
  - `set_input(input)`, `input()`, `ast()`, `is_complete()`
  - `add_binding(name, ty)`, `clear_bindings()`
- Functions: `version()`, `regex_matches(pattern, text)`, `regex_prefix_valid(pattern, prefix)`

Build:

```bash
cargo build --features python-ffi
```

Package (maturin config in `pyproject.toml`):

```bash
maturin develop
```
