.PHONY: all build clean rust help test test-rust test-py dev check-deps check lc run \
        verif verif-build verif-check verif-clean clean-verif \
        verif-obligations ocaml ocaml-run test-ocaml

ARGS ?=

all: build

build: rust
	@echo "✓ Build complete"

run: build
	@./target/release/aufbau $(ARGS)

rust:
	@echo "Building Rust library..."
	@cargo build --release
	@echo "✓ Rust build complete"

clean: clean-rust clean-verif
	@echo "✓ All build artifacts cleaned"

clean-rust:
	@echo "Cleaning Rust artifacts..."
	@cargo clean

test: test-rust test-py test-ocaml

test-rust:
	@echo "Running Rust tests..."
	@cargo test
	@echo "✓ Rust tests passed"

test-py: dev
	@echo "Building Python FFI..."
	@maturin develop -q
	@echo "Running Python tests..."
	@python -m pytest src/ffi/python/test.py -v
	@echo "✓ Python tests passed"

dev: dev-rust
	@echo "✓ Development build complete"

dev-rust:
	@echo "Building Rust (debug)..."
	@cargo build

# The full gate. `verif-check` re-validates every Rocq module with rocqchk and
# `verif-obligations` fails if a `.v` grew an `Admitted` that is not declared in
# verification/OBLIGATIONS.md — a "proven" claim has to stay reproducible.
# Needs `nix develop` for rocq; use `check-rust` for the Rust-only subset.
check: check-rust verif-check verif-obligations
	@echo "✓ All checks passed"

# Every feature that changes what compiles. `ocaml-ffi` is not reachable from
# --all-targets, so it silently rotted until a `make test-ocaml` caught it.
check-rust:
	@cargo check --all-targets --locked
	@cargo check --all-targets --features python-ffi --locked
	@cargo check --features ocaml-ffi --locked
	@cargo check --all-targets --features trace --locked

check-deps:
	@echo "Checking build dependencies..."
	@command -v cargo >/dev/null 2>&1 || { echo "✗ cargo not found"; exit 1; }
	@command -v python >/dev/null 2>&1 || { echo "✗ python not found"; exit 1; }
	@command -v maturin >/dev/null 2>&1 || { echo "✗ maturin not found"; exit 1; }
	@command -v rocq >/dev/null 2>&1 || { echo "✗ rocq not found (run inside \`nix develop\`)"; exit 1; }
	@echo "✓ All dependencies available"

# ---- Rocq verification ----------------------------------------------------
# These targets delegate to verification/Makefile.  They are expected to be
# run inside `nix develop`, which provides Rocq 9.1.1 + Stdlib on PATH.

verif: verif-build

verif-build:
	@echo "Building Rocq verification (verification/)..."
	@$(MAKE) -C verification build
	@echo "✓ Rocq build complete (artifacts in verification/.build/)"

verif-check:
	@echo "Running rocqchk on the verification library..."
	@$(MAKE) -C verification check
	@echo "✓ Rocq modules validated"

verif-obligations:
	@$(MAKE) -C verification obligations

verif-clean:
	@$(MAKE) -C verification clean

clean-verif: verif-clean
	@echo "✓ Rocq artifacts cleaned"

# ---- OCaml FFI -------------------------------------------------------------
# The inductive type-algebra binding (ocaml/). Builds the engine as a static
# archive (with the ocaml-ffi exports), stages it alongside boxroot, and builds
# the dune library + demo.
#
# NOTE: Requires OCaml >= 5.0 for boxroot (multicore runtime symbols).
# On OCaml 4.x, only the library targets compile; executables will fail to link.

OCAML_MAJOR := $(shell ocaml -version 2>/dev/null | sed -n 's/.* \([0-9]\)\..*/\1/p')

ocaml:
	@echo "Building OCaml FFI..."
	@cargo build --features ocaml-ffi
	@cp target/debug/libaufbau.a ocaml/libaufbau.a
	@b="$$(find target/debug -name libocaml-boxroot.a | head -1)"; \
	  [ -z "$$b" ] || cp "$$b" ocaml/libocaml-boxroot.a
	@if [ "$(OCAML_MAJOR)" -lt 5 ]; then \
	  echo "⚠  OCaml $(shell ocaml -version 2>/dev/null) detected — boxroot needs >= 5.0."; \
	  echo "   Building library targets only (certify, spg, oracle, langs)."; \
	  cd ocaml && dune build certify.cma spg.cma oracle.cma lang_ml.cma lang_c.cma; \
	  echo "✓ OCaml libraries built (executables require OCaml >= 5.0)"; \
	else \
	  cd ocaml && dune build; \
	  echo "✓ OCaml FFI fully built"; \
	fi

ocaml-run: ocaml
	@if [ "$(OCAML_MAJOR)" -lt 5 ]; then \
	  echo "✗ Cannot run OCaml demo: executables require OCaml >= 5.0"; \
	  exit 1; \
	fi
	@dune exec --root ocaml ./demo.exe

test-ocaml: ocaml
	@if [ "$(OCAML_MAJOR)" -lt 5 ]; then \
	  echo "⚠  Skipping OCaml tests (need OCaml >= 5.0 for executable linking)"; \
	  echo "   Verified: certify.cma spg.cma oracle.cma lang_ml.cma lang_c.cma"; \
	else \
	  echo "Running OCaml FFI tests..."; \
	  dune runtest --root ocaml; \
	  echo "✓ OCaml tests passed"; \
	fi

# Build only the OCaml library targets (cert framework etc.), no executables.
# Works on OCaml 4.x (no boxroot linking).
test-ocaml-libs:
	@echo "Building OCaml certification libraries..."
	@cd ocaml && dune build certify.cma spg.cma oracle.cma lang_ml.cma lang_c.cma
	@echo "✓ OCaml certification libraries built"

help:
	@echo "Aufbau Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  all          - Build everything (default)"
	@echo "  build        - Build Rust components in release mode"
	@echo "  run          - Run aufbau binary (use ARGS='...' to pass arguments)"
	@echo "  dev          - Build all components in debug mode"
	@echo "  rust         - Build only Rust components"
	@echo "  test         - Run all tests (Rust + Python)"
	@echo "  test-rust    - Run only Rust tests"
	@echo "  test-py      - Run only Python FFI tests"
	@echo "  check        - Full gate: Rust + rocqchk + admitted-obligation drift"
	@echo "  check-rust   - Rust-only subset of check (no Rocq needed)"
	@echo "  verif           - Build the Rocq verification library"
	@echo "  verif-build     - Same as verif"
	@echo "  verif-check     - Re-validate compiled modules with rocqchk"
	@echo "  verif-clean     - Remove Rocq build artifacts"
	@echo "  verif-obligations - Fail if a .v grew an undeclared Admitted"
	@echo "  ocaml           - Build the OCaml FFI (needs OCaml >= 5.0)"
	@echo "  test-ocaml      - Run the OCaml differential certification"
	@echo "  clean           - Remove all build artifacts (Rust + Rocq)"
	@echo "  clean-rust   - Remove only Rust artifacts"
	@echo "  check-deps   - Verify all build tools are installed"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make              # Build everything"
	@echo "  make test         # Run all tests"
	@echo "  make run          # Run aufbau"
	@echo "  make run ARGS='--help'  # Run with arguments"
	@echo "  make dev          # Fast development build"
	@echo "  make check        # Verify compilation"
	@echo "  make verif        # Build Rocq verification (needs nix develop)"
	@echo "  make verif-check  # Re-validate with rocqchk"
	@echo "  make clean build  # Clean and rebuild"
