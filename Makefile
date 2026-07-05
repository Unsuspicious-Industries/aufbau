.PHONY: all build clean rust help test test-rust test-py dev check-deps check lc run \
        verif verif-build verif-check verif-clean clean-verif \
        verif-ocaml verif-ocaml-test ocaml ocaml-run test-ocaml

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

check:
	@cargo check --all-targets --locked
	@cargo check --all-targets --features python-ffi --locked

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

verif-clean:
	@$(MAKE) -C verification clean

clean-verif: verif-clean
	@echo "✓ Rocq artifacts cleaned"

# ---- OCaml extraction & tests ----------------------------------------------
# These run the Rocq build (which includes extraction.v), then build & test
# the extracted OCaml via dune.  Must be run inside `nix develop`.

verif-ocaml:
	@echo "Extracting Rocq -> OCaml..."
	@$(MAKE) -C verification ocaml-build
	@echo "✓ OCaml extraction + build complete"

verif-ocaml-test: verif-ocaml
	@$(MAKE) -C verification ocaml-test
	@echo "✓ OCaml tests passed"

# ---- OCaml FFI -------------------------------------------------------------
# The inductive type-algebra binding (ocaml/). Builds the engine as a static
# archive (with the ocaml-ffi exports), stages it alongside boxroot, and builds
# the dune library + demo.

ocaml:
	@echo "Building OCaml FFI..."
	@cargo build --features ocaml-ffi
	@cp target/debug/libaufbau.a ocaml/libaufbau.a
	@b="$$(find target/debug -name libocaml-boxroot.a | head -1)"; \
	  [ -z "$$b" ] || cp "$$b" ocaml/libocaml-boxroot.a
	@dune build --root ocaml
	@echo "✓ OCaml FFI built"

ocaml-run: ocaml
	@dune exec --root ocaml ./demo.exe

test-ocaml: ocaml
	@echo "Running OCaml FFI tests..."
	@dune runtest --root ocaml
	@echo "✓ OCaml tests passed"

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
	@echo "  check        - Check all targets compile (including python-ffi)"
	@echo "  verif           - Build the Rocq verification library"
	@echo "  verif-build     - Same as verif"
	@echo "  verif-check     - Re-validate compiled modules with rocqchk"
	@echo "  verif-clean     - Remove Rocq build artifacts"
	@echo "  verif-ocaml     - Extract Rocq -> OCaml and build"
	@echo "  verif-ocaml-test- Extract, build, and run OCaml tests"
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
