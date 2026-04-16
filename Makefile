.PHONY: all build clean rust ocaml coq verification help test run

ARGS ?=

# Default target builds everything
all: build

# Main build target - builds all components in correct order
build: rust verification
	@echo "✓ All components built successfully"

# Run the aufbau binary (equivalent to cargo run)
run: build
	@./target/release/aufbau $(ARGS)

# Build Rust library (both for standalone and OCaml FFI)
rust:
	@echo "Building Rust library..."
	@cargo build --release
	@echo "✓ Rust build complete"

# Build Rust library with OCaml FFI support
rust-ffi:
	@echo "Building Rust library with OCaml FFI..."
	@cargo build --release --features ocaml-ffi
	@echo "✓ Rust FFI build complete"

# Build OCaml and Coq verification components
verification: coq ocaml
	@echo "✓ Verification components built"

# Build Coq proofs
coq:
	@echo "Building Coq proofs..."
	@cd verification && dune build @coq
	@echo "✓ Coq proofs compiled"

# Build OCaml orchestrator (depends on Rust with OCaml FFI)
ocaml: rust-ffi
	@echo "Building OCaml orchestrator..."
	@cd verification && \
	 AUFBAU_ROOT=$(PWD) AUFBAU_VERIFICATION_DIR=$(PWD)/verification \
	 dune build orchestrator.exe
	@echo "✓ OCaml orchestrator built"

# Clean all build artifacts
clean: clean-rust clean-ocaml clean-coq
	@echo "✓ All build artifacts cleaned"

clean-rust:
	@echo "Cleaning Rust artifacts..."
	@cargo clean

clean-ocaml:
	@echo "Cleaning OCaml artifacts..."
	@cd verification && dune clean 2>/dev/null || true

clean-coq:
	@echo "Cleaning Coq artifacts..."
	@find verification/coq -name "*.vo" -delete
	@find verification/coq -name "*.vok" -delete
	@find verification/coq -name "*.vos" -delete
	@find verification/coq -name "*.glob" -delete
	@find verification/coq -name ".*.aux" -delete

# Run tests
test: build
	@echo "Running Rust tests..."
	@cargo test --release
	@echo "✓ Tests passed"

# Development build (faster, debug mode)
dev: dev-rust dev-verification
	@echo "✓ Development build complete"

dev-rust:
	@echo "Building Rust (debug)..."
	@cargo build

dev-rust-ffi:
	@echo "Building Rust with OCaml FFI (debug)..."
	@cargo build --features ocaml-ffi

dev-verification: dev-coq dev-ocaml

dev-coq:
	@echo "Building Coq proofs (debug)..."
	@cd verification && dune build @coq

dev-ocaml: dev-rust-ffi
	@echo "Building OCaml orchestrator (debug)..."
	@cd verification && \
	 AUFBAU_ROOT=$(PWD) AUFBAU_VERIFICATION_DIR=$(PWD)/verification \
	 dune build orchestrator.exe

# Check if all tools are available
check-deps:
	@echo "Checking build dependencies..."
	@command -v cargo >/dev/null 2>&1 || { echo "✗ cargo not found"; exit 1; }
	@command -v dune >/dev/null 2>&1 || { echo "✗ dune not found"; exit 1; }
	@command -v coqc >/dev/null 2>&1 || { echo "✗ coqc not found"; exit 1; }
	@command -v ocamlc >/dev/null 2>&1 || { echo "✗ ocamlc not found"; exit 1; }
	@echo "✓ All dependencies available"

# Help target
help:
	@echo "Aufbau Build System"
	@echo ""
	@echo "Available targets:"
	@echo "  all          - Build everything (default)"
	@echo "  build        - Build all components in release mode"
	@echo "  run          - Run aufbau binary (use ARGS='...' to pass arguments)"
	@echo "  dev          - Build all components in debug mode (faster)"
	@echo "  rust         - Build only Rust components"
	@echo "  ocaml        - Build only OCaml components"
	@echo "  coq          - Build only Coq proofs"
	@echo "  verification - Build OCaml and Coq components"
	@echo "  test         - Run all tests"
	@echo "  clean        - Remove all build artifacts"
	@echo "  clean-rust   - Remove only Rust artifacts"
	@echo "  clean-ocaml  - Remove only OCaml artifacts"
	@echo "  clean-coq    - Remove only Coq artifacts"
	@echo "  check-deps   - Verify all build tools are installed"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make              # Build everything"
	@echo "  make run          # Run aufbau"
	@echo "  make run ARGS='--help'  # Run with arguments"
	@echo "  make dev          # Fast development build"
	@echo "  make clean build  # Clean and rebuild"
	@echo "  make coq          # Build only Coq proofs"
