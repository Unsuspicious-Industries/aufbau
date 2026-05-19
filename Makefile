.PHONY: all build clean rust help test test-rust test-py dev check-deps check run

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

clean: clean-rust
	@echo "✓ All build artifacts cleaned"

clean-rust:
	@echo "Cleaning Rust artifacts..."
	@cargo clean

test: test-rust test-py

test-rust:
	@echo "Running Rust tests..."
	@cargo test
	@echo "✓ Rust tests passed"

test-py: dev
	@echo "Building Python FFI..."
	@maturin develop -q
	@echo "Running Python tests..."
	@python -m pytest src/ffi/test.py -v
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
	@echo "✓ All dependencies available"

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
	@echo "  clean        - Remove all build artifacts"
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
	@echo "  make clean build  # Clean and rebuild"
