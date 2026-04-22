.PHONY: all build clean rust help test run dev check-deps

ARGS ?=

# Default target builds everything
all: build

# Main build target
build: rust
	@echo "✓ Build complete"

# Run the aufbau binary (equivalent to cargo run)
run: build
	@./target/release/aufbau $(ARGS)

# Build Rust library
rust:
	@echo "Building Rust library..."
	@cargo build --release
	@echo "✓ Rust build complete"

# Clean all build artifacts
clean: clean-rust
	@echo "✓ All build artifacts cleaned"

clean-rust:
	@echo "Cleaning Rust artifacts..."
	@cargo clean

# Run tests
test: build
	@echo "Running Rust tests..."
	@cargo test --release
	@echo "✓ Tests passed"

# Development build (faster, debug mode)
dev: dev-rust
	@echo "✓ Development build complete"

dev-rust:
	@echo "Building Rust (debug)..."
	@cargo build

# Check if all tools are available
check-deps:
	@echo "Checking build dependencies..."
	@command -v cargo >/dev/null 2>&1 || { echo "✗ cargo not found"; exit 1; }
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
	@echo "  test         - Run all tests"
	@echo "  clean        - Remove all build artifacts"
	@echo "  clean-rust   - Remove only Rust artifacts"
	@echo "  check-deps   - Verify all build tools are installed"
	@echo "  help         - Show this help message"
	@echo ""
	@echo "Examples:"
	@echo "  make              # Build everything"
	@echo "  make run          # Run aufbau"
	@echo "  make run ARGS='--help'  # Run with arguments"
	@echo "  make dev          # Fast development build"
	@echo "  make clean build  # Clean and rebuild"
