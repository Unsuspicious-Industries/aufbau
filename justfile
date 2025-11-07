# Run tests with nextest
test:
    cargo nextest run

# Watch and check on file changes
watch:
    cargo watch -x check -x test

# Benchmark the type search algorithm
bench:
    cargo criterion

# Profile with flamegraph
profile:
    cargo flamegraph --dev

# Build docs and open
doc:
    cargo doc --open

# Format all code
fmt:
    cargo fmt

# Lint with clippy
lint:
    cargo clippy -- -D warnings

# Run all checks
ci: fmt lint test

# Build release optimized binary
release:
    cargo build --release

# Generate proof tree visualization (placeholder)
viz:
    python scripts/visualize_proofs.py

# Experiment with Coq translations
coq-test:
    coqc experiments/simple_types.v
