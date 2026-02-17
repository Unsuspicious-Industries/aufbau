#!/usr/bin/env bash
set -euo pipefail

# Build and run the `aufbau validate` runner per-module, mirroring the CI workflow.

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

echo "Repository root: $repo_root"

export CARGO_TERM_COLOR=always

echo "Building aufbau (release)..."
cargo build --bin aufbau --release --verbose

aufbau_bin="$repo_root/target/release/aufbau"
if [ ! -x "$aufbau_bin" ]; then
  echo "Error: built binary not found at $aufbau_bin" >&2
  exit 2
fi

reports_dir="$repo_root/validation/reports"
mkdir -p "$reports_dir"

# Always run `parseable` first, then the completable test suites. If callers
# pass explicit modules, preserve their order but avoid duplicating
# `parseable`.
modules=("parseable")
if [ "$#" -gt 0 ]; then
  # Use provided modules from args (preserve order, skip duplicate parseable)
  for arg in "$@"; do
    [ "$arg" = "parseable" ] && continue
    modules+=("$arg")
  done
else
  # auto-detect completable modules from src/validation/completable/*.rs
  for f in "$repo_root"/src/validation/completable/*.rs; do
    [ -f "$f" ] || continue
    name="$(basename "$f" .rs)"
    [ "$name" = "mod" ] && continue
    modules+=("$name")
  done
fi

if [ ${#modules[@]} -eq 0 ]; then
  echo "No modules found to test." >&2
  exit 3
fi

echo "Modules: ${modules[*]}"

failures=0
for m in "${modules[@]}"; do
  echo
  echo "========================================"
  echo "Running validation for module: $m"
  echo "========================================"
  if [ "$m" = "parseable" ]; then
    if ! "$aufbau_bin" validate -m parseable; then
      echo "Module $m: FAILED" >&2
      failures=$((failures + 1))
    else
      echo "Module $m: OK"
    fi
  else
    if ! "$aufbau_bin" validate --filter "$m"; then
      echo "Module $m: FAILED" >&2
      failures=$((failures + 1))
    else
      echo "Module $m: OK"
    fi
  fi
done

echo
echo "Reports directory: $reports_dir"
ls -la "$reports_dir" || true

if [ "$failures" -ne 0 ]; then
  echo
  echo "Validation finished: $failures module(s) failed." >&2
  exit 1
else
  echo
  echo "Validation finished: all modules passed."
  exit 0
fi
