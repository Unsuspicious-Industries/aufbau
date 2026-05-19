#!/usr/bin/env bash
# Collect benchmark data and run formal property tests.
# Output goes to data/ (CSV) and target/criterion/ (Criterion HTML).
#
# Usage:
#   ./scripts/collect.sh             # full run
#   ./scripts/collect.sh --fast      # skip criterion benches

set -euo pipefail

FAST=0
for arg in "$@"; do
  [[ "$arg" == "--fast" ]] && FAST=1
done

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
cd "$ROOT"

bold() { printf '\033[1m%s\033[0m\n' "$*"; }
ok()   { printf '\033[32m✓\033[0m %s\n' "$*"; }
run()  { printf '\033[36m$ %s\033[0m\n' "$*"; "$@"; }

bold "=== aufbau data collection ==="
echo

# 1. Build
bold "1. Release build"
run cargo build --release --quiet
ok "build done"
echo

# 2. Property tests (formal verification approximations)
bold "2. Property tests"
run cargo test --release --lib validation::properties -- --nocapture 2>&1 \
  | grep -E "^test |^test result|FAILED|ok$" || true
ok "property tests done"
echo

# 3. Full validation suites (parseable)
bold "3. Validation suites  (informational)"
set +e
cargo test --release --lib validation::parseable \
  -- --nocapture 2>&1 \
  | grep --line-buffered -E "test result|FAILED" ; :
set -e
echo

# 4. Complexity / regression bounds
bold "4. Complexity bounds"
run cargo test --release --lib complexity -- --nocapture 2>&1 \
  | grep -E "^test |^test result" || true
ok "complexity tests done"
echo

# 5. Chart growth data (CSV for paper)
bold "5. Chart growth data"
run cargo run --release -- chart
ok "data written to data/chart_growth.csv"
echo

# 6. Criterion benchmarks (optional, slow)
if [[ "$FAST" -eq 0 ]]; then
  bold "6. Criterion benchmarks  (skip with --fast)"
  run cargo bench --bench chart_growth -- --warm-up-time 2 --measurement-time 6
  ok "HTML reports in target/criterion/"
else
  bold "6. Criterion benchmarks  [skipped -- --fast]"
fi
echo

bold "=== done ==="
echo "  CSV data : $ROOT/data/chart_growth.csv"
if [[ "$FAST" -eq 0 ]]; then
  echo "  Criterion: $ROOT/target/criterion/"
fi
