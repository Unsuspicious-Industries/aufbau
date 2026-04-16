#!/usr/bin/env bash
set -euo pipefail

VERIFICATION_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$VERIFICATION_DIR/.." && pwd)"

export AUFBAU_ROOT="$ROOT_DIR"
export AUFBAU_VERIFICATION_DIR="$VERIFICATION_DIR"

dune build --root "$VERIFICATION_DIR" orchestrator.exe
exec "$VERIFICATION_DIR/_build/default/orchestrator.exe" "$@"
