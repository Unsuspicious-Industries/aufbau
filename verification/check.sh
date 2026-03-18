#!/usr/bin/env bash
set -euo pipefail

VERIFICATION_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$VERIFICATION_DIR/.." && pwd)"
COQ_BUILD_DIR="$VERIFICATION_DIR/_build/default/coq"

usage() {
  cat <<'EOF'
Usage:
  verification/check.sh <language> <prefix>
  verification/check.sh --program <language> <program>
  verification/check.sh --skip-reverify --program <language> <program>

Arguments:
  <language>  One of: stlc, fun, imp
  <prefix>    Partial program prefix to complete with aufbau
  <program>   Already-complete program to typecheck with Coq

Options:
  --program         Treat input as a complete program
  --skip-reverify   Skip `dune build` (for batch callers that already verified)

Example:
  verification/check.sh fun "let x : Int ="
  verification/check.sh --program fun "let x : Int = 0 ; true"
EOF
}

DIRECT_PROGRAM=0
SKIP_REVERIFY=0

while [[ $# -gt 0 ]]; do
  case "$1" in
    --program)
      DIRECT_PROGRAM=1
      shift
      ;;
    --skip-reverify)
      SKIP_REVERIFY=1
      shift
      ;;
    -h|--help)
      usage
      exit 0
      ;;
    --)
      shift
      break
      ;;
    -* )
      echo "unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
    *)
      break
      ;;
  esac
done

if [[ $# -lt 2 ]]; then
  usage >&2
  exit 2
fi

LANGUAGE="$1"
shift
INPUT_TEXT="$*"

case "$LANGUAGE" in
  stlc)
    SPEC_PATH="$ROOT_DIR/examples/stlc.auf"
    COQ_IMPORT="verification.coq.STLC"
    COQ_CHECK_TERM="STLC.typecheck"
    ;;
  fun)
    SPEC_PATH="$ROOT_DIR/examples/fun.auf"
    COQ_IMPORT="verification.coq.Fun"
    COQ_CHECK_TERM="FunLang.typecheck"
    ;;
  imp)
    SPEC_PATH="$ROOT_DIR/examples/imp.auf"
    COQ_IMPORT="verification.coq.Imp"
    COQ_CHECK_TERM="ImpLang.typecheck_program"
    ;;
  *)
    echo "unknown language: $LANGUAGE" >&2
    usage >&2
    exit 2
    ;;
esac

if [[ -x "$ROOT_DIR/target/debug/aufbau" ]]; then
  AUFBAU_BIN="$ROOT_DIR/target/debug/aufbau"
elif [[ -x "$ROOT_DIR/target/release/aufbau" ]]; then
  AUFBAU_BIN="$ROOT_DIR/target/release/aufbau"
else
  AUFBAU_BIN="cargo run --quiet --"
fi

coq_reverify() {
  (cd "$VERIFICATION_DIR" && dune build coq/Common.vo coq/STLC.vo coq/Fun.vo coq/Imp.vo)
}

coq_escape_string() {
  PYTHONIOENCODING=UTF-8 python3 - "$1" <<'PY'
import sys
s = sys.argv[1]
s = s.replace("\\", "\\\\").replace('"', '\\"').replace("\n", "\\n")
sys.stdout.buffer.write((s + "\n").encode("utf-8"))
PY
}

run_aufbau_complete() {
  if [[ "$AUFBAU_BIN" == cargo* ]]; then
    printf '%s\n' "$INPUT_TEXT" | (cd "$ROOT_DIR" && cargo run --quiet -- complete -s "$SPEC_PATH")
  else
    printf '%s\n' "$INPUT_TEXT" | "$AUFBAU_BIN" complete -s "$SPEC_PATH"
  fi
}

run_coq_check() {
  local program="$1"
  local escaped
  local temp_file
  escaped="$(coq_escape_string "$program")"
  temp_file="$(mktemp /tmp/coq_check_XXXXXX.v)"

  cat > "$temp_file" <<EOF
Require Import Corelib.Strings.PrimStringAxioms.
Require Import $COQ_IMPORT.
Open Scope pstring_scope.
Eval vm_compute in ($COQ_CHECK_TERM "$escaped").
EOF

  coqtop -quiet -Q "$COQ_BUILD_DIR" verification.coq < "$temp_file"
  rm -f "$temp_file"
}

if [[ "$SKIP_REVERIFY" -eq 1 ]]; then
  echo "Skipping Coq re-verify (already verified for this batch)..." >&2
else
  echo "Re-verifying Coq modules..." >&2
  coq_reverify >&2
fi

if [[ "$DIRECT_PROGRAM" -eq 1 ]]; then
  echo "Checking provided complete program with Coq..." >&2
  COMPLETION="$INPUT_TEXT"
else
  echo "Completing prefix with aufbau..." >&2
  COMPLETION="$(run_aufbau_complete)"
fi

echo "Running verified Coq checker..." >&2
COQ_OUTPUT="$(run_coq_check "$COMPLETION")"

echo "language: $LANGUAGE"
if [[ "$DIRECT_PROGRAM" -eq 1 ]]; then
  echo "program: $INPUT_TEXT"
else
  echo "prefix: $INPUT_TEXT"
fi
echo "completion: $COMPLETION"
echo "coq-result:"
echo "$COQ_OUTPUT"

if grep -q "= Some" <<<"$COQ_OUTPUT"; then
  echo "status: accepted by Coq verifier"
  exit 0
fi

echo "status: rejected by Coq verifier" >&2
exit 1
