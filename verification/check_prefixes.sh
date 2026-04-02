#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
VERIFICATION_DIR="$ROOT_DIR/verification"
COQ_DUNE_BUILD_DIR="$VERIFICATION_DIR/_build/default/coq"
COQ_SOURCE_DIR="$VERIFICATION_DIR/coq"
COQ_BUILD_DIR="$COQ_DUNE_BUILD_DIR"
DEFAULT_JOBS="$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)"
PREFIX_FILE="$VERIFICATION_DIR/prefixes.txt"
K=3
JOBS="${JOBS:-$DEFAULT_JOBS}"
DEPTH=""
STATES=""
CHILDREN=""
EXAMPLES=""

if [[ -t 1 && -z "${NO_COLOR:-}" ]]; then
  C_INFO='\033[36m'
  C_OK='\033[32m'
  C_ERR='\033[31m'
  C_RESET='\033[0m'
else
  C_INFO=''
  C_OK=''
  C_ERR=''
  C_RESET=''
fi

log_info() {
  printf '%b[info]%b %s\n' "$C_INFO" "$C_RESET" "$1"
}

log_ok() {
  printf '%b[ok]%b %s\n' "$C_OK" "$C_RESET" "$1"
}

current_rss_mb() {
  awk '/VmRSS:/ { printf "%.1f", $2 / 1024.0 }' /proc/$$/status 2>/dev/null || printf '0.0'
}

log_err() {
  printf '%b[err]%b %s\n' "$C_ERR" "$C_RESET" "$1" >&2
}

usage() {
  cat <<'EOF'
Usage:
  verification/check_prefixes.sh [options]
  verification/check_prefixes.sh [prefix-file] [k] [jobs]

Prefix file format:
  language|prefix

Options:
  -f, --prefix-file FILE   Prefix file path
  -k, --count N            Number of completions per prefix
  -j, --jobs N             Parallel Coq checks
      --depth N            complete-k max depth
      --states N           complete-k max states
      --children N         complete-k max children per state
      --examples N         complete-k max regex examples
  -h, --help               Show this help

Example:
  verification/check_prefixes.sh -f verification/prefixes.txt -k 3 -j 8 --depth 12 --states 256
EOF
}

parse_args() {
  need_value() {
    local opt="$1"
    local val="${2:-}"
    if [[ -z "$val" ]]; then
      log_err "missing value for $opt"
      usage >&2
      exit 2
    fi
  }

  local positional=()
  while [[ $# -gt 0 ]]; do
    case "$1" in
      -f|--prefix-file)
        need_value "$1" "${2:-}"
        PREFIX_FILE="$2"
        shift 2
        ;;
      -k|--count)
        need_value "$1" "${2:-}"
        K="$2"
        shift 2
        ;;
      -j|--jobs)
        need_value "$1" "${2:-}"
        JOBS="$2"
        shift 2
        ;;
      --depth)
        need_value "$1" "${2:-}"
        DEPTH="$2"
        shift 2
        ;;
      --states)
        need_value "$1" "${2:-}"
        STATES="$2"
        shift 2
        ;;
      --children)
        need_value "$1" "${2:-}"
        CHILDREN="$2"
        shift 2
        ;;
      --examples)
        need_value "$1" "${2:-}"
        EXAMPLES="$2"
        shift 2
        ;;
      -h|--help)
        usage
        exit 0
        ;;
      --)
        shift
        break
        ;;
      -*)
        log_err "unknown option: $1"
        usage >&2
        exit 2
        ;;
      *)
        positional+=("$1")
        shift
        ;;
    esac
  done

  if [[ "${#positional[@]}" -ge 1 ]]; then
    PREFIX_FILE="${positional[0]}"
  fi
  if [[ "${#positional[@]}" -ge 2 ]]; then
    K="${positional[1]}"
  fi
  if [[ "${#positional[@]}" -ge 3 ]]; then
    JOBS="${positional[2]}"
  fi
  if [[ "${#positional[@]}" -gt 3 ]]; then
    log_err "too many positional arguments"
    usage >&2
    exit 2
  fi
}

parse_args "$@"

if [[ ! -f "$PREFIX_FILE" ]]; then
  echo "error: prefix file not found: $PREFIX_FILE" >&2
  usage >&2
  exit 2
fi

if [[ -x "$ROOT_DIR/target/debug/aufbau" ]]; then
  AUFBAU_BIN="$ROOT_DIR/target/debug/aufbau"
elif [[ -x "$ROOT_DIR/target/release/aufbau" ]]; then
  AUFBAU_BIN="$ROOT_DIR/target/release/aufbau"
else
  AUFBAU_BIN="cargo run --quiet --"
fi

spec_for_language() {
  case "$1" in
    stlc) printf '%s\n' "$ROOT_DIR/examples/stlc.auf" ;;
    fun) printf '%s\n' "$ROOT_DIR/examples/fun.auf" ;;
    imp) printf '%s\n' "$ROOT_DIR/examples/imp.auf" ;;
    typescript) printf '%s\n' "$ROOT_DIR/examples/typescript.auf" ;;
    *)
      echo "error: unknown language '$1'" >&2
      return 1
      ;;
  esac
}

language_supported() {
  case "$1" in
    stlc|fun|imp|typescript) return 0 ;;
    *) return 1 ;;
  esac
}

run_complete_k() {
  local spec_path="$1"
  local prefix="$2"
  local -a cmd=(complete-k -s "$spec_path" -k "$K")

  if [[ -n "$DEPTH" ]]; then
    cmd+=(--depth "$DEPTH")
  fi
  if [[ -n "$STATES" ]]; then
    cmd+=(--states "$STATES")
  fi
  if [[ -n "$CHILDREN" ]]; then
    cmd+=(--children "$CHILDREN")
  fi
  if [[ -n "$EXAMPLES" ]]; then
    cmd+=(--examples "$EXAMPLES")
  fi

  if [[ "$AUFBAU_BIN" == cargo* ]]; then
    printf '%s\n' "$prefix" | (cd "$ROOT_DIR" && cargo run --quiet -- "${cmd[@]}")
  else
    printf '%s\n' "$prefix" | "$AUFBAU_BIN" "${cmd[@]}"
  fi
}

coq_reverify_once() {
  if command -v dune >/dev/null 2>&1; then
    (cd "$VERIFICATION_DIR" && dune build coq/Common.vo coq/STLC.vo coq/Fun.vo coq/Imp.vo coq/Typescript.vo)
    COQ_BUILD_DIR="$COQ_DUNE_BUILD_DIR"
  else
    (cd "$COQ_SOURCE_DIR" && \
      coqc -Q . verification.coq -noglob Common.v && \
      coqc -Q . verification.coq -noglob STLC.v && \
      coqc -Q . verification.coq -noglob Fun.v && \
      coqc -Q . verification.coq -noglob Imp.v && \
      coqc -Q . verification.coq -noglob Typescript.v)
    COQ_BUILD_DIR="$COQ_SOURCE_DIR"
  fi
}

resolve_coq_build_dir() {
  if [[ -f "$COQ_DUNE_BUILD_DIR/Common.vo" ]]; then
    COQ_BUILD_DIR="$COQ_DUNE_BUILD_DIR"
  elif [[ -f "$COQ_SOURCE_DIR/Common.vo" ]]; then
    COQ_BUILD_DIR="$COQ_SOURCE_DIR"
  elif command -v dune >/dev/null 2>&1; then
    COQ_BUILD_DIR="$COQ_DUNE_BUILD_DIR"
  else
    COQ_BUILD_DIR="$COQ_SOURCE_DIR"
  fi
}

if [[ ! "$JOBS" =~ ^[1-9][0-9]*$ ]]; then
  log_err "invalid jobs value: $JOBS"
  usage >&2
  exit 2
fi

if [[ ! "$K" =~ ^[1-9][0-9]*$ ]]; then
  log_err "invalid count value: $K"
  usage >&2
  exit 2
fi

for num_arg in "$DEPTH" "$STATES" "$CHILDREN" "$EXAMPLES"; do
  if [[ -n "$num_arg" && ! "$num_arg" =~ ^[1-9][0-9]*$ ]]; then
    log_err "search parameters must be positive integers"
    usage >&2
    exit 2
  fi
done

TMP_DIR="$(mktemp -d /tmp/check_prefixes_jobs_XXXXXX)"
cleanup() {
  rm -rf "$TMP_DIR"
}
trap cleanup EXIT

declare -a JOB_PIDS=()
declare -a JOB_LOGS=()
job_count=0

run_start_ts="$(date +%s)"
queued_prefixes=0
completed_prefixes=0
completed_checks=0

format_duration() {
  local total="$1"
  local h m s
  h=$((total / 3600))
  m=$(((total % 3600) / 60))
  s=$((total % 60))
  printf '%02d:%02d:%02d' "$h" "$m" "$s"
}

count_total_prefix_entries() {
  local count=0
  local raw_line line language
  while IFS= read -r raw_line || [[ -n "$raw_line" ]]; do
    line="${raw_line#"${raw_line%%[![:space:]]*}"}"
    if [[ -z "$line" || "${line:0:1}" == "#" ]]; then
      continue
    fi
    if [[ "$line" != *"|"* ]]; then
      continue
    fi
    language="${line%%|*}"
    if language_supported "$language"; then
      count=$((count + 1))
    fi
  done < "$PREFIX_FILE"
  printf '%s\n' "$count"
}

print_job_log() {
  local log_file="$1"
  local parse_error=0
  local generated_zero=0
  local job_checks=0
  local job_unsound=0
  local line

  while IFS= read -r line || [[ -n "$line" ]]; do
    if [[ "$line" == __JOB_PARSE_ERROR__:* ]]; then
      parse_error=1
      continue
    fi
    if [[ "$line" == __JOB_ZERO_COMPLETIONS__:* ]]; then
      generated_zero=1
      continue
    fi
    if [[ "$line" == __JOB_STATS__:* ]]; then
      local payload
      payload="${line#__JOB_STATS__:}"
      job_checks="${payload%%:*}"
      job_unsound="${payload#*:}"
      continue
    fi
    printf '%s\n' "$line"
  done < "$log_file"

  completed_prefixes=$((completed_prefixes + 1))

  if [[ "$parse_error" -eq 1 || "$generated_zero" -eq 1 ]]; then
    completion_errors=$((completion_errors + 1))
  fi

  completed_checks=$((completed_checks + job_checks))
  unsound_rejections=$((unsound_rejections + job_unsound))
}

report_progress() {
  local now elapsed eta_text active
  now="$(date +%s)"
  elapsed=$((now - run_start_ts))
  active="${#JOB_PIDS[@]}"

  if [[ "$total_prefixes" -eq 0 ]]; then
    eta_text="00:00:00"
  elif [[ "$completed_prefixes" -gt 0 && "$total_prefixes" -gt "$completed_prefixes" ]]; then
    local remaining eta
    remaining=$((total_prefixes - completed_prefixes))
    eta=$((elapsed * remaining / completed_prefixes))
    eta_text="$(format_duration "$eta")"
  else
    eta_text="--:--:--"
  fi

  log_info "prefixes ${completed_prefixes}/${total_prefixes} | checks $completed_checks | active $active | eta $eta_text"
}

wait_for_slot() {
  while [[ "${#JOB_PIDS[@]}" -ge "$JOBS" ]]; do
    wait -n || true
    compact_running_jobs
  done
}

compact_running_jobs() {
  local -a pids=()
  local -a logs=()
  local i pid log_file
  for i in "${!JOB_PIDS[@]}"; do
    pid="${JOB_PIDS[$i]}"
    log_file="${JOB_LOGS[$i]}"
    if kill -0 "$pid" 2>/dev/null; then
      pids+=("$pid")
      logs+=("$log_file")
    else
      wait "$pid" || true
      print_job_log "$log_file"
    fi
  done
  JOB_PIDS=("${pids[@]}")
  JOB_LOGS=("${logs[@]}")
  report_progress
}

queue_check_job() {
  local language="$1"
  local prefix="$2"
  local spec_path="$3"
  local log_file
  local completion_file
  local result_file

  job_count=$((job_count + 1))
  log_file="$TMP_DIR/job_${job_count}.log"
  completion_file="$TMP_DIR/job_${job_count}.programs"
  result_file="$TMP_DIR/job_${job_count}.results"

  (
    echo "============================================================"
    log_info "language: $language"
    log_info "prefix: $prefix"

    if ! completions="$(run_complete_k "$spec_path" "$prefix")"; then
      log_err "failed to generate completions"
      echo "__JOB_PARSE_ERROR__:1"
      echo "__JOB_STATS__:0:0"
      exit 0
    fi

    local completion_count=0
    local completion_failures=0
    : > "$completion_file"
    while IFS= read -r completion || [[ -n "$completion" ]]; do
      [[ -z "$completion" ]] && continue
      completion_count=$((completion_count + 1))
      log_info "completion[$completion_count]: $completion"
      printf '%s\n' "$completion" >> "$completion_file"
    done <<< "$completions"

    if [[ "$completion_count" -eq 0 ]]; then
      log_err "no completions returned"
      echo "__JOB_ZERO_COMPLETIONS__:1"
      echo "__JOB_STATS__:0:0"
      exit 0
    fi

    if ! python3 "$VERIFICATION_DIR/coq_batch_check.py" \
      "$language" "$completion_file" --coq-build-dir "$COQ_BUILD_DIR" > "$result_file"; then
      log_err "batch Coq verification failed"
      completion_failures=$completion_count
      echo "__JOB_PARSE_ERROR__:1"
    else
      while IFS='|' read -r idx status summary || [[ -n "$idx" ]]; do
        [[ -z "$idx" ]] && continue
      if [[ "$status" == "ok" ]]; then
          printf 'coq-result[%s]: %s\n' "$idx" "$summary"
          printf 'status[%s]: accepted by Coq verifier\n' "$idx"
        else
          printf 'coq-result[%s]: %s\n' "$idx" "$summary"
          printf 'status[%s]: rejected by Coq verifier\n' "$idx"
          completion_failures=$((completion_failures + 1))
        fi
      done < "$result_file"
    fi

    echo "__JOB_STATS__:${completion_count}:${completion_failures}"
    echo "__JOB_MEMORY_MB__:$(current_rss_mb)"
  ) >"$log_file" 2>&1 &

  JOB_PIDS+=("$!")
  JOB_LOGS+=("$log_file")
}

unsound_rejections=0
completion_errors=0
input_errors=0
total_prefixes="$(count_total_prefix_entries)"

log_info "re-verifying Coq modules once for this run"
coq_reverify_once
resolve_coq_build_dir
log_info "using $JOBS parallel jobs"
log_info "search params: count=$K depth=${DEPTH:-default} states=${STATES:-default} children=${CHILDREN:-default} examples=${EXAMPLES:-default}"
log_info "total prefixes to process: $total_prefixes"

while IFS= read -r raw_line || [[ -n "$raw_line" ]]; do
  line="${raw_line#"${raw_line%%[![:space:]]*}"}"
  if [[ -z "$line" || "${line:0:1}" == "#" ]]; then
    continue
  fi

  if [[ "$line" != *"|"* ]]; then
    echo "error: invalid prefix entry '$line' (expected language|prefix)" >&2
    input_errors=$((input_errors + 1))
    continue
  fi

  language="${line%%|*}"
  prefix="${line#*|}"
  if ! language_supported "$language"; then
    log_err "unknown language '$language' in prefix file"
    input_errors=$((input_errors + 1))
    continue
  fi

  spec_path="$(spec_for_language "$language")"

  queued_prefixes=$((queued_prefixes + 1))
  log_info "queued prefix ${queued_prefixes}/${total_prefixes}: $language"
  wait_for_slot
  queue_check_job "$language" "$prefix" "$spec_path"
  report_progress
done < "$PREFIX_FILE"

while [[ "${#JOB_PIDS[@]}" -gt 0 ]]; do
  wait -n || true
  compact_running_jobs
done

total_failures=$((unsound_rejections + completion_errors + input_errors))

echo "============================================================"
log_info "queued prefixes: $queued_prefixes"
log_info "unsoundness (Coq rejected): $unsound_rejections"
log_info "completion errors (no completion): $completion_errors"
log_info "input errors (bad prefix rows): $input_errors"

if [[ "$total_failures" -eq 0 ]]; then
  log_ok "all prefix completions accepted by the Coq verifier"
  exit 0
fi

if [[ "$unsound_rejections" -gt 0 ]]; then
  log_err "unsoundness detected: $unsound_rejections"
fi
if [[ "$completion_errors" -gt 0 ]]; then
  log_err "completion errors detected: $completion_errors"
fi
if [[ "$input_errors" -gt 0 ]]; then
  log_err "input errors detected: $input_errors"
fi
log_err "total failures: $total_failures"
exit 1
