#!/usr/bin/env bash
# ── lc.sh (line-count enforcer) ────────────────────────────────────────────────
# Enforces line-count budgets defined in STYLE.md §1.
# Exits 0 if all budgets pass, 1 if any fails.
#
# Usage:
#   scripts/lc.sh                 Show all budgets with current counts
#   scripts/lc.sh --check         CI mode: exit 1 if any budget is exceeded
#   scripts/lc.sh --json          Output machine-readable JSON to stdout
#   scripts/lc.sh --per-fn        Also check per-function budget (≤ 40 lines)

set -euo pipefail
cd "$(dirname "$0")/.."

# ── Terminal colours ──────────────────────────────────────────────────────────
if [[ -t 1 ]]; then
    BOLD='\033[1m'
    DIM='\033[2m'
    GREEN='\033[32m'
    RED='\033[31m'
    CYAN='\033[36m'
    RESET='\033[0m'
else
    BOLD='' DIM='' GREEN='' RED='' CYAN='' RESET=''
fi

CHECK_MODE=false
JSON_MODE=false
PER_FN=false

# Read budgets BEFORE argument parsing so $1 doesn't shadow the path.
STYLE_TOML="${PWD}/style.toml"

for arg in "$@"; do
    case "$arg" in
        --check)  CHECK_MODE=true ;;
        --json)   JSON_MODE=true ;;
        --per-fn) PER_FN=true ;;
        *) echo "Unknown flag: $arg"; exit 1 ;;
    esac
done

# ── Budget definitions ─────────────────────────────────────────────────────────
# Read from ./style.toml (canonical source).  Fallback defaults on parse failure.

parse_toml_budgets() {
    local toml="$1"
    if [[ ! -f "$toml" ]]; then
        echo "lc: $toml not found" >&2
        return 1
    fi
    # Strip comments, then grab [budgets] section until next [section] or EOF
    awk '
    BEGIN { in_section = 0 }
    /^\[budgets\]/  { in_section = 1; next }
    /^\[/           { if (in_section) exit }
    in_section {
        sub(/#.*$/, "")                          # strip trailing comments
        if ($0 ~ /^[[:space:]]*$/) next          # skip blank lines
        if (match($0, /^[[:space:]]*"([^"]+)"[[:space:]]*=[[:space:]]*([0-9]+)/, m)) {
            print m[1] "\t" m[2]
        }
    }
    ' "$toml"
}

parse_toml_limits() {
    local toml="$1"
    awk '
    BEGIN { in_section = 0 }
    /^\[limits\]/   { in_section = 1; next }
    /^\[/           { if (in_section) exit }
    in_section {
        sub(/#.*$/, "")
        if ($0 ~ /^[[:space:]]*$/) next
        if (match($0, /^[[:space:]]*([a-z-]+)[[:space:]]*=[[:space:]]*([0-9]+)/, m)) {
            print m[1] "\t" m[2]
        }
    }
    ' "$toml"
}

declare -A BUDGETS
PER_FILE_BUDGET=400
PER_FN_BUDGET=40

if [[ -f "$STYLE_TOML" ]]; then
    while IFS=$'\t' read -r path budget; do
        [[ -n "$path" ]] || continue
        BUDGETS["$path"]="$budget"
    done < <(parse_toml_budgets "$STYLE_TOML")
    while IFS=$'\t' read -r key val; do
        [[ -n "$key" ]] || continue
        case "$key" in
            per-file) PER_FILE_BUDGET="$val" ;;
            per-function) PER_FN_BUDGET="$val" ;;
        esac
    done < <(parse_toml_limits "$STYLE_TOML")
fi

# ── Awk utility: strip test code and count non-blank lines ─────────────────────
# Strips:
#   · #[cfg(test)] mod tests { … }            (whole module block)
#   · #[test] fn test_foo() { … }             (whole test function)
#   · #[cfg(test)] on individual statements    (single-line guards)
#
# Reads stdin, prints line count to stdout.

awk_strip_tests='
BEGIN {
    depth = 0
    skip_depth = -1          # inside #[cfg(test)] or #[test] block when >= 0
    pending_cfg_test = 0     # saw #[cfg(test)], waiting for next brace-open
    in_block_comment = 0     # inside /* ... */ block comment
    total = 0
}

# ── Handle block-comment exit from previous line ────────────────────────────
{
    if (in_block_comment) {
        apply_braces($0)
        if (match($0, /\*\//)) {
            rest = substr($0, RSTART + RLENGTH)
            in_block_comment = 0
            # There might be code after the */ on this line
            if (rest ~ /[^[:space:]]/ && !(rest ~ /^[[:space:]]*\/\//)) {
                total++
            }
        }
        next
    }
}

# ── Detect line starting a block comment ────────────────────────────────────
{
    stripped = $0
    sub(/^[[:space:]]+/, "", stripped)
    # Check for /* that is NOT after a // (i.e., not inside a line comment)
    if (match(stripped, "/\\*")) {
        before = substr(stripped, 1, RSTART - 1)
        after  = substr(stripped, RSTART + RLENGTH)
        # Count line if it has meaningful code before the block comment
        if (before ~ /[^[:space:]]/) total++
        apply_braces($0)
        # Check if block comment ends on same line
        if (!match(after, "\\*/")) in_block_comment = 1
        next
    }
}

# ── If currently inside a skipped block, only track braces and skip ─────────
{
    if (skip_depth >= 0 && depth > skip_depth) {
        apply_braces($0)
        next
    }
    if (skip_depth >= 0 && depth <= skip_depth) {
        skip_depth = -1
    }
}

# ── #[cfg(test)] ─────────────────────────────────────────────────────────────
/^[[:space:]]*#\[cfg\(test\)\]/ {
    rest = $0
    sub(/^[[:space:]]*#\[cfg\(test\)\][[:space:]]*/, "", rest)
    sub(/[[:space:]]*\/\/.*$/, "", rest)

    if (rest == "" || rest ~ /^(mod|fn)[[:space:]]/) {
        pending_cfg_test = 1
        apply_braces($0)
    } else {
        apply_braces($0)
    }
    next
}

# ── #[test] ──────────────────────────────────────────────────────────────────
/^[[:space:]]*#\[test\]/ {
    apply_braces($0)
    next
}
/^[[:space:]]*#\[(should_panic|ignore)\]/ {
    apply_braces($0)
    next
}

# ── Normal line ──────────────────────────────────────────────────────────────
{
    old_depth = depth
    apply_braces($0)

    if (pending_cfg_test && depth > old_depth) {
        skip_depth = old_depth
        pending_cfg_test = 0
        next
    }
    pending_cfg_test = 0

    # Skip pure comment lines
    stripped = $0
    sub(/^[[:space:]]+/, "", stripped)
    if (stripped ~ /^\/\//) next
    if (stripped == "") next

    if (/[^[:space:]]/) total++
}

# ── Function: count braces on a line, update depth and skip_depth ────────────
function apply_braces(str,   i, ch) {
    for (i = 1; i <= length(str); i++) {
        ch = substr(str, i, 1)
        if (ch == "{") depth++
        if (ch == "}") {
            depth--
            if (skip_depth >= 0 && depth <= skip_depth) skip_depth = -1
        }
    }
}

END { print total + 0 }
'

# ── Line-count helpers ─────────────────────────────────────────────────────────

count_lines() {
    local file="$1"
    if [[ -f "$file" ]]; then
        awk "$awk_strip_tests" "$file" 2>/dev/null || echo 0
    else
        echo 0
    fi
}

count_dir_lines() {
    local dir="$1"
    if [[ ! -d "$dir" ]]; then
        echo 0
        return
    fi
    local total=0 count
    while IFS= read -r -d '' file; do
        [[ "$file" == */tests/* ]] && continue
        [[ "$(basename "$file")" == "tests.rs" ]] && continue
        count=$(awk "$awk_strip_tests" "$file" 2>/dev/null || echo 0)
        total=$((total + count))
    done < <(find "$dir" -name '*.rs' -print0 2>/dev/null)
    echo "$total"
}

# ── find_functions: extract (name, start_line, end_line, count) for each fn ──

awk_find_fns='
/^[[:space:]]*(pub([[:space:]]+\([^)]*\))?[[:space:]]+)?(async[[:space:]]+)?(unsafe[[:space:]]+)?fn[[:space:]]+[a-zA-Z_]/ {
    if (in_fn) next
    fn_name = $0
    sub(/^[[:space:]]*(pub[[:space:]]+(\([^)]*\)[[:space:]]+)?)?(async[[:space:]]+)?(unsafe[[:space:]]+)?fn[[:space:]]+/, "", fn_name)
    sub(/\(.*$/, "", fn_name)
    sub(/^<.*>[[:space:]]*/, "", fn_name)
    fn_start = NR; fn_braces = 0; in_fn = 1
    next
}
in_fn {
    split($0, chars, "")
    for (i = 1; i <= length($0); i++) {
        if (chars[i] == "{") fn_braces++
        if (chars[i] == "}") fn_braces--
    }
    if (fn_braces == 0) {
        printf "%s\t%s\t%s\t%s\n", fn_name, fn_start, NR, (NR - fn_start + 1)
        in_fn = 0
    }
}
'

find_functions() {
    local file="$1"
    [[ -f "$file" ]] || return
    awk "$awk_find_fns" "$file"
}

# ── status_line ────────────────────────────────────────────────────────────────

status_line() {
    local label="$1" current="$2" budget="$3"
    local excess=$((current - budget))
    if (( excess > 0 )); then
        printf "  ${RED}FAIL${RESET}  %-45s ${RED}%4d${RESET} / %4d  ${RED}(+%d over)${RESET}\n" \
            "$label" "$current" "$budget" "$excess"
        return 1
    else
        printf "  ${GREEN}PASS${RESET}  %-45s ${GREEN}%4d${RESET} / %4d\n" \
            "$label" "$current" "$budget"
        return 0
    fi
}

# ═══════════════════════════════════════════════════════════════════════════════
# JSON output
# ═══════════════════════════════════════════════════════════════════════════════

if $JSON_MODE; then
    echo '{'
    echo '  "generated": "'"$(date -u +%Y-%m-%dT%H:%M:%SZ)"'",'
    echo '  "commit": "'"$(git rev-parse --short HEAD 2>/dev/null || echo 'unknown')"'",'
    echo '  "budgets": ['
    comma=""
    any_failures=false

    for path in "${!BUDGETS[@]}"; do
        budget="${BUDGETS[$path]}"
        [[ "$path" == */ ]] && current=$(count_dir_lines "$path") || current=$(count_lines "$path")
        excess=$((current - budget))
        (( excess > 0 )) && any_failures=true

        printf '%s    {"path": "%s", "current": %d, "budget": %d, "excess": %d}\n' \
            "$comma" "$path" "$current" "$budget" "$excess"
        comma=","
    done

    while IFS= read -r -d '' file; do
        rel="${file#./}"
        current=$(count_lines "$file")
        excess=$((current - PER_FILE_BUDGET))
        if (( excess > 0 )); then
            any_failures=true
            printf '%s    {"path": "%s", "current": %d, "budget": %d, "excess": %d, "rule": "per-file"}\n' \
                "$comma" "$rel" "$current" "$PER_FILE_BUDGET" "$excess"
            comma=","
        fi
    done < <(find src -name '*.rs' ! -path '*/validation/parseable/*' ! -path '*/tests/*' ! -name 'tests.rs' -print0 2>/dev/null | sort -z)

    echo '  ],'
    echo '  "exit_code": '"$($any_failures && echo 1 || echo 0)"
    echo '}'
    $any_failures && exit 1 || exit 0
fi

# ═══════════════════════════════════════════════════════════════════════════════
# Human-readable output
# ═══════════════════════════════════════════════════════════════════════════════

echo ""
echo -e "${BOLD}═══ aufbau line-count budgets (STYLE.md §1) ═══${RESET}"
echo ""

any_failures=false

for path in "${!BUDGETS[@]}"; do
    budget="${BUDGETS[$path]}"
    [[ "$path" == */ ]] && current=$(count_dir_lines "$path") || current=$(count_lines "$path")
    status_line "$path" "$current" "$budget" || any_failures=true
done

# ── Per-file budget ────────────────────────────────────────────────────────────

echo ""
echo -e "${DIM}─── Per-file budget (≤ $PER_FILE_BUDGET lines, unless overridden below) ───${RESET}"
per_file_failures=0
while IFS= read -r -d '' file; do
    rel="${file#./}"
    current=$(count_lines "$file")
    budget="$PER_FILE_BUDGET"
    # file-specific override from style.toml
    [[ -v "BUDGETS[$rel]" ]] && budget="${BUDGETS[$rel]}"
    if (( current > budget )); then
        excess=$((current - budget))
        printf "  ${RED}FAIL${RESET}  %-45s ${RED}%4d${RESET} / %-4d ${RED}(+%d over)${RESET}\n" \
            "$rel" "$current" "$budget" "$excess"
        per_file_failures=$((per_file_failures + 1))
        any_failures=true
    fi
done < <(find src -name '*.rs' ! -path '*/validation/parseable/*' ! -path '*/tests/*' ! -name 'tests.rs' -print0 2>/dev/null | sort -z)

(( per_file_failures == 0 )) && echo -e "  ${GREEN}PASS${RESET}  all files within budget"

# ── Per-function (--per-fn) ─────────────────────────────────────────────────────

if $PER_FN; then
    echo ""
    echo -e "${DIM}─── Per-function budget (≤ $PER_FN_BUDGET lines) ───${RESET}"
    fn_failures=0
    while IFS= read -r -d '' file; do
        rel="${file#./}"
        while IFS=$'\t' read -r fn_name start end count; do
            if (( count > PER_FN_BUDGET )); then
                excess=$((count - PER_FN_BUDGET))
                printf "  ${RED}FAIL${RESET}  %-45s %s:L%s    ${RED}%4d${RESET} / %-4d ${RED}(+%d over)${RESET}\n" \
                    "$rel" "$fn_name" "$start" "$count" "$PER_FN_BUDGET" "$excess"
                fn_failures=$((fn_failures + 1))
                any_failures=true
            fi
        done < <(find_functions "$file")
    done < <(find src -name '*.rs' ! -path '*/validation/parseable/*' ! -path '*/tests/*' -print0 2>/dev/null | sort -z)

    (( fn_failures == 0 )) && echo -e "  ${GREEN}PASS${RESET}  all functions within budget"
fi

# ── Summary ────────────────────────────────────────────────────────────────────

echo ""
if $any_failures; then
    echo -e "${BOLD}${RED}═══ FAIL: some budgets exceeded ═══${RESET}"
    $CHECK_MODE && exit 1
else
    echo -e "${BOLD}${GREEN}═══ PASS: all budgets satisfied ═══${RESET}"
fi

exit 0
