#!/usr/bin/env sh
# Every `Admitted` in the .v sources must be listed in OBLIGATIONS.md.
#
# An admitted statement is an assumption. This keeps the set of assumptions
# explicit, so a new one cannot appear without someone writing it down.
#
#   ./obligations.sh          report, exit 1 on drift
#   ./obligations.sh --list   just print what is admitted
set -eu

cd "$(dirname "$0")"
MANIFEST=OBLIGATIONS.md

# `Proof. Admitted.` closes the statement named by the nearest preceding
# Theorem/Lemma/Corollary/Fact/Remark/Example/Proposition/Definition.
scan() {
  for f in *.v; do
    awk -v file="$f" '
      /^[ \t]*(Theorem|Lemma|Corollary|Fact|Proposition|Remark|Example)[ \t]+/ {
        name = $2
        sub(/[^A-Za-z0-9_'"'"'].*$/, "", name)
      }
      /Admitted[ \t]*\./ && !/^[ \t]*\(\*/ && !/^[ \t]*[-*]/ {
        if (name != "") print file ":" name
      }
    ' "$f"
  done | sort -u
}

found=$(scan)

if [ "${1:-}" = "--list" ]; then
  printf '%s\n' "$found"
  exit 0
fi

declared=$(sed -n '/BEGIN MANIFEST/,/END MANIFEST/p' "$MANIFEST" \
  | grep -v 'MANIFEST' | grep -v '^[[:space:]]*$' | sort -u)

new=$(printf '%s\n' "$found" | comm -23 - <(printf '%s\n' "$declared"))
gone=$(printf '%s\n' "$declared" | comm -23 - <(printf '%s\n' "$found"))

status=0
if [ -n "$new" ]; then
  echo "✗ Admitted, but not in $MANIFEST:"
  printf '%s\n' "$new" | sed 's/^/    /'
  echo "  Add it deliberately, or prove it."
  status=1
fi
if [ -n "$gone" ]; then
  echo "✗ Listed in $MANIFEST but no longer admitted:"
  printf '%s\n' "$gone" | sed 's/^/    /'
  echo "  If it is proven now, delete the line (good news)."
  status=1
fi
[ "$status" -eq 0 ] && echo "✓ $(printf '%s\n' "$found" | grep -c . ) admitted obligation(s), all declared"
exit "$status"
