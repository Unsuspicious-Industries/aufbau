#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<EOF >&2
Usage: $0 user@host:/path/to/p7 [-- <validate-args>]

Examples:
  # sync and run default validate
  $0 user@server:/home/me/p7

  # sync and pass args to validate (e.g. -m parseable)
  $0 user@server:/home/me/p7 -- -m parseable
EOF
  exit 2
}

if [ $# -lt 1 ]; then
  usage
fi

DEST="$1"
shift || true
REMOTE_ARGS=("$@")

HOST="${DEST%%:*}"
DIR="${DEST#*:}"

if [ -z "$HOST" ] || [ -z "$DIR" ]; then
  echo "Invalid destination: $DEST" >&2
  usage
fi

echo "Syncing project to $HOST:$DIR (excluding build/artifacts)..."
RSYNC_EXCLUDES=(
  --exclude 'target'
  --exclude 'target/**'
  --exclude '.git'
  --exclude '.git/**'
  --exclude 'node_modules'
  --exclude '.venv'
  --exclude 'venv'
  --exclude '.idea'
  --exclude '.vscode'
  --exclude '__pycache__'
  --exclude '*.pyc'
  --exclude '.DS_Store'
  --exclude '.cache'
  --exclude 'build'
)

rsync -az --progress --delete "${RSYNC_EXCLUDES[@]}" ./ "$DEST"

echo "Sync complete. Running validate on remote..."

# Build a safely-escaped argument string for the remote shell
REMOTE_ARG_STR=""
if [ ${#REMOTE_ARGS[@]} -gt 0 ]; then
  for a in "${REMOTE_ARGS[@]}"; do
    REMOTE_ARG_STR+=" $(printf %q "$a")"
  done
fi

SSH_CMD="cd '$DIR' && cargo run validate ${REMOTE_ARG_STR}"

echo "SSH ${HOST} -> ${SSH_CMD}"
# allocate a TTY so output is streamed with colors/interactive output preserved
# run ssh but do NOT exit the script immediately if the remote command fails —
# we still want to fetch the report files that the remote validate produced.
set +e
ssh -t "$HOST" "$SSH_CMD"
SSH_RC=$?
set -e

echo "Remote validate finished (exit code: $SSH_RC)."

# fetch remote reports back to local workspace (if any)
mkdir -p validation/reports
echo "Pulling remote reports from $HOST:$DIR/validation/reports -> ./validation/reports/"
rsync -az --progress --delete "$HOST:$DIR/validation/reports/" ./validation/reports/ || true

# print the most-recent report (if present)
LATEST=$(ls -1t validation/reports 2>/dev/null | head -n1 || true)
if [ -n "$LATEST" ]; then
  echo "\n=== Latest report: validation/reports/$LATEST ==="
  echo "----- BEGIN REPORT -----"
  cat "validation/reports/$LATEST" || true
  echo "-----  END REPORT  -----"
else
  echo "No report files found in validation/reports"
fi

# return the remote validate's exit code so callers can see success/failure
exit $SSH_RC
