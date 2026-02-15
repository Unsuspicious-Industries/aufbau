#!/usr/bin/env bash
# Deploy the P7 demo (backend + frontend) to a remote server.
# - Rsync repo to remote
# - Builds frontend
# - Installs backend deps + Python package
# - Sets up systemd services (backend + frontend)

set -euo pipefail
IFS=$'\n\t'

usage() {
  cat <<EOF
Usage: $(basename "$0") [options] user@host[:remote_path]

Options:
  -p, --port PORT           SSH port (default: 22)
  -i, --key PATH            SSH private key (optional)
  -d, --remote-dir DIR      Remote directory (default: ~/p7-demo)
  --backend-port PORT       Backend port (default: 5001)
  --frontend-port PORT      Frontend port (default: 3000)
  --no-systemd              Skip systemd service setup
  --bootstrap               Attempt apt-based dependency install (requires sudo)
  --dry-run                 Print commands only
  -h, --help                Show help

Examples:
  ./scripts/remote_deploy.sh ubuntu@1.2.3.4
  ./scripts/remote_deploy.sh -p 2222 -i ~/.ssh/id_rsa ubuntu@host:/opt/p7-demo
EOF
}

SSH_PORT=22
SSH_KEY=""
REMOTE_DIR="~/p7-demo"
BACKEND_PORT=5001
FRONTEND_PORT=3000
NO_SYSTEMD=0
BOOTSTRAP=0
DRY_RUN=0

REMOTE=""
while [[ $# -gt 0 ]]; do
  case "$1" in
    -p|--port) SSH_PORT="$2"; shift 2;;
    -i|--key) SSH_KEY="$2"; shift 2;;
    -d|--remote-dir) REMOTE_DIR="$2"; shift 2;;
    --backend-port) BACKEND_PORT="$2"; shift 2;;
    --frontend-port) FRONTEND_PORT="$2"; shift 2;;
    --no-systemd) NO_SYSTEMD=1; shift;;
    --bootstrap) BOOTSTRAP=1; shift;;
    --dry-run) DRY_RUN=1; shift;;
    -h|--help) usage; exit 0;;
    --) shift; break;;
    -*) echo "Unknown option: $1"; usage; exit 1;;
    *)
      if [[ -z "$REMOTE" ]]; then
        REMOTE="$1"; shift
      else
        echo "Multiple remote targets provided"; usage; exit 1
      fi
      ;;
  esac
done

if [[ -z "$REMOTE" ]]; then
  echo "Missing remote target (user@host[:path])" >&2
  usage
  exit 1
fi

if [[ "$REMOTE" == *:* ]]; then
  REMOTE_HOST="${REMOTE%%:*}"
  REMOTE_PATH="${REMOTE#*:}"
  if [[ -n "$REMOTE_PATH" ]]; then
    REMOTE_DIR="$REMOTE_PATH"
  fi
else
  REMOTE_HOST="$REMOTE"
fi

REPO_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

EXCLUDES=(
  --exclude 'target'
  --exclude '*/target'
  --exclude '.git'
  --exclude '.git/*'
  --exclude 'node_modules'
  --exclude '__pycache__'
  --exclude '*.pyc'
  --exclude '*.pyo'
  --exclude '.venv'
  --exclude 'python/.venv'
)

SSH_OPTS=( -p "$SSH_PORT" -o BatchMode=yes -o ConnectTimeout=10 )
if [[ -n "$SSH_KEY" ]]; then
  SSH_OPTS+=( -i "$SSH_KEY" )
fi

echo "Project root: $REPO_DIR"
echo "Target: $REMOTE_HOST:$REMOTE_DIR"

SSH_CMD_STR="ssh"
for opt in "${SSH_OPTS[@]}"; do
  SSH_CMD_STR+=" $opt"
done

RSYNC_CMD=( rsync -az --delete )
RSYNC_CMD+=( "${EXCLUDES[@]}" )
RSYNC_CMD+=( -e "$SSH_CMD_STR" )
RSYNC_CMD+=( "$REPO_DIR/" "$REMOTE_HOST:$REMOTE_DIR/" )

echo "-> Running rsync to remote..."
if [[ "$DRY_RUN" == "1" ]]; then
  echo "${RSYNC_CMD[*]}"
else
  "${RSYNC_CMD[@]}"
fi

if [[ "$DRY_RUN" == "1" ]]; then
  echo "DRY-RUN: ssh ${SSH_OPTS[*]} $REMOTE_HOST mkdir -p '$REMOTE_DIR'"
  echo "DRY-RUN: ssh ${SSH_OPTS[*]} $REMOTE_HOST bash -s -- ..."
  exit 0
fi

ssh "${SSH_OPTS[@]}" "$REMOTE_HOST" mkdir -p "$REMOTE_DIR"
ssh "${SSH_OPTS[@]}" "$REMOTE_HOST" bash -s -- \
  "$REMOTE_DIR" "$BACKEND_PORT" "$FRONTEND_PORT" "$NO_SYSTEMD" "$BOOTSTRAP" <<'END_REMOTE'
set -euo pipefail
REMOTE_DIR="$1"
BACKEND_PORT="$2"
FRONTEND_PORT="$3"
NO_SYSTEMD="$4"
BOOTSTRAP="$5"

cd "$REMOTE_DIR"
echo "[remote] pwd=$(pwd) host=$(hostname)"

if [[ "$BOOTSTRAP" == "1" ]]; then
  if command -v apt-get >/dev/null 2>&1; then
    sudo apt-get update
    sudo apt-get install -y python3 python3-venv python3-pip nodejs npm
  fi
fi

PY_BIN=""
for c in python3.11 python3 python; do
  if command -v "$c" >/dev/null 2>&1; then PY_BIN="$c"; break; fi
done
if [[ -z "$PY_BIN" ]]; then
  echo "[remote] python not found (use --bootstrap or install python3)" >&2
  exit 1
fi

if ! command -v npm >/dev/null 2>&1; then
  echo "[remote] npm not found (use --bootstrap or install nodejs/npm)" >&2
  exit 1
fi

VENV_DIR="python/visualization/.venv"
"$PY_BIN" -m venv "$VENV_DIR"
# shellcheck disable=SC1091
source "$VENV_DIR/bin/activate"
python -m pip install --upgrade pip setuptools wheel
python -m pip install -e python
python -m pip install -r python/visualization/backend/requirements.txt
python -m pip install gunicorn

cd python/visualization/frontend
# Prefer a reproducible install when a lockfile exists; otherwise fall back to legacy install.
if [ -f package-lock.json ]; then
  npm ci --legacy-peer-deps || npm install --legacy-peer-deps
else
  npm install --legacy-peer-deps
  # create a package-lock on the remote for reproducible builds (won't be committed)
  npm install --package-lock-only --legacy-peer-deps || true
fi
npm install --no-save serve
npm run build
cd "$REMOTE_DIR"

if [[ "$NO_SYSTEMD" == "1" ]]; then
  echo "[remote] systemd setup skipped (--no-systemd)"
  echo "[remote] backend: $REMOTE_DIR/python/visualization/.venv/bin/gunicorn -w 2 -b 0.0.0.0:${BACKEND_PORT} app:app"
  echo "[remote] frontend: $REMOTE_DIR/python/visualization/frontend/node_modules/.bin/serve -s build -l ${FRONTEND_PORT}"
  exit 0
fi

BACKEND_SERVICE="/etc/systemd/system/p7-demo-backend.service"
FRONTEND_SERVICE="/etc/systemd/system/p7-demo-frontend.service"

sudo tee "$BACKEND_SERVICE" >/dev/null <<SERVICE
[Unit]
Description=P7 Demo Backend
After=network.target

[Service]
Type=simple
WorkingDirectory=$REMOTE_DIR/python/visualization/backend
ExecStart=$REMOTE_DIR/python/visualization/.venv/bin/gunicorn -w 2 -b 0.0.0.0:${BACKEND_PORT} app:app
Restart=always
RestartSec=2

[Install]
WantedBy=multi-user.target
SERVICE

sudo tee "$FRONTEND_SERVICE" >/dev/null <<SERVICE
[Unit]
Description=P7 Demo Frontend
After=network.target

[Service]
Type=simple
WorkingDirectory=$REMOTE_DIR/python/visualization/frontend
ExecStart=$REMOTE_DIR/python/visualization/frontend/node_modules/.bin/serve -s build -l ${FRONTEND_PORT}
Restart=always
RestartSec=2

[Install]
WantedBy=multi-user.target
SERVICE

sudo systemctl daemon-reload
sudo systemctl enable p7-demo-backend p7-demo-frontend
sudo systemctl restart p7-demo-backend p7-demo-frontend

echo "[remote] demo deployed"
echo "[remote] backend: http://<host>:${BACKEND_PORT}"
echo "[remote] frontend: http://<host>:${FRONTEND_PORT}"
END_REMOTE

echo "Deploy complete."
