#!/usr/bin/env bash
set -euo pipefail

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PORT="${1:-8501}"
HOST="${CSL_WEB_HOST:-0.0.0.0}"
CSL_ROOT="${CSL_CODESPRINGLAB_ROOT:-$HOME/CodeSpringLab}"
LOG_DIR="${CSL_WEB_LOG_DIR:-$HOME/.codespringweb}"
mkdir -p "$LOG_DIR"
LOG_FILE="$LOG_DIR/codespringweb_${PORT}.log"

if [[ ! -d "$CSL_ROOT/scripts_DoNotTouch" ]]; then
  printf '\033[31mCodeSpringLab root not found:\033[0m %s\n' "$CSL_ROOT"
  printf 'Run again with: \033[1mCSL_CODESPRINGLAB_ROOT=/path/to/CodeSpringLab %s\033[0m\n' "$0"
  exit 1
fi

install_r_package_if_missing() {
  local pkg="$1"
  if Rscript -e "quit(status = if (requireNamespace('$pkg', quietly = TRUE)) 0 else 1)" >/dev/null 2>&1; then
    return 0
  fi

  printf '\033[33mInstalling required R package %s into your user library...\033[0m\n' "$pkg"
  Rscript -e "pkg <- '$pkg'; lib <- Sys.getenv('R_LIBS_USER'); if (!nzchar(lib)) lib <- file.path(path.expand('~'), 'R', paste0(R.version$platform, '-library'), paste(R.version$major, sub('\\\\..*', '', R.version$minor), sep='.')); lib <- path.expand(lib); dir.create(lib, recursive = TRUE, showWarnings = FALSE); .libPaths(c(lib, .libPaths())); install.packages(pkg, lib = lib, repos = 'https://cloud.r-project.org'); if (!requireNamespace(pkg, quietly = TRUE)) stop('Could not install required R package: ', pkg)"
}

install_r_package_if_missing "DT"

if command -v lsof >/dev/null 2>&1; then
  OLD_PIDS="$(lsof -tiTCP:"$PORT" -sTCP:LISTEN 2>/dev/null || true)"
  if [[ -n "$OLD_PIDS" ]]; then
    kill $OLD_PIDS 2>/dev/null || true
    sleep 1
  fi
fi

nohup env CSL_CODESPRINGLAB_ROOT="$CSL_ROOT" Rscript -e "shiny::runApp('$APP_DIR', host='$HOST', port=$PORT)" > "$LOG_FILE" 2>&1 &
APP_PID="$!"
echo "$APP_PID" > "$LOG_DIR/codespringweb_${PORT}.pid"

sleep 2
if ! kill -0 "$APP_PID" 2>/dev/null; then
  printf '\033[31mCodeSpringWeb did not start. Last log lines:\033[0m\n'
  tail -40 "$LOG_FILE" || true
  exit 1
fi

USER_NAME="${USER:-rouse}"
printf '\n\033[32mCodeSpringWeb is running on bamdev1 port %s.\033[0m\n' "$PORT"
printf '\033[1;36mCopy/paste this command into your laptop terminal:\033[0m\n'
printf '\033[1mssh -N -L %s:localhost:%s %s@bamdev1\033[0m\n' "$PORT" "$PORT" "$USER_NAME"
printf '\033[1;36mThen open:\033[0m \033[1mhttp://localhost:%s\033[0m\n' "$PORT"
printf '\033[90mServer log: %s\033[0m\n\n' "$LOG_FILE"
