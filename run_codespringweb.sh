#!/usr/bin/env bash
set -euo pipefail

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
START_PORT="${1:-8601}"
MAX_PORT="${CSL_WEB_MAX_PORT:-8699}"
HOST="${CSL_WEB_HOST:-0.0.0.0}"
CSL_ROOT="${CSL_CODESPRINGLAB_ROOT:-$HOME/CodeSpringLab}"
LOG_DIR="${CSL_WEB_LOG_DIR:-$HOME/.codespringweb}"
mkdir -p "$LOG_DIR"
R_MAKEVARS_FILE="$LOG_DIR/Makevars.codespringweb"

write_codespring_r_makevars() {
  if [[ ! -f "$R_MAKEVARS_FILE" ]]; then
    {
      printf 'CFLAGS=-O2 -pipe -fPIC\n'
      printf 'CXXFLAGS=-O2 -pipe -fPIC\n'
      printf 'CXX11FLAGS=-O2 -pipe -fPIC\n'
      printf 'CXX14FLAGS=-O2 -pipe -fPIC\n'
      printf 'CXX17FLAGS=-O2 -pipe -fPIC\n'
      printf 'CXX20FLAGS=-O2 -pipe -fPIC\n'
    } > "$R_MAKEVARS_FILE"
  fi
}

write_codespring_r_makevars

if [[ ! -d "$CSL_ROOT/scripts_DoNotTouch" ]]; then
  printf '\033[31mCodeSpringLab root not found:\033[0m %s\n' "$CSL_ROOT"
  printf 'Run again with: \033[1mCSL_CODESPRINGLAB_ROOT=/path/to/CodeSpringLab %s\033[0m\n' "$0"
  exit 1
fi

install_r_package_if_missing() {
  local pkg="$1"
  if R_MAKEVARS_USER="$R_MAKEVARS_FILE" Rscript -e "quit(status = if (requireNamespace('$pkg', quietly = TRUE)) 0 else 1)" >/dev/null 2>&1; then
    return 0
  fi

  printf '\033[33mInstalling required R package %s into your user library...\033[0m\n' "$pkg"
  R_MAKEVARS_USER="$R_MAKEVARS_FILE" Rscript -e 'pkg <- commandArgs(TRUE)[1]; lib <- Sys.getenv("R_LIBS_USER"); if (!nzchar(lib)) lib <- file.path(path.expand("~"), "R", paste0(R.version$platform, "-library"), paste(R.version$major, sub("\\..*", "", R.version$minor), sep = ".")); lib <- path.expand(lib); dir.create(lib, recursive = TRUE, showWarnings = FALSE); .libPaths(c(lib, .libPaths())); install.packages(pkg, lib = lib, repos = "https://cloud.r-project.org"); if (!requireNamespace(pkg, quietly = TRUE)) stop("Could not install required R package: ", pkg)' "$pkg"
}

install_r_package_if_missing "DT"
install_r_package_if_missing "base64enc"
install_r_package_if_missing "ggplot2"

current_user() {
  if [[ -n "${USER:-}" ]]; then
    printf '%s\n' "$USER"
  else
    id -un 2>/dev/null || true
  fi
}

pid_user() {
  ps -o user= -p "$1" 2>/dev/null | awk '{print $1}' || true
}

pid_command() {
  ps -o command= -p "$1" 2>/dev/null || true
}

listener_pids_for_port() {
  local port="$1"
  if command -v lsof >/dev/null 2>&1; then
    lsof -nP -iTCP:"$port" -sTCP:LISTEN -t 2>/dev/null || true
  fi
}

looks_like_codespring_app() {
  local cmd="$1"
  [[ "$cmd" =~ (CodeSpringApp|CodeSpringWeb|shiny::runApp|runApp\(|/Rscript|/R[[:space:]]) ]]
}

stop_pid_if_ours() {
  local pid="$1"
  local reason="$2"
  local owner
  local me
  local cmd
  pid="${pid//[!0-9]/}"
  [[ -n "$pid" ]] || return 1
  kill -0 "$pid" 2>/dev/null || return 1
  me="$(current_user)"
  owner="$(pid_user "$pid")"
  cmd="$(pid_command "$pid")"
  if [[ -n "$owner" && -n "$me" && "$owner" != "$me" ]]; then
    return 1
  fi
  if ! looks_like_codespring_app "$cmd"; then
    return 1
  fi
  kill "$pid" 2>/dev/null || true
  sleep 0.4
  if kill -0 "$pid" 2>/dev/null; then
    kill -9 "$pid" 2>/dev/null || true
  fi
  printf '\033[33mStopped previous CodeSpringApp process %s (%s).\033[0m\n' "$pid" "$reason"
  return 0
}

cleanup_previous_codespring_app_ports() {
  local pid
  local pf
  local candidate

  shopt -s nullglob
  for pf in "$LOG_DIR"/codespringweb_*.pid "$LOG_DIR"/codespringapp_*.pid; do
    pid="$(head -n 1 "$pf" 2>/dev/null || true)"
    stop_pid_if_ours "$pid" "pidfile $(basename "$pf")" || true
    rm -f "$pf"
  done
  shopt -u nullglob

  for candidate in $(seq "$START_PORT" "$MAX_PORT"); do
    for pid in $(listener_pids_for_port "$candidate"); do
      stop_pid_if_ours "$pid" "port $candidate" || true
    done
  done
}

cleanup_previous_codespring_app_ports

port_is_busy() {
  local port="$1"

  if command -v lsof >/dev/null 2>&1 && lsof -nP -iTCP:"$port" -sTCP:LISTEN -t >/dev/null 2>&1; then
    return 0
  fi

  if command -v ss >/dev/null 2>&1 && ss -ltn 2>/dev/null | awk '{print $4}' | grep -Eq "(^|:)$port$"; then
    return 0
  fi

  if command -v netstat >/dev/null 2>&1 && netstat -ltn 2>/dev/null | awk '{print $4}' | grep -Eq "(^|:)$port$"; then
    return 0
  fi

  return 1
}

PORT=""
for candidate in $(seq "$START_PORT" "$MAX_PORT"); do
  if ! port_is_busy "$candidate"; then
    PORT="$candidate"
    break
  fi
done

if [[ -z "$PORT" ]]; then
  printf '\033[31mNo open CodeSpringApp port found from %s to %s.\033[0m\n' "$START_PORT" "$MAX_PORT"
  printf 'Try a different starting port, for example: \033[1m%s 8700\033[0m\n' "$0"
  exit 1
fi

if [[ "$PORT" != "$START_PORT" ]]; then
  printf '\033[33mPort %s was busy, so CodeSpringApp will use port %s instead.\033[0m\n' "$START_PORT" "$PORT"
fi

APP_PID=""
LOG_FILE=""

for candidate in $(seq "$PORT" "$MAX_PORT"); do
  PORT="$candidate"
  LOG_FILE="$LOG_DIR/codespringweb_${PORT}.log"

  nohup env CSL_CODESPRINGLAB_ROOT="$CSL_ROOT" Rscript -e "shiny::runApp('$APP_DIR', host='$HOST', port=$PORT)" > "$LOG_FILE" 2>&1 &
  APP_PID="$!"
  echo "$APP_PID" > "$LOG_DIR/codespringweb_${PORT}.pid"

  sleep 2
  if kill -0 "$APP_PID" 2>/dev/null; then
    break
  fi

  if grep -Eqi "address already in use|Failed to create server|port.*busy|cannot open.*port" "$LOG_FILE" 2>/dev/null; then
    rm -f "$LOG_DIR/codespringweb_${PORT}.pid"
    printf '\033[33mPort %s became busy while starting, trying %s instead.\033[0m\n' "$PORT" "$((PORT + 1))"
    APP_PID=""
    continue
  fi

  printf '\033[31mCodeSpringApp did not start. Last log lines:\033[0m\n'
  tail -40 "$LOG_FILE" || true
  exit 1
done

if [[ -z "$APP_PID" ]] || ! kill -0 "$APP_PID" 2>/dev/null; then
  printf '\033[31mNo open CodeSpringApp port found from %s to %s.\033[0m\n' "$START_PORT" "$MAX_PORT"
  exit 1
fi

USER_NAME="${USER:-rouse}"
printf '\n\033[32mCodeSpringApp is running on bamdev1 port %s.\033[0m\n' "$PORT"
printf '\033[1;36mCopy/paste this command into your laptop terminal:\033[0m\n'
printf '\033[1mssh -N -L %s:localhost:%s %s@bamdev1\033[0m\n' "$PORT" "$PORT" "$USER_NAME"
printf '\033[1;36mThen open:\033[0m \033[1mhttp://localhost:%s\033[0m\n' "$PORT"
printf '\033[90mServer log: %s\033[0m\n\n' "$LOG_FILE"
