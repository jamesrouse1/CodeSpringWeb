#!/usr/bin/env bash
set -euo pipefail

APP_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CHECK_CONFIG=0
REQUESTED_START_PORT=""
if [[ "${1:-}" == "--check-config" ]]; then
  CHECK_CONFIG=1
elif [[ -n "${1:-}" ]]; then
  REQUESTED_START_PORT="$1"
fi
# Only SSH tunnels and other processes on the server itself may reach Shiny.
HOST="127.0.0.1"

USER_NAME="$(id -un 2>/dev/null || true)"
if [[ -z "$USER_NAME" ]]; then
  printf '\033[31mCould not determine the effective Unix user. CodeSpringApp was not started.\033[0m\n'
  exit 1
fi

USER_HOME=""
if command -v getent >/dev/null 2>&1; then
  USER_HOME="$(getent passwd "$USER_NAME" 2>/dev/null | awk -F: 'NR == 1 { print $6 }')" || USER_HOME=""
elif command -v dscl >/dev/null 2>&1; then
  USER_HOME="$(dscl . -read "/Users/$USER_NAME" NFSHomeDirectory 2>/dev/null | awk 'NR == 1 { print $2 }')" || USER_HOME=""
fi
if [[ -z "$USER_HOME" && -n "$(command -v Rscript 2>/dev/null || true)" ]]; then
  USER_HOME="$(Rscript -e 'user <- commandArgs(TRUE)[1]; home <- path.expand(paste0("~", user)); if (!identical(home, paste0("~", user))) cat(home)' "$USER_NAME" 2>/dev/null)" || USER_HOME=""
fi
if [[ -z "$USER_HOME" || ! -d "$USER_HOME" ]]; then
  printf '\033[31mCould not determine a valid home directory for Unix user %s. CodeSpringApp was not started.\033[0m\n' "$USER_NAME"
  exit 1
fi
USER_HOME="$(cd "$USER_HOME" && pwd -P)"

# Give every Unix account its own predictable port block by default.  This
# avoids two people who start the launcher at the same time racing for 8601.
# A numeric argument still explicitly selects a different starting port.
USER_ID="$(id -u 2>/dev/null || true)"
if [[ -n "$REQUESTED_START_PORT" ]]; then
  START_PORT="$REQUESTED_START_PORT"
elif [[ "$USER_ID" =~ ^[0-9]+$ ]]; then
  START_PORT="$((20000 + (USER_ID % 20000)))"
else
  START_PORT=8601
fi
if [[ ! "$START_PORT" =~ ^[0-9]+$ ]] || (( START_PORT < 1024 || START_PORT > 65435 )); then
  printf '\033[31mStarting port must be a number from 1024 to 65435.\033[0m\n'
  exit 1
fi
MAX_PORT="${CSL_WEB_MAX_PORT:-$((START_PORT + 99))}"
if [[ ! "$MAX_PORT" =~ ^[0-9]+$ ]] || (( MAX_PORT < START_PORT || MAX_PORT > 65535 )); then
  printf '\033[31mCSL_WEB_MAX_PORT must be a number from %s to 65535.\033[0m\n' "$START_PORT"
  exit 1
fi

path_within_user_home() {
  local path="$1"
  local parent
  local resolved
  if [[ -e "$path" ]]; then
    if [[ -d "$path" ]]; then
      resolved="$(cd "$path" && pwd -P)"
    else
      parent="$(cd "$(dirname "$path")" && pwd -P)"
      resolved="$parent/$(basename "$path")"
    fi
  else
    parent="$(dirname "$path")"
    [[ -d "$parent" ]] || return 1
    parent="$(cd "$parent" && pwd -P)"
    resolved="$parent/$(basename "$path")"
  fi
  [[ "$resolved" == "$USER_HOME" || "$resolved" == "$USER_HOME/"* ]]
}

if ! path_within_user_home "$APP_DIR"; then
  printf '\033[31mRefusing to start a CodeSpringApp checkout outside %s\033[0m\n' "$USER_HOME"
  printf 'Log in as %s and clone or pull CodeSpringApp inside that user home.\n' "$USER_NAME"
  exit 1
fi

export HOME="$USER_HOME"
export USER="$USER_NAME"
export LOGNAME="$USER_NAME"

if [[ -n "${CSL_CODESPRINGLAB_ROOT:-}" ]]; then
  CSL_ROOT="$CSL_CODESPRINGLAB_ROOT"
elif [[ -d "$USER_HOME/CodeSpringLab/scripts_DoNotTouch" ]]; then
  CSL_ROOT="$USER_HOME/CodeSpringLab"
else
  CSL_ROOT="$USER_HOME/CSH/CodeSpringLab"
fi
LOG_DIR="${CSL_WEB_LOG_DIR:-$USER_HOME/.codespringweb}"
if ! path_within_user_home "$CSL_ROOT"; then
  printf '\033[31mRefusing CodeSpringLab root outside %s:\033[0m %s\n' "$USER_HOME" "$CSL_ROOT"
  exit 1
fi
if ! path_within_user_home "$LOG_DIR"; then
  printf '\033[31mRefusing CodeSpringApp config/log directory outside %s:\033[0m %s\n' "$USER_HOME" "$LOG_DIR"
  exit 1
fi
if [[ ! -d "$CSL_ROOT/scripts_DoNotTouch" ]]; then
  printf '\033[31mCodeSpringLab root not found:\033[0m %s\n' "$CSL_ROOT"
  printf 'Run again with: \033[1mCSL_CODESPRINGLAB_ROOT=/path/to/CodeSpringLab %s\033[0m\n' "$0"
  exit 1
fi

if [[ "$CHECK_CONFIG" == "1" ]]; then
  printf '\033[32mCodeSpringApp configuration is isolated correctly.\033[0m\n'
  printf 'Unix user: %s\n' "$USER_NAME"
  printf 'Default private port block: %s-%s\n' "$START_PORT" "$MAX_PORT"
  printf 'User home: %s\n' "$USER_HOME"
  printf 'CodeSpringApp: %s\n' "$APP_DIR"
  printf 'CodeSpringLab: %s\n' "$CSL_ROOT"
  printf 'Private app state: %s\n' "$LOG_DIR"
  exit 0
fi

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

generate_access_token() {
  if command -v openssl >/dev/null 2>&1; then
    openssl rand -hex 32
  else
    Rscript -e 'set.seed(NULL); cat(paste(sample(c(0:9, letters[1:6]), 64, replace = TRUE), collapse = ""))'
  fi
}

ACCESS_TOKEN="$(generate_access_token)"
if [[ ! "$ACCESS_TOKEN" =~ ^[0-9a-f]{64}$ ]]; then
  printf '\033[31mCould not generate a secure CodeSpringApp access token.\033[0m\n'
  exit 1
fi

IDLE_SHUTDOWN_SECONDS="${CSL_WEB_IDLE_SHUTDOWN_SECONDS:-300}"
if [[ ! "$IDLE_SHUTDOWN_SECONDS" =~ ^[0-9]+$ ]]; then
  printf '\033[31mCSL_WEB_IDLE_SHUTDOWN_SECONDS must be a non-negative whole number.\033[0m\n'
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
  id -un 2>/dev/null || true
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
  [[ "$cmd" =~ (CodeSpringApp|CodeSpringWeb|codespringweb_[0-9]+\.log) ]]
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
    rm -f "$pf" "${pf%.pid}.url"
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

  nohup env HOME="$USER_HOME" USER="$USER_NAME" LOGNAME="$USER_NAME" \
    CSL_CODESPRINGLAB_ROOT="$CSL_ROOT" CSL_WEB_HOME="$LOG_DIR" \
    CSL_WEB_ACCESS_TOKEN="$ACCESS_TOKEN" CSL_WEB_IDLE_SHUTDOWN_SECONDS="$IDLE_SHUTDOWN_SECONDS" \
    Rscript -e "shiny::runApp('$APP_DIR', host='$HOST', port=$PORT)" > "$LOG_FILE" 2>&1 &
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

APP_URL="http://localhost:${PORT}/?token=${ACCESS_TOKEN}"
URL_FILE="$LOG_DIR/codespringweb_${PORT}.url"
printf '%s\n' "$APP_URL" > "$URL_FILE"
chmod 600 "$URL_FILE"

printf '\n\033[32mCodeSpringApp is running on bamdev1 port %s.\033[0m\n' "$PORT"
printf '\033[1;36mCopy/paste this command into your laptop terminal:\033[0m\n'
printf '\033[1mssh -N -L %s:localhost:%s %s@bamdev1\033[0m\n' "$PORT" "$PORT" "$USER_NAME"
printf '\033[1;36mThen open this private URL:\033[0m \033[1m%s\033[0m\n' "$APP_URL"
if [[ "$IDLE_SHUTDOWN_SECONDS" == "0" ]]; then
  printf '\033[90mAutomatic idle shutdown: disabled\033[0m\n'
else
  printf '\033[90mAutomatic idle shutdown: %s seconds after the last browser session closes\033[0m\n' "$IDLE_SHUTDOWN_SECONDS"
fi
printf '\033[90mServer log: %s\033[0m\n\n' "$LOG_FILE"
