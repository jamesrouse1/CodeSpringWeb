#!/usr/bin/env bash
set -Eeuo pipefail

repo_root="$(cd -- "$(dirname -- "${BASH_SOURCE[0]}")/.." && pwd)"
lab_root="${1:-$(cd -- "$repo_root/../CodeSpringLab-fix" && pwd)}"
(cd "$repo_root" && Rscript tests/smoke_test_app_helpers.R "$lab_root")
CSL_CODESPRINGLAB_ROOT="$lab_root" "$repo_root/run_codespringweb.sh" --check-config
echo "All CodeSpringApp tests passed."
