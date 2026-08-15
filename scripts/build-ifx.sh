#!/usr/bin/env bash
#
# build-ifx.sh — Build the ModSIM Fortran engine with Intel IFX (oneAPI) on Linux.
#
# Auto-detects Intel oneAPI, sources setvars.sh, then invokes the GNUmakefile
# in Modsim/Modsimdl to build libmodsim.so, libUserModels.so and
# libModsimCurveFit.so.
#
# This is the Linux equivalent of scripts/build-ifx.ps1, invoked by the
# top-level GNUmakefile when COMPILER=ifx on non-Windows platforms.
#
# Usage:
#   ./scripts/build-ifx.sh [release|debug]
#
set -euo pipefail

BUILD="${1:-release}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
DL_DIR="$REPO_ROOT/Modsim/Modsimdl"

# ── Locate Intel oneAPI setvars.sh ───────────────────────────────────────────
ONEAPI_PATHS=(
    "/opt/intel/oneapi/setvars.sh"
    "$HOME/intel/oneapi/setvars.sh"
    "/usr/local/intel/oneapi/setvars.sh"
)

setvars=""
for candidate in "${ONEAPI_PATHS[@]}"; do
    if [[ -f "$candidate" ]]; then
        setvars="$candidate"
        break
    fi
done

if [[ -z "$setvars" ]]; then
    echo "Intel oneAPI setvars.sh not found. Searched:" >&2
    for p in "${ONEAPI_PATHS[@]}"; do echo "  $p" >&2; done
    echo "Install Intel oneAPI, or build with gfortran instead: make COMPILER=gfortran" >&2
    exit 1
fi

echo "Sourcing Intel oneAPI..."
echo "  setvars: $setvars"

# shellcheck disable=SC1090
source "$setvars" --force > /dev/null 2>&1 || true

if ! command -v ifx &>/dev/null; then
    echo "ifx is not on PATH after sourcing oneAPI. Check your oneAPI installation." >&2
    exit 1
fi

# ── Run make in the Modsimdl directory ────────────────────────────────────────
echo ""
echo "Building libmodsim.so + libUserModels.so + libModsimCurveFit.so (ifx / $BUILD)..."
cd "$DL_DIR"
make COMPILER=ifx BUILD="$BUILD" all

echo ""
echo "BUILD SUCCEEDED"
echo "  Output: $(pwd)/build/ifx/$BUILD/libmodsim.so"
echo "  Run with: make run"
