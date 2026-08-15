#!/usr/bin/env bash
#
# run.sh — Launch the ModSIM GUI (PySide6) on Linux/macOS.
#
# Points the MODSIM environment variable at the built Fortran engine
# directory, sets LD_LIBRARY_PATH so dependent .so files find each other,
# ensures the Python GUI package is installed, then launches
# `python -m modsim`.
#
# This is the Linux equivalent of scripts/run.ps1, invoked by the top-level
# GNUmakefile's `run` target on non-Windows platforms.
#
# Usage:
#   ./scripts/run.sh [compiler] [release|debug]
#
set -euo pipefail

COMPILER="${1:-ifx}"
BUILD="${2:-release}"

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(dirname "$SCRIPT_DIR")"
ENGINE_DIR="$REPO_ROOT/Modsim/Modsimdl/build/$COMPILER/$BUILD"
PY_DIR="$REPO_ROOT/Modsim/Modsimpy"

# ── Determine the engine library file name ───────────────────────────────────
OS_NAME="$(uname -s)"
case "$OS_NAME" in
    Darwin) ENGINE_LIB="$ENGINE_DIR/libmodsim.dylib" ;;
    *)      ENGINE_LIB="$ENGINE_DIR/libmodsim.so" ;;
esac

if [[ ! -f "$ENGINE_LIB" ]]; then
    echo "Engine library not found:"
    echo "  $ENGINE_LIB"
    echo ""
    echo "Build it first with:  make"
    echo "  (or: make COMPILER=gfortran)"
    exit 1
fi

# MODSIM tells the ctypes engine bridge where to find the engine library.
export MODSIM="$ENGINE_DIR"

# LD_LIBRARY_PATH (DYLD_LIBRARY_PATH on macOS) lets the dynamic linker find
# dependent shared libraries (libUserModels.so, libModsimCurveFit.so) in the
# same directory as libmodsim.so. The engine .so also has rpath=$ORIGIN, but
# setting this explicitly is belt-and-suspenders for manual runs.
if [[ "$OS_NAME" == "Darwin" ]]; then
    export DYLD_LIBRARY_PATH="$ENGINE_DIR:${DYLD_LIBRARY_PATH:-}"
else
    export LD_LIBRARY_PATH="$ENGINE_DIR:${LD_LIBRARY_PATH:-}"
fi

echo "Engine: $ENGINE_LIB"

# ── Ensure the Python GUI package is installed (one-time, idempotent) ────────
if ! python -c "import modsim" &>/dev/null; then
    echo "Installing Python GUI dependencies (first run)..."
    pip install -e "$PY_DIR"
fi

# ── Launch the GUI ────────────────────────────────────────────────────────────
echo "Launching ModSIM GUI..."
cd "$PY_DIR"
python -m modsim
