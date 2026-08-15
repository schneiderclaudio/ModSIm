# =============================================================================
# ModSIM — top-level developer entry point (cross-platform)
# =============================================================================
#
# One command to build, one command to run — works on Windows and Linux:
#
#   make              build the Fortran engine (default: ifx release)
#   make run          launch the ModSIM GUI
#   make install      install the Python GUI dependencies (one-time)
#   make clean        remove build artefacts
#   make help         show this help
#
# Compiler / build overrides:
#
#   make COMPILER=gfortran          build with gfortran (no oneAPI needed)
#   make COMPILER=ifx BUILD=debug   IFX debug build
#   make run COMPILER=gfortran      run against a gfortran build tree
#
# IFX (the default):
#   Windows — scripts/build-ifx.ps1 auto-detects VS2022 + oneAPI
#   Linux   — scripts/build-ifx.sh  auto-detects oneAPI setvars.sh
#
# gfortran needs only gfortran + make on PATH (no env sourcing).
# =============================================================================

# ── Defaults ─────────────────────────────────────────────────────────────────
COMPILER ?= ifx
BUILD    ?= release

# ── Paths ─────────────────────────────────────────────────────────────────────
DL_DIR  := Modsim/Modsimdl
PY_DIR  := Modsim/Modsimpy
OUTDIR  := $(DL_DIR)/build/$(COMPILER)/$(BUILD)

# ── Platform detection ───────────────────────────────────────────────────────
# Windows: use PowerShell helpers for IFX env sourcing + GUI launch.
# Linux/macOS: use bash helpers.
ifeq ($(OS),Windows_NT)
  SH := powershell -NoProfile -ExecutionPolicy Bypass
  BUILD_IFX = $(SH) -File scripts/build-ifx.ps1 -Build $(BUILD)
  RUN_GUI   = $(SH) -File scripts/run.ps1 -Compiler $(COMPILER) -Build $(BUILD)
  ENGINE_LIB := ModsimMain.dll
else
  SH := bash
  BUILD_IFX = $(SH) scripts/build-ifx.sh $(BUILD)
  RUN_GUI   = $(SH) scripts/run.sh $(COMPILER) $(BUILD)
  ENGINE_LIB := libmodsim.so
endif

.DEFAULT_GOAL := build

.PHONY: help build all run install clean

help:
	@echo "ModSIM — commands:"
	@echo ""
	@echo "  make              build the Fortran engine (COMPILER=$(COMPILER), BUILD=$(BUILD))"
	@echo "  make run          launch the ModSIM GUI"
	@echo "  make install      install Python GUI dependencies (one-time)"
	@echo "  make clean        remove build artefacts"
	@echo "  make help         show this help"
	@echo ""
	@echo "Overrides:"
	@echo "  make COMPILER=gfortran          use gfortran (no oneAPI needed)"
	@echo "  make COMPILER=ifx BUILD=debug   IFX debug build"
	@echo "  make run COMPILER=gfortran      run against a gfortran build tree"

# ── Build ────────────────────────────────────────────────────────────────────
build: all

all:
ifeq ($(COMPILER),ifx)
	$(BUILD_IFX)
else
	$(MAKE) -C $(DL_DIR) COMPILER=$(COMPILER) BUILD=$(BUILD) all
	@echo ""
	@echo "BUILD SUCCEEDED ($(COMPILER)/$(BUILD))"
	@echo "  Output: $(OUTDIR)/$(ENGINE_LIB)"
	@echo "  Run with: make run"
endif

# ── Run ──────────────────────────────────────────────────────────────────────
run:
	$(RUN_GUI)

# ── Install Python GUI deps ──────────────────────────────────────────────────
install:
	pip install -e $(PY_DIR)

# ── Clean ────────────────────────────────────────────────────────────────────
clean:
	$(MAKE) -C $(DL_DIR) COMPILER=$(COMPILER) BUILD=$(BUILD) clean
