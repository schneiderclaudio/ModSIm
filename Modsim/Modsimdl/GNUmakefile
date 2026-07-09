# =============================================================================
# GNUmakefile – Build ModsimMain.dll, UserModels.dll, ModsimCurveFit.dll
# =============================================================================
#
# Usage (from the Modsimdl directory):
#
#   make                              # release build with gfortran  (default)
#   make COMPILER=ifx                 # release build with Intel IFX
#   make BUILD=debug                  # debug   build with gfortran
#   make COMPILER=ifx BUILD=debug     # debug   build with Intel IFX
#   make modsim                       # build ModsimMain.dll only
#   make usermodels                   # build UserModels.dll only
#   make curvefit                     # build ModsimCurveFit.dll only
#   make clean                        # remove ALL build artefacts
#   make COMPILER=ifx clean           # remove only the IFX build tree
#
# Output DLLs (all in build/<compiler>/<build>/):
#   ModsimMain.dll     + ModsimMain.lib
#   UserModels.dll     + UserModels.lib
#   ModsimCurveFit.dll + ModsimCurveFit.lib
#
# Build order enforced by make dependencies:
#   UserModels.dll  ->  ModsimMain.dll  ->  ModsimCurveFit.dll
#
# Notes:
#   - For IFX+Windows: both vcvarsall.bat x64 and oneAPI vars.bat must be
#     sourced before invoking make so that ifx, cl, link, and MKL are on PATH.
#   - ModsimCurveFit links MKL; the MKL lib directory must be on LIB (set by
#     oneAPI vars.bat). Required libs: mkl_intel_lp64 mkl_sequential mkl_core.
# =============================================================================

# ── OS-specific shell commands ────────────────────────────────────────────────
ifeq ($(OS),Windows_NT)
  MKDIR_P = if not exist "$(subst /,\,$1)" mkdir "$(subst /,\,$1)"
  RMRF    = if exist "$(subst /,\,$1)" rmdir /s /q "$(subst /,\,$1)"
else
  MKDIR_P = mkdir -p $1
  RMRF    = rm -rf $1
endif

# ── Tuneable variables ────────────────────────────────────────────────────────
COMPILER ?= gfortran
BUILD    ?= release

# ── Output directory tree ─────────────────────────────────────────────────────
OUTDIR := build/$(COMPILER)/$(BUILD)

# ModsimMain DLL
TARGET  := $(OUTDIR)/ModsimMain.dll
IMPLIB  := $(OUTDIR)/ModsimMain.lib

# UserModels DLL — objects/modules in UM_DIR to avoid filename collisions
UM_DIR    := $(OUTDIR)/UserModels
UM_TARGET := $(OUTDIR)/UserModels.dll
UM_IMPLIB := $(OUTDIR)/UserModels.lib

# ModsimCurveFit DLL — objects/modules in CF_DIR
CF_DIR    := $(OUTDIR)/CurveFit
CF_TARGET := $(OUTDIR)/ModsimCurveFit.dll
CF_IMPLIB := $(OUTDIR)/ModsimCurveFit.lib

# ModsimMain links against the UserModels import lib built here
USERMODELS_LIB := $(UM_IMPLIB)

# =============================================================================
# Compiler / linker configuration
# =============================================================================

# ── gfortran (MinGW / MSYS2 / Cygwin) ────────────────────────────────────────
ifeq ($(COMPILER),gfortran)
  FC := gfortran

  ifeq ($(BUILD),debug)
    FCFLAGS := -g -fcheck=all -fbacktrace -Wall -Wextra
  else
    FCFLAGS := -O2
  endif

  # Module output/search: -J writes .mod; -I searches for .mod
  MOD_FLAG    := -J$(OUTDIR)
  MODINC_FLAG := -I$(OUTDIR)
  UM_MOD_FLAG := -J$(UM_DIR) -I$(UM_DIR)
  CF_MOD_FLAG := -J$(CF_DIR) -I$(CF_DIR)

  obj_out = -o $1

  DLL_FLAGS := -shared -Wl,--out-implib,$(IMPLIB)
  SYS_LIBS  := -lkernel32 -luser32 -lgdi32 -lwinspool -lcomdlg32 \
               -ladvapi32 -lshell32 -lole32 -loleaut32 -luuid \
               -lodbc32 -lodbccp32
  IMSL_LIBS :=
  MKL_LIBS  :=

  # C++ compiler (gfortran builds exclude ConnectorToC.cpp from UM_OBJS)
  CPP       := g++
  CPP_FLAGS := -c -IUserModels
  CPP_OUT    = -o $@

  LINK_CMD    = $(FC) $(DLL_FLAGS) -o $@
  LINK_CMD_UM = $(FC) -shared -Wl,--out-implib,$(UM_IMPLIB) -o $@
  LINK_CMD_CF = $(FC) -shared -Wl,--out-implib,$(CF_IMPLIB) -o $@

# ── Intel IFX (oneAPI, Windows) ───────────────────────────────────────────────
else ifeq ($(COMPILER),ifx)
  FC := ifx

  ifeq ($(BUILD),debug)
    FCFLAGS := -Od -Zi -debug:full -traceback -check:bounds
  else
    FCFLAGS := -O2
  endif

  # IFX: a single -module:<dir> covers both .mod output and .mod search
  MOD_FLAG    := -module:$(OUTDIR)
  MODINC_FLAG :=
  UM_MOD_FLAG := -module:$(UM_DIR)
  CF_MOD_FLAG := -module:$(CF_DIR)

  obj_out = -o $1
  DLL_FLAGS :=
  IMSL_LIBS :=

  ifeq ($(OS),Windows_NT)
    ifeq ($(BUILD),debug)
      IFX_LINK_DBG := /debug
    else
      IFX_LINK_DBG :=
    endif

    SYS_LIBS  := kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
                 advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib \
                 odbc32.lib odbccp32.lib

    # MKL lib names — directory added to LIB by oneAPI vars.bat at login
    MKL_LIBS  := mkl_intel_lp64.lib mkl_sequential.lib mkl_core.lib

    # C++ compiler for UserModels/ConnectorToC.cpp
    CPP := cl.exe
    ifeq ($(BUILD),debug)
      CPP_FLAGS := /c /nologo /Od /Zi /MDd /W3 /D WIN32 /D _DEBUG \
                   /D _WINDOWS /D _USRDLL /IUserModels
    else
      CPP_FLAGS := /c /nologo /O2  /MT  /W3 /D WIN32 /D NDEBUG \
                   /D _WINDOWS /D _USRDLL /IUserModels
    endif
    # /Fo<file> with backslashes so cl.exe is happy
    CPP_OUT = /Fo$(subst /,\,$@)

    # IFX+Windows: use link.exe directly to avoid for_main.obj being pulled in
    LINK_CMD    = link.exe /nologo /dll /incremental:no /machine:X64 \
                  $(IFX_LINK_DBG) /out:$@ /implib:$(IMPLIB)
    LINK_CMD_UM = link.exe /nologo /dll /incremental:no /machine:X64 \
                  $(IFX_LINK_DBG) /out:$@ /implib:$(UM_IMPLIB)
    LINK_CMD_CF = link.exe /nologo /dll /incremental:no /machine:X64 \
                  $(IFX_LINK_DBG) /out:$@ /implib:$(CF_IMPLIB)
  else
    # IFX on Linux (future-proofing; untested)
    SYS_LIBS   :=
    MKL_LIBS   :=
    CPP        :=
    CPP_FLAGS  :=
    CPP_OUT    :=
    LINK_CMD    = $(FC) -shared -o $@
    LINK_CMD_UM = $(FC) -shared -o $@
    LINK_CMD_CF = $(FC) -shared -o $@
  endif

else
  $(error Unsupported COMPILER="$(COMPILER)". Choose gfortran or ifx)
endif

# Combined Fortran compile flags — one set per DLL (each has its own module dir)
FFLAGS    := $(FCFLAGS) $(MOD_FLAG) $(MODINC_FLAG)
UM_FFLAGS := $(FCFLAGS) $(UM_MOD_FLAG)
CF_FFLAGS := $(FCFLAGS) $(CF_MOD_FLAG)

# =============================================================================
# ModsimMain source files
#
# Ordering is significant: module-defining files MUST appear before the files
# that USE them so that .mod files are ready when the compiler needs them.
# =============================================================================

# --- Module-defining files (compiled first, in dependency order) --------------
MOD_SRCS := \
    GLOBALS.F90        \
    ModelVariables.f90 \
    SIMOPMOD.F90

# gfortran does not have Intel's PORTLIB / MSFLIB; compile stubs instead.
ifeq ($(COMPILER),gfortran)
  MOD_SRCS += portlib_stub.f90 msflib_stub.f90
endif

# --- Remaining Fortran sources ------------------------------------------------
OTHER_SRCS := \
    BETAAMD.FOR  \
    CALC.FOR     \
    CALC1.FOR    \
    CLASMODS.FOR \
    CLASREPS.FOR \
    COALMODS.FOR \
    COALREPS.FOR \
    CRSHMODS.FOR \
    CRSHREPS.FOR \
    DATAINPT.FOR \
    DMINP.FOR    \
    FLTNMODS.FOR \
    Fltnreps.for \
    GENLMODS.FOR \
    GENLREPS.FOR \
    GENLSUBS.FOR \
    GRAVMODS.FOR \
    GRAVREPS.FOR \
    LJUBAMD.FOR  \
    MAGNMODS.FOR \
    MAGNREPS.FOR \
    MILLMODS.FOR \
    MILLREPS.FOR \
    ORDER.FOR    \
    OUTPMODS.FOR \
    PPHI2.FOR    \
    Pphi5.for    \
    PPHO2.FOR    \
    REPORT.FOR   \
    REPWRT.FOR   \
    SCRNMODS.FOR \
    SCRNREPS.FOR \
    SIMOP.FOR    \
    SIMULATE.FOR \
    UNITREPS.FOR \
    UNITS.FOR    \
    WSEPMODS.FOR \
    WSEPREPS.FOR

MOD_OBJS   := $(addprefix $(OUTDIR)/,$(addsuffix .o,$(basename $(MOD_SRCS))))
OTHER_OBJS := $(addprefix $(OUTDIR)/,$(addsuffix .o,$(basename $(OTHER_SRCS))))
ALL_OBJS   := $(MOD_OBJS) $(OTHER_OBJS)

# =============================================================================
# UserModels object list
# Note: gfortran builds exclude ConnectorToC.cpp (C++ link complexity);
#       IFX builds include it as ConnectorToC.obj (MSVC COFF format).
# =============================================================================
UM_OBJS := \
    $(UM_DIR)/GLOBALS.o        \
    $(UM_DIR)/ModelVariables.o \
    $(UM_DIR)/Utilities.o      \
    $(UM_DIR)/UserModels.o     \
    $(UM_DIR)/Connector.o

ifeq ($(COMPILER),gfortran)
  # msflib_stub needed because Utilities.f90 does USE MSFLIB
  UM_OBJS += $(UM_DIR)/msflib_stub.o
else
  # C++ connector is compiled and linked only with IFX (cl.exe available)
  UM_OBJS += $(UM_DIR)/ConnectorToC.obj
endif

# =============================================================================
# ModsimCurveFit object list
# =============================================================================
CF_OBJS := \
    $(CF_DIR)/GLOBALS.o          \
    $(CF_DIR)/WorkingCommon.o    \
    $(CF_DIR)/NumericalReplace.o \
    $(CF_DIR)/minpack.o          \
    $(CF_DIR)/Modelresponse.o    \
    $(CF_DIR)/ModsimCurveFit.o

# =============================================================================
# Phony targets
# =============================================================================
.PHONY: all modsim usermodels curvefit clean

all: $(TARGET) $(UM_TARGET) $(CF_TARGET)
modsim:     $(TARGET)
usermodels: $(UM_TARGET)
curvefit:   $(CF_TARGET)

# =============================================================================
# Output directory creation
# =============================================================================
$(OUTDIR):
	$(call MKDIR_P,$(OUTDIR))

$(UM_DIR): | $(OUTDIR)
	$(call MKDIR_P,$(UM_DIR))

$(CF_DIR): | $(OUTDIR)
	$(call MKDIR_P,$(CF_DIR))

# =============================================================================
# ModsimMain: link step
# Depends on UserModels.dll so that UserModels.lib exists before linking.
# =============================================================================
$(TARGET): $(ALL_OBJS) $(UM_TARGET) | $(OUTDIR)
	$(LINK_CMD) $(ALL_OBJS) $(USERMODELS_LIB) $(SYS_LIBS) $(IMSL_LIBS)
	@echo Built: $@

# =============================================================================
# ModsimMain: explicit rules for module-defining sources
# Each one declares explicit prerequisites so make rebuilds them in order.
# =============================================================================

$(OUTDIR)/GLOBALS.o: GLOBALS.F90 | $(OUTDIR)
	$(FC) $(FFLAGS) -c $< $(call obj_out,$@)

$(OUTDIR)/ModelVariables.o: ModelVariables.f90 $(OUTDIR)/GLOBALS.o | $(OUTDIR)
	$(FC) $(FFLAGS) -c $< $(call obj_out,$@)

$(OUTDIR)/SIMOPMOD.o: SIMOPMOD.F90 | $(OUTDIR)
	$(FC) $(FFLAGS) -c $< $(call obj_out,$@)

# =============================================================================
# ModsimMain: module-use dependency declarations
# These ensure make re-compiles USEr files whenever a module file changes.
# =============================================================================

# Files that USE GLOBALS
GLOBALS_USERS := \
    $(OUTDIR)/CALC.o      \
    $(OUTDIR)/CALC1.o     \
    $(OUTDIR)/CLASMODS.o  \
    $(OUTDIR)/CLASREPS.o  \
    $(OUTDIR)/COALMODS.o  \
    $(OUTDIR)/COALREPS.o  \
    $(OUTDIR)/DMINP.o     \
    $(OUTDIR)/FLTNMODS.o  \
    $(OUTDIR)/GENLMODS.o  \
    $(OUTDIR)/GRAVMODS.o  \
    $(OUTDIR)/GRAVREPS.o  \
    $(OUTDIR)/MAGNMODS.o  \
    $(OUTDIR)/MILLMODS.o  \
    $(OUTDIR)/MILLREPS.o  \
    $(OUTDIR)/PPHI2.o     \
    $(OUTDIR)/Pphi5.o     \
    $(OUTDIR)/PPHO2.o     \
    $(OUTDIR)/REPWRT.o    \
    $(OUTDIR)/SCRNMODS.o  \
    $(OUTDIR)/SCRNREPS.o  \
    $(OUTDIR)/UNITS.o     \
    $(OUTDIR)/WSEPMODS.o  \
    $(OUTDIR)/WSEPREPS.o

$(GLOBALS_USERS): $(OUTDIR)/GLOBALS.o

# Files that USE MODELVARIABLES
$(OUTDIR)/Pphi5.o: $(OUTDIR)/ModelVariables.o

# Files that USE SIMOPMODULE
$(OUTDIR)/SIMOP.o: $(OUTDIR)/SIMOPMOD.o

# gfortran stub dependencies
ifeq ($(COMPILER),gfortran)
PORTLIB_USERS := \
    $(OUTDIR)/BETAAMD.o  \
    $(OUTDIR)/LJUBAMD.o  \
    $(OUTDIR)/MILLMODS.o \
    $(OUTDIR)/SIMOP.o    \
    $(OUTDIR)/SIMULATE.o

MSFLIB_USERS := \
    $(OUTDIR)/SIMULATE.o \
    $(OUTDIR)/SIMOP.o    \
    $(OUTDIR)/REPWRT.o   \
    $(OUTDIR)/REPORT.o   \
    $(OUTDIR)/PPHO2.o    \
    $(OUTDIR)/PPHI2.o    \
    $(OUTDIR)/ORDER.o    \
    $(OUTDIR)/LJUBAMD.o  \
    $(OUTDIR)/GENLSUBS.o \
    $(OUTDIR)/GRAVMODS.o \
    $(OUTDIR)/DMINP.o    \
    $(OUTDIR)/BETAAMD.o

$(PORTLIB_USERS): $(OUTDIR)/portlib_stub.o
$(MSFLIB_USERS):  $(OUTDIR)/msflib_stub.o
endif

# =============================================================================
# ModsimMain: pattern rules  –  compile .FOR/.for/.F90/.f90  →  .o
# The explicit rules above take precedence for the three module files.
# =============================================================================

$(OUTDIR)/%.o: %.FOR | $(OUTDIR)
	$(FC) $(FFLAGS) -c $< $(call obj_out,$@)

$(OUTDIR)/%.o: %.for | $(OUTDIR)
	$(FC) $(FFLAGS) -c $< $(call obj_out,$@)

$(OUTDIR)/%.o: %.F90 | $(OUTDIR)
	$(FC) $(FFLAGS) -c $< $(call obj_out,$@)

$(OUTDIR)/%.o: %.f90 | $(OUTDIR)
	$(FC) $(FFLAGS) -c $< $(call obj_out,$@)

# =============================================================================
# UserModels DLL: link step
# =============================================================================
$(UM_TARGET): $(UM_OBJS) | $(OUTDIR)
	$(LINK_CMD_UM) $(UM_OBJS) $(SYS_LIBS)
	@echo Built: $@

# =============================================================================
# UserModels DLL: compilation rules
#
# GLOBALS.F90 and ModelVariables.f90 are re-compiled into UM_DIR so that
# their .mod files are in UM_DIR (UM_FFLAGS uses -module:$(UM_DIR)).
# =============================================================================

$(UM_DIR)/GLOBALS.o: GLOBALS.F90 | $(UM_DIR)
	$(FC) $(UM_FFLAGS) -c $< $(call obj_out,$@)

$(UM_DIR)/ModelVariables.o: ModelVariables.f90 $(UM_DIR)/GLOBALS.o | $(UM_DIR)
	$(FC) $(UM_FFLAGS) -c $< $(call obj_out,$@)

# gfortran: Utilities.f90 does USE MSFLIB — need the stub module in UM_DIR
ifeq ($(COMPILER),gfortran)
$(UM_DIR)/msflib_stub.o: msflib_stub.f90 | $(UM_DIR)
	$(FC) $(UM_FFLAGS) -c $< $(call obj_out,$@)
$(UM_DIR)/Utilities.o: $(UM_DIR)/msflib_stub.o
endif

$(UM_DIR)/Utilities.o: UserModels/Utilities.f90 \
    $(UM_DIR)/GLOBALS.o $(UM_DIR)/ModelVariables.o | $(UM_DIR)
	$(FC) $(UM_FFLAGS) -c $< $(call obj_out,$@)

$(UM_DIR)/UserModels.o: UserModels/UserModels.f90 \
    $(UM_DIR)/GLOBALS.o $(UM_DIR)/ModelVariables.o | $(UM_DIR)
	$(FC) $(UM_FFLAGS) -c $< $(call obj_out,$@)

$(UM_DIR)/Connector.o: UserModels/Connector.f90 \
    $(UM_DIR)/GLOBALS.o | $(UM_DIR)
	$(FC) $(UM_FFLAGS) -c $< $(call obj_out,$@)

# C++ connector — IFX builds only (requires cl.exe from MSVC)
# CPP_OUT = /Fo<backslash-path> for cl.exe; -o $@ for g++
$(UM_DIR)/ConnectorToC.obj: UserModels/ConnectorToC.cpp | $(UM_DIR)
	$(CPP) $(CPP_FLAGS) $(CPP_OUT) $<

# =============================================================================
# ModsimCurveFit DLL: link step
# Depends on ModsimMain.dll so that ModsimMain.lib exists before linking.
# MKL libs are found via LIB env var (set by oneAPI vars.bat).
# =============================================================================
$(CF_TARGET): $(CF_OBJS) $(TARGET) | $(OUTDIR)
	$(LINK_CMD_CF) $(CF_OBJS) $(IMPLIB) $(MKL_LIBS) $(SYS_LIBS)
	@echo Built: $@

# =============================================================================
# ModsimCurveFit DLL: compilation rules
#
# GLOBALS.F90 is re-compiled into CF_DIR (ModsimCurveFit.f90 USEs GLOBALS).
# WorkingCommon.f90 defines the WorkingCommon module (no external USE).
# NumericalReplace.f90 defines BCLSF_State (self-contained, no external USE).
# =============================================================================

$(CF_DIR)/GLOBALS.o: GLOBALS.F90 | $(CF_DIR)
	$(FC) $(CF_FFLAGS) -c $< $(call obj_out,$@)

$(CF_DIR)/WorkingCommon.o: ModsimCurveFitDll/WorkingCommon.f90 | $(CF_DIR)
	$(FC) $(CF_FFLAGS) -c $< $(call obj_out,$@)

$(CF_DIR)/NumericalReplace.o: ModsimCurveFitDll/NumericalReplace.f90 | $(CF_DIR)
	$(FC) $(CF_FFLAGS) -c $< $(call obj_out,$@)

$(CF_DIR)/minpack.o: ModsimCurveFitDll/minpack.f90 | $(CF_DIR)
	$(FC) $(CF_FFLAGS) -c $< $(call obj_out,$@)

$(CF_DIR)/Modelresponse.o: ModsimCurveFitDll/Modelresponse.f90 \
    $(CF_DIR)/WorkingCommon.o | $(CF_DIR)
	$(FC) $(CF_FFLAGS) -c $< $(call obj_out,$@)

$(CF_DIR)/ModsimCurveFit.o: ModsimCurveFitDll/ModsimCurveFit.f90 \
    $(CF_DIR)/GLOBALS.o $(CF_DIR)/WorkingCommon.o | $(CF_DIR)
	$(FC) $(CF_FFLAGS) -c $< $(call obj_out,$@)

# =============================================================================
# Clean
# =============================================================================
clean:
	$(call RMRF,build/$(COMPILER)/$(BUILD))
	@echo Cleaned build/$(COMPILER)/$(BUILD)
