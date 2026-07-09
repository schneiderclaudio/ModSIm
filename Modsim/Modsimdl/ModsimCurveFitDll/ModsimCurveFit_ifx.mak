# NMAKE file for ModsimCurveFit DLL -- Intel IFX compiler (oneAPI)
#
# Usage:
#   NMAKE /f "ModsimCurveFit_ifx.mak"                               (debug, default)
#   NMAKE /f "ModsimCurveFit_ifx.mak" CFG="ModsimCurveFit - Win32 Release"
#   NMAKE /f "ModsimCurveFit_ifx.mak" CFG="ModsimCurveFit - Win32 Debug"
#   NMAKE /f "ModsimCurveFit_ifx.mak" CLEAN
#
# Adapted from ModsimCurveFit.mak (originally for Digital Visual Fortran df.exe).
# IMSL replaced by NumericalReplace.f90 (pure-Fortran/MKL) and minpack.f90 (MINPACK LM).
# MKL static libs required at link time.

!IF "$(CFG)" == ""
CFG=ModsimCurveFit - Win32 Debug
!MESSAGE No configuration specified. Defaulting to ModsimCurveFit - Win32 Debug.
!ENDIF

!IF "$(CFG)" != "ModsimCurveFit - Win32 Release" && "$(CFG)" != "ModsimCurveFit - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "ModsimCurveFit_ifx.mak" CFG="ModsimCurveFit - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "ModsimCurveFit - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ModsimCurveFit - Win32 Debug"   (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE
!ERROR An invalid configuration is specified.
!ENDIF

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF

# MKL static libraries (Intel oneAPI MKL, LP64, sequential)
MKLROOT=N:\Program Files (x86)\Intel\oneAPI\mkl\2025.3
MKL_LIBS="$(MKLROOT)\lib\mkl_intel_lp64.lib" \
	"$(MKLROOT)\lib\mkl_sequential.lib" \
	"$(MKLROOT)\lib\mkl_core.lib"

# =============================================================================
# RELEASE configuration
# =============================================================================
!IF "$(CFG)" == "ModsimCurveFit - Win32 Release"

OUTDIR=.\Release_ifx
INTDIR=.\Release_ifx

ALL : "$(OUTDIR)\ModsimCurveFit.dll"

CLEAN :
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\workingcommon.mod"
	-@erase "$(INTDIR)\WorkingCommon.obj"
	-@erase "$(INTDIR)\NumericalReplace.obj"
	-@erase "$(INTDIR)\minpack.obj"
	-@erase "$(INTDIR)\Modelresponse.obj"
	-@erase "$(INTDIR)\ModsimCurveFit.obj"
	-@erase "$(OUTDIR)\ModsimCurveFit.dll"
	-@erase "$(OUTDIR)\ModsimCurveFit.exp"
	-@erase "$(OUTDIR)\ModsimCurveFit.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=ifx.exe
F90_PROJ=/c /nologo /O2 /module:"$(OUTDIR)" /Fo"$(OUTDIR)/"
F90_OBJS=.\Release_ifx/

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
	odbccp32.lib $(MKL_LIBS) \
	/nologo /subsystem:windows /dll /incremental:no \
	/pdb:"$(OUTDIR)\ModsimCurveFit.pdb" /machine:X64 \
	/out:"$(OUTDIR)\ModsimCurveFit.dll" /implib:"$(OUTDIR)\ModsimCurveFit.lib"

# =============================================================================
# DEBUG configuration
# =============================================================================
!ELSEIF "$(CFG)" == "ModsimCurveFit - Win32 Debug"

OUTDIR=.\Debug_ifx
INTDIR=.\Debug_ifx

ALL : "$(OUTDIR)\ModsimCurveFit.dll"

CLEAN :
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\workingcommon.mod"
	-@erase "$(INTDIR)\WorkingCommon.obj"
	-@erase "$(INTDIR)\NumericalReplace.obj"
	-@erase "$(INTDIR)\minpack.obj"
	-@erase "$(INTDIR)\Modelresponse.obj"
	-@erase "$(INTDIR)\ModsimCurveFit.obj"
	-@erase "$(OUTDIR)\ModsimCurveFit.dll"
	-@erase "$(OUTDIR)\ModsimCurveFit.exp"
	-@erase "$(OUTDIR)\ModsimCurveFit.lib"
	-@erase "$(OUTDIR)\ModsimCurveFit.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=ifx.exe
F90_PROJ=/c /nologo /Od /Zi /debug:full /traceback /check:bounds \
	/module:"$(OUTDIR)" /Fo"$(OUTDIR)/"
F90_OBJS=.\Debug_ifx/

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
	odbccp32.lib $(MKL_LIBS) \
	/nologo /subsystem:windows /dll /incremental:no \
	/pdb:"$(OUTDIR)\ModsimCurveFit.pdb" /debug /machine:X64 \
	/out:"$(OUTDIR)\ModsimCurveFit.dll" /implib:"$(OUTDIR)\ModsimCurveFit.lib"

!ENDIF

# =============================================================================
# Suffix rules (both configurations)
# =============================================================================
!IF "$(CFG)" == "ModsimCurveFit - Win32 Release" || "$(CFG)" == "ModsimCurveFit - Win32 Debug"

.SUFFIXES: .for .f .f90 .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

LINK32_OBJS= \
	"$(INTDIR)\Globals.obj" \
	"$(INTDIR)\WorkingCommon.obj" \
	"$(INTDIR)\NumericalReplace.obj" \
	"$(INTDIR)\minpack.obj" \
	"$(INTDIR)\Modelresponse.obj" \
	"$(INTDIR)\ModsimCurveFit.obj" \
	"..\ModsimMain\$(OUTDIR)\Modsim.lib"

"$(OUTDIR)\ModsimCurveFit.dll" : "$(OUTDIR)" $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

# =============================================================================
# Per-file compilation rules
# Module-producing files come first to ensure .mod files exist for USErs.
# =============================================================================

SOURCE=..\Globals.f90
"$(INTDIR)\Globals.obj" "$(INTDIR)\globals.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=.\WorkingCommon.f90
"$(INTDIR)\WorkingCommon.obj" "$(INTDIR)\workingcommon.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=.\NumericalReplace.f90
"$(INTDIR)\NumericalReplace.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=.\minpack.f90
"$(INTDIR)\minpack.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=.\Modelresponse.f90
"$(INTDIR)\Modelresponse.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\workingcommon.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=.\ModsimCurveFit.f90
"$(INTDIR)\ModsimCurveFit.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\workingcommon.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

!ENDIF
