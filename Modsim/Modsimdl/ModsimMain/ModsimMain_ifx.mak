# NMAKE file for ModsimMain DLL -- Intel IFX compiler (oneAPI)
#
# Usage:
#   NMAKE /f "ModsimMain_ifx.mak"                          (debug, default)
#   NMAKE /f "ModsimMain_ifx.mak" CFG="ModsimMain - Win32 Release"
#   NMAKE /f "ModsimMain_ifx.mak" CFG="ModsimMain - Win32 Debug"
#   NMAKE /f "ModsimMain_ifx.mak" CLEAN
#
# Adapted from ModsimMain.mak (originally for Digital Visual Fortran df.exe).
# Compiler changed to ifx.exe (Intel oneAPI IFX).

!IF "$(CFG)" == ""
CFG=ModsimMain - Win32 Debug
!MESSAGE No configuration specified. Defaulting to ModsimMain - Win32 Debug.
!ENDIF

!IF "$(CFG)" != "ModsimMain - Win32 Release" && "$(CFG)" != "ModsimMain - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "ModsimMain_ifx.mak" CFG="ModsimMain - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "ModsimMain - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ModsimMain - Win32 Debug"   (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE
!ERROR An invalid configuration is specified.
!ENDIF

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE
NULL=nul
!ENDIF

# =============================================================================
# RELEASE configuration
# =============================================================================
!IF "$(CFG)" == "ModsimMain - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : "..\Modsim.dll"

CLEAN :
	-@erase "$(INTDIR)\Betaamd.obj"
	-@erase "$(INTDIR)\Calc.obj"
	-@erase "$(INTDIR)\Calc1.obj"
	-@erase "$(INTDIR)\Clasmods.obj"
	-@erase "$(INTDIR)\Clasreps.obj"
	-@erase "$(INTDIR)\Coalmods.obj"
	-@erase "$(INTDIR)\Coalreps.obj"
	-@erase "$(INTDIR)\Crshmods.obj"
	-@erase "$(INTDIR)\Crshreps.obj"
	-@erase "$(INTDIR)\Datainpt.obj"
	-@erase "$(INTDIR)\Dminp.obj"
	-@erase "$(INTDIR)\Fltnmods.obj"
	-@erase "$(INTDIR)\Fltnreps.obj"
	-@erase "$(INTDIR)\Genlmods.obj"
	-@erase "$(INTDIR)\Genlreps.obj"
	-@erase "$(INTDIR)\Genlsubs.obj"
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\Gravmods.obj"
	-@erase "$(INTDIR)\Gravreps.obj"
	-@erase "$(INTDIR)\Ljubamd.obj"
	-@erase "$(INTDIR)\Magnmods.obj"
	-@erase "$(INTDIR)\Magnreps.obj"
	-@erase "$(INTDIR)\Millmods.obj"
	-@erase "$(INTDIR)\Millreps.obj"
	-@erase "$(INTDIR)\modelvariables.mod"
	-@erase "$(INTDIR)\ModelVariables.obj"
	-@erase "$(INTDIR)\simopmodule.mod"
	-@erase "$(INTDIR)\Simopmod.obj"
	-@erase "$(INTDIR)\Order.obj"
	-@erase "$(INTDIR)\Outpmods.obj"
	-@erase "$(INTDIR)\Pphi2.obj"
	-@erase "$(INTDIR)\Pphi5.obj"
	-@erase "$(INTDIR)\Ppho2.obj"
	-@erase "$(INTDIR)\Report.obj"
	-@erase "$(INTDIR)\Repwrt.obj"
	-@erase "$(INTDIR)\Scrnmods.obj"
	-@erase "$(INTDIR)\Scrnreps.obj"
	-@erase "$(INTDIR)\Simop.obj"
	-@erase "$(INTDIR)\Simulate.obj"
	-@erase "$(INTDIR)\Unitreps.obj"
	-@erase "$(INTDIR)\Units.obj"
	-@erase "$(INTDIR)\Wsepmods.obj"
	-@erase "$(INTDIR)\Wsepreps.obj"
	-@erase "$(OUTDIR)\Modsim.exp"
	-@erase "$(OUTDIR)\Modsim.lib"
	-@erase "..\Modsim.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# IFX compile flags -- Release
#   /c            compile only (no link)
#   /nologo       suppress banner
#   /O2           optimise
#   /module:dir   module search + output directory
#   /Fo"dir/"     object output directory (forward slash avoids NMAKE quoting bug)
F90=ifx.exe
F90_PROJ=/c /nologo /O2 /module:"Release" /Fo"Release/"
F90_OBJS=.\Release/

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
	odbccp32.lib \
	/nologo /subsystem:windows /dll /incremental:no \
	/pdb:"$(OUTDIR)\Modsim.pdb" /machine:X64 \
	/out:"../Modsim.dll" /implib:"$(OUTDIR)\Modsim.lib"
LINK32_OBJS= \
	"$(INTDIR)\Betaamd.obj" \
	"$(INTDIR)\Calc.obj" \
	"$(INTDIR)\Calc1.obj" \
	"$(INTDIR)\Clasmods.obj" \
	"$(INTDIR)\Clasreps.obj" \
	"$(INTDIR)\Coalmods.obj" \
	"$(INTDIR)\Coalreps.obj" \
	"$(INTDIR)\Crshmods.obj" \
	"$(INTDIR)\Crshreps.obj" \
	"$(INTDIR)\Datainpt.obj" \
	"$(INTDIR)\Dminp.obj" \
	"$(INTDIR)\Fltnmods.obj" \
	"$(INTDIR)\Fltnreps.obj" \
	"$(INTDIR)\Genlmods.obj" \
	"$(INTDIR)\Genlreps.obj" \
	"$(INTDIR)\Genlsubs.obj" \
	"$(INTDIR)\Globals.obj" \
	"$(INTDIR)\Gravmods.obj" \
	"$(INTDIR)\Gravreps.obj" \
	"$(INTDIR)\Ljubamd.obj" \
	"$(INTDIR)\Magnmods.obj" \
	"$(INTDIR)\Magnreps.obj" \
	"$(INTDIR)\Millmods.obj" \
	"$(INTDIR)\Millreps.obj" \
	"$(INTDIR)\ModelVariables.obj" \
	"$(INTDIR)\Simopmod.obj" \
	"$(INTDIR)\Order.obj" \
	"$(INTDIR)\Outpmods.obj" \
	"$(INTDIR)\Pphi2.obj" \
	"$(INTDIR)\Pphi5.obj" \
	"$(INTDIR)\Ppho2.obj" \
	"$(INTDIR)\Report.obj" \
	"$(INTDIR)\Repwrt.obj" \
	"$(INTDIR)\Scrnmods.obj" \
	"$(INTDIR)\Scrnreps.obj" \
	"$(INTDIR)\Simop.obj" \
	"$(INTDIR)\Simulate.obj" \
	"$(INTDIR)\Unitreps.obj" \
	"$(INTDIR)\Units.obj" \
	"$(INTDIR)\Wsepmods.obj" \
	"$(INTDIR)\Wsepreps.obj" \
	"..\UserModels\Release\UserModels.lib"

"..\Modsim.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

# =============================================================================
# DEBUG configuration
# =============================================================================
!ELSEIF "$(CFG)" == "ModsimMain - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "..\Modsim.dll"

CLEAN :
	-@erase "$(INTDIR)\Betaamd.obj"
	-@erase "$(INTDIR)\Calc.obj"
	-@erase "$(INTDIR)\Calc1.obj"
	-@erase "$(INTDIR)\Clasmods.obj"
	-@erase "$(INTDIR)\Clasreps.obj"
	-@erase "$(INTDIR)\Coalmods.obj"
	-@erase "$(INTDIR)\Coalreps.obj"
	-@erase "$(INTDIR)\Crshmods.obj"
	-@erase "$(INTDIR)\Crshreps.obj"
	-@erase "$(INTDIR)\Datainpt.obj"
	-@erase "$(INTDIR)\Dminp.obj"
	-@erase "$(INTDIR)\Fltnmods.obj"
	-@erase "$(INTDIR)\Fltnreps.obj"
	-@erase "$(INTDIR)\Genlmods.obj"
	-@erase "$(INTDIR)\Genlreps.obj"
	-@erase "$(INTDIR)\Genlsubs.obj"
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\Gravmods.obj"
	-@erase "$(INTDIR)\Gravreps.obj"
	-@erase "$(INTDIR)\Ljubamd.obj"
	-@erase "$(INTDIR)\Magnmods.obj"
	-@erase "$(INTDIR)\Magnreps.obj"
	-@erase "$(INTDIR)\Millmods.obj"
	-@erase "$(INTDIR)\Millreps.obj"
	-@erase "$(INTDIR)\modelvariables.mod"
	-@erase "$(INTDIR)\ModelVariables.obj"
	-@erase "$(INTDIR)\simopmodule.mod"
	-@erase "$(INTDIR)\Simopmod.obj"
	-@erase "$(INTDIR)\Order.obj"
	-@erase "$(INTDIR)\Outpmods.obj"
	-@erase "$(INTDIR)\Pphi2.obj"
	-@erase "$(INTDIR)\Pphi5.obj"
	-@erase "$(INTDIR)\Ppho2.obj"
	-@erase "$(INTDIR)\Report.obj"
	-@erase "$(INTDIR)\Repwrt.obj"
	-@erase "$(INTDIR)\Scrnmods.obj"
	-@erase "$(INTDIR)\Scrnreps.obj"
	-@erase "$(INTDIR)\Simop.obj"
	-@erase "$(INTDIR)\Simulate.obj"
	-@erase "$(INTDIR)\Unitreps.obj"
	-@erase "$(INTDIR)\Units.obj"
	-@erase "$(INTDIR)\Wsepmods.obj"
	-@erase "$(INTDIR)\Wsepreps.obj"
	-@erase "$(OUTDIR)\Modsim.exp"
	-@erase "$(OUTDIR)\Modsim.pdb"
	-@erase "..\Modsim.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

# IFX compile flags -- Debug
#   /c            compile only
#   /nologo       suppress banner
#   /Od           disable optimisation
#   /Zi           full debug info (PDB)
#   /debug:full   embed full debug symbols
#   /traceback    enable run-time traceback on error
#   /check:bounds enable array bounds checking
#   /warn:interfaces  warn on argument type mismatches (IFX equivalent of argument_checking)
#   /module:dir   module search + output directory
#   /Fo"dir/"     object output directory (forward slash avoids NMAKE quoting bug)
F90=ifx.exe
F90_PROJ=/c /nologo /Od /Zi /debug:full /traceback /check:bounds \
	/module:"Debug" /Fo"Debug/"
F90_OBJS=.\Debug/

.SUFFIXES: .fpp

.for{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.f{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.f90{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

.fpp{$(F90_OBJS)}.obj:
   $(F90) $(F90_PROJ) $<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
	odbccp32.lib \
	/nologo /subsystem:windows /dll /incremental:no \
	/pdb:"$(OUTDIR)\Modsim.pdb" /debug /machine:X64 \
	/out:"../Modsim.dll" /implib:"$(OUTDIR)\Modsim.lib"
LINK32_OBJS= \
	"$(INTDIR)\Betaamd.obj" \
	"$(INTDIR)\Calc.obj" \
	"$(INTDIR)\Calc1.obj" \
	"$(INTDIR)\Clasmods.obj" \
	"$(INTDIR)\Clasreps.obj" \
	"$(INTDIR)\Coalmods.obj" \
	"$(INTDIR)\Coalreps.obj" \
	"$(INTDIR)\Crshmods.obj" \
	"$(INTDIR)\Crshreps.obj" \
	"$(INTDIR)\Datainpt.obj" \
	"$(INTDIR)\Dminp.obj" \
	"$(INTDIR)\Fltnmods.obj" \
	"$(INTDIR)\Fltnreps.obj" \
	"$(INTDIR)\Genlmods.obj" \
	"$(INTDIR)\Genlreps.obj" \
	"$(INTDIR)\Genlsubs.obj" \
	"$(INTDIR)\Globals.obj" \
	"$(INTDIR)\Gravmods.obj" \
	"$(INTDIR)\Gravreps.obj" \
	"$(INTDIR)\Ljubamd.obj" \
	"$(INTDIR)\Magnmods.obj" \
	"$(INTDIR)\Magnreps.obj" \
	"$(INTDIR)\Millmods.obj" \
	"$(INTDIR)\Millreps.obj" \
	"$(INTDIR)\ModelVariables.obj" \
	"$(INTDIR)\Simopmod.obj" \
	"$(INTDIR)\Order.obj" \
	"$(INTDIR)\Outpmods.obj" \
	"$(INTDIR)\Pphi2.obj" \
	"$(INTDIR)\Pphi5.obj" \
	"$(INTDIR)\Ppho2.obj" \
	"$(INTDIR)\Report.obj" \
	"$(INTDIR)\Repwrt.obj" \
	"$(INTDIR)\Scrnmods.obj" \
	"$(INTDIR)\Scrnreps.obj" \
	"$(INTDIR)\Simop.obj" \
	"$(INTDIR)\Simulate.obj" \
	"$(INTDIR)\Unitreps.obj" \
	"$(INTDIR)\Units.obj" \
	"$(INTDIR)\Wsepmods.obj" \
	"$(INTDIR)\Wsepreps.obj" \
	"..\UserModels\Debug\UserModels.lib"

"..\Modsim.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF

# =============================================================================
# Per-file compilation rules (both configurations)
# Module-producing files come first to ensure .mod files exist for USErs.
# =============================================================================
!IF "$(CFG)" == "ModsimMain - Win32 Release" || "$(CFG)" == "ModsimMain - Win32 Debug"

# --- Module files (must be built first) --------------------------------------

SOURCE=..\Globals.f90
"$(INTDIR)\Globals.obj" "$(INTDIR)\globals.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\ModelVariables.f90
"$(INTDIR)\ModelVariables.obj" "$(INTDIR)\modelvariables.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\SIMOPMOD.f90
"$(INTDIR)\Simopmod.obj" "$(INTDIR)\simopmodule.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

# --- Files that USE GLOBALS --------------------------------------------------

SOURCE=..\Betaamd.for
"$(INTDIR)\Betaamd.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Calc.for
"$(INTDIR)\Calc.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Calc1.for
"$(INTDIR)\Calc1.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Clasmods.for
"$(INTDIR)\Clasmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Clasreps.for
"$(INTDIR)\Clasreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Coalmods.for
"$(INTDIR)\Coalmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Coalreps.for
"$(INTDIR)\Coalreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Crshmods.for
"$(INTDIR)\Crshmods.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Crshreps.for
"$(INTDIR)\Crshreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Datainpt.for
"$(INTDIR)\Datainpt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Dminp.for
"$(INTDIR)\Dminp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Fltnmods.for
"$(INTDIR)\Fltnmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Fltnreps.for
"$(INTDIR)\Fltnreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Genlmods.for
"$(INTDIR)\Genlmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Genlreps.for
"$(INTDIR)\Genlreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Genlsubs.for
"$(INTDIR)\Genlsubs.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Gravmods.for
"$(INTDIR)\Gravmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Gravreps.for
"$(INTDIR)\Gravreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Ljubamd.for
"$(INTDIR)\Ljubamd.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Magnmods.for
"$(INTDIR)\Magnmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Magnreps.for
"$(INTDIR)\Magnreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Millmods.for
"$(INTDIR)\Millmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Millreps.for
"$(INTDIR)\Millreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Order.for
"$(INTDIR)\Order.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Outpmods.for
"$(INTDIR)\Outpmods.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Pphi2.for
"$(INTDIR)\Pphi2.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Pphi5.for
"$(INTDIR)\Pphi5.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\modelvariables.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Ppho2.for
"$(INTDIR)\Ppho2.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Report.for
"$(INTDIR)\Report.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Repwrt.for
"$(INTDIR)\Repwrt.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Scrnmods.for
"$(INTDIR)\Scrnmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Scrnreps.for
"$(INTDIR)\Scrnreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Simop.for
"$(INTDIR)\Simop.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\simopmodule.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Simulate.for
"$(INTDIR)\Simulate.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Unitreps.for
"$(INTDIR)\Unitreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Units.for
"$(INTDIR)\Units.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Wsepmods.for
"$(INTDIR)\Wsepmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\Wsepreps.for
"$(INTDIR)\Wsepreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

!ENDIF
