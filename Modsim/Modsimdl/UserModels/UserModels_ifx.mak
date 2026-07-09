# NMAKE file for UserModels DLL -- Intel IFX (Fortran) + cl.exe (C++) x64
#
# Usage:
#   NMAKE /f "UserModels_ifx.mak"                               (debug, default)
#   NMAKE /f "UserModels_ifx.mak" CFG="UserModels - Win32 Release"
#   NMAKE /f "UserModels_ifx.mak" CFG="UserModels - Win32 Debug"
#   NMAKE /f "UserModels_ifx.mak" CLEAN
#
# Adapted from UserModels.dsp (originally for Digital Visual Fortran df.exe + cl.exe).
# ConnectorToC.cpp is compiled with cl.exe; all .f90 files with ifx.exe.

!IF "$(CFG)" == ""
CFG=UserModels - Win32 Debug
!MESSAGE No configuration specified. Defaulting to UserModels - Win32 Debug.
!ENDIF

!IF "$(CFG)" != "UserModels - Win32 Release" && "$(CFG)" != "UserModels - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE
!MESSAGE NMAKE /f "UserModels_ifx.mak" CFG="UserModels - Win32 Debug"
!MESSAGE
!MESSAGE Possible choices for configuration are:
!MESSAGE
!MESSAGE "UserModels - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "UserModels - Win32 Debug"   (based on "Win32 (x86) Dynamic-Link Library")
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
!IF "$(CFG)" == "UserModels - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release

ALL : "$(OUTDIR)\UserModels.dll"

CLEAN :
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\modelvariables.mod"
	-@erase "$(INTDIR)\ModelVariables.obj"
	-@erase "$(INTDIR)\Utilities.obj"
	-@erase "$(INTDIR)\UserModels.obj"
	-@erase "$(INTDIR)\Connector.obj"
	-@erase "$(INTDIR)\ConnectorToC.obj"
	-@erase "$(OUTDIR)\UserModels.dll"
	-@erase "$(OUTDIR)\UserModels.exp"
	-@erase "$(OUTDIR)\UserModels.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=ifx.exe
F90_PROJ=/c /nologo /O2 /module:"$(OUTDIR)" /Fo"$(OUTDIR)/"

CPP=cl.exe
CPP_PROJ=/c /nologo /O2 /MT /W3 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_USRDLL" \
	/Fo"$(INTDIR)\\" /FD

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
	odbccp32.lib \
	/nologo /subsystem:windows /dll /incremental:no \
	/pdb:"$(OUTDIR)\UserModels.pdb" /machine:X64 \
	/out:"$(OUTDIR)\UserModels.dll" /implib:"$(OUTDIR)\UserModels.lib"

# =============================================================================
# DEBUG configuration
# =============================================================================
!ELSEIF "$(CFG)" == "UserModels - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug

ALL : "$(OUTDIR)\UserModels.dll"

CLEAN :
	-@erase "$(INTDIR)\globals.mod"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\modelvariables.mod"
	-@erase "$(INTDIR)\ModelVariables.obj"
	-@erase "$(INTDIR)\Utilities.obj"
	-@erase "$(INTDIR)\UserModels.obj"
	-@erase "$(INTDIR)\Connector.obj"
	-@erase "$(INTDIR)\ConnectorToC.obj"
	-@erase "$(OUTDIR)\UserModels.dll"
	-@erase "$(OUTDIR)\UserModels.exp"
	-@erase "$(OUTDIR)\UserModels.lib"
	-@erase "$(OUTDIR)\UserModels.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=ifx.exe
F90_PROJ=/c /nologo /Od /Zi /debug:full /traceback /check:bounds \
	/module:"$(OUTDIR)" /Fo"$(OUTDIR)/"

CPP=cl.exe
CPP_PROJ=/c /nologo /Od /Zi /MDd /W3 /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_USRDLL" \
	/Fo"$(INTDIR)\\" /FD /GZ

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib \
	advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib \
	odbccp32.lib \
	/nologo /subsystem:windows /dll /incremental:no \
	/pdb:"$(OUTDIR)\UserModels.pdb" /debug /machine:X64 \
	/out:"$(OUTDIR)\UserModels.dll" /implib:"$(OUTDIR)\UserModels.lib"

!ENDIF

# =============================================================================
# Per-file compilation rules (both configurations)
# =============================================================================
!IF "$(CFG)" == "UserModels - Win32 Release" || "$(CFG)" == "UserModels - Win32 Debug"

.SUFFIXES: .f90

LINK32_OBJS= \
	"$(INTDIR)\Globals.obj" \
	"$(INTDIR)\ModelVariables.obj" \
	"$(INTDIR)\Utilities.obj" \
	"$(INTDIR)\UserModels.obj" \
	"$(INTDIR)\Connector.obj" \
	"$(INTDIR)\ConnectorToC.obj"

"$(OUTDIR)\UserModels.dll" : "$(OUTDIR)" $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

# --- Module files (must be built first) --------------------------------------

SOURCE=..\Globals.f90
"$(INTDIR)\Globals.obj" "$(INTDIR)\globals.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=..\ModelVariables.f90
"$(INTDIR)\ModelVariables.obj" "$(INTDIR)\modelvariables.mod" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

# --- Fortran sources ---------------------------------------------------------

SOURCE=.\Utilities.f90
"$(INTDIR)\Utilities.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\modelvariables.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=.\UserModels.f90
"$(INTDIR)\UserModels.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod" "$(INTDIR)\modelvariables.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

SOURCE=.\Connector.f90
"$(INTDIR)\Connector.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\globals.mod"
	$(F90) $(F90_PROJ) $(SOURCE)

# --- C++ connector -----------------------------------------------------------

SOURCE=.\ConnectorToC.cpp
"$(INTDIR)\ConnectorToC.obj" : $(SOURCE) "$(INTDIR)"
	$(CPP) $(CPP_PROJ) $(SOURCE)

!ENDIF
