# Microsoft Developer Studio Generated NMAKE File, Based on ModsimCurveFit.dsp
!IF "$(CFG)" == ""
CFG=ModsimCurveFit - Win32 Debug
!MESSAGE No configuration specified. Defaulting to ModsimCurveFit - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "ModsimCurveFit - Win32 Release" && "$(CFG)" != "ModsimCurveFit - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ModsimCurveFit.mak" CFG="ModsimCurveFit - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ModsimCurveFit - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ModsimCurveFit - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "ModsimCurveFit - Win32 Release"

OUTDIR=.\Release
INTDIR=.\Release
# Begin Custom Macros
OutDir=.\Release
# End Custom Macros

ALL : "$(OUTDIR)\ModsimCurveFit.dll"


CLEAN :
	-@erase "$(INTDIR)\GLOBALS.MOD"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\Modelresponse.obj"
	-@erase "$(INTDIR)\ModsimCurveFit.obj"
	-@erase "$(INTDIR)\WorkingCommon.mod"
	-@erase "$(INTDIR)\WorkingCommon.obj"
	-@erase "$(OUTDIR)\ModsimCurveFit.dll"
	-@erase "$(OUTDIR)\ModsimCurveFit.exp"
	-@erase "$(OUTDIR)\ModsimCurveFit.lib"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/compile_only /dll /nologo /warn:nofileopt /module:"Release/" /object:"Release/" 
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

CPP=cl.exe
CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /Fp"$(INTDIR)\ModsimCurveFit.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "NDEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ModsimCurveFit.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\ModsimCurveFit.pdb" /machine:I386 /out:"$(OUTDIR)\ModsimCurveFit.dll" /implib:"$(OUTDIR)\ModsimCurveFit.lib" 
LINK32_OBJS= \
	"$(INTDIR)\Globals.obj" \
	"$(INTDIR)\Modelresponse.obj" \
	"$(INTDIR)\ModsimCurveFit.obj" \
	"$(INTDIR)\WorkingCommon.obj" \
	"..\ModsimMain\Release\Modsim.lib"

"$(OUTDIR)\ModsimCurveFit.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "ModsimCurveFit - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "$(OUTDIR)\ModsimCurveFit.dll"


CLEAN :
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\Modelresponse.obj"
	-@erase "$(INTDIR)\ModsimCurveFit.obj"
	-@erase "$(INTDIR)\WorkingCommon.obj"
	-@erase "$(OUTDIR)\ModsimCurveFit.dll"
	-@erase "$(OUTDIR)\ModsimCurveFit.exp"
	-@erase "$(OUTDIR)\ModsimCurveFit.lib"
	-@erase "$(OUTDIR)\ModsimCurveFit.pdb"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/check:bounds /compile_only /dbglibs /debug:full /dll /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
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

CPP=cl.exe
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /Fp"$(INTDIR)\ModsimCurveFit.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

.c{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.obj::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.c{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cpp{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

.cxx{$(INTDIR)}.sbr::
   $(CPP) @<<
   $(CPP_PROJ) $< 
<<

MTL=midl.exe
MTL_PROJ=/nologo /D "_DEBUG" /mktyplib203 /win32 
RSC=rc.exe
BSC32=bscmake.exe
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ModsimCurveFit.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\ModsimCurveFit.pdb" /debug /machine:I386 /out:"$(OUTDIR)\ModsimCurveFit.dll" /implib:"$(OUTDIR)\ModsimCurveFit.lib" /pdbtype:sept 
LINK32_OBJS= \
	"$(INTDIR)\Globals.obj" \
	"$(INTDIR)\Modelresponse.obj" \
	"$(INTDIR)\ModsimCurveFit.obj" \
	"$(INTDIR)\WorkingCommon.obj" \
	"..\ModsimMain\Release\Modsim.lib"

"$(OUTDIR)\ModsimCurveFit.dll" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("ModsimCurveFit.dep")
!INCLUDE "ModsimCurveFit.dep"
!ELSE 
!MESSAGE Warning: cannot find "ModsimCurveFit.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "ModsimCurveFit - Win32 Release" || "$(CFG)" == "ModsimCurveFit - Win32 Debug"
SOURCE=..\Globals.f90

!IF  "$(CFG)" == "ModsimCurveFit - Win32 Release"

F90_MODOUT=\
	"GLOBALS"


"$(INTDIR)\Globals.obj"	"$(INTDIR)\GLOBALS.MOD" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimCurveFit - Win32 Debug"


"$(INTDIR)\Globals.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=.\Modelresponse.f90

"$(INTDIR)\Modelresponse.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.MOD" "$(INTDIR)\WorkingCommon.mod"


SOURCE=.\ModsimCurveFit.f90

"$(INTDIR)\ModsimCurveFit.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.MOD" "$(INTDIR)\WorkingCommon.mod"


SOURCE=.\WorkingCommon.f90

!IF  "$(CFG)" == "ModsimCurveFit - Win32 Release"

F90_MODOUT=\
	"WorkingCommon"


"$(INTDIR)\WorkingCommon.obj"	"$(INTDIR)\WorkingCommon.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimCurveFit - Win32 Debug"


"$(INTDIR)\WorkingCommon.obj" : $(SOURCE) "$(INTDIR)"


!ENDIF 


!ENDIF 

