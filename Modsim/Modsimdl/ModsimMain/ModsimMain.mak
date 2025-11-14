# Microsoft Developer Studio Generated NMAKE File, Based on ModsimMain.dsp
!IF "$(CFG)" == ""
CFG=ModsimMain - Win32 Debug
!MESSAGE No configuration specified. Defaulting to ModsimMain - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "ModsimMain - Win32 Release" && "$(CFG)" != "ModsimMain - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE
!MESSAGE by defining the macro CFG on the command line. For example:
!MESSAGE 
!MESSAGE NMAKE /f "ModsimMain.mak" CFG="ModsimMain - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "ModsimMain - Win32 Release" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE "ModsimMain - Win32 Debug" (based on "Win32 (x86) Dynamic-Link Library")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 

!IF  "$(CFG)" == "ModsimMain - Win32 Release"

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
	-@erase "$(INTDIR)\GLOBALS.mod"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\Gravmods.obj"
	-@erase "$(INTDIR)\Gravreps.obj"
	-@erase "$(INTDIR)\Ljubamd.obj"
	-@erase "$(INTDIR)\Magnmods.obj"
	-@erase "$(INTDIR)\Magnreps.obj"
	-@erase "$(INTDIR)\Millmods.obj"
	-@erase "$(INTDIR)\Millreps.obj"
	-@erase "$(INTDIR)\MODELVARIABLES.MOD"
	-@erase "$(INTDIR)\ModelVariables.obj"
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
CPP_PROJ=/nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /Fp"$(INTDIR)\ModsimMain.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ModsimMain.bsc" 
BSC32_SBRS= \
	
LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib imsl.lib IMSLMPISTUB.LIB IMSLS_ERR.LIB /nologo /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\Modsim.pdb" /machine:I386 /out:"../Modsim.dll" /implib:"$(OUTDIR)\Modsim.lib" 
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

!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"

OUTDIR=.\Debug
INTDIR=.\Debug
# Begin Custom Macros
OutDir=.\Debug
# End Custom Macros

ALL : "..\Modsim.dll" "$(OUTDIR)\ModsimMain.bsc"


CLEAN :
	-@erase "$(INTDIR)\Betaamd.obj"
	-@erase "$(INTDIR)\Betaamd.sbr"
	-@erase "$(INTDIR)\Calc.obj"
	-@erase "$(INTDIR)\Calc.sbr"
	-@erase "$(INTDIR)\Calc1.obj"
	-@erase "$(INTDIR)\Calc1.sbr"
	-@erase "$(INTDIR)\Clasmods.obj"
	-@erase "$(INTDIR)\Clasmods.sbr"
	-@erase "$(INTDIR)\Clasreps.obj"
	-@erase "$(INTDIR)\Clasreps.sbr"
	-@erase "$(INTDIR)\Coalmods.obj"
	-@erase "$(INTDIR)\Coalmods.sbr"
	-@erase "$(INTDIR)\Coalreps.obj"
	-@erase "$(INTDIR)\Coalreps.sbr"
	-@erase "$(INTDIR)\Crshmods.obj"
	-@erase "$(INTDIR)\Crshmods.sbr"
	-@erase "$(INTDIR)\Crshreps.obj"
	-@erase "$(INTDIR)\Crshreps.sbr"
	-@erase "$(INTDIR)\Datainpt.obj"
	-@erase "$(INTDIR)\Datainpt.sbr"
	-@erase "$(INTDIR)\DF60.PDB"
	-@erase "$(INTDIR)\Dminp.obj"
	-@erase "$(INTDIR)\Dminp.sbr"
	-@erase "$(INTDIR)\Fltnmods.obj"
	-@erase "$(INTDIR)\Fltnmods.sbr"
	-@erase "$(INTDIR)\Fltnreps.obj"
	-@erase "$(INTDIR)\Fltnreps.sbr"
	-@erase "$(INTDIR)\Genlmods.obj"
	-@erase "$(INTDIR)\Genlmods.sbr"
	-@erase "$(INTDIR)\Genlreps.obj"
	-@erase "$(INTDIR)\Genlreps.sbr"
	-@erase "$(INTDIR)\Genlsubs.obj"
	-@erase "$(INTDIR)\Genlsubs.sbr"
	-@erase "$(INTDIR)\Globals.obj"
	-@erase "$(INTDIR)\Globals.sbr"
	-@erase "$(INTDIR)\Gravmods.obj"
	-@erase "$(INTDIR)\Gravmods.sbr"
	-@erase "$(INTDIR)\Gravreps.obj"
	-@erase "$(INTDIR)\Gravreps.sbr"
	-@erase "$(INTDIR)\Ljubamd.obj"
	-@erase "$(INTDIR)\Ljubamd.sbr"
	-@erase "$(INTDIR)\Magnmods.obj"
	-@erase "$(INTDIR)\Magnmods.sbr"
	-@erase "$(INTDIR)\Magnreps.obj"
	-@erase "$(INTDIR)\Magnreps.sbr"
	-@erase "$(INTDIR)\Millmods.obj"
	-@erase "$(INTDIR)\Millmods.sbr"
	-@erase "$(INTDIR)\Millreps.obj"
	-@erase "$(INTDIR)\Millreps.sbr"
	-@erase "$(INTDIR)\ModelVariables.obj"
	-@erase "$(INTDIR)\ModelVariables.sbr"
	-@erase "$(INTDIR)\Order.obj"
	-@erase "$(INTDIR)\Order.sbr"
	-@erase "$(INTDIR)\Outpmods.obj"
	-@erase "$(INTDIR)\Outpmods.sbr"
	-@erase "$(INTDIR)\Pphi2.obj"
	-@erase "$(INTDIR)\Pphi2.sbr"
	-@erase "$(INTDIR)\Pphi5.obj"
	-@erase "$(INTDIR)\Pphi5.sbr"
	-@erase "$(INTDIR)\Ppho2.obj"
	-@erase "$(INTDIR)\Ppho2.sbr"
	-@erase "$(INTDIR)\Report.obj"
	-@erase "$(INTDIR)\Report.sbr"
	-@erase "$(INTDIR)\Repwrt.obj"
	-@erase "$(INTDIR)\Repwrt.sbr"
	-@erase "$(INTDIR)\Scrnmods.obj"
	-@erase "$(INTDIR)\Scrnmods.sbr"
	-@erase "$(INTDIR)\Scrnreps.obj"
	-@erase "$(INTDIR)\Scrnreps.sbr"
	-@erase "$(INTDIR)\Simop.obj"
	-@erase "$(INTDIR)\Simop.sbr"
	-@erase "$(INTDIR)\Simulate.obj"
	-@erase "$(INTDIR)\Simulate.sbr"
	-@erase "$(INTDIR)\Unitreps.obj"
	-@erase "$(INTDIR)\Unitreps.sbr"
	-@erase "$(INTDIR)\Units.obj"
	-@erase "$(INTDIR)\Units.sbr"
	-@erase "$(INTDIR)\Wsepmods.obj"
	-@erase "$(INTDIR)\Wsepmods.sbr"
	-@erase "$(INTDIR)\Wsepreps.obj"
	-@erase "$(INTDIR)\Wsepreps.sbr"
	-@erase "$(OUTDIR)\Modsim.exp"
	-@erase "$(OUTDIR)\Modsim.pdb"
	-@erase "$(OUTDIR)\ModsimMain.bsc"
	-@erase "..\Modsim.dll"

"$(OUTDIR)" :
    if not exist "$(OUTDIR)/$(NULL)" mkdir "$(OUTDIR)"

F90=df.exe
F90_PROJ=/browser:"Debug/" /check:bounds /compile_only /dbglibs /debug:full /dll /nologo /traceback /warn:argument_checking /warn:nofileopt /module:"Debug/" /object:"Debug/" /pdbfile:"Debug/DF60.PDB" 
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
CPP_PROJ=/nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /FR"$(INTDIR)\\" /Fp"$(INTDIR)\ModsimMain.pch" /YX /Fo"$(INTDIR)\\" /Fd"$(INTDIR)\\" /FD /GZ /c 

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
BSC32_FLAGS=/nologo /o"$(OUTDIR)\ModsimMain.bsc" 
BSC32_SBRS= \
	"$(INTDIR)\Betaamd.sbr" \
	"$(INTDIR)\Calc.sbr" \
	"$(INTDIR)\Calc1.sbr" \
	"$(INTDIR)\Clasmods.sbr" \
	"$(INTDIR)\Clasreps.sbr" \
	"$(INTDIR)\Coalmods.sbr" \
	"$(INTDIR)\Coalreps.sbr" \
	"$(INTDIR)\Crshmods.sbr" \
	"$(INTDIR)\Crshreps.sbr" \
	"$(INTDIR)\Datainpt.sbr" \
	"$(INTDIR)\Dminp.sbr" \
	"$(INTDIR)\Fltnmods.sbr" \
	"$(INTDIR)\Fltnreps.sbr" \
	"$(INTDIR)\Genlmods.sbr" \
	"$(INTDIR)\Genlreps.sbr" \
	"$(INTDIR)\Genlsubs.sbr" \
	"$(INTDIR)\Globals.sbr" \
	"$(INTDIR)\Gravmods.sbr" \
	"$(INTDIR)\Gravreps.sbr" \
	"$(INTDIR)\Ljubamd.sbr" \
	"$(INTDIR)\Magnmods.sbr" \
	"$(INTDIR)\Magnreps.sbr" \
	"$(INTDIR)\Millmods.sbr" \
	"$(INTDIR)\Millreps.sbr" \
	"$(INTDIR)\ModelVariables.sbr" \
	"$(INTDIR)\Order.sbr" \
	"$(INTDIR)\Outpmods.sbr" \
	"$(INTDIR)\Pphi2.sbr" \
	"$(INTDIR)\Pphi5.sbr" \
	"$(INTDIR)\Ppho2.sbr" \
	"$(INTDIR)\Report.sbr" \
	"$(INTDIR)\Repwrt.sbr" \
	"$(INTDIR)\Scrnmods.sbr" \
	"$(INTDIR)\Scrnreps.sbr" \
	"$(INTDIR)\Simop.sbr" \
	"$(INTDIR)\Simulate.sbr" \
	"$(INTDIR)\Unitreps.sbr" \
	"$(INTDIR)\Units.sbr" \
	"$(INTDIR)\Wsepmods.sbr" \
	"$(INTDIR)\Wsepreps.sbr"

"$(OUTDIR)\ModsimMain.bsc" : "$(OUTDIR)" $(BSC32_SBRS)
    $(BSC32) @<<
  $(BSC32_FLAGS) $(BSC32_SBRS)
<<

LINK32=link.exe
LINK32_FLAGS=kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib imsl.lib IMSLMPISTUB.LIB IMSLS_ERR.LIB /nologo /subsystem:windows /dll /incremental:no /pdb:"$(OUTDIR)\Modsim.pdb" /debug /machine:I386 /out:"../Modsim.dll" /implib:"$(OUTDIR)\Modsim.lib" /pdbtype:sept 
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


!IF "$(NO_EXTERNAL_DEPS)" != "1"
!IF EXISTS("ModsimMain.dep")
!INCLUDE "ModsimMain.dep"
!ELSE 
!MESSAGE Warning: cannot find "ModsimMain.dep"
!ENDIF 
!ENDIF 


!IF "$(CFG)" == "ModsimMain - Win32 Release" || "$(CFG)" == "ModsimMain - Win32 Debug"
SOURCE=..\Betaamd.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Betaamd.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Betaamd.obj"	"$(INTDIR)\Betaamd.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Calc.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Calc.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Calc.obj"	"$(INTDIR)\Calc.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Calc1.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Calc1.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Calc1.obj"	"$(INTDIR)\Calc1.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Clasmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Clasmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Clasmods.obj"	"$(INTDIR)\Clasmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Clasreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Clasreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Clasreps.obj"	"$(INTDIR)\Clasreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Coalmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Coalmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Coalmods.obj"	"$(INTDIR)\Coalmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Coalreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Coalreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Coalreps.obj"	"$(INTDIR)\Coalreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Crshmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Crshmods.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Crshmods.obj"	"$(INTDIR)\Crshmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Crshreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Crshreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Crshreps.obj"	"$(INTDIR)\Crshreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Datainpt.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Datainpt.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Datainpt.obj"	"$(INTDIR)\Datainpt.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Dminp.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Dminp.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Dminp.obj"	"$(INTDIR)\Dminp.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Fltnmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Fltnmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Fltnmods.obj"	"$(INTDIR)\Fltnmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Fltnreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Fltnreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Fltnreps.obj"	"$(INTDIR)\Fltnreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Genlmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Genlmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Genlmods.obj"	"$(INTDIR)\Genlmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Genlreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Genlreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Genlreps.obj"	"$(INTDIR)\Genlreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Genlsubs.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Genlsubs.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Genlsubs.obj"	"$(INTDIR)\Genlsubs.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Globals.f90

!IF  "$(CFG)" == "ModsimMain - Win32 Release"

F90_MODOUT=\
	"GLOBALS"


"$(INTDIR)\Globals.obj"	"$(INTDIR)\GLOBALS.mod" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Globals.obj"	"$(INTDIR)\Globals.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Gravmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Gravmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Gravmods.obj"	"$(INTDIR)\Gravmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Gravreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Gravreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Gravreps.obj"	"$(INTDIR)\Gravreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Ljubamd.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Ljubamd.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Ljubamd.obj"	"$(INTDIR)\Ljubamd.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Magnmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Magnmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Magnmods.obj"	"$(INTDIR)\Magnmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Magnreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Magnreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Magnreps.obj"	"$(INTDIR)\Magnreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Millmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Millmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Millmods.obj"	"$(INTDIR)\Millmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Millreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Millreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Millreps.obj"	"$(INTDIR)\Millreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\ModelVariables.f90

!IF  "$(CFG)" == "ModsimMain - Win32 Release"

F90_MODOUT=\
	"MODELVARIABLES"


"$(INTDIR)\ModelVariables.obj"	"$(INTDIR)\MODELVARIABLES.MOD" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\ModelVariables.obj"	"$(INTDIR)\ModelVariables.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Order.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Order.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Order.obj"	"$(INTDIR)\Order.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Outpmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Outpmods.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Outpmods.obj"	"$(INTDIR)\Outpmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Pphi2.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Pphi2.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Pphi2.obj"	"$(INTDIR)\Pphi2.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Pphi5.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Pphi5.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod" "$(INTDIR)\MODELVARIABLES.MOD"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Pphi5.obj"	"$(INTDIR)\Pphi5.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Ppho2.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Ppho2.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Ppho2.obj"	"$(INTDIR)\Ppho2.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Report.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Report.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Report.obj"	"$(INTDIR)\Report.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Repwrt.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Repwrt.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Repwrt.obj"	"$(INTDIR)\Repwrt.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Scrnmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Scrnmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Scrnmods.obj"	"$(INTDIR)\Scrnmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Scrnreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Scrnreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Scrnreps.obj"	"$(INTDIR)\Scrnreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Simop.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Simop.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Simop.obj"	"$(INTDIR)\Simop.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Simulate.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Simulate.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Simulate.obj"	"$(INTDIR)\Simulate.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Unitreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Unitreps.obj" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Unitreps.obj"	"$(INTDIR)\Unitreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Units.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Units.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Units.obj"	"$(INTDIR)\Units.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Wsepmods.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Wsepmods.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Wsepmods.obj"	"$(INTDIR)\Wsepmods.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 

SOURCE=..\Wsepreps.for

!IF  "$(CFG)" == "ModsimMain - Win32 Release"


"$(INTDIR)\Wsepreps.obj" : $(SOURCE) "$(INTDIR)" "$(INTDIR)\GLOBALS.mod"
	$(F90) $(F90_PROJ) $(SOURCE)


!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"


"$(INTDIR)\Wsepreps.obj"	"$(INTDIR)\Wsepreps.sbr" : $(SOURCE) "$(INTDIR)"
	$(F90) $(F90_PROJ) $(SOURCE)


!ENDIF 


!ENDIF 

