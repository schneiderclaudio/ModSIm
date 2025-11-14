# Microsoft Developer Studio Project File - Name="ModsimMain" - Package Owner=<4>
# Microsoft Developer Studio Generated Build File, Format Version 6.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Dynamic-Link Library" 0x0102

CFG=ModsimMain - Win32 Debug
!MESSAGE This is not a valid makefile. To build this project using NMAKE,
!MESSAGE use the Export Makefile command and run
!MESSAGE 
!MESSAGE NMAKE /f "ModsimMain.mak".
!MESSAGE 
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

# Begin Project
# PROP AllowPerConfigDependencies 0
# PROP Scc_ProjName ""
# PROP Scc_LocalPath ""
CPP=cl.exe
F90=df.exe
MTL=midl.exe
RSC=rc.exe

!IF  "$(CFG)" == "ModsimMain - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP BASE Output_Dir "Release"
# PROP BASE Intermediate_Dir "Release"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
# PROP Output_Dir "Release"
# PROP Intermediate_Dir "Release"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /compile_only /dll /nologo /warn:nofileopt
# ADD F90 /compile_only /dll /nologo /warn:nofileopt
# ADD BASE CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /c
# ADD CPP /nologo /MT /W3 /GX /O2 /D "WIN32" /D "NDEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /c
# ADD BASE MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "NDEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "NDEBUG"
# ADD RSC /l 0x409 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /machine:I386
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib imsl.lib IMSLMPISTUB.LIB IMSLS_ERR.LIB /nologo /subsystem:windows /dll /machine:I386 /out:"Release/Modsim.dll"

!ELSEIF  "$(CFG)" == "ModsimMain - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP BASE Output_Dir "Debug"
# PROP BASE Intermediate_Dir "Debug"
# PROP BASE Target_Dir ""
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
# PROP Output_Dir "Debug"
# PROP Intermediate_Dir "Debug"
# PROP Ignore_Export_Lib 0
# PROP Target_Dir ""
# ADD BASE F90 /check:bounds /compile_only /dbglibs /debug:full /dll /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD F90 /browser /check:bounds /compile_only /dbglibs /debug:full /dll /nologo /traceback /warn:argument_checking /warn:nofileopt
# ADD BASE CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /YX /FD /GZ /c
# ADD CPP /nologo /MTd /W3 /Gm /GX /ZI /Od /D "WIN32" /D "_DEBUG" /D "_WINDOWS" /D "_MBCS" /D "_USRDLL" /FR /YX /FD /GZ /c
# ADD BASE MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD MTL /nologo /D "_DEBUG" /mktyplib203 /win32
# ADD BASE RSC /l 0x409 /d "_DEBUG"
# ADD RSC /l 0x409 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib /nologo /subsystem:windows /dll /debug /machine:I386 /pdbtype:sept
# ADD LINK32 kernel32.lib user32.lib gdi32.lib winspool.lib comdlg32.lib advapi32.lib shell32.lib ole32.lib oleaut32.lib uuid.lib odbc32.lib odbccp32.lib imsl.lib IMSLMPISTUB.LIB IMSLS_ERR.LIB /nologo /subsystem:windows /dll /incremental:no /debug /machine:I386 /out:"../Modsim.dll" /pdbtype:sept

!ENDIF 

# Begin Target

# Name "ModsimMain - Win32 Release"
# Name "ModsimMain - Win32 Debug"
# Begin Group "Source Files"

# PROP Default_Filter "cpp;c;cxx;rc;def;r;odl;idl;hpj;bat;f90;for;f;fpp"
# Begin Source File

SOURCE=..\Betaamd.for
# End Source File
# Begin Source File

SOURCE=..\Calc.for
DEP_F90_CALC_=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Calc1.for
DEP_F90_CALC1=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Clasmods.for
DEP_F90_CLASM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Clasreps.for
DEP_F90_CLASR=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Coalmods.for
DEP_F90_COALM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Coalreps.for
DEP_F90_COALR=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Crshmods.for
# End Source File
# Begin Source File

SOURCE=..\Crshreps.for
# End Source File
# Begin Source File

SOURCE=..\Datainpt.for
# End Source File
# Begin Source File

SOURCE=..\Dminp.for
DEP_F90_DMINP=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Fltnmods.for
DEP_F90_FLTNM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Fltnreps.for
# End Source File
# Begin Source File

SOURCE=..\Genlmods.for
DEP_F90_GENLM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Genlreps.for
# End Source File
# Begin Source File

SOURCE=..\Genlsubs.for
# End Source File
# Begin Source File

SOURCE=..\Globals.f90
# End Source File
# Begin Source File

SOURCE=..\Gravmods.for
DEP_F90_GRAVM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Gravreps.for
DEP_F90_GRAVR=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Ljubamd.for
# End Source File
# Begin Source File

SOURCE=..\Magnmods.for
DEP_F90_MAGNM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Magnreps.for
# End Source File
# Begin Source File

SOURCE=..\Millmods.for
DEP_F90_MILLM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Millreps.for
DEP_F90_MILLR=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\ModelVariables.f90
# End Source File
# Begin Source File

SOURCE=..\Order.for
# End Source File
# Begin Source File

SOURCE=..\Outpmods.for
# End Source File
# Begin Source File

SOURCE=..\Pphi2.for
DEP_F90_PPHI2=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Pphi5.for
DEP_F90_PPHI5=\
	".\Release\GLOBALS.mod"\
	".\Release\MODELVARIABLES.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Ppho2.for
DEP_F90_PPHO2=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Report.for
# End Source File
# Begin Source File

SOURCE=..\Repwrt.for
DEP_F90_REPWR=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Scrnmods.for
DEP_F90_SCRNM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Scrnreps.for
DEP_F90_SCRNR=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Simop.for
DEP_F90_SIMOP=\
	".\Release\SIMOPMODULE.MOD"\
	
# End Source File
# Begin Source File

SOURCE=..\SIMOPMOD.f90
# End Source File
# Begin Source File

SOURCE=..\Simulate.for
# End Source File
# Begin Source File

SOURCE=..\Unitreps.for
# End Source File
# Begin Source File

SOURCE=..\Units.for
DEP_F90_UNITS=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Wsepmods.for
DEP_F90_WSEPM=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# Begin Source File

SOURCE=..\Wsepreps.for
DEP_F90_WSEPR=\
	".\Release\GLOBALS.mod"\
	
# End Source File
# End Group
# Begin Group "Header Files"

# PROP Default_Filter "h;hpp;hxx;hm;inl;fi;fd"
# End Group
# Begin Group "Resource Files"

# PROP Default_Filter "ico;cur;bmp;dlg;rc2;rct;bin;rgs;gif;jpg;jpeg;jpe"
# End Group
# Begin Source File

SOURCE=..\UserModels\Release\UserModels.lib
# End Source File
# End Target
# End Project
