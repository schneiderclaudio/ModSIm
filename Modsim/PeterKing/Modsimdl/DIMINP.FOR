C******************* 	PROGRAM DIMINP	    ***************************C
C  MODSIM  MODULAR SIMULATOR FOR ORE DRESSING PLANTS                   C
C                                                                      C
C                                                                      C
C  THIS PROGRAM SETS THE DEFAULT DIMENSIONS.                           C
C  IT IS EXECUTED ONLY WHEN THE DEFAULTS MUST BE ESTABLISHED OR CHANGEDC
C                                                                      C
C  MODSIM (C) COPYRIGHT  RP KING  JOHANNESBURG 1985                    C
C**********************************************************************C
C
      INTEGER DIMPPM,DIMPRM,DIMPAR
      INTEGER TextParM
C  NCYC IS THE MAXIMUM NUMBER OF CYCLES EXPECTED IN ANY COMPLEX NODE.
      NCYC=15
      NCYG=2**NCYC
C  NCNODES IS THE MAXIMUM NUMBER OF COMPLEX NODES
      NCNODES = 32
      NPLANT=5
      NCNCT=5
      NMINM=7
      !Set the maximum number of units here
      NUMU=100
      NUMM=100
      NUMM1=100
      NSMM=100
      NSMM1=100

C  Remember to set these in the VB Sysdat.BAS module also.
C  Set the maximum number of size classes here
      NDCM=35
C  Set the maximum number of grade classes here
      NGCM=22
C  Set the maximum number of S-classes here
      NSCM=10
C
      NNODM=100
C  Set maximum number of input streams
      NPINPM=5
C  Set maximum number of output streams
      NPOUTM=48
C  Set the maximum number of parameters for any model here.
      DIMPAR=50
      DIMPRM=NUMM*DIMPAR
      NVARM=NDCM*NGCM*NSCM+2
C Set the maximum number of physical properties here.
      NPPM=50
C Set the maximum number of physical property values here.
      DIMPPM=1000
C Set the maximum number of texture parameters
      TextParM = 50

C Write the GLOBALS module
C ************************
      OPEN(101,FILE= '..\GLOBALS.f90')
      Write(101,'(''!This module is created by program DIMINP.FOR'')')
      Write(101,'(''!It must not be changed by the user.'')')
      Write(101,'(''MODULE GLOBALS'')')
      WRITE(101,5004)
 5004 FORMAT('  !MS$ ATTRIBUTES DLLEXPORT :: /VBVariables/')
      Write(101,'(''  Integer MaxOutputStreams,MaxInputStreams'')')
      Write(101,'(''  Real MeshSizes('',I3,'')'')')NDCM
      Write(101,'(''  COMMON /Global/MaxOutputStreams,MaxInputStreams,Me
     *shSizes'')')
      Write(101,*)
      Write(101,'(''  CHARACTER*255 UnitJobPath'')')
      Write(101,'(''  INTEGER*4 UnitExitValue,UnitDiagFile'')')
      Write(101,'(''  COMMON /VBVariables/UnitExitValue,UnitDiagFile,Uni
     *tJobPath'')')

      Write(101,'(''End MODULE GLOBALS'')')
      Close(101)
C
      !Write the MODELVARIABLES module
      !*******************************
      OPEN(101,FILE = '..\ModelVariables.f90')
      Write(101,'(''!This module is created by program DIMINP.FOR'')')
      Write(101,'(''!It must not be changed by the user.'')')
      Write(101,'(''MODULE MODELVARIABLES'')')
      WRITE(101,5005)
 5005 FORMAT('  !MS$ ATTRIBUTES DLLEXPORT :: /MODELVARS/')
      WRITE(101,5006)
 5006 FORMAT(
     *'  INTEGER UnitNumber'/
     *'  LOGICAL Reporting'/
     *'  Character*80 JobName'/
     *'  CHARACTER*256 Message'/
     *'  INTEGER NumSizeClasses,NumGClasses,NumSClasses'/
     *'  REAL TotalSolidsF'/
     *'  REAL TotalSolidsT, TotalSolidsC, TotalSolidsM'/
     *'  REAL FeedWater'/
     *'  REAL TailingsWater, ConcentrateWater, MiddlingsWater'/
     *'  INTEGER NumberOfMessages,NumberOfMinerals,UnitType'/)

      WRITE(101,5007)NGCM,NMINM,NGCM,NMINM
 5007 FORMAT(
     *'  REAL GradeM(',I3,',',I3,'), GradeV(',I3,',',I3,')')

      WRITE(101,5008)NGCM,TextParM,NGCM,NGCM,NSCM,NSCM,NSCM,NGCM,
     *               NGCM,NGCM
 5008 FORMAT(
     *'  REAL SolidSpGr(',I3,')'/
     *'  REAL Texture(',I3,')'/
     *'  REAL MagnSusceptG(',I3,')'/
     *'  REAL OtherPropG(',I3,')'/
     *'  REAL FlotnRateConst(',I3,')'/
     *'  REAL MagnSusceptS(',I3,')'/
     *'  REAL OtherPropS(',I3,')'/
     *'  REAL CalValue(',I3,')'/
     *'  REAL TotalSulfur(',I3,')'/
     *'  REAL PyriticSulf(',I3,')')

      Write(101,5010)NVARM,NDCM,NGCM,NSCM
      Write(101,5020)NVARM,NDCM,NGCM,NSCM
      Write(101,5030)NVARM,NDCM,NGCM,NSCM
      Write(101,5035)NVARM,NDCM,NGCM,NSCM
      Write(101,5040)DIMPAR
 5010 FORMAT(
     *'  REAL F_(',I6,')'/
     *'  REAL Feed(',I3,',',I3,',',I3,')'/
     *'  EQUIVALENCE (F_(1),Feed(1,1,1))')

 5020 FORMAT(
     *'  Real T_(',I6,')'/
     *'  REAL Tailing(',I3,',',I3,',',I3,')'/
     *'  EQUIVALENCE (T_(1),Tailing(1,1,1))')
 5030 FORMAT(
     *'  Real C_(',I6,')'/
     *'  REAL Concentrate(',I3,',',I3,',',I3,')'/
     *'  EQUIVALENCE (C_(1),Concentrate(1,1,1))')
 5035 FORMAT(
     *'  Real M_(',I6,')'/
     *'  REAL Middling(',I3,',',I3,',',I3,')'/
     *'  EQUIVALENCE (M_(1),Middling(1,1,1))')

 5040 FORMAT(
     *'  REAL Parameters(',I4,')')
      WRITE(101,5050)NDCM
 5050 FORMAT(
     *'  REAL RepSize(',I4,')')

      Write(101,*)
      WRITE(101,5100)
 5100 FORMAT('  COMMON /MODELVARS/&'/
     *'  UnitNumber,Reporting,NumSizeClasses,NumGClasses,NumSClasses,&'/
     *'  UnitType,Feed,Tailing,Concentrate,Middling,&'/
     *'  TotalSolidsF,TotalSolidsT,TotalSolidsC,TotalSolidsM,&'/
     *'  FeedWater, TailingsWater, ConcentrateWater, MiddlingsWater,&'/
     *'  Parameters,RepSize,NumberOfMessages,NumberOfMinerals,&'/
     *'  GradeM,GradeV,&'/
     *'  SolidSpGr,Texture,MagnSusceptG,OtherPropG,FlotnRateConst,&'/
     *'  MagnSusceptS,OtherPropS,CalValue,TotalSulfur,PyriticSulf,&'/
     *'  JobName')
      Write(101,'(''End MODULE MODELVARIABLES'')')
      Close(101)


c     Write the module for SIMOP.FOR
      OPEN(103,FILE='..\SIMOPMOD.f90')
      Write(103,'(''!This module is created by program DIMINP.FOR'')')
      Write(103,'(''!It must not be changed by the user.'')')
      Write(103,'(''MODULE SIMOPMODULE'')')
      Write(103,5310)NGCM,NVARM,NDCM,DIMPPM,NGCM,NDCM,NDCM,NDCM,NDCM,
     *NDCM,NGCM,NGCM,NGCM
 5310 FORMAT(
     *'  REAL GRDM(',I3,',7)'/
     *'  REAL VALS(',I5,')'/
     *'  REAL MFMIN(7,100,',I3,')'/
     *'  REAL PPROP(',I4,')'/
     *'  REAL MFSGC(',I3,',100)'/
     *'  REAL SIZE(',I3,'),CUMSIZ(',I3,'),CUM(',I3,'),DENS(',I3,')'/
     *'  REAL MEMCUM(100,',I3,')'/
     *'  REAL CALVAL(',I3,'),TOTSUL(',I3,'),PYRSUL(',I3,')')
      Write(103,'(''  COMMON /SIMOPCOMMON/ GRDM,MFSGC'')')
      Write(103,'(''End MODULE SIMOPMODULE'')')
	CLOSE(103)
	


c     Write the C header file
      OPEN(102, FILE='..\UserModels\CModelVariables.h')
      Write(102,'(''//This file is created by program DIMINP.FOR'')')
      Write(102,'(''//It must not be changed by the user.'')')
	Write(102,*)
	Write(102,5200)
 5200 FORMAT(
     *'#pragma pack(2)'/
     *'extern struct modsimvariables{'/
     *'    int UnitNumber;'/
     *'    int Reporting;'/
     *'    int NumSizeClasses,NumGClasses,NumSClasses;'/
     *'    int UnitType;')
	Write(102,5210)NSCM,NGCM,NDCM
 5210 FORMAT(
     *'    float Feed[',I3,'][',I3,'][',I3,'];')
	Write(102,5220)NSCM,NGCM,NDCM
 5220 FORMAT(
     *'    float Tailing[',I3,'][',I3,'][',I3,'];')
	Write(102,5230)NSCM,NGCM,NDCM
 5230 FORMAT(
     *'    float Concentrate[',I3,'][',I3,'][',I3,'];')
	Write(102,5240)NSCM,NGCM,NDCM
 5240 FORMAT(
     *'    float Middling[',I3,'][',I3,'][',I3,'];')
	Write(102,5250) DIMPAR,NDCM
 5250 FORMAT(
     *'    float TotalSolidsF;'/
     *'    float TotalSolidsT, TotalSolidsC,TotalSolidsM;'/
     *'    float FeedWater;'/
     *'    float TailingsWater, ConcentrateWater, MiddlingsWater;'/
     *'    float Parameters[',I3,'];'/
     *'    float RepSize[',I3,'];'/
     *'    int NumberOfMessages, NumberOfMinerals;')
	Write(102,5260)NMINM,NGCM,NMINM,NGCM
 5260 FORMAT(
     *'    float GradeM[',I3,'][',I3,'], GradeV[',I3,'][',I3,'];')
      WRITE(102,5270)NGCM,TextParM,NGCM,NGCM,NSCM,NSCM,NSCM,NGCM,
     *               NGCM,NGCM
 5270 FORMAT(
     *'    float SolidSpGr[',I3,'];'/
     *'    float Texture[',I3,'];'/
     *'    float MagnSusceptG[',I3,'];'/
     *'    float OtherPropG[',I3,'];'/
     *'    float FlotnRateConst[',I3,'];'/
     *'    float MagnSusceptS[',I3,'];'/
     *'    float OtherPropS[',I3,'];'/
     *'    float CalValue[',I3,'];'/
     *'    float TotalSulfur[',I3,'];'/
     *'    float PyriticSulf[',I3,'];'/
     *'    char JobName[80];')
	Write(102,5280)
 5280 FORMAT('}MODELVARS;'/
     *'#pragma pack()')
	Write(102,*)
	Write(102,5290)
 5290 FORMAT(
     *'#pragma pack(2)'/
     *'extern struct VisualBasicVariables{'/
     *'    long UnitExitValue;'/
     *'    long UnitDiagFile;'/
     *'    long UnitJobPath[255];'/
     *'}VBVARIABLES;'/
     *'#pragma pack()')
	Write(102,*)
	Write(102, 5300)
 5300 FORMAT('char JobPathC[256];')
      CLOSE(102)



c     Write subroutine DMINP.FOR
      IW=21
      OPEN(IW,FILE='..\DMINP.FOR')
      WRITE(IW,1000)
      WRITE(IW,2000) NMINM,NUMU
      WRITE(IW,2001) NPPM,NPLANT,NPLANT,NPLANT,NPLANT,NCNCT,NCNCT
      WRITE(IW,2002) NUMM,NUMM,NSMM,NUMM,NSMM
      WRITE(IW,2003) NDCM, NMINM,NGCM,NMINM,NGCM,NMINM,DIMPPM
      WRITE(IW,2004) NVARM,NDCM,NGCM,NSCM,DIMPRM,NUMM,NUMM
c      WRITE(IW,3004)
      WRITE(IW,2005) NDCM,NGCM,NSCM,NPPM,NUMM,NSMM,NCNCT,NMINM,NCYG,NVAR
     *M
      WRITE(IW,3005)
      WRITE(IW,2006)
      WRITE(IW,2007)
      WRITE(IW,2008)
      WRITE(IW,2010)
      WRITE(IW,2011)
C
1000  FORMAT(6X,'SUBROUTINE DMINP(JobPath,ExitValue,DiagFile)'/
     *'C     ********************************************'/
     *'C     Initiates the data input phase for the simulation.'/
     *'C     This file generated by program DIMINP.FOR'//
     *'      USE MSFLIB'/
     *'      USE GLOBALS'/
     *'      CHARACTER*255 JobPath'/
     *'      INTEGER*4 ExitValue, DiagFile'/ )

2000  FORMAT(6X,'CHARACTER*4 NAMES(',I3,'),NAM(',I3,')')
2001  FORMAT(6X,'INTEGER INDPP(',I3,',2),NDCL(',I3,'),NGCL(',I3,'),NSCL(
     *',I3,'),NVR(',I3,/5X,'*),INDCNC(',I3,'),CONCT(',I3,',4)')
2002  FORMAT(6X,'INTEGER INDPAR(',I3,',2),OUTP(10),UNTYP(',I3,'),'/5X,'*
     *SCM(',I3,',3),UNUM(',I3,'),B(',I3,')')
2003  FORMAT(6X,'REAL SIZ(',I3,'), PROP(',I3,'),GRM(',I3,',',I3,'),GRV('
     *,I3,',',I3,'),PPROP(',I4,')')
2004  FORMAT(6X,'REAL STRV(',I5,'),FD(',I3,'),FG(',I3,'),FS(',I3,'),UNTP
     *AR(',I4,'),WTR(',I3,',2),',/5X,'*WTRFUN(',I3,')')
3004  FORMAT(6X,'N1 = 1'/6X,'CALL GETARG(N1,JobPath)'/
     *6X,'ExitValue = 0')
2005  FORMAT(6X,'NDCM=',I4/6X,'NGCM=',I4/6X,'NSCM=',I4/6X,'NPPM=',I4/
     *6X,'NUMM=',I4/6X,'NSMM=',I4/6X,'NCNCT=',I4/6X
     *,'NMIN=',I4/6X,'NCYG=',I6/6X,'NVARM=',I5/
     *)

3005  FORMAT(6X,'WRITE(Diagfile,1001)'/
     *' 1001 FORMAT('' Will now enter the subroutine to read the input d
     *ata'')')

2006  FORMAT(6X,'CALL INPT(NPPM,NUMM,NSMM,NCNCT,NGCM,NMIN,NCYG,CONCT,IND
     *PP,INDPAR,'/5X,'*SCM,WTR,GRM,GRV,NAMES,NAM,WTRFUN,')
2007  FORMAT(5X,'*NDCL,NGCL,NSCL,NVARM,NVR,INDCNC,NDCM,NGCM,NSCM,')
2008  FORMAT(5X,'*SIZ,OUTP,UNTYP,UNUM,B,PROP,PPROP,STRV,FD,FG,FS,UNTPAR,
     *'/'     *JobPath,ExitValue,DiagFile)')
2010  FORMAT(6X,'RETURN')
2011  FORMAT(6X,'END')
      CLOSE(IW)


C  CALL SUBROUTINE TO SET UP DIMENSIONS FOR PHASE 3.
C  *************************************************
      CALL PHASE3(NSMM,NUMM,NCYG,NSMM1,NUMM1,NCNODES)


C  CALL SUBROUTINE TO SET UP DIMENSIONS FOR PHASE 4.
C  *************************************************
      CALL PHASE4(NUMM,NSMM,NNODM,NMINM,NPLANT,NCNCT,NPPM,NPOUTM,
     *NPINPM,DIMPAR,DIMPPM,NVARM,NCYC,NDCM,NGCM)
C
C Call the subroutine to set up the dimensions for the report writing program
C ***************************************************************************
      CALL PHASE5(NDCM,NGCM,NSCM,NMINM,DIMPAR,NPPM,DIMPPM)
      STOP 'DIMINP.EXE has been run'
      END
C
C
      SUBROUTINE PHASE3(NSSM,NUUM,NCYG,NSMM1,NUMM1,NCNODES)
C     *****************************************************
C
C **SET UP DIMENSIONS FOR ORDERING PHASE
C
      IWD=4
      OPEN(IWD,FILE='..\PPHI2.FOR')
      WRITE(IWD,2044)
2044  FORMAT(6X,'SUBROUTINE PPHI2(JobPath,ExitValue,DiagFile)'/
     *'C     ********************************************'/
     *'C     Initiates the ordering phase for the simulation.'/
     *'C     This file generated by program DIMINP.FOR'/
     *'      USE MSFLIB'/
     *'      USE GLOBALS'/
     *'      CHARACTER*255 JobPath'/
     *'      INTEGER*4 ExitValue, DiagFile'/ )

C
      NS2=NSSM*2
      NS5=NSSM*5
      NSU=NSSM+5*NUUM
      WRITE(IWD,2023) NSSM,NUUM,NS5,NSMM1,NSSM,NSSM,NSSM,NSMM1,NSMM1
      WRITE(IWD,2024) NSSM,NSMM1,NSMM1,NSMM1,NSMM1,NSU,NSMM1,NSU,NSMM1
      WRITE(IWD,2025) NSMM1,NSMM1,NSSM,NS5
      WRITE(IWD,2026) NUUM,NUMM1,NUUM,NSMM1,NSSM,NUUM,NSSM,NCNODES,NSSM
      WRITE(IWD,2027) NUUM,NUUM,NUUM,NUUM,NUUM,NSSM,NUUM,NUUM,NUUM
      WRITE(IWD,2028) NSSM,NCYG,NCYG,NSSM
      WRITE(IWD,2029) NSMM1,NSSM,NCYG
      WRITE(IWD,2030) NSMM1,NSMM1,NSMM1,NSMM1,NUUM,NSMM1,NUMM1
C
2023  FORMAT(6X,'INTEGER*2 COEF(',I3,'),INDNP(',I3,'),PTHVEC(',I4,'),IND
     *PTH(',I3,'),PTHI(',I3,'),'/5X,'*BLANK(',I3,'),NODFED(',I3,'),INDVA
     *L(',I3,'),INDFT(',I3,')')
2024  FORMAT(6X,'INTEGER*2 SCM(',I3,',3),WT(',I3,'),UNIT(',I3,'),UNOUT('
     *,I3,'),STRM(',I3,')'/6X,'INTEGER*2 STRVEC(',I3,'),INDSV(',I3,'),IN
     *VSV(',I3,'),INDISV(',I3,')')
2025  FORMAT(6X,'INTEGER*2 NOUT(',I3,'),IOUT(',I3,'),PATH(',I3,'),CYCVEC
     *(',I3,')')
2026  FORMAT(6X,'INTEGER*2 INDCYC(',I3,'),NINPU(',I3,'),NINPN(',I3,'),NO
     *DINP(',I3,')'/6X,'INTEGER*2 NODOUT(',I3,'),INDN(',I3,'),CYCGRP(',I
     *3,'),INDCG(',I3,'),GRPVEC(',I3,')')
2027  FORMAT(6X,'INTEGER*2 INDGRP(',I3,'),NODUNT(',I3,'),PLINP(',I3,'),N
     *DINAV(',I3,')'/6X,'INTEGER*2 UNINAV(',I3,'),TEAR(',I3,'),INDT(',I3
     *,'),ORDUNT(',I3,'),INDORD(',I3,')')
2028  FORMAT(6X,'INTEGER*2 B(',I3,'),E(',I5,'),G(',I5,'),PLOUT(',I3,')')
2029  FORMAT(6X,'INTEGER*4 STRCYC(',I3,'),GRSTCY(',I3,'),M(',I5,')')
2030  FORMAT(6X,'LOGICAL   BLOCK(',I3,'),CIRCUT(',I3,'),PATHST(',I3,'),U
     *SEDST(',I3,')'/6X,'LOGICAL   USEDCY(',I3,'),STRGRP(',I3,'),UNTGRP(
     *',I3,')')
3004  FORMAT(6X,'ExitValue = -1')
C
      WRITE(IWD,3004)
      WRITE(IWD,2031) NSSM,NCYG
      WRITE(IWD,3005)
      WRITE(IWD,2032)
      WRITE(IWD,2033)
      WRITE(IWD,2034)
      WRITE(IWD,2035)
      WRITE(IWD,2036)
2031  FORMAT(6X,'NSSM=',I4/6X,'NCYG=',I5)

3005  FORMAT(6X,'WRITE(Diagfile,1001)'/
     *' 1001 FORMAT('' Will now enter the ordering subroutine'')')


2032  FORMAT(6X,'CALL ORDER(SCM,NSSM,NCYG,'/
     *5X,'*WT,UNIT,UNOUT,STRM,STRVEC,INDSV,INVSV,INDISV,
     *'/5X,'*NOUT,IOUT,PATH,')
2033  FORMAT(5X,'*CYCVEC,INDCYC,NINPU,NINPN,NODINP,NODOUT,INDN,CYCGRP,'/
     *5X,'*INDCG,GRPVEC,INDGRP,NODUNT,')
2034  FORMAT(5X,'*PLINP,NDINAV,UNINAV,TEAR,INDT,ORDUNT,INDORD,B,E,G,'/
     *5X,'*PLOUT,STRCYC,GRSTCY,M,')
2035  FORMAT(5X,'*BLOCK,CIRCUT,PATHST,USEDST,USEDCY,STRGRP,UNTGRP,'/
     *5X,'*COEF,INDNP,PTHVEC,INDPTH,PTHI,BLANK,NODFED,INDVAL,INDFT,'/
     *5X,'*JobPath,ExitValue,DiagFile)')
2036  FORMAT(6X,'RETURN'/6X,'END')
      RETURN
      END
C
C
      SUBROUTINE PHASE4(NUMM,NSMM,NNODM,NMIN,NPLANT,NCNCT,NPP,NPOUTM,
     *NPINPM,DIMPAR,DIMPP,NVARM,NCYC,NDCM,NGCM)
C     ***************************************************************
C
C **SET UP DIMENSIONS FOR CALCULATION PHASE
C
      INTEGER DIMPAR,DIMPP,DIMCNC,DIMFTM,DIMUTP,DIMVLM,PTOT,DIMSVM,
     *DIMIVM,DIMPTM,DIMALM
C
      IWD=7
      OPEN(IWD,FILE='..\PPHO2.FOR')
      REWIND(IWD)
      NUUM=NUMM
      NTTM=NCYC
      NTMM=NCYC
      NPTHM=NNODM
      DIMCNC=NVARM*NPLANT
      DIMFTM=(NPINPM+NTTM)*NVARM
      DIMUTP=NUUM*DIMPAR
      PTOT=NPINPM+NPOUTM+NTTM
      DIMVLM=PTOT*NVARM
      DIMALM=NTTM*NVARM
      NVTM=NTMM*NVARM
      DIMSVM=NSMM+1
      DIMIVM=NSMM+1
      DIMPTM=NSMM
      NTMM1=NTMM*(NTMM+1)
      NTMM2=NTMM*NTMM

      WRITE(IWD,2044)
2044  FORMAT(6X,'SUBROUTINE PPHO2(JobPath,ExitValue,DiagFile)'/
     *'C     *********************************************'/
     *'C     Sets up the dimensions and initiates the calculation'/
     *'C     phase for the simulation.'/
     *'C     This program is generated by DIMINP.FOR'/
     *'      USE MSFLIB'/
     *'      USE GLOBALS'/
     *'      CHARACTER*255 JobPath'/
     *'      INTEGER*2 N1'/
     *'      INTEGER*4 ExitValue,DiagFile')

      WRITE(IWD,4000) NMIN,NUMM,NMIN,NMIN
      WRITE(IWD,4001) NPLANT,NPLANT,NPLANT,NCNCT,NCNCT,NPP,NPLANT
      WRITE(IWD,4002) NNODM,NUUM,NNODM,NTTM,NSMM,DIMSVM,NSMM
      WRITE(IWD,4003) DIMIVM,NPOUTM,NPINPM,NUMM,NSMM,NSMM,NSMM
      WRITE(IWD,4004) DIMPTM,NNODM,NPTHM,NUMM,NUMM,NTMM2,NUMM
      WRITE(IWD,4005) DIMCNC,DIMPP,DIMFTM,NUMM,DIMUTP,DIMVLM,DIMALM
      WRITE(IWD,4006) NVARM,NVARM,NVARM,NVARM,NVARM,NVARM,NVARM,DIMPAR
      WRITE(IWD,4007) NVTM,NTMM1,NVTM,NDCM,NGCM,NMIN,NGCM,NMIN
C
 4000 FORMAT(6X,'CHARACTER*4 NAMES(',I3,'),NAMU(',I3,')'/
     *6X,'REAL FM(',I3,'),FMM(',I4,')')
 4001 FORMAT(6X,'INTEGER NDCL(',I3,'),NGCL(',I3,'),NSCL(',I3,'),CONCT(',
     *I4,',4),'/5X,'*INDCNC(',I4,'),INDPP(',I4,',2),NVR(',I4,')')
 4002 FORMAT(6X,'INTEGER INDORD(',I4,'),ORDUNT(',I4,'),INDT(',I4,'),TEAR
     *(',I4,'),'/5X,'*INDSV(',I4,'),STRVEC(',I4,'),INDISV(',I4,')')
 4003 FORMAT(6X,'INTEGER INVSV(',I4,'),PLOUT(',I4,'),PLINP(',I4,'),STRM(
     *',I4,'),'/5X,'*INDVAL(',I4,'),INDFT(',I4,'),INDPTH(',I4,')')
 4004 FORMAT(6X,'INTEGER PTHVEC(',I4,'),INDNP(',I4,'),COEF(',I4,'),UNTYP
     *(',I4,'),'/5X,'* INDPAR(',I4,',2),INDCF(',I4,'),OUTP(10),WTRFUN(',
     *I4,')')
 4005 FORMAT(6X,'REAL CNCVAL(',I6,'),PPROP(',I6,'),FTVEC(',I6,'),WTRF(',
     *I6,',2),'/5X,'*UNTPAR(',I6,'),STRVAL(',I6,'),ALFA(',I6,')')
 4006 FORMAT(6X,'REAL FEED(',I5,'),OUT1(',I5,'),OUT2(',I5,'),OUT3(',I5,'
     *),'/5X,'*DER1(',I5,'),DER2(',I5,'),DER3(',I5,'),PARAM(',I5,')')
 4007 FORMAT(6X,'REAL F(',I6,'),A(',I6,'),T(',I6,'),SIZE(',I4,'),GRDM(',
     *I4,',',I4,'),'/5X,'*GRDV(',I4,',',I4,')')
C
C
 3004 FORMAT(6X,'N1 = 1'/'      ExitValue = -1')

      WRITE(IWD,3004)
      WRITE(IWD,4016) NCNCT,NPP,NUMM,NGCM,NMIN
 4016 FORMAT(6X,'NCNCT=',I4/6X,'NPP=',I4/6X,'NUMM=',I4/6X,'NGCM=',I4/6X,
     *'NMIN=',I4)

      WRITE(IWD,4017)NPOUTM
 4017 FORMAT(6X,'MaxOutputStreams = ',I4)
      WRITE(IWD,4018)NPINPM
 4018 FORMAT(6X,'MaxInputStreams = ',I4)

      WRITE(IWD,4008)
      WRITE(IWD,4009)
      WRITE(IWD,4010)
      WRITE(IWD,4011)
      WRITE(IWD,4012)
      WRITE(IWD,4013)
      WRITE(IWD,4014)
      WRITE(IWD,4015)
C
 4008 FORMAT(6X,'CALL CALC(INDPP,INDPAR,CONCT,GRDM,GRDV,WTRF,NAMES,FM,FM
     *M,NCNCT,'/5X,'*NPP,NUMM,NGCM,NMIN,WTRFUN,')
 4009 FORMAT(5X,'*NDCL,NGCL,NSCL,INDCNC,NVR,INDORD,ORDUNT,INDT,')
 4010 FORMAT(5X,'*TEAR,INDSV,STRVEC,INDISV,INVSV,PLOUT,PLINP,STRM,INDVAL
     *,')
 4011 FORMAT(5X,'*INDFT,INDPTH,PTHVEC,INDNP,COEF,UNTYP,INDCF,OUTP,')
 4012 FORMAT(5X,'*CNCVAL,PPROP,FTVEC,UNTPAR,STRVAL,ALFA,')
 4013 FORMAT(5X,'*FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,PARAM,')
 4014 FORMAT(5X,'*F,A,T,SIZE,NAMU,JobPath,ExitValue,DiagFile)')
 4015 FORMAT(6X,'END')

      RETURN
      END


      SUBROUTINE PHASE5(NDCM,NGCM,NSCM,NMINM,DIMPAR,NPPM,DIMPPM)
C     **********************************************************
C
C Writes the program that will eventually write the report file.
      INTEGER DIMPAR,DIMPPM
      OPEN(31,FILE = '..\REPORT.FOR')
      WRITE(31,1001)
 1001 FORMAT(
     *'      SUBROUTINE REPORT(JobPath,ExitValue,DiagFile)'/
     *'C     *********************************************'/
     *'C This program was written by subroutine PHASE5 in DIMINP.FOR'/
     *'      USE MSFLIB'/
     *'      CHARACTER*255 JobPath'/
     *'      INTEGER*4 ExitValue,DiagFile')

      WRITE(31,1002)NDCM,NGCM,NSCM
 1002 FORMAT(6X,'REAL FEED(',I4,',',I4,',',I4,')')
      WRITE(31,1003)NDCM,NGCM,NSCM
 1003 FORMAT(6X,'REAL OUT1(',I4,',',I4,',',I4,')')
      WRITE(31,1004)NDCM,NGCM,NSCM
 1004 FORMAT(6X,'REAL OUT2(',I4,',',I4,',',I4,')')
      WRITE(31,2004)NDCM,NGCM,NSCM
 2004 FORMAT(6X,'REAL OUT3(',I4,',',I4,',',I4,')')
      WRITE(31,3003)NDCM,NGCM,NSCM
 3003 FORMAT(6X,'REAL DER1(',I4,',',I4,',',I4,')')
      WRITE(31,3004)NDCM,NGCM,NSCM
 3004 FORMAT(6X,'REAL DER2(',I4,',',I4,',',I4,')')
      WRITE(31,4004)NDCM,NGCM,NSCM
 4004 FORMAT(6X,'REAL DER3(',I4,',',I4,',',I4,')')
      WRITE(31,1005) NGCM,NMINM
 1005 FORMAT(6X,'REAL GRDM(',I4,',',I4,')')
      WRITE(31,1006) NGCM,NMINM
 1006 FORMAT(6X,'REAL GRDV(',I4,',',I4,')')
      WRITE(31,2005) NDCM
 2005 FORMAT(6X,'REAL SIZE(',I4,')')
      WRITE(31,2006) DIMPAR
 2006 FORMAT(6X,'REAL PARAM(',I4,')')
      WRITE(31,2007) NPPM
 2007 FORMAT(6X,'INTEGER INDPP(',I4,',2)')
      WRITE(31,2008) DIMPPM
 2008 FORMAT(6X,'REAL PPROP(',I4,')')
      WRITE(31,2009)
 2009 FORMAT(6X,'INTEGER DIMPPM,DIMPAR')
C
 4005 FORMAT(
     *'      LENG = LEN_TRIM(JobPath)'/
     *'      ExitValue = -1')
      WRITE(31,4005)

      WRITE(31,1007)NDCM,NGCM,NSCM
 1007 FORMAT(6X,'NDCM = ',I4/6X,'NGCM = ',I4/6X,'NSCM = ',I4)
      WRITE(31,1008)NMINM
 1008 FORMAT(6X,'NMINM = ',I4)
      WRITE(31,1009)DIMPAR
 1009 FORMAT(6X,'DIMPAR = ',I4)
      WRITE(31,1010)NPPM
 1010 FORMAT(6X,'NPPM = ',I4)
      WRITE(31,1011)DIMPPM
 1011 FORMAT(6X,'DIMPPM = ',I4)

      WRITE(31,1020)
 1020 FORMAT(6X,'CALL REPWRT(NDCM,NGCM,NSCM,NMINM,FEED,OUT1,OUT2,OUT3,DE
     *R1,DER2,'/5X,'*DER3,GRDM,GRDV,SIZE,DIMPAR,PARAM,NPPM,DIMPPM,INDPP,
     *PPROP,'/5x,'*JobPath,ExitValue,DiagFile)')
      WRITE(31,1021)
 1021 FORMAT(6X,'END')
      RETURN
      END
