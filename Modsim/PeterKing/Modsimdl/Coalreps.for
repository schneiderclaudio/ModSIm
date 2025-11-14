C*******************     COALREPS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C This file contains the report writers for the models in file COALMODS
C                                                                      C
C NOTE: The report writing subroutines have the same names as their    C
C       corresponding models.                                          C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,
     *GRDM,NMIN,PPROP,Model)
C     **************************************************************
C Calculate the performance measures for typical coal washing units.
      REAL FEED(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC)
      REAL DER1(1),PPROP(1)
      REAL GRDM(NGCM,NMIN)
      CHARACTER*4 NAMES(7)
	CHARACTER*4 Model
      COMMON/MINNAMES/NAMES
      REAL IMPERFECTION

      LU = 8
C Return at once if only 1 G-class.
      IF(NGC .LE. 1) RETURN
C Get theoretical yield
      IF(NAMES(1) .EQ. 'Ash ' .OR. NAMES(1) .EQ. ' Ash') THEN
        ASH = ASHCONT(1,OUT2,NDC,NGC,NSC,NGCM,GRDM,NMIN)
        WRITE(LU,104) ASH*100
  104 FORMAT(/'The ash content of the clean coal from this unit is '
     *  ,F6.2,'%')
        WRITE(LU,203) 100.0*TMS2/TMSF
  203 FORMAT('The actual yield in this unit is ',F6.1,'%')
        WS = 100.0*THEOYIELD(ASH,1,FEED,NDC,NGC,NSC,NGCM,GRDM,NMIN)
        WRITE(LU,204) WS
  204 FORMAT('The theoretical yield at this ash content is ',F6.1,'%')
        FASH = ASHCONT(1,FEED,NDC,NGC,NSC,NGCM,GRDM,NMIN)
        WRITE(LU,105) FASH*100
  105 FORMAT(/'The ash content of the feed to this unit is '
     *  ,F6.2,'%')
        WRITE(LU,106) ASH/FASH
  106 FORMAT(' The ash downgrade ratio is ',F7.2)
      END IF

C Calculate the composite partition factors and separation density
      WRITE(LU,304)
  304 FORMAT(/'Partition factors in this unit for the composite feed'/
     *        'Specific gravity  Partition factor')
      CALL ACTPART(FEED,OUT2,NDC,NGC,NSC,DER1)
      WRITE(LU,205) (PPROP(J),DER1(J),J = 1,NGC)
  205 FORMAT(5X,F7.3,8X,F8.4)


C     Write the actual partition function data to plot from the flowsheet screen
      CALL PartitionFunc(NGC,Model,PPROP,DER1)


      RETURN
      END

      SUBROUTINE ACTPART(FEED,OUT2,NDC,NGC,NSC,DER1)
C     **********************************************
c Calculates the actual partition function that was achieved over all the classes.
      REAL FEED(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC)
      REAL DER1(1)
      DO 30 J = 1,NGC
        PARTF = 0.0
        PART2 = 0.0
        DO 20 I = 1,NDC
          DO 10 K = 1,NSC
            PARTF = PARTF + FEED(I,J,K)
            PART2 = PART2 + OUT2(I,J,K)
   10     CONTINUE
   20   CONTINUE
        DER1(J) = 0.0
        IF(PARTF .GT. 0.0) THEN
          DER1(J) = PART2/PARTF
        ELSE
          IF(J .EQ. 1) THEN
             DER1(J) = 0.0
          ELSE
             DER1(J) = DER1(J-1)
          END IF
        END IF
   30 CONTINUE
      RETURN
      END

      SUBROUTINE RDMCY(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      CHARACTER*4 NAMES(7)
      COMMON /MODELDAT/NUNIT
      COMMON/MINNAMES/NAMES
C
      ITERM=0
      LU=8
      ISELECT = NINT(PARAM(3))
      CALL HEADER(LU,ITERM,NUNIT,'DENSE MEDIUM CYCLONE',20,'DMCY')

      IF(ISELECT .EQ. 1) THEN
      WRITE(LU,100)
  100 FORMAT(/'The Gottfried-Jacobsen procedure has been used to estimat
     *e the cut point for'/'each size.')
      WRITE(LU,101) PARAM(2)*1000, PARAM(1)
  101 FORMAT(' Cyclone diameter: ',F6.0,' mm'/
     *       ' Target specific gravity: ',F6.3)
      END IF

      IF(ISELECT .EQ. 2) THEN
      WRITE(LU,102)
  102 FORMAT('The normalised cut point shift procedure has been used to
     *estimate the cut point'/'for each size.')

      WRITE(LU,103) PARAM(2)*1000, PARAM(1)
  103 FORMAT(' Cyclone diameter: ',F6.0,' mm'/
     *       ' Medium specific gravity: ',F6.3)
      END IF
      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'DMCY')

      RETURN
      END


      SUBROUTINE RDYNA(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'DYNAWHIRLPOOL',13,'DYNA')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Operating density: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'DYNA')

      RETURN
      END

      SUBROUTINE RNORW(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'NORWALT BATH',12,'NORW')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Operating density: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'NORW')

      RETURN
      END

      SUBROUTINE RWEMC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'WEMCO DRUM',10,'WEMC')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Operating density: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'WEMC')

      RETURN
      END

      SUBROUTINE RDREW(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'DREWBOY',7,'DREW')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Operating density: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'DREW')

      RETURN
      END

      SUBROUTINE RCHAN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'CHANCE SAND CONE',16,'CHAN')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Operating density: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'CHAN')

      RETURN
      END

      SUBROUTINE RSLIP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'SHALLOW BATH WASHER',19,'SLIP')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Operating density: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'SLIP')

      RETURN
      END

      SUBROUTINE RBAUJ(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'BAUM JIG',8,'BAUJ')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Cut point: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'BAUJ')

      RETURN
      END


      SUBROUTINE RBATJ(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'BATAC JIG',9,'BATJ')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Cut point: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'BATJ')

      RETURN
      END
C
      SUBROUTINE RWASH(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'DREWBOY',7,'WASH')
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Operating density: ',G9.3,' g/cc')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'WASH')

      RETURN
      END

      SUBROUTINE RSHAK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'CONCENTRATING TABLE',19,'SHAK')
       WRITE(LU,1000)
       WRITE(LU,101)PARAM(1)
	WRITE(LU,'('' Water recovery to tailing '',F7.1)')PARAM(2)
  101  FORMAT(' Target specific gravity for the separation: ',F5.2)
 1000 FORMAT(/'The Gottfried-Jacobsen procedure was used to estimate'/
     *'the cut-point for each size class.')

      CALL COALPERF(TMSF,TMS1,FEED,OUT1,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'SHAK')

      RETURN
      END

      SUBROUTINE RDRUM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
       ITERM=0
       LU=8
       CALL HEADER(LU,ITERM,NUNIT,'DENSE-MEDIUM DRUM',17,'DRUM')
       WRITE(LU,1000)
       WRITE(LU,101)PARAM(1)
  101  FORMAT(' Target specific gravity for the separation: ',F5.2)
 1000 FORMAT(/'The Gottfried-Jacobsen procedure was used to estimate'/
     *'the cut-point for each size class.')

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'DRUM')

      RETURN
      END

      SUBROUTINE RWOCY(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
      ITERM=0
      LU=8
      CALL HEADER(LU,ITERM,NUNIT,'Water-only cyclone',18,'WOCY')
      ISELECT = NINT(PARAM(1))
      IF(ISELECT .EQ. 1) THEN
        WRITE(LU,1001)
 1001 FORMAT('The Gottfried-Jacobsen procedure was used to estimate'/
     *       'the cutpoint for each size fraction.')
        WRITE(LU,1002) PARAM(2)
 1002 FORMAT('The target specific gravity was ',F6.2)
      ELSEIF(ISELECT .EQ. 2) THEN
        WRITE(LU,1003)
 1003 FORMAT('The cut points were estimated from the cutpoint at 1mm')
        WRITE(LU,1004)PARAM(2)
 1004 FORMAT(' Cut point at 1 mm: ',F7.3)
      END IF
      IF (PARAM(3) .LT. 2) THEN
        WRITE(LU,*)'A single stage cyclone was used'
      ELSE
        WRITE(LU,*)'A two-stage cyclone arrangement was used'
      END IF

      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
     *PPROP,'WOCY')

      RETURN
      END

      SUBROUTINE RSJIG(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT

      LOGICAL ReportFlag
      COMMON /REPORTING/ReportFlag

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

c Set the report flag to signal N_Stratify that stratification profiles
c must be printed
      ReportFlag = .TRUE.

c Set up a file to take data to plot the stratification profiles.
      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')'SJIG',NUNIT,Index

      WRITE(21,'(''Stratification profiles for unit '',I3,I5)')NUNIT,NGC

      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'Jig',3,'SJIG')

      WRITE(LU,1001) (PARAM(I),I = 1,2)
 1001 FORMAT('Model parameters:'/
     *       '  Cut fraction ',F5.2,/
     *       '  Specific stratification constant ',F8.4)

      hs = PARAM(1)
      alpha = PARAM(2)
      IF(NGC .EQ. 2) THEN
        CALL BinStratify(FEED,OUT1,OUT2,NDC,NGC,NSC,SIZE,
     *  hs,alpha,PPROP,NPP,INDPP)
      END IF
      IF (NGC .GT. 2) THEN
        CALL N_Stratify(FEED,OUT1,OUT2,NDC,NGC,NSC,SIZE,
     *  hs,alpha,PPROP,NPP)
      END IF
      TMS1 = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TMS1 = TMS1 + OUT1(I,J,K)
          END DO
        END DO
      END DO
      TMS2 = TMSF - TMS1
      WTR1 = TMS1*WTR/TMSF
      WTR2 = WTR - WTR1

c Calculate the water flows if water is added to the jig.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
        END IF
      END IF

C      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
C     *PPROP)

      ReportFlag =.FALSE.
      CLOSE(21)

      RETURN
      END

      SUBROUTINE RVJIG(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT

	Real alpha,hs,kappa

      LOGICAL ReportFlag
      COMMON /REPORTING/ReportFlag

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

c Set the report flag to signal N_Stratify that stratification profiles
c must be printed
      ReportFlag = .TRUE.

c Set up a file to take data to plot the stratification profiles.
      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')'VJIG',NUNIT,Index

      WRITE(21,'(''Stratification profiles for unit '',I3,I5)')NUNIT,NGC

      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'Jig',3,'VJIG')

      WRITE(LU,1001) (PARAM(I),I = 1,3)
 1001 FORMAT('Model parameters:'/
     *       '  Cut fraction ',F5.2,/
     *       '  Specific stratification constant ',F8.4,/
     *       '  Velocity profile constant kappa  ',F8.4)

      hs = PARAM(1)
      alpha = PARAM(2)
	kappa = PARAM(3)
      IF (NGC .GT. 1) THEN
        CALL N_StratifyV(FEED,OUT1,OUT2,NDC,NGC,NSC,SIZE,
     *  hs,alpha,kappa,PPROP,NPP)
      END IF
      TMS1 = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TMS1 = TMS1 + OUT1(I,J,K)
          END DO
        END DO
      END DO
      TMS2 = TMSF - TMS1
      WTR1 = TMS1*WTR/TMSF
      WTR2 = WTR - WTR1

c Calculate the water flows if water is added to the jig.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
        END IF
      END IF

C      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER1,NDC,NGC,NSC,NGCM,GRDM,NMIN,
C     *PPROP)

      ReportFlag =.FALSE.
      CLOSE(21)

      RETURN
      END

      SUBROUTINE RTESK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      RETURN
      END
