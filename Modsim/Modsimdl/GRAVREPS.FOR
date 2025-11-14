      SUBROUTINE RSPIR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
       ITERM=0
       LU=8
      R1=PARAM(1)
      R2=PARAM(2)
      IP=NINT(PARAM(3))
      LU = 8
C
C  WRITE THE REPORT FILE.
      CALL HEADER(8,5,NUNIT,'SPIRAL CONCENTRATOR',19,'SPIR')
      WRITE(8,1001)IP
 1001 FORMAT('Parameters:'/
     *       '  Number of spirals in parallel ',I3)
      IF(R2.LT.R1) THEN
        WRITE(8,1002)R2,R1
 1002   FORMAT('  Inner relative splitter position ',F6.3/
     *         '  Outer relative splitter position ',F6.3)
      ELSE
        WRITE(8,1003)R1
 1003   FORMAT('  Relative splitter position ',F6.3)
      END IF
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      VS=TMSF*SVM/PARAM(3)
      Q=VS+WTR*1.0E-3/PARAM(3)
      VOLFR=VS/Q
      TONS = TMSF*3.6/PARAM(3)
      WRITE(8,1004) TONS,60000.0*Q,100.0*VOLFR
 1004 FORMAT(/' Solid feedrate ',F6.3,' tons/hr'/
     *        ' Volumetric feedrate ',F5.1,' liters/min'/
     *        ' Solid content in feed by volume ',F5.1,' %')
      IF(TONS .GT. 2.5) THEN
        WRITE(8,1005)
 1005   FORMAT(/'****WARNING****'/
     *  'Feed rate is higher than recommended maximum of 2.5 tons/hr')
      END IF
      IF(TONS .LT. 0.5) THEN
        WRITE(8,1006)
 1006   FORMAT(/'****WARNING****'/
     *  'Feed rate is lower than recommended minimum of 0.5 tons/hr')
      END IF

C Calculate the composite partition factors and separation density
      WRITE(LU,304)
  304 FORMAT(/'Partition factors in this unit for the composite feed'/
     *        'Specific gravity  Partition factor')
      CALL ACTPART(FEED,OUT2,NDC,NGC,NSC,DER1)
      WRITE(LU,205) (PPROP(J),DER1(J),J = 1,NGC)
  205 FORMAT(5X,F6.2,8X,F8.4)

C     Write the actual partition function data to plot from the flowsheet screen
      CALL PartitionFunc(NGC,'SPIR',PPROP,DER1)

      RETURN
      END

	SUBROUTINE RSPLM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
       ITERM=0
       LU=8
      R1=PARAM(1)
      R2=PARAM(2)
      IP=NINT(PARAM(3))
      LU = 8
C
C  WRITE THE REPORT FILE.
      CALL HEADER(8,5,NUNIT,'SPIRAL CONCENTRATOR',19,'SPLM')
      WRITE(8,1001)IP
 1001 FORMAT('Parameters:'/
     *       '  Number of spirals in parallel ',I3)
      IF(R2.LT.R1) THEN
        WRITE(8,1002)R2,R1
 1002   FORMAT('  Inner relative splitter position ',F6.3/
     *         '  Outer relative splitter position ',F6.3)
      ELSE
        WRITE(8,1003)R1
 1003   FORMAT('  Relative splitter position ',F6.3)
      END IF
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      VS=TMSF*SVM/PARAM(3)
      Q=VS+WTR*1.0E-3/PARAM(3)
      VOLFR=VS/Q
      TONS = TMSF*3.6/PARAM(3)
      WRITE(8,1004) TONS,60000.0*Q,100.0*VOLFR
 1004 FORMAT(/' Solid feedrate ',F6.3,' tons/hr'/
     *        ' Volumetric feedrate ',F5.1,' liters/min'/
     *        ' Solid content in feed by volume ',F5.1,' %')
      IF(TONS .GT. 2.5) THEN
        WRITE(8,1005)
 1005   FORMAT(/'****WARNING****'/
     *  'Feed rate is higher than recommended maximum of 2.5 tons/hr')
      END IF
      IF(TONS .LT. 0.5) THEN
        WRITE(8,1006)
 1006   FORMAT(/'****WARNING****'/
     *  'Feed rate is lower than recommended minimum of 0.5 tons/hr')
      END IF

C Calculate the composite partition factors and separation density
      WRITE(LU,304)
  304 FORMAT(/'Partition factors in this unit for the composite feed'/
     *        'Specific gravity  Partition factor')
      CALL ACTPART(FEED,OUT2,NDC,NGC,NSC,DER1)
      WRITE(LU,205) (PPROP(J),DER1(J),J = 1,NGC)
  205 FORMAT(5X,F6.2,8X,F8.4)

C     Write the actual partition function data to plot from the flowsheet screen
      CALL PartitionFunc(NGC,'SPLM',PPROP,DER1)

      RETURN
      END

      SUBROUTINE RLISP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
       ITERM=0
       LU=8
      R1=PARAM(1)
      R2=PARAM(2)
      IP=NINT(PARAM(3))
C
C  WRITE THE REPORT FILE.
      CALL HEADER(8,5,NUNIT,'SPIRAL CONCENTRATOR',19,'LISP')
      WRITE(8,1001)IP
 1001 FORMAT('Parameters:'/
     *       '  Number of spirals in parallel ',I3)
      IF(R2.LT.R1) THEN
        WRITE(8,1002)R2,R1
 1002   FORMAT('  Inner relative splitter position ',F6.3/
     *         '  Outer relative splitter position ',F6.3)
      ELSE
        WRITE(8,1003)R1
 1003   FORMAT('  Relative splitter position ',F6.3)
      END IF
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      VS=TMSF*SVM/PARAM(3)
      Q=VS+WTR*1.0E-3/PARAM(3)
      VOLFR=VS/Q
      TONS = TMSF*3.6/PARAM(3)
      WRITE(8,1004) TONS,60000.0*Q,100.0*VOLFR
 1004 FORMAT(/' Solid feedrate ',F6.3,' tons/hr'/
     *        ' Volumetric feedrate ',F5.1,' liters/min'/
     *        ' Solid content in feed by volume ',F5.1,' %')
      IF(60000.0*Q .GT. 108) THEN
        WRITE(8,1005)
 1005   FORMAT(/'****WARNING****'/
     *  'Feed rate is higher than modeled maximum of 108 liters/min')
      END IF
      IF(60000.0*Q .LT. 45) THEN
        WRITE(8,1006)
 1006   FORMAT(/'****WARNING****'/
     *  'Feed rate is lower than modeled minimum of 45 liters/min')
      END IF

C Calculate the composite partition factors and separation density
      WRITE(LU,304)
  304 FORMAT(/'Partition factors in this unit for the composite feed'/
     *        'Specific gravity  Partition factor')
      CALL ACTPART(FEED,OUT2,NDC,NGC,NSC,DER1)
      WRITE(LU,205) (PPROP(J),DER1(J),J = 1,NGC)
  205 FORMAT(5X,F6.2,8X,F8.4)

C     Write the actual partition function data to plot from the flowsheet screen
      CALL PartitionFunc(NGC,'LISP',PPROP,DER1)

      RETURN
      END


      SUBROUTINE RKELL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
       ITERM=0
       LU=8
      IP=NINT(PARAM(1))
C
C  WRITE THE REPORT FILE.
      CALL HEADER(8,5,NUNIT,'SPIRAL CONCENTRATOR',19,'KELL')
      WRITE(8,1001)IP
 1001 FORMAT('Parameters:'/
     *       '  Number of spirals in parallel ',I3)
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)

      VS=TMSF*SVM/PARAM(1)
      Q=VS+WTR*1.0E-3/PARAM(1)
      VOLFR=VS/Q
      TONS = TMSF*3.6/PARAM(1)
      WRITE(8,1004) TONS,60000.0*Q,100.0*VOLFR
 1004 FORMAT(/' Solid feedrate ',F6.3,' tons/hr'/
     *        ' Volumetric feedrate ',F5.1,' liters/min'/
     *        ' Solid content in feed by volume ',F5.1,' %')
      IF(TONS .GT. 3.32) THEN
        WRITE(8,1005)
 1005   FORMAT(/'****WARNING****'/
     *  'Feed rate is higher than the modeled maximum of 3.32 tons/hr')
      END IF
      IF(TONS .LT. 0.84) THEN
        WRITE(8,1006)
 1006   FORMAT(/'****WARNING****'/
     *  'Feed rate is lower than the modeled minimum of 0.84 tons/hr')
      END IF

C Calculate the composite partition factors and separation density
      WRITE(LU,304)
  304 FORMAT(/'Partition factors in this unit for the composite feed'/
     *        'Specific gravity  Partition factor')
      CALL ACTPART(FEED,OUT2,NDC,NGC,NSC,DER1)
      WRITE(LU,205) (PPROP(J),DER1(J),J = 1,NGC)
  205 FORMAT(5X,F6.2,8X,F8.4)

C     Write the actual partition function data to plot from the flowsheet screen
      CALL PartitionFunc(NGC,'KELL',PPROP,DER1)

      RETURN
      END

      SUBROUTINE RKNEL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C*
C*  Report writer for model KNEL
C
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      EXTERNAL RMKNEL
C
C  SPLIT THE FEED FOR MULTIPLE SCREENS IN PARALLEL AND RECOMBINE THE 
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMKNEL,PARAM(4))
      RETURN
      END
C
C
      SUBROUTINE RMKNEL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT


c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'Knelson concentrator',20,'KNEL')

      WRITE(LU,1001) (PARAM(I),I = 1,3)
 1001 FORMAT('Model parameters:'/
     *       '  Maximum recovery of free gold',T45,F5.1,'%'/
     *       '  Maximum recovery of locked gold',T45,F5.1,'%'/
     *       '  Solid content of concentrate',T45,F6.2,' kg/liter water'
     *)

c Calculate the grade and recovery of the gold and yield of solids
      DO MIN = 1,NMIN
        YIELD = 0.0
        RECOVERY = 0.0
        FEEDMIN = 0.0
        DO I = 1,NDC
          DO J= 1,NGC
            DO K = 1,NSC
              YIELD = YIELD + OUT2(I,J,K)
              RECOVERY = RECOVERY + OUT2(I,J,K)*GRDM(J,MIN)
              FEEDMIN = FEEDMIN + FEED(I,J,K)*GRDM(J,MIN)
            END DO
          END DO
        END DO
        GRADE = 100*RECOVERY/YIELD
        RECOVERY =100*RECOVERY/FEEDMIN
        WRITE(LU,1002)MIN, RECOVERY, MIN, GRADE
      END DO
 1002 FORMAT(/'Recovery of mineral',I3,' to concentrate',T45,G9.3,'%'/
     *        'Grade of mineral',I3,T45,G9.3,'%')
      FEEDGRADE = 1E6*FEEDMIN/TMSF
      YIELD = 100*YIELD/TMSF
      WRITE(LU,1003)FEEDGRADE,YIELD
 1003 FORMAT(/'Feed grade',T45,G9.3,' g/tonne'/
     *        'Yield of total solids to concentrate',T45,G9.3,'%')
      IP = NINT(PARAM(4))
      THROUGHPUT = TMSF*3.6
      WRITE(LU,1004) IP, THROUGHPUT
 1004 FORMAT('There are',I2,' units in parallel'/
     *       'Each unit handles',F9.2,' tonnes/hr')

      RETURN
      END

      SUBROUTINE RCONE(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
c     *********************************************************
C     Model for the Reichert cone using stratification.
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  SPLIT THE FEED FOR MULTIPLE CONES IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      EXTERNAL RMCONE
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMCONE,PARAM(2))
      RETURN
      END
C
      SUBROUTINE RMCONE(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
c     **********************************************************
C     Model for the Reichert cone using stratification.
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT

      LOGICAL ReportFlag
      COMMON /REPORTING/ReportFlag

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      INTEGER Configuration

c Set the report flag to signal N_Stratify that stratification profiles
c must be printed
      ReportFlag = .TRUE.

c Set up a file to take data to plot the stratification profiles.
      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')'CONE',NUNIT,Index

      WRITE(21,'(''Stratification profiles for unit '',I3,I5)')NUNIT,NGC

      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'Reichert cone',13,'CONE')

c Calculate the water flows if water is added to the cone.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTRF = WTR + WATERADD
        ELSE
          WTRF = TMSF*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTRF - WTR
        END IF
      ELSE
        WTRF = WTR
      END IF

      alpha = PARAM(1)
      Configuration = NINT(PARAM(3))
      TMS3 = 0.0
      OUT3 = 0.0
      WTR3 = 0.0

      WRITE(LU,'(/''Parameters:''/
     *''  Specific stratification constant:'',F7.4/
     *''  There is/are '',I2,'' cone/s in parallel''/)')PARAM(1),
     *    NINT(PARAM(2))

      IF (Configuration .EQ. 1) THEN
        !Single SV cone
        WRITE(LU,'(''Cone configuration used is SV with slot number ''
     *  ,I2,/)')NINT(PARAM(4))
        WRITE(21,'(''Cone configuration used is SV with slot number ''
     *  ,I2)')NINT(PARAM(4))
        WRITE(21,'('' SV stage '')')
        slot = PARAM(4)
        CALL SVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,
     *  NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot,alpha,PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'(/'' Grade of mineral '',I2,
     *    '' in heavies leaving cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
      END IF

      IF (Configuration .EQ. 2) THEN
        !DSV cone
        WRITE(LU,'(
     *  ''Cone configuration used is DSV with slot number '',I2,/)')
     *  NINT(PARAM(4))
        WRITE(21,'(''Cone configuration used is DSV with slot number ''
     *  ,I2)')NINT(PARAM(4))
        WRITE(21,'(''DSV stage ''\)')
        slot = PARAM(4)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,
     *  NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot,alpha,PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
      END IF

      IF (Configuration .EQ. 3) THEN
        !2DSV cone configuration.
        WRITE(LU,'(
     *  ''Cone configuration used is 2DSV with slot numbers '',2I2,/)')
     *  NINT(PARAM(4)),NINT(PARAM(5))
        WRITE(21,'(
     *  ''Cone configuration used is 2DSV with slot numbers '',2I2)')
     *  NINT(PARAM(4)),NINT(PARAM(5))
        WRITE(21,'(''First DSV stage ''\)')
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'(/'' Grade of mineral '',I2,
     *    '' in heavies leaving first SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        WRITE(21,'(''Second DSV stage ''\)')
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot2,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving second SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,2,2,NINT(PARAM(6)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in combined lights is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT1,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
      END IF

      IF (Configuration .EQ. 4) THEN
        !3DSV cone configuration.
        WRITE(LU,'(
     *  ''Cone configuration used is 3DSV with slot numbers '',3I2,/
     *  ''    The middling stream includes '',I1,'' stage/s.''/)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),NINT(PARAM(7))
        WRITE(21,'(
     *  ''Cone configuration used is 3DSV with slot numbers '',3I2)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6))
        WRITE(21,'(''First DSV stage ''\)')
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving first SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        WRITE(21,'(''Second DSV stage ''\)')
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot2,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving second SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,2,NINT(PARAM(7)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        WRITE(21,'(''Third DSV stage ''\)')
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving third SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,3,NINT(PARAM(7)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in combined lights is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT1,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
      END IF

      IF (Configuration .EQ. 5) THEN
        !4DSV cone configuration.
        WRITE(LU,'(
     *  ''Cone configuration used is 4DSV with slot numbers '',4I2/
     *  ''    The middling stream includes '',I1,'' stage/s.''/)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),NINT(PARAM(7)),
     *  NINT(PARAM(8))
        WRITE(21,'(
     *  ''Cone configuration used is 4DSV with slot numbers '',4I2)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),NINT(PARAM(7))
        WRITE(21,'(''First DSV stage ''\)')
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving first SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        WRITE(21,'(''Second DSV stage ''\)')
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot2,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving second SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,4,2,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        WRITE(21,'(''Third DSV stage ''\)')
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving third SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,4,3,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        WRITE(21,'(''Fourth DSV stage ''\)')
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot4,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving fourth SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,4,4,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in combined lights is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT1,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
      END IF

      IF (Configuration .EQ. 6) THEN
C       DSVSV configuration
        WRITE(LU,'(
     *  ''Cone configuration used is DSVSV with slot numbers '',2I2,/)
     *  ')
     *  NINT(PARAM(4)),NINT(PARAM(5))
        WRITE(21,'(
     *  ''Cone configuration used is DSVSV with slot numbers '',2I2)
     *  ')
     *  NINT(PARAM(4)),NINT(PARAM(5))
        WRITE(21,'(''First DSVSV stage ''\)')
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving second SV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
      END IF

      IF (Configuration .EQ. 7) THEN
        !2DSVSV configuration
        WRITE(LU,'(
     *  ''Cone configuration used is 2DSVSV with slot numbers '',4I2/
     *  ''    The middling stream includes '',I1,'' stage/s.''/)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),NINT(PARAM(7)),
     *  NINT(PARAM(8))
        WRITE(21,'(
     *  ''Cone configuration used is 2DSVSV with slot numbers '',4I2)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),NINT(PARAM(7))
        WRITE(21,'(''First DSVSV module ''\)')
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving first DSVSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        WRITE(21,'(''Second DSVSV module ''\)')
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,slot4,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving second DSVSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,2,2,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      IF (Configuration .EQ. 8) THEN
        !3DSVSV configuration
        WRITE(LU,'(
     *  ''Cone configuration used is 3DSVSV with slot numbers '',6I2/
     *  ''    The middling stream includes '',I1,'' stage/s.''/)'
     *  ) NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),NINT(PARAM(7)),
     *  NINT(PARAM(8)),NINT(PARAM(9)),NINT(PARAM(10))
        WRITE(21,'(
     *  ''Cone configuration used is 3DSVSV with slot numbers '',6I2)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),NINT(PARAM(7)),
     *  NINT(PARAM(8)),NINT(PARAM(9))
        WRITE(21,'(''First DSVSV module ''\)')
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        slot5 = PARAM(8)
        slot6 = PARAM(9)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving first DSVSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        WRITE(21,'(''Second DSVSV module ''\)')
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,slot4,alpha,
     *  PPROP,INDPP,NPP)

        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving second DSVSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,2,NINT(PARAM(10)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        WRITE(21,'(''Third DSVSV module ''\)')
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot5,slot6,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving third DSVSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,3,NINT(PARAM(10)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      IF (Configuration .EQ. 9) THEN
        !2DSVSV.DSV configuration
        WRITE(LU,'(
     *  ''Cone configuration used is 2DSVSV.DSV with slot numbers ''
     *  5I2,/
     *  ''    The middling stream includes '',I1,'' stage/s.'')')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),
     *  NINT(PARAM(7)),NINT(PARAM(8)),NINT(PARAM(9))
        WRITE(21,'(
     *  ''Cone configuration used is 2DSVSV.DSV with slot numbers ''
     *  5I2)')
     *  NINT(PARAM(4)),NINT(PARAM(5)),NINT(PARAM(6)),
     *  NINT(PARAM(7)),NINT(PARAM(8))
        WRITE(21,'(''First DSVSV module ''\)')
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        Slot5 = PARAM(8)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving first DSVSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        WRITE(21,'(''Second DSVSV module ''\)')
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,slot4,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving second DSVSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,2,NINT(PARAM(9)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        WRITE(21,'(''Final DSV module ''\)')
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot5,alpha,
     *  PPROP,INDPP,NPP)
        DO MIN = 1,NMIN
          WRITE(LU,'('' Grade of mineral '',I2,
     *    '' in heavies leaving DSV cone is'',F8.2,'' %'')')
     *    MIN,100*AvGrade(MIN,DER2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
        END DO
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,3,NINT(PARAM(9)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      ReportFlag = .FALSE.
      CLOSE(21)

      RETURN
      END
C
C
      SUBROUTINE RPAN1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
      RETURN
      END

      SUBROUTINE PartitionFunc(NGC,Model,PPROP,ParFunc)
C     *************************************************
C Set up the data to plot the classification function
      USE GLOBALS
      REAL PPROP(1),ParFunc(1)
      CHARACTER*4 Model
      CHARACTER*30 TITLE
      COMMON /MODELDAT/NUNIT

      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')Model,NUNIT,Index
      WRITE(TITLE,'(''Partition function for unit '',I2)')NUNIT
      WRITE(21,'(A30)') TITLE


      WRITE(21,'(I4)') NGC
      WRITE(21,'(G10.4,2x,G10.4)') (PPROP(I),ParFunc(I),I = 1,NGC)

      CLOSE(21)
      RETURN
      END
