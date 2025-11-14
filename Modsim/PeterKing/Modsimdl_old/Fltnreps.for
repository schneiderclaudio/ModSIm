      SUBROUTINE RFLTK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
      EXTERNAL RMFLTK
C
C  SPLIT THE FEED FOR MULTIPLE BANKS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMFLTK,PARAM(9))

      RETURN
      END
C
C
      SUBROUTINE RMFLTK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT
      REAL NU,MSC,MST
      INTEGER CELL

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
C     FLOTATION MODEL DUE TO RP KING
C     ******************************
C     REF.   MODEL FOR THE DESIGN AND CONTROL OF FLOTATION PLANTS
C            APCOM 10 (1972) S.A. INST. MIN. METALL.
C            EDS SALAMON AND LANCASTER P.341
C
C     THIS MODEL IS THE DISCRETE DISTRIBUTED FLOTATION CONSTANT MODEL
C     THE SUBROUTINE CALCULATES THE TRUE CELL RESIDENCE TIME. THE
C     PERCENTAGE SOLIDS IN THE FROTH IS ASSUMED KNOWN
C
C     PARAMETERS IN ORDER
C     *******************
C     1....NUMBER OF CELLS IN SERIES FOR THIS STAGE
C     2....CELL VOLUME
C     3....AERATION RATE PER M**3 OF CELL VOLUME
C     4....FROTH TRANSMISSION COEFFIENT
C     5....BUBBLE SIZE
C     6....BUBBLE RESIDENCE TIME
C     7....ESTIMATE OF CELL HOLDING TIME
C     8....PERCENT SOLIDS IN CONCENTRATE
C     9....NUMBER OF BANKS IN PARALLEL
C     10...Size at maximum recovery  microns
C     11...Largest floatable particle  microns
C     12+...Specific flotation rate constants  m/s
C
      NOCELL=PARAM(1)
      GG=PARAM(3)*PARAM(2)/60.0
      GAMMA=PARAM(4)
      BUBSI=PARAM(5)
      BUBSIM=BUBSI*1000.0
      TAUB=PARAM(6)
      THETA=PARAM(7)
      PERCS=PARAM(8)
      Dmax = 1.0e-6*PARAM(10)
      ColDel = 1.0e-6*PARAM(11)

      !Record the last parameter index before the rate constants
      IPAR = 11

      PERCSF = 100.0*TMSF/(TMSF+WTR)
C Get the air-free pulp volume
      VOL = PARAM(2)*(1.0 - PARAM(3)*TAUB/60)
C
C  Start writing the report file
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BANK OF FLOTATION CELLS',23,'FLTK')
      NBANKS = NINT(PARAM(9))
      WRITE(LU,100)NOCELL,NBANKS,PARAM(2),PARAM(3),VOL,GAMMA,BUBSIM,
     *TAUB, PERCSF,PERCS,PARAM(10),PARAM(11)
  100 FORMAT(/
     *' There are ',I2,' cells in this bank and ',I2,
     *' banks in parallel'/
     *' Volume of cells in this bank ',F9.3,' m**3'/
     *' Aeration rate ',F8.4,' m**3/min per m**3 of cell volume.'/
     *' Air-free pulp volume ',F9.3,' m**3'/
     *' Froth transmission coefficient ',F7.3,/
     *' Bubble size ',F7.3,' mms'/
     *' Bubble residence time ',F9.2,' secs'/
     *' Percent solids in feed ',F6.1/
     *' Percent solids in concentrate ',F6.1/
     *' Maximum flotation recovery at ',F7.1,' microns'/
     *' Largest floatable particle    ',F7.0,' microns')
      WRITE(LU,110) NSC, (PARAM(IPAR+K), K = 1,NSC)
  110 FORMAT(' Flotation rate constants are distributed over ',I3,
     *' values:'/(8G10.3))
      WRITE(LU,*)
      WT=WTR/1000.0
      DSAVTH=0.0
      SAV=1.0
      A=6.0*TAUB*GG/(BUBSI*VOL)
C
      DO 20 ID=1,NDC
      DO 20 IG=1,NGC
      DO 20 IK=1,NSC
        OUT3(ID,IG,IK)=FEED(ID,IG,IK)
        OUT2(ID,IG,IK)=0.0
   20 CONTINUE
      WTR2=0.0
      TMS2=0.0
C
C ITERATE OVER ALL CELLS IN THE BANK
C     OUT3  HOLDS THE FEED FLOWS TO SUCCESSIVE CELLS
C     DER3 HOLDS THE CONCENTRATE FLOWS FROM SUCCESSIVE CELLS
      DO  900 CELL =1,NOCELL
        ICYCLE=0
 30     WK=0.0
        DWKTH=0.0
        DMSTTH=0.0
        MSC=0.0
        MST=0.0
        VOLSC=0.0
        VOLST=0.0
        DVOLST=0.0
C
        DO 40 ID=1,NDC
          ETA=5.55E-04/SIZE(ID)
          DO 41 IG=1,NGC
            DTTH=0.0
            DENS=PPROP(IG)*1000.0
            PHI=PHID(IG,ID,SIZE,Dmax,ColDel)
            DO 39 IK=1,NSC
              OUT1(ID,IG,IK)=0.0
              DER3(ID,IG,IK)=0.0
              IF(OUT3(ID,IG,IK).LT.0.00000001) GO TO 39
              PHIKK=PHI*PARAM(IK + IPAR)
              NU=THETA*GAMMA*PHIKK*A*SAV
              DNUTH=NU/THETA+NU*DSAVTH/SAV
              WS=OUT3(ID,IG,IK)/(1.0+NU)
              WK=WK+PHIKK*ETA*WS
              OUT1(ID,IG,IK)=WS
              DWSTH=-WS*DNUTH/(1.0+NU)
              DWKTH=DWKTH+PHIKK*ETA*DWSTH
              DTTH=DTTH+DWSTH
              DER3(ID,IG,IK)=OUT3(ID,IG,IK)-OUT1(ID,IG,IK)
              MST=MST+OUT1(ID,IG,IK)
              MSC=MSC+DER3(ID,IG,IK)
              VOLST=VOLST+OUT1(ID,IG,IK)/DENS
              VOLSC=VOLSC+DER3(ID,IG,IK)/DENS
 39         CONTINUE
            DMSTTH=DMSTTH+DTTH
            DVOLST=DVOLST+DTTH/DENS
 41       CONTINUE
 40     CONTINUE
C
        WS=THETA*WK*TAUB/VOL
        WS1=0.0
        IF(WS.LT.50.0)WS1=EXP(-WS)*(1.0+WS)
        DSAVTH=(WS1-1.0)*(1.0/THETA+DWKTH/WK)/WS
        WSA=WS
        IF(WSA.LT.50) WS=(1.0-EXP(-WS))/WS
        IF(WSA.GE.50) WS=1.0/WS
        IF(WSA.LT.0.00001) WS=1.0
        WS1=WS-SAV
        SAV1=SAV
        SAV=WS
        IF(ABS(WS1).GT.0.01*SAV1) GO TO 30
        WS=(100.0-PERCS)/(PERCS*1000.0)
        WT2=WS*MSC
        WT1=WT-WT2
        QPT=WT1+VOLST
        ERR=VOL-THETA*QPT
        DQPTTH=WS*DMSTTH +DVOLST
        WS=ERR/(THETA*DQPTTH+QPT)
        THETA=THETA+WS
        IF(THETA.LT.0.0) THETA=(THETA-WS)*0.5
        ICYCLE=ICYCLE+1
        IF(ABS(ERR).GT.0.001*VOL.AND.ICYCLE.LT.10) GO TO 30
        IF(ICYCLE.GE.10) FL=5
        S1=100.0*(1.0-SAV)
        WRITE(LU,2000) NUNIT,CELL,S1,THETA,ERR,ICYCLE
 2000 FORMAT(10X,' Unit number ',I3,'  Cell number ',I3,/
     *' Bubble loading= ',F6.2,'%'/'Pulp residence time= ',F8.2,' secs'/
     *' Difference between pulp vol and res time * flowrate = ',G9.3,/
     *' Iterations needed= ',I4)
        WTR1=WT1*1000.0
        WTR2=WT2*1000.0+WTR2
        TMS2=MSC+TMS2
        TMS1=MST
        DO 50 ID=1,NDC
        DO 50 IG=1,NGC
        DO 50 IK=1,NSC
          OUT3(ID,IG,IK)=OUT1(ID,IG,IK)
          OUT2(ID,IG,IK)=DER3(ID,IG,IK)+OUT2(ID,IG,IK)
   50   CONTINUE
        WT=WT1
  900 CONTINUE
      PSTAIL=100.0*TMS1/(TMS1+WTR1)
      WRITE(LU,1005) PSTAIL
 1005 FORMAT(/' Percent solids in tailings from this bank = ',F7.2)

      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WTR2 = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
        END IF
        WRITE(LU,1006) 100*TMS2/(TMS2 + WTR2)
        IF (WATERADD .LT. 0.0)THEN
          WRITE(LU,1007)
        end if
      END IF
 1006 FORMAT(/' This bank has water addition to the concentrate launder'
     */'  Percent solid in final concentrate is ',F7.2)
 1007 FORMAT(' This is higher than the floated concentrate!')


C      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER2,NDC,NGC,NSC,NGCM,GRDM,NMIN,
C     *PPROP)

      RETURN
      END
C
      SUBROUTINE RFLTN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C     *
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(NDC),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  PARAMETERS IN ORDER
C  *******************
C    1....NUMBER OF CELLS IN SERIES
C    2....Number of banks in parallel
C    3....CELL VOLUME  M**3
C    4....SOLID HOLDUP IN KG/M**3
c    5....Air holdup as a percentage of the cell volume.
C    6+...Specific rate contants in this bank
C
C  NOTES
C  *****
C    OUT3 HOLDS THE MASS FLOWS FROM CELL TO CELL
C    T HOLDS THE TOTAL TAILINGS MASS FLOW FROM CELL TO CELL
C
C
      EXTERNAL RMFLTN
C
C  SPLIT THE FEED FOR MULTIPLE BANKS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMFLTN,PARAM(2))

      RETURN
      END
C
C
      SUBROUTINE RMFLTN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
      V=PARAM(3)*(1.0 - 0.01*PARAM(5))
      H=PARAM(4)
      NBANKS = PARAM(2)
      NOCEL=NINT(PARAM(1))
      THETA=H*V/TMSF
C
C  Start writing the report file
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BANK OF FLOTATION CELLS',23,'FLTN')
      WRITE(LU,100)NOCEL,NBANKS,PARAM(3),H,PARAM(5)
  100 FORMAT(/
     *' There are ',I2,' cells in this bank and ',I2,
     *' banks in parallel.'/
     *' Volume of cells in this bank ',T40,F8.2,' m**3'/
     *' Solids holdup in the cells',T40,F7.1,' kg solid/m**3 pulp'/
     *' Air holdup in the cells',T40,F6.2,' %' )
      WRITE(LU,103) TMSF,WTR
  103 FORMAT(
     *' Solid feedrate ',T40,G10.4,' kg/s'/
     *' Water feedrate ',T40,G10.4,' kg/s')
      WRITE(LU,101) NSC
  101 FORMAT(/
     *' Rate constants are distributed over',I3, ' values')
      WRITE(LU,102)(PARAM(K), K = 6,NSC+5)
  102 FORMAT(8G10.4)
      TMS2=0.0
      DO 110 ID =1,NDC
        DO 110 IG =1,NGC
          DO 110 IK =1,NSC
            OUT3(ID,IG,IK)=FEED(ID,IG,IK)
            OUT2(ID,IG,IK)=0.0
  110 CONTINUE
      DO 40 NC=1,NOCEL
        ITNO=0
   10   CONTINUE
        ITNO=ITNO+1
        DTTH=0.0
        T=0.0
        VOLSOL=0.0
        DO 20 ID=1,NDC
          DO 20 IG=1,NGC
            DO 20 IK=1,NSC
              FK=PHI(SIZE(ID))*PARAM(IK+5)/60.0
              OUT1(ID,IG,IK)=OUT3(ID,IG,IK)/(1.0+FK*THETA)
              T=T+OUT1(ID,IG,IK)
              VOLSOL=VOLSOL + OUT1(ID,IG,IK)*THETA/(PPROP(IG)*1000.0)
              DTTH=DTTH-OUT3(ID,IG,IK)*FK/((1.0+FK*THETA)**2)
   20   CONTINUE
        TH=H*V/T
        DELTA = -(THETA-TH)/(1.0 + TH*DTTH/T)
        THETA=THETA+DELTA
C        WRITE(LU,1001) THETA,NC,ITNO
 1001   FORMAT(' RESIDENCE TIME IS ',G8.3,' IN CELL ',I4,
     *  ' AT ITERATION ',I4)
        IF(ABS(DELTA).GT.0.01*THETA.AND.ITNO.LT.10) GO TO 10
        IF(ITNO.GE.10) WRITE(LU,1000)NUNIT,NC
 1000   FORMAT('  WARNING:NO CONVERGENCE ON THETA IN UNIT',I4,
     *  ' CELL',I4)
        WRITE(LU,2000) NUNIT,NC,THETA
 2000   FORMAT(/' Unit number ',I3,'  Cell number ',I3,/
     *  '    Pulp residence time = ',F8.2,' secs')
        WTMS2 = 0.0
        DO 30 ID=1,NDC
          DO 30 IG=1,NGC
            DO 30 IK=1,NSC
              WS=OUT2(ID,IG,IK)+OUT3(ID,IG,IK)-OUT1(ID,IG,IK)
              OUT2(ID,IG,IK)=WS
              WTMS2=WTMS2+OUT3(ID,IG,IK)-OUT1(ID,IG,IK)
              OUT3(ID,IG,IK)=OUT1(ID,IG,IK)
   30   CONTINUE
        TMS2 = TMS2 + WTMS2
        WRITE(LU,2010) NC,T,NC,(V-VOLSOL)*1.0E3/THETA
 2010   FORMAT(
     *  '    Tailings transfer from cell',I3,' is ',G10.4,' kg/s'/
     *  '    Water transfer from cell',I3,' is ',G10.4,' kg/s')
        WRITE(LU,1010)NC,WTMS2
 1010   FORMAT(
     *  '    Concentrate solid flow from cell',I3,' is ',G10.4,' kg/s')
        DO Min = 1,NMIN
          WS = AvGrade(MIN,OUT2,NDC,NGC,NSC,GRDM,NGCM,NMIN)
          Write(LU,1020)MIN,NC,WS
 1020     FORMAT(
     *    '     Grade of mineral',I3,' in concentrate from cell',I3,
     *    ' = ',G10.4)
        END DO
   40 CONTINUE
      TMS1=T
      WTR1=(V-VOLSOL)*1.0E3/THETA
 2020 FORMAT(/' ****ERROR****'/
     *'    The value specified for the solids holdup is incompatible'/
     *'    with the solid content of the feed to this bank.')
      WTR2=WTR-WTR1
      IF(WTR1 .LE. 0.0) THEN
        WRITE(LU, 2020)
        WRITE(LU, 2030)
      END IF
 2030 FORMAT('    The solid content of the feed is too low or the'/
     *'    specified holdup is too high.')
      IF(WTR2 .LE. 0.0) THEN
        WRITE(LU, 2020)
        WRITE(LU, 2040)
      END IF
 2040 FORMAT('    The solid content of the feed is too high or the'/
     *'    specified holdup is too low.')

      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WTR2 = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
        END IF
        WRITE(LU,1006) 100*TMS2/(TMS2 + WTR2)
        IF(WATERADD .LE. 0.0) THEN
          WRITE(LU,1007)
        END IF
      END IF
 1006 FORMAT(/'This bank has water addition to the concentrate launder.'
     */'Percent solid in final concentrate is ',F7.2)
 1007 FORMAT(' This requires water to be removed from the launder!'/
     *' The specified solid holdup is too high.')

c      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER2,NDC,NGC,NSC,NGCM,GRDM,NMIN,
c     *PPROP)

      RETURN
      END
C
      SUBROUTINE RKLIM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C     *
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(NDC),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  PARAMETERS IN ORDER
C  *******************
C   Parameters:
C       1.....Number of cells in the bank.
C       2.....Volume of each cell.
C       3.....Volume fraction of air in the pulp.
c       4.....Pecent solids in concentrate.
C       5.....Number of banks in parallel.
C       6.....Ultimate recovery for G-class 1.
C       7.....Kinetic constant for G-class 1.
C       8.....repeat 5 & 6
C
C  NOTES
C  *****
C
      EXTERNAL RMKLIM
C
C  SPLIT THE FEED FOR MULTIPLE BANKS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMKLIM,PARAM(5))

      RETURN
      END
C
C
      SUBROUTINE RMKLIM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
      V=PARAM(2)*(1.0 - 0.01*PARAM(3))
      NBANKS = NINT(PARAM(5))
      NOCEL=NINT(PARAM(1))
C
C  Start writing the report file
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BANK OF FLOTATION CELLS',23,'KLIM')
      WRITE(LU,100)NOCEL,NBANKS,PARAM(2),PARAM(3),PARAM(4)
  100 FORMAT(/
     *' There are ',I2,' cells in this bank and ',I2,
     *' banks in parallel.'/
     *' Volume of cells in this bank ',T40,F8.2,' m**3'/
     *' Air holdup in the cells',T40,F6.2,' %'/
     *' Percent solids in concentrate',T40,F6.2 )
      WRITE(LU,103) TMSF,WTR
  103 FORMAT(
     *' Solid feedrate ',T40,G10.4,' kg/s'/
     *' Water in feed ',T40,G10.4,' kg/s')
      WRITE(LU,102)(J,PARAM(2*J + 4),PARAM(2*J + 5), J = 1,NGC)
  102 FORMAT(/' Ultimate recovery and kinetic constant for each G-class'
     */ (I3,2G10.4))

      WRITE(LU,104)WTR1,WTR2
  104 FORMAT(/
     *' Water flowrate in tailing ',T40,G10.4,' kg/s'/
     *' Water flowrate in concentrate ',T40,G10.4,' kg/s')

 2020 FORMAT(/' ****ERROR****'/
     *'    The value specified for the percent solids in the concentrate
     * is incompatible'/
     *'    with the solid content of the feed to this bank.')
      IF(WTR1 .LE. 0.0) THEN
        WRITE(LU, 2020)
        WRITE(LU, 2030)
      END IF
 2030 FORMAT('    The solid content of the feed is too low or the'/
     *'    specified % solid in the conentrate is too low.')

      IF(UNITWFEED) THEN
        WATERADD = WTR2 - WTR + WTR1
        WRITE(LU,1006) 100*TMS2/(TMS2 + WTR2)
        IF (WATERADD .LT. 0.0)THEN
          WRITE(LU,1007)
        end if
      END IF
 1006 FORMAT(/'This bank has water addition to the concentrate launder.'
     */'  Percent solid in final concentrate is ',F7.2)
 1007 FORMAT(' This is higher than the floated concentrate!')

c      CALL COALPERF(TMSF,TMS2,FEED,OUT2,DER2,NDC,NGC,NSC,NGCM,GRDM,NMIN,
c     *PPROP)

      RETURN
      END
