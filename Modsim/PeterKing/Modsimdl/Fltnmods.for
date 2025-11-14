C*******************	   FLTNMODS.FOR       **************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE MODSIM MODELS FOR FLOTATION UNITS.           C                              C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
      SUBROUTINE FLTK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

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
C     11...Largest flotable particle  microns
C     12+..FLOTATION RATE CONSTANTS
C
      EXTERNAL MFLTK
C
C  SPLIT THE FEED FOR MULTIPLE BANKS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MFLTK,PARAM(9))

      RETURN
      END
C
C
      SUBROUTINE MFLTK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      REAL NU,MSC,MST
      INTEGER CELL

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

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

      WT=WTR/1000.0
      DSAVTH=0.0
      SAV=1.0
C Get the air-free pulp volume
      VOL = PARAM(2)*(1.0 - PARAM(3)*TAUB/60)
      IF(VOL .LE. 0.0) THEN
        WRITE(UnitDiagFile,'(/'' Specified value of aeration rate and/or
     * bubble''/'' residence time is too high in unit'',I3)') NUNIT
      END IF
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
C    ITERATE OVER ALL CELLS IN THE BANK
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
        DQPTTH=WS*DMSTTH + DVOLST
        WS=ERR/(THETA*DQPTTH+QPT)
        THETA=THETA+WS
        IF(THETA.LT.0.0) THETA=(THETA-WS)*0.5
        ICYCLE=ICYCLE+1
        IF(ABS(ERR).GT.0.001*VOL.AND.ICYCLE.LT.10) GO TO 30
        IF(ICYCLE.GE.10) FL=5
        S1=100.0*(1.0-SAV)
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

c Calculate the water flows if water is added to the bank.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WS = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WS - WTR2
          WTR2 = WS
        END IF
      END IF
      WTR3 = 0.0
      PARAM(7)=THETA
      RETURN
      END
C
      FUNCTION PHID(IG,ID,SIZE,Dmax,ColDel)
C     *************************************
C     THIS FUNCTION CALCULATES THE FUNCTION PHI FOR THE EFFECT OF
C     PARTICLE SIZE
      DIMENSION SIZE(1)
      EPSLN = 0.5*Dmax**2

C   PHID FOR OUTOKUMPU PLANT
C      EPSLN=3.0E-9
C      IF(IG.EQ.1) EPSLN=1.8E-9
C      IF(IG.EQ.2) EPSLN=1.25E-9
C      ColDel=5.0E-04

      PHID=2.33*SQRT(EPSLN)*EXP(-EPSLN/(SIZE(ID)**2))/SIZE(ID)
      IF(SIZE(ID) .LT. ColDel) PHID=PHID*(1.0-(SIZE(ID)/ColDel)**1.5)
      IF(SIZE(ID) .GE. ColDel) PHID=0.0
      RETURN
      END
C
      SUBROUTINE FLTN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C     *
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(NDC),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C  UNIT TYPE: BANK OF FLOTATION CELLS
C  **********************************
C
C    THIS SUBROUTINE USES THE DISCRETE DISTRIBUTED KINETIC
C    MODEL BASED ON THE CHEMICAL REACTION RATE APPROACH.
C    RATE OF FLOTATION OF SPECIES I IS PROPORTIONAL TO THE
C    MASS OF SPECIES I PRESENT IN THE CELL   R(I)=K(I)*W(I).
C    THE VOLUME OF THE CELL MUST BE SPECIFIED AND THE  PULP
C    RESIDENCE TIME IS CALCULATED TO BE CONSISTENT WITH THIS
C    VOLUME AND THE TAILINGS FLOW FROM THE CELL IN THE CIRCUIT
C
C    THE WATER BALANCE IS FIXED BY ASUMING THAT THE SOLIDS
C    HOLDUP PER UNIT VOLUME OF CELL IS FIXED AS USED BY D N SUTHERLAND
C    INT J MINERAL PROCESSING 4 (1977) 149-162
C
C
C  PARAMETERS IN ORDER
C  *******************
C    1....NUMBER OF CELLS IN SERIES
C    2....Number of banks in parallel
C    3....CELL VOLUME  M**3
C    4....SOLID HOLDUP IN KG/M**3
C    5....Air holdup as a percentage of the cell volume
C    5-   Specific rate contants
C
C
C  NOTES
C  *****
C    OUT3 HOLDS THE MASS FLOWS FROM CELL TO CELL
C    T HOLDS THE TOTAL TAILINGS MASS FLOW FROM CELL TO CELL
C
C
      EXTERNAL MFLTN
C
C  SPLIT THE FEED FOR MULTIPLE BANKS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MFLTN,PARAM(2))

      RETURN
      END
C
C
      SUBROUTINE MFLTN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     *******************************************************************
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      V=PARAM(3)*(1.0 - 0.01*PARAM(5))
      H=PARAM(4)
      NOCEL = NINT(PARAM(1))
      THETA=H*V/TMSF
C
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
              FK=PHI(SIZE(ID))*PARAM(5+IK)/60.0
              OUT1(ID,IG,IK)=OUT3(ID,IG,IK)/(1.0+FK*THETA)
              T=T+OUT1(ID,IG,IK)
              VOLSOL=VOLSOL + OUT1(ID,IG,IK)*THETA/(PPROP(IG)*1000.0)
              DTTH=DTTH-OUT3(ID,IG,IK)*FK/((1.0+FK*THETA)**2)
   20   CONTINUE
        TH=H*V/T
c        write(UnitDiagFile,*)'ITNO,T,TH,THETA ',ITNO,T,TH,THETA
        DELTA = -(THETA-TH)/(1.0 + TH*DTTH/T)
        THETA=THETA+DELTA
        IF(ABS(DELTA).GT.0.01*THETA) THEN
          IF(ITNO.LT.10) GO TO 10
          WRITE(UnitDiagFile,*)' Could not converge on a value of theta'
        END IF
        DO 30 ID=1,NDC
          DO 30 IG=1,NGC
            DO 30 IK=1,NSC
              WS=OUT2(ID,IG,IK)+OUT3(ID,IG,IK)-OUT1(ID,IG,IK)
              OUT2(ID,IG,IK)=WS
              TMS2=TMS2+OUT3(ID,IG,IK)-OUT1(ID,IG,IK)
              OUT3(ID,IG,IK)=OUT1(ID,IG,IK)
   30   CONTINUE
   40 CONTINUE
      TMS1=T
      WTR1=(V-VOLSOL)*1.0E3/THETA
      WTR2=WTR-WTR1
	Write(13,*)'WTR,WTR1,WTR2',WTR,WTR1,WTR2
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WS = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WS - WTR2
          WTR2 = WS
        END IF
      END IF
	Write(13,*)'WTR,WTR1,WTR2',WTR,WTR1,WTR2
      RETURN
      END
C
      FUNCTION PHI(SIZE)
C     ******************
C  SIZE FUNCTION FROM DN SUTHERLAND, "BATCH FLOTATION BEHAVIOUR OF COMPOSITE
C  PARTICLES" Minerals Engineering Vol 2.  pp. 351-367, 1989.
C  THIS WAS DETERMINED FOR CHALCOPYRITE IN THE MOUNT LYELL ORE.
C
      PHI=(0.0000349/SIZE)**0.9
      IF(PHI.GT.1.0) PHI=1.0
      RETURN
      END

      SUBROUTINE KLIM (TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C The Klimpel R-K flotation model
C Each G-class is presumed to be characterized by a value for the ultimate
C recovery R and the kinetic constant K.
C The water balance and therefore the residence time in a cell is specified
C by specifying the percent solids in the concentrate.
C
C   Parameters:
C       1.....Number of cells in the bank.
C       2.....Volume of each cell.
C       3.....Volume fraction of air in the pulp.
C       4.....Percent solids in concentrate
C       5.....Number of banks in parallel.
C       6.....Ultimate recovery for G-class 1.
C       7.....Kinetic constant for G-class 1.
C       8.....repeat 5 & 6
C

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
C  Split the feed for multiple units in parallel and recombine the
C  products before exit.
      EXTERNAL MKLIM
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MKLIM,PARAM(5))
      RETURN
      END
C
C
      SUBROUTINE MKLIM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      REAL, ALLOCATABLE::R(:),Kl(:)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      NOCELL = NINT(PARAM(1))
      CellVolume = PARAM(2)
      PulpVolume = CellVolume*(1.0 - 0.01*PARAM(3))
      PERCS = PARAM(4)
      IF (.NOT. ALLOCATED(R)) ALLOCATE(R(NGC))
      IF (.NOT. ALLOCATED(Kl)) ALLOCATE(Kl(NGC))
      DO J = 1,NGC
        R(J) = 0.01*PARAM(6+2*(J-1))
        Kl(J) = PARAM(7+2*(J-1))/60.0
      END DO

c Get an initial estimate of the average residence time.
      !Start with the largest possible value of theta.
      TMS1 = 0.0
      DO J = 1,NGC
        Factor = 1.0 - R(J)
        DO I = 1,NDC
          DO K = 1,NSC
            OUT1(I,J,K) = FEED(I,J,K)*Factor
            TMS1 = TMS1 + OUT1(I,J,K)
          END DO
        END DO
      END DO
      TMS2 = TMSF - TMS1
      WTR2 = TMS2*(100.0 - PERCS)/PERCS
      WTR1 = WTR - WTR2
      CALL SGM(OUT1,NDC,NGC,NSC,PPROP,SVM,SGA)
      TailsVolume = TMS1*SVM + WTR1*0.001
      IF (TailsVolume .LE. 0.0) Then
        Theta = 1000
      ELSE
        Theta = PulpVolume/TailsVolume
      END IF

      TMS = TMSF
      WTRF = WTR
      DER3 = FEED
      OUT2 = 0.0
      TMS2 = 0.0
      DO N = 1,NOCELL
        CALL KLIMPEL(DER3,TMS,OUT1,TMS1,OUT3,TMS3,NDC,NGC,NSC,
     *  WTRF,WTR1,WTR2,R,Kl,PERCS,PulpVolume,PPROP,INDPP,FL,NPP,Theta)
        OUT2 = OUT2 + OUT3
        TMS2 = TMS2 + TMS3
        DER3 = OUT1
        TMS = TMS1
        WTRF = WTR1
      END DO
      WTR2 = WTR - WTR1

c Calculate the water flows if water is added to the unit.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WTR2 = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR2 - WTR + WTR1
        END IF
      END IF

C De-allocate and return to MODSIM from the model subroutine.
      IF ( ALLOCATED(R)) DEALLOCATE(R)
      IF ( ALLOCATED(Kl)) DEALLOCATE(Kl)
      RETURN
      END

      SUBROUTINE KLIMPEL(FEED,TMSF,TAILS,TMS1,CONC,TMS2,NDC,NGC,NSC,
     *WTR,WTR1,WTR2,R,Kl,PERCS,PulpVolume,PPROP,INDPP,FL,NPP,Theta)
C     **************************************************************
C The Klimpel flotation model for a single cell.
c Theta is an estimate of the average residence time.
c PERCS is the percentage solid in the concentrate.
      REAL FEED(NDC,NGC,NSC),TAILS(NDC,NGC,NSC),CONC(NDC,NGC,NSC)
      REAL PPROP(*)
      INTEGER INDPP(NPP,2),FL
      REAL R(NGC),Kl(NGC)

      !Find the average residence time by iteration.
      ITNO = 1
   10 CONTINUE
c      write(*,*)'ITNO,Theta',ITNO,Theta
      TMS1 = 0.0
      DTMS1DTheta = 0.0
      DO J = 1,NGC
        WS = R(J)*Kl(J)*Theta/(1.0 + Kl(J)*Theta)
        DWSDTheta = WS/Theta - Kl(J)*WS/(1.0 + Kl(J)*Theta)
        Factor = 1.0 - WS
        DFactorDTheta = -DWSDTheta
        DO I = 1,NDC
          DO K = 1,NSC
            TAILS(I,J,K) = FEED(I,J,K)*Factor
            TMS1 = TMS1 + TAILS(I,J,K)
            DTMS1DTheta = DTMS1DTheta + FEED(I,J,K)*DFactorDTheta
          END DO
        END DO
      END DO
      TMS2 = TMSF - TMS1
      DTMS2DTheta = -DTMS1DTheta
      WTR2 = TMS2*(100.0 - PERCS)/PERCS
      WTR1 = WTR - WTR2
      DWTR1DTheta = -DTMS2DTheta*(100.0 - PERCS)/PERCS
      CALL SGM(TAILS,NDC,NGC,NSC,PPROP,SVM,SGA)
      TailsVolume = TMS1*SVM + 0.001*WTR1
      DTvDTheta = SVM*DTMS1DTheta + 0.001*DWTR1DTheta
      Del = (PulpVolume - Theta*TailsVolume)/
     *(TailsVolume+Theta*DTvDTheta)
      IF(ABS(DEL) .GT. 0.01*Theta .AND. ITNO .LE. 10) THEN
        Theta = Theta + Del
        ITNO = ITNO + 1
        GO TO 10
      END IF
      CONC = FEED - TAILS
      RETURN
      END
