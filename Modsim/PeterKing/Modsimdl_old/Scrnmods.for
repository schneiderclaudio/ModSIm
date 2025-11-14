C*******************     SCRNMODS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE MODSIM MODELS FOR SCREENS                    C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE DSC2 (TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C*
C*  SIMULATION OF A DOUBLE DECK SCREEN USING THE V.K. KARRA PROCEDURE
C*
C*  SEE KARRA,V.K., DEVELOPMENT OF A MODEL FOR PREDICTING THE
C*  SCREENING PERFORMANCE OF A VIBRATING SCREEN. CIM BULLETIN
C*  NO 804, APRIL 1978, P.167-171.
C*
C*      NDC: NUMBER OF PARTICLE SIZE FRACTIONS
C*      NGC: NUMBER OF G-CLASSES OF THE FEED STREAM
C*      NSC: NUMBER OF S-CLASSES OF THE FEED STREAM
C*
C
C       PARAMETERS IN ORDER ARE:
C
C       1..... SQUARE MESH OF APERTURE OF TOP DECK
C       2..... SQUARE MESH OF APERTURE OF SECOND DECK
C       3..... WIRE DIAMETER OF TOP DECK
C       4..... WIRE DIAMETER OF SECOND DECK
C       5..... ANGLE OF THE SCREEN WITH RESPECT TO THE HORIZONTAL
C       6..... LENGTH OF THE SCREEN (m.)
C       7..... WIDTH OF THE SCREEN (m.)
C       8..... BULK DENSITY OF THE MINERAL (Kg/m3)
C       9..... SCREEN MATERIAL TYPE
C      10..... LENGTH OF LOWER DECK
C      11..... NUMBER OF SCREENS IN PARALLEL
C      12..... Surface water on oversize from top deck.
C      13..... Surface water on oversize from lower deck.
C
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      EXTERNAL MDSC2
C
C  SPLIT THE FEED FOR MULTIPLE SCREENS IN PARALLEL AND RECOMBINE THE 
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MDSC2,PARAM(11))

c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 + WTR3 - WTR
        END IF
      END IF

      RETURN
      END
C
C
      SUBROUTINE MDSC2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      EXTERNAL D501,PF2
C
C   TOP DECK CALCULATIONS
C   *********************
C
      THETA=PARAM(5)
      A11=PARAM(6)
      A12=PARAM(7)
      AREA=A11*A12
      U=PARAM(8)
      JT=PARAM(9)
      SCOS=0.0
      RW=0.10
      S=1.
      H1=PARAM(1)
      DIA1=PARAM(3)
      SurfaceWaterTop = PARAM(12)
      SurfaceWaterLower = PARAM(13)
      CALL KARRA(S,H1,DIA1,THETA,AREA,U,TMSF,FEED,SIZE,DER1,DER2,DER3,N
     *DC,NGC,NSC,WTR,HT,THEO,Q,R,XN,A,B,C,D,E,F,W,G,VEL)
      ALPHA=0.0

      CALL CLASSI(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,
     *WTR,WTR1,WTR2,RW,SIZE,D501,SCOS,PF2,PPROP,LU)

      WTR2 = SurfaceWaterTop*TMS2/(100.0 - SurfaceWaterTop)
      WTR1 = WTR - WTR2
C
C  LOWER DECK CALCULATIONS
C  ***********************
      S=2.
      H2=PARAM(2)
      DIA2=PARAM(4)
      A11=PARAM(10)
      A12=PARAM(7)
      AREA=A11*A12
      TMS=TMS1
      WTRF=WTR1
      CALL KARRA(S,H2,DIA2,THETA,AREA,U,TMS1,OUT1,SIZE,DER1,DER2,DER3,N
     *DC,NGC,NSC,WTR,HT,THEO,Q,R,XN,A,B,C,D,E,F,W,G,VEL)

      CALL CLASSI(TMS,TMS1,TMS3,OUT1,OUT1,OUT3,DER1,DER3,NDC,NGC,NSC,
     *WTRF,WTR1,WTR3,RW,SIZE,D501,SCOS,PF2,PPROP,LU)
      WTR3 = SurfaceWaterLower*TMS3/(100.0 - SurfaceWaterLower)
      WTR1 = WTRF - WTR3

      RETURN
      END
C
C
      SUBROUTINE SCR2 (TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C***********************************************************************
C*                                                                     *
C*  SIMULATION OF A SINGLE DECK SCREEN USING THE V.K. KARRA PROCEDURE  *
C*                                                                     *
C*  SEE KARRA,V.K., DEVELOPMENT OF A MODEL FOR PREDICTING THE          *
C*  SCREENING PERFORMANCE OF A VIBRATING SCREEN. CIM BULLETIN          *
C*  NO 804, APRIL 1978, P.167-171.                                     *
C*                                                                     *
C*      NDC: NUMBER OF PARTICLE SIZE FRACTIONS                         *
C*      NGC: NUNBER OF G-CLASSES OF THE FEED STREAM                    *
C*      NSC: NUMBER OF S-CLASSES OF THE FEED STREAM                    *
C*                                                                     *
C***********************************************************************
C
C       PARAMETERS IN ORDER ARE:
C
C       1..... SQUARE MESH OF APERTURE OF THE DECK
C       2..... WIRE DIAMETER OF THE DECK
C       3..... ANGLE OF THE SCREEN WITH RESPECT TO THE HORIZONTAL
C       4..... LENGTH OF THE SCREEN (m.)
C       5..... WIDTH OF THE SCREEN (m.)
C       6..... BULK DENSITY OF THE ORE (Kg/m3)
C       7..... SCREEN MATERIAL TYPE
c       8..... Surface water on the oversize %
C       9..... NUMBER OF SCREENS IN PARALLEL.
C
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      EXTERNAL MSCR2

C
C
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MSCR2,PARAM(9))
      RETURN
      END
C
C
      SUBROUTINE MSCR2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,G RDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      EXTERNAL D501,PF2
C
C
      THETA=PARAM(3)
      A11=PARAM(4)
      A12=PARAM(5)
      U=PARAM(6)
      JT=PARAM(7)
      SurfaceWater = PARAM(8)
      AREA=A11*A12
      SCOS=0.0
      RW=0.10
      S=1.
      H=PARAM(1)
      LU=8
      ITERM=5
      DIA=PARAM(2)
      CALL KARRA(S,H,DIA,THETA,AREA,U,TMSF,FEED,SIZE,DER1,DER2,DER3,
     *NDC,NGC,NSC,WTR,HT,THEO,Q,R,XN,A,B,C,D,E,F,W,G,VEL)
      ALPHA=0.0
      CALL CLASSI(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,
     *WTR,WTR1,WTR2,RW,SIZE,D501,SCOS,PF2,PPROP,LU)

      WTR2 = SurfaceWater*TMS2/(100.0 - SurfaceWater)
      WTR1 = WTR - WTR2
      WTR3 = 0.0

c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF

      RETURN
      END
C
C
      SUBROUTINE KARRA(S,H,DIA,THETA,AREA,U,TMSF,FEED,SIZE,DER1,DER2,DER
     *3,NDC,NGC,NSC,WTR,HT,THEO,Q,R,XN,A,B,C,D,E,F,W,G,VEL)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),SIZE(1)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/FND501/ D50
      COMMON/FUNPF2/ AN
      HT=((H+DIA)*COS(THETA*0.017))-DIA
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      CALL FRPASS(DER2,DER3,NDC,P,HT)
      THEO=(P*TMSF*3.6)/AREA
      Q=(1.-P)*100.
      PSIZE=HT*0.5
      CALL FRPASS(DER2,DER3,NDC,P,PSIZE)
      R=100.*P
      PSIZE=HT*1.25
      CALL FRPASS(DER2,DER3,NDC,XN1,PSIZE)
      PSIZE=HT*0.75
      CALL FRPASS(DER2,DER3,NDC,XN2,PSIZE)
      XN=XN1-XN2
      OA=H**2/(H+DIA)**2
      OA=OA*100.0
      OAF=OA/(21.5*ALOG10(H)+101.5)
      CALL FACTOR (HT,Q,R,S,U,WTR,A,B,C,D,E,F,W,OAF)
      G=0.844*((1.-XN)**3.453)
      D50=HT*((THEO/(W*G))**(-0.148))
      AN=5.846
      RETURN
      END
C
C
C
      SUBROUTINE FACTOR(HT,Q,R,S,U,WTR,A,B,C,D,E,F,W,OAF)
C     **************************************************
C

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      P=HT*1000.0
      IF (P.LT.50.8) A=12.1286*(P**0.3162)-10.2991
      IF (P.GE.50.8) A=0.3388*P+14.4122
      !Smallest aperture is 1mm
      If(A .LT. 1.83) A = 1.83
      A=A*OAF
      IF (Q.GT.87.) B=-0.0425*Q+4.275
      IF (Q.LE.87.) B=-0.012*Q+1.6
      IF (R.LE.30.) C=0.012*R+0.7
      IF (R.LT.55..AND.R.GT.30.) C=0.1528*(R**0.564)
      IF (R.LT.80..AND.R.GE.55.) C=0.0061*(R**1.37)
      IF (R.GE.80.) C=0.05*R-1.5
      D=1.1-0.1*S
      T=1.26*P
      IF(UNITWFEED) THEN
       IF (T.LT.1.) E=1.0
       IF (T.GE.1..AND.T.LE.2.) E=T
       IF (T.GT.2..AND.T.LT.4.) E=1.5+0.25*T
       IF (T.GE.4..AND.T.LE.6.) E=2.5
       IF (T.GT.6..AND.T.LE.10.) E=3.25-0.125*T
       IF (T.GT.10..AND.T.LT.12.) E=4.5-0.25*T
       IF (T.GE.12..AND.T.LE.16.) E=2.1-0.05*T
       IF (T.GT.16..AND.T.LT.24.) E=1.5-0.0125*T
       IF (T.GE.24..AND.T.LE.32.) E=1.35-0.00625*T
       IF (T.GT.32.) E=1.15
      ELSE
       E=1.0
      END IF
      F=U/1602.
      W=A*B*C*D*E*F
      RETURN
      END
C
      SUBROUTINE SCRN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  SIMPLE IDEAL MODEL FOR SCREENING
C
C     PARAMETERS IN ORDER
C     *******************
C     1....MESH SIZE OF SCREEN
C     2....SCREEN EFFICIENCY
C     3....surface water on screen oversize
C     4....SCREEN DIMENSIONS?
C     5....SCREEN LENGTH
C     6....SCREEN WIDTH
C     7....Number of screens in parallel
C
      EXTERNAL MSCRN

C
C
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MSCRN,PARAM(7))
      RETURN
      END
C
C
      SUBROUTINE MSCRN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,G RDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
	USE GLOBALS
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT


      TMS1=0.0
      TMS2=0.0
      EFF=PARAM(2)
      SurfaceWater = 0.01 * PARAM(3)
      DO 20 J=1,NGC
      DO 20 K=1,NSC
	IF (MeshSizes(1) .LT. PARAM(1)) THEN
	  IF(SIZE(1) .LT. PARAM(1)) THEN
	    factor = 1.0
	  ELSE
	    factor = SIZE(1)/PARAM(1)
	  END IF
	  OUT1(1,J,K) = factor*EFF*FEED(1,J,K)
	ELSE
	  OUT1(1,J,K) = 0.0
	END IF
        OUT2(1,J,K) = FEED(1,J,K) - OUT1(1,J,K)
        TMS1 = TMS1 + OUT1(1,J,K)

	DO I=2,NDC-1
	  OUT1(I,J,K)=0.0
	  DI = MeshSizes(I)
	  DIminus1 = MeshSizes(I-1)
	  IF(MeshSizes(I).LE.PARAM(1).AND.MeshSizes(I-1).GT.PARAM(1)) THEN
	    factor = ALOG(PARAM(1)/DI)/ALOG(DIminus1/DI)
	    OUT1(I,J,K) = factor*EFF*FEED(I,J,K)
	  ELSE IF (MeshSizes(I-1) .LE. PARAM(1)) THEN
	    OUT1(I,J,K)=EFF*FEED(I,J,K)
	  END IF
	  OUT2(I,J,K)=FEED(I,J,K)-OUT1(I,J,K)
	  TMS1=TMS1+OUT1(I,J,K)
	END DO
	IF(PARAM(1) .LT. MeshSizes(NDC-1)) THEN
	  factor = PARAM(1)/MeshSizes(NDC-1)
	ELSE
	  factor = 1.0
	end if
	OUT1(NDC,J,K) = factor*EFF*FEED(NDC,J,K)
	OUT2(NDC,J,K)=FEED(NDC,J,K)-OUT1(NDC,J,K)
	TMS1=TMS1+OUT1(NDC,J,K)
   20 CONTINUE
      TMS2 = TMSF - TMS1
      WTR2 = SurfaceWater*TMS2/(1.0 - SurfaceWater)
      WTR1 = WTR - WTR2
      WTR3 = 0.0


c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF
      RETURN
      END
C
      SUBROUTINE CSCR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  Simple model based on the traditional capacity factors.
C
C     PARAMETERS IN ORDER
C     *******************
C     1....MESH SIZE OF SCREEN
C     2....Open area
C     3....Angle of inclination
C     4....Aperture shape 1=Round, 2=Square, 3=2:1 Rectangle,4=3:1 rectangle, 5 = 4:1 recangle
C     5....Surface condition of material. 1=Sticky, 2=Surface wet, 3=Dry crushed, 4=Artificially dried
C     6....Surface water on screen oversize
C     7....SCREEN LENGTH
C     8....SCREEN WIDTH
C     9....Number of screens in parallel
C
      EXTERNAL MCSCR

C
C
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MCSCR,PARAM(9))
      RETURN
      END
C
C
      SUBROUTINE MCSCR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,G RDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
	USE GLOBALS
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

	REAL K1,K2,K3,K4,K5,K6,K7,K8,K9,K10
	REAL h,IU

	REAL, ALLOCATABLE::CUM(:)
	REAL, ALLOCATABLE::CUMSIZ(:)
	REAL, ALLOCATABLE::DENS(:)

	IF (.NOT. ALLOCATED(CUM)) Allocate(CUM(NDC)) 
	IF (.NOT. ALLOCATED(CUMSIZ)) Allocate(CUMSIZ(NDC)) 
	IF (.NOT. ALLOCATED(DENS)) Allocate(DENS(NDC)) 

      TMS1=0.0
      TMS2=0.0
	h = PARAM(1)
	!Calculate the capacity factors
	IF(h .GT. 0.025) Then
	  IU = 0.783*h*1000 +37
      Else
	  IU = 20.0*(1000*h)**0.33 - 1.28
      End IF
	IU = IU/3.6
	K1 = PARAM(2)/50
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
	BulkDensity = 0.6*SGA
	IF (BulkDensity .LT. 800) K1 = PARAM(2)/60
	Call PARTSZ(SIZE,FEED,NDC,NGC,NSC,CUMSIZ,CUM,DENS)
	PSIZE = 0.5*h
	Call FRPASS(CUMSIZ,CUM,NDC,P,PSIZE)
	K2 = 2*P + 0.2
	PSIZE = h
	Call FRPASS(CUMSIZ,CUM,NDC,P,PSIZE)
	K3 = 0.914*EXP(EXP(4.22*(1-P) - 3.50))
	K4 = 1000*BulkDensity/1600
	K5 = 1.0
	K6 = 1.0 - 0.01*(PARAM(3) - 15)
	IF(UNITWFEED) Then
	  IF (h .LT. 0.025) then
	    K7 = 1.0 + 2.4e-4*(25 - 1000*h)
	  Else
	    K7 = 1.0
	  End If
      ELSE
	  K7 = 1.0
	END IF
	K = NINT(PARAM(4))
	SELECT CASE (K)
		Case (1)
	    K8 = 0.8
		Case (2)
	    K8 = 1.0
		Case (3)
			K8 = 1.15
		Case (4)
			K8 = 1.2
		Case (5)
			K8 = 1.25
	END SELECT
	K9 = 1.0
	K = NINT(PARAM(5))
	SELECT CASE (K)
		Case (1)
	    K10 = 0.75
		Case (2)
	    K10 = 0.85
		Case (3)
			K10 = 1.0
		Case (4)
			K10 = 1.25
	END SELECT
      !Write(UNitDiagFile,*)' Factors', K1,K2,K3,K4,K5,K6,K7,K8,K9,K10
	!Calculate screen efficiency
	Area = PARAM(7)*PARAM(8)
	RR = TMSF/(IU*K1*K2*K3*K4*K5*K6*K7*K8*K9*K10*Area)
	IF(RR .GT. 0.8) Then
	  EFF = 0.95 - 0.25*(RR-0.8) - 0.05*(RR-0.8)**2
	Else
	  EFF = 0.95 -1.67*(0.8-RR)**2
	END IF
	!Write(UnitDiagFile,*)' Area,IU,RR,EFF',Area,Iu,RR,EFF

      SurfaceWater = 0.01 * PARAM(6)
      DO 20 J=1,NGC
      DO 20 K=1,NSC
	IF (MeshSizes(1) .LT. PARAM(1)) THEN
	  IF(SIZE(1) .LT. PARAM(1)) THEN
	    factor = 1.0
	  ELSE
	    factor = SIZE(1)/PARAM(1)
	  END IF
	  OUT1(1,J,K) = factor*EFF*FEED(1,J,K)
	ELSE
	  OUT1(1,J,K) = 0.0
	END IF
        OUT2(1,J,K) = FEED(1,J,K) - OUT1(1,J,K)
        TMS1 = TMS1 + OUT1(1,J,K)

	DO I=2,NDC-1
	  OUT1(I,J,K)=0.0
	  DI = MeshSizes(I)
	  DIminus1 = MeshSizes(I-1)
	  IF(MeshSizes(I).LE.PARAM(1).AND.MeshSizes(I-1).GT.PARAM(1)) THEN
	    factor = ALOG(PARAM(1)/DI)/ALOG(DIminus1/DI)
	    OUT1(I,J,K) = factor*EFF*FEED(I,J,K)
	  ELSE IF (MeshSizes(I-1) .LE. PARAM(1)) THEN
	    OUT1(I,J,K)=EFF*FEED(I,J,K)
	  END IF
	  OUT2(I,J,K)=FEED(I,J,K)-OUT1(I,J,K)
	  TMS1=TMS1+OUT1(I,J,K)
	END DO
	IF(PARAM(1) .LT. MeshSizes(NDC-1)) THEN
	  factor = PARAM(1)/MeshSizes(NDC-1)
	ELSE
	  factor = 1.0
	end if
	OUT1(NDC,J,K) = factor*EFF*FEED(NDC,J,K)
	OUT2(NDC,J,K)=FEED(NDC,J,K)-OUT1(NDC,J,K)
	TMS1=TMS1+OUT1(NDC,J,K)
   20 CONTINUE
      TMS2 = TMSF - TMS1
      WTR2 = SurfaceWater*TMS2/(1.0 - SurfaceWater)

      WTR1 = WTR - WTR2
      WTR3 = 0.0


c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF

	IF(ALLOCATED(CUM)) DEALLOCATE(CUM)
	IF(ALLOCATED(CUM)) DEALLOCATE(CUM)
	IF(ALLOCATED(CUM)) DEALLOCATE(CUM)
      RETURN
      END
C
      SUBROUTINE SCR1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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
C  MODEL FOR WET SCREENING PROPOSED BY RSC ROGERS
C  REF: POWDER TECHNOLOGY 31(1982) 135-137
C
C  PARAMETERS IN ORDER
C  *******************
C  1...D50 SIZE IN METERS
C  2... WATER SPLIT TO UNDERSIZE
C  3... EFFICIENCY PARAMETER ALPHA
C
      LU=8
      D50=PARAM(1)
      A=1.0-PARAM(2)
      ALPHA=PARAM(3)
      TMS1=0.0
      DO 10 I=1,NDC
      WS=SIZE(I)/D50
      WS1=ALPHA*(1.0-WS**3)
      C=1.0
      IF(ABS(WS1).LT.10.0) C=1.0/(1.0+(EXP(WS1))/WS)
      S=(1.0-A)*C + A
      DO 10 J=1,NGC
      DO 10 K=1,NSC
      OUT2(I,J,K)=FEED(I,J,K)*S
      OUT1(I,J,K)=FEED(I,J,K)-OUT2(I,J,K)
      TMS1=TMS1+OUT1(I,J,K)
      DER1(I,J,K)=1.0-S
      DER2(I,J,K)=S
   10 CONTINUE
      TMS2=TMSF-TMS1
      WTR2=A*WTR
      WTR1=WTR-WTR2
      WTR3 = 0.0

c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF

      FL=1
      RETURN
      END


      SUBROUTINE DSC1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  SIMPLE IDEAL MODEL FOR DOUBLE DECK SCREEN.
C
C     PARAMETERS IN ORDER
C     *******************
C     1....MESH SIZE OF SCREEN ON TOP DECK.
C     2....SCREEN EFFICIENCY FOR TOP DECK.
c     3....Surface water on top deck oversize
C     4....MESH SIZE OF SCREEN ON LOWER DECK.
C     5....SCREEN EFFIENCY FOR LOWER DECK.
c     6....Surface water on lower deck oversize
C     7....SCREEN DIMENSIONS?
C     8....SCREEN LENGTH-TOP DECK
C     9....SCREEN LENGTH LOWER DECK
C     10...SCREEN WIDTH
c     11...Number of screens in parallel
C

      EXTERNAL MDSC1
C
C  SPLIT THE FEED FOR MULTIPLE SCREENS IN PARALLEL AND RECOMBINE THE 
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MDSC1,PARAM(11))
      RETURN
      END
C
C
      SUBROUTINE MDSC1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
	USE GLOBALS
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
	REAL, ALLOCATABLE :: IFEED(:,:,:)

      IF(.NOT. ALLOCATED(IFEED)) ALLOCATE(IFEED(NDC,NGC,NSC))

      TMSI=0.0
      TMS2=0.0
      EFF=PARAM(2)
      SurfaceWater = 0.01 * PARAM(3)
      DO 20 J=1,NGC
      DO 20 K=1,NSC
	IF (MeshSizes(1) .LT. PARAM(1)) THEN
	  IF(SIZE(1) .LT. PARAM(1)) THEN
	    factor = 1.0
	  ELSE
	    factor = SIZE(1)/PARAM(1)
	  END IF
	  IFEED(1,J,K) = factor*EFF*FEED(1,J,K)
	ELSE
	  IFEED(1,J,K) = 0.0
	END IF
        OUT2(1,J,K) = FEED(1,J,K) - IFEED(1,J,K)
	TMSI=TMSI+IFEED(1,J,K)

	DO I=2,NDC-1
	  IFEED(I,J,K)=0.0
	  DI = MeshSizes(I)
	  DIminus1 = MeshSizes(I-1)
	  IF(MeshSizes(I).LE.PARAM(1).AND.MeshSizes(I-1).GT.PARAM(1)) THEN
	    factor = ALOG(PARAM(1)/DI)/ALOG(DIminus1/DI)
	    IFEED(I,J,K) = factor*EFF*FEED(I,J,K)
	  ELSE IF (MeshSizes(I-1) .LE. PARAM(1)) THEN
	    IFEED(I,J,K)=EFF*FEED(I,J,K)
	  END IF
	  OUT2(I,J,K)=FEED(I,J,K)-IFEED(I,J,K)
	  TMSI=TMSI+IFEED(I,J,K)
	END DO
	IF(PARAM(1) .LT. MeshSizes(NDC-1)) THEN
	  factor = PARAM(1)/MeshSizes(NDC-1)
	ELSE
	  factor = 1.0
	end if
	IFEED(NDC,J,K) = factor*EFF*FEED(NDC,J,K)
	OUT2(NDC,J,K)=FEED(NDC,J,K)-IFEED(NDC,J,K)
	TMSI=TMSI+IFEED(NDC,J,K)
   20 CONTINUE
      TMS2 = TMSF - TMSI
      WTR2 = SurfaceWater*TMS2/(1.0 - SurfaceWater)
      WTRI = WTR - WTR2


      TMS1=0.0
      TMS3=0.0
      EFF=PARAM(5)
      SurfaceWater = 0.01 * PARAM(6)
      DO 120 J=1,NGC
      DO 120 K=1,NSC
	IF (MeshSizes(1) .LT. PARAM(4)) THEN
	  IF(SIZE(1) .LT. PARAM(4)) THEN
	    factor = 1.0
	  ELSE
	    factor = SIZE(1)/PARAM(4)
	  END IF
	  OUT1(1,J,K) = factor*EFF*IFEED(1,J,K)
	ELSE
	  OUT1(1,J,K) = 0.0
	END IF
        OUT3(1,J,K) = IFEED(1,J,K) - OUT1(1,J,K)
	TMS1 = TMS1 + OUT1(1,J,K)

	DO I=2,NDC-1
	  OUT1(I,J,K)=0.0
	  DI = MeshSizes(I)
	  DIminus1 = MeshSizes(I-1)
	  IF(MeshSizes(I).LE.PARAM(4).AND.MeshSizes(I-1).GT.PARAM(4)) THEN
	    factor = ALOG(PARAM(4)/DI)/ALOG(DIminus1/DI)
	    OUT1(I,J,K) = factor*EFF*IFEED(I,J,K)
	  ELSE IF (MeshSizes(I-1) .LE. PARAM(4)) THEN
	    OUT1(I,J,K)=EFF*IFEED(I,J,K)
	  END IF
	  OUT3(I,J,K)=IFEED(I,J,K)-OUT1(I,J,K)
	  TMS1=TMS1+OUT1(I,J,K)
	END DO
	IF(PARAM(4) .LT. MeshSizes(NDC-1)) THEN
	  factor = PARAM(4)/MeshSizes(NDC-1)
	ELSE
	  factor = 1.0
	end if
	OUT1(NDC,J,K) = factor*EFF*IFEED(NDC,J,K)
	OUT3(NDC,J,K)=IFEED(NDC,J,K)-OUT1(NDC,J,K)
	TMS1=TMS1+OUT1(NDC,J,K)
  120 CONTINUE
      TMS3 = TMSI - TMS1
      WTR3 = SurfaceWater*TMS3/(1.0 - SurfaceWater)
      WTR1 = WTRI - WTR3


c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF

      DEALLOCATE(IFEED)
      RETURN
      END


      SUBROUTINE DWSC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(4,4),GRDV(4,4)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C Dewatering screen using the model proposed by K L Ng. Dewatering performance
c vibrating screens. Proc. Instn Mech Engrs.  Part E Journal of Process
c Mechanical Engineering.  204(1990)pp73-79.
C Undersize solid is carried through the screen in proportion to the water flow.

C PARAMETERS:
C ***********
C     1....Ultimate moisture content of the material (%)
C     2....Mesh size of the screen
C     3....Length of the screen
c     4....Width of the screen
c     5....Angle of inclination (degrees)
c     6....Vibration frequency (rpm)
c     7....Amplitude of vibration
C     8....Angle of vibration relative to screen surface (degrees)

      REAL MESH, LENGTH
      PARAMETER (PI = 3.14159)

c Receive information about water FEED to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
      WN0 = PARAM(1)
      MESH = PARAM(2)
      LENGTH = PARAM(3)
      WD = PARAM(4)
      THETA = PARAM(5)*PI/180.0
      OMEGA = 2*PI*PARAM(6)/60.0
      AMPL = PARAM(7)
      XI = PI*PARAM(8)/180.0

C Calculate the water content of the discharge
      P = 23.4
      !Calculate water content of the feed
	WFEED = 100*WTR/(WTR + TMSF)
	IF(WFEED .LT. P + WN0) Then
	  P = WFEED - WN0
      END IF
	Q = 0.33
      IF(MESH .GT. 0.001) Q = 0.33 + 80.9*(MESH - 0.001)
      T = LENGTH/TRANSVEL(THETA,OMEGA,AMPL,XI)
      WN = WN0 + P*(T)**(-Q)

C Get the water flow in the discharge
      WTR2 = TMSF*WN/(100.0 - WN)
      IF(WTR2 .GT. WTR) WTR2 = WTR
      WTR1 = WTR - WTR2
      TMS1 = 0.0
      DO 30 I = 1,NDC
        DO 20 J = 1,NGC
          DO 10 K = 1,NSC
            OUT1(I,J,K) = 0.0
            IF(SIZE(I). LT. MESH) OUT1(I,J,K) = FEED(I,J,K)*WTR1/WTR
            OUT2(I,J,K) = FEED(I,J,K) - OUT1(I,J,K)
            TMS1 = TMS1 + OUT1(I,J,K)
	      TMS2 = TMS2 + OUT2(I,J,K)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
C Correct the water flow in the discharge
      WTR2 = TMS2*WN/(100.0 - WN)
      IF(WTR2 .GT. WTR) WTR2 = WTR
      WTR1 = WTR - WTR2
      WTR3 = 0.0
      RETURN
      END

      FUNCTION TRANSVEL(THETA,OMEGA,AMPL,Xi)
C     **************************************
C
C Function to calculate the transport velocity of granular material on
c a vibrating conveyor.
c The transport velocity is calculated using K L Ng, L A Ang and S C Chng. A
c computer model for vibrating conveyor. Proc Instn Mech Engrs
c Vol 200 no.B2 (1986) pp 123-130. This model is reduced to an empirical
c exponential model which should be accurate for most purposes.
      PARAMETER (PI = 3.14159)


C      IF(Xi .GE. PI/2.0) Xi = PI/2.01
C Calculate the velocity of travel
c ********************************
      COST = COS(THETA)
      Ay = AMPL*SIN(Xi)
      GY = 9.81*COST/(OMEGA*OMEGA*Ay)

C This is the empirical model for velocity
      SLOPE = 1.44 - 2.52*THETA
      XDOT = 1.87*EXP(-SLOPE*GY)
      Ax = AMPL*cos(Xi)
      TRANSVEL = XDOT*OMEGA*Ax
      RETURN
      END

      SUBROUTINE PSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(9),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      EXTERNAL MPSCN

c Receive information about water FEED to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  Model for the probability screen.
C  Ref:Chen Rongguang, J M Beekmans and Chen Qingru, "A convenient
C  correlation for modelling the performace of probability screens"
C  Intl. Jnl. Mineral Processing  36(1992) 31-40
C
c  Parameters
C  1..Amplitude of vibration.
C  2..Vibration frequency.
C  3..Angle of inclination of the screen - degrees
C  4..Screen vibration throw angle.
C  5..Screen aperture size.
C  6..Screen width.
C  7..Screen length.
c  8..Surface water on screen oversize.
C  9..Number of screens in parallel.
C
C  Split the FEED for multiple units in parallel and recombine the
C  products before exit.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MPSCN,PARAM(9))

c  Return to MODSIM.
      RETURN
      END
C
C
      SUBROUTINE MPSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      PARAMETER (PI = 3.14159)
      REAL Kv,K1,K2,length
C

c Receive information about water FEED to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      A = PARAM(1)
      f = PARAM(2)
      alpha = PI*PARAM(3)/180.0
      beta = PI*PARAM(4)/180.0
      Xa = PARAM(5)
      W = PARAM(6)
      length = PARAM(7)
      SurfaceWater = 0.01*PARAM(8)
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      Fr = TMSF/(W*SGA*1000)
      WS1 = Fr/Xa**1.5
      WS2 = SQRT(WS1)
      WS3 = SQRT(Fr/(Xa*COS(alpha)))
      Kv = (A*(2*PI*F)**2*SIN(beta))/(9.81*COS(alpha))
      K1 = 6.7114 - 254.82*Fr - 176.598*Xa*alpha - 0.1636*f*alpha
      K1 = K1 - 1036.44*A*alpha - 3.2884*WS2 + 14.735*WS3
      K2 = 8.4326 + 0.07306*Kv - 535.354*Xa*alpha -4828.0*Xa*Fr
      K2 = K2 + 0.09497*WS1 - 4.4453*WS2 + 13.512*WS3
      TMS1 = 0.0
      TMS2 = 0.0
      DO I = 1,NDC
        IF(SIZE(I) .LT. Xa) THEN
          B = EXP(K1)*(1.0 - SIZE(I)/Xa)**K2
        ELSE
          B = 0.0
        END IF
        WS = 1.0 - EXP(-B*length)

        DO J = 1,NGC
          DO K = 1,NSC
            OUT1(I,J,K) = FEED(I,J,K)*WS
            TMS1 = TMS1 + OUT1(I,J,K)
            OUT2(I,J,K) = FEED(I,J,K) - OUT1(I,J,K)
          END DO
        END DO
      END DO
      TMS2 = TMSF - TMS1
      WTR2 = SurfaceWater*TMS2/(1.0 - SurfaceWater)
      WTR1 = WTR - WTR2
      WTR3 = 0.0

c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF
      FL = 0
C Return to the model subroutine.
      RETURN
      END


      SUBROUTINE KSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(10),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      EXTERNAL MKSCN

c Receive information about water FEED to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  Model for the vibrating screen using the simple kinetic model.
C  Ref:King R P . Modeling and Simulation of Mineral Processing Systems. Section 4.3
C
c  Parameters
C  1..Kinetic constant for crowded region  k0/u.
C  2..Kinetic constant for separated region  s50.
C  3..Power exponent for separated region.  sigma
C  4..Screen aperture size in mm.
C  5..Screen width.
C  6..Screen length.
c  7..Surface water on screen oversize.
c  8..Transmission velocity down the screen.
c  9..Adhesion constant for fines
C  10..Number of screens in parallel.
C
C  Split the FEED for multiple units in parallel and recombine the
C  products before exit.

      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MKSCN,PARAM(10))

c  Return to MODSIM.
      RETURN
      END
C
C
      SUBROUTINE MKSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)

      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

	Real MeshSize,Lc,k0
C

c Receive information about water FEED to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

	Real, Allocatable :: CUMSIZE(:)
	Real, Allocatable :: CUM(:),DENS(:)

	IF (.NOT. Allocated(CUMSIZE)) Allocate(CUMSIZE(NDC))
	IF (.NOT. Allocated(CUM)) Allocate(CUM(NDC))
	IF (.NOT. Allocated(DENS)) Allocate(DENS(NDC))

      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,CUMSIZE,CUM,DENS)
	CALL PASSSZ(CUMSIZE,CUM,NDC,0.8,d80F)

      k0 = PARAM(1)
	s0 = PARAM(2)
	sigma = PARAM(3)
	MeshSize = 0.001*PARAM(4)
	Width = PARAM(5)
	ScreenLength = PARAM(6)
      SurfaceWater = 0.01*PARAM(7)
	Velocity = PARAM(8)
	Attach = PARAM(9)
	CALL KineticSreen(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,NDC,NGC,NSC,
     *SIZE,PPROP,k0,s0,sigma,MeshSize,ScreenLength,Width,Velocity,
     *Attach,Lc,d80F)
      OUT3 = 0.0
	TMS3 = 0.0
	WTR3 = 0.0
      WTR2 = SurfaceWater*TMS2/(1.0 - SurfaceWater)
      WTR1 = WTR - WTR2

c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF
      FL = 0

	IF ( Allocated(CUMSIZE)) DeAllocate(CUMSIZE)
	IF ( Allocated(CUM)) DeAllocate(CUM)
	IF ( Allocated(DENS)) DeAllocate(DENS)

C Return to the model subroutine.
      RETURN
      END

	SUBROUTINE KineticSreen(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,NDC,NGC,NSC,
     *  SIZE,PPROP,k0,s0,sigma,MeshSize,ScreenLength,Width,Velocity,
     *  Attach,Lc,d80F)
      !*****************************************************************
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      REAL koveru,soveru,k0,s0
	Real Lc,MeshSize, ScreenLength,Width

	Real, Allocatable :: OUT2Lc(:,:,:)
	Real, Allocatable :: AttachFr(:)
	IF (.NOT. Allocated(OUT2Lc)) Allocate(OUT2Lc(NDC,NGC,NSC))
	IF (.NOT. Allocated(AttachFr)) Allocate(AttachFr(NDC))


	OUT2 = FEED
	OUT2Lc = Feed
	OUT1 = 0.0
	TMS2 = TMSF
	Lc = 0.0

c     Calculate the transition from crowded to separated.
  100	  CALL SGM(OUT2,NDC,NGC,NSC,PPROP,SVM,SGA)
        BedSV = SVM/0.6
	  BedDepth = TMS2*BedSV/(Width*Velocity)
     	  Lc = Lc + 0.02
	  TMS2 = 0.0
c      write(13,*)' Lc = ',Lc

  	  Do I = 1,NDC
	    IF(Size(I) .LT. MeshSize) Then
	      koveru = k0*(1-SIZE(I)/MeshSize)/Velocity
	      If(koveru .GT. 1/Lc) koveru = 1/Lc
	      AttachFr(I) = Attach*(1 - SIZE(I)/MeshSize)
          ELSE
            koveru = 0.0
	      AttachFr(I) = 0.0 
          END IF
	    Do J = 1,NGC
	      Do K = 1,NSC
	        OUT2(I,J,K) = Feed(I,J,K)*(1-koveru*Lc) + AttachFr(I)*TMS2
	        If(OUT2(I,J,K) .GT. FEED(I,J,K)) OUT2(I,J,K) = FEED(I,J,K)
              TMS2 = TMS2 + OUT2(I,J,K)
            End Do
	    End do
        End do
	  If (BedDepth .GT. 2*d80F) Then
	    If (Lc .GE. ScreenLength) Go To 200
          Go To 100
        End if
     
c Calculate the separated region.
      TMS2 = 0.0
	OUT2Lc = OUT2
	Do I = 1,NDC
	  IF(Size(I) .LT. MeshSize) Then
	    soveru = s0*((2*(1 - SIZE(I)/MeshSize))**sigma)/Velocity
        ELSE
	    soveru = 0.0
        END IF
	  Do J = 1,NGC
	    Do K = 1,NSC
            OUT2(I,J,K) = OUT2Lc(I,J,K)*exp(-soveru*(ScreenLength-Lc))+
     *      AttachFr(I)*TMS2
	      TMS2 = TMS2 + OUT2(I,J,K)
	    END do
	  END Do
	END Do
  200 TMS1 = TMSF - TMS2
	OUT1 = Feed - OUT2


	IF ( Allocated(OUT2Lc)) DeAllocate(OUT2Lc)
	IF ( Allocated(AttachFr)) DeAllocate(AttachFr)

      RETURN
	END

      SUBROUTINE CSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(12),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      EXTERNAL MCSCN

c Receive information about water FEED to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  Model for the vibrating screen using the simple kinetic model.
C  Ref:King R P . Modeling and Simulation of Mineral Processing Systems. Section 4.3
C      This model allows for compound screens
C
c  Parameters
C  1..Kinetic constant for crowded region  k0/u.
C  2..Kinetic constant for separated region  s50.
C  3..Power exponent for separated region.  sigma
C  4..Screen width.
c  5..Surface water on screen oversize.
c  6..Transmission velocity down the screen.
c  7..Mesh size on first section.
c  8..Length of first section.
c  9..Mesh size on second section.
c 10..Length of second section.
c 11..Attachment factor for fines
C 12..Number of screens in parallel.

C
C  Split the FEED for multiple units in parallel and recombine the
C  products before exit.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MCSCN,PARAM(12))

c  Return to MODSIM.
      RETURN
      END
C
C
      SUBROUTINE MCSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)

      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      REAL koveru,soveru,length,k0
	Real Lc,MeshSize, ScreenLength
      Real MeshSize1, ScreenLength1, MeshSize2, ScreenLength2
	Real, Allocatable :: CUMSIZE(:)
	Real, Allocatable :: CUM(:),DENS(:)
C

c Receive information about water FEED to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

	IF (.NOT. Allocated(CUMSIZE)) Allocate(CUMSIZE(NDC))
	IF (.NOT. Allocated(CUM)) Allocate(CUM(NDC))
	IF (.NOT. Allocated(DENS)) Allocate(DENS(NDC))

      k0 = PARAM(1)
	s0 = PARAM(2)
	sigma = PARAM(3)
	Width = PARAM(4)
      SurfaceWater = 0.01*PARAM(5)
	Velocity = PARAM(6)
	MeshSize1 = 0.001*PARAM(7)
	ScreenLength1 = PARAM(8)
	MeshSize2 = 0.001*PARAM(9)
	ScreenLength2 = PARAM(10)
	Attach = PARAM(11)

c Calculate the transition from crowded to separated.
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,CUMSIZE,CUM,DENS)
	CALL PASSSZ(CUMSIZE,CUM,NDC,0.8,d80F)
	!Calculate bed depth at feed end

      TMS1 = 0.0
	TMS2 = 0.0
	TMS3 = 0.0
	OUT1 = 0.0
	OUT2 = 0.0
	OUT3 = 0.0

      Feedrate = TMSF*3.6/(Width)
      WRITE(13,1002)Feedrate,TMSF
 1002 FORMAT(/'Load on screen',F10.3,' t/h m',G12.4)

	CALL KineticSreen(TMSF,TMS1,TMS2,FEED,OUT1,DER2,NDC,NGC,NSC,
     *SIZE,PPROP,k0,s0,sigma,MeshSize1,ScreenLength1,Width,Velocity,
     *Attach,Lc,d80F)
	WS = TMS2
c	write(13,*)'Lc =',Lc

	CALL KineticSreen(WS,TMS3,TMS2,DER2,OUT3,OUT2,NDC,NGC,NSC,
     *SIZE,PPROP,k0,s0,sigma,MeshSize2,ScreenLength2,Width,Velocity,
     *Attach,Lc,d80F)
c	write(13,*)'Lc =',Lc

	!Calculate bed depth at feed end
	CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
	BedDepth = TMSF*SVM/(Width*Velocity)
	Write(13,1003) 1000*BedDepth,1000*2*d80F
 1003 FORMAT(/'Bed depth at feed end ',F 7.2,' mm'/
     *'Bed depth at transition from loaded to separated regions ',
     *F7.2,' mm')

      WTR2 = SurfaceWater*TMS2/(1.0 - SurfaceWater)

c Calculate the water flows if water is added to the screen.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = TMS1*(WATERADD + WTR - WTR2)/(TMS1 + TMS3)
          WTR3 = TMS3*(WATERADD + WTR - WTR2)/(TMS1 + TMS3)
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
	    WTR3 = TMS3*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 + WTR3 - WTR
        END IF
	ELSE
	  WATERADD = 0.0
      END IF
      FL = 0

C Return to the model subroutine.
	IF ( Allocated(CUMSIZE)) DeAllocate(CUMSIZE)
	IF ( Allocated(CUM)) DeAllocate(CUM)
	IF ( Allocated(DENS)) DeAllocate(DENS)
      RETURN
      END
