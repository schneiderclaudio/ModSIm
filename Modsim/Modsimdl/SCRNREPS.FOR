C*******************     SCRNREPS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C This file contains the report writers for the models in file SCRNMODS
C                                                                      C
C NOTE: The report writing subroutines have the same names as their    C
C       corresponding models.                                          C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
      SUBROUTINE RDSC2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C*
C*  REPORT WRITE FOR DOUBLE DECK SCREEN USING THE V.K. KARRA PROCEDURE
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
C      12..... Surface water on top deck
C      13..... Surface water on lower deck
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

      EXTERNAL RMDSC2
C
C  SPLIT THE FEED FOR MULTIPLE SCREENS IN PARALLEL AND RECOMBINE THE 
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMDSC2,PARAM(11))
      RETURN
      END
C
C
      SUBROUTINE RMDSC2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      EXTERNAL D501,PF2
      COMMON /MODELDAT/NUNIT
C
C   TOP DECK CALCULATIONS
C   *********************
C
      LU = 8
      THETA=PARAM(5)
      A11=PARAM(6)
      A12=PARAM(7)
      AREA=A11*A12
      U=PARAM(8)
      JT=PARAM(9)
      SurfaceWater = 100*WTR2/(TMS2+WTR2)
      SCOS=0.0
      RW=0.0
      S=1.
      H1=PARAM(1)
      DIA1=PARAM(3)
      CALL KARRA(S,H1,DIA1,THETA,AREA,U,TMSF,FEED,SIZE,DER1,DER2,DER3,N
     *DC,NGC,NSC,WTR,HT,THEO,Q,R,XN,A,B,C,D,E,F,W,G,VEL)
      ALPHA=0.0
      CALL CLASSI(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,
     *WTR,WTR1,WTR2,RW,SIZE,D501,SCOS,PF2,PPROP,LU)

      CALL HEADER (LU,ITERM,NUNIT,'DOUBLE DECK SCREEN',18,'DSC2')
      IPAR=PARAM(11)

      WRITE(LU,35)IPAR
   35 FORMAT(//' This unit represents ',I3,' screens in parallel')
      WRITE (LU,36)
   36 FORMAT (//15X,'TOP DECK')
c Report the performance of the upper deck.
      CALL RFSC2(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,DER3,NDC,NGC,NS
     *C,WTR,WTR1,WTR2,SIZE,S,THETA,A11,A12,H1,DIA1,U,A,B,C,D,E,F,W,HT,
     *Q,R,XN,SurfaceWater,LU)
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
      WTRF=WTR
      SurfaceWater = 100*WTR3/(TMS3 + WTR3)

c Set up the Karra parameters for the lower deck.
      CALL KARRA(S,H2,DIA2,THETA,AREA,U,TMS1,OUT1,SIZE,DER1,DER2,OUT3,N
     *DC,NGC,NSC,WTR,HT,THEO,Q,R,XN,A,B,C,D,E,F,W,G,VEL)
c Classify the feed to the lower deck. Put undersize into OUT2
      CALL CLASSI(TMS,TMS1,TMS3,OUT1,OUT2,OUT3,DER1,DER3,NDC,NGC,NSC,
     *WTRF,WTR1,WTR3,RW,SIZE,D501,SCOS,PF2,PPROP,LU)

      WRITE (LU,37)
   37 FORMAT (//15X,'LOWER DECK')
c Report performance of lower deck.
      CALL RFSC2(TMS,TMS1,TMS3,OUT1,OUT2,OUT3,DER1,DER2,DER3,NDC,NGC,NS
     *C,WTRF,WTR1,WTR2,SIZE,S,THETA,A11,A12,H2,DIA2,U,A,B,C,D,E,F,W,HT,
     *Q,R,XN,SurfaceWater,LU)
      RETURN
      END
C
C
      SUBROUTINE RSCR2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C***********************************************************************
C*                                                                     *
C*  REPORT WRITER FOR SINGLE DECK SCREEN USING THE V.K. KARRA MODEL    *
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
C       6..... BULK DENSITY OF THE MINERAL (Kg/m3)
C       7..... SCREEN MATERIAL TYPE
c       8..... Surface water on oversize
C       9..... NUMBER OF SCREENS IN PARALLEL.
C
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT
      EXTERNAL RMSCR2
C
C
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMSCR2,PARAM(9))
      RETURN
      END
C
C
      SUBROUTINE RMSCR2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,G RDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL

      COMMON /MODELDAT/NUNIT
C
      EXTERNAL D501,PF2
C
      THETA=PARAM(3)
      A11=PARAM(4)
      A12=PARAM(5)
      U=PARAM(6)
      JT=PARAM(7)
      SurfaceWater = 100*WTR2/(TMS2+WTR2)
      AREA=A11*A12
      SCOS=0.0
      RW=0.0
      S=1.
      H=PARAM(1)
      LU=8
      ITERM=5
      DIA=PARAM(2)
      CALL HEADER(LU,ITERM,NUNIT,'SINGLE DECK SCREEN',18,
     *'SCR2')
      CALL KARRA(S,H,DIA,THETA,AREA,U,TMSF,FEED,SIZE,DER1,DER2,DER3,
     *NDC,NGC,NSC,WTR,HT,THEO,Q,R,XN,A,B,C,D,E,F,W,G,VEL)
      CALL CLASSI(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,
     *WTR,WTR1,WTR2,RW,SIZE,D501,SCOS,PF2,PPROP,LU)
C
C   WRITE REPORT
C   ************
C
      IPARAL=NINT(PARAM(9))
      IF(IPARAL.GT.1) WRITE(LU,1005) IPARAL
 1005 FORMAT(/' There are ',I3,' screens in parallel.')
      CALL RFSC2(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,DER3,NDC,NGC,NS
     *C,WTR,WTR1,WTR2,SIZE,S,THETA,A11,A12,H,DIA,U,A,B,C,D,E,F,W,HT,
     *Q,R,XN,Surfacewater,LU)
      FL=0

	!Write data to plot the classification function
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'SCR2',
     *FEED,OUT2,'Vibrating screen',16)

      RETURN
      END

      SUBROUTINE RSCRN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      LOGICAL DIMS

      COMMON /MODELDAT/NUNIT
C
C  SIMPLE IDEAL MODEL FOR SCREENING
C
C     PARAMETERS IN ORDER
C     *******************
C     1....MESH SIZE OF SCREEN
C     2....SCREEN EFFICIENCY
c     3....Surface water on the screen oversize.
C     4....SPECIFY IF SCREEN DIMENSIONS ARE GIVEN.
C     5....SCREEN LENGTH
C     6....SCREEN WIDTH
C     7....number of screens in parallel
C
C WRITE THE DESIGN REPORT.
C ************************
      EXTERNAL RMSCRN
C
C
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMSCRN,PARAM(7))
      RETURN
      END
C
C
      SUBROUTINE RMSCRN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,G RDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      LOGICAL DIMS
      COMMON /MODELDAT/NUNIT

      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'VIBRATING SCREEN',16,'SCRN')
      Np = NINT(PARAM(7))
      WRITE(LU,'(/'' There is/are '',I2,'' screen/s in parallel'')')Np
      IF(PARAM(4) .LE. 0) THEN
        DIMS = .FALSE.
      ELSE
        DIMS = .TRUE.
      END IF

      CALL RFSC(1,TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,SIZE,PARAM(1),PPROP,INDPP,NPP,DIMS,
     *PARAM(5),PARAM(6),PARAM(3))
      RETURN
      END
C
      SUBROUTINE RSCR1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
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
C
C  WRITE THE REPORT FILE
        ITERM=0
        CALL HEADER(LU,ITERM,NUNIT,'WET SCREEN',10,'SCR1')
        WRITE(LU,1001) D50*1E6,PARAM(2),ALPHA
 1001   FORMAT(/
     *  ' PARAMETERS:'/
     *  '   D50 size               ',F8.1,' microns',/
     *  '   Water split to undersize ',F7.2,/
     *  '   Sharpness parameter alpha ',F6.3)
C
C  WRITE TO REPORT FILE
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1055)
 1055 FORMAT(/5X,'Particle size distribution in the feed.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL FRPASS(DER2,DER3,NDC,F50,CUT)
      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1080)
 1080 FORMAT(/5X,'Particle size distribution in the overflow.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1090)
 1090 FORMAT(/5X,'Particle size distribution in the underflow')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL NOTES(LU,5)
      RETURN
      END
C
      SUBROUTINE RCSCR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      !COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      Integer NUNIT
      COMMON /MODELDAT/NUNIT

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
C     7....SCREEN DIMENSIONS?
C     8....SCREEN LENGTH
C     9....SCREEN WIDTH
C     10....Number of screens in parallel
C
      EXTERNAL MRCSCR

C
C
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MRCSCR,PARAM(9))
      RETURN
      END
C
C
      SUBROUTINE MRCSCR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,G RDM,GRDV,NMIN,NGCM)
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

C  WRITE THE REPORT FILE
        LU = 8
        ITERM=0
        CALL HEADER(LU,ITERM,NUNIT,'VIBRATING SCREEN',16,'CSCR')

        WRITE(LU,1001) 100*PARAM(1),PARAM(2),PARAM(3),PARAM(6),PARAM(7),
     *  PARAM(8),NINT(PARAM(9))
 1001   FORMAT(/
     *  ' PARAMETERS:'/
     *  '   Mesh size ',T50,F8.1,' cm',/
     *  '   Open area ',T50,F8.1,'%',/
     *  '   Angle of inclination',T50,F8.1,' degrees',/
     *  '   Surface water on screen oversize',T50,F8.2,' %',/
     *  '   Length of screen',T50,F8.2,' m',/
     *  '   Width of screen',T50,F8.2,' m',/
     *  '   There is/are ',I2,' screen/s in parallel')
	Write(LU,*)

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
	BulkDensity = 0.6*SGA*1000
	IF (BulkDensity .LT. 800) K1 = PARAM(2)/60
	Call PARTSZ(SIZE,FEED,NDC,NGC,NSC,CUMSIZ,CUM,DENS)
	PSIZE = 0.5*h
	Call FRPASS(CUMSIZ,CUM,NDC,P,PSIZE)
	K2 = 2*P + 0.2
	PSIZE = h
	Call FRPASS(CUMSIZ,CUM,NDC,P,PSIZE)
	K3 = 0.914*EXP(EXP(4.22*(1-P) - 3.50))
	K4 = BulkDensity/1600
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
	    Write(LU,'(''Round mesh was specified'')')
		Case (2)
	    K8 = 1.0
	    Write(LU,'(''Square mesh was specified'')')
		Case (3)
			K8 = 1.15
	    Write(LU,'(''2:1 rectangular mesh was specified'')')
		Case (4)
			K8 = 1.2
	    Write(LU,'(''3:1 rectangular mesh was specified'')')
		Case (5)
			K8 = 1.25
	    Write(LU,'(''4:1 rectangular mesh was specified'')')
	END SELECT
	K9 = 1.0
	K = NINT(PARAM(5))
	SELECT CASE (K)
		Case (1)
	    Write(LU,'(''Material is specified as sticky'')')
	    K10 = 0.75
		Case (2)
	    K10 = 0.85
	    Write(LU,'(''Material is specified as surface wet'')')
		Case (3)
			K10 = 1.0
	    Write(LU,'(''Material is specified as dry crushed'')')
		Case (4)
			K10 = 1.25
	    Write(LU,'(''Material is specified as artificially dried'')')
	END SELECT

	!Calculate screen efficiency
	Area = PARAM(7)*PARAM(8)
	RR = TMSF/(IU*K1*K2*K3*K4*K5*K6*K7*K8*K9*K10*Area)
	IF(RR .GT. 0.8) Then
	  EFF = 0.95 - 0.25*(RR-0.8) - 0.05*(RR-0.8)**2
	Else
	  EFF = 0.95 -1.67*(0.8-RR)**2
	END IF

      Write(LU,'(/''Calculated quantities'')')
	Write(LU,1002)3.6*IU,K1,K2,K3,K4,K5,K6,K7,K8,K9,K10
 1002 FORMAT('  Capacity factors',/
     *'    Basic capacity Iu ',T50,F8.2,' t/h m^2',/
     *'    Open area factor K1',T50,F8.4,/
     *'    Half-size factor K2',T50,F8.4,/
     *'    Oversize factor K3',T50,F8.4,/
     *'    Bulk density factor K4',T50,F8.4,/
     *'    Deck position factor K5',T50,F8.4,/
     *'    Screen angle factor K6',T50,F8.4,/
     *'    Wet screening K7',T50,F8.4,/
     *'    Aperture shape factor K8',T50,F8.4,/
     *'    Particle shape factor K9',T50,F8.4,/
     *'    Surface condition factor K10',T50,F8.4,/
     *)

	Write(LU,1003)Area,RR,EFF
 1003 FORMAT('  Screen area',T50,F8.3,' m^2',/
     *'  Rating ratio',T50,F8.3,/
     *'  Screening efficiency',T50,F8.3,/
     *)
      CALL VELOC(1,PARAM(3),TMSF,PARAM(8),VEL)
	Depth = TMS2/(BulkDensity*VEL*PARAM(8))
      WRITE(LU,2004) VEL,100*Depth
 2004 FORMAT('  Transport velocity along screen from USBM model ',
     *T50,F8.3,' m/s',/
     *'  Bed depth at discharge end',T50,F8.2,' cm')

	IF(ALLOCATED(CUM)) DEALLOCATE(CUM)
	IF(ALLOCATED(CUM)) DEALLOCATE(CUM)
	IF(ALLOCATED(CUM)) DEALLOCATE(CUM)
      RETURN
      END


      SUBROUTINE RDSC1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      LOGICAL DIMS
      COMMON /MODELDAT/NUNIT
C
C  SIMPLE IDEAL MODEL FOR DOUBLE DECK SCREEN.
C
C     PARAMETERS IN ORDER
C     *******************
C     1....MESH SIZE OF SCREEN ON TOP DECK.
C     2....SCREEN EFFICIENCY FOR TOP DECK.
C     3....Surface water on top screen oversize.
C     4....MESH SIZE OF SCREEN ON LOWER DECK.
C     5....SCREEN EFFIENCY FOR LOWER DECK.
C     6....Surface water on lower screen oversize.
C     7....SPECIFIES WHETHER SCREEN DIMENSIONS ARE GIVEN.
C     8....LENGTH OF UPPER SCREEN
C     9....LENGTH OF LOWER SCREEN
C     10...WIDTH OF SCREEN
C     11...Number of screens in parallel
C
C  WRITE DESIGN REPORT.
      EXTERNAL RMDSC1
C
C  SPLIT THE FEED FOR MULTIPLE SCREENS IN PARALLEL AND RECOMBINE THE 
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMDSC1,PARAM(11))
      RETURN
      END
C
C
      SUBROUTINE RMDSC1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      LOGICAL DIMS
      COMMON /MODELDAT/NUNIT

      LU=8
      ITERM=0
      IF(PARAM(7) .LE. 0) THEN
        DIMS = .FALSE.
      ELSE
        DIMS = .TRUE.
      END IF
      CALL HEADER(LU,ITERM,NUNIT,'DOUBLE DECK SCREEN',18,'DSC1')
      Np = NINT(PARAM(11))
      WRITE(LU,'(/'' There is/are '',I2,'' screen/s in parallel'')')Np
      WRITE(LU,1010)
 1010 FORMAT(/15X,'TOP DECK')
      WTR1=WTR -WTR3
      TMS1=TMSF-TMS2
      DO 30 I=1,NDC
        DO 30 J=1,NGC
          DO 30 K=1,NSC
            OUT1(I,J,K)=FEED(I,J,K) -OUT2(I,J,K)
   30 CONTINUE
      CALL RFSC(1,TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,SIZE,PARAM(1),PPROP,INDPP,NPP,DIMS,
     *PARAM(8),PARAM(10),PARAM(3))
      WRITE(LU,1020)
 1020 FORMAT(//15X,'LOWER DECK')
      TMSF=TMS1
      WTR =WTR1
      TMS1=TMSF-TMS3
      WTR1=WTR -WTR3
      DO 40 I=1,NDC
      DO 40 J=1,NGC
      DO 40 K=1,NSC
      FEED(I,J,K)=OUT1(I,J,K)
      OUT1(I,J,K)=FEED(I,J,K) -OUT3(I,J,K)
   40 CONTINUE
      CALL RFSC(2,TMSF,TMS1,TMS2,FEED,OUT1,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR3,SIZE,PARAM(4),PPROP,INDPP,NPP,DIMS,
     *PARAM(9),PARAM(10),PARAM(6))
      RETURN
      END

      SUBROUTINE RDWSC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(4,4),GRDV(4,4)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
      CHARACTER*7 UNITS
      REAL MESH, LENGTH
      REAL LOAD
      LOGICAL SATISFY
      PARAMETER (PI = 3.14159)
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'Dewatering screen',17,'DWSC')
      WRITE(LU,1000)
 1000 FORMAT(/'Model Parameters:')
      WRITE(LU,1001) PARAM(1)
 1001 FORMAT(3X,'Ultimate moisture content of the material ',F6.2,'%')
      CALL SZSCAL(PARAM(2),UNITS,FACTR,IPWR)
      WRITE(LU,1002) PARAM(2)*FACTR,UNITS
 1002 FORMAT(3X,'Mesh size of the screen ',F6.2,1X,A7)
      WRITE(LU,1003) PARAM(3)
 1003 FORMAT(3X,'Length of the screen ',F7.3,' m')
      WRITE(LU,1004) PARAM(4)
 1004 FORMAT(3X,'Width of the screen ',F7.3,' m')
      WRITE(LU,1005) PARAM(5)
 1005 FORMAT(3X,'Angle of declination ',F5.1,' degrees')
      WRITE(LU,1006) PARAM(6)
 1006 FORMAT(3X,'Vibration frequency ',F6.1,' rpm')
      CALL SZSCAL(PARAM(7),UNITS,FACTR,IPWR)
      WRITE(LU,1007) PARAM(7)*FACTR,UNITS
 1007 FORMAT(3X,'Amplitude of vibration ',F6.1,A7)
      WRITE(LU,1008) PARAM(8)
 1008 FORMAT(3X,'Angle of vibration relative to screen surface ',F5.1,
     *' degrees')


      WN0 = PARAM(1)
      MESH = PARAM(2)
      LENGTH = PARAM(3)
      WD = PARAM(4)
      THETA = PARAM(5)*PI/180.0
      OMEGA = 2*PI*PARAM(6)/60.0
      AMPL = PARAM(7)
      XI = PI*PARAM(8)/180.0

      P = 23.4
      Q = 0.33 + 80.9*(MESH - 0.001)
      V = TRANSVEL(THETA,OMEGA,AMPL,XI)
      T = LENGTH/V
      WN = WN0 + P*(T)**(-Q)
      WRITE(LU,2001) WN
 2001 FORMAT(/'Water content of the dewatered solid ',F6.2,' %')
      IF (TMS1 + WTR1 .LE. 0.0) Then
	  Write(LU,*)' There is no flow in the underflow'
      ELSE
        SOL = 100.0*TMS1/(TMS1+WTR1)
        WRITE(LU,2002) SOL
 2002   FORMAT('Solid content of the underflow ',F6.2,' %')
      End IF
      WRITE(LU,2003) V
 2003 FORMAT(/'Transport velocity along screen from Ng model ',
     *G8.3,' m/s')
      CALL VELOC(1,THETA,TMSF,WD,V1)
      WRITE(LU,2004) V1
 2004 FORMAT('Transport velocity along screen from USBM model ',
     *G8.3,' m/s')
      WRITE(LU,2005) TMS1
 2005 FORMAT(/'Solid flowrate in underflow ',G8.3,' kg/s')

C Check the screen capacity and compare loadings with the data in
c Leonard J W Coal Preparation Wiley 4th Edition 1979 Table 12-3
      SATISFY = .TRUE.
      CALL SGM(OUT2,NDC,NGC,NSC,PPROP,SVM,SGA)
      VOL2 = TMS2*SVM
      BED = VOL2/(0.6*WD*V)
      WRITE(LU,2006) BED*100.0
 2006 FORMAT('Bed depth at discharge end ',F5.1,' cm')
      WATERF = WTR*3.6/WD
      WRITE(LU,2007) WATERF
 2007 FORMAT('Water rate at feed end is ',F6.1,
     *' m**3/hr per m of screen width.')
      IF(WATERF .GT. 150000*MESH) THEN
        WRITE(LU,2008)15000*MESH
 2008   FORMAT('  This exceeds the recommended capacity of ',F6.1)
        SATISFY = .FALSE.
      END IF
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER1,DER2,DER3)
      CALL PASSSZ(DER1,DER2,NDC,0.8,PSIZE)
      IF(PSIZE*1000 .LE. 12.0) CINF = 20 + 2.5*PSIZE*1000
      IF(PSIZE*1000 .GT. 12.0) CINF = 50 + 0.31*PSIZE*1000
      CAPACITY = CINF*(1.0-EXP(-4.0*MESH*1000))
      LOAD = TMSF*3.6/WD
      WRITE(LU,2009) LOAD
 2009 FORMAT('The load on the screen is ',F6.2,
     *' tons/hr per meter of screen width.')
      IF(LOAD .GT. CAPACITY) THEN
        WRITE(LU,2010) CAPACITY
 2010   FORMAT('  This exceeds the the recommended capacity of ',F6.2)
        SATISFY = .FALSE.
      END IF
      IF(SATISFY) WRITE(LU,2011)
 2011 FORMAT('The load on the screen is within the recommended capacity
     *limits.')
      WRITE(LU,2012)100.0*LOAD/CAPACITY
 2012 FORMAT('The load on the screen is about ',F5.1,'% of the recommend
     *ed capacity.')
      RETURN
      END
C
      SUBROUTINE RFSC2(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,DER3,NDC,
     *NGC,NSC,WTR,WTR1,WTR2,SIZE,S,THETA,A11,A12,H,DIA,U,A,B,C,D,E,F,W,
     *HT,Q,R,XN,SurfaceWater,LU)
C     ******************************************************************
C     REPORT FOR AN HORIZONTAL OR INCLINED VIBRATING SCREEN
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL SIZE(1)
      CHARACTER*7 UNITS

      !COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      Integer NUNIT
      COMMON /MODELDAT/NUNIT

      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      COMMON/FND501/D50
      LU=8
      TONS=TMSF*3.6
      TU=TMS1*3.6
      TU1=TONS-TU
      CALL SZSCAL(H,UNITS,FACTR,IPWR)
      W1=H*FACTR
      W2=DIA*FACTR
      WRITE(LU,1) TONS,A11,A12,W1,UNITS,W2,UNITS,THETA
   1  FORMAT(/5X,'Tonnage to be processed           : ',G10.3,'tons/hour
     *.'/5X,'Screen length is                  : ',F6.2,' m'
     */5X,'Screen width is                   : ',F6.2,' m'
     */5X,'Mesh aperture is                  : ',F6.2,1X,A7
     */5X,'Wire diameter is                  : ',F6.2,1X,A7
     */5X,'Screen is inclined at ',F5.2,' degrees to the horizontal')
      IF(S.LT.2.0)THEN
       CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
       WRITE(LU,5)
   5   FORMAT(//5X,'PARTICLE SIZE DISTRIBUTION IN THE FEED.')
       CALL WRSZDN(DER2,DER3,NDC,IPWR,LU)
      END IF
C
      AREA=A11*A12
      PCM=(WTR*100.)/(WTR+TMSF)
      GC=0.975*((1.0-XN)**0.511)
      WS=100.0*XN
      W1=HT*FACTR
      W2=D50*FACTR
      WRITE (LU,8) W1,UNITS,Q,R,WS,W2,UNITS,PCM,U
   8  FORMAT (//5X,'Calculated throughfall aperture      : ',F6.2,1X,A7
     */5X,'Percent oversize in feed             : ',F6.3,' %'
     */5X,'Percent half size in feed            : ',F6.3,' %'
     */5X,'Near size in feed                    : ',F6.3,' %'
     */5X,'D50 for separation                   : ',F6.2,1X,A7
     */5X,'Percent moisture in feed             : ',F6.2,' %'
     */5X,'Bulk density of material in feed     : ',F7.2,' kgs/m**3')
C
      WRITE(LU,14) A,B,C,D,E,F,GC
  14  FORMAT(//5X,'Basic capacity factor A              : ',F6.3,' tph/m
     **m'/5X,'Oversize factor B                    : ',F6.3
     */5X,'Half-size factor C                   : ',F6.3
     */5X,'Deck location factor D               : ',F6.3
     */5X,'Wet screening factor E               : ',F6.3
     */5X,'Material weight factor F             : ',F6.3
     */5X,'Near-size factor Gc                  : ',F6.3)
C
      EFF1=(B*C*D*E*F*GC)*100.
      EFF2=(TMS1/((100.-Q)*TMSF))*10000.0
      AUF=TU/(AREA*W*GC)
      CALL VELOC(JT,THETA,TMSF,A12,VEL)
      BEND=(TMSF/(VEL*U*A12))*100.0
      BDIS=(TMS2/(VEL*U*A12))*100.0
      WRITE (LU,55) TU,EFF1,EFF2
   55 FORMAT (/5X,'Tonnage in underflow                 : ',F7.2,' tph'
     */5X,'Total capacity factor (B*C*D*E*F*Gc) : ',F6.2,' %'
     */5X,'Simulated efficiency                 : ',F6.2,' %')
      WRITE (LU,57) AUF
   57 FORMAT (/5X,'Area utilisation factor              : ',F6.2)
      IF (AUF.GT.1.) WRITE (LU,*)' WARNING: This screen is overloaded.',
     *' A larger screen should be considered.'
      IF (AUF.LE.1..AND.AUF.GE.0.8) WRITE (LU,*)
      IF (AUF.LT..8) WRITE (LU,*)' WARNING: This screen is underloaded.'
     *,' A smaller screen could be considered.'
      WRITE (LU,61) VEL,BEND,BDIS
   61 FORMAT (/5X,'Calculated rate of travel            : ',F6.2,' m/s'
     */5X,'Bed depth at feed end                : ',F6.2,' cms'
     */5X,'Bed depth at discharge end           : ',F6.2,' cms')
      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,15)
   15 FORMAT(//5X,'PARTICLE SIZE DISTRIBUTION IN THE OVERFLOW.')
      CALL WRSZDN(DER2,DER3,NDC,IPWR,LU)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,16)
   16 FORMAT(/5X,'PARTICLE SIZE DISTRIBUTION IN THE UNDERFLOW')
      CALL WRSZDN(DER2,DER3,NDC,IPWR,LU)
      CALL NOTES(LU,1)
      IF(.NOT.UNITWFEED) THEN
        WRITE(LU,17)
   17 FORMAT(6x,'The screen was shown as operating without water sprays'
     &/6x,'Using water sprays could increase the capacity of the screen.
     &')
      END IF
      WRITE(LU,18)SurfaceWater
   18 FORMAT(/' Surface water retained on the oversize',F6.2,' %')
      IF(WTR1 .LT. 0.0) THEN
        WRITE(LU,19)
   19   FORMAT('    Water sprays are necessary to achieve this water ret
     &ention')
      END IF
      RETURN
      END
C
      SUBROUTINE VELOC(JT,THETA,TMSF,DW,VEL)
C     **************************************
C
C  THIS MODEL IS BASED ON THE REPORT 'A SIMULATION MODEL FOR PREDICTING
C  THE PERFORMANCE OF VIBRATING SCREENS',USBM CONTRACT REPORT,
C  J0395138, DECEMBER 1982,p. 102-104.
C  JT IS DECK TYPE.0=Resilient 1=Woven wire 2=Punched steel
C  THETA IS ANGLE OF INCLINATION. TMSF IS FEEDRATE.
C  DW IS DECK WIDTH. VEL IS CONVEYING VELOCITY.
      IF (JT.EQ.0) B1=-0.01*THETA
      IF (JT.EQ.1) B1=-0.018*THETA
      IF (JT.EQ.2) B1=-0.027*THETA
      IF (JT.EQ.0) B2=48.1+5.19*THETA
      IF (JT.EQ.1) B2=49.3+4.81*THETA
      IF (JT.EQ.2) B2=55.0+9.0*THETA
      VEL=((B1*TMSF*1.097/DW)+B2)*5.08E-3
      RETURN
      END
C
      SUBROUTINE RFSC(DECK,TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,SIZE,CUT,PPROP,INDPP,NPP,DIMS,
     *LENGTH,WIDTH,SurfaceWater)
C     ********************************************************
C
C  DESIGN REPORT FOR SIMPLE MODEL FOR HORIZONTAL OR INCLINED
C  VIBRATING SCREENS.
C
      INTEGER DECK
      INTEGER INDPP(NPP,2)
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)

      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      LOGICAL DIMS
      REAL LENGTH,WIDTH

      LU=8
      TONS =TMSF*3.6
      TU=TMS1*3.6
      WRITE(LU,1010)TONS,TU
 1010 FORMAT(/5X,'Tonnage to be processed',G10.4,' tonnes/hr.'
     */5X,'Tonnage in undersize product',G10.4,' tonnes/hr.')
      D50=CUT*1000.0
      WRITE(LU,1020)D50,SurfaceWater
 1020 FORMAT(/5X,'Mesh size',F7.2,' mms.'/
     *5X,'Surface water on deck oversize ',F5.1,' %')
      WRITE(LU,1040)
 1040 FORMAT(/5X,'Factors are calculated for horizontal screen.')
      IF(D50 .LE. 25) FA = 20.0*D50**0.33 - 1.28
      IF(D50 .GT. 25) FA = 0.783*D50 + 37
      WRITE(LU,1050) FA
 1050 FORMAT(/5X,'Basic capacity = ',F7.3,
     *' tons per hour per square meter')
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1055)
 1055 FORMAT(/5X,'Particle size distribution in the feed.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL FRPASS(DER2,DER3,NDC,F50,CUT)
      CALL FRPASS(DER2,DER3,NDC,FH,0.5*CUT)
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      F50=(1.0-F50)
      WS = EXP(4.22*F50-3.5)
      OFACTOR = 0.914*EXP(WS)
      HFACTOR = 2.0*FH + 0.2
      DFACTOR = 1.1 - 0.1*DECK
      BDM=SGA*0.6*1000.0
      BFACTOR = BDM/1600
      WRITE(LU,1060)F50*100,OFACTOR,FH*100,HFACTOR,
     *DFACTOR,SGA,BDM,BFACTOR
 1060 FORMAT(/5X,'Percent oversize in feed',F6.2,'%'
     */5X,'Oversize factor is ',F6.3,
     */5X,'Percent half size in feed',F6.2,'%'
     */5X,'Half-size factor is ',F6.3,
     */5X,'Deck location factor ',F5.2,
     */5X,'Specific gravity of material in feed',F5.2,
     */5X,'Bulk density of material in feed',F6.0,' kgs/m**3'
     */5X,'Bulk density factor is ',F6.3)
      AREA = TONS/(FA*OFACTOR*HFACTOR*BFACTOR*DFACTOR)
      WRITE(LU,1070) AREA
 1070 FORMAT(/5X,
     *'Screen area required ',T35,F7.3,' sq meters')
      IF(DIMS) THEN
        WRITE(LU,1100) LENGTH,WIDTH,LENGTH*WIDTH
 1100   FORMAT(
     *'       Length of screen ',T35,F7.3,' m'/
     *'       Width of screen ',T35,F7.3,' m'/
     *'     Screen area installed ',T35,F7.3,' sq meters')
      END IF
      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1080)
 1080 FORMAT(/5X,'Particle size distribution in the overflow.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1090)
 1090 FORMAT(/5X,'Particle size distribution in the underflow')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL NOTES(LU,5)
      RETURN
      END

      SUBROUTINE RPSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      LOGICAL DIMS

      COMMON /MODELDAT/NUNIT
C
C  Probability screen
C     PARAMETERS IN ORDER
C     *******************
C     1....Amplitude of vibration
C     2....Vibration frequency
c     3....Angle of inclination
C     4....Vibration throw angle
C     5....Aperture size
C     6....Screen width
c     7....Screen length
c     8....Surface water on screen oversize
c     9....Number of screens in parallel.
C
C WRITE THE DESIGN REPORT.
C ************************
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'VIBRATING SCREEN',16,'PSCN')

      IPAR = PARAM(9)
      WRITE(LU,1001)PARAM(1)*1000,PARAM(2),PARAM(3),PARAM(4),
     *PARAM(5)*1000,PARAM(6),PARAM(7),PARAM(8),IPAR
 1001 FORMAT(/
     *'Amplitude of vibration ',F6.1,' mm'/
     *'Vibration frequency ',F6.0,' Hz'/
     *'Angle of inclination 'F6.1,' degrees'/
     *'Vibration throw angle ',F6.1,' degrees'/
     *'Aperture size ',F6.1,' mm'/
     *'Screen width ',F7.2,' m'/
     *'Screen length ',F7.2,' m'/
     *'Surface water on screen oversize ',F6.1,' %'/
     *'Number of screens in parallel ',I3)

      Width = PARAM(6)
      Feedrate = TMSF*3.6/(Width*IPAR)
      WRITE(LU,1002)Feedrate
 1002 FORMAT(/'Load on screen',F10.3,' tonnes/hr m')
      IF(Feedrate .LT. 0.36) THEN
        WRITE(LU,*)'   This is less than the recommended minimum of 0.36
     * tonnes/hr m'
      ELSE IF(Feedrate .GT. 60) THEN
        WRITE(LU,*)'   This exceeds the recommended maximum of 60 tonnes
     */ hr m'
      END IF

      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1055)
 1055 FORMAT(/5X,'Particle size distribution in the feed.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)

      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1080)
 1080 FORMAT(/5X,'Particle size distribution in the overflow.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1090)
 1090 FORMAT(/5X,'Particle size distribution in the underflow')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)

	!Write data to plot the classification function
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'PSCN',
     *FEED,OUT2,'Vibrating screen',16)

      RETURN
      END


      SUBROUTINE RKSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
	EXTERNAL MRKSCN

      COMMON /MODELDAT/NUNIT
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MRKSCN,PARAM(9))

c  Return to MODSIM.
      RETURN
      END
C
C
      SUBROUTINE MRKSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

	Real k0,s0,sigma,MeshSize,Lc

C
C  Probability screen
C     PARAMETERS IN ORDER
C     *******************
C     1....Amplitude of vibration
C     2....Vibration frequency
c     3....Angle of inclination
C     4....Vibration throw angle
C     5....Aperture size
C     6....Screen width
c     7....Screen length
c     8....Surface water on screen oversize
c     9....Attachment factor for fines
c    10....Number of screens in parallel.


	Real, Allocatable :: OUT2Lc(:,:,:)
	Real, Allocatable :: CUMSIZE(:)
	Real, Allocatable :: CUM(:),DENS(:)

	IF (.NOT. Allocated(CUMSIZE)) Allocate(CUMSIZE(NDC))
	IF (.NOT. Allocated(CUM)) Allocate(CUM(NDC))
	IF (.NOT. Allocated(DENS)) Allocate(DENS(NDC))

      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,CUMSIZE,CUM,DENS)
	CALL PASSSZ(CUMSIZE,CUM,NDC,0.8,d80F)
C
C WRITE THE DESIGN REPORT.
C ************************


      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'VIBRATING SCREEN',16,'KSCN')

      IPAR = PARAM(10)
      WRITE(LU,1001)PARAM(1),PARAM(2),PARAM(3),PARAM(4),
     *PARAM(5),PARAM(6),PARAM(7),PARAM(8),PARAM(9),IPAR
 1001 FORMAT(/
     *'Kinetic constant for crowded region   ',F8.3,' 1/m'/
     *'Kinetic constant for separated region ',F8.3,' 1/m'/
     *'Power exponent for separated region   'F6.1,/
     *'Aperture size ',F6.1,' mm'/
     *'Screen width  ',F7.2,' m'/
     *'Screen length ',F7.2,' m'/
     *'Surface water on screen oversize ',F6.1,' %'/
     *'Velocity of travel down screen ',F7.2,' m/s'/
     *'Attachment factor for fine particles ',F8.4/
     *'Number of screens in parallel ',I3)

      k0 = PARAM(1)
	s0 = PARAM(2)
	sigma = PARAM(3)
      MeshSize = 0.001*PARAM(4)
      Width = PARAM(5)
	ScreenLength = PARAM(6)
	Velocity = PARAM(8)
      Feedrate = TMSF*3.6/(Width)
      WRITE(LU,1002)Feedrate
 1002 FORMAT(/'Load on screen',F10.3,' t/h m')

	CALL KineticSreen(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,NDC,NGC,NSC,
     *SIZE,PPROP,k0,s0,sigma,MeshSize,ScreenLength,Width,Velocity,
     *Attach,Lc,d80F)

	!Calculate bed depth at feed end
	CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      BedSV = SVM/0.6
	BedDepth = TMSF*BedSV/(Width*Velocity)
	Write(LU,1003) 1000*BedDepth,1000*2*d80F
 1003 FORMAT(/'Bed depth at feed end ',F 7.2,' mm'/
     *'Bed depth at transition from loaded to separated regions ',
     *F7.2,' mm')
      If (Lc .LE. 0.02) then
	  Write(LU,*)'Screen is lightly loaded over the entire length'
      Else If (Lc .LE. ScreenLength) then
        Write(LU,1004) Velocity*2*d80F*3.6/BedSV,Lc
 1004   FORMAT('Flow on screen at end of heavily loaded section ',
     *  F10.2,' t/h m'/
     *  'Transition from loaded to separated regions at ',
     *   F6.2,' m'/)
	Else
	  Write(LU,*)'Screen is heavily loaded over the entire length'
      End If

  200 CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1055)
 1055 FORMAT(/5X,'Particle size distribution in the feed.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)

      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1080)
 1080 FORMAT(/5X,'Particle size distribution in the overflow.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1090)
 1090 FORMAT(/5X,'Particle size distribution in the underflow')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)

	!Write data to plot the classification function
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'KSCN',
     *FEED,OUT2,'Vibrating screen',16)

	IF ( Allocated(CUMSIZE)) DeAllocate(CUMSIZE)
	IF ( Allocated(CUM)) DeAllocate(CUM)
	IF ( Allocated(DENS)) DeAllocate(DENS)



      RETURN
      END


      SUBROUTINE RCSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
	EXTERNAL MRCSCN

      COMMON /MODELDAT/NUNIT
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MRCSCN,PARAM(12))

c  Return to MODSIM.
      RETURN
      END
C
C
      SUBROUTINE MRCSCN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

	Real k0,s0,sigma,MeshSize1,MeshSize2,Lc1,Lc2

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


	Real, Allocatable :: OUT2Lc(:,:,:)
	Real, Allocatable :: CUMSIZE(:)
	Real, Allocatable :: CUM(:),DENS(:)

	IF (.NOT. Allocated(CUMSIZE)) Allocate(CUMSIZE(NDC))
	IF (.NOT. Allocated(CUM)) Allocate(CUM(NDC))
	IF (.NOT. Allocated(DENS)) Allocate(DENS(NDC))

      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,CUMSIZE,CUM,DENS)
	CALL PASSSZ(CUMSIZE,CUM,NDC,0.8,d80F)
C
C WRITE THE DESIGN REPORT.
C ************************


      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'VIBRATING COMPOUND SCREEN',25,'CSCN')

      IPAR = PARAM(12)
      WRITE(LU,1001)PARAM(1),PARAM(2),PARAM(3),PARAM(4),
     *PARAM(5),PARAM(6),PARAM(11),IPAR
 1001 FORMAT(/
     *'Kinetic constant for crowded region   ',F8.3,' 1/s'/
     *'Kinetic constant for separated region ',F8.3,' 1/s'/
     *'Power exponent for separated region   'F6.1,/
     *'Screen width  ',F7.2,' m'/
     *'Surface water on screen oversize ',F6.1,' %'/
     *'Velocity of travel down screen ',F7.2,' m/s'/
     *'Attachment factor for fine particles ',F8.4/
     *'Number of screens in parallel ',I3)

      WRITE(LU,1005) PARAM(7),PARAM(8),PARAM(9),PARAM(10)
 1005 FORMAT(/
     *'Aperture size on first screen ',F6.1,' mm'/
     *'Screen length for first screen ',F7.2,' m'/
     *'Aperture size on second screen ',F6.1,' mm'/
     *'Screen length for second screen ',F7.2,' m')

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

      Feedrate = TMSF*3.6/(Width)
      WRITE(LU,1002)Feedrate,TMSF
 1002 FORMAT(/'Load on screen',F10.3,' t/h m',G12.4)

	CALL KineticSreen(TMSF,TMS1,TMS2,FEED,OUT1,DER2,NDC,NGC,NSC,
     *SIZE,PPROP,k0,s0,sigma,MeshSize1,ScreenLength1,Width,Velocity,
     *Attach,Lc1,d80F)
	WS = TMS2

	CALL KineticSreen(WS,TMS3,TMS2,DER2,OUT3,OUT2,NDC,NGC,NSC,
     *SIZE,PPROP,k0,s0,sigma,MeshSize2,ScreenLength2,Width,Velocity,
     *Attach,Lc2,d80F)


	!Calculate bed depth at feed end
	CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
	BedSV = SVM/0.6
	BedDepth1 = TMSF*BedSV/(Width*Velocity)
	BedDepth2 = WS*BedSV/(Width*Velocity)
	Write(LU,1003) 1000*BedDepth1,1000*BedDepth2,1000*2*d80F
 1003 FORMAT(/'Bed depth at feed end ',F 7.2,' mm'/
     *'Bed depth at start of second screen section ',F7.2,' mm'/
     *'Bed depth at transition from loaded to separated region ',
     *F7.2,' mm')
      If (Lc1 .LE. 0.02) then
	  Write(LU,*)'Screen is lightly loaded over the entire length'
      Else If (Lc1 .LE. ScreenLength1) then
        Write(LU,1004) Velocity*2*d80F*3.6/BedSV,Lc1
 1004   FORMAT('Flow on screen at end of heavily loaded section ',
     *  F10.2,' t/h m'/
     *  'Transition from loaded to separated regions at ',
     *   F6.2,' m'/)
      Else If (Lc2 .LE. ScreenLength2) then
        Write(LU,1004) Velocity*2*d80F*3.6/BedSV,Lc1+Lc2
	Else
	  Write(LU,*)'Screen is heavily loaded over the entire length'
      End If

  200 CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1055)
 1055 FORMAT(/5X,'Particle size distribution in the feed.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)

      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1080)
 1080 FORMAT(/5X,'Particle size distribution in the overflow.')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1090)
 1090 FORMAT(/5X,'Particle size distribution in the fine underflow')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      CALL PARTSZ(SIZE,OUT3,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1091)
 1091 FORMAT(/5X,'Particle size distribution in the coarse underflow')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)

	!Write data to plot the classification function
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'CSCN',
     *FEED,OUT2,'Compound vibrating screen',25)
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'CSCN',
     *FEED,OUT3,'Compound vibrating screen',25)
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'CSCN',
     *FEED,OUT1,'Compound vibrating screen',25)

	IF ( Allocated(CUMSIZE)) DeAllocate(CUMSIZE)
	IF ( Allocated(CUM)) DeAllocate(CUM)
	IF ( Allocated(DENS)) DeAllocate(DENS)



      RETURN
      END
