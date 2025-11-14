C*******************	  GENLMODS.FOR	   *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS UTILTY ROUTINES TO SUPPPORT THE UNIT MODEL       C
C  SUBROUTINES IN MODSIM.  See APPENDIX B in the MODSIM MANUAL.        C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C

      SUBROUTINE MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MODEL,FACTOR)
C      *****************************************************************
C
C  THIS SUBROUTINE DIVIDES THE FEED AND RECOMBINES THE PRODUCTS FOR
C  MULTIPLE UNITS IN PARALLEL.
      USE GLOBALS
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
      
	IF(FACTOR .LT. 1.0) Then
        FACTOR = 1.0
	  WRITE(UnitDiagFile,*)
     *  ' Number of units in parallel was specified incorrectly'
	  WRITE(UnitDiagFile,*)
     *  ' A single unit has been assumed'
	END IF
      TMSF=TMSF/FACTOR
      TMS1=TMS1/FACTOR
      TMS2=TMS2/FACTOR
      TMS3=TMS3/FACTOR
      WTR=WTR/FACTOR
	WTR1 = WTR1/FACTOR
	WTR2 = WTR2/FACTOR
	WTR3 = WTR3/FACTOR
      WATERADD = WATERADD/FACTOR
      DO 10 ND=1,NDC
      DO 10 NG=1,NGC
      DO 10 NS=1,NSC
      FEED(ND,NG,NS)=FEED(ND,NG,NS)/FACTOR
      OUT1(ND,NG,NS)=OUT1(ND,NG,NS)/FACTOR
      OUT2(ND,NG,NS)=OUT2(ND,NG,NS)/FACTOR
      OUT3(ND,NG,NS)=OUT3(ND,NG,NS)/FACTOR
   10 CONTINUE
C
      CALL MODEL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
      WTR=WTR*FACTOR
      WTR1=WTR1*FACTOR
      WTR2=WTR2*FACTOR
      WTR3=WTR3*FACTOR
      WATERADD = WATERADD*FACTOR
      TMSF=TMSF*FACTOR
      TMS1=TMS1*FACTOR
      TMS2=TMS2*FACTOR
      TMS3=TMS3*FACTOR
      DO 20 ND=1,NDC
      DO 20 NG=1,NGC
      DO 20 NS=1,NSC
      FEED(ND,NG,NS)=FEED(ND,NG,NS)*FACTOR
      OUT1(ND,NG,NS)=OUT1(ND,NG,NS)*FACTOR
      OUT2(ND,NG,NS)=OUT2(ND,NG,NS)*FACTOR
      OUT3(ND,NG,NS)=OUT3(ND,NG,NS)*FACTOR
   20 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE SPLT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C    THIS SUBROUTINE SPLITS THE FEED INTO N STREAMS
C
C    PARAMETERS IN ORDER
C    *******************
C    1....NUMBER OF OUTPUT STREAMS
C    2....FRACTIONAL SPLIT TO OUTPUT STREAM 1
C    3....FRACTIONAL SPLIT TO OUTPUT STREAM 2
C
      NoSplits = NINT(PARAM(1))
      SP1=PARAM(2)
      SP2=PARAM(3)

      IF(NoSplits .EQ. 1) Then
        SP1 = 1.0
        SP2 = 0.0
        SP3 = 0.0
      ELSE IF (NoSplits .EQ. 2) THEN
        SP1 = PARAM(2)
        IF(SP1 .LT. 0.0) THEN
          SP1 = 0.0
        END IF
        IF(SP1 .GT. 1.0) THEN
          SP1 = 1.0
        END IF
        SP2 = 1.0 - SP1
        SP3 = 0.0
      ELSE IF (NoSplits .EQ. 3) THEN
        SP1 = PARAM(2)
        SP2 = PARAM(3)
        IF(SP1+SP2 .GT. 1.0) THEN
          SP2 = 1.0 - SP1
        END IF
        SP3 = 1.0 - SP1 - SP2
      ELSE
        RETURN
      End IF
      IF(SP1 .LT. 0.0 .OR. SP2 .LT. 0.0) THEN
        write(13,1001) NUNIT
 1001 FORMAT(/' One of the split fractions from unit',I3,
     *        ' is negative!')
        RETURN
      END IF
      IF(SP3 .LT. -0.00001) THEN
        write(13,1001) NUNIT
        RETURN
      END IF
C
      TMS1=SP1*TMSF
      TMS2=SP2*TMSF
      TMS3=SP3*TMSF
      WTR1=SP1*WTR
      WTR2=SP2*WTR
      WTR3=SP3*WTR
C
      NV2=NDC*NGC*NSC
      DO 10 I=1,NDC
        DO 10 J=1,NGC
          DO 10 K=1,NSC
            OUT1(I,J,K)=SP1*FEED(I,J,K)
            OUT2(I,J,K)=SP2*FEED(I,J,K)
            OUT3(I,J,K)=SP3*FEED(I,J,K)
 10   CONTINUE
      RETURN
      END
C
C
      SUBROUTINE SPL1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
      CALL SPLT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
      RETURN
      END

      SUBROUTINE PUMP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C   THIS SUBROUTINE PASSES THE STREAM STRAIGHT THROUGH THE UNIT
C   WITHOUT CHANGE. It simulates a pump and allows gland water to be added.

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C   NO PARAMETERS
C   *************
C
      DO 10 ID=1,NDC
		DO 10 IG=1,NGC
			DO 10 IS=1,NSC
				OUT1(ID,IG,IS)=FEED(ID,IG,IS)
				DER1(ID,IG,IS)=1.0
   10 CONTINUE

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
c	Write(13,*)'WTR,WTR1,WATERADD',WTR,WTR1,WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF
      WTR2 = 0.0
      WTR3 = 0.0
      FL=1
      RETURN
      END

C
      SUBROUTINE NOP_(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C   THIS SUBROUTINE PASSES THE STREAM STRAIGHT THROUGH THE UNIT
C   WITHOUT CHANGE
C
C   NO PARAMETERS
C   *************
C
      DO 10 ID=1,NDC
      DO 10 IG=1,NGC
      DO 10 IS=1,NSC
      OUT1(ID,IG,IS)=FEED(ID,IG,IS)
      DER1(ID,IG,IS)=1.0
   10 CONTINUE
      TMS1=TMSF
      WTR1=WTR
      FL=1
      RETURN
      END
C
      SUBROUTINE READER
C     *****************
      RETURN
      END
C 
      SUBROUTINE MIXR(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C  This is a dummy subroutine that is never called.  It is here to prevent
C  spurious unresolved external message from the linker.
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      RETURN
      END

      SUBROUTINE CONV(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C  This is a dummy conveyor subroutine that is never called.  It is here to prevent
C  spurious unresolved external message from the linker.
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      RETURN
      END

      SUBROUTINE MESH_SIZE(SIZE,NDC)
C     ******************************
C  Calculates the mesh sizes from the representative sizes and places the result in GLOBALS.
      !DEC$ ATTRIBUTES DLLEXPORT :: MESH_SIZE
      USE GLOBALS
      REAL SIZE(NDC)
      MeshSizes(NDC) = 0.0
	IF(NDC .GT. 1) then
		MeshSizes(NDC-1) = SQRT(SIZE(NDC)*SIZE(NDC-1))
		DO I = NDC-2,1,-1
		  MeshSizes(I) = (SIZE(I+1)*SIZE(I+1))/MeshSizes(I+1)
		END DO
	End if
      RETURN
      END

C
      SUBROUTINE PARTSZ(SIZE,FIDGK,NDC,NGC,NSC,CUMSIZ,CUM,DENS)
C     *********************************************************
C  CONVERTS THE 3-WAY DISTRIBUTED PARTICULATE MATERIAL TO PARTICLE SIZE
C  DENSITY AND CUMULATIVE SIZE DISTRIBUTION.
C
      !DEC$ ATTRIBUTES DLLEXPORT :: PARTSZ
	USE GLOBALS
      REAL SIZE(NDC),FIDGK(NDC,NGC,NSC),CUMSIZ(NDC),CUM(NDC),DENS(NDC)
	Do I = 1,NDC
	  CUMSIZ(I) = MeshSizes(I)
	End Do
      TOT=0.0
      ND=NDC-1
      DO 20 I=1,NDC
       DENS(I)=0.
       DO 10 J=1,NGC
         DO 10 K=1,NSC
         DENS(I)=DENS(I)+FIDGK(I,J,K)
   10  CONTINUE
       TOT=TOT+DENS(I)
   20 CONTINUE
C
C  CHECK FOR CASE WHEN STREAM HAS NO SOLIDS.
      IF(TOT.LE.0.00000001)THEN
        write(13,1001)
 1001   FORMAT(' STREAM HAS NO SOLIDS. PARTICLE SIZE DISTRIBUTION CANNOT
     & BE CALCULATED')
        TOT=1.0
      END IF
C      CUMSIZ(NDC)=0.0
      DENS(NDC)=DENS(NDC)/TOT
      CUM(NDC)=0.0
C      WS = SIZE(NDC)*SIZE(NDC-1)
C      CUMSIZ(NDC-1) = SQRT(WS)
      DO 30 I=1,ND
        J=NDC-I
C        IF(I.GT.1) CUMSIZ(J)=SIZE(J+1)**2/CUMSIZ(J+1)
        DENS(J)=DENS(J)/TOT
        CUM(J)=CUM(J+1)+DENS(J+1)
   30 CONTINUE
      RETURN
      END
C
      SUBROUTINE WRSZDN(CUMSIZ,CUM,NDC,ISCALE,LU)
C     *******************************************
C
C  WRITES THE PARTICLE SIZE DISTRIBUTION TO LOGICAL UNIT LU.
C  ISCALE IS THE SCALE FACTOR: 0 FOR METERS, 2 FOR CMS, 3 FOR MMS, 6 FOR
C  MICRONS.
C
      REAL CUMSIZ(NDC),CUM(NDC)
      CHARACTER*7 UNITS(7)
      DATA UNITS /' meters','       ','  cms  ','  mms  ','       ',
     *'       ','microns'/
      I=ISCALE+1
      WRITE(LU,1001)UNITS(I)
 1001 FORMAT(5X,'  Size      % passing'/6X,A7)
      DO 10 I=1,NDC
		CS=CUMSIZ(I)*10**ISCALE
		C=CUM(I)*100.0
		WRITE(LU,1010)CS,C
   10 CONTINUE
 1010 FORMAT(5X,G10.3,F11.3)
      RETURN
      END
C
C
      SUBROUTINE PASSSZ(CUMSIZ,CUM,NDC,P,PSIZE)
C     *****************************************
C  INTERPOLATES A SIZE DISTRIBUTION TO FIND THE P% PASSING SIZE.
C  THE SIZE DISTRIBUTION IS ASSUMED TO BE LOCALLY ROSIN-RAMMLER.
C
      !DEC$ ATTRIBUTES DLLEXPORT :: PASSSZ
      USE GLOBALS
      REAL CUMSIZ(NDC),CUM(NDC)
      LOGICAL PROPER

      ALGR(X)=ALOG(1.0/(1.0-X))

	If (NDC .EQ. 1) Then
	  PSIZE = 0.0
	  Return
	End If
C Check validity of the data
      PROPER = .TRUE.
      IF(CUM(1) .GT. 100.01) PROPER = .FALSE.
      IF(CUM(1) .GT. 1.0001 .AND. P .LT. 1.0) P = P*100
      IF(CUM(1) .LE. 1.0001 .AND. P .GT. 1.0) P = P*0.01
      DO  I = 1,NDC
        IF(CUM(I) .LT. 0.0 .OR. CUMSIZ(I) .LT. 0.0) Then
          PROPER = .FALSE.
        End if
      END DO
	DO I = 1,NDC-1
	  IF(CUM(I+1) .GT. CUM(I)) Then
          PROPER = .FALSE.
	  End if
      END DO
      IF(.NOT. PROPER) THEN
	  write(13,*)'Improper size distribution in subroutine PASSSZ'
	  UnitExitValue = 17
	  RETURN
      END IF

      ND=NDC-1
      I=1
      IF(CUM(I).LE.P) GO TO 100
      DO 10 I=1,ND
        IF(CUM(I) .GT. P .AND. CUM(I+1) .LE. P) GO TO 100
   10 CONTINUE
      I=NDC-1
  100 CONTINUE
      IF(CUM(I) .LE. CUM(I+1)) Then
        Ratio = 0 
	Else 
	  Ratio = (P-CUM(I+1))/(CUM(I)-CUM(I+1))
      End if
      IF((CUM(I).GE.0.99999999) .OR. (CUM(I+1) .LE. 0.000001)) Then 
	  PSIZE = CUMSIZ(I+1) + Ratio*(CUMSIZ(I) - CUMSIZ(I+1))
      Else
      CHI=ALOG(ALGR(P)/ALGR(CUM(I+1)))/ALOG(ALGR(CUM(I))/ALGR(CUM(I+1)))
      PSIZE=CUMSIZ(I+1)*((CUMSIZ(I)/CUMSIZ(I+1))**CHI)
	End if
      RETURN
      END
C
C
      SUBROUTINE FRPASS(CUMSIZ,CUM,NDC,P,PSIZE)
C     *****************************************
C  CALCULATES THE FRACTION PASSING A GIVEN SIZE. DISTRIBUTION IS ASSUMED
C  TO BE LOCALLY ROSIN-RAMMLER.
      !DEC$ ATTRIBUTES DLLEXPORT :: FRPASS
      REAL CUMSIZ(NDC),CUM(NDC)
      LOGICAL COND1,COND2

      ALGR(X)=ALOG(1.0/(1.0-X))

	If (NDC .EQ. 1) Then
	  P = 0.0
	  RETURN
	End If
      ND=NDC-2
      I=1
      P=1.0
      IF(CUMSIZ(I).LE.PSIZE) GO TO 100
      DO 10 I=1,ND
      IF(CUMSIZ(I).LE.PSIZE.AND.CUM(I).GE.1.0)RETURN
      COND1=CUMSIZ(I).GT.PSIZE.AND.CUMSIZ(I+1).LE.PSIZE.AND.CUM(I).LT.1.
      COND2=CUMSIZ(I).LE.PSIZE.AND.CUMSIZ(I+1).LE.PSIZE.AND.CUM(I).LT.1.
      IF(COND1.OR.COND2) GO TO 100
   10 CONTINUE
      alpha = alog(CUM(NDC-2)/CUM(NDC-1))
      alpha = alpha/alog(CUMSIZ(NDC-2)/CUMSIZ(NDC-1))
      P=CUM(NDC-1)*(PSIZE/CUMSIZ(NDC-1))**alpha
      RETURN
  100 CONTINUE
      IF(CUM(I).LE.0.000001.AND.CUM(I+1).LE.0.000001) THEN
        P=0.0
        RETURN
      END IF
      IF(CUM(I).GE.0.999999) I=I+1
      IF(CUM(I+1).LE.0.000001) I=I-1
      XI=ALOG(PSIZE/CUMSIZ(I+1))/ALOG(CUMSIZ(I)/CUMSIZ(I+1))
      ETA=(ALGR(CUM(I))/ALGR(CUM(I+1)))**XI
      P=1.0-(1.0-CUM(I+1))**ETA
      RETURN
      END

      FUNCTION AvGrade(MIN,FIDGK,NDC,NGC,NSC,GRDM,NGCM,NMIN)
C     ******************************************************
C     Calculate the average grade of mineral MIN in a stream.
      REAL FIDGK(NDC,NGC,NSC),GRDM(NGCM,NMIN)
      AvGrade = 0.0
      Flow = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            Flow = Flow + FIDGK(I,J,K)
            AvGrade = Avgrade + FIDGK(I,J,K)*GRDM(J,MIN)
          END DO
        END DO
      END DO
      IF(Flow .LE. 0.0) then
        AvGrade = 0.0
        RETURN
      ELSE
        AvGrade = AvGrade/Flow
      END IF
      RETURN
      END

      FUNCTION THEOYIELD(ASH,MIN,FIDGK,NDC,NGC,NSC,NGCM,GRDM,NMIN)
c     ************************************************************
C Function to calculate the theoretical yield of mineral MIN if the
c feed were separated perfectly to give the cumulative ASH value.
      REAL FIDGK(NDC,NGC,NSC),GRDM(NGCM,NMIN)
      THEOYIELD = 0.0
      TOT = 0.0
      DO 30 J = 1,NGC
        DO 20 I = 1,NDC
          DO 10 K = 1,NSC
            TOT = TOT + FIDGK(I,J,K)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      IF(TOT .LE. 0.0) RETURN
      CUMASH = 0.0
      A1 = 0.0
      CUMYIELD = 0.0
      DO 130 J = 1,NGC
        YIELD = 0.0
        DO 120 K = 1,NSC
          DO 110 I = 1,NDC
            YIELD = YIELD + FIDGK(I,J,K)
  110     CONTINUE
  120   CONTINUE
        ASHPT = YIELD*GRDM(J,MIN)
        IF(J .GT.1) A1 = CUMASH/CUMYIELD
        A2 = (CUMASH + ASHPT)/(CUMYIELD + YIELD)
        IF(A2 .GE. ASH) THEN
          THEOYIELD = CUMYIELD + YIELD*(ASH - A1)/(A2 - A1)
          GO TO 140
        ELSE
          CUMASH = CUMASH + ASHPT
          CUMYIELD = CUMYIELD + YIELD
          THEOYIELD = CUMYIELD
        END IF
  130 CONTINUE
  140 CONTINUE
      THEOYIELD = THEOYIELD/TOT
      RETURN
      END

      FUNCTION ASHCONT(MIN,FIDGK,NDC,NGC,NSC,NGCM,GRDM,NMIN)
C     ******************************************************
C Function to calculate the content of mineral MIN in a stream
      REAL FIDGK(NDC,NGC,NSC),GRDM(NGCM,NMIN)
      ASHCONT = 0.0
      TOT = 0.0
      DO 30 J = 1,NGC
        DO 20 I = 1,NDC
          DO 10 K = 1,NSC
            ASHCONT = ASHCONT + GRDM(J,MIN)*FIDGK(I,J,K)
            TOT = TOT + FIDGK(I,J,K)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      IF(TOT .GT. 0.0) ASHCONT = ASHCONT/TOT
      RETURN
      END
C
      FUNCTION AVSIZE(FIDGK,NDC,NGC,NSC,SIZE)
C     ***************************************
C Calculates the mass average particle size
      REAL FIDGK(NDC,NGC,NSC)
      REAL SIZE(NDC)
      WS = 0.0
      TOT = 0.0
      DO 30 I = 1,NDC
        DO 20 J = 1,NGC
          DO 10 K = 1,NSC
            WS = WS + SIZE(I)*FIDGK(I,J,K)
            TOT = TOT + FIDGK(I,J,K)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE

C  CHECK FOR CASE WHEN STREAM HAS NO SOLIDS.
      IF(TOT.LE.0.00000001)THEN
        write(13,1001)
 1001 FORMAT(' STREAM HAS NO SOLIDS. AVERAGE SIZE CANNOT BE CALCULATED')
        TOT=1.0
      END IF
      AVSIZE = WS/TOT
      RETURN
      END

      SUBROUTINE WASHABIL(MIN,FIDGK,NDC,NGC,NSC,GRDM,NGCM,NMIN,YIELD,
     *CUMASH,CUMYIELD)
C     ***************************************************************
C Calculate the washability curve for mineral MIN in a stream.
      REAL FIDGK(NDC,NGC,NSC),GRDM(NGCM,NMIN)
      REAL YIELD(NGC),CUMASH(NGC),CUMYIELD(NGC)
      TOT = 0.0
      DO 30 J = 1,NGC
        DO 20 I = 1,NDC
          DO 10 K = 1,NSC
            TOT = TOT + FIDGK(I,J,K)
   10     CONTINUE
   20   CONTINUE
        CUMYIELD(J) = TOT
   30 CONTINUE
      IF(TOT .LE. 0.0) RETURN
      CUMYIELD(1) = CUMYIELD(1)/TOT
      YIELD(1) = CUMYIELD(1)
      CUMASH(1) = GRDM(1,MIN)
      DO 130 J = 2,NGC
        YIELD(J) = 0.0
        DO 120 K = 1,NSC
          DO 110 I = 1,NDC
            YIELD(J) = YIELD(J) + FIDGK(I,J,K)
  110     CONTINUE
  120   CONTINUE
        YIELD(J) = YIELD(J)/TOT
        CUMASH(J) = CUMASH(J-1)*CUMYIELD(J-1) + YIELD(J)*GRDM(J,MIN)
        CUMYIELD(J) = CUMYIELD(J)/TOT
        CUMASH(J) = CUMASH(J)/CUMYIELD(J)
  130 CONTINUE
      RETURN
      END

      SUBROUTINE MINSIZE(MIN,FIDGK,NDC,NGC,NSC,GRDM,NGCM,NMIN,CONT)
C     *************************************************************
C Calculate the content of mineral MIN in a stream by size.
      REAL FIDGK(NDC,NGC,NSC),GRDM(NGCM,NMIN)
      REAL CONT(NDC)
      DO 30 I = 1,NDC
        TOT = 0.0
        CONT(I) = 0.0
        DO 20 J = 1,NGC
          DO 10 K = 1,NSC
            TOT = TOT + FIDGK(I,J,K)
            CONT(I) = CONT(I) + GRDM(J,MIN)*FIDGK(I,J,K)
   10     CONTINUE
   20   CONTINUE
        IF(TOT .GT. 0.0) CONT(I) = CONT(I)/TOT
   30 CONTINUE
      RETURN
      END


      SUBROUTINE SGM(FIDGK,NDC,NGC,NSC,PPROP,SVM,SGA)
C     ***********************************************
C  CALCULATES THE MEAN SPECIFIC VOLUME AND MEAN SPECIFIC GRAVITY OF THE
C  SOLIDS IN A MODSIM STREAM.
C
      REAL FIDGK(NDC,NGC,NSC),PPROP(1)
      SVM=0.0
      TOT=0.0
      DO 10 J=1,NGC
	  SV=1.0/PPROP(J)
      DO 10 I=1,NDC
      DO 10 K=1,NSC
        TOT=TOT+FIDGK(I,J,K)
        SVM=SVM+FIDGK(I,J,K)*SV
   10 CONTINUE
      SVM=SVM/(1000.0*TOT)
      SGA=1.0/(SVM*1000.0)
      RETURN
      END
C
C
      SUBROUTINE SZSCAL(X,UNITS,FACTR,IPWR)
C     *************************************
C
C  FINDS SUITABLE UNITS FOR A VARIABLE X HAVING DIMENSIONS OF METERS.
C  UNITS RETURNS THE UNIT NAME AND FACTR THE SCALE FACTOR.
      CHARACTER*7 DIMS(7),UNITS
      DATA DIMS/' meters','  dms  ','  cms  ','  mms  ','       ',
     *'       ','microns'/
      IF(X.LT.1.0E-4) THEN
        I=7
      ELSE IF(X.LT.1.0E-2) THEN
        I=4
      ELSE IF(X.LT.0.1) THEN
        I=3
      ELSE
        I=1
      END IF
      UNITS=DIMS(I)
      IPWR=I-1
      FACTR=10**IPWR
      RETURN
      END
C
C
      SUBROUTINE VOLFLW(MF,VF,NDC,NGC,NSC,PPROP,TM,TV)
C     ************************************************
C  CONVERTS THE MASS FLOW IN EACH CLASS TO VOLUME FLOWRATE.
C
      REAL MF(NDC,NGC,NSC),VF(NDC,NGC,NSC)
	REAL PPROP(*)
	TM=0.0
	TV=0.0
	DO 50 J=1,NGC
	  SV=1.0/(1000*PPROP(J))
	  DO 50 I=1,NDC
	  DO 50 K=1,NSC
	    VF(I,J,K)=MF(I,J,K)*SV
	    TM=TM+MF(I,J,K)
	    TV=TV+VF(I,J,K)
   50 CONTINUE
	RETURN
	END

C
      FUNCTION SETVEL(DP,RHOS,RHO,MU)
C     *******************************
C  CALCULATES THE TERMINAL SETTLING VELOCITY OF A SPHERICAL
C  PARTICLE.
C  THE METHOD DEVELOPED BY CONCHA AND ALMENDRA IS USED.
C  REFERENCE:  Concha F and Almendra E R. Settling velocity of
C  particulate systems, 1 Settling velocities of individual sperical
C  particles.  Int. Journal of Mineral Processing. 5(1979)349-367.
      REAL MU
      WS=1.333*ABS(RHOS-RHO)*9.81
      DSTAR3=WS*RHO*DP*DP*DP/(MU*MU)
      DSTAR=DSTAR3**0.3333
      USTAR=20.52*(SQRT(1.0+0.0921*SQRT(DSTAR3))-1.0)**2/DSTAR
      SETVEL=USTAR*(WS*MU/(RHO*RHO))**0.3333
      IF(RHOS.LT.RHO) SETVEL=-SETVEL
      RETURN
      END
C
      FUNCTION SETVELN(DP,RHOS,RHO,MU,PSI)
C     ***********************************
C  CALCULATES THE TERMINAL SETTLING VELOCITY OF A NON SPHERICAL
C  PARTICLE.
C  THE METHOD DEVELOPED BY CONCHA AND ALMENDRA IS USED.
C  REFERENCE:  Concha F and Almendra E R. Settling velocity of
C  particulate systems, 1 Settling velocities of individual sperical
C  particles.  Int. Journal of MIneral Processing. 5(1979)349-367.
      REAL MU

C  First calculate the correction factors for sphericity and density ratio
      FA=(5.42 - 4.75*PSI)/0.67
      FB=(0.843*FA*ALOG10(PSI/0.065))**(-0.5)
      FC=(RHO/RHOS)**0.0145
      FD=(RHOS/RHO)**0.00725
      ALPHA=FB*FB
      BETA=1.0/(SQRT(FA)*ALPHA)
      GAMMA=FD*FD
      ETA=1.0/(SQRT(FC)*FD*FD)

C  Now calculate the modified dp*
      WS=1.333*ABS(RHOS-RHO)*9.81
      DSTAR3=WS*RHO*DP*DP*DP/(MU*MU)
      DSTARM3=DSTAR3*BETA*BETA*ETA*ETA
      DSTAR = DSTAR3**0.3333
      DSTARM=DSTARM3**0.3333

C Now calculate the modified V*
      VSTARM=20.52*(SQRT(1.0+0.0921*SQRT(DSTARM3))-1.0)**2/DSTARM
C ..and then V*
      VSTAR=VSTARM*ALPHA*GAMMA*BETA**0.6667*ETA**0.6667

      SETVELN=VSTAR*(WS*MU/(RHO*RHO))**0.3333
      IF(RHOS.LT.RHO) SETVELN=-SETVELN
      RETURN
      END
C
      FUNCTION LYC(R,IC)
C     ******************
C  CALCULATES THE COMPLEMENTARY LYNCH FUNCTION.
      REAL LYC,IC
      COMMON /LYCDER/ DLYCDR,DLYDIC
      B=1.099/IC
      IF(ABS(B*(R-1.0)).LT.18.4) THEN
        WE1=0.0
        IF(B.LT.50.0) WE1=EXP(-B)
        WE2=EXP(B*(R-1.0))
        WS1=1.0-WE1
        WS2=1.0+WE2-2.0*WE1
        LYC=WS1/WS2
        DLYCDR=-LYC*B*WE2/WS2
        DLYCDB=(WE1+LYC*((1.0-R)*WE2-2.0*WE1))/WS2
        DLYDIC=-DLYCDB*B/IC
      ELSE
        IF(R.LT.1.0-18.4/B) THEN
          LYC=1.0
          DLYCDR=0.0
          DLYDIC=0.0
        ELSE
          LYC=0.0
          DLYCDR=0.0
          DLYDIC=0.0
        END IF
      END IF
      RETURN
      END
C
      FUNCTION LYNCHF(X,X50C,IC)
C     **************************
C  CALCULATE THE LYNCH PARTITION FUNCTION
      REAL IC,LYNCHF,LYC
      R=X/X50C
      LYNCHF=1.0-LYC(R,IC)
      RETURN
      END
