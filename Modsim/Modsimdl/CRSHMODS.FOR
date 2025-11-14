C*******************     CRSHMODS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE CRUSHER MODELS FOR MODSIM.                   C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE GYRA(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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
C
C  MODEL FOR GYRATORY  CRUSHER
C  *****************************
C
C  THIS SUBROUTINE ASSUMES THAT THE SIZE DISTRIBUTION IN THE
C  PRODUCT IS OF STANDARD TYPE
C  DATA FROM NORDBERG PROCESS MACHINERY REFERENCE MANUAL MAY 1976
C
C  PARAMETERS IN ORDER
C  *******************
C  1...OPEN SIDE SETTING IN METERS
C  2..."PRODUCT TYPE"
C  3...WORK INDEX OF MATERIAL
      REAL LK,LN, CumFeed(NDC,NGC,NSC), CumPrdct(NDC,NGC,NSC)
	REAL Prdct(NDC,NGC,NSC), PrdctAdj(NDC,NGC,NSC)
	REAL CumPrdctAdj(NDC,NGC,NSC)
      ALGR(X)=ALOG(1.0/(1.0-X))
      G=PARAM(1)
      PT=PARAM(2)
      UN=1.5
      LN=0.85
      UK=ALGR(PT)**(-1.0/UN)
      PB=1.0-EXP(-(0.5/UK)**UN)
      LK=0.5*(ALGR(PB)**(-1./LN))
      ND=NDC-1
C	Zero position 1 in DER2
      DO J=1,NGC
        DO K=1,NSC
          DER2(1,J,K)=0.0
        END DO
      END DO
      DO I=1,NDC
        DO J=1,NGC
          DO K =1,NSC
            DER2(1,J,K)=DER2(1,J,K)+FEED(I,J,K)
          END DO
        END DO
      END DO
      Y1=1.0
      DO I=1,NDC
        WS=0.0
        IF(I.LT.NDC) WS=SQRT(SIZE(I)*SIZE(I+1))/G
        IF(WS.GE.0.5) THEN
          AN=UN
          AK=UK
        ELSE
          AN=LN
          AK=LK
        END IF
        WS1=(WS/AK)**AN
        Y=1.0
        IF(WS1.LT.10.0) Y=1.0-EXP(-WS1)
        DO J=1,NGC
          DO K=1,NSC
            OUT1(I,J,K)=(Y1-Y)*DER2(1,J,K)
            DER1(I,J,K)=Y1-Y
          END DO
        END DO
        Y1=Y
      END DO
      TMS1=0.0
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
            TMS1=TMS1+OUT1(I,J,K)
          END DO
        END DO
      END DO
C     START ADJUSTMENT WHEN THE PRODUCT SIZE DISTRIBUTION CROSSES THE FEED 
C     THIS SHOULD BE DONE IN ALL STANDARD TYPE MODELS 
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
		  CumFeed(I,J,K) = 0.0 
		  CumPrdct(I,J,K) = 0.0
	      CumPrdctAdj(I,J,K) = 0.0
          END DO
        END DO
      END DO

      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
            CumFeed(I,J,K) = CumFeed(I,J,K) + FEED(I,J,K)/TMSF
		  CumPrdct(I,J,K) = CumPrdct(I,J,K) + OUT1(I,J,K)/TMS1
          END DO
        END DO
      END DO
      DO I=NDC-1,1,-1
	    DO J=1,NGC
          DO K=1,NSC
	       CumFeed(I,J,K) = CumFeed(I+1,J,K)+CumFeed(I,J,K)
             CumPrdct(I,J,K) = CumPrdct(I+1,J,K)+CumPrdct(I,J,K)
	    END DO
	    END DO
	END DO
	DO I=NDC,1,-1
		DO J=1,NGC
          DO K=1,NSC
		   IF(CumPrdct(I,J,K) .LT. CumFeed(I,J,K)) THEN
		      CumPrdctAdj(I,J,K) = CumFeed(I,J,K)
		   ELSE
	          CumPrdctAdj(I,J,K) = CumPrdct(I,J,K)
	       END IF
		END DO
	    END DO
	END DO
C     DEACUMULATE
	DO J=1,NGC
      DO K=1,NSC
	   PrdctAdj(NDC,J,K)=CumPrdctAdj(NDC,J,K)
	   Prdct(NDC,J,K)=CumPrdct(NDC,J,K)
	END DO
	END DO
	DO I=NDC-1,1,-1
		DO J=1,NGC
          DO K=1,NSC    
	        PrdctAdj(I,J,K) = CumPrdctAdj(I,J,K) - CumPrdctAdj(I+1,J,K)
		    Prdct(I,J,K) = CumPrdct(I,J,K) - CumPrdct(I+1,J,K)
	  	    IF(Prdct(I,J,K) .LT. 0.0) Prdct(I,J,K) = 0.0
	        IF(PrdctAdj(I,J,K) .LT. 0.0) PrdctAdj(I,J,K) = 0.0
	    END DO
	    END DO
	END DO
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
             OUT1(I,J,K) = PrdctAdj(I,J,K)*TMS1
          END DO
        END DO
      END DO

      FL=1
      WTR1=WTR
      RETURN
      END
C
      SUBROUTINE CRSH(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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
      REAL K1,K2,K3,KK,N,M
C
C
C  UNIT TYPE: STANDARD CONE CRUSHER
C  ********************************
C
C  THIS SUBROUTINE IS BASED ON THE CRUSHING ZONE AND INTERNAL CLASSIFICA
C  CLASSIFICATION MODEL DESCRIBED BY WHITEN ET AL.
C  REF:
C   1 WHITEN WJ THE SIMULATION OF CRUSHING PLANTS WITH MODELS
C     DEVELOPED USING MULTIPLE SPLINE REGRESSION. APPLICATION OF
C     COMPUTER METHODS IN THE MINERAL INDUSTRY. ED MDG SALAMON AND
C     LANCASTER. S. AFR.INST.MIN.METALL. JOHANNESBURG,1973. P317-323
C
C   2 WHITEN WJ, WALTER GW, AND WHITE ME, A BREAKAGE FUNCTION SUITABLE
C     FOR CRUSHER MODELS. 4TH TEWKSBURY SYPOSIUM,MELBOURNE,FEB 1973
C     P19.1-19.32
C
C  BREAKAGE AND CLASSIFICATION FUNCTIONS FROM REF 2 ABOVE
C
C  PARAMETERS IN ORDER
C  *******************
C  1...CLOSED SIDE SETTING IN METERS
C  2...PROPORTION OF FINES PRODUCED DURING BREAKAGE EVENTS
C  3...IMPACT WORK INDEX OF MATERIAL
C  4...FACTOR FOR CLASSIFICATION PARAMETER K1
C  5...FACTOR FOR CLASSIFICATION PARAMETER K2
      CSS=PARAM(1)
      KK=PARAM(2)
      K1=PARAM(4)*CSS
      K2=PARAM(5)*CSS
      K3=1.0
      N=4.5
      M=0.5
      FL=1
      TMS1=0.0
      WTR1=WTR
C  OUT2 HOLDS THE PRODUCT C*X FOR EACH SIZE
C  ----------------------------------------
      DO 30 I=1,NDC
      C=1.0
      IF(SIZE(I).LT.K2) C=1.0-((SIZE(I)-K2)/(K1-K2))**K3
      IF(SIZE(I).LT.K1) C=0.0
      WS1=0.0
      WS=SIZE(1)
      IF(I.LT.NDC)WS1=SQRT(SIZE(I)*SIZE(I+1))
      IF(I.GT.1) WS=SQRT(SIZE(I-1)*SIZE(I))
      DO 30 J=1,NGC
      DO 30 K=1,NSC
      W=FEED(I,J,K)
      L1=I-1
      IF(L1.EQ.0) GO TO 20
      DO 10 L=1,L1
      B1=(1.0-KK)*(WS1/SIZE(L))**N + KK*(WS1/SIZE(L))**M
      B=(1.0-KK)*(WS/SIZE(L))**N + KK*(WS/SIZE(L))**M - B1
      W=W+B*OUT2(L,J,K)
   10 CONTINUE
   20 CONTINUE
      BII=1.0-(1.0-KK)*(WS1/SIZE(I))**N - KK*(WS1/SIZE(I))**M
      W=W/(1.0-BII*C)
      OUT2(I,J,K)=W*C
      OUT1(I,J,K)=W*(1.0-C)
      TMS1=TMS1+OUT1(I,J,K)
      DER1(I,J,K)=(1.0-C)/(1.0-BII*C)
   30 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE SHHD(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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
      REAL K1,K2,K3,KK,N,M
C
C
C  UNIT TYPE: SHORT HEAD CONE CRUSHER
C  **********************************
C
C  THIS SUBROUTINE IS BASED ON THE CRUSHING ZONE AND INTERNAL CLASSIFICA
C  CLASSIFICATION MODEL DESCRIBED BY WHITEN ET AL.
C  REFS:
C   1 WHITEN WJ THE SIMULATION OF CRUSHING PLANTS WITH MODELS
C     DEVELOPED USING MULTIPLE SPLINE REGRESSION. APPLICATION OF
C     COMPUTER METHODS IN THE MINERAL INDUSTRY. ED MDG SALAMON AND
C     LANCASTER. S. AFR.INST.MIN.METALL. JOHANNESBURG,1973. P317-323
C
C   2 WHITEN WJ, WALTER GW, AND WHITE ME, A BREAKAGE FUNCTION SUITABLE
C     FOR CRUSHER MODELS. 4TH TEWKSBURY SYPOSIUM,MELBOURNE,FEB 1973
C     P19.1-19.32
C
C   3 Karra V K. A process performance model for cone crushers. PROC.
C     15th INT. MINERAL PROCESSING CONGRESS. TORONTO. CAN INST.MIN.
C     METALL. 1982. pp III-6.1 - III-6.14.
C
C  BREAKAGE AND CLASSIFICATION FUNCTIONS FROM REF 3 ABOVE.
C
C  PARAMETERS IN ORDER
C  *******************
C  1...CLOSED SIDE SETTING IN METERS
C  2...PROPORTION OF FINES PRODUCED DURING BREAKAGE EVENTS
C  3...IMPACT WORK INDEX OF MATERIAL
C  4...FACTOR FOR CLASSIFICATION PARAMETER K1
C  5...FACTOR FOR CLASSIFICATION PARAMETER K2
      CSS=PARAM(1)
      KK=PARAM(2)
      K1=PARAM(4)*CSS
      K2=PARAM(5)*CSS+0.004826
      K3=2.0
      N=2.475
      M=0.518
      FL=1
      TMS1=0.0
      WTR1=WTR
C  OUT2 HOLDS THE PRODUCT C*X FOR EACH SIZE
C  ----------------------------------------
      DO 30 I=1,NDC
      C=1.0
      IF(SIZE(I).LT.K2) C=1.0-((SIZE(I)-K2)/(K1-K2))**K3
      IF(SIZE(I).LT.K1) C=0.0
      WS1=0.0
      WS=SIZE(1)
      IF(I.LT.NDC)WS1=SQRT(SIZE(I)*SIZE(I+1))
      IF(I.GT.1) WS=SQRT(SIZE(I-1)*SIZE(I))
      DO 30 J=1,NGC
      DO 30 K=1,NSC
      W=FEED(I,J,K)
      L1=I-1
      IF(L1.EQ.0) GO TO 20
      DO 10 L=1,L1
      B1=(1.0-KK)*(WS1/SIZE(L))**N + KK*(WS1/SIZE(L))**M
      B=(1.0-KK)*(WS/SIZE(L))**N + KK*(WS/SIZE(L))**M - B1
      W=W+B*OUT2(L,J,K)
   10 CONTINUE
   20 CONTINUE
      BII=1.0-(1.0-KK)*(WS1/SIZE(I))**N - KK*(WS1/SIZE(I))**M
      W=W/(1.0-BII*C)
      OUT2(I,J,K)=W*C
      OUT1(I,J,K)=W*(1.0-C)
      TMS1=TMS1+OUT1(I,J,K)
      DER1(I,J,K)=(1.0-C)/(1.0-BII*C)
   30 CONTINUE
      IF(IFLAG.LT.1)RETURN
      RETURN
      END
C
C
      SUBROUTINE CRS1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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
C
C  MODEL FOR SYMONS CONE CRUSHER
C  *****************************
C
C  THIS SUBROUTINE ASSUMES THAT THE SIZE DISTRIBUTION IN THE
C  PRODUCT IS OF STANDARD TYPE
C  DATA FROM NORDBERG PROCESS MACHINERY REFERENCE MANUAL MAY 1976
C
C  PARAMETERS IN ORDER
C  *******************
C  1...CLOSED SIDE SETTING IN METERS
C
      REAL CumFeed(NDC,NGC,NSC), CumPrdct(NDC,NGC,NSC)
	REAL Prdct(NDC,NGC,NSC), PrdctAdj(NDC,NGC,NSC)
	REAL CumPrdctAdj(NDC,NGC,NSC)
      G=PARAM(1)
      ND=NDC-1
      DO 190 J=1,NGC
      DO 190 K=1,NSC
  190 DER2(1,J,K)=0.0
      DO 200 I=1,NDC
      DO 200 J=1,NGC
      DO 200 K =1,NSC
      DER2(1,J,K)=DER2(1,J,K)+FEED(I,J,K)
  200 CONTINUE
      Y1=1.0
      DO 100 I=1,NDC
      WS=0.0
      IF(I.LT.NDC) WS=SQRT(SIZE(I)*SIZE(I+1))/G
      IF(WS.LT.0.9) GO TO 10
      AN=2.373
      AK=1.03
      GO TO 40
   10 IF(WS.LT.0.48) GO TO 20
      AN=1.779
      AK=1.10
      GO TO 40
   20 AN=0.985
      AK=2.2
   40 CONTINUE
      WS1=(WS/AK)**AN
      Y=1.0
      IF(WS1.LT.10.0) Y=1.0-EXP(-WS1)
      DO 50 J=1,NGC
      DO 50 K=1,NSC
      OUT1(I,J,K)=(Y1-Y)*DER2(1,J,K)
      DER1(I,J,K)=Y1-Y
   50 CONTINUE
      Y1=Y
  100 CONTINUE
      TMS1=0.0
      DO 300 I=1,NDC
      DO 300 J=1,NGC
      DO 300 K=1,NSC
  300 TMS1=TMS1+OUT1(I,J,K)

C     START ADJUSTMENT WHEN THE PRODUCT SIZE DISTRIBUTION CROSSES THE FEED 
C     THIS SHOULD BE DONE IN ALL STANDARD TYPE MODELS 
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
		  CumFeed(I,J,K) = 0.0 
		  CumPrdct(I,J,K) = 0.0
	      CumPrdctAdj(I,J,K) = 0.0
          END DO
        END DO
      END DO

      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
            CumFeed(I,J,K) = CumFeed(I,J,K) + FEED(I,J,K)/TMSF
		  CumPrdct(I,J,K) = CumPrdct(I,J,K) + OUT1(I,J,K)/TMS1
          END DO
        END DO
      END DO
      DO I=NDC-1,1,-1
	    DO J=1,NGC
          DO K=1,NSC
	       CumFeed(I,J,K) = CumFeed(I+1,J,K)+CumFeed(I,J,K)
             CumPrdct(I,J,K) = CumPrdct(I+1,J,K)+CumPrdct(I,J,K)
	    END DO
	    END DO
	END DO
	DO I=NDC,1,-1
		DO J=1,NGC
          DO K=1,NSC
		   IF(CumPrdct(I,J,K) .LT. CumFeed(I,J,K)) THEN
		      CumPrdctAdj(I,J,K) = CumFeed(I,J,K)
		   ELSE
	          CumPrdctAdj(I,J,K) = CumPrdct(I,J,K)
	       END IF
		END DO
	    END DO
	END DO
C     DEACUMULATE
	DO J=1,NGC
      DO K=1,NSC
	   PrdctAdj(NDC,J,K)=CumPrdctAdj(NDC,J,K)
	   Prdct(NDC,J,K)=CumPrdct(NDC,J,K)
	END DO
	END DO
	DO I=NDC-1,1,-1
		DO J=1,NGC
          DO K=1,NSC    
	        PrdctAdj(I,J,K) = CumPrdctAdj(I,J,K) - CumPrdctAdj(I+1,J,K)
		    Prdct(I,J,K) = CumPrdct(I,J,K) - CumPrdct(I+1,J,K)
	  	    IF(Prdct(I,J,K) .LT. 0.0) Prdct(I,J,K) = 0.0
	        IF(PrdctAdj(I,J,K) .LT. 0.0) PrdctAdj(I,J,K) = 0.0
	    END DO
	    END DO
	END DO
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
             OUT1(I,J,K) = PrdctAdj(I,J,K)*TMS1
          END DO
        END DO
      END DO

      FL=1
      WTR1=WTR
      RETURN
      END
C
C
C
      SUBROUTINE JAW1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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
C
C  MODEL FOR JAW CRUSHER
C  *********************
C
C  THIS SUBROUTINE ASSUMES THAT THE SIZE DISTRIBUTION IN THE
C  PRODUCT IS OF STANDARD TYPE.
C  DATA FROM NORDBERG PROCESS MACHINERY REFERENCE MANUAL MAY 1976
C
C  PARAMETERS IN ORDER
C  *******************
C  1...OPEN-SIDE SETTING IN METERS
C  2...IMPACT WORK INDEX OF MATERIAL
      REAL CumFeed(NDC,NGC,NSC), CumPrdct(NDC,NGC,NSC)
	REAL Prdct(NDC,NGC,NSC), PrdctAdj(NDC,NGC,NSC)
	REAL CumPrdctAdj(NDC,NGC,NSC)
      ITERM=0
      G=PARAM(1)
      ND=NDC-1
      DO 190 J=1,NGC
      DO 190 K=1,NSC
  190 DER2(1,J,K)=0.0
      DO 200 I=1,NDC
      DO 200 J=1,NGC
      DO 200 K =1,NSC
        DER2(1,J,K)=DER2(1,J,K)+FEED(I,J,K)
  200 CONTINUE
      Y1=1.0
      DO 100 I=1,NDC
        WS=0.0
        IF(I.LT.NDC) WS=SQRT(SIZE(I)*SIZE(I+1))/G
        IF(WS.LE.0.4073) THEN
          AN=0.9756
          AK=0.81
        ELSE
          AN=1.435
          AK=0.65
        END IF
        WS1=(WS/AK)**AN
        Y=1.0
        IF(WS1.LT.10.0) Y=1.0-EXP(-WS1)
        DO 50 J=1,NGC
        DO 50 K=1,NSC
          OUT1(I,J,K)=(Y1-Y)*DER2(1,J,K)
          DER1(I,J,K)=Y1-Y
   50   CONTINUE
        Y1=Y
  100 CONTINUE
      TMS1=0.0
      DO 300 I=1,NDC
      DO 300 J=1,NGC
      DO 300 K=1,NSC
  300 TMS1=TMS1+OUT1(I,J,K)

C     START ADJUSTMENT WHEN THE PRODUCT SIZE DISTRIBUTION CROSSES THE FEED 
C     THIS SHOULD BE DONE IN ALL STANDARD TYPE MODELS 
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
		  CumFeed(I,J,K) = 0.0 
		  CumPrdct(I,J,K) = 0.0
	      CumPrdctAdj(I,J,K) = 0.0
          END DO
        END DO
      END DO

      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
            CumFeed(I,J,K) = CumFeed(I,J,K) + FEED(I,J,K)/TMSF
		  CumPrdct(I,J,K) = CumPrdct(I,J,K) + OUT1(I,J,K)/TMS1
          END DO
        END DO
      END DO
      DO I=NDC-1,1,-1
	    DO J=1,NGC
          DO K=1,NSC
	       CumFeed(I,J,K) = CumFeed(I+1,J,K)+CumFeed(I,J,K)
             CumPrdct(I,J,K) = CumPrdct(I+1,J,K)+CumPrdct(I,J,K)
	    END DO
	    END DO
	END DO
	DO I=NDC,1,-1
		DO J=1,NGC
          DO K=1,NSC
		   IF(CumPrdct(I,J,K) .LT. CumFeed(I,J,K)) THEN
		      CumPrdctAdj(I,J,K) = CumFeed(I,J,K)
		   ELSE
	          CumPrdctAdj(I,J,K) = CumPrdct(I,J,K)
	       END IF
		END DO
	    END DO
	END DO
C     DEACUMULATE
	DO J=1,NGC
      DO K=1,NSC
	   PrdctAdj(NDC,J,K)=CumPrdctAdj(NDC,J,K)
	   Prdct(NDC,J,K)=CumPrdct(NDC,J,K)
	END DO
	END DO
	DO I=NDC-1,1,-1
		DO J=1,NGC
          DO K=1,NSC    
	        PrdctAdj(I,J,K) = CumPrdctAdj(I,J,K) - CumPrdctAdj(I+1,J,K)
		    Prdct(I,J,K) = CumPrdct(I,J,K) - CumPrdct(I+1,J,K)
	  	    IF(Prdct(I,J,K) .LT. 0.0) Prdct(I,J,K) = 0.0
	        IF(PrdctAdj(I,J,K) .LT. 0.0) PrdctAdj(I,J,K) = 0.0
	    END DO
	    END DO
	END DO
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
             OUT1(I,J,K) = PrdctAdj(I,J,K)*TMS1
          END DO
        END DO
      END DO

      FL=1
      WTR1=WTR
      RETURN
      END
C
C
      SUBROUTINE JAW2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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
C
C  MODEL FOR JAW CRUSHER
C  *********************
C
C  THIS SUBROUTINE ASSUMES THAT THE SIZE DISTRIBUTION IN THE
C  PRODUCT IS OF STANDARD TYPE.
C  DATA FROM SAMANCOR'S MAMATWAN CRUSHING PLANT.
C
C  PARAMETERS IN ORDER
C  *******************
C  1...OPEN-SIDE SETTING IN METERS
C  2...Impact work index of the material in this crusher
      REAL CumFeed(NDC,NGC,NSC), CumPrdct(NDC,NGC,NSC)
	REAL Prdct(NDC,NGC,NSC), PrdctAdj(NDC,NGC,NSC)
	REAL CumPrdctAdj(NDC,NGC,NSC)
      G=PARAM(1)
      ND=NDC-1
      DO 190 J=1,NGC
      DO 190 K=1,NSC
  190 DER2(1,J,K)=0.0
      DO 200 I=1,NDC
      DO 200 J=1,NGC
      DO 200 K =1,NSC
      DER2(1,J,K)=DER2(1,J,K)+FEED(I,J,K)
  200 CONTINUE
      Y1=1.0
      DO 100 I=1,NDC
      WS=0.0
      IF(I.LT.NDC) WS=SQRT(SIZE(I)*SIZE(I+1))/G
      IF(WS.GT.0.455) GO TO 10
      AN=0.588
      AK=2.555
      GO TO 40
   10 CONTINUE
      AN=2.099
      AK=0.744
   40 CONTINUE
      WS1=(WS/AK)**AN
      Y=1.0
      IF(WS1.LT.10.0) Y=1.0-EXP(-WS1)
      DO 50 J=1,NGC
      DO 50 K=1,NSC
      OUT1(I,J,K)=(Y1-Y)*DER2(1,J,K)
      DER1(I,J,K)=Y1-Y
   50 CONTINUE
      Y1=Y
  100 CONTINUE
      TMS1=0.0
      DO 300 I=1,NDC
      DO 300 J=1,NGC
      DO 300 K=1,NSC
  300 TMS1=TMS1+OUT1(I,J,K)
C     START ADJUSTMENT WHEN THE PRODUCT SIZE DISTRIBUTION CROSSES THE FEED 
C     THIS SHOULD BE DONE IN ALL STANDARD TYPE MODELS 
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
		  CumFeed(I,J,K) = 0.0 
		  CumPrdct(I,J,K) = 0.0
	      CumPrdctAdj(I,J,K) = 0.0
          END DO
        END DO
      END DO

      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
            CumFeed(I,J,K) = CumFeed(I,J,K) + FEED(I,J,K)/TMSF
		  CumPrdct(I,J,K) = CumPrdct(I,J,K) + OUT1(I,J,K)/TMS1
          END DO
        END DO
      END DO
      DO I=NDC-1,1,-1
	    DO J=1,NGC
          DO K=1,NSC
	       CumFeed(I,J,K) = CumFeed(I+1,J,K)+CumFeed(I,J,K)
             CumPrdct(I,J,K) = CumPrdct(I+1,J,K)+CumPrdct(I,J,K)
	    END DO
	    END DO
	END DO
	DO I=NDC,1,-1
		DO J=1,NGC
          DO K=1,NSC
		   IF(CumPrdct(I,J,K) .LT. CumFeed(I,J,K)) THEN
		      CumPrdctAdj(I,J,K) = CumFeed(I,J,K)
		   ELSE
	          CumPrdctAdj(I,J,K) = CumPrdct(I,J,K)
	       END IF
		END DO
	    END DO
	END DO
C     DEACUMULATE
	DO J=1,NGC
      DO K=1,NSC
	   PrdctAdj(NDC,J,K)=CumPrdctAdj(NDC,J,K)
	   Prdct(NDC,J,K)=CumPrdct(NDC,J,K)
	END DO
	END DO
	DO I=NDC-1,1,-1
		DO J=1,NGC
          DO K=1,NSC    
	        PrdctAdj(I,J,K) = CumPrdctAdj(I,J,K) - CumPrdctAdj(I+1,J,K)
		    Prdct(I,J,K) = CumPrdct(I,J,K) - CumPrdct(I+1,J,K)
	  	    IF(Prdct(I,J,K) .LT. 0.0) Prdct(I,J,K) = 0.0
	        IF(PrdctAdj(I,J,K) .LT. 0.0) PrdctAdj(I,J,K) = 0.0
	    END DO
	    END DO
	END DO
      DO I=1,NDC
        DO J=1,NGC
          DO K=1,NSC
             OUT1(I,J,K) = PrdctAdj(I,J,K)*TMS1
          END DO
        END DO
      END DO
      FL=1
      WTR1=WTR
      RETURN
      END

C
      SUBROUTINE EMJC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
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

      LOGICAL FIRST

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
C  Empirical Model for Jaw and Gyratory Crushers
C  *********************************************
C
C  This model is based on reference Csoke B, Petho S, Foldesi J
C  and Meszaros. Optimization of stone-quarry technologies.  Intnl Jnl.
C  of Mineral Processing 44-45 (1996) 447 - 459.
C
C  The model is based on the idea that material in the feed smaller than
C  the gap passes straight through the crusher and the larger material
C  is crushed to a predefined size distribution that is modeled by
C       B(r) = (r/rmax)^m
C          r  = dp/GAP
C        rmax = dpmax/GAP
C

C  PARAMETERS IN ORDER
C  *******************
C  1...Crusher gap
C  2...rmax
C  3...Coefficient m
C  4...Impact work index of the material in this crusher

      GAP = PARAM(1)
      RMAX = PARAM(2)
      AM = PARAM(3)
      TMS1 = 0.0
      DO K = 1,NSC
        DO J = 1, NGC
          WS = 0.0
          DO I = 1, NDC
            IF(SIZE(I) .LT. GAP) THEN
C             Pass undersize material straight through
              OUT1(I,J,K) = FEED(I,J,K)
            ELSE
C             Accumulate the rest for breakage
              WS = WS + FEED(I,J,K)
              OUT1(I,J,K) = 0.0
            END IF
          END DO
          FIRST = .TRUE.
          DO I = 1, NDC
            IF(SIZE(I) .LE. RMAX*GAP) THEN
C             Get the fraction of the broken material in this class
              IF(I .EQ. NDC) THEN
                BL = 0.0
              ELSE
                BL = (SQRT(SIZE(I)*SIZE(I+1))/(RMAX*GAP))**AM
              END IF
              IF(I .EQ. 1 .OR. FIRST) THEN
                BU = 1.0
                FIRST = .FALSE.
              ELSE
                BU = (SQRT(SIZE(I-1)*SIZE(I))/(RMAX*GAP))**AM
              END IF
              OUT1(I,J,K) = (BU - BL)*WS + OUT1(I,J,K)
            END IF
            TMS1 = TMS1 + OUT1(I,J,K)
          END DO
        END DO
      END DO
      WTR1 = WTR
      RETURN
      END
