C*******************      MILLMODS.FOR     ****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE STANDARD UNIT MODELS FOR TUMBLING MILLS.     C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
      SUBROUTINE MILL(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

C Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C
C  MODEL IS BASED ON THE BREAKAGE AND SELECTION FUNCTION.
C  SIMPLIFIED VERSIONS OF THE AUSIN CORRELATIONS FOR THE BREAKAGE AND
C  SELECTION FUNCTIONS ARE USED. NO SCALE-UP RELATIONSHIPS ARE PROVIDED.
C  THE MEAN RESIDENCE TIME IS PRESUMED TO BE GIVEN.
C
C  REFERENCES: 1.AUSTIN LG  CHAP.7 OF "GRINDING-THEORY AND PRACTICE"
C                SCHOOL NOTES  S.AFR.INST. MIN. METALL. JOHANNESBURG
C     1977
C              2.AUSTIN LG AND WELLER KR.  SIMULATION AND SCALE-UP OF
C     WET BALL MILLING. PROC. 14THINT.MINERAL PROCESSING
C     CONGRESS. PDR MALTBY (ED.) CAN. INST. MIN. METALL.
C     MONTREAL (1982)  PPI 8.1-8.13
C              3.ROGERS RSC,SHOJI K,HUKKI AM AND LINN. THE EFFECT OF
C     LINER DESIGN ON THE PERFORMANCE OF A CONTINUOUS WET
C     BALL MILL.   PROC 14TH MINERAL PROCESSING CONGRESS.
C     MALTBY PDR. (ED.) CAN. INST. MIN. METALL. MONTREAL
C     (1982) PP I 5.1-5.20
C
C
C SELECTION FUNCTION S(I) IS STORED IN OUT2(I,1,1)
C PARAMETER PHI IS STORED IN OUT3(I,1,1)
C
      REAL MU, LAMBDA
      FL=1
      ALPHA=PARAM(1)
      BETA=PARAM(2)
      GAMMA=PARAM(3)
      DELTA=PARAM(4)
      A = PARAM(5)
      OUT3(1,1,1)=PARAM(6)*(5.0E-3/SIZE(1))**DELTA
      TAU=PARAM(7)
      MU = 0.001*PARAM(8)
      LAMBDA = PARAM(9)
C

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF
      WTR2 = 0.0
      WTR3 = 0.0

      DO 12 I=1,NDC
        OUT2(I,1,1) = Austins(SIZE(I), A , ALPHA, MU, LAMBDA)
        IF(I.EQ.NDC) OUT2(I,1,1)=0.0
        OUT3(I,1,1)=OUT3(1,1,1)*((SIZE(I)/SIZE(1))**(-DELTA))
        IF(OUT3(I,1,1).GT.1.0) OUT3(I,1,1)=1.0
        DO 11 J=1,NGC
          DO 10 K=1,NSC
            PROD(I,J,K)=FEED(I,J,K)/(1.0+TAU*OUT2(I,1,1))
            DER1(I,J,K)=1.0/(1.0+TAU*OUT2(I,1,1))
 10       CONTINUE
 11     CONTINUE
 12   CONTINUE
C
      DO 31 I=2,NDC
        L=I-1
        DO 30 II=1,L
          SZJ=SIZE(I-1)/SIZE(II)
          SZL=0.0
          IF(I.NE.NDC) SZL=SIZE(I)/SIZE(II)
          BIJ=OUT3(II,1,1)*SZJ**GAMMA+(1.0-OUT3(II,1,1))*SZJ**BETA
          BIL=OUT3(II,1,1)*SZL**GAMMA+(1.0-OUT3(II,1,1))*SZL**BETA
          BS=BIJ-BIL
          FR=TAU/(1.0+TAU*OUT2(I,1,1))*OUT2(II,1,1)*BS
C
          DO 21 J=1,NGC
            DO 20 K=1,NSC
              IF(PROD(II,J,K).LT.1.0E-50) GO TO 20
              PROD(I,J,K)=PROD(I,J,K)+FR*PROD(II,J,K)
 20         CONTINUE
 21       CONTINUE
C
 30     CONTINUE
 31   CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE HFMI(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C
C  MODEL IS BASED ON THE BREAKAGE AND SELECTION FUNCTION.
C  The Herbst-Fuerstenau correlation for the energy-specific selection
C  is used.
C  NO SCALE-UP RELATIONSHIPS ARE PROVIDED.
C
C SELECTION FUNCTION S(I)*TAU IS STORED IN OUT2(I,1,1)
C PARAMETER PHI IS STORED IN OUT3(I,1,1)
C
      FL=1
      SE1 = PARAM(1)
      ZETA1 = PARAM(2)
      ZETA2 = PARAM(3)
      BETA = PARAM(4)
      GAMMA=PARAM(5)
      DELTA=PARAM(6)
      OUT3(1,1,1) = PARAM(7)*(5.0E-3/SIZE(1))**DELTA
      PowerDraw = PARAM(8)
C
c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0

      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

C Set up the selection function
      DO 12 I=1,NDC
        OUT2(I,1,1) = HERBST_FUER_SEL(SIZE(I),SE1,ZETA1,ZETA2)*
     *  PowerDraw/(TMSF*3.600)
        IF(I.EQ.NDC) OUT2(I,1,1)=0.0
        OUT3(I,1,1)=OUT3(1,1,1)*((SIZE(I)/SIZE(1))**(-DELTA))
        IF(OUT3(I,1,1).GT.1.0) OUT3(I,1,1)=1.0
        DO 11 J=1,NGC
          DO 10 K=1,NSC
            PROD(I,J,K)=FEED(I,J,K)/(1.0+OUT2(I,1,1))
            DER1(I,J,K)=1.0/(1.0+OUT2(I,1,1))
 10       CONTINUE
 11     CONTINUE
 12   CONTINUE
C
      DO 31 I=2,NDC
        DO 30 II=1,I-1
          SZJ=SIZE(I-1)/SIZE(II)
          SZL=0.0
          IF(I.NE.NDC) SZL=SIZE(I)/SIZE(II)
          BIJ=OUT3(II,1,1)*SZJ**GAMMA+(1.0-OUT3(II,1,1))*SZJ**BETA
          BIL=OUT3(II,1,1)*SZL**GAMMA+(1.0-OUT3(II,1,1))*SZL**BETA
          BS=BIJ-BIL
          FR=OUT2(II,1,1)*BS/(1.0+OUT2(I,1,1))
C
          DO 21 J=1,NGC
            DO 20 K=1,NSC
              IF(PROD(II,J,K).LT.1.0E-50) GO TO 20
              PROD(I,J,K)=PROD(I,J,K)+FR*PROD(II,J,K)
 20         CONTINUE
 21       CONTINUE
C
 30     CONTINUE
 31   CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE GMIL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C MODEL FOR A BALL MILL HAVING THREE PERFECTLY MIXED STAGES IN SERIES.
C NO CLASSIFICATION BETWEEN STAGES.
C PARAMETERS;
C     1....Residence time in the mill.
C     2....SELECTION FUNCTION CHOSEN. 1=STANDARD QUARTZITE, 2=ROGERS
C          FUNCTION FOR PHOSPHATE. 3=Reed,Brame and Austin scale-up
C          model for coal.
C     3....BREAKAGE FUNCTION CHOSEN. 1=STANDARD QUARTZITE, 2=ROGERS
C          FUNCTION FOR PHOSPHATE. 3=Reed,Brame and Austin scale-up
C          model for coal.
C     4....SWITCH FOR ALLOWING FOR OVERFILLING.
C     5....Hardgrove grindability index - only used for coal.
C     6....Choice of liberation model. 1 = Ljubljana,  2 = Beta function
C     7....Specific rate of breakage at 1mm.
C     8....Specific rate parameter alpha.
C     9....Specific rate parameter mu.
C     10...Specific rate parameter Lambda.
C     11...Breakage function parameter beta.
C     12...Breakage function parameter gamma.
C     13...Breakage function parameter delta
C     14...Breakage function parameter Phi.
C     15...Choice of post classification function.  0 = None, 1 = Logistic,
C          2 = Rosin-Rammler, 3 = Exponential sum.
C     16...D50 for post classifier.
C     17...Sharpness index for post classifier.

      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(NDC),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      REAL NewTauPrime
      REAL MRT
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL
      COMMON /COAL/HGI
      COMMON/SCALEUP/OVFILL
 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      EXTERNAL AustinSel
      EXTERNAL BR_FUNCTION

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      NSEL=NINT(PARAM(2))
      NBREAK=NINT(PARAM(3))
      S1 = PARAM(7)
      Alpha = PARAM(8)
      Mu = 0.001*PARAM(9)
      Lambda = PARAM(10)
      Beta = PARAM(11)
      Gamma = PARAM(12)
      Delta = PARAM(13)
      Phi5 = PARAM(14)
      D50 = 0.001*PARAM(16)
      SI = PARAM(17)
C Set SelUsesRep to TRUE in case this calls AustinSelBall
	SelUsesRep = 1 
c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

C Put the overfill multiplier into common /SCALEUP/
      OVFILL=1.0
      IF(PARAM(4).LT.0.0) THEN
        U=1.3
        IF(TMSF.GT.105.5) U=1.3*(TMSF/105.5)**0.5
        OVFILL=EXP(-1.32*(U-1.3))
      END IF

C Put the HGI parameter into common /COAL/
      HGI = PARAM(5)

c Select the liberation model
      LJUBLJANA = .FALSE.
      AM_MODEL = .FALSE.
      IF(NINT(PARAM(6)) .EQ. 1) THEN
        LJUBLJANA = .TRUE.
        AM_MODEL = .FALSE.
      END IF
      IF(NINT(PARAM(6)) .EQ. 2) THEN
        LJUBLJANA = .FALSE.
        AM_MODEL = .TRUE.
      END IF

      DO 10 I=1,NDC
        DO 10 J=1,NGC
          DO 10 K=1,NSC
            DER1(I,J,K)=1.0
   10 CONTINUE
      MRT = PARAM(1)

      NCALL=1
      TAU=0.0137*MRT
      DTBDF = 0.0
      CALL PMMILL(FEED,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,AustinSel,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

      NCALL=2
      TAU=0.2123*MRT
      DTBDF = 0.0
      CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,AustinSel,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

      NCALL=3
      TAU=0.7740*MRT
      DTBDF =  0.0
      TAUPRIME = TAU
      DER2 = OUT2
C If post classification is significant, iteration is required.
      ITNO = 0
  100 CONTINUE
      CALL PMMILL(DER2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,PPROP,
     *INDPP,NPP,AustinSel,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
      IF (NINT(PARAM(15)) .GT. 0) THEN
        WS = 0.0
        Recycle = 0.0
        DO I = 1,NDC
          CI = Classification(SIZE(I),NINT(PARAM(15)),D50,SI)
          DO J = 1,NGC
            DO K = 1,NSC
              Recycle = Recycle + CI*OUT1(I,J,K)
              DER2(I,J,K) = CI*OUT1(I,J,K) + OUT2(I,J,K)
              OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
              DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
            END DO
          END DO
        END DO
        NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
        ITNO = ITNO + 1
        WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
        TAUPRIME = NewTauPrime
        IF(WS .GT. 0.001 .AND. ITNO .LT. 30) GO TO 100
        IF(ITNO .GE. 30) THEN
          WRITE(UnitDiagFile,'(''***Post classification calculation did
     *not converge for unit '',I2/
     *    ''    Try increasing the D50 size of the grate.'')') NUNIT
          CALL FLUSH(UnitDiagFile)
        END IF
      END IF
      FL=1
c Calculate the grade in each size fraction.
c      FEEDMIN = 0.0
c      OUTMIN = 0.0
c      DO I = 1,NDC
c        OUTSIZE = 0.0
c        GRADE = 0.0
c        DO J = 1,NGC
c          DO K = 1,NSC
c            FEEDMIN = FEEDMIN + FEED(I,J,K)*GRDM(J,2)
c            OUTSIZE = OUTSIZE + OUT1(I,J,K)
c            GRADE = GRADE + OUT1(I,J,K)*GRDM(J,2)
c          END DO
c        END DO
c        OUTMIN = OUTMIN + GRADE
c        IF(OUTSIZE.GT.0.0)GRADE = GRADE/OUTSIZE
c      END DO
      RETURN
      END
C
      SUBROUTINE GMI1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C MODEL FOR A BALL MILL HAVING THREE PERFECTLY MIXED STAGES IN SERIES.
C NO CLASSIFICATION BETWEEN STAGES.
C PARAMETERS;
C     1....HOLD UP IN THE MILL IN METRIC TONS.
C     2....SELECTION FUNCTION CHOSEN. 1=STANDARD QUARTZITE, 2=ROGERS
C          FUNCTION FOR PHOSPHATE. 3=Reed,Brame and Austin scale-up
C          model for coal.
C     3....BREAKAGE FUNCTION CHOSEN. 1=STANDARD QUARTZITE, 2=ROGERS
C          FUNCTION FOR PHOSPHATE. 3=Reed,Brame and Austin scale-up
C          model for coal.
C     4....SWITCH FOR ALLOWING FOR OVERFILLING.
C     5....Hardgrove grindability index - only used for coal.
C     6....Choice of liberation model. 1 = Ljubljana,  2 = Beta function
C     7....Specific rate of breakage at 1mm.
C     8....Specific rate parameter alpha.
C     9....Specific rate parameter mu.
C     10...Specific rate parameter Lambda.
C     11...Breakage function parameter beta.
C     12...Breakage function parameter gamma.
C     13...Breakage function parameter delta
C     14...Breakage function parameter Phi.
C     15...Choice of post classification function.  0 = None, 1 = Logistic,
C          2 = Rosin-Rammler, 3 = Exponential sum.
C     16...D50 for post classifier.
C     17...Sharpness index for post classifier.

      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(NDC),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      REAL NewTauPrime
      REAL MRT
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL
      COMMON /COAL/HGI
      COMMON/SCALEUP/OVFILL
 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      EXTERNAL AustinSel
      EXTERNAL BR_FUNCTION

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      NSEL=NINT(PARAM(2))
      NBREAK=NINT(PARAM(3))
      S1 = PARAM(7)
      Alpha = PARAM(8)
      Mu = 0.001*PARAM(9)
      Lambda = PARAM(10)
      Beta = PARAM(11)
      Gamma = PARAM(12)
      Delta = PARAM(13)
      Phi5 = PARAM(14)
      D50 = 0.001*PARAM(16)
      SI = PARAM(17)

C Set SelUsesRep to TRUE in case this calls AustinSelBall
	SelUsesRep = 1 
c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

C Put the overfill multiplier into common /SCALEUP/
      OVFILL=1.0
      IF(PARAM(4).LT.0.0) THEN
        U=1.3
        IF(TMSF.GT.105.5) U=1.3*(TMSF/105.5)**0.5
        OVFILL=EXP(-1.32*(U-1.3))
      END IF

C Put the HGI parameter into common /COAL/
      HGI = PARAM(5)

c Select the liberation model
      LJUBLJANA = .FALSE.
      AM_MODEL = .FALSE.
      IF(NINT(PARAM(6)) .EQ. 1) THEN
        LJUBLJANA = .TRUE.
        AM_MODEL = .FALSE.
      END IF
      IF(NINT(PARAM(6)) .EQ. 2) THEN
        LJUBLJANA = .FALSE.
        AM_MODEL = .TRUE.
      END IF
c      Write(UnitDiagFile,*)' LJUBLJANA,AM_MODEL',LJUBLJANA,AM_MODEL
      DO 10 I=1,NDC
        DO 10 J=1,NGC
          DO 10 K=1,NSC
            DER1(I,J,K)=1.0
   10 CONTINUE
      MRT=1000.0*PARAM(1)/(60.0*TMSF)

      NCALL=1
      TAU=0.0137*MRT
      DTBDF=-TAU/TMSF
      CALL PMMILL(FEED,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,AustinSel,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

      NCALL=2
      TAU=0.2123*MRT
      DTBDF=-TAU/TMSF
      CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,AustinSel,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

      NCALL=3
      TAU=0.7740*MRT
      DTBDF=-TAU/TMSF
      TAUPRIME = TAU
      DER2 = OUT2
C If post classification is significant, iteration is required.
      ITNO = 0
  100 CONTINUE
      CALL PMMILL(DER2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,PPROP,
     *INDPP,NPP,AustinSel,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
      IF (NINT(PARAM(15)) .GT. 0) THEN
        WS = 0.0
        Recycle = 0.0
        DO I = 1,NDC
          CI = Classification(SIZE(I),NINT(PARAM(15)),D50,SI)
          DO J = 1,NGC
            DO K = 1,NSC
              Recycle = Recycle + CI*OUT1(I,J,K)
              DER2(I,J,K) = CI*OUT1(I,J,K) + OUT2(I,J,K)
              OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
              DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
            END DO
          END DO
        END DO
        NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
        ITNO = ITNO + 1
        WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
        TAUPRIME = NewTauPrime
        IF(WS .GT. 0.001 .AND. ITNO .LT. 30) GO TO 100
        IF(ITNO .GE. 30) THEN
          WRITE(UnitDiagFile,'(''***Post classification calculation did
     *not converge for unit '',I2/
     *    ''    Try increasing the D50 size of the grate.'')') NUNIT
          CALL FLUSH(UnitDiagFile)
        END IF
      END IF

      FL=1
c Calculate the grade in each size fraction.
c      FEEDMIN = 0.0
c      OUTMIN = 0.0
c      DO I = 1,NDC
c        OUTSIZE = 0.0
c        GRADE = 0.0
c        DO J = 1,NGC
c          DO K = 1,NSC
c            FEEDMIN = FEEDMIN + FEED(I,J,K)*GRDM(J,2)
c            OUTSIZE = OUTSIZE + OUT1(I,J,K)
c            GRADE = GRADE + OUT1(I,J,K)*GRDM(J,2)
c          END DO
c        END DO
c        OUTMIN = OUTMIN + GRADE
c        IF(OUTSIZE.GT.0.0)GRADE = GRADE/OUTSIZE
c      END DO
      RETURN
      END
C
      SUBROUTINE GMSU(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C MODEL FOR A BALL MILL HAVING THREE PERFECTLY MIXED STAGES IN SERIES.
C NO CLASSIFICATION BETWEEN STAGES.
c This model uses the Austin scale-up procedure
C PARAMETERS;
c   Selection function parameters:
C     1...Specific rate of breakage at 1mm     1/min
C     2...Particle size exponent  alpha
C     3...Size coefficient for maximum breakage  mm
C     4...Exponent for fall off of selection function with size in the
c          abnormal breakage region.
C   Breakage function paprameters:
C     5...Beta
C     6...Gamma
c     7...Delta
c     8...Phi at 5mm
c   Mill dimensions:
C     9...Test mill diameter.
C     10..Test mill length.
C     11..Ball load in test mill.
C     12..Fraction of media filled with solids in test mill.
C     13..Mill speed of test mill.  % of critical.
C     14..Ball size in test mill.   mm
C     15..Full size mill diameter.
C     16..Full size mill length.
c     17..Ball load in full size mill. %
c     18..Fraction of media filled with solids in full size mill. %
c     19..Speed of full size mill  % of critical.
c     20..Make-up ball size in full size mill.  mm
C     21..D50 for post classifier.
C     22..Sharpness index for post classifier.
C     23..Number of ball sizes.
C     24-35 Ball size - fraction pairs.

c     36..Choice of liberation model.  0 = none, 1 = Ljubljana, 2 = Beta function.
C     37..SWITCH FOR ALLOWING FOR OVERFILLING.
C     38..Choice of post classification function.  0 = None, 1 = Logistic,
C          2 = Rosin-Rammler, 3 = Exponential sum.
C     39..Choice of Transport Model.  0 = pm;pm;pm-cl, 1 = pm;pm;pm;cl,
C	   2 = pm-cl;pm-cl;pm-cl
C     40..Fractional residence time, Region 1
C     41..Fractional residence time, Region 2
C     42..Choice for Selection function calculation. 0 = Calculate Selection function using 
C	the upper sieve size of screens in batch test, 1 = Calculate Selection function using 
C     representative sizes.
C     43..Specific gravity of media.  Should only influence power.  

      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(NDC),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      REAL NewTauPrime
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL
      COMMON/SCALEUP/OVFILL
      REAL LmT,Lm,JmT,Jm,MuT
      REAL MillLoad,MRT
      PARAMETER (Pi = 3.1415927)
 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      EXTERNAL AuSelBall
      EXTERNAL BR_FUNCTION

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      S1T = PARAM(1)           !1/min
      Alpha = PARAM(2)
      MuT = 0.001*PARAM(3)
      Lambda = PARAM(4)
      Beta = PARAM(5)
      Gamma = PARAM(6)
      Delta = PARAM(7)
      Phi5 = PARAM(8)
      DmT = PARAM(9)
      LmT = PARAM(10)
      JmT  = 0.01*PARAM(11)
      UT  = PARAM(12)
      PhiCT = 0.01*PARAM(13)
      DbT  = 0.001*PARAM(14)
      Dm  = PARAM(15)
      Lm  = PARAM(16)
      Jm  = 0.01*PARAM(17)
      Udry  = PARAM(18)
      PhiC = 0.01*PARAM(19)
      Db  = 0.001*PARAM(20)
      D50 = 0.001*PARAM(21)
      SI = PARAM(22)
      Mclas = Nint(PARAM(23))
      AN0 = 1.0
      AN2 = 0.0  !AN2 = 0.2
      AN3 = 1.2  !AN3 = 1.0
      Do K = 1,Mclas
	Dk(K) = PARAM(24 + 2*(K-1))
	Mk(K) = 0.01*PARAM(25 + 2*(K-1))*(DbT/Dk(K))**AN0
	MuK(k) = MuT*(DM/DMT)**AN2*(Dk(K)/DbT)**AN3
      End Do
      MU = MuK(1)
C Tolerance and max iterations for internal classifier
	tolIC = 0.00005
	maxitIC = 120
C Set SelUsesRep 
	SelUsesRep = PARAM(42)
C Set the density of media.  Only needed for power calculations.
C The value is not necessary in GMSU.  Only in RGMSU.
	RhoB = PARAM(43) 

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

c Calculate the charge density.
      WS = 0.0
      TOT = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TOT = TOT + FEED(I,J,K)
            WS = WS + FEED(I,J,K)/PPROP(J)
          END DO
        END DO
      END DO
      RhoO = 1000*TOT/WS
      PhiV = (TMS1/RhoO)/(TMS1/RhoO+WTR1/1000.0)

c Calculate Austin's scale up coefficients for the selection function.
      Eps = 0.4
      IF(Dm .GE. 3.81) THEN
        C2 = ((Dm/DmT)**0.5)*((3.81/Dm)**0.2)
      Else
        C2 = ((Dm/DmT)**0.5)
      End If
      C3 = (1.0 + 6.6*(JmT**2.3))/(1.0 + 6.6*(Jm**2.3))
      C4 = (PhiC-0.1)/(PhiCT-0.1)
      C4 = C4*(1.0+EXP(15.7*(PhiCT-0.94)))/(1.0+EXP(15.7*(PhiC-0.94)))
      C5 = exp(-1.32*(Udry - UT))

c Calculate the parameters for Austin's selection function.
      S1 = S1T*C2*C3*C4*C5

c Calculate the total internal volume of the mill.
      Vm = 0.25*Pi*Dm*Dm*Lm

c Calculate the load in the mill.
      Um = Udry*(1-Eps)/PhiV
      VolumeOfSlurry = Jm*Um*Eps
c      write(UnitDiagfile,*)'Jm,Um,Eps',Jm,Um,Eps
c  As per Len's sugestion on 04/15/2003, change this to
c  the formal method, as all scale up coefficients
c  have been derived as such
c      MillLoad = Vm*VolumeOfSlurry*PhiV*RhoO
	MillLoad = Vm*Jm*Udry*Eps*(1-Eps)*RhoO
c      MillLoad = Vm*VolumeOfSlurry*(1.0-Eps)*RhoO
c      write(UnitDiagfile,'('' Total mill volume'',F7.2,'' m**3''
c     */'' Mill load excluding balls'',F7.2,'' tonnes'')')
c     * Vm,0.001*MillLoad

c  Calculate the average residence time.
      MRT = MillLoad/(TMSF*60.0)     !min
c      write(UnitDiagfile,'('' Calculated residence time in the mill''
c     *,G10.4,'' mins'')')MRT

C Put the overfill multiplier into common /SCALEUP/
      OVFILL=1.0
      IF(PARAM(37).LT.0.0) THEN
        U=1.3
        IF(TMSF.GT.105.5) U=1.3*(TMSF/105.5)**0.5
        OVFILL=EXP(-1.32*(U-1.3))
      END IF
c	Write(13,*)' OVFILL',OVFILL

c Select the liberation model
      LJUBLJANA = .FALSE.
      AM_MODEL = .FALSE.
      IF(NINT(PARAM(36)) .EQ. 1) THEN
        LJUBLJANA = .TRUE.
        AM_MODEL = .FALSE.
      END IF
      IF(NINT(PARAM(36)) .EQ. 2) THEN
        LJUBLJANA = .FALSE.
        AM_MODEL = .TRUE.
      END IF

      DO 10 I=1,NDC
        DO 10 J=1,NGC
          DO 10 K=1,NSC
            DER1(I,J,K)=1.0
   10 CONTINUE
C	Set the fractional residence times
	FTAU1 = PARAM(40) !FTAU1 = 0.0137
	FTAU2 = PARAM(41) !FTAU2 = 0.2123
	FTAU3 = 1.0 - FTAU2 - FTAU1
c	Write(UnitDiagFile,*)' FTAU1,FTAU2,FTAU3',FTAU1,FTAU2,FTAU3

C Evaluate the product from the various transport models
C     NO CLASSIFICATION FUNCTION
      IF (NINT(PARAM(38)) .EQ. 0) THEN
	  NCALL=1
        TAU=FTAU1*MRT
        DTBDF=-TAU/TMSF
        CALL PMMILL(FEED,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *  INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

        NCALL=2
        TAU=FTAU2*MRT
        DTBDF=-TAU/TMSF
        CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *  INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

        NCALL=3
        TAU=FTAU3*MRT
        DTBDF=-TAU/TMSF
        CALL PMMILL(OUT2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *  INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
      END IF
C	CLASSIFICATION FUNCTION
      IF (NINT(PARAM(38)) .GT. 0) THEN
C	LAST REGION WITH CLASSIFIER OR PM;PM;PM-CL
	IF (NINT(PARAM(39)) .EQ. 0) THEN
          NCALL=1
          TAU=FTAU1*MRT
          DTBDF=-TAU/TMSF
          CALL PMMILL(FEED,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *    INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=2
          TAU=FTAU2*MRT
          DTBDF=-TAU/TMSF
          CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *    INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=3
          TAU=FTAU3*MRT
          DTBDF=-TAU/TMSF
C START CLASSIFIER LOOP
          TAUPRIME = TAU
          DER2 = OUT2
          ITNO = 0
  100 CONTINUE
          CALL PMMILL(DER2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(38)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT1(I,J,K)
                DER2(I,J,K) = CI*OUT1(I,J,K) + OUT2(I,J,K)
                OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 100
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C END IF FOR PM;PM;PM-CL
	  END IF
C	POST-CLASSIFIER
	  IF (NINT(PARAM(39)) .EQ. 1) THEN
C START CLASSIFIER LOOP
	    DER2 = FEED
	    TAUPRIME = MRT
          ITNO = 0
  200 CONTINUE
          NCALL=1
          TAU=FTAU1*TAUPRIME
          DTBDF=-FTAU1*MRT/TMSF
          CALL PMMILL(DER2,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=2
          TAU=FTAU2*TAUPRIME
          DTBDF=-FTAU2*MRT/TMSF
          CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=3
          TAU=FTAU3*TAUPRIME
          DTBDF=-FTAU3*MRT/TMSF
          CALL PMMILL(OUT2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(38)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT1(I,J,K)
                DER2(I,J,K) = CI*OUT1(I,J,K) + FEED(I,J,K)
                OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = MRT/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 200
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C END IF FOR POST-CLASSIFIER OR PM;PM;PM;CL
	  END IF
C EACH REGION FOLLOWED BY A CLASSIFIER OR PM-CL;PM-CL;PM-CL
	  IF (NINT(PARAM(39)) .EQ. 2) THEN
C START CLASSIFIER LOOP 1
	    DER2 = FEED
	    TAU = FTAU1*MRT
	    DTBDF=-TAU/TMSF
	    TAUPRIME = TAU
          ITNO = 0
  301 CONTINUE
          CALL PMMILL(DER2,OUT3,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(38)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT3(I,J,K)
                DER2(I,J,K) = CI*OUT3(I,J,K) + FEED(I,J,K)
                OUT3(I,J,K) = (1.0 - CI)*OUT3(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 301
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2, ''  Region 1''/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C START CLASSIFIER LOOP 2
	    DER2 = OUT3
	    TAU = FTAU2*MRT
	    DTBDF=-TAU/TMSF
	    TAUPRIME = TAU
          ITNO = 0
  302 CONTINUE
          CALL PMMILL(DER2,OUT2,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(38)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT2(I,J,K)
                DER2(I,J,K) = CI*OUT2(I,J,K) + OUT3(I,J,K)
                OUT2(I,J,K) = (1.0 - CI)*OUT2(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 302
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2, ''  Region 2''/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C START CLASSIFIER LOOP 3
	    DER2 = OUT2
	    TAU = FTAU3*MRT
	    DTBDF=-TAU/TMSF
	    TAUPRIME = TAU
          ITNO = 0
  303 CONTINUE
          CALL PMMILL(DER2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,AuSelBall,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(38)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT1(I,J,K)
                DER2(I,J,K) = CI*OUT1(I,J,K) + OUT2(I,J,K)
                OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 303
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2, ''  Region 3''/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C EACH REGION FOLLOWED BY A CLASSIFIER OR PM-CL;PM-CL;PM-CL
	  END IF
C END IF FOR CLASSIFIER
	END IF

      FL=1
c Calculate the grade in each size fraction.
c      FEEDMIN = 0.0
c      OUTMIN = 0.0
c      DO I = 1,NDC
c        OUTSIZE = 0.0
c        GRADE = 0.0
c        DO J = 1,NGC
c          DO K = 1,NSC
c            FEEDMIN = FEEDMIN + FEED(I,J,K)*GRDM(J,2)
c            OUTSIZE = OUTSIZE + OUT1(I,J,K)
c            GRADE = GRADE + OUT1(I,J,K)*GRDM(J,2)
c          END DO
c        END DO
c        OUTMIN = OUTMIN + GRADE
c        IF(OUTSIZE.GT.0.0)GRADE = GRADE/OUTSIZE
c      END DO
      RETURN
      END

      SUBROUTINE AustinSel(I,Dpi,S)
C     *****************************
C     Austin's selection function compatible with PMMILL
      USE GLOBALS
 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      S = AUSTINS(Dpi,S1,Alpha,Mu,Lambda)
      RETURN
      END

      FUNCTION AUSTINS(DPI,AT,ALPHA,MUT,LAMBDAT)
C     *****************************************
C Calculate the selection function using the Austin, Klimpel, Luckie
c model equations at the test condition (usually a batch test).
c See Austin L G , Klimpel R R and Luckie P T. Process Engineering: Ball Milling
c SME 1984 p112.
C XI represents the upper mesh size of the interval. DPI the representative
c size.
C AT is the specific rate of breakage at 1mm.

      REAL MUT,LAMBDAT
c
C Convert the size to upper mesh size.
c      XI = DPI*1.189
      XI = DPI
      C1 = 1/(1 + (Xi/MuT)**LAMBDAT)
      AUSTINS = AT*((XI*1000)**ALPHA)*C1
      RETURN
      END

      SUBROUTINE AuSelBall(I,Dpi,S)
C     *****************************
C     Austin's selection function compatible with PMMILL and including effect
C     of ball size distribution
      USE GLOBALS
 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      IF(NINT(SelUsesRep) .EQ. 0) THEN
C Convert the size to upper mesh size.
         XI = DPI*1.189
	ELSE
         XI = DPI
	END IF
      C1 = 0.0
      Do K = 1,Mclas
	C1 = C1 + Mk(k)/(1 + (XI/MuK(K))**Lambda)
      End Do
      S = S1*((XI*1000)**ALPHA)*C1
      RETURN
      END


      FUNCTION AUSTINBR(X,GAMMA,BETA,PHI)
C     ***********************************
C The standard Austin breakage function

      AUSTINBR = PHI*X**GAMMA + (1.0-PHI)*X**BETA
      RETURN
      END
C
      SUBROUTINE SB16(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

C Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C
C  MODEL IS BASED ON THE BREAKAGE AND SELECTION FUNCTION AS PROVIDED BY USER FOR 
C  A FIXED NUMBER OF SIZE CLASSES (16).  THIS MODEL IS FOR RESEARCH PURPOSES.
C  NO SCALE-UP RELATIONSHIPS ARE PROVIDED.
C  THE MEAN RESIDENCE TIME IS PRESUMED TO BE GIVEN.
C
C  REFERENCES: 1.AUSTIN LG  CHAP.7 OF "GRINDING-THEORY AND PRACTICE"
C                SCHOOL NOTES  S.AFR.INST. MIN. METALL. JOHANNESBURG
C     1977
C              2.AUSTIN LG AND WELLER KR.  SIMULATION AND SCALE-UP OF
C     WET BALL MILLING. PROC. 14THINT.MINERAL PROCESSING
C     CONGRESS. PDR MALTBY (ED.) CAN. INST. MIN. METALL.
C     MONTREAL (1982)  PPI 8.1-8.13
C              3.ROGERS RSC,SHOJI K,HUKKI AM AND LINN. THE EFFECT OF
C     LINER DESIGN ON THE PERFORMANCE OF A CONTINUOUS WET
C     BALL MILL.   PROC 14TH MINERAL PROCESSING CONGRESS.
C     MALTBY PDR. (ED.) CAN. INST. MIN. METALL. MONTREAL
C     (1982) PP I 5.1-5.20
C
C
C
      FL=1
C
      TAU = PARAM(137)
c Calculate the water flows if water is added to the mill
       
      TMS1=TMSF
      WTR1=WTR
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF
      WTR2 = 0.0
      WTR3 = 0.0
        IF(NDC .NE. 16) THEN
          WRITE(UnitDiagFile,'(''*****Wrong number of size classes  
     *in unit '',I2/
     *    ''    This model requires 16 size classe always'')') NUNIT
          CALL FLUSH(UnitDiagFile)
        END IF

      DO 12 I=1,NDC
        OUT2(I,1,1) = PARAM(I)
        IF(I.EQ.NDC) OUT2(I,1,1)=0.0
        DO 11 J=1,NGC
          DO 10 K=1,NSC
            PROD(I,J,K)=FEED(I,J,K)/(1.0+TAU*OUT2(I,1,1))
 10       CONTINUE
 11     CONTINUE
 12   CONTINUE
C
      DO 31 I=2,NDC
        L=I-1
        DO 30 II=1,L      
	    BS = PARAM(NDC+1+(II-1)*(NDC-II)+(II-1)*II/2+L-II)
          FR=TAU/(1.0+TAU*OUT2(I,1,1))*OUT2(II,1,1)*BS
C
          DO 21 J=1,NGC
            DO 20 K=1,NSC
              IF(PROD(II,J,K).LT.1.0E-50) GO TO 20
              PROD(I,J,K)=PROD(I,J,K)+FR*PROD(II,J,K)
 20         CONTINUE
 21       CONTINUE
C
 30     CONTINUE
 31   CONTINUE
C
      RETURN
      END
C
C
      SUBROUTINE HFML(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C MODEL FOR A BALL MILL HAVING THREE PERFECTLY MIXED STAGES IN SERIES.
C NO CLASSIFICATION BETWEEN STAGES.
C
C  MODEL IS BASED ON THE BREAKAGE AND SELECTION FUNCTION.
C  The Herbst-Fuerstenau correlation for the energy-specific selection
C  is used.
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(NDC),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON/SCALEUP/OVFILL
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL
      COMMON/HERBST/SE1, ZETA1, ZETA2
      EXTERNAL HERBST_FUER
      COMMON/BREAKAGE_PARS/GAMMA,BETA,PHI5,DELTA
      EXTERNAL BR_FUNCTION

      REAL D50, NewTauPrime

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

c Overfilling is not modeled
      OVFILL = 1.0
      FL=1
      SE1 = PARAM(1)
      ZETA1 = PARAM(2)
      ZETA2 = PARAM(3)
      BETA = PARAM(4)
      GAMMA=PARAM(5)
      DELTA=PARAM(6)
      PHI5 = PARAM(7)
      PowerDraw = PARAM(8)
C     9....Choice of liberation model. 0 = None, 1 = Ljubljana,  2 = Beta function
C     10...Choice of classification function. 0 = None, 1 = Rosin-Rammler, 2 =
C     Exponential sum, 3 = Logistic
      D50 = 0.001*PARAM(11)
      SI = PARAM(12)  !Sharpness index for classification function.

c Select the liberation model
      LJUBLJANA = .FALSE.
      AM_MODEL = .FALSE.
      IF(NINT(PARAM(9)) .EQ. 1) THEN
        LJUBLJANA = .TRUE.
        AM_MODEL = .FALSE.
      ELSE IF(NINT(PARAM(9)) .EQ. 2) THEN
        LJUBLJANA = .FALSE.
        AM_MODEL = .TRUE.
      END IF
      DER1 = 1.0
      PoverW = PARAM(8)/(3.60*TMSF)   !kW h/t

      NCALL=1
      TAU=0.0137*PoverW
      DTBDF=-TAU/TMSF
      CALL PMMILL(FEED,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

      NCALL=2
      TAU=0.2123*PoverW
      DTBDF=-TAU/TMSF
      CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

      NCALL=3
      TAU=0.7740*PoverW
      DTBDF=-TAU/TMSF
      TAUPRIME = TAU
      DER2 = OUT2
C If post classification is significant, iteration is required.
      ITNO = 0
  100 CONTINUE
      CALL PMMILL(DER2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,PPROP,
     *INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

      IF (NINT(PARAM(10)) .GT. 0) THEN
        WS = 0.0
        Recycle = 0.0
        DO I = 1,NDC
          CI = Classification(SIZE(I),NINT(PARAM(10)),D50,SI)
          DO J = 1,NGC
            DO K = 1,NSC
              Recycle = Recycle + CI*OUT1(I,J,K)
              DER2(I,J,K) = CI*OUT1(I,J,K) + OUT2(I,J,K)
              OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
              DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
            END DO
          END DO
        END DO
        NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
        ITNO = ITNO + 1
        WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
        TAUPRIME = NewTauPrime
        IF(WS .GT. 0.001 .AND. ITNO .LT. 30) GO TO 100
        IF(ITNO .GE. 30) THEN
          WRITE(UnitDiagFile,'(''***Post classification calculation did
     *not converge for unit '',I2/
     *    ''    Try increasing the D50 size of the grate.'')') NUNIT
          CALL FLUSH(UnitDiagFile)
        END IF
      END IF
      FL=1

      RETURN
      END


      FUNCTION Classification(Size,Kind,D50,SI)
c     *****************************************
C General clasification function
      USE GLOBALS
      REAL CLAMBDA

      X = Size/D50
      PF = 0.0
      IF(SI .LE. 0.0) THEN
        RETURN
      END IF
c Exponential-sum or Lynch model
      IF(Kind .EQ. 3)THEN
	CLAMBDA = 1.099*(1.0+SI)/(1.0-SI)
	WS = CLAMBDA*(X - 1.0)
        PF = 1.0
        IF(WS.LT.14.0) THEN
	  WS1 = EXP(-CLAMBDA)
          WS2 = EXP(WS)
          PF = (WS2 - WS1)/(WS2 - 2.0*WS1 + 1.0)
        END IF
      END IF

c Rossin-Rammler model
      IF(Kind .EQ. 2) THEN
	CLAMBDA=-1.5725/ALOG(SI)
        IF(X.LT.0.0) THEN
          WRITE(UnitDiagFile,1001)
 1001     FORMAT('Relative size is negative in call to post classifier')
          CALL FLUSH(UnitDiagFile)
          PF=0.0
          RETURN
        ELSE
	  WS=0.693*(X)**CLAMBDA
          IF(WS.LT.14.0) THEN
            PF=1.0-EXP(-WS)
          ELSE
            PF=1.0
          END IF
        END IF
      END IF

C Logistic model
      IF(Kind .EQ. 1) THEN
	CLAMBDA=-2.1972/ALOG(SI)
	PF=1.0/(1.0+(X**(-CLAMBDA)))
      END IF

      Classification = PF
      RETURN
      END

C
      SUBROUTINE HFSU(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
c Herbst-Fuerstenau mill with scale up based on calculated power draw.
C MODEL FOR A BALL MILL HAVING THREE PERFECTLY MIXED STAGES IN SERIES.
C NO CLASSIFICATION BETWEEN STAGES.
C No post classification at the grate.
C
C  MODEL IS BASED ON THE BREAKAGE AND SELECTION FUNCTION.
C  The Herbst-Fuerstenau correlation for the energy-specific selection
C  is used.
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(NDC),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL

      REAL MillLength,Jt,Jb,RhoB
      REAL No_Load_Power, Mill_PowerM, Mill_PowerA, Mill_PowerB
      REAL NoLoadPower,NetPower,UserPower

      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON/SCALEUP/OVFILL
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL
      COMMON/HERBST/SE1, ZETA1, ZETA2
      EXTERNAL HERBST_FUER
      COMMON/BREAKAGE_PARS/GAMMA,BETA,PHI5,DELTA
      EXTERNAL BR_FUNCTION

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      REAL D50, SI, NewTauPrime

c Calculate the water flows if water is added to the mill.
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

c Overfilling is not modeled
      OVFILL = 1.0
      FL=1
      SE1 = PARAM(1)
      ZETA1 = PARAM(2)
      ZETA2 = PARAM(3)
      BETA = PARAM(4)
      GAMMA=PARAM(5)
      DELTA=PARAM(6)
      PHI5 = PARAM(7)
      Diameter = PARAM(8)
      MillLength = PARAM(9)
      BallLoad = 0.01*PARAM(10)
      PhiC = 0.01*PARAM(11)
      Jt = BallLoad
      Jb = BallLoad
C     12...Choice of liberation model.
C          0 = None, 1 = Ljubljana,  2 = Beta function
c     13...Post classification function
c     14...Post classification d50, in mm
      D50 = PARAM(14)/1000
c     15...Post classification SI
      SI = PARAM(15)
C     16...Specific gravity of charge media
      RhoB = PARAM(16)*1000
C     17...Choice of power model
C          0 = None(user value), 1 = Bond, 2 = Morrell, 3 = Austin
C     18...User net grinding power in kW
C     19...Makeup ball size (mm) needed by Bond's power formula
C     20..Choice of Transport Model.  0 = pm;pm;pm-cl, 1 = pm;pm;pm;cl,
C	   2 = pm-cl;pm-cl;pm-cl
C     21..Fractional residence time, Region 1
C     22..Fractional residence time, Region 2
c Calculate the charge density
      WS = 0.0
      TOT = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TOT = TOT + FEED(I,J,K)
            WS = WS + FEED(I,J,K)/PPROP(J)
          END DO
        END DO
      END DO
      RhoO = 1000*TOT/WS
C      write(*,'(/'' Average density of the ore'',F8.1,'' kg/m**3'')')
C     * RhoO
      PhiV = (TMS1/RhoO)/(TMS1/RhoO+WTR1/1000.0)
      E = 0.40
      U = 1.0
      RhoC = Charge_Density(Jt, Jb, E, U, RhoB, RhoO, PhiV)
C      write(*,'('' Calculated density of the total mill charge'',F8.1,
C     *'' kg/m**3'')') RhoC

C Calculate the net power draw.
      CenterlineLength = MillLength
      BellyLength = Milllength
      Dm = Diameter
      ConeLength = 0.5*(CenterLineLength + BellyLength)
      NoLoadPower = No_Load_Power(BellyLength,Conelength,Dm,PhiC)
C      write(*,*) ' No-load power',NoLoadPower
      IF(NINT(PARAM(17)) .EQ. 0) THEN
	NetPower = PARAM(18)*1000
	ELSE IF(NINT(PARAM(17)) .EQ. 1) THEN
C     Bond. Need Db, makeup ball diameter
	Db = PARAM(19)/1000
	NetPower =  Mill_PowerB(BellyLength, Dm, PhiC, RhoB, Jt, Db) 
	ELSE IF(NINT(PARAM(17)) .EQ. 2) THEN
      NetPower = Mill_PowerM(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, 'Overflow')
C      write(*,*)' Morell power',NetPower
	ELSE 
C     This is Austin SAG mill power
      Dt = 0.1*Dm   !Dt is the feed opening.
      NetPower =  Mill_PowerA(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, Dt)
C      write(*,*)' Austin net power',NetPower
      END IF
      Ecs = NetPower/TMSF
C      write(*,'('' Specific power consumption'',F7.2,'' kW h/tonne'')')
C     * Ecs/3600.0

c Select the liberation model
      LJUBLJANA = .FALSE.
      AM_MODEL = .FALSE.
      IF(NINT(PARAM(12)) .EQ. 1) THEN
        LJUBLJANA = .TRUE.
        AM_MODEL = .FALSE.
      END IF
      IF(NINT(PARAM(12)) .EQ. 2) THEN
        LJUBLJANA = .FALSE.
        AM_MODEL = .TRUE.
      END IF
      DER1 = 1.0
      PowerW=NetPower/(3600*TMSF)    !Net power is in Watts

C	Set the fractional residence times
	FTAU1 = PARAM(21) !FTAU1 = 0.0137
	FTAU2 = PARAM(22) !FTAU2 = 0.2123
	FTAU3 = 1.0 - FTAU2 - FTAU1
c	Write(UnitDiagFile,*)' FTAU1,FTAU2,FTAU3',FTAU1,FTAU2,FTAU3

C Evaluate the product from the various transport models
C     NO CLASSIFICATION FUNCTION
      IF (NINT(PARAM(13)) .EQ. 0) THEN
	  NCALL=1
        TAU=FTAU1*PowerW
        DTBDF=-TAU/TMSF
        CALL PMMILL(FEED,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *  INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

        NCALL=2
        TAU=FTAU2*PowerW
        DTBDF=-TAU/TMSF
        CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *  INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

        NCALL=3
        TAU=FTAU3*PowerW
        DTBDF=-TAU/TMSF
        CALL PMMILL(OUT2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *  INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
      END IF
C	CLASSIFICATION FUNCTION
      IF (NINT(PARAM(13)) .GT. 0) THEN
C Tolerance and max iterations for internal classifier
	tolIC = 0.00005
	maxitIC = 120
C	LAST REGION WITH CLASSIFIER OR PM;PM;PM-CL
	IF (NINT(PARAM(20)) .EQ. 0) THEN
          NCALL=1
          TAU=FTAU1*PowerW
          DTBDF=-TAU/TMSF
          CALL PMMILL(FEED,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *    INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=2
          TAU=FTAU2*PowerW
          DTBDF=-TAU/TMSF
          CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *    INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=3
          TAU=FTAU3*PowerW
          DTBDF=-TAU/TMSF
C START CLASSIFIER LOOP
          TAUPRIME = TAU
          DER2 = OUT2
          ITNO = 0
  100 CONTINUE
          CALL PMMILL(DER2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(13)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT1(I,J,K)
                DER2(I,J,K) = CI*OUT1(I,J,K) + OUT2(I,J,K)
                OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 100
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C END IF FOR PM;PM;PM-CL
	  END IF
C	POST-CLASSIFIER
	  IF (NINT(PARAM(20)) .EQ. 1) THEN
C START CLASSIFIER LOOP
	    DER2 = FEED
	    TAUPRIME = PowerW
          ITNO = 0
  200 CONTINUE
          NCALL=1
          TAU=FTAU1*TAUPRIME
          DTBDF=-FTAU1*PowerW/TMSF
          CALL PMMILL(DER2,OUT3,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=2
          TAU=FTAU2*TAUPRIME
          DTBDF=-FTAU2*PowerW/TMSF
          CALL PMMILL(OUT3,OUT2,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)

          NCALL=3
          TAU=FTAU3*TAUPRIME
          DTBDF=-FTAU3*PowerW/TMSF
          CALL PMMILL(OUT2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP,
     *INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(13)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT1(I,J,K)
                DER2(I,J,K) = CI*OUT1(I,J,K) + FEED(I,J,K)
                OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = PowerW/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 200
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C END IF FOR POST-CLASSIFIER OR PM;PM;PM;CL
	  END IF
C EACH REGION FOLLOWED BY A CLASSIFIER OR PM-CL;PM-CL;PM-CL
	  IF (NINT(PARAM(20)) .EQ. 2) THEN
C START CLASSIFIER LOOP 1
	    DER2 = FEED
	    TAU = FTAU1*PowerW
	    DTBDF=-TAU/TMSF
	    TAUPRIME = TAU
          ITNO = 0
  301 CONTINUE
          CALL PMMILL(DER2,OUT3,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(13)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT3(I,J,K)
                DER2(I,J,K) = CI*OUT3(I,J,K) + FEED(I,J,K)
                OUT3(I,J,K) = (1.0 - CI)*OUT3(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 301
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2, ''  Region 1''/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C START CLASSIFIER LOOP 2
	    DER2 = OUT3
	    TAU = FTAU2*PowerW
	    DTBDF=-TAU/TMSF
	    TAUPRIME = TAU
          ITNO = 0
  302 CONTINUE
          CALL PMMILL(DER2,OUT2,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(13)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT2(I,J,K)
                DER2(I,J,K) = CI*OUT2(I,J,K) + OUT3(I,J,K)
                OUT2(I,J,K) = (1.0 - CI)*OUT2(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 302
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2, ''  Region 2''/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C START CLASSIFIER LOOP 3
	    DER2 = OUT2
	    TAU = FTAU3*PowerW
	    DTBDF=-TAU/TMSF
	    TAUPRIME = TAU
          ITNO = 0
  303 CONTINUE
          CALL PMMILL(DER2,OUT1,DER1,NDC,NGC,NSC,SIZE,TAUPRIME,DTBDF,
     *PPROP,INDPP,NPP,HERBST_FUER,BR_FUNCTION,GRDM,NGCM,NMIN,NCALL,DER3)
          WS = 0.0
          Recycle = 0.0
          DO I = 1,NDC
            CI = Classification(SIZE(I),NINT(PARAM(13)),D50,SI)
            DO J = 1,NGC
              DO K = 1,NSC
                Recycle = Recycle + CI*OUT1(I,J,K)
                DER2(I,J,K) = CI*OUT1(I,J,K) + OUT2(I,J,K)
                OUT1(I,J,K) = (1.0 - CI)*OUT1(I,J,K)
                DER1(I,J,K) = (1.0 - CI)*DER1(I,J,K)
              END DO
            END DO
          END DO
          NewTauPrime = TAU/(1.0 + RECYCLE/TMSF)
          ITNO = ITNO + 1
          WS = ABS((NewTauPrime - TAUPRIME)/TAUPRIME)
          TAUPRIME = NewTauPrime
          IF(WS .GT. tolIC .AND. ITNO .LT. maxitIC) GO TO 303
          IF(ITNO .GE. maxitIC) THEN
            WRITE(UnitDiagFile,'(''***Post classification calculation 
     *did not converge for unit '',I2, ''  Region 3''/
     *  ''    Try increasing the D50 size of the grate.'')') NUNIT
            CALL FLUSH(UnitDiagFile)
          END IF
C EACH REGION FOLLOWED BY A CLASSIFIER OR PM-CL;PM-CL;PM-CL
	  END IF
C END IF FOR CLASSIFIER
	END IF

      FL=1
      RETURN
      END
C
C
      SUBROUTINE PMMILL(FEED,OUT1,DER1,NDC,NGC,NSC,SIZE,TAU,DTBDF,PPROP
     *,INDPP,NPP,SELFN,BRFN,GRDM,NGCM,NMIN,NCALL,DER3)
C     *****************************************************************
C  Simulation of a perfectly mixed segment of a mill.
C  Liberation of the mineral phases is computed using the Andrews-Mika
C  model as developed by in King R P "Calculation of the Liberation Spectrum
C  in Products Produced in Continuous Milling  Circuits". Proc 7th European
C  Symposium on Comminution. Ljubljana June 1990 Vol 2 pp429-444. THIS LIMITS
C  THIS MODEL TO BINARY ORES!!
C
C  DER1 BRINGS IN THE DERIVATIVE OF THE FLOW IN I,J,K WITH RESPECT TO
C  THE FEED IN THIS CLASS TO THE MILL.
C  DER3 sends out the derivative of OUT1 wrt TAU.
C  DTBDF BRINGS IN THE DERIVATIVE OF TAU WITH RESPECT TO THE TOTAL FEED
C  NCALL IS THE NUMBER OF THIS CALL


      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),DER1(NDC,NGC,NSC)
      REAL DER3(NDC,NGC,NSC)
      REAL SIZE(NDC),PPROP(1)
      REAL GRDM(NGCM,NMIN)
      INTEGER INDPP(NPP,2),FL

      LOGICAL SWITCH
      COMMON /AMDSwitch/SWITCH
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL
      COMMON/SCALEUP/OVFILL

      
      OUT1 = FEED
      DO 60 K=1,NSC
      DO 50 I=1,NDC-1
        CALL SELFN(I,SIZE(I),S)
        S=OVFILL*S
        WS1=S*TAU
        DO 40 J=1,NGC
          OUT1(I,J,K)=OUT1(I,J,K)/(1.0+S*TAU)
          WS2=WS1*OUT1(I,J,K)
          DER1(I,J,K)=DER1(I,J,K)*(1.0-OUT1(I,J,K)*S*DTBDF)/(1.0+S*TAU)
	    BRF = 0.0
          DO 30 II=I+1,NDC
            SZII=SIZE(II)
            IF(II.EQ.NDC) SZII=0.0
            CALL BRFN(I,II,SIZE(II-1),SZII,SIZE(I),DELB)
            WS3=WS2*DELB
	      BRF = BRF + DELB
            DO 30 M=1,NGC
              BR=CONDBF(M,J,II,I,NDC,NGC,NOMIN,SWITCH)
              IF(BR .LE. -1) Then
	          Write(UnitDiagFile,*)
     *          'Error calculating conditional breakage function'
 	          UnitExitValue = 44
                RETURN
              END IF
              WS4=WS3*BR
              OUT1(II,M,K) = OUT1(II,M,K) + WS4
   30     CONTINUE
   40   CONTINUE
   50 CONTINUE
   60 CONTINUE
      RETURN

      END

      FUNCTION CONDBF(JJ,LL,II,KK,NDC,NGC,NOMIN,SWITCH)
C     *************************************************
C  Generates the conditional breakage function for transfer from G-class L
C  to G-class J while transferring from size class K to size class I.


      USE GLOBALS
      USE PORTLIB

      CHARACTER*80 DUMMY
      INTEGER ERR_ALLOCATION
      INTEGER*2, ALLOCATABLE, SAVE:: Bvg(:,:,:,:)
      LOGICAL SWITCH
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL

C
      IF(.NOT.SWITCH) THEN
        IF( ALLOCATED(Bvg)) DEALLOCATE(Bvg)
	  Write(UnitDiagFile,*)'Allocating Bvg NDC,NGC',NDC,NGC,NDC,NGC
        ALLOCATE (Bvg(NDC,NGC,NDC,NGC),STAT = ERR_ALLOCATION)
        IF(ERR_ALLOCATION .NE. 0 ) THEN
          WRITE(UnitDiagFile,*)
     *    'Error in allocation of memory for Andrews-Mika diagram'
           CALL FLUSH(UnitDiagFile)
          ExitValue = 38
          RETURN
        END IF

        LENG = LEN_TRIM(UnitJobPath)
C
C       Set up the Andrews-Mika model coefficients from the disk file
        IF(AM_MODEL) THEN
          WRITE(UnitDiagFile,'(''Reading data for AMD Beta model'')')
          CALL FLUSH(UnitDiagFile)
          OPEN(34, FILE =UnitJobPath(1:LENG)//'BETAAMD.DAT',ERR=9992)
          DO K = 1,NDC
            DO L = 1,NGC
              DO I = K+1,NDC
                READ(34,'(42I6)',END=9997) (Bvg(I,J,K,L),J = 1,NGC)
              END DO
              READ(34,*,END = 9997)
            END DO
          END DO
          CLOSE (34)
        END IF
c
c       Set up the Ljubljana model
        IF(LJUBLJANA) THEN
          WRITE(UnitDiagFile,*)'Reading data for AMD Ljubljana model'
          CALL FLUSH(UnitDiagFile)
          OPEN(33,FILE=UnitJobPath(1:LENG)//'LJUBAMD.DAT',ERR = 9993)
          DO K = 1,NDC
            DO L = 1,NGC
              DO I = K+1,NDC
                READ(33,'(42I6)',END=9999) (Bvg(I,J,K,L),J = 1,NGC)
              END DO
              READ(33,*,END = 9999)
            END DO
          END DO
	  CLOSE (33)
	  WRITE(UnitDiagfile,'(''AMD data has been read'')')
	  Call FLUSH(UnitDiagfile)
        END IF
        SWITCH = .TRUE.
      END IF
C
      CONDBF=0.0
      IF(NGC.LT.5 .OR..NOT.(LJUBLJANA .OR. AM_MODEL)) THEN
        IF(JJ .EQ. LL) CONDBF = 1.0
      ELSE
        CONDBF = 0.0001*Bvg(II,JJ,KK,LL)
      END IF
      RETURN

 9992 WRITE(UnitDiagFile,*)'Error opening file LJUBAMD.DAT'
      CALL FLUSH(UnitDiagFile)
      ExitValue = 38
      CONDBF = -2
      RETURN

 9993 WRITE(UnitDiagFile,*)'Error opening file LJUBAMD.DAT'
      CALL FLUSH(UnitDiagFile)
      ExitValue = 38
      CONDBF = -2
      RETURN

 9996 FORMAT  (/'The file BETAAMD.DAT which contains the tranfer coeffic
     *ients for'/' the "Beta function" liberation model has not been set
     * up correctly for this job.')
 9997 WRITE(UnitDiagFile,9996)
      CALL FLUSH(UnitDiagFile)
      CLOSE (34)
      CONDBF = -2
      RETURN

 9998 FORMAT  (/' The file LJUBAMD.DAT which contains the tranfer coeffi
     *cients for'/' the "Ljubljana" liberation model has not been set up
     * correctly for this job.')
 9999 WRITE(UnitDiagFile,9998)
      CALL FLUSH(UnitDiagFile)

      CLOSE (33)
      ExitValue = 38
      CONDBF = -2
      RETURN
      END


      SUBROUTINE JKPM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      DIMENSION C(25,25)
      SAVE C
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
C
C Model for the ball mill based on the JKMRC model which by the way sucks
C
C
      REAL KNOTX(5), KNOTY(5)
	REAL MillLength, MillDiameter, MillCS,  MillJ, MillWI, MillBall
	REAL TestMillDiameter, TestMillCS, TestMillJ
	REAL TestMillWI, TestMillK, TestMillBall
	FL=1
	DO I=1,5
	  KNOTX(I) = PARAM(I)
	  KNOTY(I) = PARAM(I+5)
	END DO
	MillDiameter = PARAM(11)
	MillLength = PARAM(12)
	MillCS = PARAM(13)/100.0
	MillJ = PARAM(14)/100.0
	MillWI = PARAM(15)
	MillBall = PARAM(16)/1000.0
	TestMillDiameter = PARAM(17)
	TestMillCS = PARAM(18)/100.0
	TestMillJ = PARAM(19)/100.0
	TestMillWI = PARAM(20)
	TestMillK = PARAM(21)
	TestMillBall = PARAM(22)/1000.0
	NP = Nint(PARAM(23))
	NK = Nint(PARAM(24))

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

C Make this a passthrough model for now
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            PROD(I,J,K) = FEED(I,J,K)
            DER1(I,J,K) = 1
          END DO
        END DO
      END DO
	

      RETURN
      END

      SUBROUTINE RODM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      DIMENSION C(25,25)
      SAVE C
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
C
C Model for the rod mill based on the linear transport model without
C axial mixing.
C
C
      REAL MU, LAMBDA
      V(J) = VelProfile(SIZE(J), PARAM(10))
      FL=1
      ALPHA=PARAM(1)
      BETA=PARAM(2)
      GAMMA=PARAM(3)
      DELTA=PARAM(4)
      A = PARAM(5)
      PHI1 = PARAM(6)
      TAU=PARAM(7)
      MU = 0.001*PARAM(8)
      LAMBDA = PARAM(9)
C

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

C Set up the C matrix
      DO I = 1,NDC
        C(I,I) = 1.0
      END DO
      DO I = 2,NDC
        DO L = 1,I-1
          SUM = 0.0
          DO J = L,I-1
            SJ = Austins(SIZE(J), A, ALPHA, MU, LAMBDA)/V(J)
            BIJ = AUSTINBR(SIZE(I-1)/SIZE(J),GAMMA,BETA,PHI1)
            IF(I .LT. NDC) THEN
              BIJ = BIJ - AUSTINBR(SIZE(I)/SIZE(J),GAMMA,BETA,PHI1)
            END IF
            SUM = SUM + BIJ*SJ*C(J,L)
          END DO
          SI = Austins(SIZE(I), A, ALPHA, MU, LAMBDA)/V(I)
          IF (I .EQ. NDC) SI = 0.0
          SL = Austins(SIZE(L), A, ALPHA, MU, LAMBDA)/V(L)
          C(I,L) = SUM/(SI - SL)
        END DO
      END DO
C Calculate the size distribution in the product
      DO N = 1,NSC
        DO M = 1,NGC
          DO I = 1,NDC
            SUM = 0.0
C           DER3 holds the coeffients A(I)
            DER3(I,M,N) = FEED(I,M,N)
            DO J = 1,I-1
              DER3(I,M,N) = DER3(I,M,N) - C(I,J)*DER3(J,M,N)
            END DO
            PROD(I,M,N) = 0.0
            DO J = 1,I
              SJ = Austins(SIZE(J), A, ALPHA, MU, LAMBDA)/V(J)
              IF (J .EQ. NDC) SJ = 0.0
              ALPHAIJ = C(I,J)*DER3(J,M,N)
              PROD(I,M,N) = PROD(I,M,N) +
     *                      C(I,J)*DER3(J,M,N)*EXP(-SJ*TAU)
            END DO
          END DO
        END DO
      END DO
      RETURN
      END

      FUNCTION VelProfile(dp, c)
c     **************************
c Calculates the longitudinal velocity as a function of size in a rod mill.
c The lowest velocity is 1/10 of the water velocity.
      VelProfile = 0.9*EXP(-c*dp*1000) + 0.1
      RETURN
      END

      SUBROUTINE RODL(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C Model for the rod mill based on the linear transport model without
C axial mixing.
C This version allows for the effect of liberation of the minerals.
C Only 2-component ores can be handled at present.
C tHE ALOGORITHM IS BASED ON THE NOTES FOR mete 567 AS MODIFIED TO
C HANDLE THE LIBERATION ASPECTS
C The parameters A(IJN) are held in array AA
      REAL B(25,25),ST(25)
      REAL AA(12,25,25)
      REAL MU, LAMBDA
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      LOGICAL SWITCH
      COMMON /AMDSwitch/SWITCH
      LOGICAL LJUBLJANA, AM_MODEL
      COMMON /LINCOR/PHIA,LJUBLJANA,AM_MODEL
      FL=1
      A = PARAM(1)
      ALPHA=PARAM(2)
      MU = 0.001*PARAM(3)
      LAMBDA = PARAM(4)
      BETA=PARAM(5)
      GAMMA=PARAM(6)
      DELTA=PARAM(7)
      PHI5 = PARAM(8)
      TAU=PARAM(9)
      LJUBLJANA = .FALSE.
      AM_MODEL = .FALSE.
	IF( NINT(PARAM(11)) .EQ. 0)THEN
        LJUBLJANA = .FALSE.
        AM_MODEL = .FALSE.
      END IF
      IF (PARAM(11) .EQ. 1) THEN
        LJUBLJANA = .TRUE.
        AM_MODEL = .FALSE.
      END IF
      IF (PARAM(11) .EQ. 2) THEN
        AM_MODEL = .TRUE.
        LJUBLJANA = .FALSE.
      END IF

c Calculate the water flows if water is added to the mill
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMSF*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

C Calculate the products S*T/v
      DO I = 1,NDC-1
        V = VelProfile(SIZE(I), PARAM(10))
        ST(I) = Austins(SIZE(I), A, ALPHA, MU, LAMBDA)  !/V
      END DO
      ST(NDC) = 0.0
C Calculate the size breakage function
      DO L = 1,NDC
        B(L,L) = 0.0
	  PHI1 = PHI5*(0.005/SIZE(L))**DELTA
        DO J = L+1,NDC
          B(J,L) = AUSTINBR(SIZE(J-1)/SIZE(L),GAMMA,BETA,PHI1)
          IF(J .LT. NDC) THEN
            B(J,L) =B(J,L) - AUSTINBR(SIZE(J)/SIZE(L),GAMMA,BETA,PHI1)
          END IF
        END DO
      END DO

C
C Set up the AA coefficients
      DO IS = 1,NSC
        DO J = 1,NDC
          DO N = 1,J
            DO I = 1,NGC
              SUM = 0.0
              DO L = N,J-1
                WS = 0.0
                DO K = 1,NGC
                  BR=CONDBF(I,K,J,L,NDC,NGC,NOMIN,SWITCH)
	            IF (BR .LT. -1) Then
	              WRITE(UnitDiagFile,*)
     *              'Error calculating transfer coefficients'
	              UnitExitValue = 44
	            END IF
                  WS = WS + BR*AA(K,L,N)
                END DO
                SUM = SUM + WS*B(J,L)*ST(L)
              END DO
              IF(N .LT. J) AA(I,J,N) = SUM/(ST(J) - ST(N))
              IF(N .EQ. J) THEN
                AA(I,J,J) = FEED(J,I,IS)
                DO L = 1,J-1
                  AA(I,J,J) = AA(I,J,J) - AA(I,J,L)
                END DO
              END IF
            END DO
          END DO
        END DO
C Calculate the size distribution in the product
        DO I = 1,NGC
          DO J = 1,NDC
            PROD(J,I,IS) = 0.0
            DO L = 1,J
              PROD(J,I,IS) = PROD(J,I,IS) +
     *                      AA(I,J,L)*EXP(-ST(L)*TAU)
            END DO
          END DO
        END DO
      END DO

c Normalise the amounts in the two extreme classes to get the overall
c balance correct.  This is necessary because of the fairly broad
c grade classes.
c Calculate the grade in each size fraction also.
      TMS1=0.0
      FEEDMIN1 = 0.0
      FEEDMIN2 = 0.0
      OUTMIN1 = 0.0
      OUTMIN2 = 0.0
      DO I = 1,NDC
        OUTSIZE = 0.0
        GRADE1 = 0.0
        GRADE2 = 0.0
        DO J = 1,NGC
          DO K = 1,NSC
            FEEDMIN1 = FEEDMIN1 + FEED(I,J,K)*GRDM(J,1)
            FEEDMIN2 = FEEDMIN2 + FEED(I,J,K)*GRDM(J,2)
            OUTSIZE = OUTSIZE + PROD(I,J,K)
            GRADE1 = GRADE1 + PROD(I,J,K)*GRDM(J,1)
            GRADE2 = GRADE2 + PROD(I,J,K)*GRDM(J,2)
          END DO
        END DO
        OUTMIN1 = OUTMIN1 + GRADE1
        OUTMIN2 = OUTMIN2 + GRADE2
        IF(OUTSIZE.GT.0.0) THEN
          GRADE1 = GRADE1/OUTSIZE
          GRADE2 = GRADE2/OUTSIZE
        END IF
        TMS1 = TMS1 + OUTSIZE
      END DO
      CORRECTION1 = FEEDMIN1 - OUTMIN1
      CORRECTION2 = FEEDMIN2 - OUTMIN2
      PROD(NDC,1,1) = PROD(NDC,1,1) + (FEEDMIN1 - OUTMIN1)
      PROD(NDC,NGC,1) = PROD(NDC,NGC,1) - (FEEDMIN1 - OUTMIN1)
      IF (PROD(NDC,1,1) .LT. 0.0) THEN
        PROD(NDC-1,1,1) = PROD(NDC-1,1,1) + PROD(NDC,1,1)
        PROD(NDC,1,1) = 0.0
      END IF
      IF (PROD(NDC,NGC,1) .LT. 0.0) THEN
        PROD(NDC-1,NGC,1) = PROD(NDC-1,NGC,1) + PROD(NDC,NGC,1)
        PROD(NDC,NGC,1) = 0.0
      END IF
      RETURN
      END

      SUBROUTINE FAGM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      INTEGER FL,INDPP(NPP,2)

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

C Model for the FAG mill based on the Hoyer-Austin population balance
C formulation.
      REAL FMU, FLAMBDA, KAPPA
      REAL LAMBDAa
      REAL LOAD_VOL

      CLASSI(I) = ALOG(SIZE(I)/GRATE_SIZE)/ALOG(0.001/GRATE_SIZE)

      FL=10
      ALPHA=PARAM(1)
      BETA=PARAM(2)
      GAMMA=PARAM(3)
      DELTA=PARAM(4)
      A = PARAM(5)/60.0
      PHI5 = PARAM(6)
      TAU = 60*PARAM(17)
      FMU = 0.001*PARAM(7)
      FLAMBDA = PARAM(8)
      AJK = 0.01*PARAM(9)
      BJK = PARAM(10)
      DA = 0.001*PARAM(11)
      TA = PARAM(12)
      DEL = 1.0
      KAPPA = 0.0457**(1-DEL)*0.1*TA/(3*600.0)
      LAMBDAa = 0.67
      DM = PARAM(13)
      LOAD_VOL = 0.01*PARAM(14)
      PHIC = 0.01*PARAM(15)
      GRATE_SIZE = 0.001*PARAM(16)
C     Particle breakage energy for large particles
	Eminf = PARAM(18)
	Dp0 = 0.001*PARAM(19)
	Phi = PARAM(20)
C

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

C Calculate the size distribution in the product
      SIGMAI = 0.957
      TAU_PRIME = TAU
      RECYCLEM = 0.0
      NTRIALS = 0
   10 CONTINUE
      RECYCLE = 0.0
      DO N = 1,NSC
        DO M = 1,NGC
          DO I = 1,NDC
            IF(I .EQ. NDC) THEN
              SI = 0.0
              SELI = 0.0
              GAMMAI = 0.0
            ELSE
	      SI = Austins(SIZE(I),A,ALPHA,FMU,FLAMBDA)
              SELI = SELF_BR_SEL(DM,PHIC,LOAD_VOL,SIZE(I),Eminf,Dp0,Phi)
              GAMMAI = 3.0*KAPPA/(SIZE(I)**(1-DEL))
            END IF
            ZETAI = SIGMAI*GAMMAI*TAU_PRIME
            XII = (SI + SELI + GAMMAI)*TAU_PRIME
!	IF(RECYCLEM .LE. 0.0) then
!	write(13,*)'SI,SELI,GAMMAI',SI,SELI,GAMMAI
!	End if
            IF(SIZE(I) .GT. GRATE_SIZE) THEN
              CI = 1.0
            ELSE IF(SIZE(I) .GT. 0.001) THEN
              CI = 1.0 - CLASSI(I)
            ELSE
              CI = 0.0
            END IF
            OUT2(I,M,N) = FEED(I,M,N)
            DO J = 1,I-1
              AIJ = ATTRITION_BR(MeshSizes(I-1),MeshSizes(J),DA,LAMBDAa)
              BIJ = AUSTINBR(SIZE(I-1)/SIZE(J),GAMMA,BETA,PHI5)
              CIJ = SELF_BR(SIZE(J)/SIZE(I-1),SIZE(J),AJK,BJK,DM)
              IF(I .LT. NDC) THEN
                AIJ = AIJ -
     *           ATTRITION_BR(MeshSizes(I),Meshsizes(J),DA,LAMBDAa)
                BIJ = BIJ - AUSTINBR(SIZE(I)/SIZE(J),GAMMA,BETA,PHI5)
                CIJ = CIJ -
     *           SELF_BR(SIZE(J)/SIZE(I),SIZE(J),AJK,BJK,DM)
              END IF
	        SJ = Austins(SIZE(J), A, ALPHA, FMU, FLAMBDA)
              SELJ = SELF_BR_SEL(DM,PHIC,LOAD_VOL,SIZE(J),Eminf,Dp0,Phi)
              GAMMAJ = 3.0*KAPPA/(SIZE(J)**(1-DEL))
              DELIJ = (GAMMAJ*AIJ + SJ*BIJ + SELJ*CIJ)*TAU_PRIME
              OUT2(I,M,N) = OUT2(I,M,N) + DELIJ*OUT2(J,M,N)
            END DO
            IF(I .GT. 1)OUT2(I,M,N) = OUT2(I,M,N) + ZETAIM*OUT2(I-1,M,N)
            OUT2(I,M,N) = OUT2(I,M,N)/(1.0 + ZETAI + XII - CI)
            ZETAIM = ZETAI
            RECYCLE = RECYCLE + CI*OUT2(I,M,N)
            PROD(I,M,N) = (1.0 - CI)*OUT2(I,M,N)
          END DO

        END DO
      END DO
	Write(13,*)'Recycle',RECYCLE
      IF(ABS((RECYCLE-RECYCLEM)/RECYCLE) .GT. 0.01) THEN
        TAU_PRIME = TAU*TMSF/(TMSF+RECYCLE)
        RECYCLEM = RECYCLE
        NTRIALS = NTRIALS + 1
        IF(NTRIALS .LT. 20) GO TO 10
        WRITE(UnitDiagfile,*)' Convergence not reached in model FAGM',
     *  ' while calculating the internal recycle'
        WRITE(Unitdiagfile,*)'   Try increasing the grate aperture'
        CALL FLUSH(UnitDiagFile)
        UnitExitValue = 38
      END IF
      TMS2 = TMSF + RECYCLE
      RETURN
      END

      SUBROUTINE FAGT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
!     FAG mill with trommel.
      !Parameter 21 is the trommel mesh size
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

	Real,ALLOCATABLE :: PROD(:,:,:)

      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      IF(.NOT. ALLOCATED(PROD)) ALLOCATE(PROD(NDC,NGC,NSC))
      CALL FAGM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)

	OUT3 = OUT2
	TMS3 = TMS2
	WTR3 = WTR2
	TMS1 = 0.0
	TMS2 = 0.0
	!write(13,*)'TMSF',TMSF
	!write(13,*)'SUM(FEED),SUM(PROD)',SUM(FEED),SUM(PROD)
      FL = 20   ! pseudo stream is allowed

      CLAMBDA = 6.9 ! 4.8
	Do I = 1,NDC
	  X = SIZE(I)/(0.001*PARAM(21))
	  WS=0.693*(X)**CLAMBDA
        IF(WS.LT.14.0) THEN
          PF2=1.0-EXP(-WS)
        ELSE
          PF2=1.0
        END IF
	  Do J = 1,NGC
	    Do K = 1,NSC
	      OUT2(I,J,K) = PF2*PROD(I,J,K)
	      OUT1(I,J,K) = (1.0 - PF2)*PROD(I,J,K)
	      TMS1 = TMS1 + OUT1(I,J,K)
	      TMS2 = TMS2 + OUT2(I,J,K)
	    End Do
	  End Do
	End Do
	WTR2 = 5.0*TMS2/95.0
	WTR1 = WTR + WATERADD - WTR2
	!write(13,*)'WTR1,WTR2',WTR1,WTR2
	!write(13,*)'SUM(OUT1),SUM(OUT2)',SUM(OUT1),SUM(OUT2)

      END SUBROUTINE

      SUBROUTINE SAGT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      !Parameter 25 is trommel mesh size
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

	Real,ALLOCATABLE :: PROD(:,:,:)

      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      IF(.NOT. ALLOCATED(PROD)) ALLOCATE(PROD(NDC,NGC,NSC))

	write(13,*)'TMSF',TMSF
	write(13,*)'SUM(FEED)',SUM(FEED)

      CALL SAGM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)

	OUT3 = OUT2
	TMS3 = TMS2
	WTR3 = WTR2
	TMS1 = 0.0
	TMS2 = 0.0
	FL = 20  !pseudo stream is allowed

      CLAMBDA = 6.9
	Do I = 1,NDC
	  X = Size(I)/(0.001*PARAM(25))
	  WS=0.693*(X)**CLAMBDA
        IF(WS.LT.14.0) THEN
          PF2=1.0-EXP(-WS)
        ELSE
          PF2=1.0
        END IF
	  Do J = 1,NGC
	    Do K = 1,NSC
	      OUT2(I,J,K) = PF2*PROD(I,J,K)
	      TMS2 = TMS2 + OUT2(I,J,K)
	      OUT1(I,J,K) = (1.0 - PF2)*PROD(I,J,K)
	      TMS1 = TMS1 + OUT1(I,J,K)
	    End Do
	  End Do
	End Do
	WTR2 = 5.0*TMS2/95.0
      WTR1 = WTR + WATERADD - WTR2
	write(13,*)'WTR1,WTR2',WTR1,WTR2
	write(13,*)'SUM(OUT1),SUM(OUT2)',SUM(OUT1),SUM(OUT2)
	write(13,*)'TMS1,TMS2',TMS1,TMS2

      END SUBROUTINE


      SUBROUTINE SAGM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C Model for the SAG mill based on the Hoyer-Austin population balance
C formulation.
      PARAMETER (Pi = 3.1415927)
      REAL MuT, SLambda, KAPPA
      REAL LAMBDAa
      REAL Jt,Jb


      REAL No_Load_Power, Mill_PowerM, Mill_PowerA
      REAL NoLoadPower,NetPower
      REAL MillLoad

      REAL, ALLOCATABLE::MEDIA(:,:,:)
      REAL, ALLOCATABLE::Zeta(:),GammaI(:),Self(:),Sel(:),Classi(:)
      REAL, ALLOCATABLE::SelfBreakageFunction(:)



      IF(.NOT. ALLOCATED(MEDIA)) ALLOCATE(MEDIA(NDC,NGC,NSC))
      IF(.NOT. ALLOCATED(Zeta)) ALLOCATE(Zeta(0:NDC))
      IF(.NOT. ALLOCATED(GammaI)) ALLOCATE(GammaI(NDC))
      IF(.NOT. ALLOCATED(Self)) ALLOCATE(Self(NDC))
      IF(.NOT. ALLOCATED(Sel)) ALLOCATE(Sel(NDC))
      IF(.NOT. ALLOCATED(Classi)) ALLOCATE(Classi(NDC))
      IF(.NOT. ALLOCATED(SelfBreakageFunction)) Then
        ALLOCATE(SelfBreakageFunction(NDC))
      END IF
c      write(UnitDiagFile,*)' Starting SAGM'
c      response = COMMITQQ(DiagFile)

      FL=10  !To signal a pseudo stream
      A = PARAM(1)/60.0
      Alpha = PARAM(2)
      MuT = 0.001*PARAM(3)
      SLambda = PARAM(4)
      AJK = 0.01*PARAM(5)
      BJK = PARAM(6)
      DA = 0.001*PARAM(7)
      TA = PARAM(8)
      DEL = 1.0   !Goldman and Barbery value.  See equation 5.154
      KAPPA = 0.0457**(1-DEL)*0.1*TA/(3*600.0) ! Equation 5.159
      LAMBDAa = 0.67
      DmT = PARAM(9)
      FillT = 0.01*PARAM(10)
      PhiCT = 0.01*PARAM(11)
      DbT = 0.001*PARAM(12)
      Dm = PARAM(13)
      CenterLineLength = PARAM(14)
      BellyLength = PARAM(15)
      Dt = PARAM(16)
      Jt = 0.01*PARAM(17)
	Fill = Jt
      Jb = 0.01*PARAM(18)
      BallSize = 0.001*PARAM(19)
      PhiC = 0.01*PARAM(20)
      GrateSize = 0.001*PARAM(21)
	Eminf = PARAM(22)
	Dp0 = 0.001*PARAM(23)
	Phi = PARAM(24)

      E = 0.40
      Um = 1.0
	UT = 1.0

      RhoB = 7800
      OUT2 = FEED

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

c Calculate the charge density
      WS = 0.0
      TOT = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TOT = TOT + OUT2(I,J,K)
            WS = WS + OUT2(I,J,K)/PPROP(J)
          END DO
        END DO
      END DO
      RhoO = 1000*TOT/WS
c      write(13,'(/'' Average density of the ore'',F8.1,'' kg/m**3'')')
c     * RhoO
      PhiV = TMS1/RhoO/(TMS1/RhoO+WTR1/1000.0)
      RhoC = Charge_Density(Jt, Jb, E, U, RhoB, RhoO, PhiV)
c      write(13,'('' Calculated density of the total mill charge'',F8.1,
c     *'' kg/m**3'')') RhoC

C Calculate the net power draw.
      ConeLength = 0.5*(CenterLineLength + BellyLength)
      NoLoadPower = No_Load_Power(BellyLength,Conelength,Dm,PhiC)
      write(UnitDiagFile,'('' No-load power'',T20,F9.1,'' W'')')
     *NoLoadPower
      NetPower = Mill_PowerM(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, 'Grate')
      write(UnitDiagFile,'('' Morell power'',T20,F9.1,'' W'')')NetPower
      !Dt is the feed opening
      NetPower =  Mill_PowerA(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, Dt)
      write(UnitDiagFile,'('' Austin net power'',T20,F9.1,'' W'')')
     *NetPower
      Ecs = NetPower/TMSF
      write(UnitDiagFile,'('' Specific power consumption'',F7.2,
     *'' kW hr/tonne'')') Ecs/3600

c Calculate Austin's scale up parameters for the selection function
      DELTA = 0.0
      IF(Dm .GE. 3.81) DELTA = 0.2
      C2 = ((Dm/DmT)**0.5)*((3.81/Dm)**DELTA)
      C3 = (1.0 + 6.6*(FillT**2.3))/(1.0 + 6.6*(Fill**2.3))
      C4 = (PhiC-0.1)/(PhiCT-0.1)
      C4 = C4*(1.0+EXP(15.7*(PhiCT-0.94)))/(1.0+EXP(15.7*(PhiC-0.94)))
	C5 = exp(-1.34*(Um - UT))

c Calculate the total internal volume of the mill.
      WS = CenterLineLength/BellyLength
      Vm = 0.25*Pi*Dm*Dm*BellyLength
      Vm = Vm*(1.0 + 0.6667*(WS - 1)*(1-(Dt/Dm)**3)/(1-Dt/Dm))

c Calculate the load in the mill
      VolumeOfSlurry = JT*U*E
      VolumeOfMedia = (JT - JB)*(1.0 - E)
      MillLoad = (VolumeOfSlurry*PhiV + VolumeOfMedia)*RhoO*Vm
c      write(UnitDiagFile,'('' Total mill volume'',F7.2,'' m**3''
c     */'' Mill load excluding balls'',F7.2,'' tonnes'')')
c     * Vm,0.001*MillLoad

c  Calculate the average residence time.
      TAU = MillLoad/TMSF
c      write(UnitDiagFile,'('' Calculated residence time in the mill''
c     *,F7.2,'' mins'')')TAU/60.0


C Calculate the size distribution in the product
      SIGMAI = 0.957  !1.353 ! Equation 5.107
      Zeta(0) = 0.0
      DO I = 1,NDC
c        IF(SIZE(I) .GT. GrateSize)  THEN
c          Classi(I) = 1.0
c        ELSE IF (SIZE(I) .GT. 0.001) THEN
c          Classi(I) = 1 - ALOG(SIZE(I)/GrateSize)/ALOG(0.001/Gratesize)
c        ELSE
c          Classi(I) = 0.0
c        END IF
       Classi(I) = 1 - exp(-0.693*(Size(I)/GrateSize)**6)
        IF(I .EQ. NDC) THEN
          Self(I) = 0.0
          GammaI(I) = 0.0
        ELSE
          Self(I) = SELF_BR_SEL(DM,PHIC,Fill,SIZE(I),Eminf,Dp0,Phi)
          GammaI(I) = 3.0*KAPPA/(SIZE(I)**(1-DEL)) !Equations 5.106 & 2.108
        END IF
      END DO
c	Write(UnitDiagFile,*)'Self breakage function'
c      DO I = 2,NDC
c        SelfBreakageFunction(I) =
c     *  SELF_BR(SIZE(1)/SIZE(I-1),SIZE(1),AJK,BJK,DM)
c        WRITE(UnitDiagFile,*) SIZE(I-1),SelfBreakageFunction(I)
c      END DO
      TAU_PRIME = TAU
      RECYCLEM = 0.0
      NTRIALS = 0

c Calculate the charge density.

   10 CONTINUE

      RECYCLE = 0.0
      CALL MEDIUM(SIZE,OUT2,MEDIA,NDC,NGC,NSC,TOTMEDIA,GrateSize)
      DO N = 1,NSC
        DO M = 1,NGC
          DR = PPROP(M)*1000/RhoB
          DO I = 1,NDC
            IF(I .EQ. NDC) THEN
              Sel(I) = 0.0
            ELSE
              dpi = SIZE(I)
              WS1 = AustinSelMedia(dpi,SIZE,MEDIA,NDC,NGC,NSC,
     *		    MuT*DR,SLambda,Dm,DmT,DbT,GrateSize,A,Alpha)
              WS2 = AustinSelSteel(dpi,BallSize,DbT,MuT,
     *		    SLambda,Dm,DmT,A,Alpha)
              Sel(I) = C2*C3*C4*C5*((Jt - Jb)*WS1*DR/Jt + Jb*WS2/Jt)
            END IF
            Zeta(I) = SIGMAI*GammaI(I)*TAU_PRIME        ! Equation 5.110
            XII = (Sel(I) + Self(I) + GammaI(I))*TAU_PRIME  ! Eqs. 5.111 & 5.126

            OUT2(I,M,N) = FEED(I,M,N)
            DO J = 1,I-1
              AIJ = ATTRITION_BR(MeshSizes(I-1),MeshSizes(J),DA,LAMBDAa)
              BETA = 3.0
              GAMMA = 0.6
              PHI5 = 0.4
              !BIJ = AUSTINBR(SIZE(I-1)/SIZE(J),GAMMA,BETA,PHI5)
	        BIJ = EnSpBrFunc(SIZE(J)/SIZE(I-1),SIZE(J),AJK,BJK,Ecs)
              CIJ = SELF_BR(SIZE(J)/SIZE(I-1),SIZE(J),AJK,BJK,DM)
              IF(I .LT. NDC) THEN
                AIJ = AIJ -
     *           ATTRITION_BR(MeshSizes(I),MeshSizes(J),DA,LAMBDAa)
                !BIJ = BIJ - AUSTINBR(SIZE(I)/SIZE(J),GAMMA,BETA,PHI5)
	          BIJ = BIJ - 
     *           EnSpBrFunc(SIZE(J)/SIZE(I),SIZE(J),AJK,BJK,Ecs)
                CIJ = CIJ -
     *           SELF_BR(SIZE(J)/SIZE(I),SIZE(J),AJK,BJK,DM)
              END IF
              DELIJ = (GammaI(J)*AIJ + Sel(J)*BIJ + Self(J)*CIJ)
              DELIJ = DELIJ*TAU_PRIME                   !Equation 5.112
              OUT2(I,M,N) = OUT2(I,M,N) + DELIJ*OUT2(J,M,N)
            END DO
            CI = Classi(I)
            IF(I .GT. 1) Then
              OUT2(I,M,N) = OUT2(I,M,N) + Zeta(I-1)*OUT2(I-1,M,N) 
            END IF
            OUT2(I,M,N) = OUT2(I,M,N)/(1.0 + Zeta(I) + XII - CI)! Equation 5.124
            RECYCLE = RECYCLE + CI*OUT2(I,M,N)
            PROD(I,M,N) = (1.0 - CI)*OUT2(I,M,N)    !Equation 5.126
          END DO
        END DO
      END DO
      IF(ABS((RECYCLE-RECYCLEM)/RECYCLE) .GT. 0.0001) THEN
        Flow = TMSF + RECYCLEM + (RECYCLE - RECYCLEM)
        TAU_PRIME = TAU*TMSF/Flow
        WRITE(UnitDiagFile,*)' TAU_PRIME,RECYCLE', TAU_PRIME,RECYCLE
        RECYCLEM = RECYCLE
        NTRIALS = NTRIALS + 1
        IF(NTRIALS .LT. 200) GO TO 10
        WRITE(UnitDiagfile,*)' Convergence not reached in model SAGM',
     *  ' while calculating the internal recycle'
        WRITE(Unitdiagfile,*)'   Try increasing the grate aperture'
        CALL FLUSH(UnitDiagFile)
        IF(IFLAG .EQ. 1) UnitExitValue = 38
      END IF
	N = UnitDiagFile
c	Write(UnitDiagFile,*)'A, Eminf',A, Eminf
c      WRITE(N,*)'      Size           Sel            Self          Sum'
c      DO I = 1,NDC
c        WRITE(UnitDiagFile,*)Size(I),Sel(I),Self(I),SEL(I)+Self(I)
c      END DO
      TMS2 = TMSF + RECYCLE
c      write(UnitDiagFile,*)'Sum prod,TMS1',SUM(PROD),TMS1

      DEALLOCATE(MEDIA)
      DEALLOCATE(Zeta,GammaI,Self,Sel,Classi)
      DEALLOCATE(SelfBreakageFunction)
      RETURN
      END

      SUBROUTINE SAGL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      !Parameter 27 is trommel mesh size
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

	Real,ALLOCATABLE :: PROD(:,:,:)

      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      IF(.NOT. ALLOCATED(PROD)) ALLOCATE(PROD(NDC,NGC,NSC))

	write(13,*)'TMSF',TMSF
	write(13,*)'SUM(FEED)',SUM(FEED)

C	Param(17) has the initial value of Jt
	! Initialize with these values and check if convergence has been reached
      CALL SAG_CLAUDIO(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
	
	IF (TMS2 .EQ. 0.0) Goto 110
	IF(NINT(PARAM(28)) .EQ. 1) Goto 110

	NTRIALS = 0
  100 CONTINUE
	NTRIALS = NTRIALS + 1
	RECYCLE = TMS2/TMSF
	IF(RECYCLE .GT. 10.05) Then 
	PARAM(17) = PARAM(17) + 1
	CALL SAG_CLAUDIO(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
	RECYCLEM = TMS2/TMSF
	IF(RECYCLEM .LT. 9.95) Goto 110
	EndIF
	IF(RECYCLE .LT. 9.95) Then 
	PARAM(17) = PARAM(17) - 1
	CALL SAG_CLAUDIO(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
	RECYCLEM = TMS2/TMSF
	IF(RECYCLEM .GT. 9.05) Goto 110
	EndIf
	IF(ABS(RECYCLE - 10.0) .LT. 0.05) Goto 110
	IF(NTRIALS .GT. 40) Goto 110
	Goto 100

  110	CONTINUE

	
	OUT3 = OUT2
	TMS3 = TMS2
	WTR3 = WTR2
	TMS1 = 0.0
	TMS2 = 0.0
	FL = 20  !pseudo stream is allowed

      CLAMBDA = 6.9
	Do I = 1,NDC
	  X = Size(I)/(0.001*PARAM(27))
	  WS=0.693*(X)**CLAMBDA
        IF(WS.LT.14.0) THEN
          PF2=1.0-EXP(-WS)
        ELSE
          PF2=1.0
        END IF
	  Do J = 1,NGC
	    Do K = 1,NSC
	      OUT2(I,J,K) = PF2*PROD(I,J,K)
	      TMS2 = TMS2 + OUT2(I,J,K)
	      OUT1(I,J,K) = (1.0 - PF2)*PROD(I,J,K)
	      TMS1 = TMS1 + OUT1(I,J,K)
	    End Do
	  End Do
	End Do
C  Water split in trommel
	WTR2 = 5.0*TMS2/95.0
      WTR1 = WTR + WATERADD - WTR2
	write(13,*)'WTR1,WTR2',WTR1,WTR2
	write(13,*)'SUM(OUT1),SUM(OUT2)',SUM(OUT1),SUM(OUT2)
	write(13,*)'TMS1,TMS2',TMS1,TMS2

      END SUBROUTINE

      SUBROUTINE SAGP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

	Real,ALLOCATABLE :: PROD(:,:,:)

      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      IF(.NOT. ALLOCATED(PROD)) ALLOCATE(PROD(NDC,NGC,NSC))

	write(13,*)'TMSF',TMSF
	write(13,*)'SUM(FEED)',SUM(FEED)

C	Param(17) has the initial value of Jt
	! Initialize with these values and check if convergence has been reached
      CALL SAG_CLAUDIO(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)

	IF (TMS2 .EQ. 0.0) Goto 110
	IF(NINT(PARAM(27)) .EQ. 1) Goto 110

	NTRIALS = 0
  100 CONTINUE
	NTRIALS = NTRIALS + 1
	RECYCLE = TMS2/TMSF
	IF(RECYCLE .GT. 5.05) Then 
	PARAM(17) = PARAM(17) + 1
	CALL SAG_CLAUDIO(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
	RECYCLEM = TMS2/TMSF
	IF(RECYCLEM .LT. 4.95) Goto 110
	EndIF
	IF(RECYCLE .LT. 4.95) Then 
	PARAM(17) = PARAM(17) - 1
	CALL SAG_CLAUDIO(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
	RECYCLEM = TMS2/TMSF
	IF(RECYCLEM .GT. 5.05) Goto 110
	EndIf
	IF(ABS(RECYCLE - 5.0) .LT. 0.05) Goto 110
	IF(NTRIALS .GT. 40) Goto 110
	Goto 100

  110	CONTINUE
C	
	
	OUT3 = OUT2
	TMS3 = TMS2
	WTR3 = WTR2
c TMS1 = 0.0
c TMS2 = 0.0
	FL = 20  !pseudo stream is allowed
	OUT1 = PROD
	write(13,*)'WTR1,WTR2',WTR1,WTR2
	write(13,*)'SUM(OUT1),SUM(OUT2)',SUM(OUT1),SUM(OUT2)
	write(13,*)'TMS1,TMS2',TMS1,TMS2

      END SUBROUTINE

      SUBROUTINE SAG_CLAUDIO(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,
     *INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
     
	USE GLOBALS
C	IMPLICIT NONE
	INTEGER NDC, NGC, NSC, NGCM, NMIN, NPP, I, J, K, NTRIALS, N, M
	INTEGER IFLAG, NPLNT, NUNIT, ITER, IW
	REAL Pi, A, Alpha, AJK, BJK, DA, TA, DEL, DmT, FillT,PhiCT,DbT,Dm
	REAL CenterLineLength,BellyLength, Dt,BallSize, PhiC, Fill
	REAL GrateSize, Eminf, Dp0, Phi, E, Um, UT, RhoB, TMS1, TMSF, RhoO
	REAL PhiV, RhoC, ConeLength, Ecs, DELTA, C2, C3, C4, C5, Vm
	REAL VolumeOfSlurry, VolumeOfMedia, TAU, SIGMAI, Xpass, Cg, Cp
	REAL SELF_BR_SEL, Charge_Density, TAU_PRIME, RECYCLEM, RECYCLE
	REAL TOTMEDIA, DR, dpi, WS1, WS2, AustinSelMedia, AustinSelSteel
	REAL XII, AIJ, ATTRITION_BR, BETA, GAMMA, PHI5, BIJ, EnSpBrFunc
	REAL CIJ, SELF_BR, DELIJ, CI, Flow, TMS2, TMS3, XOA
	REAL WTR1, WTR, WTR2, WTR3, SOLIDCONT, WATERADD, WS, TOT

      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
	

      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C Model for the SAG mill based on the Hoyer-Austin population balance
C formulation.
      PARAMETER (Pi = 3.1415927)
      REAL MuT, SLambda, KAPPA
      REAL LAMBDAa
      REAL Jt,Jb

	REAL PPortGrateOARatio, PPortSize


      REAL No_Load_Power, Mill_PowerM, Mill_PowerA
      REAL NoLoadPower,NetPower
      REAL MillLoad

      REAL, ALLOCATABLE::MEDIA(:,:,:)
      REAL, ALLOCATABLE::Zeta(:),GammaI(:),Self(:),Sel(:),Classi(:)
      REAL, ALLOCATABLE::SelfBreakageFunction(:)



      IF(.NOT. ALLOCATED(MEDIA)) ALLOCATE(MEDIA(NDC,NGC,NSC))
      IF(.NOT. ALLOCATED(Zeta)) ALLOCATE(Zeta(0:NDC))
      IF(.NOT. ALLOCATED(GammaI)) ALLOCATE(GammaI(NDC))
      IF(.NOT. ALLOCATED(Self)) ALLOCATE(Self(NDC))
      IF(.NOT. ALLOCATED(Sel)) ALLOCATE(Sel(NDC))
      IF(.NOT. ALLOCATED(Classi)) ALLOCATE(Classi(NDC))
      IF(.NOT. ALLOCATED(SelfBreakageFunction)) Then
        ALLOCATE(SelfBreakageFunction(NDC))
      END IF
c      write(UnitDiagFile,*)' Starting SAGM'
c      response = COMMITQQ(DiagFile)

      FL=10  !To signal a pseudo stream
      A = PARAM(1)/60.0
      Alpha = PARAM(2)
      MuT = 0.001*PARAM(3)
      SLambda = PARAM(4)
      AJK = 0.01*PARAM(5)
      BJK = PARAM(6)
      DA = 0.001*PARAM(7)
      TA = 0.01*PARAM(8)
      DEL = 1.0
      KAPPA = 0.0457**(1-DEL)*0.1*TA/(3*600.0)
      LAMBDAa = 0.67
      DmT = PARAM(9)
      FillT = 0.01*PARAM(10)
      PhiCT = 0.01*PARAM(11)
      DbT = 0.001*PARAM(12)
      Dm = PARAM(13)
      CenterLineLength = PARAM(14)
      BellyLength = PARAM(15)
      Dt = PARAM(16)
      Jt = 0.01*PARAM(17)
	Fill = Jt
      Jb = 0.01*PARAM(18)
      BallSize = 0.001*PARAM(19)
      PhiC = 0.01*PARAM(20)
      GrateSize = 0.001*PARAM(21)
	Eminf = PARAM(22)
	Dp0 = 0.001*PARAM(23)
	Phi = PARAM(24)
	PPortSize = 0.001*PARAM(25)
	PPortGrateOARatio = PARAM(26)

C	Formal bed porosity
      E = 0.40
C	Fill for mill and test mill
      Um = 1.0
	UT = 1.0

      RhoB = 7800
      OUT2 = FEED

c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      WTR2 = 0.0
      WTR3 = 0.0
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF

c Calculate the charge density
      WS = 0.0
      TOT = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TOT = TOT + OUT2(I,J,K)
            WS = WS + OUT2(I,J,K)/PPROP(J)
          END DO
        END DO
      END DO
      RhoO = 1000*TOT/WS
c      write(13,'(/'' Average density of the ore'',F8.1,'' kg/m**3'')')
c     * RhoO
      PhiV = TMS1/RhoO/(TMS1/RhoO+WTR1/1000.0)
      RhoC = Charge_Density(Jt, Jb, E, Um, RhoB, RhoO, PhiV)
c      write(13,'('' Calculated density of the total mill charge'',F8.1,
c     *'' kg/m**3'')') RhoC

C Calculate the net power draw.
      ConeLength = 0.5*(CenterLineLength + BellyLength)
      NoLoadPower = No_Load_Power(BellyLength,Conelength,Dm,PhiC)
      write(UnitDiagFile,'('' No-load power'',T20,F9.1,'' W'')')
     *NoLoadPower
      NetPower = Mill_PowerM(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, 'Grate')
      write(UnitDiagFile,'('' Morell power'',T20,F9.1,'' W'')')NetPower
      !Dt is the feed opening aka trunion diameter
      NetPower =  Mill_PowerA(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, Dt)
      write(UnitDiagFile,'('' Austin net power'',T20,F9.1,'' W'')')
     *NetPower
      Ecs = NetPower/TMSF
      write(UnitDiagFile,'('' Specific power consumption'',F7.2,
     *'' kW hr/tonne'')') Ecs/3600

c Calculate Austin's scale up parameters for the selection function
      DELTA = 0.0
      IF(Dm .GE. 3.81) DELTA = 0.2
      C2 = ((Dm/DmT)**0.5)*((3.81/Dm)**DELTA)
      C3 = (1.0 + 6.6*(FillT**2.3))/(1.0 + 6.6*(Fill**2.3))
      C4 = (PhiC-0.1)/(PhiCT-0.1)
      C4 = C4*(1.0+EXP(15.7*(PhiCT-0.94)))/(1.0+EXP(15.7*(PhiC-0.94)))
	C5 = exp(-1.34*(Um - UT))
	
c Calculate the total internal volume of the mill.
      WS = CenterLineLength/BellyLength
      Vm = 0.25*Pi*Dm*Dm*BellyLength
      Vm = Vm*(1.0 + 0.6667*(WS - 1)*(1-(Dt/Dm)**3)/(1-Dt/Dm))

c Calculate the load in the mill
      VolumeOfSlurry = Jt*Um*E
      VolumeOfMedia = (Jt - Jb)*(1.0 - E)
      MillLoad = (VolumeOfSlurry*PhiV + VolumeOfMedia)*RhoO*Vm
      write(UnitDiagFile,'('' Total mill volume'',F7.2,'' m**3''
     */'' Mill load excluding balls'',F7.2,'' tonnes'')')
     * Vm,0.001*MillLoad

c  Calculate the average residence time.
      TAU = MillLoad/TMSF
      write(UnitDiagFile,'('' Calculated residence time in the mill''
     *,F7.2,'' mins'')')TAU/60.0


C Calculate the size distribution in the product
      SIGMAI = 0.957
      Zeta(0) = 0.0

      DO I = 1,NDC
C     Need to add to this model the grate open area.  This is also
C	going to an important parameter to predict FILL
	Xpass = 1.0
	XOA = PPortGrateOARatio*2.5*(PPortSize-GrateSize)/GrateSize
	IF(SIZE(I) .LE. GrateSize/Xpass) THEN
		Cg = 0.0
	ELSE
	    IF(SIZE(I) .LT. GrateSize) THEN
			Cg = 1.0 - LOG(GrateSize/SIZE(I))/LOG(Xpass)
		ELSE
			Cg = 1.0
		END IF
	END IF
	IF(SIZE(I) .LE. PPortSize/Xpass) THEN
		Cp = 0.0
	ELSE
	    IF(SIZE(I) .LT. PPortSize) THEN
			Cp = 1.0 - LOG(PPortSize/SIZE(I))/LOG(Xpass)
		ELSE
			Cp = 1.0
		END IF
	END IF
	Classi(I) = (Cg + Cp*XOA)/(1.0+XOA)
C     This model for the SAG report 2006
C	IF(PPortGrateOARatio .GT. 1.0) THEN

C		Gratesize = PPortSize
C          Classi(I) = 1 - exp(-0.693*(Size(I)/GrateSize)**6)

C	ELSE
c     This is the model suggested by Vladmir Alves, CVRD
C	  IF(Size(I) .LT. GrateSize/2.0) THEN 
C	     Classi(I) = 0.0
C	  ELSE
C	     IF(Size(I) .LT. GrateSize) THEN
C	        Classi(I) = 1.0 - (1.0 - PPortGrateOARatio)*(ALOG(Size(I))-
C    *ALOG(GrateSize))/(ALOG(GrateSize/2.0)-ALOG(GrateSize)) 
C    *- PPortGrateOARatio
C	     ELSE
C	        IF(Size(I) .LT. PPortSize) THEN
C	           Classi(I) = 1.0 - PPortGrateOARatio*(ALOG(Size(I)) - 
C     *ALOG(PPortSize))/(ALOG(GrateSize)-ALOG(PPortSize))
C	        ELSE
C	           Classi(I) = 1.0
C	        END IF
C	     END IF
C	  END IF
C	END IF
c
c        Classi(I) = 1 - exp(-0.693*(Size(I)/GrateSize)**6)
        IF(I .EQ. NDC) THEN
          Self(I) = 0.0
          GammaI(I) = 0.0
        ELSE
          Self(I) = SELF_BR_SEL(DM,PHIC,Fill,SIZE(I),Eminf,Dp0,Phi)
          GammaI(I) = 3.0*KAPPA/(SIZE(I)**(1-DEL))
        END IF
      END DO
c	Write(UnitDiagFile,*)'Self breakage function'
c      DO I = 2,NDC
c        SelfBreakageFunction(I) =
c     *  SELF_BR(SIZE(1)/SIZE(I-1),SIZE(1),AJK,BJK,DM)
c        WRITE(UnitDiagFile,*) SIZE(I-1),BreakageFunction(I)
c      END DO
      TAU_PRIME = TAU
      RECYCLEM = 0.0
      NTRIALS = 0

c Calculate the charge density.

   10 CONTINUE

      RECYCLE = 0.0
      CALL MEDIUM(SIZE,OUT2,MEDIA,NDC,NGC,NSC,TOTMEDIA,GrateSize)
      DO N = 1,NSC
        DO M = 1,NGC
          DR = PPROP(M)*1000.0/RhoB
          DO I = 1,NDC
            IF(I .EQ. NDC) THEN
              Sel(I) = 0.0
            ELSE
              dpi = SIZE(I)
              WS1 = AustinSelMedia(dpi,SIZE,MEDIA,NDC,NGC,NSC,
     *		    MuT*DR,SLambda,Dm,DmT,DbT,GrateSize,A,Alpha)
              WS2 = AustinSelSteel(dpi,BallSize,DbT,MuT,
     *		    SLambda,Dm,DmT,A,Alpha)
              Sel(I) = C2*C3*C4*C5*((Jt - Jb)*WS1*DR/Jt + Jb*WS2/Jt)
            END IF
            Zeta(I) = SIGMAI*GammaI(I)*TAU_PRIME        ! Equation 5.110
            XII = (Sel(I) + Self(I) + GammaI(I))*TAU_PRIME  ! Eqs. 5.111 & 5.126

            OUT2(I,M,N) = FEED(I,M,N)
            DO J = 1,I-1
              AIJ = ATTRITION_BR(MeshSizes(I-1),MeshSizes(J),DA,LAMBDAa)
             BETA = 3.02
             GAMMA = 0.34
             PHI5 = 0.63
             BIJ = AUSTINBR(SIZE(I-1)/SIZE(J),GAMMA,BETA,PHI5)
	       ! BIJ = EnSpBrFunc(SIZE(J)/SIZE(I-1),SIZE(J),AJK,BJK,Ecs)
              CIJ = SELF_BR(SIZE(J)/SIZE(I-1),SIZE(J),AJK,BJK,DM)
              IF(I .LT. NDC) THEN
                AIJ = AIJ -
     *           ATTRITION_BR(MeshSizes(I),MeshSizes(J),DA,LAMBDAa)
                BIJ = BIJ - AUSTINBR(SIZE(I)/SIZE(J),GAMMA,BETA,PHI5)
	          ! BIJ = BIJ - 
     *          ! EnSpBrFunc(SIZE(J)/SIZE(I),SIZE(J),AJK,BJK,Ecs)
                CIJ = CIJ -
     *           SELF_BR(SIZE(J)/SIZE(I),SIZE(J),AJK,BJK,DM)
              END IF
              DELIJ = (GammaI(J)*AIJ + Sel(J)*BIJ + Self(J)*CIJ)
              DELIJ = DELIJ*TAU_PRIME                   !Equation 5.112
              OUT2(I,M,N) = OUT2(I,M,N) + DELIJ*OUT2(J,M,N)
            END DO
            CI = Classi(I)
            IF(I .GT. 1) Then
              OUT2(I,M,N) = OUT2(I,M,N) + Zeta(I-1)*OUT2(I-1,M,N) 
            END IF
            OUT2(I,M,N) = OUT2(I,M,N)/(1.0 + Zeta(I) + XII - CI)! Equation 5.124
            RECYCLE = RECYCLE + CI*OUT2(I,M,N)
            PROD(I,M,N) = (1.0 - CI)*OUT2(I,M,N)    !Equation 5.126
          END DO
        END DO
      END DO
      IF(ABS((RECYCLE-RECYCLEM)/RECYCLE) .GT. 0.00001) THEN
C        Flow = TMSF + RECYCLEM + (RECYCLE - RECYCLEM)
        Flow = TMSF + RECYCLE
	IF(RECYCLE .LT. 0.0) THEN
        TAU_PRIME = TAU*TMSF/Flow
	ELSE
        TAU_PRIME = TAU*Flow/TMSF	
	END IF
        WRITE(UnitDiagFile,*)' TAU_PRIME,RECYCLE', TAU_PRIME,RECYCLE
        RECYCLEM = RECYCLE
        NTRIALS = NTRIALS + 1
        IF(NTRIALS .LT. 200) GO TO 10
        WRITE(UnitDiagfile,*)' Convergence not reached in model SAGM',
     *  ' while calculating the internal recycle'
        WRITE(Unitdiagfile,*)'   Try increasing the grate aperture'
        CALL FLUSH(UnitDiagFile)
	  RECYCLE = -TMSF
        IF(IFLAG .EQ. 1) UnitExitValue = 38
      END IF
	N = UnitDiagFile
c	Write(UnitDiagFile,*)'A, Eminf',A, Eminf
c      WRITE(N,*)'      Size           Sel            Self          Sum'
c      DO I = 1,NDC
c        WRITE(UnitDiagFile,*)Size(I),Sel(I),Self(I),SEL(I)+Self(I)
c      END DO
      TMS2 = TMSF + RECYCLE
      write(UnitDiagFile,*)'TMS2',TMS2
      write(UnitDiagFile,*)'Sum prod,TMS1',SUM(PROD),TMS1

      DEALLOCATE(MEDIA)
      DEALLOCATE(Zeta,GammaI,Self,Sel,Classi)
      DEALLOCATE(SelfBreakageFunction)
      RETURN
      END


      SUBROUTINE MEDIUM(SIZE,CONTENTS,MEDIA,NDC,NGC,NSC,TOTAL,GrateSize)
c     ******************************************************************
c Function to calculate the size distribution of the pebbles in a SAG mill
      REAL SIZE(NDC)
      REAL CONTENTS(NDC,NGC,NSC)
      REAL MEDIA(NDC,NGC,NSC)

      TOTAL = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            IF(SIZE(I) .GE. GrateSize) THEN
              TOTAL = TOTAL + CONTENTS(I,J,K)
            END IF
          END DO
        END DO
      END DO
      MEDIA = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1, NSC
            IF(SIZE(I) .GE. GrateSize) THEN
              MEDIA(I,J,K) = CONTENTS(I,J,K)/TOTAL
            ELSE
              MEDIA(I,J,K) = 0.0
            END IF
          END DO
        END DO
      END DO
      RETURN
      END

C Function to calculate the density of the charge in a mill
C *********************************************************
      FUNCTION Charge_Density(JT, JB, E, U, RhoB, RhoO, PhiV)
c     *******************************************************
C Calculation of overall density of the charge in a mill.
c   In the following, MEDIA means lumps of ore that are large enough
c   to be considered autogenous grinding media.
c   JT is fraction of mill volume occupied by the total charge.
c   JB is fraction of mill volume occupied by the steel balls.
c   E is the voidage between the balls and media.
c   U is the fraction of the voidage that is filled by slurry.
c   RhoB is the density of the balls.
c   RhoO is average density of the solid ore.
c   PhiV is the volume fraction of solid in the slurry.
c
      REAL JT, JB
      VolumeOfBalls = JB*(1.0 - E)
      VolumeOfSlurry = JT*U*E
      VolumeOfMedia = (JT - JB)*(1.0 - E)
      Charge_Density = VolumeOfBalls*RhoB + VolumeOfMedia*RhoO
     *                 + VolumeOfSlurry*PhiV*RhoO
     *                 + VolumeOfSlurry*(1.0 - PhiV)*1000
      Charge_Density = Charge_Density/JT
      RETURN
      END


C Functions to calculate mill power.
C **********************************

      FUNCTION No_Load_Power(Length, ConeLength, Diameter,CriticalSpeed)
c     ******************************************************************
c Calculation of the power drawn by the mill under no load conditions
c using Steve Morrell's empirical model.  Morrell S., Trans. Instn. Min.
c Metall. 105 (1996) pp C54 - C62
      REAL No_Load_Power
      REAL Length, ConeLength, CriticalSpeed, Diameter
      Phi = CriticalSpeed
      WS = Phi*(0.667*ConeLength + Length)
      No_Load_Power = 1000*1.68*Diameter**2.05*WS**0.82
      RETURN
      END

      FUNCTION Mill_PowerM(Length, ConeLength, Diameter, CriticalSpeed,
     * ChargeDensity, FilledFract, TYPE)
c     *****************************************************************
c Calculation of the power drawn by the mill using Steve Morrell's empirical
c model.  Morrell S., Trans. Instn. Min. Metall. 105 (1996) pp C54 - C62
	USE Globals
      REAL Mill_PowerM
      REAL Length, ConeLength, CriticalSpeed, ChargeDensity
      REAL FilledFract, Diameter
      REAL Jt
      REAL K
      CHARACTER*8 TYPE   !Must be 'Overflow' or 'Grate'

      K = 9.10
      IF( TYPE .EQ. 'Overflow') K = 7.98
      phi = CriticalSpeed
      omega = 2*(2.9863*phi - 2.2129*phi*phi - 0.49267)
      Jt = FilledFract
      alpha = Jt*(omega - Jt)/(omega**2)
      phi_max = 0.954 - 0.135*Jt
      delta = phi*(1.0 - (1.0 - phi_max)*exp(-19.42*(phi_max-phi)))
      EffectiveLength = Length*(1 + 2.28*Jt*(1 - Jt)*ConeLength/Length)
      Mill_PowerM =
     * K*(Diameter**2.5)*EffectiveLength*ChargeDensity*alpha*delta
c	write(UnitDiagFile,*)'K, omega, alpha, delta', K,omega,alpha,delta
c	write(UnitDiagFile,*)'Length, ConeLength, Diameter',Length,
c     *ConeLength,Diameter
c	write(UnitDiagFile,*)'Jt,Phi,Phimax,Eff Length',Jt,phi,phi_max,
c     *EffectiveLength
      RETURN
      END

      FUNCTION Mill_PowerA(Length, ConeLength, Diameter, CriticalSpeed,
     * ChargeDensity, FilledFract, FeedOpening)
c     *****************************************************************
c Calculation of the power drawn by the mill using Len Austin's model.
c Austin L G "A mill power equation for SAG mills" Minerals & Metallurgical
c Processing  February 1990  pp 57-
      REAL Mill_PowerA
      REAL Length, ConeLength, CriticalSpeed, ChargeDensity
      REAL FilledFract, Diameter
      REAL Jt
      REAL K

      K = 10.6
      Jt = FilledFract
      delta = CriticalSpeed*(1.0 - 0.1/(2**(9-10*CriticalSpeed)))
      alpha = Jt*(1.0 - 1.03*Jt)
      Mill_PowerA =K*(Diameter**2.5)*Length*ChargeDensity*alpha*delta
c Calculate the correction for the conical ends.  The cone is assumed to extend
c the circumference.
      IF(Jt .LT. 0.5 .AND. ConeLength .GT. 0.0) THEN
        ws = 0.625/(0.5 - Jt)
        f3 = ConeLength/(Length*(1.0-FeedOpening/Diameter))
        f3 = f3*(ws**0.1 - ws**(-4))
        f3 = f3*0.092 /alpha
        Mill_PowerA = Mill_PowerA*(1+f3)
c        write(*,*) ' f3',f3
      END IF
      RETURN
      END

      FUNCTION Mill_PowerB(Length, Diameter, CriticalSpeed,
     * MediaDensity, FilledFract, MaxBallDiameter)
c     *****************************************************************
c Calculation of the shaft power drawn by the mill using Bond's formula.
c See Austin L G , Klimpel R R and Luckie P T. Process Engineering of
c Size Reduction, p 236.

      REAL Mill_PowerB
      REAL Length, Diameter, CriticalSpeed, MediaDensity
      REAL FilledFract, MaxBallDiameter
      REAL Jt, CB0

      CB0 = 15.6
      Jt = FilledFract
	delta = CriticalSpeed*(1.0 - 0.1/(2**(9-10*CriticalSpeed)))
      Mill_PowerB = CB0*(Diameter**0.3)*(1.0-0.937*Jt)*delta
c Calculate the slump correction for the ball diameter.
	IF(MaxBallDiameter .LT. 0.04572) THEN
		IF(Diameter .GT. 2.4) THEN
	       IF(Diameter .LT. 3.6) THEN
	          Ss = (1.8 - MaxBallDiameter*1000/25.4)/(2*0.90718)
		      Mill_PowerB = Mill_PowerB - Ss		
	       ELSE IF(Diameter .GE. 3.6) THEN
c Rowland's modification
	          Ss = (3*Diameter*3.28084/20 - MaxBallDiameter*1000/25.4)
		      Ss = Ss/(2*0.90718)
		      Mill_PowerB = Mill_PowerB - Ss
		   END IF
		END IF
      END IF
	Mass_of_media = 0.47*Jt*Length*(Diameter**2)*MediaDensity
	Mill_PowerB = Mill_PowerB*Mass_of_media
      RETURN
      END

      FUNCTION HERBST_FUER_SEL(SIZE,SE1,ZETA1,ZETA2)
C     **********************************************
C     Herbst-Fuerstenau selection function
      DPR = SIZE*1000
      WS = ALOG(DPR)
      WS1 = ZETA1*WS + ZETA2*WS*WS
      HERBST_FUER_SEL = SE1*EXP(WS1)
	IF(HERBST_FUER_SEL .LT. 0.0) HERBST_FUER_SEL = 0.0
      RETURN
      END

      SUBROUTINE HERBST_FUER(I,SIZE,S)
C     ********************************
C H-F selection function that is compatible with PMMILL
      COMMON/HERBST/SE1, ZETA1, ZETA2
      S = HERBST_FUER_SEL(SIZE,SE1,ZETA1,ZETA2)
      RETURN
      END

      SUBROUTINE BR_FUNCTION(I,J,SJ1,SJ2,SI,DELB)
C     *******************************************
C The standard breakage function compatible with PMMIL
      USE GLOBALS
      COMMON/BREAKAGE_PARS/GAMMA,BETA,PHI5,DELTA
      PHI = PHI5*(5.0E-3/SI)**DELTA
      SR1=SJ1/SI
      SR2=SJ2/SI
      DELB=AUSTINBR(SR1,GAMMA,BETA,PHI)-AUSTINBR(SR2,GAMMA,BETA,PHI)
      RETURN
      END
C
      SUBROUTINE SCOAL(I,SIZE,S)
C     **************************
C Selection function for ball milling of coal.
c See Rogers RSC Brame KA and Austin LG Simulation for the design of ball
C mills for coal grinding. Coal Preparation Vol 5 71-83 (1987)
C HGI is the Hardgrove Grindability Index.

      COMMON /COAL/HGI
      REAL MILLD,MILLT
      REAL MUT,LAMBDAT
      LOGICAL WET

      PARAMETER (LAMBDAT = 2.2)
      PARAMETER(MILLT=0.0195,BALLT=0.0254,FILLT=0.2,PHICT=0.70)
      WET = .TRUE.
      ALPHA = 0.55 + 0.0055*HGI
      MUT = 0.001*(HGI/10.0 - 1.66)*(ALPHA/(3.0-ALPHA))**0.333
      IF(WET) AT = 1.1 + 0.076*(HGI - 36.0)
      IF(.NOT.WET) AT = 1.1 + 0.042*(HGI - 36.0)
C Set up the dimensions of the operating mill
      MILLD = 3.0
      BALLD = 0.0508
      FILL = 0.37
      PHIC = 0.7
      CALL AUS_SCALE_UP(BALLD,BALLT,MILLD,MILLT,FILL,FILLT,
     *PHIC,PHICT,C1,C2,C3,C4,C5)
      S = AUSTINS(SIZE,AT,ALPHA,C1*MUT,LAMBDAT)
      S = S*C2*C3*C4*C5
      RETURN
      END


      SUBROUTINE AUS_SCALE_UP(BALLD,BALLT,MILLD,MILLT,FILL,FILLT,
     *PHIC,PHICT,C1,C2,C3,C4,C5)
C     ***********************************************************
C Calculates the scale-up factors for Austin's method
C BALLD is the ball diameter and BALLT is the ball diameter in the test mill.
C MILLD is the mill diameter and MILLT is the diameter of the test mill.
C MFILL and MFILLT are the fractional mill filling with media.
C PHIC and PHICT are the mill speeds as fraction of critical.
      REAL MILLD,MILLT
      C1 = ((MILLD/MILLT)**0.2)*(BALLD/BALLT)**2
      C2 = BALLT/BALLD
      DEL = 0.0
      IF(MILLD .GE. 3.81) DEL = 0.2
      C3 = ((MILLD/MILLT)**0.5)*((3.81/MILLD)**DEL)
      C4 = (1.0 + 6.6*(FILLT**2.3))/(1.0 + 6.6*(FILL**2.3))
      C5 = (PHIC-0.1)/(PHICT-0.1)
      C5 = C5*(1.0+EXP(15.7*(PHICT-0.94)))/(1.0+EXP(15.7*(PHIC-0.94)))
      RETURN
      END


C  The following routines are developed for autogenous mills
c  *********************************************************

      FUNCTION AustinSelMedia(dpi,SIZE,MEDIA,NDC,NGC,NSC,MuT,Lambda,
     *Dm,DmT,DbT,GrateSize,A,Alpha)
C     **************************************************************
C  Averages Austin's selection function with respect to media size.
c  muT and lambdaT are the values for media particles equal to the
c  grate size. DmT is the test mill diameter.
      REAL SIZE(NDC)
      REAL MEDIA(NDC,NGC,NSC)
      REAL MuT,Lambda
      REAL N0,N2,N3
      REAL Mu

      N0 = 1.0
      N2 = 0.2
      N3 = .5
c
C Convert the size to upper mesh size.
C Average over all media sizes
      TERM = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            IF(SIZE(I) .GE. GrateSize) THEN
              C1 = (Dm/DmT)**N2 * (SIZE(I)/DbT)**N3
              Mu = MuT*C1
              C2 = (DbT/SIZE(I))**N0
              TERM = TERM + (MEDIA(I,J,K)*C2)/(1.0+(dpi/Mu)**Lambda)
            END IF
          END DO
        END DO
      END DO
      AustinSelMedia = A*(dpi/0.001)**Alpha * TERM
      RETURN
      END

      FUNCTION AustinSelSteel(dpi,BallSize,DbT,MuT,Lambda,
     *Dm,DmT,A,Alpha)
C     **************************************************************
C  Averages Austin's selection function with respect to ball size.
c  muT and lambdaT are the values for balls at the test size.
c  DmT is the test mill diameter. DbT is the test ball size.
      REAL MuT,Lambda
      REAL N0,N2,N3
      REAL Mu

      N0 = 1.0
      N2 = 0.2
      N3 = 0.5
c
      TERM = 0.0
      C1 = (Dm/DmT)**N2 * (BallSize/(DbT))**N3
      Mu = MuT*C1
c      C2 = (DbT/BallSize)**N0 * (1.0+.002/DbT)/(1.0+.002/BallSize)
      C2 = (DbT/BallSize)**N0 
      TERM = TERM + C2/(1.0+(dpi/Mu)**Lambda)
      AustinSelSteel = A*(dpi/0.001)**Alpha * TERM
      RETURN
      END

      FUNCTION SELF_BR_SEL(DM,PHIC,LOAD_VOL,DP,Eminf,Dp0,Phi)
C     *******************************************************
C Selection function for self breakage on impact.
C Rate of breakage calculated as the number
C of drops of lumps of size DP per second x mass of lump x probability of
C breakage. At present all drops are assumed to be 0.2 x mill diameter. Later
C distribution of drop heights from DEM simulations will be incorporated.

C DM       Mill diameter
C PHIC     Mill speed as fractIon of critical
C LOAD_VOL Fraction of mill volume occupied by the load
C DP       Particle size

      REAL LOAD_VOL
C First calculate frequency of drops per lump.
C Calculated as once per revolution at 50% mill load.
      CRITICAL = 0.705/SQRT(DM)
      FREQ = PHIC*CRITICAL

C Now calculate the average specific impact energy per drop....
      E = 9.81*0.2*DM
c and the probability of breakage.
      CALL BRPROB(E,DP,PROBA,Eminf,Dp0,Phi)

C Now the specific rate of breakage
      SELF_BR_SEL = FREQ*PROBA
      RETURN
      END


      SUBROUTINE BRPROB(E,DP,PROBA,Eminf,Dp0,Phi)
C     *******************************************
C FUNCTION TO CALCULATE THE PROBABILITY OF BREAKAGE
C E IS THE SPECIFIC ENERGY   J/kg
C DP IS THE PARTICLE SIZE in meters
C THE LOG NORMAL MODEL IS USED
      REAL*8 X, QNRML
      INTEGER*2 IG, IE

C Variation of E50 with size from Tavares and King "Single-particle fracture
c under impact loading", Int. Jnl Mineral Processing, 54(1998) 1-28
      E50 = Eminf*(1.0 + Dp0/DP)**Phi
      SIGMA = 0.662
      X = ALOG(E/E50)/SIGMA
      PROBA = 1.0 - QNRML(X,IG,IE)
      RETURN
      END

      FUNCTION QNRML(X,IG,IE)
C     ***********************
C Complementary Normal probabilty function ex Mathematical Functions package
         REAL*8 QNRML,X,X1,X3,X4,X5,X6,BE,PP,SQ2PIE
         INTEGER*2 IG,IE
         DIMENSION BE(6)

         DATA PP/0.2316419D0/
         DATA SQ2PIE/0.398942280401433D0/
         DATA (BE(I),I=1,6)/
     C        0.0D0,
     C        0.31938153D0,
     C       -0.356563782D0,
     C        1.781477937D0,
     C       -1.821255978D0,
     C        1.330274429D0/

         IE=0
         IF(X.EQ.0.0D0) QNRML=0.5D0
         X1=DABS(X)
         IF(X1.LE.40.0D0) GO TO 1
            QNRML=0.0D0
            IF(X.LT.0.0D0) QNRML=1.0D0
            RETURN
1        CONTINUE
         X3=DEXP(-X1*X1/2.0D0)
         IF(X1.GT.4.25D0) GO TO 2
            X4=1.0D0/(1.0D0+PP*X1)
            QNRML=BE(6)
            DO 3 I=1,5
               QNRML=QNRML*X4+BE(6-I)
3           CONTINUE
            QNRML=QNRML*X3*SQ2PIE
            IF(X.LT.0.0D0) QNRML=1.0D0-QNRML
            RETURN

2        CONTINUE
            X4=1.0/X1
            X4=X4*X4
            X5=X4*X4
            X6=X4*X5
            QNRML=X6*X4*105.0D0+1.0D0-X4+3.0D0*X5
            QNRML=X3*(QNRML-15.0D0*X6)*SQ2PIE/X1
            IF(X.LT.0.0D0) QNRML=1.0D0-QNRML
            RETURN
      END

      FUNCTION ATTRITION_BR(MeshSize,ParentSize, DA, LAMBDAa)
C     *******************************************************
C Breakage function for products from ATTRITION in autogenous mills
C Data from Leung K, Morrison RD and Whiten WJ. An Energy-Based
C Ore-Specific Model for Autogenous and Semi-Autogenous Grinding.
C Copper 87 Santiago, Chile, Universidad de Chile (1987-1988) pp71-85
C Size classes must be a root 2 sequence.
      REAL LAMBDAa,MeshSize,ParentSize
      UPPER = DA
      IF(ParentSize .LE. DA) UPPER = ParentSize
      IF(MeshSize .GE. UPPER) THEN
        ATTRITION_BR = 1.0
      ELSE
        ATTRITION_BR = (MeshSize/UPPER)**LAMBDAa
      END IF
      RETURN
      END

      FUNCTION SELF_BR(RINDEX,DP,A,B,DM)
C     **********************************
C The breakage function for self breakage.
C Leung, Morrison and Whiten Copper '87 recommend the t10 breakage function
C as determined on the dual pendulum. T10 is modeled as a function of
C impact energy using a simple exponential function and the JKMRC test
C produces two parameters A and b to describe this function. These are
C ore-specific and MODSIM will supply them as unit parameters. ECS is the
C mass specific energy absorbed during breakage in kWhr/tonne. The energy
C is related to the height of fall and therefore proportional to the mill
C diameter.

      COMMON/T10JK/ AA,BB
      AA = A
      BB = B
      Ecs = 2.725e-3*0.3*DM   !Ecs = 9.81/3600
      IF(RINDEX .GT. 1.0) THEN
        SELF_BR = TN1(Ecs,DP,RINDEX)
c	WRITE(13,*) 'RINDEX,SELF_BR',RINDEX,SELF_BR
      ELSE
        SELF_BR = 1.0
      END IF
      RETURN
      END

      Function EnSpBrFunc(RINDEX,DP,A,B,Ecs)
	!****************************************
	!Calculate the breakage function using the T10 method and the average specific comminution
	!energy in the mill.
	!Ecs is in kWh/t

      COMMON/T10JK/ AA,BB
      AA = A
      BB = B
      EnSpBrFunc = TN1(Ecs,DP,RINDEX)
	End Function


      FUNCTION T10(Ecs,DP)
C     ********************
C     Function to calculate T10 using the standard JKMRC parameters.
c     T10 is fraction.
C     Ecs is the specific comminution energy in kW hr/tonne
      COMMON/T10JK/ A,B
      T10 = A*(1.0 - EXP(-B*Ecs))
c	Write(13,*)' T10' ,T10
      RETURN
      END

      FUNCTION TN1(Ecs,DP,RINDEX)
c     ***************************
C     Calculates the value of TN using the truncated Rosin-Rammler model.
c     ENERGY in J/kg.  DP is parent size in meters. RINDEX is the index n
C     Tn and T10 are in %.

      TN1 = 1.0 - (1.0 - T10(Ecs,DP))**((9.0/(RINDEX-1.0))**0.9)

	END FUNCTION


      FUNCTION TN(Ecs,DP,RINDEX)
C     **************************
C Calculates the value of TN using Ludovic Milin's continuous model
c Ecs in kWh/t.  DP is parent size in meters. RINDEX is the index n
C Tn and T10 are fractions.

      DOUBLE PRECISION a,b,Ibeta,x
      a = -0.7846*exp(-0.4988*RINDEX)
      a = a+(0.9190*(RINDEX**(0.04526))/
     &     ((0.000972*RINDEX*RINDEX+1.)**0.03797))
      b = (1.193/((RINDEX-1.)**1.392))
      b = b+(2.722*(RINDEX**(-0.3404))/
     &    ((.01175*RINDEX**2+1.)**.3381))
      x = T10(Ecs,DP)
      TN = Ibeta(a,b,x)
C      write(*,*)' x ,TN ' ,x,TN
      RETURN
      END

      double precision function Ibeta(a,b,x)
C     **************************************
      double precision a,b,x
C  uses betacf, gammln.  Returns the incomplete beta function
      double precision bt,betacf,gammln
C      if (x.lt.0..or.x.gt.1.) pause 'bad argument x in Ibeta'
      if (x.eq.0..or.x.eq.1.) then
         bt=0.d0
      else
         bt=exp(gammln(a+b)-gammln(a)-gammln(b)+a*log(x)+b*log(1.-x))
      endif
      if (x.lt.(a+1.)/(a+b+2.)) then
         Ibeta=bt*betacf(a,b,x)/a
         return
      else
         Ibeta=1.-bt*betacf(b,a,1.-x)/b
         return
      endif
      return
      end
************************************************************************
      double precision function betacf(a,b,x)
      double precision a,b,x,eps,fpmin
      parameter(maxit=100,eps=3.0d-7,fpmin=1.0d-30)
C  used by Ibeta: evaluates continued fraction for incomplete beta function
C  by modified Lentz's method.
      integer m,m2
      double precision aa,c,del,h,qab,qam,qap

      qab=a+b
      qap=a+1.
      qam=a-1.
      c=1.
      d=1.-qab*x/qap
      if (abs(d).lt.fpmin) d=fpmin
      d=1./d
      h=d
      do 11 m=1,maxit
         m2=2*m
         aa=m*(b-m)*x/((qam+m2)*(a+m2))
         d=1.+aa*d
         if (abs(d).lt.fpmin) d=fpmin
         c=1.+aa/c
         if (abs(c).lt.fpmin) c=fpmin
         d=1./d
         h=h*d*c
         aa=-(a+m)*(qab+m)*x/((a+m2)*(qap+m2))
         d=1.+aa*d
         if (abs(d).lt.fpmin) d=fpmin
         c=1.+aa/c
         if (abs(c).lt.fpmin) c=fpmin
         d=1./d
         del=d*c
         h=h*del
         if (abs(del-1.).lt.eps) goto 1
11    continue
C      pause 'a or b too big, or maxit too small in betacf'
1     betacf=h
      return
      end
************************************************************************
      double precision function gammln(xx)
      double precision xx
C returns the value ln(Gamma(xx)) for xx>0
      integer j
      double precision ser,stp,tmp,x,y,cof(6)
      save cof,stp
      data cof,stp /76.1800917294746d0,-86.50532032941677d0,
     &             24.01409824083091d0,-1.231739572450155d0,
     &             .1208650973866179d-2,-.5395239384953d-5,
     &             2.5066282746310005d0/

      x=xx
      y=x
      tmp=x+5.5d0
      tmp=(x+0.5d0)*log(tmp)-tmp
      ser=1.000000000190015d0
      do 11 j=1,6
         y=y+1.d0
         ser=ser+cof(j)/y
11    continue
      gammln=tmp+log(stp*ser/x)
      return
      end
************************************************************************
      SUBROUTINE HPGR(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
	USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

C Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C
C  MODEL IS BASED ON THE BREAKAGE AND SELECTION FUNCTION.
C  SIMPLIFIED VERSIONS OF THE AUSIN CORRELATIONS FOR THE BREAKAGE AND
C  SELECTION FUNCTIONS ARE USED.
C
C
      PARAMETER (Pi = 3.1415927)
	REAL Xg, X, mu, lambda, kappa, alpha, phi, gamma, beta
	REAL phiP,gammaP,betaP,PMD,PML,PMu,PMGP,TMD,TML,TMu,TMGP
	REAL, ALLOCATABLE::A(:)
	REAL, ALLOCATABLE::AP(:)
	REAL, ALLOCATABLE::AA(:)
	REAL, ALLOCATABLE::B(:,:)
      REAL, ALLOCATABLE::BP(:,:)
	REAL, ALLOCATABLE::B_(:,:)
      REAL, ALLOCATABLE::BP_(:,:)
      REAL, ALLOCATABLE::PS(:,:)	
	REAL, ALLOCATABLE::D(:,:)

      IF(.NOT. ALLOCATED(A)) ALLOCATE(A(NDC))
	IF(.NOT. ALLOCATED(AP)) ALLOCATE(AP(NDC))
	IF(.NOT. ALLOCATED(AA)) ALLOCATE(AA(NDC))
	IF(.NOT. ALLOCATED(B)) ALLOCATE(B(NDC,NDC))
      IF(.NOT. ALLOCATED(BP)) ALLOCATE(BP(NDC,NDC))
	IF(.NOT. ALLOCATED(B_)) ALLOCATE(B_(NDC,NDC))
      IF(.NOT. ALLOCATED(BP_)) ALLOCATE(BP_(NDC,NDC))
	IF(.NOT. ALLOCATED(PS)) ALLOCATE(PS(NDC,NDC))
	IF(.NOT. ALLOCATED(D)) ALLOCATE(D(NDC,NDC))
      FL=1

	PMD=PARAM(1) !Production Mill Roll Diameter
	PML=PARAM(2) !Production Mill Roll Length
	PMu = PARAM(3) !Production Mill peripheral velocity
	PMGP=PARAM(4) !Production Mill Grinding Pressure
      alphaC=PARAM(5) !Critical angle of nip
      mu=PARAM(6) !nipping selection function
      lambda=PARAM(7) !nipping selection function
C      kappa=PARAM(8)/1000 !selection function compressing grind
C      alpha = PARAM(9) !selection function slope
	phi = PARAM(10) !nipping breakage function
	gamma = PARAM(11) !nipping breakage function
	beta = PARAM(12) !nipping breakage function
	phiP = PARAM(13) !compressing grind breakage function
	gammaP = PARAM(14) !compressing grind breakage function
	betaP = PARAM(15) !compressing grind breakage function
	TMD=PARAM(16) !Test Mill Roll Diameter
	fm =PARAM(17) !Test Mill specific capacity factor
	TMu = PARAM(18) !Test Mill peripheral velocity
	fp=PARAM(19) !Test Mill specific power factor
	F80T=PARAM(20)/1000 !Test Mill F80, in m
C
      PMRPM = PMu*60/(PMD*3.1416) !Calculated roll speed in RPM
c Calculate the water flows if water is added to the mill
      TMS1=TMSF
      WTR1=WTR
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF
      WTR2 = 0.0
      WTR3 = 0.0

C Calculate solids content and make a decision if this is going to press or not
C  need to write code for this (Claudio)


C      Calculate the feed density.
      WS = 0.0
      TOT = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TOT = TOT + FEED(I,J,K)
            WS = WS + FEED(I,J,K)/PPROP(J)
          END DO
        END DO
      END DO
      RhoF = 1000*TOT/WS
C     Calculate the capacity of this press
	Q = PMu*PMD*PML*RhoF*fm
	write(UnitDiagFile,*)'Feed density kg/m3, capacity kg/s', RhoF,Q
C     Calculate the operating gap assuming bulk has 40% bed porosity..
      alphaC = 2*Pi*alphaC/360
	BulkRho = RhoF*(1.0 - 0.4)
	Xc = Q/(PML*BulkRho*PMu*COS(alphaC))
	Xg = Xc - (1-COS(alphaC))*PMD
C     Scale up parameter kappa
	kappa = ((PMD/TMD)**(3.0/2.0))*PARAM(8)/(PMGP*1000.0)
C     Scale up parameter alpha
C	alpha = PARAM(9)*TMu/PMu  This did not work as per Polysius tests at 0.88 m/s
C     It looks like alpha is scaled by roll diameter as well
      alpha = PARAM(9)*TMD/(PMD*.675)
C	write(UnitDiagFile,*)'Xg (mm), kappa (mm)', Xg*1000, kappa*1000

      DO 12 I=1,NDC
	  X = SIZE(I)*(2.0**0.25)
        A(I) = 1/(1+(mu*Xg/X)**lambda)
        IF(I.EQ.NDC) A(I)=0.0

	  IF(X .GT. kappa) Then 
		 AP(I) = 1.0
	  ELSE
	    AP(I) = (X/kappa)**alpha
	  END IF
	  IF(I.EQ.NDC) AP(I)=0.0

	  AA(I) = (1-A(I))*(1-AP(I))

C      write(UnitDiagFile,*)'I, A(I), AP(I), AA(I)', I,A(I),AP(I), AA(I)
 12   CONTINUE

      DO 13 I=1,NDC
	   IF(SIZE(I) .LT. SIZE(2)) THEN
			   B(I,1) = phi*(SIZE(I)/SIZE(2))**gamma
	           B(I,1) = B(I,1)+(1.0-phi)*(SIZE(I)/SIZE(2))**beta
	           BP(I,1) = phiP*(SIZE(I)/SIZE(2))**gammaP
	           BP(I,1) = BP(I,1)+(1.0-phiP)*(SIZE(I)/SIZE(2))**betaP
		ELSE
			   B(I,1) = 1.0
			   BP(I,1) = 1.0
		END IF
   13 CONTINUE
      DO 14 I=1,NDC
	DO 14 J=2,NDC
	    IF(SIZE(I) .LT. SIZE(J)) THEN
	      B(I,J) = B(I-J,1)
	      BP(I,J) = BP(I-J,1)
	    ELSE
		  B(I,J) = 1.0
		  BP(I,J) = 1.0
	    END IF
   14 CONTINUE
C     Get the density breakage functions
      DO 15 I=1,NDC
	DO 15 J=1,NDC
		IF(I .LT. NDC) THEN 
		   B_(I,J) = B(I,J) - B(I+1,J)
		   BP_(I,J) = BP(I,J) - BP(I+1,J)	
	     ELSE
             B_(I,J) = B(I,J)
	       BP_(I,J) = BP(I,J)
		 END IF  
   15 CONTINUE
C     Check
C      DO 16 J=1,NDC
C	SUM = 0.0
C	SUM2 = 0.0
C	DO 17 I=1,NDC
C	 SUM  = SUM + B_(I,J)
C	 SUM2 = SUM2 + BP_(I,J)
C   17 CONTINUE
C       write(UnitDiagFile,*)'I, SUM b(I,J), SUM bP(I,J)', J,SUM, SUM2
C   16 CONTINUE


C
C	DO 31 I=1,NDC
C       DO 30 J=1,NDC-1
C
C	
C	       IF (SIZE(I) .LT. SIZE(J+1)) Then
C	           B(I,J) = phi*(SIZE(I)/SIZE(J+1))**gamma
C	           B(I,J) = B(I,J)+(1.0-phi)*(SIZE(I)/SIZE(J+1))**beta
C	           BP(I,J) = phiP*(SIZE(I)/SIZE(J+1))**gammaP
C	           BP(I,J) = BP(I,J)+(1.0-phiP)*(SIZE(I)/SIZE(J+1))**betaP
C	       ELSE
C	           B(I,J) = 1.0
C	           BP(I,J) = 1.0
C	       End If
C
C 30     CONTINUE
C 31   CONTINUE
C      DO 32 J=1,NDC
C	    B(NDC,J) = 0.0
C	    BP(NDC,J) = 0.0
C 32   Continue

	PS(1,1) = 0.0
	DO 35 I=1,NDC
        DO 34 J=1,NDC
	     IF( I .LE. J) Then 
	        PS(I,J) = 0.0
	     Else
              SUM = 0.0
	        DO 33 K=J,I-1 
	           SUM = SUM + (B_(I,K)*A(K)+BP_(I,K)*AP(K)*(1.0-A(K)))*PS(K,J)
   33         CONTINUE
              PS(I,J) = B_(I,J)*A(J)+BP_(I,J)*AP(J)*(1.0-A(J)) + SUM
		 End If
 34	  Continue
 35	Continue


C Calculate the transfer matrix
	DO 37 I=1,NDC
        DO 36 J=1,NDC
	      IF(I .LT. J) Then
		     D(I,J) = 0.0
		  Else
		     IF(I .EQ. J) Then
	            D(I,J) = AA(J)
	         Else
	            D(I,J) = PS(I,J)*AA(I)
	         End If
	      End If
 36     Continue
 37   Continue
	TMS = 0.0
C Calculate the product size distriubution
      DO 39 I=1,NDC
	    DO 40 J=1,NGC
	    DO 41 K=1,NSC
	    PROD(I,J,K) = 0.0
	    DER1(I,J,K) = 0.0
		DO 42 L=1,NDC
            PROD(I,J,K) = PROD(I,J,K) + D(I,L)*FEED(L,J,K)
            DER1(I,J,K) = DER1(I,J,K) + D(I,L)
   42     CONTINUE
   	    TMS = TMS + PROD(I,J,K)
   41     CONTINUE
   40     CONTINUE
   39 CONTINUE
      
	write(UnitDiagFile,*)'Sum prod,TMS', TMS,TMSF	
 	   
	DEALLOCATE(A,AP,AA,B,BP,B_,BP_,PS,D)		    
    
C
      FL = 1
	RETURN
      END
