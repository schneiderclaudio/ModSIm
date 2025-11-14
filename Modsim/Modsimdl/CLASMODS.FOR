C*******************     CLASMODS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE MODSIM MODELS FOR CLASSIFIER UNITS           C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE CYCL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      INTEGER INDPP(NPP,2),FL
      REAL DER1(NDC,NGC,NSC),FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(ND
     *C,NGC,NSC),OUT3(1)
      REAL DER2(NDC,NGC,NSC),DER3(1),PARAM(*),PPROP(1),SIZE(1),GRDM(1),G
     *RDV(1)

c Set up common areas to get data from CALC.FOR.
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C     CYCLONE MODEL ACCORDING TO PLITT CIM BUL.DEC 76 P114
C
C     PARAMETERS IN ORDER (SI UNITS)
C     ************************************
C     1...CYCLONE DIAMETER DC
C     2...VORTEX-SPIGOT DIST AS FRACTION OF DC
C     3...INLET DIAMETER AS FRACTION OF DC
C     4...VORTEX FINDER DIAMETER AS FRACTION OF DC
C     5...SPIGOT DIAMETER AS FRACTION OF DC
C     6...HEAD OF FEED SLURRY
C     7...NO. OF CYCLONES IN PARALLEL
C     8...PLITT'S CALIBRATION FACTOR FOR D50
C     9...PLITT'S CALIBRATION FACTOR FOR SHARPNESS
C     10..PLITT'S CALIBRATION FACTOR FOR FLOW SPLIT
C     11..Slurry viscosity
c     12..Exponent for density variation
c     13..Slurry density in separating zone.  As a fraction of the difference
c         between the carrier fluid and the lightest solid

C 
      DC = PARAM(1)
	H = PARAM(2)*DC
      DI = PARAM(3)*DC
      DOV = PARAM(4)*DC
      DU = PARAM(5)*DC
      HEAD = PARAM(6)
      PARALL=PARAM(7)
      F1=PARAM(8)
      F2=PARAM(9)
      F4=PARAM(10)
      REL_VISCOSITY = PARAM(11)/0.001
      AN = PARAM(12)
C
C     CALCULATE VOLUME FLOWRATES IN FEED
C
      RhoMin = PPROP(1)
      VS = 0.0
      DO 12 J=1,NGC
        SM = 0.0
        DO 11 I=1,NDC
          DO 10 K=1,NSC
            SM = SM+FEED(I,J,K)
   10     CONTINUE
   11   CONTINUE
        IF(PPROP(J).LT.0.001) THEN
          WRITE(UnitDiagFile,*)' PARTICLE TYPE ',J,
     *    ' DOES NOT HAVE A PROPER DENSITY'
          UnitExitValue = 38
          RETURN
        ENDIF
        VS = VS+SM/PPROP(J)
        IF(PPROP(J) .LT. RhoMin) RhoMin = PPROP(J)
   12 CONTINUE
      SG=TMSF/VS
      VS = VS/1000.0
      Q = WTR/1000.0 + VS
      PHI = VS/Q
      Q=Q/PARALL
      RhoSl = 1.0 + PARAM(13)*(RhoMin - 1.0)
C
C  CALCULATE CORRECTED EFFICIENCY CURVE
C
      D50 = F1*2690.0*(DC**0.46)*(DI**0.6)*(DOV**1.21)*EXP(6.3*PHI)
      D50 = D50/(1000**0.5)
      WS = (DU**0.71)*(H**0.38)*(Q**0.45)
      D50 =D50/WS
C Adjust for the slurry viscosity
      D50 = D50*(REL_VISCOSITY)**0.35

      S = 3.79*((DU/DOV)**3.31)*(H**0.54)*((DU*DU+DOV*DOV)**0.36)*
     *EXP(0.54*PHI)
      S = F4*S/((HEAD**0.24)*(DC**1.11))
      RV = S/(S+1.0)
      AM = F2*2.96*EXP(-1.58*RV)*(DC*DC*H/Q)**0.15
      TMS1 = 0.0
      TMS2 = 0.0
      feedsum = 0.0
      DO 22 I=1,NDC
        D = SIZE(I)*1.0E6
        DO 21 J=1,NGC
          D50J = D50/(ABS(PPROP(J)-RhoSl))**AN
          WS=0.693*(D/D50J)**AM
          C=1.0
          IF(WS.LT.10.0) C=1.0 - EXP(-WS)
C
C         CALCULATION OF OUTPUTS
C
          DO 20 K=1,NSC
            OUT1(I,J,K)=C*FEED(I,J,K)
            TMS1 = TMS1+OUT1(I,J,K)
   20     CONTINUE
   21   CONTINUE
   22 CONTINUE
C
C  CALCULATE ACTUAL EFFIENCY CURVE
C
      TMS1 = TMS1/TMSF
      RF =(RV-PHI*TMS1)/(1.0-PHI*TMS1)
      IF(RF .LT. 0.0) RF = 0.0
   25 CONTINUE
      TMS1 =0.0
      DO 30 I=1,NDC
      DO 30 J=1,NGC
      DO 30 K=1,NSC
        C=0.0
        IF(FEED(I,J,K) .NE. 0.0) THEN
          C = OUT1(I,J,K)/FEED(I,J,K)
          E = C + RF*(1.0 - C)
          OUT1(I,J,K) = E*FEED(I,J,K)
        ELSE
          OUT1(I,J,K) = 0.0
        END IF
        TMS1 = TMS1 + OUT1(I,J,K)
        OUT2(I,J,K) = FEED(I,J,K) - OUT1(I,J,K)
   30 CONTINUE
      TMS2 = SUM(OUT2)
      WTR1 = RF*WTR
      WTR2 = WTR-WTR1
C CHECK FOR ROPING (MULAR AND BHAPPU "MINERAL PROCESSING PLANT DESIGN"
C ****AIME 1978 P384
      PU=TMS1/(TMS1+WTR1)
      IF(DU.LT.(0.145-0.417/(2.65-SG+SG/PU)+0.0279*ALOG(TMS1/PARALL/SG))
     *.AND.FL.NE.4) GO TO 60
      RETURN
   60 FL=4
      RETURN
      END
C
C
      SUBROUTINE CYCB(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
C This is model CYCA with output streams reversed
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      INTEGER CHOICE
      REAL CLAMBDA
      COMMON /FND501/D50,EXPONENT,REF_SPGR
      COMMON /FUNPF2/CLAMBDA
      EXTERNAL D501,PF3,PF2,PF1

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      CALL CYCA(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
      OUT3 = OUT2
      OUT2 = OUT1
      OUT1 = OUT3
      TMS3 = TMS2
      TMS2 = TMS1
      TMS1 = TMS3
      WTR3 = WTR2
      WTR2 = WTR1
      WTR1 = WTR3
c      Write(*,*)' TMSF,Tms1,TMS2 ',TMSF,TMS1,TMS2
c      WRITE(*,*)'FEED,OUT1,OUT2',SUM(FEED),SUM(OUT1),SUM(OUT2)
      RETURN
      END
C
      SUBROUTINE CYCA(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      INTEGER CHOICE
      REAL CLAMBDA
      COMMON /FND501/D50,EXPONENT,REF_SPGR
      COMMON /FUNPF2/CLAMBDA
      EXTERNAL D501,PF3,PF2,PF1

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C   GENERAL EMPIRICAL MODELS FOR CLASSIFIERS.
C   See LG Austin RR KLIMPEL & PT Luckie PROCESS ENGINEERING OF SIZE
C   REDUCTION: BALL MILLING page 305
C
C     PARAMETERS IN ORDER (SI UNITS)
C     ************************************
C
C  1....By-pass fraction.
C  2....Sharpness index.
C  3....d50
C  4....EXPONENT FOR THE VARIATION OF D50 WITH DENSITY
C  5....Choice of model.
      REF_SPGR = PPROP(1)
      EXPONENT = PARAM(4)
      D50=PARAM(3)
      SI=PARAM(2)
      SCOS=PARAM(1)
      CHOICE=NINT(PARAM(5))
c Exponential-sum or Lynch model
      IF(CHOICE.EQ.1) CLAMBDA=1.099*(1.0+SI)/(1.0-SI)
c Rossin-Rammler model
      IF(CHOICE.EQ.2) CLAMBDA=-1.5725/ALOG(SI)
C Logistic model
      IF(CHOICE.EQ.3) CLAMBDA=-2.1972/ALOG(SI)
      LU=8
      RW = 0.05
      IF(SCOS .GT. 0.05) RW=SCOS
      IF(CHOICE.EQ.1)
     *CALL CLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF1,PPROP,LU)
      IF(CHOICE.EQ.2)
     *CALL CLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF2,PPROP,LU)
      IF(CHOICE.EQ.3)
     *CALL CLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF3,PPROP,LU)


c Calculate the water flows if water is added to the classifier.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WTR2 = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF

      RETURN
      END
C
C      
      SUBROUTINE ELUT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      REAL CLAMBDA,MU
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON /FUNPF2/CLAMBDA
      EXTERNAL PF3,SETVELN 
C
c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
      FL=1
C A recovery model for an elutriator based on the logistic model
C and the particle terminal settling velocity in water
c Parameters:
C      1...By-pass fraction.
C      2...Sharpness index.
C      3...V50, separation velocity, average velocity of liquid flow in
C          the elutriator's separation section.
C      4...Particle Sphericity. (This can be measured by I.A.)
C
      SCOS = PARAM(1)
      SI = PARAM(2)
      V50 = PARAM(3)
      PSI = PARAM(4)
C THIS IS THE LOGISTIC EXPONENT FOR SEPARATION SHARPNESS
      CLAMBDA = -2.1972/ALOG(SI)
      TMS1 = 0.0
C DENSITY OF WATER IN Kg/m3
      RHO = 1000.0
C VISCOSITY OF WATER IN Kg/m.sec
      MU = 0.001 
C CALCULATE THE RECOVERIES
      DO 10 I=1,NDC
        DP = SIZE(I)
	DO 10 J=1,NGC
          RHOS = PPROP(J)*1000.0
	  VTERM = SETVELN(DP,RHOS,RHO,MU,PSI)
	  X = VTERM/V50
	  PF = SCOS + (1.0 - SCOS)*PF3(X)
	  DO 10 K=1,NSC
	    OUT1(I,J,K)=FEED(I,J,K)*PF
	    DER1(I,J,K)=PF
	    OUT2(I,J,K) = FEED(I,J,K) - OUT1(I,J,K)
	    DER2(I,J,K)= 1.0 - PF
	    TMS1 = TMS1 + OUT1(I,J,K)
	    TMS2 = TMS2 + OUT2(I,J,K)
   10 CONTINUE
      WTR1 = PARAM(1)*WTR
      WTR2 = WTR - WTR1
      FL=1

c Calculate the water flows if water is added to the classifier.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WTR2 = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF
      RETURN
      END

      SUBROUTINE WICY(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C     Hydrocyclone with water injection at the apex.
C     Abandoned Honacker and implemented Bhaskar
C     
C     Based on Honacker R Q, Ozsever A V, Singh N, and Parekh B K.
C     Apex water injection for improved hydrocyclone classification effeiciency
C     Minerals Engineering 14 No 11 pp1445-1457, 2001
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

	Real Models(3)
	Real SlurryCapacity

	Common /WICYCommon/CycloWashRate,Models,SlurryCapacity

	EXTERNAL MWICY

c      Parameters:
c      1....Apex diameter
c      2....Truncated cone diameter
c      3....Vortex finder diameter
c      4....Feed pressure
c      5....Number of units in parallel
C
c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
      FL=0
C     Calculate the cyclowash water rate
C     Calculate the water flows if water is added to the cyclone.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTRF = WTR + WATERADD
        ELSE
          WTRF = TMSF*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTRF - WTR
        END IF
      ELSE
	  WATERADD = 0.0
      END IF
	Write(UnitDiagFile,*)' WATERADD',WATERADD
      CycloWashRate = 60*WATERADD/PARAM(5)

c	Write(UnitDiagFile,*)' TMSF,TMS1,TMS2',TMSF,TMS1,TMS2
c	Write(UnitDiagFile,*)' Sum FEED,OUT1, OUT2',SUM(FEED),SUM(OUT1),
c     *SUM(OUT2)

C  Split the FEED for multiple units in parallel and recombine the
C  products before exit.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MWICY,PARAM(5))

c     CLAUDIO******* Added this to balance the water flowrates 
c Calculate the water flows if water is added to the classifier.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR2 = WTR2 + WATERADD
        ELSE
          WTR2 = TMS2*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 + WTR2 - WTR
        END IF
      END IF
c  Return to MODSIM.
      RETURN
      END
C
C
      SUBROUTINE MWICY(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
c     ******************************************************************

      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      Real D50c,Imperfection
	Real Coeff(3,20),Factors(20),Models(3),Offset(3)

	Real VF, SPIG, P, IR, MDP
	Real SlurryCapacity

      EXTERNAL D501, PF2
      COMMON/FND501/D50c,EXPONENT,REF_SPGR
      REAL CLAMBDA
      COMMON/FUNPF2/CLAMBDA
	Common /WICYCommon/CycloWashRate,Models,SlurryCapacity

c     Patil - Rao model
c     This model does not allow for the effect of varying truncated cone diameter.
      CALL VOLFLW(FEED,DER3,NDC,NGC,NSC,PPROP,TM,TV)
	FeedVolRate = 60*(WTR + TV)
	Write(13,*)'FeedVolRate',FeedVolRate
	OFVolRate = 0.825*FeedVolRate - 29
	Write(13,*)'OFVolRate',OFVolRate

c     Honaker model
	EXPONENT = 0.5
	REF_SPGR = 1.6
	A = PARAM(1)
	B = PARAM(2)
	C = PARAM(3)
	D = CycloWashRate
	E = PARAM(4)
C	Write(UnitDiagFile,*)' A,B,C,D,E', A,B,C,D,E

c     Bhaskar model VF and SPIG should be in mm
      VF = 10*Param(3)
	SPIG = 10*Param(1)
	! P should be in kPa
	P = Param(4)
      ! Injection Rate IR in m3/h.  CycloWashRate is in kg/min
	IR = CycloWashRate*0.001*60
	Q=0.131 * (VF**0.569) * (SPIG**0.201) * (P**0.445) * (IR**0.177)
	SlurryCapacity = Q
	MDP = Exp(1.962 - 0.061*VF + 0.116*SPIG + 0.001*P - 0.066*IR)
	D50c = Exp(5.52 + 0.030*VF - 0.108*SPIG - 0.008*P + 0.005*IR)
      WRITE(13,*)'P,VF,SPIG',P,VF,SPIG
	WRITE(13,*)'Q,SCOS,D50c using Bhaskar',Q,MDP,D50c
	IF(MDP .LT. 0) Mdp = 0
	IF(D50c .LT. 1) D50c = 1
	SCOS=MDP/100

      Factors(1) = A
	Factors(2) = B
      Factors(3) = C
	Factors(4) = D
	Factors(5) = E
	Do I = 1,5
	  Factors(5+I) = Factors(I)**2
      End Do
	Factors(11) = A*B
	Factors(12) = A*C
	Factors(13) = A*D
	Factors(14) = A*E
	Factors(15) = B*C
	Factors(16) = B*D
	Factors(17) = B*E
	Factors(18) = C*D
	Factors(19) = C*E
	Factors(20) = D*E
      !Write(13,*)' Factors',Factors
	Offset(1) = 338.340479
	Coeff(1,1) = 27.165266
	Coeff(1,2) = -71.811078
	Coeff(1,3) = -108.089571
	Coeff(1,4) = -0.213930
	Coeff(1,5) = -1.669729
	Coeff(1,6) = 26.739532
	Coeff(1,7) = 7.19357
	Coeff(1,8) = 18.183327
	Coeff(1,9) = 0.022754
	Coeff(1,10) = 0.004214
	Coeff(1,11) = 13.137252
	Coeff(1,12) = -28.072270
	Coeff(1,13) = -0.474748
	Coeff(1,14) = -0.196498
	Coeff(1,15) = 6.563226
	Coeff(1,16) = -0.624164
	Coeff(1,17) = 0.188428
	Coeff(1,18) = 0.359901
	Coeff(1,19) = 0.115569
	Coeff(1,20) = -0.003181361

	Offset(2) = -80.161369
	Coeff(2,1) = -17.699651
	Coeff(2,2) = 37.563311
	Coeff(2,3) = 58.674585
	Coeff(2,4) = 0.100230
	Coeff(2,5) = 0.224232
	Coeff(2,6) = -3.623287
	Coeff(2,7) = -2.883522
	Coeff(2,8) = -4.733857
	Coeff(2,9) = -0.001081
	Coeff(2,10) = -0.001682
	Coeff(2,11) = -6.667431
	Coeff(2,12) = -0.982526
	Coeff(2,13) = -0.241664
	Coeff(2,14) = 0.223241
	Coeff(2,15) = -6.528745
	Coeff(2,16) = 0.115486
	Coeff(2,17) = -0.031747
	Coeff(2,18) = 0.045020
	Coeff(2,19) = -0.052760
	Coeff(2,20) = 0.001865151
	
	Offset(3) = -1.581388
	Coeff(3,1) = 0.82401
	Coeff(3,2) = 0.070431
	Coeff(3,3) = -0.188797
	Coeff(3,4) = 0.010293
	Coeff(3,5) = 0.010438
	Coeff(3,6) = 0.104175
	Coeff(3,7) = 0.034147
	Coeff(3,8) = 0.020316
	Coeff(3,9) = 0.000095
	Coeff(3,10) = 0.000001
	Coeff(3,11) = -0.214218
	Coeff(3,12) = 0.024388
	Coeff(3,13) = 0.006003
	Coeff(3,14) = -0.004064
	Coeff(3,15) = 0.042399
	Coeff(3,16) = -0.000474
	Coeff(3,17) = 0.000856
	Coeff(3,18) = -0.004415
	Coeff(3,19) = 0.000091
	Coeff(3,20) = -0.000117274

	Models = MATMUL(Coeff,Factors) + Offset

C     CLAUDIO  Abandoned Honeker in favor of Bhaskar for now
C      SCOS = 0.01*Models(1)
C      D50c = 1e-6*Models(2)
C     This is required for the report subroutine
      Models(1) = SCOS*100
	Models(2) = D50c
	! Adjust this to meters
      D50c = 1e-6*D50c
C     Imperfection is constant in Bhaskar's model
	CLAMBDA = 2.239
	! Need to calculate imperfection from its definition
	! for lambda = 2.239 I = 0.34383
	D75c = exp(log(log(0.25)/log(0.5))/2.239 +log(D50c))
	D25c = exp(log(log(0.75)/log(0.5))/2.239 +log(D50c))
	Imperfection = (D75c-D25c)/(2*D50c)
	Models(3) = Imperfection
	!Imperfection = Models(3)


 	!Calculate lambda for the Rosin-Rammler partition curve
	!from the value of imperfection
	!x = 0.5
	!Delta = 3
	!Do While (Abs(Delta) .GT. 0.001)
	!  DFDX =0.6931*2**x +0.8794*0.415**x
	!  Delta = - (2**x - 0.415**x - 2*Imperfection)/DFDX
	!	Write(UnitDiagFile,*)' x,DFDX,Delta ',x,DFDX,Delta
      !  x = x + Delta
      !End Do
	!CLAMBDA = 1/x

	RW = CycloWashRate /(60*WTR)

C	Write(UnitDiagFile,*)' TMSF,TMS1,TMS2',TMSF,TMS1,TMS2
C	Write(UnitDiagFile,*)' Sum FEED,OUT1, OUT2',SUM(FEED),SUM(OUT1),
C     *SUM(OUT2)
	CALL CLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER1,DER2,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF2,PPROP,LU)
	Write(UnitDiagFile,*)' TMSF,TMS1,TMS2',TMSF,TMS1,TMS2
	Write(UnitDiagFile,*)' Sum FEED,OUT1, OUT2',SUM(FEED),SUM(OUT1),
     *SUM(OUT2)
	
      CALL VOLFLW(OUT2,DER3,NDC,NGC,NSC,PPROP,TM,TV)
	Write(13,*)'TM,TV',TM,TV
	WTR2 = OFVolRate/60 - 1000*TV
      WTR1 = WTR - WTR2
	WTR3 = 0.0

      END
C
C
      SUBROUTINE CLASSI(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,
     *NGC,NSC,WTR,WTR1,WTR2,RW,SIZE,D50,SCOS,PARTFN,PPROP,LU)
C     *******************************************************
C
C A GENERAL SUBROUTINE FOR USE WITH CLASSIFICATION UNITS.
C  FUNCTIONS ARE REQUIRED FOR D50 AND THE PARTITION FUNCTION (PARTFN).
C  RECOVERY OF WATER AND THE SHORT CIRCUIT TO UNDER FLOW MUST BE
C  TRANSMITTED AS ARGUMENTS.
C  NOTE!!  Stream 2 is the oversize!
	USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC)
      REAL SIZE(NDC),PPROP(1)
      CHARACTER*7 UNITS
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      TMS2=0.0
      DO 10 NG=1,NGC
        SPGR=PPROP(NG)
        X50=D50(SPGR)
        R=SCOS
	  Write(UnitDiagFile,*)' SPGR,X50,SCOS',SPGR,X50,R
        DO 10 ND=1,NDC
          X=SIZE(ND)/X50
          PF=R+(1.0-R)*PARTFN(X)
          DO 10 NS=1,NSC
            OUT2(ND,NG,NS)=FEED(ND,NG,NS)*PF
            DER2(ND,NG,NS)=PF
            OUT1(ND,NG,NS)=FEED(ND,NG,NS)-OUT2(ND,NG,NS)
C      Because of floating point this may produce a negative result, although very small
		  IF(OUT1(ND,NG,NS) .LT. 0.0) OUT1(ND,NG,NS) = 0.0
            DER1(ND,NG,NS)=1.0-PF
            TMS2=TMS2+OUT2(ND,NG,NS)
   10 CONTINUE
      TMS1=TMSF-TMS2
	IF(TMS1 .LT. 0.0) TMS1 = 0.0
      WTR2=RW*WTR
      WTR1=WTR-WTR2
	IF(WTR1 .LT. 0.0) WTR1 = 0.0
      RETURN
      END
C
C
      FUNCTION PF1(X)
C     ***************
C  EXPONENTIAL SUM PARTITION FUNCTION FOR SEPARATION UNITS.
      REAL CLAMBDA
      COMMON/FUNPF2/CLAMBDA
      WS=CLAMBDA*(X-1.0)
      PF1=1.0
      IF(WS.LT.14.0) THEN
	WS1=EXP(-CLAMBDA)
        WS2=EXP(WS)
        PF1=(WS2-WS1)/(WS2-2.0*WS1+1.0)
      END IF
      RETURN
      END
C
C
      FUNCTION PF2(X)
C     ***************

c Set up common areas to get data from CALC.FOR.
      USE GLOBALS
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  Rosin-Rammler PARTITION FUNCTION FOR SEPARATION UNITS.
      REAL CLAMBDA
      COMMON/FUNPF2/CLAMBDA
      IF(X.LT.0.0) THEN
        WRITE(UnitDiagFile,1001)
 1001   FORMAT(' RELATIVE SIZE IS NEGATIVE IN CALL TO CLASSIFIER',
     *  ' FUNCTION PF2')
        PF2=0.0
        RETURN
      ELSE
	WS=0.693*(X)**CLAMBDA
        IF(WS.LT.14.0) THEN
          PF2=1.0-EXP(-WS)
        ELSE
          PF2=1.0
        END IF
      END IF
      RETURN
      END
C
C
      FUNCTION PF3(X)
C     ***************
C  LOGISTIC CLASSIFICATION FUNCTION.
C  See Austin Klimpel & Luckie  Process Engineering of Size Reduction
C  page 305.
      REAL CLAMBDA
      COMMON/FUNPF2/CLAMBDA
      PF3=1.0/(1.0+(X**(-CLAMBDA)))
      RETURN
      END
C
      FUNCTION D501(SPGR)
C     *******************
C  Calculate D50 as a function of density.
      COMMON/FND501/D50,EXPONENT,REF_SPGR
      D501=D50
      IF (EXPONENT .EQ. 0.0) RETURN
      D501=D50*((REF_SPGR - 1.0)/(SPGR - 1.0))**(EXPONENT)
      RETURN
      END
