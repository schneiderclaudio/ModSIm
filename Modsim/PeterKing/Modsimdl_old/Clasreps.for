C*******************     CLASREPS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE REPORT WRITER FOR CLASSIFIER UNITS           C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE RCYCL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL DER1(NDC,NGC,NSC),FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(ND
     *C,NGC,NSC),OUT3(1)
      REAL DER2(NDC,NGC,NSC),DER3(1),PARAM(*),PPROP(1),SIZE(1),GRDM(1),G
     *RDV(1)
      COMMON /MODELDAT/NUNIT
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
      VISCOSITY = PARAM(11)
      REL_VISCOSITY = VISCOSITY/0.001
      AN = PARAM(12)
C
C     CALCULATE VOLUME FLOWRATES IN FEED
C
      VS = 0.0
      RhoMin = PPROP(1)
      DO 12 J=1,NGC
        SM = 0.0
        DO 11 I=1,NDC
          DO 10 K=1,NSC
            SM = SM+FEED(I,J,K)
   10     CONTINUE
   11   CONTINUE
        IF(PPROP(J).LT.0.001) THEN
         WRITE(8,*)' PARTICLE TYPE ',J,' DOES NOT HAVE A PROPER DENSITY'
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
C  WRITE THE REPORT FILE.
	LU=8
        ITERM=5
        CALL HEADER(LU,ITERM,NUNIT,'Hydrocyclone',12,'CYCL')
        IP=PARALL
C
C  CALCULATE THE PRESSURE DROP ACROSS THE CYCLONE.
        WS=131.6*Q**1.78*EXP(0.0055*PHI)
        PD=WS/((DC**0.37)*(DI**0.94)*(H**0.28)*(DU*DU+DOV*DOV)**0.87)
C
        WRITE(LU,1006)DC,H,DI,DOV,DU,HEAD,IP
 1006	FORMAT(/' PARAMETERS:'/' Cyclone diameter ',T25,F7.3,' meters'/
     *	' Vortex-spigot distance ',T25,F7.3,' meters'/
     *	' Inlet diameter ',T25,F7.3,' meters'/
     *	' Vortex finder diameter ',T25,F7.3,' meters'/
     *	' Spigot diameter ',T25,F7.3,' meters'/
     *	' Feed head ',T25,F7.2,' meters'/
     *	' Cyclones in cluster ',T25,I3/)
        WRITE(LU,1007) Q
 1007   FORMAT(' Flowrate through each cyclone ',G10.3,' m**3 per sec.')
        WRITE(LU,1008)PHI
 1008   FORMAT(' Volumetric fraction of solids in the feed ',F9.3)
        WRITE(LU,'('' Viscosity of slurry'',F8.4,'' kg/ms'')')VISCOSITY
        WRITE(LU,'('' Specific gravity of slurry in separation zone '',
     *  F6.2)')RhoSl
        WRITE(LU,'('' Index for variation of d50 with density '',F6.2)')
     *  AN
C
        WRITE(LU,1010) PD
 1010   FORMAT(' Pressure drop across the hydrocyclone ',F6.1,' kPa')
        WRITE(LU,1009)F1,F2,F4
 1009   FORMAT(/
     *  'Plitt''s calibration parameters:'/
     *  '                        For D50',F7.2/
     *  '       For the sharpness factor',F7.2/
     *  '             For the flow split',F7.2)
        CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1050)
 1050   FORMAT(//5X,'Size distribution in feed'/)
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  CALCULATE CORRECTED EFFIENCY CURVE
C
      D50 = F1*2690.0*(DC**0.46)*(DI**0.6)*(DOV**1.21)*EXP(6.3*PHI)
      D50 = D50/(1000**0.5)
      WS = (DU**0.71)*(H**0.38)*(Q**0.45)
      D50 =D50/WS
C Adjust for the slurry viscosity
      D50 = D50*(REL_VISCOSITY)**0.35

      S = 3.79*((DU/DOV)**3.31)*(H**0.54)*((DU*DU+DOV*DOV)**0.36)*EXP(0.
     *54*PHI)
      S = F4*S/((HEAD**0.24)*(DC**1.11))
      WRITE(LU,'(/'' Volumetric flow split to underflow '',F6.3)') S
      RV = S/(S+1.0)
      AM = F2*2.96*EXP(-1.58*RV)*(DC*DC*H/Q)**0.15
	WRITE(LU,'('' Sharpness parameter lambda = '',F6.2)') AM
      WRITE(LU,'('' Sharpness index = '',F6.3)') exp(-1.572/AM)
      TMS1 = 0.0
      TMS2 = 0.0
      DO 22 I=1,NDC
        D = SIZE(I)*1.0E6
        DO 21 J=1,NGC
          D50J = D50/(ABS(PPROP(J)-RhoSl))**AN
          IF(I.EQ.1) WRITE(LU,101) J,D50J
  101     FORMAT(' D50c for particle type',I3,' is ',G10.4,' microns')
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
      IF(RF.LT.0.0) THEN
        RF=0.0
        WRITE(LU,2001)
 2001   FORMAT(/'The model has estimated that almost all water is'/
     *'excluded from the underflow. This indicates that the spigot'/
     *'diameter is too small for the solid load. You should specify'/
     *'more cyclones in the cluster or change the cyclone geometry.'/
     *'If you are simulating an existing plant, check your data and'/
     *'check output from other units in the plant which may be high.')
      END IF
   25 CONTINUE
      TMS1 =0.0
      DO 30 I=1,NDC
      DO 30 J=1,NGC
      DO 30 K=1,NSC
        C=0.0
        IF(FEED(I,J,K).NE.0.0) C=OUT1(I,J,K)/FEED(I,J,K)
        E = C+RF*(1.0-C)
        OUT1(I,J,K) = E*FEED(I,J,K)
        TMS1 = TMS1 + OUT1(I,J,K)
        OUT2(I,J,K) = FEED(I,J,K) - OUT1(I,J,K)
   30 CONTINUE
      TMS2 = TMSF-TMS1
      WTR1 = RF*WTR
      WTR2 = WTR-WTR1
        CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1060)
 1060   FORMAT(//5X,'Size distribution in underflow.')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
        CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1070)
 1070   FORMAT(//5X,'Size distribution in overflow.')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
        WRITE(LU,1080)RF
 1080   FORMAT(/' Water recovery to underflow = ',G10.3)

C CHECK FOR ROPING (MULAR AND BHAPPU "MINERAL PROCESSING PLANT DESIGN"
C ****AIME 1978 P384
      PU=TMS1/(TMS1+WTR1)
	S = 0.145-0.417/(2.65-SG+SG/PU)+0.0279*ALOG(TMS1/(PARALL*SG))
      IF(DU .LT. S) THEN
        WRITE(LU,1005)100*S
 1005 FORMAT(/' ***WARNING!***'/
     *' This cyclone will rope under these operating conditions'/
     *' according to the Mular-Jull criterion.'/
     *' The recommended underflow diameter from this correlation is ',
     *  F8.1,' cm')
      END IF

C Check for roping using the Concha criterion. Concha FA, Barrientos A
C Montero J and Sampaio R. Air core and roping in hydrocyclones. Preprints 8th
C European Symposium on Comminution, Stockholm May 1994.
      DUBDO = DU/DOV
      SIGMA = 75.0E-3
      VISC = 0.001
      DELP = PD*1000.0
      WS = 27030*VISC*(1.0+104.6/(DELP**2.61))*(DUBDO**(-0.2)) - 0.138
      DABDO = 303*SIGMA/WS
      DABDU = DABDO/DUBDO
      WRITE(LU,1085) DABDU
 1085 FORMAT(/'Ratio of air core to underflow diameter by Concha''s form
     *ula is ',F5.2)
      IF(DABDU .GT. 1.45) THEN
        WRITE(LU,1086)
 1086 FORMAT(' ***WARNING!***'/
     *'This cyclone will rope under these operating conditions'/
     *'according to the Concha criterion.'/
     *'The underflow diameter should be increased to alleviate this cond
     *ition.')
      ELSE IF(DABDU .GT. 1.2) THEN
        WRITE(LU,1087)
 1087 FORMAT(' ***WARNING!***'/
     *'This cyclone may rope under these operating conditions'/
     *'according to the Concha criterion.'/
     *'The underflow diameter should be increased to alleviate this cond
     *ition.')
      END IF

      !Write the actual classification for graphical display
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'CYCL',
     *FEED,OUT1,'Hydrocyclone',12)
      RETURN
      END
C
C
      SUBROUTINE RCYCB(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT
      INTEGER CHOICE
      REAL LAMBDA
      COMMON /FND501/D50,EXPONENT,REF_SPGR
      COMMON /FUNPF2/LAMBDA
      EXTERNAL D501,PF1,PF2,PF3
C
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
C  4....Exponent to describe variation of d50 with density.
C  5....Choice of model.
      REF_SPGR = PPROP(1)
      EXPONENT = PARAM(4)
      D50=PARAM(3)
      SI=PARAM(2)
      SCOS=PARAM(1)
      CHOICE=NINT(PARAM(5))
      IF(CHOICE.EQ.1) LAMBDA=1.099*(1.0+SI)/(1.0-SI)
      IF(CHOICE.EQ.2) LAMBDA=-1.5725/ALOG(SI)
      IF(CHOICE.EQ.3) LAMBDA=-2.1972/ALOG(SI)
C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'CLASSIFIER',10,'CYCB')
      WRITE(LU,101)PARAM(1),PARAM(2),PARAM(3)*1.0E6,PARAM(4)
  101 FORMAT(' PARAMETERS:'/
     *'   Short circuit to oversize',T50,F8.3/
     *'   Sharpness index',T50,F8.3/
     *'   D50',T50,F8.2,' microns'/
     *'   Exponent for variation of D50 with density ',T50,F8.3)
      IF(CHOICE.EQ.1) WRITE(LU,*)
     *' Exponential sum partition function was selected.'
      IF(CHOICE.EQ.2) WRITE(LU,*)
     *' Rosin-Rammler partition function was selected.'
      IF(CHOICE.EQ.3) WRITE(LU,*)
     *' Logistic partition function was selected.'

      RW=SCOS
      IF(CHOICE.EQ.1)
     *CALL RCLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF1,PPROP,LU)
      IF(CHOICE.EQ.2)
     *CALL RCLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF2,PPROP,LU)
      IF(CHOICE.EQ.3)
     *CALL RCLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF3,PPROP,LU)
      FL=1
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'CYCB',
     *FEED,OUT1,'Classifier',10)
      RETURN
      END
C
      SUBROUTINE RCYCA(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON /MODELDAT/NUNIT
      INTEGER CHOICE
      REAL LAMBDA
      COMMON /FND501/D50,EXPONENT,REF_SPGR
      COMMON /FUNPF2/LAMBDA
      EXTERNAL D501,PF1,PF2,PF3
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
C  4....Exponent to describe variation of d50 with density.
C  5....Choice of model.
      REF_SPGR = PPROP(1)
      EXPONENT = PARAM(4)
      D50=PARAM(3)
      SI=PARAM(2)
      SCOS=PARAM(1)
      CHOICE=NINT(PARAM(5))
      IF(CHOICE.EQ.1) LAMBDA=1.099*(1.0+SI)/(1.0-SI)
      IF(CHOICE.EQ.2) LAMBDA=-1.5725/ALOG(SI)
      IF(CHOICE.EQ.3) LAMBDA=-2.1972/ALOG(SI)
C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'CLASSIFIER',10,'CYCA')
      WRITE(LU,101)PARAM(1),PARAM(2),PARAM(3)*1.0E6,PARAM(4)
  101 FORMAT(' PARAMETERS:'/
     *'   Short circuit to oversize',T50,F8.3/
     *'   Sharpness index',T50,F8.3/
     *'   D50',T50,F8.2,' microns'/
     *'   Exponent for variation of D50 with density ',T50,F8.3)
      IF(CHOICE.EQ.1) WRITE(LU,*)
     *' Exponential sum partition function was selected.'
      IF(CHOICE.EQ.2) WRITE(LU,*)
     *' Rosin-Rammler partition function was selected.'
      IF(CHOICE.EQ.3) WRITE(LU,*)
     *' Logistic partition function was selected.'

      RW=SCOS
      IF(CHOICE.EQ.1)
     *CALL RCLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF1,PPROP,LU)
      IF(CHOICE.EQ.2)
     *CALL RCLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF2,PPROP,LU)
      IF(CHOICE.EQ.3)
     *CALL RCLASSI(TMSF,TMS2,TMS1,FEED,OUT2,OUT1,DER2,DER1,NDC,
     *NGC,NSC,WTR,WTR2,WTR1,RW,SIZE,D501,SCOS,PF3,PPROP,LU)
      FL=1
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'CYCA',
     *FEED,OUT1,'Classifier',10)
      RETURN
      END

C
      SUBROUTINE RELUT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      REAL LAMBDA,MU
      COMMON /MODELDAT/NUNIT      
C     COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON /FUNPF2/LAMBDA
      EXTERNAL PF3,SETVELN
C
C A recovery model for an elutriator based on the logistic model
C and the particle terminal settling velocity in water
C Parameters:
C      1...By-pass fraction.
C      2...Sharpness index.
C      3...V50, separation velocity, average velocity of liquid flow in
C          the elutriator's separation section.
C      4...Particle Sphericity.
C
      LU = 8
      SCOS = PARAM(1)
      SI = PARAM(2)
      V50 = PARAM(3)
      PSI = PARAM(4)
C  WRITE THE REPORT FILE.
      CALL HEADER(8,5,NUNIT,'ELUTRIATOR',10,'ELUT')
      WRITE(LU,1001)PARAM(1), PARAM(2), 1000*PARAM(3), PARAM(4)
 1001 FORMAT('Parameters:'/
     &       '   By-pass fraction = ',F8.3,/
     &       '   Sharpness index = ',F8.3,/
     &       '   Separation Velocity  = ',F8.2, ' mm/s'/
     &       '   Particle Sphericity = ',F8.3)
C      
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1002)
 1002 FORMAT(/5X,'Size distribution in FEED')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C Calculate the liberation spectrum in each size fraction in the feed
c	CALL LIBSPR(FEED,DER1,NDC,NGC,NSC)
c	WRITE(LU,'('' Liberation spectra by size'')')
c	WRITE(LU,1003) (SIZE(I),I=1,NDC)
c 1003   FORMAT('Part. Size:',25G10.4)
c	DO 10 J=1,NGC
c	   WRITE(LU,1004) J,(DER1(I,J,1),I=1,NDC)
c   10   CONTINUE
c 1004   FORMAT('G-Class ',I3,25G10.4)
C
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1005)
 1005 FORMAT(/5X,'Size distribution in UNDERFLOW')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C Calculate the liberation spectrum in each size fraction in the Underflow
c	CALL LIBSPR(OUT1,DER1,NDC,NGC,NSC)
c	WRITE(LU,'('' Liberation spectra by size'')')
c	WRITE(LU,1006) (SIZE(I),I=1,NDC)
c 1006   FORMAT('Part. Size:',25G10.4)
c	DO 20 J=1,NGC
c	   WRITE(LU,1007) J,(DER1(I,J,1),I=1,NDC)
c   20   CONTINUE
c 1007   FORMAT('G-Class ',I3,25G10.4)
C
      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1008)
 1008 FORMAT(/5X,'Size distribution in OVERFLOW')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C Calculate the liberation spectrum in each size fraction in the Overflow
c	CALL LIBSPR(OUT2,DER1,NDC,NGC,NSC)
c	WRITE(LU,'('' Liberation spectra by size'')')
c	WRITE(LU,1009) (SIZE(I),I=1,NDC)
c 1009   FORMAT('Part. Size:',25G10.4)
c	DO 30 J=1,NGC
c	   WRITE(LU,1010) J,(DER1(I,J,1),I=1,NDC)
c   30   CONTINUE
c 1010   FORMAT('G-Class ',I3,25G10.4)

	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'ELUT',
     *FEED,OUT1,'Elutriator',10)

      RETURN
      END


      SUBROUTINE RWICY(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,
     *FL,NPP,GRDM,GRDV,NMIN,NGCM)
C     ****************************************************************
C     Hydrocyclone with water injection at the apex.
C     Based on Honacker R Q, Ozsever A V, Singh N, and Parekh B K.
C     Apex water injection for improved hydrocyclone classification efficiency
C     Minerals Engineering 14 No 11 pp1445-1457, 2001
C
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      REAL CLAMBDA
      COMMON/FUNPF2/CLAMBDA
	Real Models(3)
	Common /WICYCommon/ CycloWashRate,Models

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
      Else
	  WATERADD = 0.0
      END IF
      CycloWashRate = 60*WATERADD/PARAM(5)

C  WRITE THE REPORT FILE.
      LU = 8
      CALL HEADER(LU,5,NUNIT,'Water injection cyclone',23,'WICY')
      WRITE(LU,1001)PARAM(1), PARAM(3), PARAM(4), PARAM(2)
 1001 FORMAT('Parameters:'/
     &       '   Apex diameter = ',F8.3,'cm'/
     &       '   Vortex finder diameter = ',F8.3,' cm'/
     &       '   Feed pressure  = ',F8.2, ' kPa'/
     &       '   Truncated cone diameter = ',F8.3,' cm')
	Write(LU,'(/''Wash water rate = '',F8.1,'' L/min'')')CycloWashRate
	Write(LU,'(/''There are '',I3,'' units in parallel'')')
     &NINT(PARAM(5))

	Write(LU,1002)Models(1), Models(2), Models(3)
 1002 FORMAT(/'Performance parameters calculated from the model',/
     *'   By-pass      ',F7.2,' %'/
     *'   D50c         ',F7.2,' micron'/
     *'   Imperfection ',F7.3)
      Write(LU,1003) CLAMBDA
 1003 FORMAT('   Exponent for Rosin-Rammler classification function',
     *F7.2)        
C      
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1004)
 1004 FORMAT(/5X,'Size distribution in FEED')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1005)
 1005 FORMAT(/5X,'Size distribution in UNDERFLOW')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
      CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1008)
 1008 FORMAT(/5X,'Size distribution in OVERFLOW')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)

c      Write(13,*)'FEED',(FEED(I,1,1),I=1,NDC)
	Call ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,'WICY',
     *FEED,OUT1,'Water injection cyclone',23)
c  Return to MODSIM.
      RETURN
      END
C

C
C
      SUBROUTINE RCLASSI(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,
     *NGC,NSC,WTR,WTR1,WTR2,RW,SIZE,D50,SCOS,PARTFN,PPROP,LU)
C     **************************************************************
C
C A GENERAL SUBROUTINE FOR USE WITH CLASSIFICATION UNITS.
C  FUNCTIONS ARE REQUIRED FOR D50 AND THE PARTITION FUNCTION (PARTFN).
C  RECOVERY OF WATER AND THE SHORT CIRCUIT TO UNDER FLOW MUST BE
C  TRANSMITTED AS ARGUMENTS.
C  NOTE!!  Stream 2 is the oversize!
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC)
      REAL SIZE(NDC),PPROP(1)
      CHARACTER*7 UNITS
      COMMON /MODELDAT/NUNIT
      TMS2=0.0
      DO 10 NG=1,NGC
        SPGR=PPROP(NG)
        X50=D50(SPGR)
          CALL SZSCAL(X50,UNITS,FACTR,IPWR)
          WS=X50*FACTR
c          WRITE(LU,1001)NG,WS,UNITS
 1001   FORMAT(' Corrected D50 for particles of type ',I4,' is ',F7.3,
     *1X,A7)
        R=SCOS
        DO 10 ND=1,NDC
          X=SIZE(ND)/X50
          PF=R+(1.0-R)*PARTFN(X)
          DO 10 NS=1,NSC
            OUT2(ND,NG,NS)=FEED(ND,NG,NS)*PF
            DER2(ND,NG,NS)=PF
            OUT1(ND,NG,NS)=FEED(ND,NG,NS)-OUT2(ND,NG,NS)
            DER1(ND,NG,NS)=1.0-PF
            TMS2=TMS2+OUT2(ND,NG,NS)
   10 CONTINUE
      TMS1=TMSF-TMS2
      WTR2=RW*WTR
      WTR1=WTR-WTR2
      RETURN
      END


      SUBROUTINE ClassificationFunc(NUNIT,SIZE,NDC,NGC,NSC,Model,
     *Feed,Oversize,UnitType,Length)
C     ***********************************************************
C Set up the data to plot the classification function
      USE GLOBALS
      REAL SIZE(NDC)
	REAL Feed(NDC,NGC,NSC),Oversize(NDC,NGC,NSC)
      CHARACTER*4 Model
      CHARACTER*38 TITLE
	Character*(Length) UnitType
	Logical Passed

	REAL, ALLOCATABLE :: C(:),SS(:)

      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')Model,NUNIT,Index
      WRITE(TITLE,'(''Classification function for unit '',I2,'' - '')')
     *NUNIT
      WRITE(21,*) TITLE//UnitType

	IF( .NOT. ALLOCATED(C)) ALLOCATE(C(NDC)) 
	IF( .NOT. ALLOCATED(SS)) ALLOCATE(SS(NDC)) 

	Passed = .FALSE.
	L = 0
      DO I = NDC , 1, -1
	  WSF = 0.0
	  WS1 = 0.0
	  Do J = 1, NGC
	    Do K = 1 ,NSC
            WSF = WSF + Feed(I,J,K)
	      WS1 = WS1 + Oversize(I,J,K)
	    End Do
        End Do
	  If( WSF .LE. 0.0) THEN
	    If (Passed) Then
            Exit 
	    End If
        Else 
          L = L + 1
	    C(L) = WS1/WSF
	    SS(L) = Size(I)
	    Passed = .TRUE.
        End if
      END DO

      WRITE(21,'(I4)') L
      WRITE(21,'(G10.4,2x,G10.4)') (SS(I),C(I),I = 1,L)

	IF( ALLOCATED(C)) DEALLOCATE(C) 
	IF( ALLOCATED(SS)) DEALLOCATE(SS) 
      CLOSE(21)
      RETURN
      END
