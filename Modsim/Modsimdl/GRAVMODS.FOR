C*******************     GRAVMODS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE MODSIM MODELS FOR GRAVITY SEPARATION UNITS   C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
      SUBROUTINE SPIR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
C   MODEL FOR THE SPIRAL CONCENTRATOR
C Reference: King R P, Juckes A H and Stirling P A A quantitative model
C            for the prediction of fine coal cleaning in a spiral
C            concentrator. Coal Preparation 1992 Vol 11 pp 51-66.

C
C   PARAMETERS:
C    1....RELATIVE SPLITTER POSITION FOR OUTER SPLITTER.
C    2....RELATIVE SPLITTER POSITION FOR INNER SPLITTER.
C    3....NUMBER OF SPIRALS IN PARALLEL.
C     INNER SPLITTER MUST BE SET EQUAL TO OUTER SPLITTER POSITION
C     IF NO MIDDLING PRODUCT IS TAKEN FROM THE SPIRAL.

      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      EXTERNAL MSPIR
C
C  SPLIT THE FEED FOR MULTIPLE UNITS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MSPIR,PARAM(3))
      RETURN
      END

      SUBROUTINE MSPIR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      REAL IC,LYC
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C     Note:  The inside is the tailings

      WREC(R) = 1.0 - MAX(0.972-1.026*R**7.173,0.0)
      R1=PARAM(1)
      R2=PARAM(2)
      IP=NINT(PARAM(3))

      !Terminal settling cutpoint
      X50=0.00186

c The cutpoint increases with feed rate. The rate of increase is linear
c according to Gallagher E, Ellis R, Pitt G, Partridge A C, Randell J K. The
c integration of a 300t/hr spral installation at the German Creek prparation
c plant. Coal Preparation 12(1993)163-186. This is consistent with the King
c Juckes and Stirling data.
      DELCUTP = 0.3*(TMSF*3.6 - 0.3)
      IF(DELCUTP .GT. 0.3) DELCUTP = 0.3
      IF(DELCUTP .LT. 0.0) DELCUTP = 0.0
      TMS1 = 0.0
      TMS2 = 0.0
      TMS3 = 0.0
      WTR2 = 0.0
      WTR3 = 0.0
      WR1=WREC(R1)
      WTR1=WTR*WR1
      IF(R1 .LT. R2) THEN
        WR2=WREC(R2)
        WTR3=WTR*WR2-WTR1
      END IF
      DO 60 K=1,NSC
        DO 50 I=1,NDC
          DP=SIZE(I)*1.0E6
          WS=DP**0.66
          IC=0.01+2.29/WS+0.5*(R1)**11
          CP=CUTPT(DP,R1) + DELCUTP
          WR=WR1
          IPASS=1
   10     CONTINUE
          IF(IPASS.EQ.2) THEN
            IC=0.01+2.29/WS+0.5*(R2)**11
            CP=CUTPT(DP,R2) + DELCUTP
            WR=WR2
          END IF
          DO 40 J=1,NGC
            X=SETVEL(SIZE(I),PPROP(J)*1000.0,1000.0,0.001)
            R=X/X50
            PS = LYC(R,0.05)
            P1=WR*PS
            R=PPROP(J)/CP
            P2=1.0 - LYC(R,IC)
            REC=P1+(1.0-PS)*P2
            IF(IPASS.EQ.1) THEN
              OUT1(I,J,K)=REC*FEED(I,J,K)
              TMS1=TMS1+OUT1(I,J,K)
              OUT2(I,J,K)=FEED(I,J,K)-OUT1(I,J,K)
              DER1(I,J,K) = REC
              DER2(I,J,K) = 1.0 -REC
            ELSE
              OUT3(I,J,K)=REC*FEED(I,J,K)-OUT1(I,J,K)
              DER3(I,J,K) = REC - DER1(I,J,K)
              TMS3=TMS3+OUT3(I,J,K)
              OUT2(I,J,K)=OUT2(I,J,K)-OUT3(I,J,K)
              DER2(I,J,K) = DER2(I,J,K) - DER3(I,J,K)
            END IF
   40     CONTINUE
          IF(R1 .LT. R2.AND.IPASS.EQ.1) THEN
            IPASS=2
            GO TO 10
          END IF
   50   CONTINUE
   60 CONTINUE
      FL = 1
      TMS2=TMSF-TMS1-TMS3
      WTR2=WTR-WTR1-WTR3
      END
C
      FUNCTION CUTPT(DP,RELSP)
C     ************************
C
C  CALCULATES THE CUTPOINT AS FUNCTION OF THE PARTICLE SIZE AND THE
C  RELATIVE SPLITTER POSITION. King Juckes Stirling model.
C  PARTICLE SIZE IN MICRONS.
      REAL INNCP
      IF(DP.LT.880.0) THEN
        INNCP=1.197*ALOG10(DP)**2 - 5.219*ALOG10(DP) + 7.248
        COEF=0.45+0.0005*DP
        CUTPT=INNCP*(1.0-COEF*(RELSP**5.4))
      ELSE
        COEF=0.5+0.000455*DP
        POWER=5.4+0.0083*(DP-880.0)
        CUTPT=2.26*(1.0-COEF*(RELSP**POWER))
        IF(CUTPT.LT.1.2) CUTPT=1.2
      END IF
      RETURN
      END
C
      SUBROUTINE SPLM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      REAL IC,LYC
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
	REAL rho50, Ep
C     Note:  The inside is the tailings
C
      WREC(R) = 1.0 - MAX(0.972-1.026*R**7.173,0.0)
C
      R1 = 0.7
	R2 = 0.5
	ISEC =NINT(PARAM(1))


c This is from Limn Unit Models, Revision IV This model does not take flowrates
c into account and does not allow for changing the product splitter position
c so this is a very limited model

      TMS1 = 0.0
      TMS2 = 0.0
      TMS3 = 0.0
      WTR2 = 0.0
      WTR3 = 0.0
      WR1=WREC(R1)
      WTR1=WTR*WR1
      IF(R1 .LT. R2) THEN
        WR2=WREC(R2)
        WTR3=WTR*WR2-WTR1
      END IF
      DO 60 K=1,NSC
        DO 50 I=1,NDC
          DP=SIZE(I)*1.0E6
	    IF(DP .GE. 1000) THEN 
			Ep = 0.103
	        rho50 = 1.62
	    ELSE 
		   IF(DP .GE. 350) THEN 
		      Ep = -0.029*Log(DP/1000)+0.103
	          rho50 = -0.007*Log(DP/1000) + 1.62
		   ELSE 
		      Ep = -0.246*Log(DP/1000) - 0.122
			  rho50 = -0.196*Log(DP/1000) + 1.43
		   END IF
	    END IF
	    IF(ISEC .EQ. 1) THEN
		   Ep = Ep + 0.05
	       rho50 = rho50 + 0.1
		END IF
          WR=WR1
          IPASS=1
   10     CONTINUE

          IF(IPASS.EQ.2) THEN
	      Ep = Ep + 0.07
	      rho50 = rho50+0.5
            WR=WR2
          END IF

          DO 40 J=1,NGC
            REC=(1.0/(1.0+exp(1.098*(rho50-PPROP(J))/Ep)))
C		  WRITE(UnitDiagFile,*)'DP, Ep, rho50, REC', DP,Ep, rho50, REC

            IF(IPASS.EQ.1) THEN
              OUT1(I,J,K)=REC*FEED(I,J,K)
              TMS1=TMS1+OUT1(I,J,K)
              OUT2(I,J,K)=FEED(I,J,K)-OUT1(I,J,K)
              DER1(I,J,K) = REC
              DER2(I,J,K) = 1.0 -REC
            ELSE
              OUT3(I,J,K)=REC*FEED(I,J,K)-OUT1(I,J,K)
              DER3(I,J,K) = REC - DER1(I,J,K)
              TMS3=TMS3+OUT3(I,J,K)
              OUT2(I,J,K)=OUT2(I,J,K)-OUT3(I,J,K)
              DER2(I,J,K) = DER2(I,J,K) - DER3(I,J,K)
            END IF
   40     CONTINUE
          IF(R1 .LT. R2.AND.IPASS.EQ.1) THEN
            IPASS=2
            GO TO 10
          END IF
   50   CONTINUE
   60 CONTINUE
      FL = 1
      TMS2=TMSF-TMS1-TMS3
      WTR2=WTR-WTR1-WTR3
      END

C
      SUBROUTINE LISP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
C   MODEL FOR THE SPIRAL CONCENTRATOR
C Reference: Li M, Wood CJ, and Davis JJ "A study of coal
c            washing spirals" Coal Preparations 1993 Vol 12 pp 1117-131.
C
C   PARAMETERS:
C    1....Relative splitter position for outer splitter.
C    2....Relative splitter position for inner splitter.
C    3....Number of spirals in parallel.
C     Inner splitter must be set equal to outer splitter position
C     if no middling product is taken from the spiral.

      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      EXTERNAL MLISP
C
C  SPLIT THE FEED FOR MULTIPLE SCREENS IN PARALLEL AND RECOMBINE THE 
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MLISP,PARAM(3))
      RETURN
      END

      SUBROUTINE MLISP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      REAL IC,LYC
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

C Use the King Juckes Stirling model for water split.
      WREC(R)=1.0 - MAX(0.972-1.026*R**7.173,0.0)
      R1=PARAM(1)
      R2=PARAM(2)
      IP=NINT(PARAM(3))

c The cutpoint increases slightly with feedrate and % solids in the feed.
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      QFLOW = TMSF*SVM + WTR*1.0E-3
      DELCUTP = 0.59*(1000.0*QFLOW - 1.2)
      IF(DELCUTP .GT. 0.35) DELCUTP = 0.35
      IF(DELCUTP .LT. -0.24) DELCUTP = -0.24
      TMS1=0.0
      TMS2=0.0
      TMS3=0.0
      WTR3=0.0
      WR1=WREC(R1)
      WTR1=WTR*WR1
      IF(R1 .LT. R2) THEN
        WR2=WREC(R2)
        WTR3=WTR*WR2-WTR1
      END IF
      DO 60 K=1,NSC
        DO 50 I=1,NDC
          DP=SIZE(I)*1.0E6
          CP=SG50(DP,R1) + DELCUTP
          EPM = 0.165 + 20.0/DP
          IC = EPM/CP
          WR=WR1
          IPASS=1
   10     CONTINUE
          IF(IPASS.EQ.2) THEN
            CP=SG50(DP,R2) + DELCUTP
            EPM = 0.165 + 20.0/DP
            IC = EPM/CP
            WR=WR2
          END IF
          DO 40 J=1,NGC
            R=PPROP(J)/CP
            REC=1.0 - LYC(R,IC)
            IF(IPASS.EQ.1) THEN
              OUT1(I,J,K)=REC*FEED(I,J,K)
              TMS1=TMS1+OUT1(I,J,K)
              OUT2(I,J,K)=FEED(I,J,K)-OUT1(I,J,K)
              DER1(I,J,K) = REC
              DER2(I,J,K) = 1.0 -REC
            ELSE
              OUT3(I,J,K)=REC*FEED(I,J,K)-OUT1(I,J,K)
              DER3(I,J,K) = REC - DER1(I,J,K)
              TMS3=TMS3+OUT3(I,J,K)
              OUT2(I,J,K)=OUT2(I,J,K)-OUT3(I,J,K)
              DER2(I,J,K) = DER2(I,J,K) - DER3(I,J,K)
            END IF
   40     CONTINUE
          IF(R1 .LT. R2.AND.IPASS.EQ.1) THEN
            IPASS=2
            GO TO 10
          END IF
   50   CONTINUE
   60 CONTINUE
      FL = 1
      TMS2=TMSF-TMS1-TMS3
      WTR2=WTR-WTR1-WTR3
      END
C
      FUNCTION SG50(DP,RELSP)
c     ***********************
C Calculation of the cutpoint on the spiral as a function of the relative
c splitter position and the particle size.
C The model was developed by Li M, Wood CJ, and Davis JJ "A study of coal
c washing spirals" Coal Preparations 1993 Vol 12 pp 1117-131.
      IF(DP .LT. 178) THEN
        CP = 2.2 - 0.0035*DP
      ELSE
        CP = 1.51 + 0.00037*DP
      END IF
      SP = RELSP*(227.5) + 100
      SG50 = CP + ((327.5-SP)/327.5)**3 - 0.052
      RETURN
      END

C
      SUBROUTINE KELL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
C Model for the SPIRAL CONCENTRATOR (Mineral Deposits Reichert MK10A coal
c spiral.
c Reference: Kelly EG, Gomer JS, Pillai KJ, Bull WR, and Spottiswood DJ.
c            Proc. XVI International Mineral Processing Congress
c            Ed. E Forssberg. Elsevier 1988 pp1771-1780

C
C   PARAMETERS:
C     1... Number of spirals in parallel.
C     This model assumes fixed cutter positions and does not make
C     any provision for control of the cut point.

      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      EXTERNAL MKELL
C
C  SPLIT THE FEED FOR MULTIPLE UNITS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MKELL,PARAM(1))
      RETURN
      END

      SUBROUTINE MKELL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      USE MSFLIB
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      REAL IC,LYC
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      REAL AK(5,9),AN(3)
      LOGICAL*4 response

      DATA AK/0.0411, -0.0243, -0.00275, 0.225, 0.00129,
     *5*0.0, -0.178, -0.051, 0.0042, 4.35, 0.0, 5*0.0,
     *0.0396, -0.0465, 0.000254, 0.121, 0.00752,
     *0.221, -.002, -0.00545, -1.890, 0.00155,
     *1.54, 0.099, -0.0019, 0.79, 0.0,
     *1.21, 0.036, 0.00717, 2.44, 0.0,
     *1.49, 0.0703, -0.00285, 3.54, 0.0/
      DATA AN/13.05, 11.17, 6.24/
      FS = TMSF*3.6
      PS = 100.0*TMSF/(TMSF + WTR)
      WSF1 = FS/PS
      WSF2 = FS*PS
      TMS1 = 0.0
C Calculate the recovery of water.
      RF =100.0 - ( 99.9 + 3.74*FS - 0.465*PS - 26.5*WSF1)
      IF(RF .LT. 0.0) THEN
        WRITE(UnitDiagFile,1001) NUNIT
 1001   FORMAT(/'ERROR*****Feedrate to spiral unit ',I3,
     *  ' is outside the range of the model KELL.'/
     *  'Increase the number of spirals.')
      END IF
      DO 30 I = 1,NDC
        IF(SIZE(I) .GT. 0.000850) L = 1
        IF(SIZE(I) .LE. 0.000850) L = 2
        IF(SIZE(I) .LE. 0.000300) L = 3
        IF(SIZE(I) .LE. 0.000106) L = 4
        DO 20 J = 1,NGC
          DO 10 K =1,NSC
            IF(L .LT. 4) THEN
C             Calculate the bypass fraction to outside.
              Y1 = AK(1,L) + AK(2,L)*FS + AK(3,L)*PS + AK(4,L)*WSF1
              IF(Y1 .LT. 0.0) Y1 = 0.0
              M = L + 3

C             Calculate the bypass fraction to inside.
              Y2 = AK(1,M) + AK(2,M)*FS + AK(3,M)*PS + AK(4,M)*WSF1
              IF(Y2 .LT. 0.0) Y2 = 0.0
              IF(Y1+Y2 .GT. 1.0) THEN
                IF(Y1 .GT. Y2) THEN
                  Y1 = 1.0 - Y2
                ELSE
                  Y2 = 1.0 - Y1
                END IF
              END IF

C             Calculate the cutpoint.
              M = L + 6
              SG50 = AK(1,M) + AK(2,M)*FS + AK(3,M)*PS + AK(4,M)*WSF1
              IF(SG50 .LT. 1.0) THEN
                WRITE(UnitDiagFile,1001)NUNIT
                response = COMMITQQ(UnitDiagFile)
                SG50 = 1.0
              END IF
c             Calculate the partition to inside.
              Y = 1.0 - EXP(-0.693*(PPROP(J)/SG50)**AN(L))
              PF = Y2 + (1.0 - Y1 - Y2)*Y
            ELSE
              PF = 0.01*RF
            END IF
            OUT1(I,J,K) = PF*FEED(I,J,K)
            OUT2(I,J,K) = (1.0-PF)*FEED(I,J,K)
            DER1(I,J,K) = PF
            DER2(I,J,K) = (1.0 - PF)
            TMS1 = TMS1 + OUT1(I,J,K)
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      FL = 1
      TMS2 = TMSF - TMS1
      WTR1 = 0.01*RF*WTR
      WTR2 = WTR - WTR1
      WTR3 = 0.0
      RETURN
      END

      SUBROUTINE KNEL (TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C*
C  A simple model for the Knelson concentrator based on the results of Andre
C  Laplante.  Only free gold and fine particles containing gold are
C  recovered. The recovery of free gold varies with the grain size.
C
C       PARAMETERS IN ORDER ARE:
C
C       1.....Maximum recovery of free gold.
C       2.....Recovery of gold-containing fines.
C       3.....Solids content of the concentrate in kg solids/liter of water.
C       4.....Number of units in parallel.
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

C
C  SPLIT THE FEED FOR MULTIPLE UNITS IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      EXTERNAL MKNEL
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MKNEL,PARAM(4))
      RETURN
      END
C
C
      SUBROUTINE MKNEL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      TMS2 = 0.0
      DO I = 1,NDC
        WS = ALOG10(1.0e6*SIZE(I))
        DO J = 1,NGC
          DO K = 1,NSC
            RECOVERY = 0.0
            IF(J .EQ. NGC) THEN
              RECOVERY = 0.01*PARAM(1) - 1.718 + 1.885*WS - 0.517*WS*WS
              IF(RECOVERY .LT. 0.0) RECOVERY = 0.0
            END IF
            IF(J .EQ. NGC-1) THEN
              RECOVERY = 0.01*PARAM(2)*(SIZE(NDC)/SIZE(I))**5
            END IF
            OUT2(I,J,K) = RECOVERY*FEED(I,J,K)
            OUT1(I,J,K) = FEED(I,J,K)*(1.0 - RECOVERY)
            TMS2 = TMS2 + OUT2(I,J,K)
          END DO
        END DO
      END DO
      TMS1 = TMSF - TMS2
      WTR2 = TMS2/PARAM(3)
      WTR1 = WTR - WTR2
c Calculate the water flows if water is added to the concentrator.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR + WTR2
        END IF
      END IF
      RETURN
      END

C
      SUBROUTINE CONE(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
c     ******************************************************************
C     Model for the Reichert cone using stratification.
      USE GLOBALS
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
C
C  SPLIT THE FEED FOR MULTIPLE CONES IN PARALLEL AND RECOMBINE THE
C  PRODUCTS BEFORE EXIT.
      EXTERNAL MCONE
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,MCONE,PARAM(2))
      RETURN
      END
C
      SUBROUTINE MCONE(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
c     ******************************************************************
C     Model for the Reichert cone using stratification.
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      COMMON /REICHERT/POC(2),SLOPE

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      INTEGER Configuration

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

      IF (Configuration .EQ. 1) THEN
        !Single SV cone
        slot = PARAM(4)
        POC(1) = PARAM(6)
        POC(2) = PARAM(7)
        SLOPE = PARAM(8)
        CALL SVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,
     *  NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot,alpha,PPROP,INDPP,NPP)
      END IF

      IF (Configuration .EQ. 2) THEN
        !DSV cone
        slot = PARAM(4)
        POC(1) = PARAM(6)
        POC(2) = PARAM(7)
        SLOPE = PARAM(8)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,
     *  NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot,alpha,PPROP,INDPP,NPP)
      END IF

      IF (Configuration .EQ. 3) THEN
        !2DSV cone configuration.
        slot1 = NINT(PARAM(4))
        slot2 = NINT(PARAM(5))
        POC(1) = PARAM(7)
        POC(2) = PARAM(8)
        SLOPE = PARAM(9)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,alpha,
     *  PPROP,INDPP,NPP)
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot2,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,2,2,NINT(PARAM(6)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      IF (Configuration .EQ. 4) THEN
        !3DSV cone configuration.
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        POC(1) = PARAM(8)
        POC(2) = PARAM(9)
        SLOPE = PARAM(10)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,alpha,
     *  PPROP,INDPP,NPP)
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot2,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,2,NINT(PARAM(7)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,3,NINT(PARAM(7)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      IF (Configuration .EQ. 5) THEN
        !4DSV cone configuration.
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        POC(1) = PARAM(9)
        POC(2) = PARAM(10)
        SLOPE = PARAM(11)
        CALL DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,alpha,
     *  PPROP,INDPP,NPP)
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot2,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,4,2,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,4,3,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot4,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,4,4,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      IF (Configuration .EQ. 6) THEN
C       DSVSV configuration
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        POC(1) = PARAM(7)
        POC(2) = PARAM(8)
        SLOPE = PARAM(9)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
      END IF

      IF (Configuration .EQ. 7) THEN
        !2DSVSV configuration
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        POC(1) = PARAM(9)
        POC(2) = PARAM(10)
        SLOPE = PARAM(11)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,slot4,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,2,2,NINT(PARAM(8)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      IF (Configuration .EQ. 8) THEN
        !3DSVSV configuration
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        slot5 = PARAM(8)
        slot6 = PARAM(9)
        POC(1) = PARAM(11)
        POC(2) = PARAM(12)
        SLOPE = PARAM(13)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,slot4,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,2,NINT(PARAM(10)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot5,slot6,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,3,NINT(PARAM(10)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      IF (Configuration .EQ. 9) THEN
        !2DSVSV.DSV configuration
        slot1 = PARAM(4)
        slot2 = PARAM(5)
        slot3 = PARAM(6)
        slot4 = PARAM(7)
        Slot5 = PARAM(8)
        POC(1) = PARAM(10)
        POC(2) = PARAM(11)
        SLOPE = PARAM(12)
        CALL DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *  PPROP,INDPP,NPP)
        TMS = TMS1
        DER3 = OUT1
        WTRF = WTR1
        CALL DSVSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot3,slot4,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,2,NINT(PARAM(9)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
        CALL DSVCONE(TMS,TMS1,TWS2,DER3,OUT1,DER2,OUT3,DER1,DER2,
     *  DER3,NDC,NGC,NSC,WTRF,WTR1,WS2,SIZE,slot5,alpha,
     *  PPROP,INDPP,NPP)
        CALL CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,
     *  DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,3,3,NINT(PARAM(9)),
     *  TMS,TWS2,WS2,NDC,NGC,NSC)
      END IF

      RETURN
      END

      SUBROUTINE SVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,
     *NDC,NGC,NSC,WTR,WTR1,WTR2,SIZE,slot,alpha,PPROP,INDPP,NPP)
c     **********************************************************
C     Model for a single variable (SV) segment of a Reichert cone using
C     stratification model.
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      INTEGER INDPP(NPP,2)

      COMMON /REICHERT/POC(2),SLOPE

      LOGICAL ReportFlag
      COMMON /REPORTING/ReportFlag


      FC = (POC(2) - SLOPE*(9 - slot)*POC(1))/(1 - SLOPE*(9 - slot))
      FC = FC/3.6
      IF(TMSF .GT. FC) THEN
         WS = POC(2) + SLOPE*(9 - slot)*(3.6*TMSF - POC(1))
         TMS2 = WS/3.6
      ELSE
         TMS2 = TMSF
         OUT2 = FEED
         WTR2 = WTR
         TMS1 = 0.0
         OUT1 = 0.0
         WTR1 = 0.0
         IF(ReportFlag) Write(21,'(''No flow to this stage'')')
         RETURN
      END IF
      hs = TMS2/TMSF
      IF(ReportFlag) THEN
        WRITE(8,'(''Concentrate split '',F6.3)')hs
      END IF

      WS = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            WS = WS + FEED(I,J,K)
          END DO
        END DO
      END DO


      IF(NGC .EQ. 2) THEN
        CALL BinStratify(FEED,DER2,DER1,NDC,NGC,NSC,SIZE,
     *  hs,alpha,PPROP,NPP,INDPP)
      END IF
      IF (NGC .GT. 2) THEN
        CALL N_Stratify(FEED,DER2,DER1,NDC,NGC,NSC,SIZE,
     *  hs,alpha,PPROP,NPP)
      END IF

      TMS1 = 0.0
      TMS2 = 0.0
      WS = 0.0
      OUT1 = DER1
      OUT2 = DER2
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            WS = WS + FEED(I,J,K)
            TMS1 = TMS1 + OUT1(I,J,K)
            TMS2 = TMS2 + OUT2(I,J,K)
          END DO
        END DO
      END DO
      WTR1 = TMS1*WTR/TMSF
      WTR2 = WTR - WTR1
      RETURN
      END

      SUBROUTINE DSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,SIZE,slot,alpha,PPROP,INDPP,NPP)
c     **********************************************************
C     Model for a DSV segment of a Reichert cone using
C     stratification model.
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      INTEGER INDPP(NPP,2)
      REAL, ALLOCATABLE :: TAILS(:,:,:)

      LOGICAL ReportFlag
      COMMON/REPORTING/ReportFlag

      ALLOCATE(TAILS(NDC,NGC,NSC))
      IF(ReportFlag) WRITE(21,'(''Double cone of DSV combination'')')
      SPLIT = 0.5*TMSF
      DER3 = 0.5*FEED
      WTRF = 0.5*WTR
      CALL SVCONE(SPLIT,TMS1,TMS2,DER3,DER1,DER2,DER1,DER2,
     *NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,5.0,alpha,PPROP,INDPP,NPP)

      TAILS = 2*DER1
      COMBINED = 2*TMS2
      DER3 = 2*DER2
      WTRF = 2*WTR2
      WS1 = 2*TMS1
      WS = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            WS = WS + TAILS(I,J,K)
          END DO
        END DO
      END DO
      IF(ReportFlag) WRITE(21,'(''Single cone of DSV combination'')')
      WTRM = 2*WTR1
      CALL SVCONE(COMBINED,TMS1,TMS2,DER3,DER1,DER2,DER1,DER2,
     *NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot,alpha,PPROP,INDPP,NPP)
      WS = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            WS = WS + DER1(I,J,K)
          END DO
        END DO
      END DO
      OUT1 = DER1 + TAILS
      WS = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            WS = WS + OUT1(I,J,K)
          END DO
        END DO
      END DO
      TMS1 = WS1 + TMS1
      WTR1 = WTRM + WTR1
      OUT2 = DER2
      WS = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            WS = WS + OUT1(I,J,K)
          END DO
        END DO
      END DO
      DEALLOCATE(TAILS)
      RETURN
      END

      SUBROUTINE DSVSVCONE(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,slot2,alpha,
     *PPROP,INDPP,NPP)
c     ******************************************************************
C     Model for a DSVSV segment of a Reichert cone using stratification model.
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      INTEGER INDPP(NPP,2)
      REAL, ALLOCATABLE :: TAILS(:,:,:)

      LOGICAL ReportFlag
      COMMON/REPORTING/ReportFlag

      ALLOCATE(TAILS(NDC,NGC,NSC))

      CALL DSVCONE(TMSF,TMS1,TMS2,FEED,DER1,DER2,OUT3,DER1,DER2,DER3,
     *NDC,NGC,NSC,WTRF,WTR1,WTR2,SIZE,slot1,alpha,PPROP,INDPP,NPP)
      TAILS = DER1
      WS1 = TMS1
      WTRM = WTR1
      DER3 = DER2
      WS3 = TMS2
      WS2 = WTR2
      IF(ReportFlag) Then
         WRITE(21,'(''Second single cone of DSVSV combination'')')
      End if
      CALL SVCONE(WS3,TMS1,TMS2,DER3,DER1,OUT2,DER1,DER2,
     *NDC,NGC,NSC,WS2,WTR1,WTR2,SIZE,slot2,alpha,PPROP,INDPP,NPP)
      OUT1 = TAILS + DER1
      TMS1 = WS1 + TMS1
      WTR1 = WTRM + WTR1
      WS = 0.0
      WS4 = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            WS = WS + OUT1(I,J,K)
            WS4 = WS4 + OUT2(I,J,K)
          END DO
        END DO
      END DO
      DEALLOCATE(TAILS)

      RETURN
      END

      SUBROUTINE CombineConeOutputs(TMSF,TMS1,TMS2,TMS3,FEED,
     *OUT1,OUT2,OUT3,DER1,DER2,DER3,WTRF,WTR1,WTR2,WTR3,
     *NStack,ThisN,MiddlingN,TMS,TWS2,WS2,NDC,NGC,NSC)
c     *******************************************************
c     Combines the outputs from Reichert cones in a stack.
c     Set up the next stage feed
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      INTEGER ThisN
      TMS = TMS1
      DER3 = OUT1
      WTRF = WTR1
c     Now combine the outputs
      IF(ThisN .GT. NStack-MiddlingN) THEN
        TMS3 = TMS3 + TWS2
        OUT3 = OUT3 + DER2
        WTR3 = WTR3 + WS2
      ELSE
        TMS2 = TMS2 + TWS2
        OUT2 = OUT2 + DER2
        WTR2 = WTR2 + WS2
      END IF
      RETURN
      END
C
      SUBROUTINE BinStratify(FEED,Heavy,Light,NDC,NGC,NSC,
     *SIZE,hs,alpha,PPROP,NPP,INDPP)
C     ****************************************************
C     Calculate the stratification in a binary mixture with no velocity
c     gradient.
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL Heavy(NDC,NGC,NSC),Light(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      INTEGER INDPP(NPP,2)

      LOGICAL ReportFlag
      COMMON /REPORTING/ReportFlag

C     PARAMETERS:
C     hs...Relative height of the cut
c     alpha...Specific stratification constant.

      C1f = 0.0
      C2f = 0.0
      DO I = 1,NDC
        DO K = 1,NSC
          C1f = C1f + FEED(I,1,K)/(PPROP(1)*1000)
          C2f = C2f + FEED(I,2,K)/(PPROP(2)*1000)
        END DO
      END DO
      WS = C1f + C2f
      IF(WS .LE. 0.0) RETURN
      C1f = C1f/WS
      C2f = C2f/WS
      beta12 = alpha*(pprop(1)*1000 - pprop(2)*1000)
      IF(C2f .LE. 0.0 ) THEN
        CRatio = 1.0
      ELSE
        CRatio= (1.0 - exp(-beta12*C1f))*exp(beta12)/(exp(beta12*C2f)-1)
      END IF
      DO I = 1,NDC
        DO K = 1,NSC
          WS1 = (1.0 + CRatio*exp(-beta12*hs))/(1.0 + CRatio)
          IF(CRatio .LE. 0.0) THEN
            WS2 = EXP(beta12*hs)
            REC1 = 0.0
          ELSE
            WS2 =(1.0 + 1.0/(CRatio*exp(-beta12*hs)))/(1.0 + 1.0/CRatio)
            REC1 = -ALOG(WS1)/(beta12*C1f)
          END IF
          IF(C2f .LE. 0.0) THEN
            REC2 = 0.0
          ELSE
            REC2 = ALOG(WS2)/(beta12*C2f)
          END IF
          Heavy(I,1,K) = REC1*FEED(I,1,K)
          Light(I,1,K) = FEED(I,1,K) - Heavy(I,1,K)
          Heavy(I,2,K) = REC2*FEED(I,2,K)
          Light(I,2,K) = FEED(I,2,K) - Heavy(I,2,K)
        END DO
      END DO
c If reporting write the stratification profiles.
      IF(ReportFlag) THEN
c       Construct the concentration profile
        DO N = 0,100
          V = 0.01*N
          WS3 = CRatio*exp(-beta12*V)
          C1h = WS3/(1 + WS3)
          C2h = 1.0 - C1h
          Rho = 1000.0/(C1h/PProp(1) + C2h/PPROP(2))
          WRITE(21,'(F6.3,F8.4,F8.4,F8.1)')V,C1h,C2h,RHO
        End DO
      END IF

      RETURN
      END

      SUBROUTINE N_Stratify(FEED,Heavy,Light,NDC,NGC,NSC,SIZE,
     *hs,alpha,PPROP,NPP)
C     ****************************************************************
C     Calculate the stratification in an n-component mixture with no velocity
c     gradient.
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL Heavy(NDC,NGC,NSC),Light(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      INTEGER INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      CHARACTER*18 FormatText
      LOGICAL ReportFlag
      COMMON /REPORTING/ReportFlag

      REAL, ALLOCATABLE :: C(:,:), RHOI(:)
      INTEGER, ALLOCATABLE :: JJ(:)
      REAL, ALLOCATABLE :: VFEED(:,:,:),PARTFN(:,:,:)

	Double precision CjR(0:100)
      REAL V(0:100),RHO(0:100), RHOM(0:100)

      LOGICAL CHANGE, REPEAT

      IF(.NOT.ALLOCATED(C)) ALLOCATE (C(0:100,NGC))
      IF(.NOT.ALLOCATED(RHOI)) ALLOCATE(RHOI(NGC))

      IF(.NOT.ALLOCATED(JJ)) ALLOCATE(JJ(NGC))
      IF(.NOT.ALLOCATED(VFEED)) ALLOCATE (VFEED(NDC,NGC,NSC))
      IF(.NOT.ALLOCATED(PARTFN)) ALLOCATE(PARTFN(NDC,NGC,NSC))

      CHANGE = .TRUE.


C     PARAMETERS:
C     1...Relative height of the cut, hs
c     2...Specific stratification constant, alpha.

C     Convert the feed to volume fraction and put the result in VFEED as volume
c     fractions.
      CALL VOLFLW(FEED,VFEED,NDC,NGC,NSC,PPROP,TM,TV)
      CTOT = 0.0
      VFEED = VFEED/TV

C     Sort the densities in ascending order.
      DO J = 1,NGC
        JJ(J) = J
        RHOI(J) = PPROP(J) *1000
      END DO
      DO WHILE (CHANGE)
        CHANGE = .FALSE.
        DO J = 1,NGC-1
          IF(PPROP(JJ(J)) .GT. PPROP(JJ(J+1))) THEN
            JM = JJ(J)
            JJ(J) = JJ(J+1)
            JJ(J+1) = JM
            CHANGE = .TRUE.
          END IF
        END DO
      END DO
c      write(UnitDiagFile,'(''Densities sorted in the following order''/
c     *(25I3))')(JJ(L),L = 1,NGC)


c     Set up the initial total density profile as the ideal Meyer profile.
c      WRITE(UnitDiagFile,*)'Initial ideal Meyer density profile'
      L = NGC
      CUMF = 0.0
      DO I = 1,NDC
        DO K = 1,NSC
          CUMF = CUMF + VFEED(I,JJ(L),K)
        END DO
      END DO
      DO 20 N = 0,100
        V(N) = 0.01*N
        IF(V(N) .LE. CUMF .OR. N .EQ. 100) THEN
          RHO(N) = RHOI(JJ(L))
        ELSE
          L = L - 1
          RHO(N) = RHOI(JJ(L))
          DO I = 1,NDC
            DO K = 1,NSC
              CUMF = CUMF + VFEED(I,JJ(L),K)
            END DO
          END DO
        END IF
c        WRITE(UnitDiagFile,*)' N,V(N), RHO(N)', N,V(N), RHO(N)
   20 CONTINUE
C
C Start the iterative process
      NITER = 0
    1 TV = 0.0           !TV is the integral of the average density.
      ERROR = 0.0
      REPEAT = .FALSE.
      NITER = NITER + 1

      DO 50 N = 0,100
        RHOM(N) = RHO(N)          !Remember the previous density profile.
   50 CONTINUE

C     Calculate the stratification profiles.
      PARTFN = 0.0
	C = 0.0
	Do J = 1,NGC
	  Call CRatio(alpha,RHOI(J),RHOM,CjR)
	  Do I = 1,NDC
	    Do K = 1,NSC
	      Do N = 0,100
	        C(N,J) = C(N,J) + VFEED(I,J,K)*CjR(N)
            End Do
	    End Do
	  End Do
	End Do

      DO N= 0,100
        RHO(N) = 0.0
        CTOT = 0.0
        DO 70 J = 1,NGC
          DO I = 1,NDC
            DO K = 1,NSC
              Heavy(I,J,K) = C(N,J)
              CTOT = CTOT + Heavy(I,J,K)
            END DO
          END DO
   70   CONTINUE
        !Calculate the density profile to compare
        DO 75 J = 1,NGC
          !Save the concentration profile in C(N,J) for reporting if necessary
	    C(N,J) = 0.0
          DO I = 1,NDC
            DO K = 1,NSC
              Heavy(I,J,K) = Heavy(I,J,K)/CTOT
              C(N,J) = C(N,J) + Heavy(I,J,K)
              RHO(N) = RHO(N) + Heavy(I,J,K)*RHOI(J)
              IF(N .LE. hs*100 ) THEN
                PARTFN(I,J,K)=PARTFN(I,J,K) + Heavy(I,J,K)
              END IF
            END DO
          END DO
   75   CONTINUE
        DRHO = RHO(N) - RHOM(N)
        IF(ABS(DRHO) .GT. 10) REPEAT=.TRUE.
        IF(ABS(DRHO) .GT. ERROR) THEN
          ERROR = ABS(DRHO)
          NERROR = N              !Point of greatest error.
        END IF
      END DO
c	Write(UnitDiagFile,*)' J,PARTFN(1,J,1)',(J,PARTFN(1,J,1),J =1,NGC)

c     Check the integral condition for Cjf   !Equation 7.39
      Do 76 J = 1,NGC
	  Cjf = 0.0
	  WS = C(0,J)
	  Do N = 1,99,2
          WS = WS + 4*C(N,J)
	  End Do
	  Do N = 2,98,2
          WS = WS + 2*C(N,J)
	  End Do
	  WS = WS + C(100,J)
	  WS = 0.01*WS/3
	  Do I = 1,NDC
	    Do K = 1,NSC
	      Cjf = Cjf + VFEED(I,J,K)
	    End Do
	  EndDo
        !....and adjust the partition function accordingly
	  Do I = 1,NDC
	    Do K = 1,NSC
            IF(VFEED(I,J,K) .GT. 0.0) then
              PARTFN(I,J,K) = PARTFN(I,J,K)/VFEED(I,J,K)
	      End IF
	      If(WS .GT. 0.0) PARTFN(I,J,K) = PARTFN(I,J,K)*Cjf/WS
	      If(PARTFN(I,J,K) .GT. 100.0) PARTFN(I,J,K) = 100.0
	      If(PARTFN(I,J,K) .LT. 0.0) PARTFN(I,J,K) = 0.0
	    End Do
	  End Do
c	write(13,*)'J,WS,Cjf',J,WS,Cjf
   76 Continue

c	Write(UnitDiagFile,*)' J,PARTFN(1,J,1)',(J,PARTFN(1,J,1),J =1,NGC)
      WRITE(UnitDiagfile,
     *'(''After '',I3,'' iterations the largest error in the '',
     *''density profile is'' ,F6.1,'' kg/m^3 at relative height '',
     *F4.2)')NITER,ERROR,NERROR*0.01
  500 IF(REPEAT .AND. NITER .LT. 25) THEN
        GO TO 1
      ELSE IF (NITER .EQ. 25) THEN
        WRITE(UnitDiagFile,'(''Stratification profile could not be '',
     *  ''established in 25 iterations.'')')
      END IF
      WRITE(UnitDiagFile,'(''Final stratification profile''/
     *''Relative height  Average density'')')
      DO N = 1,100
        WRITE(UnitDiagFile,'(F8.2,F21.1)')V(N), RHO(N)
      END DO
      DO J = 1,NGC
        DO I = 1,NDC
          DO K = 1,NSC
            Heavy(I,J,K) = 0.01*PARTFN(I,J,K)*FEED(I,J,K)
            Light(I,J,K) = FEED(I,J,K) - Heavy(I,J,K)
          END DO
        END DO
      END DO
c If reporting write the stratification profiles.
      IF(ReportFlag) THEN
       Write(UnitDiagFile,*)' Writing profile'
        WRITE(FormatText,'(''(F6.3,''I2,''F8.4,F8.1)'')')NGC
c       Construct the concentration profile
        DO N = 0,100
          WRITE(21,FormatText)V(N),(C(N,J),J = 1,NGC),RHO(N)
        end do
      END IF

      IF(ALLOCATED(C)) DEALLOCATE (C)
      IF(ALLOCATED(RHOI)) DEALLOCATE(RHOI)
      IF(ALLOCATED(JJ)) DEALLOCATE(JJ)
      IF(ALLOCATED(VFEED)) DEALLOCATE (VFEED)
      IF(ALLOCATED(PARTFN)) DEALLOCATE(PARTFN)

      RETURN
      END

      Subroutine CRatio(alpha,Rhoj,RhoBar,Cjh)
	!***************************************
	!Calculates the concentration ratio Cjh/Cjf in double precision to prevent over- and underflows
	!This ratio is equivalent to a combination of equations 7.38 and 7.40
        Real RhoBar(0:100)
	  Real Tv(0:100)
	  Double precision Gral, DWS
	  Double precision Cjh(0:100)
      
	  Tv(0) = 0.0
	  Do N = 1,100
	    Tv(N) = Tv(N-1) + 0.005*(RhoBar(N-1)+RhoBar(N))
	  End Do
	  DWS = alpha*Tv(0)
	  Gral = DEXP(DWS)
	  Do N = 1,99,2
	    DWS = -alpha*(Rhoj*0.01*N - Tv(N))
	    Gral = Gral + 4*DEXP(DWS)
        End Do
	  Do N = 2,98,2
	    DWS = -alpha*(Rhoj*0.01*N - Tv(N))
	    Gral = Gral + 2*DEXP(DWS)
        End Do
	  DWS = -alpha*(Rhoj - Tv(100))
	  Gral = Gral + DEXP(DWS)
	  Gral = 0.01*Gral/3

        Do N = 0,100
	    DWS = -alpha*(Rhoj*0.01*N - Tv(N))
	    Cjh(N) = DEXP(DWS)/Gral
        End Do
	End Subroutine  
	  

      SUBROUTINE N_StratifyV(FEED,Heavy,Light,NDC,NGC,NSC,SIZE,
     *hs,alpha,kappa,PPROP,NPP)
C     ****************************************************************
C     Calculate the stratification in an n-component mixture with velocity profile.
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC)
      REAL Heavy(NDC,NGC,NSC),Light(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      INTEGER INDPP(NPP,2)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

      CHARACTER*18 FormatText
      LOGICAL ReportFlag
      COMMON /REPORTING/ReportFlag

      REAL, ALLOCATABLE :: C(:,:), RHOI(:)
      INTEGER, ALLOCATABLE :: JJ(:)
      REAL, ALLOCATABLE :: VFEED(:,:,:),PARTFN(:,:,:)

	Double precision CjR(0:100)
	Double precision, ALLOCATABLE :: BCj0(:)
      Double precision sigma
      REAL V(0:100),RHO(0:100), RHOM(0:100),RhoInt(0:100)
	Real kappa
	Double precision DWS

      LOGICAL CHANGE, REPEAT

      IF(.NOT.ALLOCATED(C)) ALLOCATE (C(0:100,NGC))
      IF(.NOT.ALLOCATED(RHOI)) ALLOCATE(RHOI(NGC))
	IF(.NOT. ALLOCATED(BCj0)) ALLOCATE(BCj0(NGC))

      IF(.NOT.ALLOCATED(JJ)) ALLOCATE(JJ(NGC))
      IF(.NOT.ALLOCATED(VFEED)) ALLOCATE (VFEED(NDC,NGC,NSC))
      IF(.NOT.ALLOCATED(PARTFN)) ALLOCATE(PARTFN(NDC,NGC,NSC))

      CHANGE = .TRUE.


C     PARAMETERS:
C     1...Relative height of the cut, hs
c     2...Specific stratification constant, alpha.
c     3...Velocity profile constant kappa.

C     Convert the feed to volume fraction and put the result in VFEED as volume
c     fractions.
      CALL VOLFLW(FEED,VFEED,NDC,NGC,NSC,PPROP,TM,TV)
      CTOT = 0.0
      VFEED = VFEED/TV

C     Sort the densities in ascending order.
      DO J = 1,NGC
        JJ(J) = J
        RHOI(J) = PPROP(J) *1000
      END DO
      DO WHILE (CHANGE)
        CHANGE = .FALSE.
        DO J = 1,NGC-1
          IF(PPROP(JJ(J)) .GT. PPROP(JJ(J+1))) THEN
            JM = JJ(J)
            JJ(J) = JJ(J+1)
            JJ(J+1) = JM
            CHANGE = .TRUE.
          END IF
        END DO
      END DO
c      write(UnitDiagFile,'(''Densities sorted in the following order''/
c     *(25I3))')(JJ(L),L = 1,NGC)


c     Set up the initial total density profile as the ideal Meyer profile.
c      WRITE(UnitDiagFile,*)'Initial ideal Meyer density profile'
      L = NGC
      CUMF = 0.0
      DO I = 1,NDC
        DO K = 1,NSC
          CUMF = CUMF + VFEED(I,JJ(L),K)
        END DO
      END DO
      DO 20 N = 0,100
        V(N) = 0.01*N
        IF(V(N) .LE. CUMF .OR. N .EQ. 100) THEN
          RHO(N) = RHOI(JJ(L))
        ELSE
          L = L - 1
          RHO(N) = RHOI(JJ(L))
          DO I = 1,NDC
            DO K = 1,NSC
              CUMF = CUMF + VFEED(I,JJ(L),K)
            END DO
          END DO
        END IF
c        WRITE(UnitDiagFile,*)' N,V(N), RHO(N)', N,V(N), RHO(N)
   20 CONTINUE
C
C Start the iterative process
      NITER = 0
    1 CONTINUE          
      ERROR = 0.0
      REPEAT = .FALSE.
      NITER = NITER + 1

      DO 50 N = 0,100
        RHOM(N) = RHO(N)          !Remember the previous density profile.
   50 CONTINUE
	RhoInt(0) = 0.0             !RhoInt is the integral of the average density
	Do N = 1,100
	  RhoInt(N) = RhoInt(N-1) + 0.005*(RHO(N-1)+RHO(N))
	End Do

C     Calculate the stratification profiles.
      PARTFN = 0.0
	Sigma = 0.0
	Call BetaCi0(alpha,kappa,RHOI,RhoInt,BCj0,NGC)
	Do J = 1,NGC
	  Do I = 1,NDC
	    Do K = 1,NSC
	        Sigma = Sigma + VFEED(I,J,K)*BCj0(J)
	    End Do
	  End Do
	End Do
c	write(13,*)'Cj0',(VFEED(1,J,1)*BCj0(J)/Sigma, J = 1,NGC)
	Do N = 0,100
	  Do J = 1,NGC
	    C(N,J) = 0.0
	    DWS = -alpha*(RHOI(J)*0.01*N - RhoInt(N))
	    DWS = DEXP(DWS)
	    Do I = 1,NDC
	      Do K = 1,NSC
	        C(N,J) =C(N,J) + VFEED(I,J,K)*BCj0(J)*DWS/Sigma
	      End Do
	    End Do
        End Do
c	write(13,'(I3,9E9.2)')N,(C(N,J),J = 1,NGC)
      End Do

      DO N= 0,100
        RHO(N) = 0.0
        CTOT = 0.0
        DO 70 J = 1,NGC
          CTOT = CTOT + C(N,J)
   70   CONTINUE
        !Calculate the density profile to compare
        DO 75 J = 1,NGC
          !Save the concentration profile in C(N,J) for reporting if necessary
	    C(N,J) = C(N,J)/CTOT
          RHO(N) = RHO(N) + C(N,J)*RHOI(J)
   75   CONTINUE
        DRHO = RHO(N) - RHOM(N)
        IF(ABS(DRHO) .GT. 10) REPEAT=.TRUE.
        IF(ABS(DRHO) .GT. ERROR) THEN
          ERROR = ABS(DRHO)
          NERROR = N              !Point of greatest error.
        END IF
      END DO
c	Write(UnitDiagFile,*)' J,PARTFN(1,J,1)',(J,PARTFN(1,J,1),J =1,NGC)

      Ns = NINT(100*hs)
      Do 76 J = 1,NGC
	  WS = C(0,J)*veljig(0.0,kappa)
	  Do N = 1,100
          WS = WS + 0.005*
     * (C(N-1,J)*veljig(0.01*(N-1),kappa)+C(N,J)*veljig(0.01*(N),kappa))
	    IF(N .EQ. Ns) WS1 = WS
	  End Do
	  Cjf = 0.0
	  Do I = 1,NDC
	    Do K = 1,NSC
	      Cjf = Cjf + VFEED(I,J,K)
	      PARTFN(I,J,K) = WS1/WS
	    End Do
	  End Do
   76 Continue

c	Write(UnitDiagFile,*)' J,PARTFN(1,J,1)',(J,PARTFN(1,J,1),J =1,NGC)
      WRITE(UnitDiagfile,
     *'(''After '',I3,'' iterations the largest error in the '',
     *''density profile is'' ,F6.1,'' kg/m^3 at relative height '',
     *F4.2)')NITER,ERROR,NERROR*0.01
  500 IF(REPEAT .AND. NITER .LT. 50) THEN
        GO TO 1
      ELSE IF (NITER .EQ. 50) THEN
        WRITE(UnitDiagFile,'(''Stratification profile could not be '',
     *  ''established in 25 iterations.'')')
      END IF
      WRITE(UnitDiagFile,'(''Final stratification profile''/
     *''Relative height  Average density'')')
      DO N = 1,100
        WRITE(UnitDiagFile,'(F8.2,F21.1)')V(N), RHO(N)
      END DO
      DO J = 1,NGC
        DO I = 1,NDC
          DO K = 1,NSC
            Heavy(I,J,K) = PARTFN(I,J,K)*FEED(I,J,K)
            Light(I,J,K) = FEED(I,J,K) - Heavy(I,J,K)
          END DO
        END DO
      END DO
c If reporting write the stratification profiles.
      IF(ReportFlag) THEN
       Write(UnitDiagFile,*)' Writing profile'
        WRITE(FormatText,'(''(F6.3,''I2,''F8.4,F8.1)'')')NGC
c       Construct the concentration profile
        DO N = 0,100
          WRITE(21,FormatText)V(N),(C(N,J),J = 1,NGC),RHO(N)
        end do
      END IF

      IF(ALLOCATED(C)) DEALLOCATE (C)
      IF(ALLOCATED(RHOI)) DEALLOCATE(RHOI)
      IF(ALLOCATED(BCj0)) DEALLOCATE(BCj0)
      IF(ALLOCATED(JJ)) DEALLOCATE(JJ)
      IF(ALLOCATED(VFEED)) DEALLOCATE (VFEED)
      IF(ALLOCATED(PARTFN)) DEALLOCATE(PARTFN)

      RETURN
      END

      Function veljig(h,kappa)
	!***********************
	!Model for the velocity profile in a jig
	  Real kappa
	  veljig = kappa*h + (1 - kappa)*h*h
	END

      Subroutine BetaCi0(alpha,kappa,Rhoi,RhoBar,BCi0,NGC)
	!***************************************************
	!Calculates beta*Ci0/Cif in double precision to prevent over- and underflows
	  Real Rhoi(NGC)
        Real RhoBar(0:100)
	  Real Tv(0:100)
	  Double precision Gral, DWS,vh
	  Double precision BCi0(NGC)
	  Real kappa
      
	  DO J = 1,NGC
	    DWS = alpha*RhoBar(0)
	    vh = veljig(0.0,kappa)
	    Gral = DEXP(DWS)*vh
	    Do N = 1,99,2
	      DWS = -alpha*(Rhoi(J)*0.01*N - RhoBar(N))
	      vh = veljig(0.01*N,kappa)
	      Gral = Gral + 4*DEXP(DWS)*vh
          End Do
	    Do N = 2,98,2
	      DWS = -alpha*(Rhoi(J)*0.01*N - RhoBar(N))
	      vh = veljig(0.01*N,kappa)
	      Gral = Gral + 2*DEXP(DWS)*vh
          End Do
	    DWS = -alpha*(Rhoi(J) - RhoBar(100))
	    vh = veljig(1.0,kappa)
	    Gral = Gral + DEXP(DWS)*vh
	    Gral = 0.01*Gral/3

	    BCi0(J) = 1.0/Gral
	  End DO

	End Subroutine  

C
      SUBROUTINE PAN1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      RETURN
      END
