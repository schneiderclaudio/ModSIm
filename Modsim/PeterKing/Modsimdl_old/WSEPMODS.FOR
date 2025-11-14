C*******************     WSEPMODS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE MODSIM MODELS FOR WATER SEPERATION UNITS     C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
C
C
      SUBROUTINE THIC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C      ***************************************************************
C       THIS SUBROUTINE ASSUMES THAT ALL SOLIDS LEAVE IN UNDERFLOW
C       **************************************************************
C
C       PARAMETERS
C       **********
C
C       1.........PERCENTAGE SOLIDS IN UNDERFLOWW
C
C
      FRAC = PARAM(1)*0.01
      WTR1=(TMSF-TMSF*FRAC)/FRAC
      WTR2=WTR-WTR1
      TMS2=0.0
      TMS1=TMSF
      DO 10 I=1,NDC
        DO 10 J=1,NGC
          DO 10 K=1,NSC
            OUT1(I,J,K)=FEED(I,J,K)
            OUT2(I,J,K)=0.0
            DER1(I,J,K)=1.0
            DER2(I,J,K)=0.0
10    CONTINUE
      RETURN
      END
C
      SUBROUTINE KYNC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
      USE GLOBALS

      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C Model of an ideal Kynch thickener with incompressible pulp. Settling
c velocity is modeled by an extended Wilhelm-Naide equation.

C       PARAMETERS
C
C     1...Thickener diameter.
c     2...Terminal settling velocity of an isolated floc.
C     3...Number of terms in the extended Wilhelm-Naide equation.
C     4+...Alpha and beta pairs in the extended Wilhelm-Naide equation.
C
      REAL WALPHA(10),WBETA(10)

      Area = 0.25*3.14159*PARAM(1)**2
C Set up the parameters for the flux model.
      NTERMS = PARAM(3)
      Vt = PARAM(2)

       CALL SGM(FEED,NDC,NGC,NSC,PPROP,SpecVol,SpecGr)

C Calculate the feed flux.
      PsiF = TMSF/Area

c Set up the extended Wilhelm-Naide equation.
      DO I = 1,NTERMS
        WALPHA(I) = PARAM(2*(I-1) + 4)
        WBETA(I) = PARAM(2*(I-1) + 5)
      END DO

c Calculate the point of inflection
      !Start the caculation past the point of influence of the last term
	IF(NTERMS .EQ. 1) Then
	  CI = ((1 + WBETA(1))/(WALPHA(1)*(WBETA(1) - 1)))**(1.0/WBETA(1))
      ELSE
      !Start the calculation past the point of influence of the last term
        WS = ALOG(WALPHA(NTERMS)/WALPHA(NTERMS - 1))
        WS = WS/(WBETA(NTERMS-1) - WBETA(NTERMS))
        Cz = 10*EXP(WS)
        CI = Cz
        CALL PointOfInflection(WALPHA,WBETA,NTERMS,Vt,CI)
	END IF
      FluxAtPOI = Flux(CI,WALPHA,WBETA,NTERMS,Vt)
      SlopeAtPOI = DFluxDC(CI,WALPHA,WBETA,NTERMS,Vt)

C Calculate maximum feed flux.
      PsiFMax = FluxAtPOI - CI*SlopeAtPOI

C Calculate the minimum discharge concentration
      CdMin = -PsiFMax/SlopeAtPOI

C Check that feed flux is within bounds.
      IF(PsiF .GT. PsiFMax) Then
        qu = 0.9*0.001*WTR/Area
        CO = (PsiF - PsiFMax)/qu
        CALL OVERLOADFLUX(qu,WALPHA,WBETA,NTERMS,Vt,CO,PsiF,PsiFMax)
        TMS2 = (PsiF - PsiFMax)*Area
        DO I = 1,NDC
          DO J = 1,NGC
            DO K = 1,NSC
              OUT2(I,J,K) = FEED(I,J,K)*TMS2/TMSF
              OUT1(I,J,K) = FEED(I,J,K) - OUT2(I,J,K)
            END DO
          END DO
        END DO
        CALL SGM(OUT2,NDC,NGC,NSC,PPROP,SVM,SGA)
        WTR2 = 1000*(qu*Area - TMS2*SVM)
        TMS1 = TMSF - TMS2
        WTR1 = WTR - WTR2
        RETURN
      END IF
      TMS1 = TMSF
      TMS2 = 0.0

      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1, NSC
            OUT1(I,J,K) = FEED(I,J,K)
            OUT2(I,J,K) = 0.0
          END DO
        END DO
      END DO
      TMS1 = TMSF
      TMS2 = 0.0

C Calculate the lower conjugate concentration.
      Cm = 0.5*(CI + Cz)
      CALL Lower_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,Cm,CI)
C Calculate the discharge concentration.
      Cd = Cm*PsiF/(PsiF - Flux(Cm,WALPHA,WBETA,NTERMS,Vt))
c Calculate the underflow pumping rate
      Qu = PsiF*Area/Cd
c Calculate the underflow water rate.
      SolidVol = PsiF*Area*SpecVol
      WaterVol = Qu - SolidVol
      WTR1 = WaterVol*1000
      WTR2 = WTR - WTR1

      RETURN
      END

      FUNCTION Flux(C,WALPHA,WBETA,NTERMS,Vt)
C     *************************************
C Calculates the flux using the extended Wilhelm-Naide model.
      REAL WALPHA(*),WBETA(*)
      WS1 = 1.0
      DO I = 1,NTERMS
        WS1 = WS1 + WALPHA(I)*C**WBETA(I)
      END DO
      Flux = Vt*C/WS1
      RETURN
      END

      FUNCTION DFluxDC(C,WALPHA,WBETA,NTERMS,Vt)
C     ****************************************
C Calculates the first derivative of the flux.
      REAL WALPHA(*),WBETA(*)
      WS1 = 1.0
      WS2 = 0.0
      DO I = 1,NTERMS
        AC = WALPHA(I)*C**WBETA(I)
        ABC = AC*WBETA(I)
        WS1 = WS1 + AC
        WS2 = WS2 + ABC
      END DO
      DFluxDC = Vt*(1.0 - WS2/WS1)/WS1
      RETURN
      END

      FUNCTION D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC)
C     **************************************************
C Returns the second derivative of the flux and the third.
      REAL WALPHA(*),WBETA(*)
      WS1 = 1.0
      WS2 = 0.0
      WS3 = 0.0
      DWS1DC = 0.0
      DWS2DC = 0.0
      DWS3DC = 0.0
      DO I = 1,NTERMS
        AC = WALPHA(I)*C**WBETA(I)
        ABC = AC*WBETA(I)
        ABB_1C = ABC*(WBETA(I)-1)
        ABB1C = ABC*(WBETA(I)+1)
        ABB1B_1C = ABB1C*(WBETA(I)-1)
        WS1 = WS1 + AC
        WS2 = WS2 + ABC/C
        WS3 = WS3 + ABB1C/C
        DWS1DC = DWS1DC + ABC/C
        DWS2DC = DWS2DC + ABB_1C/(C*C)
        DWS3DC = DWS3DC + ABB1B_1C/(C*C)
      END DO
      D2FluxDC = -Vt*(WS1*WS3 - 2*C*WS2**2)/WS1**3
      D3FluxDC = -Vt*(DWS1DC*WS3 + WS1*DWS3DC -2*WS2**2 -4*C*WS2*DWS2DC)
      D3FluxDC = D3FluxDC/WS1**3
      RETURN
      END


      SUBROUTINE PointOfInflection(WALPHA,WBETA,NTERMS,Vt,C)
c     ****************************************************
C Find the point of inflection.
      REAL WALPHA(*),WBETA(*)

c Find a starting value for the Newton Raphson by getting past the zero
c in the third derivative....
      WS = D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC)
      DO WHILE (D3FluxDC .LT. 0.0)
        C = C - 50
        WS = D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC)
      END DO
C ...then get past the zero in the 2nd derivative...
      DO WHILE(WS .GT. 0.0)
        C = C - 5
        WS = D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC)
      END DO
c ...then come back past the zero in the 2nd derivative...
      DO WHILE(WS .LT. 0.0)
        C = C+1
        WS = D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC)
      END DO
c ...and finish off with a Newton-Raphson.
      ITER = 0
   10 CONTINUE
      ITER = ITER + 1
      F = D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC)
      DEL = -F/D3FluxDC
      C = C + DEL
      IF (ABS(DEL/C) .GT. 0.001 .AND. ITER .LT. 100) GO TO 10
      RETURN
      END

      SUBROUTINE Lower_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,C,CI)
C     ***************************************************************
C Calculates the lower conjugate concentration.
      REAL WALPHA(*),WBETA(*)

      ITER = 0
   10 CONTINUE
      ITER = ITER + 1
      F = Flux(C,WALPHA,WBETA,NTERMS,Vt)
     *    - C*DFluxDC(C,WALPHA,WBETA,NTERMS,Vt) - PsiF
      DEL = F/(C*D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC))
      CC = C + DEL
      IF(CC .LE. CI) THEN
        C = 0.5*(C + CI)
      ELSE
        C = CC
      END IF
      IF (ABS(DEL/C) .GT. 0.001 .AND. ITER .LT. 100) GO TO 10
      RETURN
      END

      SUBROUTINE MaxFlux(WALPHA,WBETA,NTERMS,Vt,C)
c     ******************************************
c Find the maximum in the flux curve.
      REAL WALPHA(*),WBETA(*)

	IF(NTERMS .EQ. 1) Then
	  C = (1.0/(WALPHA(1)*(WBETA(1) - 1.0)))**(1.0/WBETA(1))
      ELSE
        ITER = 0
   10   CONTINUE
        ITER = ITER + 1
        F = DFluxDC(C,WALPHA,WBETA,NTERMS,Vt)
        DEL = -F/D2FluxDC(C,WALPHA,WBETA,NTERMS,Vt,D3FluxDC)
	  Write(13,*)'C,DFlux,Del',C,F,Del
        C = C + DEL
        IF (ABS(DEL/C) .GT. 0.001 .AND. ITER .LT. 100) GO TO 10
	END IF
      RETURN
      END

      SUBROUTINE OVERLOADFLUX(qu,WALPHA,WBETA,NTERMS,Vt,C,PsiF,PsiFMax)
C     ***************************************************************
C     Calculates the overload concentration.
      REAL WALPHA(*),WBETA(*)

      ITER = 0
   10 CONTINUE
        ITER = ITER + 1
        F = PsiF - PsifMax - qu*C + Flux(C,WALPHA,WBETA,NTERMS,Vt)
        DEL = -F/(DFluxDC(C,WALPHA,WBETA,NTERMS,Vt) - qu)
        C = C + DEL
        IF (ABS(DEL/C) .GT. 0.001 .AND. ITER .LT. 100) GO TO 10
      RETURN
      END
C
      SUBROUTINE ADOR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
      USE GLOBALS
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C Model of an ideal Kynch thickener with compressible pulp using Adorjan's
C model of the momentum and material balance.
c Settling velocity can be modeled by the Richardson-Zakai equation or by an
c extended Wilhelm-Naide equation.
c Solid stress can be modeled by Adorjan, Shirato or Becker equation.

C    PARAMETERS
C
C  1...Thickener diameter.
C  2...Depth of compression zone.
c  3...Terminal settling velocity of an isolated floc.
c  4...Selector for settling velocity model. 1 = Richardson-Zakai  2 = Adorjan
c  5...selector for solid stress equation.  1 = Adorjan  2 = Shirato  3 = Becker
C  6...Number of terms in the extended Wilhelm-Naide equation.
C  7-16...Alpha and beta pairs in the extended Wilhelm-Naide equation.
c  17..Solid stress at mean sediment concentration (Adorjan).
c    ..Solid stress factor(Shirato or Becker)
c  18..Exponent for solid stress.(Adorjan, Shirato or Becker)
c  19..Critical concentration.
c  20..Ultimate concentration
C
      REAL WALPHA(10),WBETA(10)

      Area = 0.25*3.14159*PARAM(1)**2
      DEPTH = PARAM(2)
      Vt = PARAM(3)

       CALL SGM(FEED,NDC,NGC,NSC,PPROP,SpecVol,SpecGr)

C Calculate the feed flux.
      PsiF = TMSF/Area

C Set up the parameters for the flux model.
      SELECT CASE (NINT(PARAM(4)))
        CASE(2)
      ! Set up the extended Wilhelm-Naide equation.
        NTERMS = PARAM(6)
        DO I = 1,NTERMS
          WALPHA(I) = PARAM(2*(I-1) + 7)
          WBETA(I) = PARAM(2*(I-1) + 8)
        END DO

      ! Calculate the point of inflection
        !Start the caculation past the point of influence of the last term
        WS = ALOG(WALPHA(NTERMS)/WALPHA(NTERMS - 1))
        WS = WS/(WBETA(NTERMS-1) - WBETA(NTERMS))
        Cz = 10*EXP(WS)
        CI = Cz
        CALL PointOfInflection(WALPHA,WBETA,NTERMS,Vt,CI)
        FluxAtPOI = Flux(CI,WALPHA,WBETA,NTERMS,Vt)
        SlopeAtPOI = DFluxDC(CI,WALPHA,WBETA,NTERMS,Vt)
      END SELECT

C Calculate maximum feed flux.
      PsiFMax = FluxAtPOI - CI*SlopeAtPOI

C Calculate the minimum discharge concentration
      CdMin = -PsiFMax/SlopeAtPOI

C Check that feed flux is within bounds.
      IF(PsiF .GT. PsiFMax) Then
        qu = 0.9*0.001*WTR/Area
        CO = (PsiF - PsiFMax)/qu
        CALL OVERLOADFLUX(qu,WALPHA,WBETA,NTERMS,Vt,CO,PsiF,PsiFMax)
        TMS2 = (PsiF - PsiFMax)*Area
        DO I = 1,NDC
          DO J = 1,NGC
            DO K = 1,NSC
              OUT2(I,J,K) = FEED(I,J,K)*TMS2/TMSF
              OUT1(I,J,K) = FEED(I,J,K) - OUT2(I,J,K)
            END DO
          END DO
        END DO
        CALL SGM(OUT2,NDC,NGC,NSC,PPROP,SVM,SGA)
        WTR2 = 1000*(qu*Area - TMS2*SVM)
        TMS1 = TMSF - TMS2
        WTR1 = WTR - WTR2
        RETURN
      END IF

      TMS1 = TMSF
      TMS2 = 0.0
      OUT1 = FEED
      OUT2 = 0.0

C Calculate the lower conjugate concentration.
      Cm = 0.5*(CI + Cz)
      CALL Lower_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,Cm,CI)
C Calculate the discharge concentration at infinite depth.
      Cd = Cm*PsiF/(PsiF - Flux(Cm,WALPHA,WBETA,NTERMS,Vt))
c Calculate the underflow pumping rate.
      Qu = PsiF*Area/Cd
c Calculate the total volumetric flux
      q = Qu/Area

C Set up the model for the solid stress
      SELECT CASE (NINT(PARAM(5)))
        CASE(1)
        MODEL = 1
        Cs = PARAM(19)*SpecGr*1000
        Cinf = PARAM(20)*SpecGr*1000
      END SELECT

C Calculate the discharge concentration at top of the compression zone.
      Cs = 0.05*SpecGr*1000
      Cinf = 0.4855*SpecGr*1000
      X = 0.0
      DELTAX = DEPTH/50
      C = Cd
      DO I = 1,50
        WS = Flux(C,WALPHA,WBETA,NTERMS,Vt) - q*Cd + q*C
        WS1 = TauDash(MODEL,C,Cs,Cinf)*Flux(C,WALPHA,WBETA,NTERMS,Vt)
        CDASH = (SpecGr - 1.0)*9.81*C*WS/(SpecGr*WS1)
        X = X + DELTAX
        C = C + CDASH*DELTAX
      END DO
c Calculate the underflow water rate.
      SolidVol = PsiF*Area*SpecVol
      WaterVol = Qu - SolidVol
      WTR1 = WaterVol*1000
      WTR2 = WTR - WTR1

      RETURN
      END

      FUNCTION TauDash(Model,C,Cs,Cinf)
C     *********************************
C  Calculate the gradient of the solid stress.
      REAL M
      A = 182.6
      M = 1.61
      IF(C .LE. Cs) THEN
        TauDash = 0.0
      ELSE IF (C .LT. Cinf) THEN
        TauDash = A*M*(C - Cs)**(M-1)*(Cinf - Cs)/(Cinf - C)**(M+1)
      ELSE
        TauDash = HUGE(C)
      END IF
      RETURN
      END

C
      SUBROUTINE FILT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C      SIMPLE MODEL FOR THE FILTER.  ALL SOLIDS LEAVE IN FILTER CAKE.
C
C       PARAMETERS
C       **********
C
C       1.........PERCENTAGE SOLIDS IN FILTER CAKE.
C
C
      FRAC = PARAM(1)*0.01
      WTR1=(TMSF-TMSF*FRAC)/FRAC
       WTR2=WTR-WTR1
       TMS2=0.0
        TMS1=TMSF
       DO 10 I=1,NDC
       DO 10 J=1,NGC
       DO 10 K=1,NSC
       OUT1(I,J,K)=FEED(I,J,K)
       OUT2(I,J,K)=0.0
       DER1(I,J,K)=1.0
       DER2(I,J,K)=0.0
10     CONTINUE
      RETURN
      END
