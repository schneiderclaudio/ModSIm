C*******************     WSEPREPS.FOR     *****************************C
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
      SUBROUTINE RTHIC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
c Set up common areas to transmit data to unit model reports.
      COMMON /MODELDAT/UNIT
      COMMON/MINNAMES/NAMES
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
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
	LU=8
        ITERM=5
        CALL HEADER (LU,ITERM,NUNIT,'THICKENER',9,'THIC')
        WRITE(LU,1001) PARAM(1)
 1001 FORMAT(' Percentage solids in the underflow ',F6.1)
      RETURN
      END
C
      SUBROUTINE RKYNC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      USE GLOBALS
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)

      COMMON /MODELDAT/NUNIT
C
C Model of an ideal Kynch thickener with incompressible pulp. Settling
c velocity is modeled by an extended Wilhelm-Naide equation.

C       PARAMETERS
C
C     1...Thickener diameter.
c     2...Terminal settling velocity of an isolated floc.
C     3...Number of terms in the extended Wilhelm-Naide equation.
C     4...Alpha and beta pairs in the extended Wilhelm-Naide equation.
C
      DOUBLE PRECISION WALPHA(10),WBETA(10)
      LOGICAL OvLoad

      LU=8
      ITERM=5
      CALL HEADER (LU,ITERM,NUNIT,'THICKENER',9,'KYNC')

      Area = 0.25*3.14159*PARAM(1)**2
C Set up the parameters for the flux model.
      NTERMS = PARAM(3)
      Vt = PARAM(2)

      WRITE(LU,1001)PARAM(1),Area,1000*PARAM(2),NTERMS
 1001 FORMAT(/
     *' Thickener diameter',f7.1,' m','  Thickener area',F9.1,' m^2'/
     *' Terminal settling velocity of an isolated floc ',F9.2,' mm/s'/
     *' Extended Wilhelm-Naide function for settling velocity has ',
     *  I2,' terms')

       CALL SGM(FEED,NDC,NGC,NSC,PPROP,SpecVol,SpecGr)

C Calculate the feed flux.
      PsiF = TMSF/Area

c Set up the extended Wilhelm-Naide equation.
      DO I = 1,NTERMS
        WALPHA(I) = PARAM(2*(I-1) + 4)
        WBETA(I) = PARAM(2*(I-1) + 5)
      END DO

      WRITE(LU,1002)
 1002 FORMAT(/' Alpha, beta pairs in the Wilhelm-Naide function')
 1003 FORMAT(2G10.4)
      DO I = 1,NTERMS
        WRITE(LU,1003) WALPHA(I),WBETA(I)
      END DO
c Convert alphas to kg/m3.
      DO I = 1,NTERMS
        WALPHA(I) = WALPHA(I)/(1000.0**WBETA(I))
      END DO

c Calculate the point of inflection
	IF(NTERMS .EQ. 1) Then
	  CI = ((1 + WBETA(1))/(WALPHA(1)*(WBETA(1) - 1)))**(1.0/WBETA(1))
      ELSE
      !Start the calculation past the point of influence of the last term
	  K = NTERMS
   10 CONTINUE
	  WS = WALPHA(K)/WALPHA(K-1)
	  IF(WS .EQ. 0.0 .AND. K .GT. 2) THEN
	     K = K - 1
	     GOTO 10
	  END IF
	  IF (WS . EQ. 0.0) THEN
	  CI = ((1 + WBETA(1))/(WALPHA(1)*(WBETA(1) - 1)))**(1.0/WBETA(1))
	  ELSE
           WS = ALOG(WS)
           WS = WS/(WBETA(K-1) - WBETA(K))
           Cz = 10*EXP(WS)
	     CI = Cz
	  END IF
        CALL PointOfInflection(WALPHA,WBETA,NTERMS,Vt,CI)
      write(UnitDiagFile,'('' CI POI'',T20,F9.1,'' kg/m3'')')
     *CI
	END IF
      FluxAtPOI = Flux(CI,WALPHA,WBETA,NTERMS,Vt)
      SlopeAtPOI = DFluxDC(CI,WALPHA,WBETA,NTERMS,Vt)
      WRITE(LU,1004)CI,FluxAtPOI,SlopeAtPOI
 1004 FORMAT(/
     *'Point of inflection on the flux curve at ',F7.1,' kg/m^3'/
     *'Flux at point of inflection ',F8.4,' kg/m^2 s'/
     *'Slope of the flux curve at point of inflection ',G10.4,' m/s')

C Calculate maximum feed flux.
      PsiFMax = FluxAtPOI - CI*SlopeAtPOI

C Calculate the minimum discharge concentration
      CdMin = -PsiFMax/SlopeAtPOI
      VolFract = CdMin*SpecVol
      PercentSolids = 100*CdMin/(CdMin + (1.0-CdMin*SpecVol)*1000)
      WRITE(LU,1005) PsiFMax, CdMin,100*VolFract, PercentSolids
 1005 FORMAT(/
     *'Maximum possible feed flux ',F8.4,' kg/m^2 s'/
     *'Minimum underflow discharge concentration ',F7.1,' kg/m^3'/
     *' Equivalent volume concentation of solids ',F6.1,' %'/
     *' Equivalent mass percent solids ',F6.1,' %')

C Check that feed flux is within bounds.
      Ovload = .FALSE.
      WRITE(LU,1006)PsiF
 1006 FORMAT(/'Required feed flux for this application ',F8.4,
     *' kg/m^2 s')
      IF(PsiF .GT. PsiFMax) Then
        WRITE(LU,1007)
 1007   FORMAT(/
     *  '*****WARNING*****'/
     *  'The feed flux exceeds the maximum capacity of this thickener'/
     *  'and it will overflow solids.')
        qu = 0.9*0.001*WTR/Area
        CO = (PsiF - PsiFMax)/qu
        CALL OVERLOADFLUX(qu,WALPHA,WBETA,NTERMS,Vt,CO,PsiF,PsiFMax)
        VolFract = CO*SpecVol
        PercentSolids = 100*TMSF/(TMSF + WTR1)
        WRITE(LU,1010)CO,100*VolFract,PercentSolids,CdMin
 1010   FORMAT(
     *  'Solids concentration above feed well ',F7.1,' kg/m^3'/
     *  ' Equivalent volume concentation of solids ',F6.1,' %'/
     *  ' Equivalent mass percent solids ',F6.1,' %'/
     *  'Solids concentration below feed well ',F7.1,' kg/m^3'/)
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
        OvLoad = .TRUE.
        CALL GRAPHSETUP(NUNIT,Vt,NTERMS,WALPHA,WBETA,PsiFMax,CI,
     *  PsiFMax,Cm,CdMin,CO,OvLoad)
        RETURN

      END IF
      TMS1 = TMSF
      TMS2 = 0.0

C Calculate the lower conjugate concentration.
      Cm = 0.5*(CI + Cz)
      CALL Lower_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,Cm,CI)

C Calculate the discharge concentration.
      Cd = Cm*PsiF/(PsiF - Flux(Cm,WALPHA,WBETA,NTERMS,Vt))
c Calculate the underflow pumping rate
      Qu = TMSF/Cd
c Calculate the underflow water rate.
      SolidVol = TMSF*SpecVol
      WaterVol = Qu - SolidVol
      WTR1 = WaterVol*1000
      VolFract = Cd*SpecVol
      PercentSolids = 100*TMSF/(TMSF + WTR1)

      WRITE(LU,1008)Cm,Cd,100*VolFract,PercentSolids,Qu
 1008 FORMAT(/
     *'Lower conjugate concentration in the thickener ',F7.1,' kg/m^3'/
     *'Underflow discharge concentration ',F7.1,' kg/m^3'/
     *' Equivalent volume concentation of solids ',F6.1,' %'/
     *' Equivalent mass percent solids ',F6.1,' %'/
     *'Underflow pumping rate ',G10.4,' m^3 slurry/s')

c Calculate the upper conjugate concentration
      Cl = 0
      CALL Upper_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,Cl,Cd)

C Calculate the maximum on the flux curve.
      Cmax = 0.5*CI
      CALL MaxFlux(WALPHA,WBETA,NTERMS,Vt,Cmax)
      PsiMax = Flux(Cmax,WALPHA,WBETA,NTERMS,Vt)
      CO = 0.0
      CALL GRAPHSETUP(NUNIT,Vt,NTERMS,WALPHA,WBETA,PsiF,CI,
     *PsiMax,Cm,Cd,CO,OvLoad)

      RETURN
      END

      SUBROUTINE GRAPHSETUP(NUNIT,Vt,NTERMS,WALPHA,WBETA,PsiF,CI,
     *PsiMax,Cm,Cd,CO,OvLoad)
c     *********************************************************
      USE GLOBALS
      DOUBLE PRECISION WALPHA(*),WBETA(*)
      CHARACTER*36 TITLE
      LOGICAL  OvLoad

c Set up a file to take data to plot the flux curve for the Kynch model.
      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')'KYNC',NUNIT,Index

      WRITE(TITLE,'(''Operating plot for thickener unit'',I3)')NUNIT
      WRITE(21,'(A36)') TITLE
      WRITE(21,'(L1)') OvLoad
      WRITE(21,'(G10.4,I5)')Vt,NTERMS
      DO I = 1,NTERMS
        WRITE(21,'(G10.4,2x,G10.4)') WALPHA(I),WBETA(I)
      END DO
      WRITE(21,'(6(G10.4,2x))') PsiF,CI,PsiMax,Cm,Cd,CO
      CLOSE(21)

      RETURN
      END

      SUBROUTINE Upper_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,C,Cd)
C     ***************************************************************
C Calculates the lower conjugate concentration.
      DOUBLE PRECISION WALPHA(*),WBETA(*)

      ITER = 0
   10 CONTINUE
      ITER = ITER + 1
      F = PsiF*(1.0-C/Cd) - Flux(C,WALPHA,WBETA,NTERMS,Vt)
      DEL = F/(PsiF/Cd + DFluxDC(C,WALPHA,WBETA,NTERMS,Vt))
      C = C + DEL
      IF (ABS(DEL/C) .GT. 0.001 .AND. ITER .LT. 1000) GO TO 10
      RETURN
      END
C
      SUBROUTINE RADOR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      USE GLOBALS
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)

      COMMON /MODELDAT/NUNIT
C
C Model of an ideal Kynch thickener with compressible pulp. Settling
c velocity is modeled by an extended Wilhelm-Naide equation. Pulp compression is
c modeled by the Adorjan model.

C       PARAMETERS
C
C     1...Thickener diameter.
c     2...Terminal settling velocity of an isolated floc.
C     3...Number of terms in the extended Wilhelm-Naide equation.
C     4...Alpha and beta pairs in the extended Wilhelm-Naide equation.
C
      DOUBLE PRECISION WALPHA(10),WBETA(10)
      LOGICAL OvLoad

      LU=8
      ITERM=5
      CALL HEADER (LU,ITERM,NUNIT,'THICKENER',9,'ADOR')
      write(LU,*)' This model is not yet completely developed'


      Area = 0.25*3.14159*PARAM(1)**2
      RETURN
C Set up the parameters for the flux model.
 100  NTERMS = PARAM(6)
      Vt = PARAM(3)

      WRITE(LU,1001)PARAM(1),Area,1000*PARAM(2),NTERMS
 1001 FORMAT(/
     *' Thickener diameter',f7.1,' m','  Thickener area',F9.1,' m^2'/
     *' Terminal settling velocity of an isolated floc ',F9.2,' mm/s'/
     *' Extended Wilhelm-Naide function for settling velocity has ',
     *  I2,' terms')

       CALL SGM(FEED,NDC,NGC,NSC,PPROP,SpecVol,SpecGr)

C Calculate the feed flux.
      PsiF = TMSF/Area

c Set up the extended Wilhelm-Naide equation.
      DO I = 1,NTERMS
        WALPHA(I) = PARAM(2*(I-1) + 7)
        WBETA(I) = PARAM(2*(I-1) + 8)
      END DO

      WRITE(LU,1002)
 1002 FORMAT(/' Alpha, beta pairs in the Wilhelm-Naide function')
 1003 FORMAT(2G10.4)
      DO I = 1,NTERMS
        WRITE(LU,1003) WALPHA(I),WBETA(I)
      END DO

c Calculate the point of inflection
      !Start the caculation past the point of influence of the last term
	IF(NTERMS .EQ. 1) Then
	  CI = ((1 + WBETA(1))/(WALPHA(1)*(WBETA(1) - 1)))**(1.0/WBETA(1))
      ELSE
      !Start the calculation past the point of influence of the last term
        WS = LOG(WALPHA(NTERMS)/WALPHA(NTERMS - 1))
        WS = WS/(WBETA(NTERMS-1) - WBETA(NTERMS))
        Cz = 10*EXP(WS)
        CI = Cz
        CALL PointOfInflection(WALPHA,WBETA,NTERMS,Vt,CI)
	END IF
      FluxAtPOI = Flux(CI,WALPHA,WBETA,NTERMS,Vt)
      SlopeAtPOI = DFluxDC(CI,WALPHA,WBETA,NTERMS,Vt)
      WRITE(LU,1004)CI,FluxAtPOI,SlopeAtPOI
 1004 FORMAT(/
     *'Point of inflection on the flux curve at ',F7.1,' kg/m^3'/
     *'Flux at point of inflection ',F8.4,' kg/m^2 s'/
     *'Slope of the flux curve at point of inflection ',G10.4,' m/s')

C Calculate maximum feed flux.
      PsiFMax = FluxAtPOI - CI*SlopeAtPOI

C Calculate the minimum discharge concentration
      CdMin = -PsiFMax/SlopeAtPOI
      VolFract = CdMin*SpecVol
      PercentSolids = 100*CdMin/(CdMin + (1.0-CdMin*SpecVol)*1000)
      WRITE(LU,1005) PsiFMax, CdMin,100*VolFract, PercentSolids
 1005 FORMAT(/
     *'Maximum possible feed flux ',F8.4,' kg/m^2 s'/
     *'Minimum underflow discharge concentration ',F7.1,' kg/m^3'/
     *' Equivalent volume concentation of solids ',F6.1,' %'/
     *' Equivalent mass percent solids ',F6.1,' %')

C Check that feed flux is within bounds.
      Ovload = .FALSE.
      WRITE(LU,1006)PsiF
 1006 FORMAT(/'Required feed flux for this application ',F8.4,
     *' kg/m^2 s')
      IF(PsiF .GT. PsiFMax) Then
        WRITE(LU,1007)
 1007   FORMAT(/
     *  '*****WARNING*****'/
     *  'The feed flux exceeds the maximum capacity of this thickener'/
     *  'and it will overflow solids.')
        qu = 0.9*0.001*WTR/Area
        CO = (PsiF - PsiFMax)/qu
        CALL OVERLOADFLUX(qu,WALPHA,WBETA,NTERMS,Vt,CO,PsiF,PsiFMax)
        VolFract = CO*SpecVol
        PercentSolids = 100*TMSF/(TMSF + WTR1)
        WRITE(LU,1010)CO,100*VolFract,PercentSolids,CdMin
 1010   FORMAT(
     *  'Solids concentration above feed well ',F7.1,' kg/m^3'/
     *  ' Equivalent volume concentation of solids ',F6.1,' %'/
     *  ' Equivalent mass percent solids ',F6.1,' %'/
     *  'Solids concentration below feed well ',F7.1,' kg/m^3'/)
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
        OvLoad = .TRUE.
        CALL GRAPHSETUP(NUNIT,Vt,NTERMS,WALPHA,WBETA,PsiFMax,CI,
     *  PsiFMax,Cm,CdMin,CO,OvLoad)
        RETURN
      END IF
      TMS1 = TMSF
      TMS2 = 0.0

C Calculate the lower conjugate concentration.
      Cm = 0.5*(CI + Cz)
      CALL Lower_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,Cm,CI)

C Calculate the discharge concentration.
      Cd = Cm*PsiF/(PsiF - Flux(Cm,WALPHA,WBETA,NTERMS,Vt))
c Calculate the underflow pumping rate
      Qu = TMSF/Cd
c Calculate the underflow water rate.
      SolidVol = TMSF*SpecVol
      WaterVol = Qu - SolidVol
      WTR1 = WaterVol*1000
      VolFract = Cd*SpecVol
      PercentSolids = 100*TMSF/(TMSF + WTR1)

      WRITE(LU,1008)Cm,Cd,100*VolFract,PercentSolids,Qu
 1008 FORMAT(/
     *'Lower conjugate concentration in the thickener ',F7.1,' kg/m^3'/
     *'Underflow discharge concentration ',F7.1,' kg/m^3'/
     *' Equivalent volume concentation of solids ',F6.1,' %'/
     *' Equivalent mass percent solids ',F6.1,' %'/
     *'Underflow pumping rate ',G10.4,' m^3 slurry/s')

c Calculate the upper conjugate concentration
      Cl = 0
      CALL Upper_Conjugate_Conc(PsiF,WALPHA,WBETA,NTERMS,Vt,Cl,Cd)

C Calculate the maximum on the flux curve.
      Cmax = 0.5*CI
      CALL MaxFlux(WALPHA,WBETA,NTERMS,Vt,Cmax)
      PsiMax = Flux(Cmax,WALPHA,WBETA,NTERMS,Vt)
      CO = 0.0
      CALL GRAPHSETUP(NUNIT,Vt,NTERMS,WALPHA,WBETA,PsiF,CI,
     *PsiMax,Cm,Cd,CO,OvLoad)

      RETURN
      END


      SUBROUTINE RFILT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON /MODELDAT/NUNIT
C
C      SIMPLE MODEL FOR THE FILTER.  ALL SOLIDS LEAVE IN FILTER CAKE.
C
C       PARAMETERS
C       **********
C
C       1.........PERCENTAGE SOLIDS IN FILTER CAKE.
C
      LU=8
      ITERM=5
      CALL HEADER (LU,ITERM,NUNIT,'FILTER',6,'FILT')
      WRITE(LU,1001) PARAM(1)
 1001 FORMAT(' Percentage solids in the underflow ',F6.1)
      RETURN
      END
