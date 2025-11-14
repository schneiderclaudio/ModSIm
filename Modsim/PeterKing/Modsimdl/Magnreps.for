C*******************     MAGNREPS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE REPORT WRITER FOR MAGNETIC SEPARATORS MODELS C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE RWDMS(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
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
      COMMON /MODELDAT/NUNIT
C
C A simple recovery model for a wet drum magnetic concentrator
c Parameters:
C      1...Exponent on the volumetric fraction of non-magnetics to model
C          the recovery.
C      2...By-pass fraction
C      3...Exponential coefficient to reduce by-pass as size increases.

       ITERM=0
       LU=8

C  WRITE THE REPORT FILE.
      CALL HEADER(8,5,NUNIT,'WET MAGNETIC DRUM SEPARATOR',27,'WDMS')
      WRITE(8,1001)PARAM(1), PARAM(2), PARAM(3)
 1001 FORMAT('Parameters:'/
     &	     '  Recovery exponent:',T35,F7.3,/
     &	     '  By-pass fraction:',T35,F8.3,/
     &	     '  Exponential factor to reduce by-pass as size increases:'
     &		 ,F8.3)
      RETURN
      END
C
      SUBROUTINE RWDM2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
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
      REAL LAMBDA
      COMMON /MODELDAT/NUNIT      
C
C A recovery model for a wet drum magnetic concentrator based on WDMS
C and the Rosin-Rammler separation curve
c Parameters:
C      1...Sharpness Index.
C      2...The Gv 50% parameter.
C      3...By-pass fraction to non mags.
C      4...Exponential coefficient to reduce by-pass as size increases.
C      5...Water split to tail stream
C
      LU = 8
C Calculate the exponent, with S.I.
      LAMBDA = -1.5725/ALOG(PARAM(1))
C The default water split is 80% to the tails
      IF(PARAM(5).LT.0.0) PARAM(5) = 0.8
      IF(1.0.LT.PARAM(5)) PARAM(5) = 0.8
C
C  WRITE THE REPORT FILE.
      CALL HEADER(8,5,NUNIT,'WET MAGNETIC DRUM SEPARATOR',27,'WDM2')
      WRITE(LU,1001)PARAM(1), LAMBDA, PARAM(2), PARAM(3), PARAM(4),
     * PARAM(5)
 1001 FORMAT('Parameters:'/
     &	     '  Sharpness Index:'T35,F8.3,/
     &	     '  Rosin-rammler exponent:'T35,F8.3,/
     &	     '  Separation volumetric grade:'T35,F8.3,/
     &	     '  By-pass fraction:'T35,F8.3,/
     &	     '  Exponential factor to reduce by-pass as size increases:'
     &		F8.3,/
     &	     '  Water split to tail stream:'T35,F8.3/)
      WRITE(LU,1008)
 1008 FORMAT('Short circuit by size:'/
     &       '  Size        Short circuit'/
     &       ' microns')
      DO 30 I = 1,NDC
	ALPHA = PARAM(3)*EXP(-PARAM(4)*SIZE(I)/0.001)
        WRITE(LU,1009)SIZE(I)*1.0e-6,ALPHA
 1009   FORMAT(F8.0,10X,F6.3)
   30 CONTINUE
C
C CALCULATE THE SIZE DISTRIBUTION IN THE CONCENTRATE
	CALL PARTSZ(SIZE,OUT2,NDC,NGC,NSC,DER2,DER3,DER1)
	WRITE(LU,1002)
 1002   FORMAT(/5X,'Size distribution in CONCENTRATE')
	CALL WRSZDN(DER2,DER3,NDC,3,LU)
C Calculate the liberation spectrum in each size fraction in the concentrate
	CALL LIBSPR(OUT2,DER1,NDC,NGC,NSC)
	WRITE(LU,'('' Liberation spectra by size'')')
	WRITE(LU,1003) (SIZE(I),I=1,NDC)      
 1003   FORMAT('Part. Size:',25G10.4)
	DO 40 J=1,NGC
	   WRITE(LU,1004) J,(DER1(I,J,1),I=1,NDC)
   40   CONTINUE
 1004   FORMAT('G-Class ',I3,25G10.4)
C
C CALCULATE THE SIZE DISTRIBUTION IN THE TAILS
	CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
	WRITE(LU,1005)
 1005   FORMAT(/5X,'Size distribution in TAILS')
	CALL WRSZDN(DER2,DER3,NDC,3,LU)
C Calculate the liberation spectrum in each size fraction in the tails
	CALL LIBSPR(OUT1,DER1,NDC,NGC,NSC)
	WRITE(LU,'('' Liberation spectra by size'')')
	WRITE(LU,1006) (SIZE(I),I=1,NDC)      
 1006   FORMAT('Part. Size:',25G10.4)
	DO 50 J=1,NGC
	   WRITE(LU,1007) J,(DER1(I,J,1),I=1,NDC)
   50   CONTINUE
 1007   FORMAT('G-Class ',I3,25G10.4)
      RETURN
      END
C
      SUBROUTINE RWHIM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      RETURN
      END


      SUBROUTINE RDOFI(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C*
C*  Report writer for model DOFI
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

      EXTERNAL RMDOFI
C
C  SPLIT THE FEED FOR MULTIPLE SCREENS IN PARALLEL AND RECOMBINE THE 
C  PRODUCTS BEFORE EXIT.
      CALL       MULT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM,RMDOFI,PARAM(8))
      RETURN
      END
C
C
      SUBROUTINE RMDOFI(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,
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


c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'Wet high intensity magnetic separator'
     *,37,'DOFI')

      MULT = PARAM(8)
      WRITE(LU,'(/
     *''Parameters:''/
     *'' Magnetic field strength '',F6.3,'' Tesla''/
     *'' Saturation magnetization of matrix material '',F6.3,'' Tesla''/
     *'' Interstitial slurry velocity through matrix '',F6.3,'' m/s''/
     *'' Fractional loading on the matrix'',F6.3/
     *'' M50 for low susceptibility particles '',F12.5/
     *'' M50 for high susceptibility particles '',F12.5/
     *'' Residual water on magnetics before flushing '',F6.1/
     *'' Number of units in parallel '',I3)')(PARAM(I),I = 1,7),MULT

      RETURN
      END
