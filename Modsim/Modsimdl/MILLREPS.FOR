C*******************      MILLREPS.FOR     ****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE REPORT GENERATORS FOR TUMBLING MILLS.        C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
      SUBROUTINE RMILL(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/ EXFACTOR,TYPE

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
      TMS1=TMSF
      WTR1=WTR

c Calculate the water flows if water is added to the mill
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
        END IF
      END IF

      TYPE = 'Ball mill'
      CALL MREP(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
      RETURN
      END
C
      SUBROUTINE RRODM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE

      TYPE = 'Rod mill '
      EXFACTOR = PARAM(10)
      CALL MREP(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
      RETURN
      END
C
      SUBROUTINE RRODL(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE

      TYPE = 'ROD mill '
      EXFACTOR = PARAM(10)
      CALL MREP(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
      RETURN
      END
C
      SUBROUTINE RFAGT(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT

      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE

	TYPE = 'FAG withT'
      CALL RFAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
	END SUBROUTINE

      SUBROUTINE RFAGM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE

	TYPE = 'FAG mill  '
      CALL RFAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
	END SUBROUTINE


      SUBROUTINE RFAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE


      REAL MILL_POWER,MILL_VOLUME,LOAD_VOL
      LOGICAL*4 response

      CHARACTER*30 GraphTitle

      REAL FMU,FLAMBDA,LENGTH,KAPPA

      FL=1
      ALPHA=PARAM(1)
      BETA=PARAM(2)
      GAMMA=PARAM(3)
      DELTA=PARAM(4)
      S1 = PARAM(5)/60.0
      FMU = 0.001*PARAM(8)
      FLAMBDA = PARAM(9)
      TAU = PARAM(17)
	FMU = 0.001*PARAM(7)
	FLAMBDA = PARAM(8)
      HOLD_UP = TMSF*TAU*60
      DM = PARAM(13)
C     Particle breakage energy for large particles
	Eminf = PARAM(18)
	Dp0 = 0.00108
	Phi = 2.30

      IF (TYPE .EQ. 'FAG mill  ') then
        Model = 'FAGM'
        LU = 8
        CALL HEADER(LU,ITERM,NUNIT,'FAG MILL',8,'FAGM')
      Else IF(TYPE .EQ. 'FAG withT') THEN
        Model = 'FAGT'
        LU = 8
        CALL HEADER(LU,ITERM,NUNIT,'FAG Mill with trommel',21,'FAGT')
      END IF
      WRITE(LU,1006)BETA,GAMMA,DELTA,PARAM(6),PARAM(5),
     *ALPHA,1000*FMU,FLAMBDA
 1006 FORMAT(/
     *' PARAMETERS:'/
     *'  Parameters for breakage function'/
     *'   Beta:',T35,F7.3,/
     *'   Gamma:',T35,F7.3,/
     *'   Delta:',T35,F7.3,/
     *'   Phi at 5mm:',T35,F7.3/
     *'  Parameters for selection function'/
     *'   Selection function at 1 mm:',T35,F7.3,' 1/min'/
     *'   Alpha:',T35,F7.3,/
     *'   Mu:',T35,F7.3,' mm'/
     *'   Lambda:',T35,F7.3/)

      TONS=TMSF*3.6
      WRITE(LU,1010)PARAM(9), PARAM(10)
 1010 FORMAT(/
     *'  Pendulum parameters:'/
     *'    A',T35,F6.2,/
     *'    b',T35,F6.2)
      KAPPA = 0.5*45.7*0.1*PARAM(12)/(3*600.0)
      WRITE(LU,1011) PARAM(11), 10*PARAM(12),KAPPA
 1011 FORMAT(/
     *'  Largest attrition fragment:',T34,F6.2,' mm'/
     *'  Fines produced by pebble wear:',T35,F6.3,' %'/
     *'  Pebble wear rate at 45.7 mm:',T35,F7.4,' mm/s'/)
	WRITE(LU,1016)Eminf,PARAM(23),Phi
 1016 FORMAT(/'  Particle fracture energy parameters:'/
     *        '  Particle fracture energy of large particles',F8.2,
     *        ' J/kg'/
     *        '  Reference size',F7.2,' mm'/
     *        '  Exponent phi',F8.3)

      WRITE(LU,1012) PARAM(13),PARAM(14),PARAM(15),PARAM(16)
 1012 FORMAT(/
     *'  Mill diameter:',T35,F7.2,' m'/
     *'  Mill filling:',T35,F7.2,' %'/
     *'  Mill speed:',T35,F7.2,' % critical'/
     *'  Grate aperture:',T35,F7.2,' mm')
      WRITE(LU,1009)TAU, TONS, 0.001*HOLD_UP
1009  FORMAT(
     *'  Residence time:',T35,F7.2,' min'//
     *'Operating variables:'/
     *'  Tonnage through mill: ',T35,F7.1,' tons/hr.'/
     *'  Calculated hold up in mill: ',T35,F7.1,' tons')
      LOAD_VOL = 0.01*PARAM(14)
C Calculate the dimensions of the mill.
      CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
      FilledFract = 0.01*PARAM(14)
      Mill_Volume = HOLD_UP*SVM*(1.0 + WTR1/TMSF)/FilledFract
      Length = Mill_Volume/(0.25*3.142*PARAM(13)**2)
      WRITE(LU,'(''  Estimated mill length is '',T36,F6.1,'' m'')')
     *Length
      ConeLength = 0.0
      E = 0.4
      U = 1.0
      Jb = 0.0
      Steel_SG = 7.8
      Vol_Fraction _solids = TMSF*SVM/(TMSF*SVM + WTR*1000)
      ChargeDensity = SGA*(1 - E + E*U*Vol_Fraction_Solids)
     *+ Jb*(Steel_SG - SGA)*(1 - E)/FilledFract
     *+ E*U*(1 - Vol_Fraction_Solids)
      WRITE(LU,1015)ChargeDensity
 1015 FORMAT('  Calculated charge density',T35,F6.2,' t/m**3')

C Calculate the power drawn by the mill.
      PHIC = 0.01*PARAM(15)
      POW =  Mill_Power(Length, ConeLength, PARAM(13), PHIC,
     *ChargeDensity, FilledFract, 'Grate')
      WRITE(LU,1013)POW
 1013 FORMAT('  Mill will draw ',T34,F8.1,' kW')

      IF(TYPE .EQ. 'FAG withT') THEN
	  WRITE(LU,'(''  Trommel mesh size '',F8.2,'' mm'')') PARAM(21)
      END IF

C
C Report the size distribution in the feed.
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(/5X,'Size distribution in FEED')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)

C Report the size distribution in the product.
      CALL PARTSZ(SIZE,PROD,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1051)
 1051 FORMAT(/5X,'Size distribution in PRODUCT')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
      PSIZEP=PSIZEP*1.0E6
      PSIZEF=PSIZEF*1.0E6
      POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))
      POWERF=POWERF*TONS
      WRITE(LU,1052) PSIZEF,PSIZEP,POWERF
 1052 FORMAT(/'80% passing size in feed   ',T45,G8.3,' microns'/
     *        '80% passing size in product',T45,G8.3,' microns'/
     *'The power required by this mill can be estimated from the'/
     *'Bond work index for the ore.'/
     *'        Power required = ',F8.1,' x W.I.  kW'/)
C

      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')Model,NUNIT,Index
      WRITE(GraphTitle,'(''Selection function for mill '',I2)')NUNIT
      WRITE(21,'(A30)') GraphTitle
      WRITE(21,'(A6)') 'Austin'
      WRITE(21,'(I4)') NDC
      DO I = NDC,1,-1
	SI = Austins(SIZE(I),S1,ALPHA,FMU,FLAMBDA)
        SELI = SELF_BR_SEL(DM,PHIC,LOAD_VOL,SIZE(I),Eminf,Dp0,Phi)
        WRITE(21,'(G10.4,2X,G10.4)') SIZE(I),(SI + SELI)*60
      END DO
      CLOSE(21)
      RETURN
      END

      SUBROUTINE MREP(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2,
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
      REAL Mill_Volume, Length, Jb
      REAL Mill_Power
      CHARACTER*4 Model

      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE

 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta
C
      EXTERNAL AustinSel

      FL=1
      ALPHA=PARAM(1)
      BETA=PARAM(2)
      GAMMA=PARAM(3)
      DELTA=PARAM(4)
      S1 = PARAM(5)
	PHI5 = PARAM(6)
      OUT3(1,1,1)=PARAM(6)*(5.0E-3/SIZE(1))**DELTA
      TAU=PARAM(7)
      MU = 0.001*PARAM(8)
      LAMBDA = PARAM(9)
C Set SelUsesRep to TRUE in case this calls AustinSelBall
	SelUsesRep = 1
      IF(TYPE .EQ. 'ROD mill' ) THEN
	  A = PARAM(1)
	  S1 = A
        ALPHA=PARAM(2)
        MU = 0.001*PARAM(3)
        LAMBDA = PARAM(4)
        BETA=PARAM(5)
        GAMMA=PARAM(6)
        DELTA=PARAM(7)
        PHI5 = PARAM(8)
        OUT3(1,1,1)=PARAM(8)*(5.0E-3/SIZE(1))**DELTA
        TAU=PARAM(9)
	END IF
      IF(TYPE .EQ. 'FAG mill ') THEN
        TAU = PARAM(17)
        MU = 0.001*PARAM(7)
        LAMBDA = PARAM(8)
      END IF
C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      IF(TYPE .EQ. 'Ball mill') THEN
        Model = 'MILL'
        CALL HEADER(LU,ITERM,NUNIT,'BALL MILL',9,'MILL')
      ELSE IF(TYPE .EQ. 'Rod mill ') THEN
        Model = 'RODM'
        CALL HEADER(LU,ITERM,NUNIT,'ROD MILL',8,'RODM')
      ELSE IF(TYPE .EQ. 'ROD mill ') THEN
        Model = 'RODL'
        CALL HEADER(LU,ITERM,NUNIT,'ROD MILL',8,'RODL')
      ELSE IF(TYPE .EQ. 'FAG mill ') THEN
        Model = 'FAGM'
        CALL HEADER(LU,ITERM,NUNIT,'FAG MILL',8,'FAGM')
      END IF
      TONS=TMSF*3.6
      HU=TMSF*60.0*TAU/1000.0
      WRITE(LU,1006)BETA,GAMMA,DELTA,PHI5,OUT3(1,1,1),S1,
     *ALPHA,1000*MU,LAMBDA
 1006 FORMAT(/
     *' PARAMETERS:'/
     *'  Parameters for breakage function'/
     *'   Beta:',T35,F7.3,/
     *'   Gamma:',T35,F7.3,/
     *'   Delta:',T35,F7.3,/
     *'   Phi at 5mm:',T35,F7.3/
     *'   Phi at top size:',T35,F7.3/
     *'  Parameters for selection function'/
     *'   S1',T35,F7.3,' 1/min'/
     *'   Alpha:',T35,F7.3,/
     *'   Mu:',T35,F7.3,' mm'/
     *'   Lambda:',T35,F7.3/)

        IF(TYPE .EQ. 'Ball mill') WRITE(LU,1007)TAU, TONS, HU
 1007 FORMAT(
     *'  Residence time:',T35,F7.2,' min'//
     *' Calculated tonnage through mill ',T35,F7.2,' tons/hr.'/
     *' Calculated hold up in mill ',T35,F7.2,' tons'/)

        IF(TYPE .EQ. 'Rod mill ') WRITE(LU,1008)TAU, EXFACTOR, TONS, HU
 1008 FORMAT(
     *'  Residence time of water:',T35,F7.2,' min'/
     *'  Exponential factor for variation of transport velocity with siz
     *e',F7.2//
     *' Calculated tonnage through mill ',T35,F7.2,' tons/hr.'/
     *' Calculated hold up in mill ',T35,F7.2,' tons')

        IF(TYPE .EQ. 'ROD mill ') THEN
          WRITE(LU,1019)TAU, EXFACTOR, TONS, HU
 1019 FORMAT(
     *'  Residence time of water:',T35,F7.2,' min'/
     *'  Exponential factor for variation of transport velocity with size
     * ',F7.2//
     *' Calculated tonnage through mill ',T35,F7.2,' tons/hr.'/
     *' Calculated hold up in mill ',T35,F7.2,' tons')
          IF(PARAM(11) .EQ. 0) THEN
            WRITE(LU,*)' Liberation was not modeled'
          END IF
          IF(PARAM(11) .EQ. 1) THEN
            WRITE(LU,*)' Ljubljana model used for liberation'
            CALL WriteAMDGRPdata(NDC,NGC,SIZE,'LJUB','RODL',NUNIT)
          END IF
          IF(PARAM(11) .EQ. 2) THEN
            WRITE(LU,*) ' Beta function model used for liberation'
            CALL WriteAMDGRPdata(NDC,NGC,SIZE,'BETA','RODL',NUNIT)
          END IF
        END IF

        IF(TYPE .EQ. 'FAG mill ') THEN
          WRITE(LU,1010)PARAM(9), PARAM(10)
 1010     FORMAT(/
     *    '  Pendulum parameters:'/
     *    '    A',T35,F6.2,/
     *    '    b',T35,F6.2)
          KAPPA = 0.5*45.7*0.1*PARAM(12)/(3*600.0)
          WRITE(LU,1011) PARAM(11), 10*PARAM(12),KAPPA
 1011     FORMAT(/
     *    '  Largest attrition fragment:',T34,F6.2,' mm'/
     *    '  Fines produced by pebble wear:',T35,F6.3,' %'/
     *    '  Pebble wear rate at 45.7 mm:',T35,F7.4,' mm/s'/)
          WRITE(LU,1012) PARAM(13),PARAM(14),PARAM(15),PARAM(16)
 1012     FORMAT(/
     *    '  Mill diameter:',T35,F7.2,' m'/
     *    '  Mill filling:',T35,F7.2,' %'/
     *    '  Mill speed:',T35,F7.2,' % critical'/
     *    '  Grate aperture:',T35,F7.2,' mm')
          WRITE(LU,1009)TAU, TONS, HU
 1009     FORMAT(
     *    '  Residence time:',T35,F7.2,' min'//
     *    'Operating variables:'/
     *    '  Calculated tonnage through mill: ',T35,F7.1,' tons/hr.'/
     *    '  Calculated hold up in mill: ',T35,F7.1,' tons')

C Calculate the dimensions of the mill.
          HOLD_UP = TMSF*TAU*60
          CALL SGM(FEED,NDC,NGC,NSC,PPROP,SVM,SGA)
          FilledFract = 0.01*PARAM(14)
          Mill_Volume = HOLD_UP*SVM*(1.0 + WTR1/TMSF)/FilledFract
          Length = Mill_Volume/(0.25*3.142*PARAM(13)**2)
          WRITE(LU,'(''  Estimated mill length is '',T36,F6.1,'' m'')')
     *    Length
          ConeLength = 0.0
          E = 0.4
          U = 1.0
          Jb = 0.0
          Steel_SG = 7.8
          Vol_Fraction _solids = TMSF*SVM/(TMSF*SVM + WTR*1000)
          ChargeDensity = SGA*(1 - E + E*U*Vol_Fraction_Solids)
     *    + Jb*(Steel_SG - SGA)*(1 - E)/FilledFract
     *    + E*U*(1 - Vol_Fraction_Solids)
          WRITE(LU,1015)ChargeDensity
 1015     FORMAT('  Calculated charge density',T35,F6.2,' t/m**3')

C Calculate the power drawn by the mill.
          PHIC = 0.01*PARAM(15)
          POW =  Mill_Power(Length, ConeLength, PARAM(13), PHIC,
     *    ChargeDensity, FilledFract, 'Grate')
          WRITE(LU,1013)POW
 1013     FORMAT('  Mill will draw ',T34,F8.1,' kW')
        END IF
C
C Report the size distribution in the feed.
        CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1050)
 1050   FORMAT(/5X,'Size distribution in FEED')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)

C Report the size distribution in the product.
        CALL PARTSZ(SIZE,PROD,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1051)
 1051   FORMAT(/5X,'Size distribution in PRODUCT')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
        PSIZEP=PSIZEP*1.0E6
        PSIZEF=PSIZEF*1.0E6
        POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))
        POWERF=POWERF*TONS
        WRITE(LU,1052) PSIZEF,PSIZEP,POWERF
 1052   FORMAT(/'80% passing size in feed   ',T45,G8.3,' microns'/
     *          '80% passing size in product',T45,G8.3,' microns'/
     *  'The power required by this mill can be estimated from the'/
     *  'Bond work index for the ore.'/
     *  '        Power required = ',F8.1,' x W.I.  kW'/)
C

C Write a file with the data to draw the graph of the selection function.
      CALL SelFuncGrSetUp(AustinSel,NUNIT,SIZE,NDC,Model)

      RETURN
      END
C
C
      SUBROUTINE RSB16(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      REAL Mill_Volume, Length, Jb
      REAL Mill_Power
      CHARACTER*4 Model
      CHARACTER*30 GraphTitle
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE

C

      FL=1
      TAU=PARAM(137)
C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
	Model = 'SB16'
      CALL HEADER(LU,ITERM,NUNIT,'BALL MILL',9,'SB16')

      TONS=TMSF*3.6
      HU=TMSF*60.0*TAU/1000.0

      WRITE(LU,1007)TAU, TONS, HU
 1007 FORMAT(
     *'  Residence time:',T35,F7.2,' min'//
     *' Calculated tonnage through mill ',T35,F7.2,' tons/hr.'/
     *' Calculated hold up in mill ',T35,F7.2,' tons'/)

      Write(LU,'(/''Selection Function:'')')

      Do K = 1, 16
        Write(LU,1008)K, PARAM(K)
      End Do
 1008 FORMAT(' S(',I3,') =',F8.5, '    1/min.')

C Report the size distribution in the feed.
        CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1050)
 1050   FORMAT(/5X,'Size distribution in FEED')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)

C Report the size distribution in the product.
        CALL PARTSZ(SIZE,PROD,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1051)
 1051   FORMAT(/5X,'Size distribution in PRODUCT')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
        PSIZEP=PSIZEP*1.0E6
        PSIZEF=PSIZEF*1.0E6
        POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))
        POWERF=POWERF*TONS
        WRITE(LU,1052) PSIZEF,PSIZEP,POWERF
 1052   FORMAT(/'80% passing size in feed   ',T45,G8.3,' microns'/
     *          '80% passing size in product',T45,G8.3,' microns'/
     *  'The power required by this mill can be estimated from the'/
     *  'Bond work index for the ore.'/
     *  '        Power required = ',F8.2,' x W.I.  kW'/)

      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')Model,NUNIT,Index
      WRITE(GraphTitle,'(''Selection function for mill '',I2)')NUNIT
      WRITE(21,'(A30)') GraphTitle
      WRITE(21,'(A6)') 'Austin'
      WRITE(21,'(I4)') 15
      DO I = 15,1,-1
	  SI = PARAM(I)
        WRITE(21,'(G10.4,2X,G10.4)') SIZE(I),SI
      END DO
      CLOSE(21)
C

C Write a file with the data to draw the graph of the selection function.
C      CALL SelFuncGrSetUp(AustinSel,NUNIT,SIZE,NDC,Model)
      RETURN
      END
C
C Function to calculate mill power.
C *********************************

      FUNCTION Mill_Power(Length, ConeLength, Diameter, CriticalSpeed,
     * ChargeDensity, FilledFract, TYPE)
c     ****************************************************************
c Calculation of the power drawn by the mill using Steve Morrell's empirical
c model.  Morrell S., Trans. Instn. Min. Metall. 105 (1996) pp C54 - C62
c This function is identical to Mill_PowerM except that it incorporates the no-load
c power.
      REAL Mill_Power
      REAL Length, ConeLength, CriticalSpeed, ChargeDensity
      REAL FilledFraction, Diameter
      REAL NoLoadPower, Jt
      REAL K
      CHARACTER(*) TYPE   !Must be 'Overflow' or 'Grate'

      K = 9.10
      IF( TYPE .EQ. 'Overflow') K = 7.98
      phi = CriticalSpeed
      WS = phi*(0.667*ConeLength + Length)
      NoLoadPower = 1.68*Diameter**2.05*WS**0.82
      omega = 2*(2.9863*phi - 2.2129*phi*phi - 0.49267)
      Jt = FilledFract
      alpha = Jt*(omega - Jt)/(omega**2)
      phi_max = 0.954 - 0.135*Jt
      delta = phi*(1.0 - (1.0 - phi_max)*exp(-19.42*(phi_max-phi)))
      EffectiveLength = Length*(1 + 2.28*Jt*(1 - Jt)*ConeLength/Length)
      Mill_Power = NoLoadPower +
     * K*Diameter**2.5*EffectiveLength*ChargeDensity*alpha*delta
      RETURN
      END


      SUBROUTINE RSAGT(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT

      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE
C     
C	SAG MILL, no Pebble Port, with Trommel
C
	TYPE = 'SAG withT'
      CALL RSAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)

      END SUBROUTINE

      SUBROUTINE RSAGM(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)
C     *********************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      CHARACTER*50 TITLE

      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE
C     
C	SAG MILL, no Pebble Port, no Trommel
C
	TYPE = 'SAG mill  '
      CALL RSAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
	END SUBROUTINE

      SUBROUTINE RSAGP(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT

      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE
C     
C	SAG With Pebble Port, no Trommel
C
	TYPE = 'SAG PPort'
      CALL RSAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)

      END SUBROUTINE

      SUBROUTINE RSAGL(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT

      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE
C     
C	SAG WITH PEBBLE PORT AND TROMMEL
C
	TYPE = 'SAG PPo T'
      CALL RSAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,
     *DER1,DER2,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,
     *PPROP,INDPP,FL,NPP,GRDM,GRDV,NMIN,NGCM)

      END SUBROUTINE


      SUBROUTINE RSAG(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      USE GLOBALS
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      INTEGER FL,INDPP(NPP,2)
      COMMON /MODELDAT/NUNIT
      CHARACTER*9 TYPE
      COMMON /MILL_TYPE/EXFACTOR,TYPE

      LOGICAL*4 response

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

      CHARACTER*4 Model
      CHARACTER*30 GraphTitle


      REAL, ALLOCATABLE::MEDIA(:,:,:)
      REAL, ALLOCATABLE::Zeta(:),GammaI(:),Self(:),Sel(:),Classi(:)
      REAL, ALLOCATABLE::SelfBreakageFunction(:),Sela(:)


      ALLOCATE(MEDIA(NDC,NGC,NSC))
      ALLOCATE(Zeta(0:NDC),GammaI(NDC),Self(NDC),Sel(NDC),Classi(NDC))
      ALLOCATE(SelfBreakageFunction(NDC), Sela(NDC))

C
C  WRITE THE REPORT FILE.
      ITERM = 5
      IF (TYPE .EQ. 'SAG mill  ') then
        Model = 'SAGM'
        LU = 8
        CALL HEADER(LU,ITERM,NUNIT,'SAG MILL',8,'SAGM')
      Else IF(TYPE .EQ. 'SAG withT') THEN
        Model = 'SAGT'
        LU = 8
        CALL HEADER(LU,ITERM,NUNIT,'SAG Mill with trommel',21,'SAGT')
	Else IF(TYPE .EQ. 'SAG PPort') THEN
        Model = 'SAGP'
        LU = 8
        CALL HEADER(LU,ITERM,NUNIT,'SAG Mill with pebble port',25,
     *      'SAGP')
	Else IF(TYPE .EQ. 'SAG PPo T') THEN
        Model = 'SAGL'
        LU = 8
        CALL HEADER(LU,ITERM,NUNIT,'SAG Mill with pebble port and 
     *	  trommel',37,'SAGL')
      END IF
      A = PARAM(1)/60.0
      Alpha = PARAM(2)
      MuT = 0.001*PARAM(3)
      SLambda = PARAM(4)
      AJK = PARAM(5)
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
C     Particle breakage energy for large particles
	Eminf = PARAM(22)
	Dp0 = 0.001*PARAM(23)
	Phi = PARAM(24)



      WRITE(LU,1001)PARAM(1),PARAM(2),PARAM(3),PARAM(4)
 1001 FORMAT(' Model parameters'/
     *       '   Test mill selection function:'/
     *       '     Specific breakage rate at 1 mm ',F7.2,' 1/min'/
     *       '     Alpha                          ',F7.3/
     *       '     MuT                            ',F7.2,' mm'/
     *	     '     SLambda                         ',F7.2)

      WRITE(LU,1002)PARAM(5),PARAM(6)
 1002 FORMAT(/'   T10 parameters:'/
     *        '     A                 ',F7.1,'%'/
     *        '     b                 ',F7.2,' t/kW h')

      WRITE(LU,1003)PARAM(7),PARAM(8)
 1003 FORMAT(/'   Attrition parameters:'/
     *        '     Largest attrition product',F7.2,' mm'/
     *        '     Ta                       ',F7.2)

	WRITE(LU,1006)Eminf,PARAM(23),Phi
 1006 FORMAT(/'   Particle fracture energy parameters:'/
     *        '     Particle fracture energy of large particles',F8.2,
     *        ' J/kg'/
     *        '     Reference size',F7.2,' mm'/
     *        '     Exponent phi',F8.3)

      WRITE(LU,1004)PARAM(9),PARAM(10),PARAM(11),PARAM(12)
 1004 FORMAT(/'   Dimensions of test mill:'/
     *        '     Diameter                  ',F7.3,' m'/
     *        '     Fraction filled           ',F7.1,'%'/
     *        '     Fraction of critical speed',F7.1,'%'/
     *        '     Ball size                 ',F7.1,' mm')

      WRITE(LU,1005)PARAM(13),PARAM(14),PARAM(15),PARAM(16),PARAM(17),
     *              PARAM(18),PARAM(19),PARAM(20),PARAM(21)
 1005 FORMAT(/'   Dimensions of SAG mill:'/
     *        '     Diameter                  ',F7.3,' m'/
     *        '     Center line length        ',F7.2,' m'/
     *        '     Belly length              ',F7.2,' m'/
     *        '     Trunnion diameter         ',F7.2,' m'/
     *        '     Fraction filled           ',F7.1,'%'/
     *        '     Fraction filled with balls',F7.1,'%'/
     *        '     Ball size                 ',F7.1,' mm'/
     *        '     Fraction of critical speed',F7.1,'%'/
     *        '     Grate opening             ',F7.1,' mm')
	IF(TYPE .EQ. 'SAG PPo T') THEN
	  WRITE(LU,'(''     Pebble port size         '',F8.2,'' mm'')')
     * PARAM(25)
	  WRITE(LU,'(''     Ratio Port/Grate Open Area'',F8.4)') PARAM(26)
      END IF
	IF(TYPE .EQ. 'SAG PPort') THEN
	  WRITE(LU,'(''     Pebble port size         '',F8.2,'' mm'')') 
     * PARAM(25)
	  WRITE(LU,'(''     Ratio Port/Grate Open Area '',F8.4)') PARAM(26)
      END IF
      IF(TYPE .EQ. 'SAG withT') THEN
	  WRITE(LU,'(''     Trommel mesh size        '',F8.2,'' mm'')')
     * PARAM(25)
      END IF
      IF(TYPE .EQ. 'SAG PPo T') THEN
	  WRITE(LU,'(''     Trommel mesh size        '',F8.2,'' mm'')') 
     * PARAM(27) 
      END IF

      IF(TYPE .EQ. 'SAG PPo T') THEN
	   IF(NINT(PARAM(28)) .EQ. 1) THEN
			WRITE(LU,'('' The charge (fraction filled) was fixed.'')')
		ELSE
			WRITE(LU,'('' The charge (fraction filled) was estimated.'')')
	   END IF
	END IF
	IF(TYPE .EQ. 'SAG PPort') THEN
	   IF(NINT(PARAM(27)) .EQ. 1) THEN
			WRITE(LU,'('' The charge (fraction filled) was fixed.'')')
		ELSE
			WRITE(LU,'('' The charge (fraction filled) was estimated.'')')
	   END IF
	END IF
C
      E = 0.40
      U = 1.0

      RhoB = 7800
      OUT2 = FEED

	write(LU,'(/''Derived quantities'')')

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
      write(LU,'(/'' Average density of the ore'',F8.1,'' kg/m**3'')')
     * RhoO
      PhiV = TMS1/RhoO/(TMS1/RhoO+WTR1/1000.0)
      RhoC = Charge_Density(Jt, Jb, E, U, RhoB, RhoO, PhiV)
      write(LU,'('' Calculated density of the total mill charge'',F8.1,
     *'' kg/m**3'')') RhoC

c Calculate the net power draw.
      ConeLength = 0.5*(CenterLineLength + BellyLength)
      NoLoadPower = No_Load_Power(BellyLength,Conelength,Dm,PhiC)
      WRITE(LU,'('' No-load power '',F9.2,'' kW'')') 0.001*NoLoadPower
      NetPower = Mill_PowerM(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, 'Grate')
      WRITE(LU,'('' Net power by Morrell''''s formula '',F9.2,'' kW'')')
     * 0.001*NetPower
      NetPower =  Mill_PowerA(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, Dt)
      WRITE(LU,'('' Net power by Austin''''s formula '',F9.2,'' kW'')')
     * 0.001*NetPower
      Ecs = NetPower/TMSF
      WRITE(LU,'('' Specific power consumption'',F7.2,
     *'' kW h/t'')') Ecs/3600

c Calculate Austin's scale up parameters for the selection function
      DELTA = 0.0
      IF(Dm .GE. 3.81) DELTA = 0.2
      C3 = ((Dm/DmT)**0.5)*((3.81/Dm)**DELTA)
      C4 = (1.0 + 6.6*(FillT**2.3))/(1.0 + 6.6*(Fill**2.3))
      C5 = (PhiC-0.1)/(PhiCT-0.1)
      C5 = C5*(1.0+EXP(15.7*(PhiCT-0.94)))/(1.0+EXP(15.7*(PhiC-0.94)))
      WRITE(LU,'('' Austin''''s mill size scale factor '',F7.2)')C3
      WRITE(LU,'('' Austin''''s mill filling scale factor '',F7.2)')C4
      WRITE(LU,'('' Austin''''s mill speed scale factor '',F7.2)')C5

c Calculate the total internal volume of the mill.
      WS = CenterLineLength/BellyLength
      Vm = 0.25*Pi*Dm*Dm*BellyLength
      Vm = Vm*(1.0 + 0.6667*(WS - 1)*(1-(Dt/Dm)**3)/(1-Dt/Dm))
	VFill = (Vm - CenterLineLength*Dt*Dt*Pi*0.25)/2.0

c Calculate the load in the mill
      VolumeOfSlurry = JT*U*E
      VolumeOfMedia = (JT - JB)*(1.0 - E)
      MillLoad = (VolumeOfSlurry*PhiV + VolumeOfMedia)*RhoO*Vm
      WRITE(LU,'('' Total mill volume'',F7.2,'' m**3''
     */'' Mill load excluding balls'',F7.2,'' t'')')
     * Vm,0.001*MillLoad
	WRITE(LU,'('' Mill fill volume'',F7.2,'' m**3''
     */'' Maximum charge fill'',F7.2,'' %'')')
     * VFill,100.0*VFill/Vm
	
c  Calculate the average residence time.
      TAU = MillLoad/TMSF
      WRITE(LU,'('' Calculated residence time in the mill'',F7.2,
     *'' mins'')')TAU/60.0
	WRITE(LU,'('' Internal circulating ratio (1+CC): '',F7.2)
     *')TMS3/TMSF

C Calculate the total selection function for plotting
      CALL MEDIUM(SIZE,OUT2,MEDIA,NDC,NGC,NSC,TOTMEDIA,GrateSize)
	Write(LU,*)
	Write(LU,*)'Calculated selection functions   1/min'
	Write(LU,*)'  Size cm   Breakage Auto-Breakage  Self breakage
     *   Attrition'
      DO M = 1,NGC
        DR = PPROP(M)*1000.0/RhoB
        DO I = 1,NDC
          dpi = SIZE(I)
          WS1 = AustinSelMedia(dpi,SIZE,MEDIA,NDC,NGC,NSC,
     *		MuT*DR,SLambda,Dm,DmT,DbT,GrateSize,A,Alpha)
          WS2 = AustinSelSteel(dpi,BallSize,DbT,MuT,
     *	       SLambda,Dm,DmT,A,Alpha)

          Sel(I) = C3*C4*C5*Jb*WS2/Jt
	    Sela(I) = C3*C4*C5*(Jt - Jb)*WS1*DR/Jt
          Self(I) = SELF_BR_SEL(DM,PHIC,Fill,SIZE(I),Eminf,Dp0,Phi)
          GammaI(I) = 3.0*KAPPA/(SIZE(I)**(1-DEL))
	    Write(LU,'(2X,E8.3,1X,E10.3,4X,E10.3,4X,E10.3,4X,E10.3)')
     *      Size(I)*100,Sel(I)*60,Sela(I)*60,Self(I)*60,GammaI(I)*60
        END DO
      END DO
	
	!Write selection function data for output on the flowsheet.
      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')Model,NUNIT,Index
      WRITE(GraphTitle,'(''Selection function for mill '',I2)')NUNIT
      WRITE(21,'(A30)') GraphTitle
      WRITE(21,'(A6)') 'Austin'

      WRITE(21,'(I4)') NDC
      DO I = NDC,1,-1
        WRITE(21,'(G10.4, 2X, G10.4)') SIZE(I),(Sel(I) + Sela(I) +
     *Self(I))*60
      END DO
      CLOSE(21)

      DEALLOCATE(MEDIA)
      DEALLOCATE(Zeta,GammaI,Self,Sel,Sela,Classi)
      DEALLOCATE(SelfBreakageFunction)

      RETURN
      END

C
      SUBROUTINE RHFMI(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT
      INTEGER FL,INDPP(NPP,2)
      COMMON/HERBST/SE1, ZETA1, ZETA2
      EXTERNAL HERBST_FUER

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
C PARAMETER PHI IS STORED IN OUT3(I,1,1)
C
C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BALL MILL',9,'HFMI')
      SE1 = PARAM(1)
      ZETA1 = PARAM(2)
      ZETA2 = PARAM(3)
      BETA = PARAM(4)
      GAMMA=PARAM(5)
      DELTA=PARAM(6)
      PowerDraw = PARAM(8)

      WRITE(LU,1006)SE1,ZETA1,ZETA2,BETA,GAMMA,DELTA,PARAM(7),PowerDraw
 1006 FORMAT(/
     *' PARAMETERS:'/
     *'  Parameters for selection function'/
     *'   Energy-specific selection function at 1 mm:',T50,F7.3,
     *' t/kWh'/
     *'   Zeta 1:',T35,F7.3,/
     *'   Zeta 2:',T35,F7.3/
     *'  Parameters for breakage function'/
     *'   Beta:',T35,F7.3,/
     *'   Gamma:',T35,F7.3,/
     *'   Delta:',T35,F7.3,/
     *'   Phi at 5mm:',T35,F7.3//
     *'  Specified power draw for the mill:',T50,F7.0,' kW')
C
      TMS1=TMSF
      WTR1=WTR
      TONS=TMSF*3.6


c Calculate the water flows if water is added to the mill
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF
C
        CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1050)
 1050   FORMAT(/5X,'Size distribution in FEED')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
        CALL PARTSZ(SIZE,PROD,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1051)
 1051   FORMAT(/5X,'Size distribution in PRODUCT')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
        PSIZEP=PSIZEP*1.0E6
        PSIZEF=PSIZEF*1.0E6
        POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))*TONS
        SpecificPower = PowerDraw/TONS
        OPERATINGWI = PowerDraw/POWERF
        WRITE(LU,1052) PSIZEF,PSIZEP,SpecificPower,OPERATINGWI
 1052   FORMAT(/'80% passing size in feed   ',T50,G8.3,' microns'/
     *    '80% passing size in product',T50,G8.3,' microns'/
     *    'Calculated specific power input',T50,F6.2,' kWh/t'/
     *    'Calculated operating work index for this mill',
     *     T50,F5.1,'  kWh/t')


C Write a file with the data to draw the graph of the selection function.
      CALL SelFuncGrSetUp(HERBST_FUER,NUNIT,SIZE,NDC,'HFMI')
      RETURN
      END
C
      SUBROUTINE RHFML(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT
      INTEGER FL,INDPP(NPP,2)
      COMMON/HERBST/SE1, ZETA1, ZETA2
      EXTERNAL HERBST_FUER

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C
C  MODEL IS BASED ON THE BREAKAGE AND SELECTION FUNCTION.
C  The Herbst-Fuerstenau correlation for the energy-specific selection
C  is used.
C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BALL MILL',9,'HFML')
      SE1 = PARAM(1)
      ZETA1 = PARAM(2)
      ZETA2 = PARAM(3)
      BETA = PARAM(4)
      GAMMA=PARAM(5)
      DELTA=PARAM(6)
      PowerDraw = PARAM(8)

      WRITE(LU,1006)SE1,ZETA1,ZETA2,BETA,GAMMA,DELTA,PARAM(7),PowerDraw
 1006 FORMAT(/
     *' PARAMETERS:'/
     *'  Parameters for selection function'/
     *'   Energy-specific selection function at 1 mm:',T50,F7.3,
     *' t/kWh'/
     *'   Zeta 1:',T40,F7.3,/
     *'   Zeta 2:',T40,F7.3/
     *'  Parameters for breakage function'/
     *'   Beta:',T40,F7.3,/
     *'   Gamma:',T40,F7.3,/
     *'   Delta:',T40,F7.3,/
     *'   Phi at 5mm:',T40,F7.3//
     *'  Specified power draw for the mill:',T50,F7.0,' kW')
      WRITE(LU,*)

      IF(NINT(PARAM(9)) .EQ. 0) WRITE(LU,*)
     *' Liberation was NOT modeled'
      IF(NINT(PARAM(9)) .EQ. 1) THEN
        WRITE(LU,*)' Ljubljana model for liberation was used'
        WRITE(LU,'(''   PhiA parameter:'',T40,F7.2)')PPROP(INDPP(2,1))
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'LJUB','HFML',NUNIT)

      END IF
      IF(NINT(PARAM(9)) .EQ. 2) THEN
        WRITE(LU,*)' Beta function model for liberation was used'
        WRITE(LU,'(''   Mineral liberation size:'',T40,F7.2)')
     *  PPROP(INDPP(2,1)+1)
        WRITE(LU,'(''   Preferential breakage factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+2)
        WRITE(LU,'(''   Andrews-Mika boundary factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+3)
        WRITE(LU,'(''   Andrews-Mika boundary sensitivity:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+4)
        WRITE(LU,'(''   Variance factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+5)
        WRITE(LU,'(''   Variance sensitivity:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+6)
        WRITE(LU,'(''   Asymmetry factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+7)
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'BETA','HFML',NUNIT)

      END IF

      WRITE(LU,*)

      IF(NINT(PARAM(10)) .EQ. 0) WRITE(LU,*)
     *' No post classification at the discharge'
      IF(NINT(PARAM(10)) .EQ. 1) WRITE(lu,*)
     *' Post classification modeled using the Rosin-Rammler function'
      IF(NINT(PARAM(10)) .EQ. 2) WRITE(lu,*)
     *' Post classification modeled using the exponential sum function'
      IF(NINT(PARAM(10)) .EQ. 3) WRITE(lu,*)
     *' Post classification modeled using the logistic function'
      IF(NINT(PARAM(10)) .GT. 0) THEN
        WRITE(LU,1007)PARAM(11),PARAM(12)
 1007   FORMAT('   d50 for the post classifier ',F6.3,' mm'/
     *         '   Sharness index for the post classifier ',F6.3)
      END IF
C
      TMS1=TMSF
      WTR1=WTR
      TONS=TMSF*3.6


c Calculate the water flows if water is added to the mill
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
          WATERADD = WTR1 - WTR
        END IF
      END IF
C
        CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1050)
 1050   FORMAT(/5X,'Size distribution in FEED')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
        CALL PARTSZ(SIZE,PROD,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1051)
 1051   FORMAT(/5X,'Size distribution in PRODUCT')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
        PSIZEP=PSIZEP*1.0E6
        PSIZEF=PSIZEF*1.0E6
        POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))*TONS
        SpecificPower = PowerDraw/TONS
        OPERATINGWI = PowerDraw/POWERF
        WRITE(LU,1052) PSIZEF,PSIZEP,SpecificPower,OPERATINGWI
 1052   FORMAT(/'80% passing size in feed   ',T50,G8.3,' microns'/
     *    '80% passing size in product',T50,G8.3,' microns'/
     *    'Calculated specific power input',T50,F6.2,' kWh/t'/
     *    'Calculated operating work index for this mill',
     *     T50,F5.1,'  kWh/t')


C Write a file with the data to draw the graph of the selection function.
      CALL SelFuncGrSetUp(HERBST_FUER,NUNIT,SIZE,NDC,'HFML')
      RETURN
      END
C
      SUBROUTINE RHFSU(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)

      REAL MillLength,Jt,Jb
      REAL No_Load_Power, Mill_PowerM, Mill_PowerA, Mill_PowerB
      REAL NoLoadPower,NetPower
	CHARACTER*30 Model

      COMMON/HERBST/SE1, ZETA1, ZETA2
      EXTERNAL HERBST_FUER
      COMMON /MODELDAT/NUNIT

      INTEGER FL,INDPP(NPP,2)

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BALL MILL',9,'HFSU')
      SE1 = PARAM(1)
      ZETA1 = PARAM(2)
      ZETA2 = PARAM(3)
      BETA = PARAM(4)
      GAMMA=PARAM(5)
      DELTA=PARAM(6)
      Diameter = PARAM(8)
      MillLength = PARAM(9)
      BallLoad = 0.01*PARAM(10)
      Jt = BallLoad
      Jb = BallLoad
      PhiC = 0.01*PARAM(11)
C     12...Choice of liberation model.
C          0 = None, 1 = Ljubljana,  2 = Beta function
c     13...Post classification function
c     14...Post classification d50
      D50 = PARAM(14)
c     15...Post classification SI
      SI = PARAM(15)
C     16...Specific gravity of charge media
      RhoB = PARAM(16)*1000
C     17...Choice of power model
C          0 = None(use user value), 1 = Bond, 2 = Morrell, 3 = Austin
C     18...User net grinding power in kW
C     19...Makeup ball size (mm) needed by Bond's power formula
C     20..Choice of Transport Model.  0 = pm;pm;pm-cl, 1 = pm;pm;pm;cl,
C	   2 = pm-cl;pm-cl;pm-cl
C     21..Fractional residence time, Region 1
C     22..Fractional residence time, Region 2

      WRITE(LU,1006)SE1,ZETA1,ZETA2,BETA,GAMMA,DELTA,PARAM(7)
 1006 FORMAT(/
     *' PARAMETERS:'/
     *'  Parameters for selection function'/
     *'   Energy-specific selection function at 1 mm:',T50,F7.3,
     *' t/kWh'/
     *'   Zeta 1:',T35,F7.3,/
     *'   Zeta 2:',T35,F7.3/
     *'  Parameters for breakage function'/
     *'   Beta:',T35,F7.3,/
     *'   Gamma:',T35,F7.3,/
     *'   Delta:',T35,F7.3,/
     *'   Phi at 5mm:',T35,F7.3/)
	IF(NINT(PARAM(17)) .GT. 0) THEN
      WRITE(LU,1007)PARAM(8),PARAM(9),PARAM(10),PARAM(11),PARAM(16)*1000
 1007 FORMAT(/
     *'  Mill conditions'/
     *'   Mill diameter:',T40,F7.2,' m'/
     *'   Mill length:',T40,F7.2,' m'/
     *'   Ball load in the mill:',T40,F7.1/
     *'   Mill speed as % of critical:',T40,F7.1/
     *'   Specific gravity of grinding media:',T40,F7.1,' kg/m**3')
	IF(NINT(PARAM(17)) .EQ. 1) THEN
      WRITE(LU,'(''   Makeup ball size:'',T40,F7.1,'' mm'')')PARAM(19)
	END IF
	END IF

      WRITE(LU,*)
	ns = NINT(PARAM(13))
      IF(ns .GT. 0) THEN
        IF(ns .EQ. 1) Model = 'Logistic model was used'
        IF(ns .EQ. 2) Model = 'Rosin-Rammler model was used'
        IF(ns .EQ. 3) Model = 'Exponential sum model was used'

        WRITE(LU,'(/''Parameters to describe the classifier:''/
     *  ''  '',A30/
     *  ''  D50 '',T45,F6.3,'' mm''/
     *  ''  Sharpness index '',T45,F6.3)')Model, D50, SI
	  nt = NINT(PARAM(20))
	  IF(nt .EQ. 0) THEN
		  WRITE(LU,'(''Transport model: PM PM (PM,CL).'')')
	  END IF
	  IF(nt .EQ. 1) THEN
		  WRITE(LU,'(''Transport model: (PM PM PM),CL.'')')
	  END IF
	  IF(nt .EQ. 2) THEN
		  WRITE(LU,'(''Transport model: (PM,CL) (PM,CL) (PM,CL)'')')
	  END IF
      ELSE
	  Write(LU,
     *   '(/'' No post classification was specified.''/
     *      ''   Transport model is PM,PM,PM'')')
      END IF

      WRITE(LU,'(''  Fractional residence times:''/
     *''    Region 1:'',T15,F6.3,/
     *''    Region 2:'',T15,F6.3,/
     *''    Region 3:'',T15,F6.3,/)') 
     *PARAM(21),PARAM(22),1.-PARAM(21)-PARAM(22)


      WRITE(LU,*)
      IF(NINT(PARAM(12)) .EQ. 0) WRITE(LU,*)
     *' Liberation was not modeled.'
      IF(NINT(PARAM(12)) .EQ. 1) THEN
        WRITE(LU,*)' Ljubljana model for liberation was used.'
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'LJUB','HFSU',NUNIT)
      END IF

      IF(NINT(PARAM(12)) .EQ. 2) THEN
        WRITE(LU,*)' Beta function model for liberation was used.'
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'BETA','HFSU',NUNIT)
      END IF

C
      TMS1=TMSF
      WTR1=WTR
      TONS=TMSF*3.6

c Calculate the water flows if water is added to the mill
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
            TOT = TOT + FEED(I,J,K)
            WS = WS + FEED(I,J,K)/PPROP(J)
          END DO
        END DO
      END DO
      RhoO = 1000*TOT/WS
      WRITE(LU,'(/'' Average density of the ore'',F8.1,'' kg/m**3'')')
     * RhoO
      PhiV = (TMS1/RhoO)/(TMS1/RhoO+WTR1/1000.0)
      E = 0.4
      U = 1.0
	IF(NINT(PARAM(17)) .EQ. 0) THEN
	NetPower = PARAM(18)
      WRITE(LU,'('' Specified net power  '',F9.2,'' kW'')') NetPower
	NetPower = NetPower*1000
	ELSE
      RhoB = PARAM(16)*1000
      RhoC = Charge_Density(Jt, Jb, E, U, RhoB, RhoO, PhiV)
      WRITE(LU,'('' Calculated density of the total mill charge'',F8.1,
     *'' kg/m**3'')') RhoC

c Calculate the net power draw.
      CenterLineLength = MillLength
      BellyLength = MillLength
      Dm = Diameter
      ConeLength = 0.5*(CenterLineLength + BellyLength)
      NoLoadPower = No_Load_Power(BellyLength,Conelength,Dm,PhiC)
      WRITE(LU,'('' No-load power '',F9.2,'' kW'')') 0.001*NoLoadPower
      END IF

	IF(NINT(PARAM(17)) .EQ. 1) THEN
	Db = PARAM(19)/1000
	NetPower =  Mill_PowerB(BellyLength, Dm, PhiC, RhoB, Jt, Db) 
      WRITE(LU,'('' Net power by Bond''''s formula '',F9.2,'' kW'')')
     * 0.001*NetPower
	END IF
	IF(NINT(PARAM(17)) .EQ. 2) THEN
      NetPower = Mill_PowerM(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, 'Overflow')
      WRITE(LU,'('' Net power by Morrell''''s formula '',F9.2,'' kW'')')
     * 0.001*NetPower
	END IF
	IF(NINT(PARAM(17)) .EQ. 3) THEN
      Dt = 0.1*Dm   !Dt is the feed opening.
      NetPower =  Mill_PowerA(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jt, Dt)
      WRITE(LU,'('' Net power by Austin''''s formula '',F9.2,'' kW'')')
     * 0.001*NetPower
	END IF
	
      Ecs = NetPower/TMSF
      WRITE(LU,'('' Specific power consumption'',F7.2,
     *'' kW h/t'')') Ecs/3600
C
        CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1050)
 1050   FORMAT(/5X,'Size distribution in FEED')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
        CALL PARTSZ(SIZE,PROD,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1051)
 1051   FORMAT(/5X,'Size distribution in PRODUCT')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
        PSIZEP=PSIZEP*1.0E6
        PSIZEF=PSIZEF*1.0E6
        POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))*TONS
        SpecificPower = 0.001*NetPower/TONS
        OPERATINGWI = 0.001*NetPower/POWERF
        WRITE(LU,1052) PSIZEF,PSIZEP,SpecificPower,OPERATINGWI
 1052   FORMAT(/'80% passing size in feed   ',T50,G8.3,' microns'/
     *    '80% passing size in product',T50,G8.3,' microns'/
     *    'Calculated specific power input',T50,F7.2,' kWh/t'/
     *    'Calculated operating work index for this mill',
     *     T50,F6.1,'  kWh/t')

C Write a file with the data to draw the graph of the selection function.
      CALL SelFuncGrSetUp(HERBST_FUER,NUNIT,SIZE,NDC,'HFSU')

      RETURN
      END

C
      SUBROUTINE RGMIL(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
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
      COMMON /MODELDAT/NUNIT
      REAL MRT
      COMMON/SCALEUP/OVFILL
      CHARACTER*13 MODEL(0:2)
      DATA MODEL/'None         ','Ljubljana    ','Beta function'/
      CHARACTER*15 CLASSMODEL(0:3)
      DATA CLASSMODEL/'None           ','Logistic       ',
     *'Rosin Rammler  ','Exponential sum'/

 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      EXTERNAL AustinSel


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
      D50 = PARAM(16)
      SI = PARAM(17)
C Set SelUsesRep to TRUE in case this calls AustinSelBall
	SelUsesRep = 1 

      OVFILL=1.0
      IF(PARAM(4).GT.0.0) THEN
        U=1.3
        IF(TMSF.GT.105.5) U=1.3*(TMSF/105.5)**0.5
        OVFILL=EXP(-1.32*(U-1.3))
      END IF
      HGI = PARAM(5)
      MRT = PARAM(1)

      LIB_MODEL = NINT(PARAM(6))

C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BALL MILL',9,'GMIL')
      TONS=TMSF*3.6
      WRITE(LU,1005)
 1005 FORMAT(/' Mill has 3 well-mixed segments in series without classif
     *ication.')
      WRITE(LU,1006) PARAM(1),TONS,MODEL(LIB_MODEL)
 1006 FORMAT(/
     *'   Residence time in the mill',T40,F7.2,' min'//
     *'   Calculated tonnage through mill ',T40,F7.2,' tons/hr.'//
     *'   Liberation model used...',T41,A13/)
      IF(NSEL .EQ. 1) WRITE(LU,1007)
 1007 FORMAT(/' Quartzite selection function used.')
      IF(NSEL .EQ. 2) WRITE(LU,1008)
 1008 FORMAT(/' Selection function for phosphate ore used.')
      IF(NSEL .EQ. 3) WRITE(LU,1011)
 1011 FORMAT(/' Selection function for coal in 3m mill was used.')
      IF(NSEL .EQ. 4) WRITE(LU,*)' Selection function for taconite was u
     *sed'
      IF(NSEL .EQ. 5) WRITE(LU,*)' Dolomite-sphalerite stage 1 selection
     * function was used'
      IF(NSEL .EQ. 6) WRITE(LU,*)' Dolomite-sphalerite stage 3 selection
     * function was used'
      IF(NSEL .EQ. 7) WRITE(LU,*)' Selection function for limestone was
     *used'
      IF(NSEL .EQ. 8) WRITE(LU,*)' Selection function for copper porphyr
     *y was used'
      IF(NSEL .EQ. 9) WRITE(LU,*)' A user defined selection function was
     * used'
      IF(NBREAK .EQ. 1) WRITE(LU,1009)
 1009 FORMAT(' Quartzite breakage function used.')
      IF(NBREAK .EQ. 2) WRITE(LU,1010)
 1010 FORMAT(' Breakage function for phosphate ore used.')
      IF(NBREAK .EQ. 3) WRITE(LU,1012)
 1012 FORMAT(' Breakage function for coal used.')
      IF(NBREAK .EQ. 4) WRITE(LU,*)' Breakage function for taconite was
     *used'
      IF(NBREAK .EQ. 5) WRITE(LU,*)' Dolomite-sphalerite stage 1 breakag
     *e function was used'
      IF(NBREAK .EQ. 6) WRITE(LU,*)' Dolomite-sphalerite stage 3 breakag
     *e function was used'
      IF(NBREAK .EQ. 7) WRITE(LU,*)' breakage function for limestone was
     * used'
      IF(NBREAK .EQ. 8) WRITE(LU,*)' breakage function for copper porphy
     *ry was used'
      IF(NBREAK .EQ. 9) WRITE(LU,*)' A user defined breakage function wa
     *s used'

      WRITE(LU,1020) S1,Alpha,1000*Mu,Lambda
 1020 FORMAT(//'Parameters in selection function:'/
     *'  S1',T10,f7.3/
     *'  Alpha',T10,F7.3/
     *'  Mu',T10,F7.3, ' mm'/
     *'  Lambda',T10,F7.3)
      WRITE(LU,1021) Beta,Gamma,Delta,Phi5
 1021 FORMAT(/'Parameters in breakage function:'/
     *'  Beta',T10,f7.3/
     *'  Gamma',T10,F7.3/
     *'  Delta',T10,F7.3/
     *'  Phi5',T10,F7.3)


      IF(NINT(PARAM(6)) .EQ. 0) WRITE(LU,*)
     *' Liberation was NOT modeled'
      IF(NINT(PARAM(6)) .EQ. 1) THEN
        WRITE(LU,*)' Ljubljana model for liberation was used'
        WRITE(LU,'(''   PhiA parameter:'',T40,F7.2)')PPROP(INDPP(2,1))
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'LJUB','GMIL',NUNIT)
      END IF
      IF(NINT(PARAM(6)) .EQ. 2) THEN
        WRITE(LU,*)' Beta function model for liberation was used'
        WRITE(LU,'(''   Mineral liberation size:'',T40,F7.2)')
     *  PPROP(INDPP(2,1)+1)
        WRITE(LU,'(''   Preferential breakage factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+2)
        WRITE(LU,'(''   Andrews-Mika boundary factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+3)
        WRITE(LU,'(''   Andrews-Mika boundary sensitivity:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+4)
        WRITE(LU,'(''   Variance factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+5)
        WRITE(LU,'(''   Variance sensitivity:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+6)
        WRITE(LU,'(''   Asymmetry factor:'',T40,F7.4)')
     *  PPROP(INDPP(2,1)+7)
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'BETA','GMIL',NUNIT)
      END IF

      IF(NBREAK .EQ. 3 .OR. NBREAK .EQ. 3) WRITE(LU,1013) HGI
 1013 FORMAT(/' Hardgrove grindability index ',F6.1)

      IF(PARAM(4).GT.0.0) WRITE(LU,1014)U
 1014 FORMAT(/' Over filling has been allowed for.  U = ',F7.3)

      WRITE(LU,1022)CLASSMODEL(NINT(PARAM(15)))
 1022 FORMAT(/'Post classification model used: ',A15)
      IF(NINT(PARAM(15)) .GT. 0) THEN
        WRITE(LU,'('' D50 for post classifier '',F6.3)')D50
        write(lu,'('' Sharpness index for post classifier '',F6.3)')SI
      END IF

C
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(/5X,'Size distribution in FEED')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
C
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1051)
 1051 FORMAT(/5X,'Size distribution in PRODUCT')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
      PSIZEP=PSIZEP*1.0E6
      PSIZEF=PSIZEF*1.0E6
      POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))
      POWERF=POWERF*TONS
      WRITE(LU,1052) PSIZEF,PSIZEP,POWERF
 1052 FORMAT(/' 80% passing size in feed   ',T45,F8.1,' microns'/
     *        ' 80% passing size in product',T45,F8.1,' microns'/
     *'The power required by this mill can be estimated from the'/
     *'Bond work index for the ore.'/
     *'        Power required = ',F8.1,' x WI x 1.20  kW'/)
C

C Write a file with the data to draw the graph of the selection function.
      CALL SelFuncGrSetUp(AustinSel,NUNIT,SIZE,NDC,'GMIL')
      RETURN
      END
C
      SUBROUTINE RGMI1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C MODEL FOR A BALL MILL HAVING THREE PERFECTLY MIXED STAGES IN SERIES.
C NO CLASSIFICATION BETWEEN STAGES.
C PARAMETERS;
C     1....Hold up in the mill.
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
      COMMON /MODELDAT/NUNIT
      REAL MRT
      COMMON/SCALEUP/OVFILL
      CHARACTER*13 MODEL(0:2)
      DATA MODEL/'None         ','Ljubljana    ','Beta function'/
      CHARACTER*15 CLASSMODEL(0:3)
      DATA CLASSMODEL/'None           ','Logistic       ',
     *'Rosin Rammler  ','Exponential sum'/
 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      EXTERNAL AustinSel


      NSEL = NINT(PARAM(2))
      NBREAK=NINT(PARAM(3))
      S1 = PARAM(7)
      Alpha = PARAM(8)
      Mu = 0.001*PARAM(9)
      Lambda = PARAM(10)
      Beta = PARAM(11)
      Gamma = PARAM(12)
      Delta = PARAM(13)
      Phi5 = PARAM(14)
      D50 = PARAM(16)
      SI = PARAM(17)
C Set SelUsesRep to TRUE in case this calls AustinSelBall
	SelUsesRep = 1 

      OVFILL=1.0
      IF(PARAM(4).GT.0.0) THEN
        U=1.3
        IF(TMSF.GT.105.5) U=1.3*(TMSF/105.5)**0.5
        OVFILL=EXP(-1.32*(U-1.3))
      END IF
      HGI = PARAM(5)
      MRT = 1000*PARAM(1)/(60*TMSF)

      LIB_MODEL = NINT(PARAM(6))

C
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'BALL MILL',9,'GMI1')
      TONS=TMSF*3.6
      WRITE(LU,1005)
 1005 FORMAT(/' Mill has 3 well-mixed segments in series without classif
     *ication.')
      WRITE(LU,1006) PARAM(1),TONS,MRT,MODEL(LIB_MODEL)
 1006 FORMAT(/'   Hold up in the mill',T40,F7.2,' t'/
     *'   Calculated tonnage through mill ',T40,F7.2,' tons/hr'/
     *'   Calculated residence time in the mill',T40,F7.2,' min'//
     *'   Liberation model used...',T41,A13/)
      IF(LIB_MODEL .GT. 0) WRITE(LU,1015)PPROP(INDPP(2,1))
 1015 FORMAT('   Mineral liberation parameter PHIA.',T41,F6.1)
      IF(NSEL .EQ. 1) WRITE(LU,1007)
 1007 FORMAT(/' Quartzite selection function used.')
      IF(NSEL .EQ. 2) WRITE(LU,1008)
 1008 FORMAT(/' Selection function for phosphate ore used.')
      IF(NSEL .EQ. 3) WRITE(LU,1011)
 1011 FORMAT(/' Selection function for coal in 3m mill was used.')
      IF(NSEL .EQ. 4) WRITE(LU,*)' Selection function for taconite was u
     *sed'
      IF(NSEL .EQ. 5) WRITE(LU,*)' Dolomite-sphalerite stage 1 selection
     * function was used'
      IF(NSEL .EQ. 6) WRITE(LU,*)' Dolomite-sphalerite stage 3 selection
     * function was used'
      IF(NSEL .EQ. 7) WRITE(LU,*)' Selection function for limestone was
     *used'
      IF(NSEL .EQ. 8) WRITE(LU,*)' Selection function for copper porphyr
     *y was used'
      IF(NSEL .EQ. 9) WRITE(LU,*)' A user defined selection function was
     * used'
      IF(NBREAK .EQ. 1) WRITE(LU,1009)
 1009 FORMAT(' Quartzite breakage function used.')
      IF(NBREAK .EQ. 2) WRITE(LU,1010)
 1010 FORMAT(' Breakage function for phosphate ore used.')
      IF(NBREAK .EQ. 3) WRITE(LU,1012)
 1012 FORMAT(' Breakage function for coal used.')
      IF(NBREAK .EQ. 4) WRITE(LU,*)' Breakage function for taconite was
     *used'
      IF(NBREAK .EQ. 5) WRITE(LU,*)' Dolomite-sphalerite stage 1 breakag
     *e function was used'
      IF(NBREAK .EQ. 6) WRITE(LU,*)' Dolomite-sphalerite stage 3 breakag
     *e function was used'
      IF(NBREAK .EQ. 7) WRITE(LU,*)' breakage function for limestone was
     * used'
      IF(NBREAK .EQ. 8) WRITE(LU,*)' breakage function for copper porphy
     *ry was used'
      IF(NBREAK .EQ. 9) WRITE(LU,*)' A user defined breakage function wa
     *s used'

      WRITE(LU,1020) S1,Alpha,1000*Mu,Lambda
 1020 FORMAT(//'Parameters in selection function:'/
     *'  S1',T10,f7.3/
     *'  Alpha',T10,F7.3/
     *'  Mu',T10,F7.3, 'mm'/
     *'  Lambda',T10,F7.3)
      WRITE(LU,1021) Beta,Gamma,Delta,Phi5
 1021 FORMAT(/'Parameters in breakage function:'/
     *'  Beta',T10,f7.3/
     *'  Gamma',T10,F7.3/
     *'  Delta',T10,F7.3/
     *'  Phi5',T10,F7.3)
      IF(NBREAK .EQ. 3 .OR. NBREAK .EQ. 3) WRITE(LU,1013) HGI
 1013 FORMAT(/' Hardgrove grindability index ',F6.1)

      IF(PARAM(4).GT.0.0) WRITE(LU,1014)U
 1014 FORMAT(' Over filling has been allowed for.  U = ',F7.3)
C

      WRITE(LU,1022)CLASSMODEL(NINT(PARAM(15)))
 1022 FORMAT(/'Post classification model used: ',A15)
      IF(NINT(PARAM(15)) .GT. 0) THEN
        WRITE(LU,'('' D50 for post classifier '',F6.3)')D50
        WRITE(LU,'('' Sharpness index for post classifier '',F6.3)')SI
      END IF

      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(/5X,'Size distribution in FEED')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
C
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1051)
 1051 FORMAT(/5X,'Size distribution in PRODUCT')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
      PSIZEP=PSIZEP*1.0E6
      PSIZEF=PSIZEF*1.0E6
      POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))
      POWERF=POWERF*TONS
      WRITE(LU,1052) PSIZEF,PSIZEP,POWERF
 1052 FORMAT(/' 80% passing size in feed   ',T45,F8.1,' microns'/
     *        ' 80% passing size in product',T45,F8.1,' microns'/
     *'The power required by this mill can be estimated from the'/
     *'Bond work index for the ore.'/
     *'        Power required = ',F8.1,' x WI x 1.20  kW'/)
C

C Write a file with the data to draw the graph of the selection function.
      CALL SelFuncGrSetUp(AustinSel,NUNIT,SIZE,NDC,'GMI1')
      RETURN
      END
C
      SUBROUTINE LIBSPR(P,SPEC,NDC,NGC,NSC)
C     *************************************
C  CALCULATES THE LIBERATION SPECTRUM AT EACH SIZE
C  THE SPECTRUM IS RETURNED IN SPEC
      REAL P(NDC,NGC,NSC),SPEC(NDC,NGC,NSC)
      DO 40 I = 1,NDC
        WS= 0.0
        DO 10 J= 1,NGC
          DO 10 K=1,NSC
           WS = WS+P(I,J,K)
   10   CONTINUE
        DO 30 J=1,NGC
          SPEC(I,J,1)=0
          DO 20 K=1,NSC
            SPEC(I,J,1) = SPEC(I,J,1) + P(I,J,K)
   20     CONTINUE
          IF(WS .GT. 0.0) SPEC(I,J,1) = SPEC(I,J,1)/WS
   30   CONTINUE
   40 CONTINUE
      RETURN
      END

      SUBROUTINE RGMSU(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C*
C*  Report writer for model GMSU
C MODEL FOR A BALL MILL HAVING THREE PERFECTLY MIXED STAGES IN SERIES.
C NO CLASSIFICATION BETWEEN STAGES.
c This model uses the Austin scale-up procedure
C PARAMETERS;
c   Selection function parameters:
C     1...Specific rate of breakage at 1mm
C     2...Particle size exponent  alpha
C     3...Size coefficient for maximum breakage  mu
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
C     12..Fraction of media filled with solid in test mill.
C     13..Mill speed of test mill.  % of critical.
C     14..Ball size in test mill.
C     15..Full size mill diameter.
C     16..Full size mill length.
c     17..Ball load in full size mill. %
c     18..Media filling in full size mill %
c     19..Speed of full size mill  % of critical.
c     20..Make-up ball size in full size mill.
C     21..D50 for post classifier.
C     22..Sharpness index for post classifier.
C     23..Number of ball sizes.
C     24-35 Ball size - fraction pairs.

c     36..Choice of liberation model.  0 = none, 1 = Ljubljana, 2 = Beta function.
C     37..SWITCH FOR ALLOWING FOR OVERFILLING.
C     38..Choice of post classification function.  0 = None, 1 = Logistic,
C          2 = Rosin-Rammler, 3 = Exponential sum.
C     39..Choice of Transport Model.  0 = pm;pm;pm-cl, 1 = pm;pm;pm;cl,
C									2 = pm-cl;pm-cl;pm-cl
C     40..Fractional residence time, Region 1
C     41..Fractional residence time, Region 2
C     42..Choice for Selection function calculation. 0 = Calculate Selection function using 
C	the upper sieve size of screens in batch test, 1 = Calculate Selection function using 
C     representative sizes.
C     43..Specific gravity of media.  Only for power calculations.

C
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(NDC),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      REAL LmT,Lm,JmT,Jm,MuT
      REAL Jt,NetPower
      REAL MillLoad,MRT,MRT1
      REAL No_Load_Power, Mill_PowerM, Mill_PowerB
      REAL NoLoadPower
      CHARACTER*30 Model
      PARAMETER (Pi = 3.1415927)
      COMMON /MODELDAT/NUNIT
 
      REAL S1,Alpha,Mu,Lambda,Beta,Gamma,Delta,Phi5
      INTEGER Mclas
      Real Mk(6), Dk(6), Muk(6)
      COMMON /AUSTIN/S1,Alpha,Mu,Lambda,Mclas,Mk,Dk,Muk,SelUsesRep
      COMMON/BREAKAGE_PARS/Gamma,Beta,Phi5,Delta

      EXTERNAL AuSelBall

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'Ball mill using Austin scale-up',
     *31,'GMSU')
      S1T = PARAM(1)
      Alpha = PARAM(2)
      MuT = 0.001*PARAM(3)
      Lambda = PARAM(4)
      WRITE(LU,'(/''Parameters for the selection function obtained from 
     *the test mill:''/
     *''  Selection function at 1 mm '',T45,F6.3,'' 1/min''/
     *''  Alpha '',T45,F6.3/
     *''  Mu '',T45,F6.3,'' mm''/
     *''  Lambda '',T45,F6.3)')
     *S1T,Alpha,1000*MuT,Lambda
C Set SelUsesRep
	SelUsesRep = PARAM(42) 
	IF(NINT(SelUsesRep) .EQ. 1) THEN
	    WRITE(LU,'(''Selection function was calculated for parameters 
     *obtained using representative size''/)')
	ELSE
	    WRITE(LU,'(''Selection function was calculated for parameters 
     *obtained using upper mesh size of the class''/)')
	END IF
      Beta = PARAM(5 )
      Gamma = PARAM(6)
      Delta = PARAM(7)
      Phi5 = PARAM(8)
      WRITE(LU,'(/''Parameters for the breakage function:''/
     *''  Beta '',T45,F6.3/
     *''  Gamma '',T45,F6.3/
     *''  Delta '',T45,F6.3/
     *''  Phi at 5 mm '',T45,F6.3)')
     *Beta,Gamma,Delta,Phi5

      DmT = PARAM(9)
      LmT = PARAM(10)
      JmT  = 0.01*PARAM(11)
      UT  = PARAM(12)
      PhiCT = 0.01*PARAM(13)
      DbT  = 0.001*PARAM(14)
      WRITE(LU,'(/''Dimensions of the test mill:''/
     *''  Diameter '',T55,F6.3,'' meters''/
     *''  Length '',T55,F6.3,'' meters''/
     *''  Fractional filling by balls'',T55,F6.3/
     *''  Fraction of ball voids that are filled with solids '',T55,F6.3/
     *''  Fraction of critical speed '',T55,F6.3/
     *''  Ball diameter '',T55,F6.3,'' cm'')')
     *DmT, LmT, JmT, UT, PhiCT, 100*DbT

      Dm  = PARAM(15)
      Lm  = PARAM(16)
      Jm  = 0.01*PARAM(17)
      Udry  = PARAM(18)
      PhiC = 0.01*PARAM(19)
      Db  = 0.001*PARAM(20)
      WRITE(LU,'(/''Dimensions of the production mill:''/
     *''  Diameter '',T55,F6.3,'' meters''/
     *''  Length '',T55,F6.3,'' meters''/
     *''  Fractional filling by balls'',T55,F6.3/
     *''  Fraction of ball voids that are filled with solids '',T55,F6.3/
     *''  Fraction of critical speed '',T55,F6.3/
     *''  Make-up ball diameter '',T55,F6.3,'' cm'')')
     *Dm, Lm, Jm, Udry, PhiC, 100*Db

      Mclas = Nint(PARAM(23))
      AN0 = 1.0
      AN2 = 0.0 ! AN2 = 0.2
      AN3 = 1.2 ! AN3 = 1.0
      Do K = 1,Mclas
	Dk(K) = PARAM(24 + 2*(K-1))
	Mk(K) = 0.01*PARAM(25 + 2*(K-1))*(DbT/Dk(K))**AN0
	MuK(k) = MuT*(DM/DMT)**AN2*(Dk(K)/DbT)**AN3
      End Do
      Write(LU,'(/''Ball size distribution:'')')
      Mclas = Nint(Param(23))
      Write(LU,'(''   Number of ball size classes'',I3)')Mclas
      Write(LU,'(''   Ball size cm    % in class   m(k)   mu(k)  mm'')')
      Do K = 1, Mclas
        Write(LU,'(8x,F6.2,6x,F6.2,6x,F5.3,4x,F5.3)')100*Dk(K),
     *  100*Mk(K)*(Dk(K)/DbT)**AN0,(DbT/Dk(K))**AN0,MuK(K)*1000
      End Do

      ns = NINT(PARAM(38))
      IF(ns .GT. 0) THEN
        IF(ns .EQ. 1) Model = 'Logistic model was used'
        IF(ns .EQ. 2) Model = 'Rosin-Rammler model was used'
        IF(ns .EQ. 3) Model = 'Exponential sum model was used'
        D50 = PARAM(21)
        SI = PARAM(22)
        WRITE(LU,'(/''Parameters to describe the classifier:''/
     *  ''  '',A30/
     *  ''  D50 '',T45,F6.3,'' mm''/
     *  ''  Sharpness index '',T45,F6.3)')Model, D50, SI
	  nt = NINT(PARAM(39))
	  IF(nt .EQ. 0) THEN
		WRITE(LU,'(''Transport model: PM PM (PM,CL).'')')
	  END IF
	  IF(nt .EQ. 1) THEN
		WRITE(LU,'(''Transport model: (PM PM PM),CL.'')')
	  END IF
	  IF(nt .EQ. 2) THEN
		WRITE(LU,'(''Transport model: (PM,CL) (PM,CL) (PM,CL)'')')
	  END IF
      END IF

      WRITE(LU,'(/''Fractional residence times:''/
     *''  Region 1:'',T15,F6.3,/
     *''  Region 2:'',T15,F6.3,/
     *''  Region 3:'',T15,F6.3,/)') 
     *PARAM(40),PARAM(41),1.-PARAM(40)-PARAM(41)


      WRITE(LU,'(//''Calculated data for the mill:'')')

c Calculate Austin's scale up coefficients for the selection function.
      IF(Dm .GE. 3.81) THEN
        C2 = ((Dm/DmT)**0.5)*((3.81/Dm)**0.2)
      Else
        C2 = ((Dm/DmT)**0.5)
      End If
      C3 = (1.0 + 6.6*(JmT**2.3))/(1.0 + 6.6*(Jm**2.3))
      C4 = (PhiC-0.1)/(PhiCT-0.1)
      C4 = C4*(1.0+EXP(15.7*(PhiCT-0.94)))/(1.0+EXP(15.7*(PhiC-0.94)))
      C5 = exp(-1.32*(Udry - UT))
      C1 = (DbT/Db)**1.0
      WRITE(LU,'(/''Austin''''s scale-up constants:''/
     *''  C1 = '',T45,F6.3/
     *''  C2 = '',T45,F6.3/
     *''  C3 = '',T45,F6.3/
     *''  C4 = '',T45,F6.3/
     *''  C5 = '',T45,F6.3)')C1,C2,C3,C4,C5

c Calculate the parameters for Austin's selection function.
      S1 = S1T*C2*C3*C4*C5
      WRITE(LU,'(''Scaled parameters for the selection function:''/
     *''  Selection function at 1mm '',T45,F6.3,'' 1/min'')') S1

c Calculate the water flows if water is added to the mill
      TMS1 = TMSF
      WTR1 = WTR
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

c Calculate the total internal volume of the mill.
      Vm = 0.25*Pi*Dm*Dm*Lm

c Calculate the load in the mill.
      Eps = 0.4
	Um = (1 - Eps)*Udry/PhiV
      VolumeOfSlurry = Jm*Um*Eps
c  As per Len's sugestion on 04/15/2003, change this to
c  the formal method, as all scale up coefficients
c  have been derived as such
      MillLoadRPK = Vm*VolumeOfSlurry*PhiV*RhoO  
	MillLoad = Vm*Jm*Udry*Eps*(1-Eps)*RhoO
      write(LU,'(/''Total mill volume'',F7.2,'' m^3''
     */''Mill load excluding balls'',F7.2,'' t''
     */''Fraction of ball load filled with slurry'',F7.2,
     */''Flow of solid through the mill'',F8.2,''t/h'')')
     * Vm,0.001*MillLoad,Um,TMSF*3.6
      
c  Calculate the average residence time.
      MRT1 = MillLoadRPK/(TMSF*60.0)
      MRT = MillLoad/(TMSF*60.0)
      write(LU,'(/''Calculated residence time in the mill'',F7.2,
     *'' min'')')MRT
c      write(LU,'(''Residence time calculated using formal bed porosity
c     *'',F7.2,'' min'')')MRT

C Put the overfill multiplier into common /SCALEUP/
      OVFILL=1.0
      IF(PARAM(22).LT.0.0) THEN
        U=1.3
        IF(TMSF.GT.105.5) U=1.3*(TMSF/105.5)**0.5
        OVFILL=EXP(-1.32*(U-1.3))
      END IF

c Select the liberation model
      WRITE(LU,*)

      IF(NINT(PARAM(36)) .EQ. 0) WRITE(LU,*)
     *' Liberation was not modeled.'
      IF(NINT(PARAM(36)) .EQ. 1) THEN
        WRITE(LU,*)' Ljubljana model for liberation was used.'
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'LJUB','GMSU',NUNIT)
      END IF
      IF(NINT(PARAM(36)) .EQ. 2) THEN
        WRITE(LU,*)' Beta function model for liberation was used.'
        CALL WriteAMDGRPdata(NDC,NGC,SIZE,'BETA','GMSU',NUNIT)
      END IF

      WRITE(LU,'(/'' Average density of the ore'',F8.1,'' kg/m^3'')')
     * RhoO
      RhoB = 1000*PARAM(43)
	WRITE(LU,'(/'' Density of grinding media'',F8.1,'' kg/m^3'')') 
     * RhoB
	PhiV = (TMS1/RhoO)/(TMS1/RhoO+WTR1/1000.0)
      RhoC = Charge_Density(Jm, Jm, Eps, Um, RhoB, RhoO, PhiV)
      WRITE(LU,'('' Calculated density of the total mill charge'',F8.1,
     *'' kg/m^3'')') RhoC

c Calculate the net power draw.
      CenterLineLength = Lm
      BellyLength = Lm
      ConeLength = 0.5*(CenterLineLength + BellyLength)
      NoLoadPower = No_Load_Power(BellyLength,Conelength,Dm,PhiC)
      WRITE(LU,'('' No-load power '',F9.2,'' kW'')') 0.001*NoLoadPower
      NetPower = Mill_PowerM(BellyLength, ConeLength, Dm, PhiC,
     * RhoC, Jm, 'Overflow')
      WRITE(LU,'('' Net power by Morrell''''s formula '',F9.2,'' kW'')')
     * 0.001*NetPower
      Dt = 0.1*Dm   !Dt is the feed opening.
      NetPower =  Mill_PowerB(BellyLength, Dm, PhiC, RhoB, Jm, Db)
      WRITE(LU,'('' Net power by Bond''''s formula '',F9.2,'' kW'')')
     * 0.001*NetPower
      Ecs = NetPower/TMSF
      WRITE(LU,'('' Specific power consumption'',F7.2,
     *'' kW h/t'')') Ecs/3600
C
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(/5X,'Size distribution in FEED')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1051)
 1051 FORMAT(/5X,'Size distribution in PRODUCT')
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
      CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)

      TONS=TMSF*3.6
      SpecificPower = 0.001*NetPower/TONS
      PSIZEP=PSIZEP*1.0E6
      PSIZEF=PSIZEF*1.0E6
      POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))
      POWERF=POWERF*TONS
      OPERATINGWI = 0.001*NetPower/POWERF
      WRITE(LU,1052) PSIZEF,PSIZEP,SpecificPower,OPERATINGWI
 1052 FORMAT(/'80% passing size in feed   ',T50,G8.3,' microns'/
     *  '80% passing size in product',T50,G8.3,' microns'/
     *  'Calculated specific power input',T50,F7.2,' kWh/t'/
     *  'Calculated operating work index for this mill',
     *   T50,F6.1,'  kWh/t')

C Write a file with the data to draw the graph of the selection function.
      CALL SelFuncGrSetUp(AuSelBall,NUNIT,SIZE,NDC,'GMSU')

      RETURN
      END

      SUBROUTINE SelFuncGrSetUp(SelFunc,NUNIT,SIZE,NDC,Model)
C     *******************************************************
C Set up the data to plot the selection function
      USE GLOBALS
      REAL SIZE(NDC)
      CHARACTER*4 Model
      CHARACTER*30 TITLE

      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 0
      WRITE(21,'(A4,/I5,I5)')Model,NUNIT,Index
      WRITE(TITLE,'(''Selection function for mill '',I2)')NUNIT
      WRITE(21,'(A30)') TITLE
      IF(MODEL(1:2) .EQ. 'HF') then
        WRITE(21,'(A6)') 'Herbst'
      else
        WRITE(21,'(A6)') 'Austin'
      END IF

      WRITE(21,'(I4)') NDC
      DO I = NDC,1,-1
        CALL SelFunc(I,SIZE(I),S)
        WRITE(21,'(G10.4,2x,G10.4)') SIZE(I),S
      END DO
      CLOSE(21)
      RETURN
      END

      SUBROUTINE WriteAMDGRPdata(NDC,NGC,SIZE,AMModel,MODEL,NUNIT)
C     ************************************************************
C  Write file AMDGRP.OUT to plot the AM diagram
      USE GLOBALS

      INTEGER*4 result
      REAL SIZE(NDC)
      CHARACTER*4 AMModel,MODEL
      CHARACTER*32 TITLE

      LENG = LEN_TRIM(UnitJobPath)
      OPEN (21,FILE = UnitJobPath(1:LENG)//'MODELGRP.OUT',
     *POSITION = 'APPEND')

      Index = 2
      WRITE(21,'(A4,/I5,I5)')MODEL,NUNIT,Index

      NDParent = 1
      NGParent = NINT((NGC + 0.9)/2)
      WRITE(TITLE,'(''Andrews-Mika diagram for mill '',I2)')NUNIT
      WRITE(21,'(A32)') TITLE
      WRITE(21,*,ERR = 9990)NDC,NGC
      DO I = 1,NDC
        WRITE(21,*,ERR = 9990)SIZE(I)
      END DO
      WRITE(21,'(A4)',ERR = 9990)AMModel
      WRITE(21,*,ERR = 9990)NDParent,NGparent
      CLOSE(21)
      RETURN

 9990 CONTINUE
      RETURN
      END

      SUBROUTINE RHPGR(TMSF,TMS1,TMS2,TMS3,FEED,PROD,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      REAL FEED(NDC,NGC,NSC),PROD(NDC,NGC,NSC)
      REAL SIZE(NDC),PARAM(*)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL PPROP(*),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT
      INTEGER FL,INDPP(NPP,2)
      COMMON/HERBST/SE1, ZETA1, ZETA2
      EXTERNAL HERBST_FUER

c Receive information about water feed to this unit.
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
      REAL Xg, X, mu, lambda, kappa, kappasu, alpha, phi, gamma, beta
	REAL phiP,gammaP,betaP,PMD,PML,PMu, PMRPM,PMGP
	REAL TMD, TML, TMu, TMRPM, TMGP
	REAL w, Rho, OForce, SGE, scf, spf, PMC, MP
	REAL a,b,c,d,n
C  WRITE THE REPORT FILE.
      LU=8
      ITERM=5
      CALL HEADER(LU,ITERM,NUNIT,'High Pressure Grinding Roll',27,
     *'HPGR')
	PMD=PARAM(1) !Production Mill Roll Diameter
	PML=PARAM(2) !Production Mill Roll Length
	PMu = PARAM(3) !Production Mill peripheral speed
	PMGP=PARAM(4) !Production Mill Grinding Pressure
      alphaC=PARAM(5) !Critical angle of nip
      mu=PARAM(6) !nipping selection function
      lambda=PARAM(7) !nipping selection function
      kappa=PARAM(8) !selection function compressing grind
      alpha = PARAM(9) !selection function slope
	phi = PARAM(10) !nipping breakage function
	gamma = PARAM(11) !nipping breakage function
	beta = PARAM(12) !nipping breakage function
	phiP = PARAM(13) !compressing grind breakage function
	gammaP = PARAM(14) !compressing grind breakage function
	betaP = PARAM(15) !compressing grind breakage function
	TMD=PARAM(16) !Test Mill Roll Diameter
	fm=PARAM(17) !Test Mill specific capacity factor
	TMu = PARAM(18) !Test Mill peripheral velocity
	fp=PARAM(19) !Test Mill specific power factor
	F80T=PARAM(20) !Test Mill F80, in mm
	
	kappasu = kappa*((PMD/TMD)**(3.0/2.0))/PMGP
	alphasu = alpha*TMu/PMu

      WRITE(LU,1006)mu,lambda,phi,gamma,beta,kappa,alpha,
     *PhiP,gammaP,betaP
 1006 FORMAT(/
     *' PARAMETERS:'//
     *'  Parameters for nipping selection function'/
     *'   Parameter mu:',T35,F7.3,/
     *'   Parameter lambda:',T35,F7.3/
     *'  Parameters for nipping breakage function'/
     *'   Phi:',T35,F7.3,/
     *'   Gamma:',T35,F7.3,/
     *'   Beta:',T35,F7.3//
     *'  Parameters for selection function'/
     *'   Parameter kappa:',T35,F7.3,'mm'/
     *'   Parameter alpha:',T35,F7.3,/
     *'  Parameters for breakage function'/
     *'   Phi:',T35,F7.3,/
     *'   Gamma:',T35,F7.3,/
     *'   Beta:',T35,F7.3)
C
      TMS1=TMSF
      WTR1=WTR
      TONS=TMSF*3.6
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
      w = WTR*100/(TMSF+WTR) 
      RhoF = TOT*1000/WS
      OForce = PMD*PML*PMGP*1e3  !Operating force
    !Calculate roll speed in RPM
      PMRPM = PMu*60/(PMD*3.1416)
	TMRPM = TMu*60/(TMD*3.1416)

	!calculate the specific capacity factor
	scf = TMSF/(PMu*PML*PMD*RhoF) !a*(w**c)*(PMGP**(-b))!specific cap. factor
	!calculate mill capacity
	PMC = fm*PMD*PML*PMu*RhoF*3.6 !mill capacity in tons/hour
	!calculate mill net power MP
	MP = fp*PMD*PML*PMu*PMGP*1e3  !in kW
	!calculate specific grinding energy
	SGE = MP/PMC !in kWh/ton
	BulkRho = RhoF*(1.0-0.4)
	Xc = PMC/(3.6*PML*BulkRho*PMu*COS(2*Pi*alphaC/360))
	Xg = Xc - (1-COS(2*Pi*alphaC/360))*PMD
      WRITE(LU,1008) TMD,TMRPM,TMu,alphaC,fm,fp,F80T
 1008 FORMAT(/
     *	'Test mill parameters'/
     *	'  Roll Diameter (meters) ',T35,F8.3/
     *	'  Velocity of Roll Surface ',T35,F7.1,' RPM  ',F8.4,' m/s'/
     *	'  Critical angle of nip (degrees) ',T35,F8.3/
     *	'  Specific capacity factor m ',T35,F8.3/
     *	'  Specific power factor p ',T35,F8.2/
     *    '  F80 mm ',T35,F8.2)
      WRITE(LU,1010) PMD,PML,PMRPM,PMu,PMGP
 1010 FORMAT(/
     *	'Industrial mill parameters'/
     *	'  Roll Diameter (meters) ',T35,F8.3/
     *	'  Roll Length (meters) ',T35,F8.3/
     *	'  Velocity of Roll Surface ',T35,F7.1,' RPM  ',F8.4,' m/s'/
     *	'  Grinding Pressure (MPascal) ',T35,F8.2)
	WRITE(LU,1020) w,RhoF,OForce
 1020 FORMAT(/
     *	'Operating variables'/
     *	'  Water content (%) ',T35,F8.2/
     *	'  Solids Density (kg/m3) ',T35,F8.2/
     *	'  Operating Force (kN) ',T35,E9.3)
C     FIND 80% passing size in feed
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
	CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
	PSIZEF=PSIZEF*1.0E3 !in mm
C     Apply Guevara and Menacho's correction for power
	GM = ((TMD*PSIZEF/(PMD*F80T))**0.25)
	MP = MP*GM
C     Number of presses required
      PNum = TONS/PMC
      WRITE(LU,1030) RhoF,BulkRho,Xc*1000.0,Xg*1000.0,PMC,PNum,GM,
     *MP,MP*PNum
 1030 FORMAT(/
     *	'Scale up calculations'/
     *	'  Feed density (kg/m3)',T35,F8.2/
     *	'  Bed bulk density (kg/m3)',T35,F8.2/
     *	'  Critical gap (mm)',T35,F8.2/
     *	'  Operating gap (mm)',T35,F8.2/
     *	'  Press capacity (t/h) ',T35,F8.2/
     *	'  Number of presses required ',T35,F8.2/
     *    '  Guevara-Menacho power corr.',T35,F8.2/	
     *	'  Net grinding power each press (kW) ',T48,F8.1/
     *	'  Total net power required for this operation ',
     *T48,F7.1,' kW'/
     *    '  All powers reported are net grinding powers.'/
     *    '  Add no-load power and correct for power'
     *    ' transmission efficiency to get motor power')
C
        CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1050)
 1050   FORMAT(/5X,'Size distribution in FEED')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE FEED.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEF)
        CALL PARTSZ(SIZE,PROD,NDC,NGC,NSC,DER2,DER3,DER1)
        WRITE(LU,1051)
 1051   FORMAT(/5X,'Size distribution in PRODUCT')
        CALL WRSZDN(DER2,DER3,NDC,3,LU)
C
C  FIND THE 80% PASSING SIZE IN THE PRODUCT.
        CALL PASSSZ(DER2,DER3,NDC,0.8,PSIZEP)
        PSIZEP=PSIZEP*1.0E6
        PSIZEF=PSIZEF*1.0E6
        POWERF=10.0*(1.0/SQRT(PSIZEP)-1.0/SQRT(PSIZEF))*TONS
        SpecificPower = MP*PNum/TONS
        OPERATINGWI = MP*PNum/POWERF
        WRITE(LU,1052) PSIZEF,PSIZEP,SpecificPower,OPERATINGWI
 1052   FORMAT(/'80% passing size in feed   ',T50,G8.3,' microns'/
     *    '80% passing size in product',T50,G8.3,' microns'/
     *    'Calculated specific power input',T50,F6.2,' kWh/t'/
     *    'Calculated operating work index for this mill',
     *     T50,F5.1,'  kWh/t')


C Write a file with the data to draw the graph of the selection function.
C     CALL SelFuncGrSetUp(HERBST_FUER,NUNIT,SIZE,NDC,'HFMI')
      RETURN
      END