C*********************** .......COAL.........**************************C
C                                                                      C
C  CONTAINS SUBROUTINES FOR COAL WASHING UNITS.                        C
C  REF: RUEDA MV AND KING RP  MODELS FOR COAL WASHING UNITS            C
C       UNIVERSITY OF THE WITWATERSRAND  DEPT OF METALLURGY REPORT     C
C       CSPCOAL 11  JULY 1986                                          C
C                                                                      C
C  MODSIM  (c) Copyright  R P KING  Johannesburg    1986               C
C**********************************************************************C
C
C
      SUBROUTINE GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,N
     *SC,SIZE,PPROP,INDPP,FL,NPP,CPSH,SCUF,SCOV,CORIMP,OPDENS,PARFUN)
C     ******************************************************************
C
C        This is a generalised subroutine for the calculation
C        of the output streams for a gravity operation unit.
C        The four parameters of the partition function are the
C        normalised cutpoint shift, short circuit to underflow
C        and overflow and the corrected imperfection.  These
C        can be a function of the particle size.  They are
C        evaluated in functions CPSH,SCUF,SCOV and CORIMP. The
C        corrected partition function is evaluated in function
C        PARFUN.
C
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC)
      REAL SIZE(1),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      TMS2=0.
      DO 15 ND=1,NDC
        SZ=SIZE(ND)
        WNCPS=CPSH(SZ)
        WSCUF=SCUF(SZ)
        WSCOV=SCOV(SZ)
        WCORIM=CORIMP(SZ)
        CUTPT=OPDENS*(1.0+WNCPS)
        DO 15 NG=1,NGC
          X=PPROP(NG)/CUTPT
          PF=1.0-WSCUF-(1.0-WSCUF-WSCOV)*PARFUN(X,WCORIM)
          DO 15 NS=1,NSC
            OUT2(ND,NG,NS)=FEED(ND,NG,NS)*PF
            DER2(ND,NG,NS)=PF
            OUT1(ND,NG,NS)=FEED(ND,NG,NS)-OUT2(ND,NG,NS)
            DER1(ND,NG,NS)=1.0-PF
            TMS2=TMS2+OUT2(ND,NG,NS)
   15 CONTINUE
      TMS1=TMSF-TMS2
      FL=1
      RETURN
      END
C
C
      FUNCTION PARFN1(X,CORIMP)
C     *************************
C
C       Corrected partition function using Lynch formula.
C       The partition to coarse or heavies is calculated.
C
      USE GLOBALS

      IF (X.LT.0.0.OR.CORIMP.LE.0.0) THEN
        WRITE (UNitDiagFile,*) 'WARNING: Invalid parameters for the partitio
     *n function'
        RETURN
      END IF
      B2=1.099/CORIMP

      WS=B2*(X-1.0)
      IF (WS.GE.14.0) THEN
        PARFN1=1.0
        RETURN
      END IF
      IF (WS.LE.-14.0) THEN
        PARFN1=0.0
        RETURN
      END IF
      PE=EXP(WS)

      IF (B2.GE.14.0) THEN
        EN=0.
      ELSE
        EN=EXP(-B2)
      END IF
      PARFN1=(PE-EN)/(PE+1.0-2.0*EN)
      RETURN
      END
C
C
      FUNCTION CORICY(X)
c     ******************
c Corrected imperfection for the dense-medium cyclone
c Estimated in the M V Rueda study and King R P and Juckes A H Performance
c of a dense-medium cyclone when beneficiating fine coal. Coal Preparation
c 5(1988)185-210
      COMMON /CDIAM/DIA
      IF (DIA.LE.0.20) CORICY=0.013+(3.8E-6/X)
      IF (DIA.GT.0.20) CORICY=0.01+(18.0E-6/X)
      RETURN
      END
C
C
      FUNCTION CYNCPS(X)
c     ******************
c Normalised cut point shift for the dense medium cyclone.
c Estimated in the M V Rueda study and King R P and Juckes A H Performance
c of a dense-medium cyclone when beneficiating fine coal. Coal Preparation
c 5(1988)185-210
      COMMON /CDIAM/DIA
      X1=X*1.E6
      IF (DIA.LE.0.20) CYNCPS=0.402*(X1**(-0.32))
      IF (DIA.GT.0.20) CYNCPS=0.081
      RETURN
      END
C
C
      FUNCTION DWNCPS(X)
      DWNCPS=0.006
      RETURN
      END
C
C
      FUNCTION DRNCPS(X)
      IF (X.GT.2.45E-3) DRNCPS=0.019
      IF (X.LE.2.45E-3) DRNCPS=0.051
      RETURN
      END
C
C
      FUNCTION SLNCPS(X)
      IF (X.GT.0.83E-3) SLNCPS=0.004
      IF (X.LE.0.83E-3) SLNCPS=0.020
      RETURN
      END
C
C
      FUNCTION CORIBJ(X)
      COMMON /CTMSF/TMS
      IF (TMS.LE.88.89) CORIBJ=0.044
      IF (TMS.GT.88.89.AND.TMS.LE.93.61) CORIBJ=0.134
      IF (TMS.GT.93.61.AND.TMS.LE.119.44) CORIBJ=0.087
      IF (TMS.GT.119.44) CORIBJ=0.054
      RETURN
      END
C
C
      FUNCTION SCOVBA(X)
      COMMON /CTMSF/TMS
      IF (TMS.GE.0.11E-3) SCOVBA=0.062
      IF (TMS.LT.0.11E-3) SCOVBA=0.100
      RETURN
      END
C
C
      FUNCTION CORIBA(X)
      CORIBA=0.06
      RETURN
      END
C
      FUNCTION CPSHBA(SZ)
C     *******************
C  CALCULATE CUTPOINT FOR THE BATAC JIG.
C  USE SG FOR EACH SIZE RELATIVE TO SG50 FOR THE COMPOSITE FEED.
C  DATA BASED ON THE MV RUEDA STUDY AND LM TAVARES and J RUBIO.
C  Performance evaluation and simulation of a batac jig cleaning
C  pyrite from coal washery tailings.  Presented at 4th Intnl. Conf.
C  on Processing and Utilization of High-Sulphur Coals. Idaho Falls 1991
      DP=1000.0*SZ
      SGR=0.95+0.112*DP**(-1.16)
      CPSHBA=SGR-1.0
      RETURN
      END
C
      FUNCTION SCUF(X)
      COMMON /CSCUF/SC
C Sets a constant value for the short circuit to underflow.
      SCUF=SC
      RETURN
      END
C
C
      FUNCTION SCOV(X)
C Sets a constant value for the short circuit to overflow.
      COMMON /CSCOV/SC
      SCOV=SC
      RETURN
      END
C
C
      FUNCTION CORIMP(X)
C Sets a constant value for the corrected imperfection.
      COMMON /CCORIM/CI
      CORIMP=CI
      RETURN
      END
C
C
      FUNCTION CPSH(X)
      COMMON/CCPSH/CPS
C Sets a constant value for the cut point shift.
      CPSH=CPS
      RETURN
      END

C The following functions calculate the cut point ratio to use the
c Gottfried Jacobsen method.
c See Gottfried B S and Jacobsen P S Generalized distribution curve for
c characterizing the performance of coal-cleaning equipment. USBM RI 8238
c 1977.
c Gottfried B S A generalization of distribution data for characterizing the
c performance of float-sink coal cleaning devices. Intl Jnl of Mineral Processing
c 5(1978) 1-20
c The estimates of the actual cut points are taken from the Rueda study.

      FUNCTION GJRDMCY(DP)
C     ********************
C Function to calculate the Gottfried-Jacobsen cutpoint ratio for the
c dense-medium cyclone. G-J ratios calculated from the Rueda study were
c used to develop the correlation. The cutpoint ratio is correlated in
c terms of the ratio of size to average size in the feed.
      COMMON /CNCPS/AV
      RATIO = DP/AV
      GJRDMCY = 0.98 + 0.1*EXP(-1.61*RATIO) - 1.0
      RETURN
      END

C
C
      SUBROUTINE DMCY(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
C     Simulation of the dense-medium cyclone using a modified version
C     of Lynch's equation for a partition curve.
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      EXTERNAL CYNCPS,CORICY,SCUF,SCOV,PARFN1,GJRDMCY
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CNCPS/AV,/CDIAM/DIAM
C
C  PARAMETERS:
C     1...Operating density of the medium OR the target cut point.
C     2...Cyclone diameter.
C     3...Selector for the model. 1=Gottfried-Jacobsen cut point ratio.
C          2=Cutpoint shift.
      SC=0.
      SCO=0.0
      OPDENS=PARAM(1)
      DIAM=PARAM(2)
      ISELECT = NINT(PARAM(3))

      IF(ISELECT .EQ. 1) THEN
        AV = AVSIZE(FEED,NDC,NGC,NSC,SIZE)
        CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,
     *  SIZE,PPROP,INDPP,FL,NPP,GJRDMCY,SCUF,SCOV,CORICY,OPDENS,PARFN1)
      END IF

      IF(ISELECT .EQ. 2) THEN
        CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,
     *  SIZE,PPROP,INDPP,FL,NPP,CYNCPS,SCUF,SCOV,CORICY,OPDENS,PARFN1)
      END IF

      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE DYNA(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
C     Simulation of the dynawhirlpool using a modified version
C     of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      EXTERNAL DWNCPS,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCORIM/CI
      SC=0.
      SCO=0.0
      CI=0.03
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,DWNCPS,SCUF,SCOV,CORIMP,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE NORW(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C     Simulation of the norwalt coal washer using a modified version
C     of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      EXTERNAL CPSH,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCORIM/CI,/CCPSH/CPS
      CPS=0.005
      SC=0.
      SCO=0.0
      CI=0.013
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,CPSH,SCUF,SCOV,CORIMP,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE WEMC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C     Simulation of the wemco drum coal washer using a modified
C     version of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      EXTERNAL CPSH,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCORIM/CI,/CCPSH/CPS
      CPS=-0.003
      SC=0.
      SCO=0.0
      CI=0.017
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,CPSH,SCUF,SCOV,CORIMP,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE TESK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C     Simulation of the Teska drum coal washer using a modified
C     version of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      EXTERNAL DRNCPS,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCORIM/CI
      SC=0.
      SCO=0.0
      CI=0.01
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,DRNCPS,SCUF,SCOV,CORIMP,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE DREW(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C     Simulation of the drewboy coal washer using a modified
C     version of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      EXTERNAL DRNCPS,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCORIM/CI
      SC=0.
      SCO=0.0
      CI=0.008
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,DRNCPS,SCUF,SCOV,CORIMP,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE CHAN(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C     Simulation of the chance sand coal washer using a modified
C     version of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      EXTERNAL CPSH,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCORIM/CI,/CCPSH/CPS
      CPS=0.033
      SC=0.
      SCO=0.0
      CI=0.015
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,CPSH,SCUF,SCOV,CORIMP,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE SLIP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C     Simulation of the shallow bath coal washer using a modified
C     version of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      EXTERNAL SLNCPS,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCORIM/CI
      SC=0.
      SCO=0.0
      CI=0.009
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,SLNCPS,SCUF,SCOV,CORIMP,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE BAUJ(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C     Simulation of the baum jig coal washer using a modified
C     version of Lynch's equation for a partition curve
C
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      EXTERNAL CPSH,CORIBJ,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC,/CSCOV/SCO,/CCPSH/CPS,/CTMSF/TMS
      TMS=TMSF
      CPS=0.
      SC=0.
      SCO=0.0
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,CPSH,SCUF,SCOV,CORIBJ,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE SHAK(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C
C Model for a concentrating table. The Gottfried-Jacobsen
c procedure is used to estimate the cut-point for each size class.
C The model is calibrated against data from USBM RI 6239.
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      EXTERNAL CPSH,CORIMP,SCUF,SCOV,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG

C     PARAMETERS
C     **********
C     1....Target cut-point for the separation.
C     2....Water recovery to tailing
C
      TMS2=0.0
      AV = AVSIZE(FEED,NDC,NGC,NSC,SIZE)
      DO 15 ND=1,NDC
        RATIO = SIZE(ND)/AV
        GJR = 0.97 + 0.3*EXP(-7.7*RATIO)
        CUTPT = GJR*PARAM(1)
        CORIM = 0.04 + 0.014/(SIZE(ND)*1000.0)
        DO 15 NG=1,NGC
          X = PPROP(NG)/CUTPT
          PF = PARFN1(X,CORIM)
          DO 15 NS=1,NSC
            OUT2(ND,NG,NS)=FEED(ND,NG,NS)*PF
            DER2(ND,NG,NS)=PF
            OUT1(ND,NG,NS)=FEED(ND,NG,NS)-OUT2(ND,NG,NS)
            DER1(ND,NG,NS)=1.0-PF
            TMS2=TMS2+OUT2(ND,NG,NS)
   15 CONTINUE
      TMS1 = TMSF-TMS2
      WTR2 = (1.0 - 0.01*PARAM(2))*WTR
      WTR1 = WTR - WTR2
      FL=1
      RETURN
      END
C
C
      SUBROUTINE BATJ(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NP
     *P,GRDM,GRDV,NMIN,NGCM)
C     *****************************************************************
C     Simulation of the Batac jig coal washer using a modified
C     version of Lynch's equation for a partition curve
C
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      EXTERNAL CPSHBA,CORIBA,SCUF,SCOVBA,PARFN1
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
      COMMON/CSCUF/SC
      SC=0.
      OPDENS=PARAM(1)
      CALL GRSEP(TMSF,TMS1,TMS2,FEED,OUT1,OUT2,DER1,DER2,NDC,NGC,NSC,SIZ
     *E,PPROP,INDPP,FL,NPP,CPSHBA,SCUF,SCOVBA,CORIBA,OPDENS,PARFN1)
      WTR2=WTR*(TMS2/TMSF)
      WTR1=WTR-WTR2
      RETURN
      END
C
C
      SUBROUTINE WASH(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(4,4),GRDV(4,4)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C     PARAMETERS
C     **********
C     1....SPECIFIC GRAVITY OF MEDIUM
C
C
C  COAL WASHING UNIT ACCORDING TO B.S.GOTTFRIED  INT J MIN PROCESS.
C  5(1978)1-20
C  DATA FOR COMPOSITE FEED TO THE DREWBOY.
C
      FO=0.01071
      A=3.7838
      B=0.00017926
      C=0.9792
      XO=0.90713
      TMS1=0.0
      DO 20 I=1,NDC
      DO 20 J=1,NGC
      DO 20 K=1,NSC
      WS=PPROP(J)/PARAM(1)-XO
      IF(WS.GT.0.0) THEN
        WS=(WS**A)/B
        IF(WS.LT.10.0) THEN
          F=FO+C*EXP(-WS)
        ELSE
          F=FO
        END IF
      ELSE
        F=FO+C
      END IF
      OUT1(I,J,K)=FEED(I,J,K)*(1.0-F)
      OUT2(I,J,K)=FEED(I,J,K)-OUT1(I,J,K)
      TMS1=TMS1+OUT1(I,J,K)
   20 CONTINUE
      TMS2=TMSF-TMS1
      WTR2=(TMS2/TMSF)*WTR
      WTR1=WTR-WTR2
      RETURN
      END

      SUBROUTINE DRUM(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(4,4),GRDV(4,4)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C Model for a dense-medium coarse coal washing drum. The Gottfried-Jacobsen
c procedure is used to estimate the cut-point for each size class.

C     PARAMETERS
C     **********
C     1....Target cut-point for the separation.
C
      TMS2=0.0
      AV = AVSIZE(FEED,NDC,NGC,NSC,SIZE)
      DO 15 ND=1,NDC
        RATIO = SIZE(ND)/AV
        GJR = 0.98 + 0.1*EXP(-1.61*RATIO)
        CUTPT = GJR*PARAM(1)
        CORIM = 0.015 + 0.25/(SIZE(ND)*1000.0)
        DO 15 NG=1,NGC
          X=PPROP(NG)/CUTPT
          PF= 1.0 - PARFN1(X,CORIM)
          DO 15 NS=1,NSC
            OUT2(ND,NG,NS)=FEED(ND,NG,NS)*PF
            DER2(ND,NG,NS)=PF
            OUT1(ND,NG,NS)=FEED(ND,NG,NS)-OUT2(ND,NG,NS)
            DER1(ND,NG,NS)=1.0-PF
            TMS2=TMS2+OUT2(ND,NG,NS)
   15 CONTINUE
      TMS1 = TMSF-TMS2
      WTR1 = TMS1*4.0/6.0
      IF(WTR1 .GT. WTR) WTR1 = 0.9*WTR
      WTR2 = WTR - WTR1
      FL=1
      RETURN
      END
C
      SUBROUTINE WOCY(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP,
     *GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(4,4),GRDV(4,4)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C A simple model for the water-only cyclone. Two models are provided for the
c calculation  of the cutpoint.
c 1..The Gottfried-Jacobsen using the generalized relationship between the
c    cutpoint ratio and the ration of particle size to the average size in
c    the feed.
c 2..A simple partition function model based on the data presented by Hornsby D T, Watson S J and
C    Clarkson C J. Fine coal cleaning by spiral and water washing cyclone.
C    Coal Preparation 1993 12 pp 133-161.
C    The effective cut point is determined primarily by the vortex finder
C    clearance.
C    The performance is specified by providing an estimate of the cut point
C    at 1mm.
C
C The efficiency of separation is calculated following Hornsby et. al.
c the imperfection defined as EPM/(d50 - 1) is assumed to be invariant
c with particle size.

C     PARAMETERS
C     **********
C     1....Selection of the cut point model 1=Gottfried-Jacobsen 2=Hornsby
C     2....Target cutpoint for the G-J procedure or cutpoint at 1mm for the
C          Hornsby procedure.
C     3....Number of stages. (1 or 2)
C
      ISELECT = NINT(PARAM(1))
      AV = AVSIZE(FEED,NDC,NGC,NSC,SIZE)
      TMS2=0.0
      DO 15 ND = 1,NDC
        IF(ISELECT .LE. 1)
     *  CUTPT = PARAM(2)*(0.8+0.6*EXP(-1.26*SIZE(ND)/AV))
        IF(ISELECT .GE. 2)
     *  CUTPT = 1.0 + (PARAM(2) - 1.0)*(SIZE(ND)/0.001)**(-0.5)
        EPM = 0.24*(CUTPT - 1.0)
        CORIM = EPM/CUTPT
        DO 15 NG = 1,NGC
          X = PPROP(NG)/CUTPT
          PF = 1.0 - PARFN1(X,CORIM)
          IF (PARAM(3) .GE. 2) PF = PF/(1.0 - PF + PF*PF)
          DO 15 NS = 1,NSC
            OUT2(ND,NG,NS) = FEED(ND,NG,NS)*PF
            DER2(ND,NG,NS) = PF
            OUT1(ND,NG,NS) = FEED(ND,NG,NS) - OUT2(ND,NG,NS)
            DER1(ND,NG,NS) = 1.0 - PF
            TMS2 = TMS2 + OUT2(ND,NG,NS)
   15 CONTINUE
      TMS1 = TMSF - TMS2
      WTR1 = TMS1*4.0/6.0
      IF(WTR1 .GT. WTR) WTR1 = 0.9*WTR
      WTR2 = WTR - WTR1
      FL=1
      RETURN
      END

C
      SUBROUTINE SJIG(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
c     ******************************************************************
C     Model for the jig using stratification.
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

      hs = PARAM(1)
      alpha = PARAM(2)
      IF(NGC .EQ. 2) THEN
        CALL BinStratify(FEED,OUT1,OUT2,NDC,NGC,NSC,SIZE,
     *  hs,alpha,PPROP,NPP,INDPP)
      END IF
      IF (NGC .GT. 2) THEN
        CALL N_Stratify(FEED,OUT1,OUT2,NDC,NGC,NSC,SIZE,
     *  hs,alpha,PPROP,NPP)
      END IF
      TMS1 = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TMS1 = TMS1 + OUT1(I,J,K)
          END DO
        END DO
      END DO
      TMS2 = TMSF - TMS1
      WTR1 = TMS1*WTR/TMSF
      WTR2 = WTR - WTR1

c Calculate the water flows if water is added to the jig.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
        END IF
      END IF

      RETURN
      END
C
      SUBROUTINE VJIG(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
c     ******************************************************************
C     Model for the jig using stratification.
      USE GLOBALS
      REAL FEED (NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(*),PPROP(1)
      INTEGER INDPP(NPP,2),FL
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
	Real kappa


c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

      hs = PARAM(1)
      alpha = PARAM(2)
	kappa = PARAM(3)
      IF (NGC .GT. 1) THEN
        CALL N_StratifyV(FEED,OUT1,OUT2,NDC,NGC,NSC,SIZE,
     *  hs,alpha,kappa,PPROP,NPP)
      END IF
      TMS1 = 0.0
      DO I = 1,NDC
        DO J = 1,NGC
          DO K = 1,NSC
            TMS1 = TMS1 + OUT1(I,J,K)
          END DO
        END DO
      END DO
      TMS2 = TMSF - TMS1
      WTR1 = TMS1*WTR/TMSF
      WTR2 = WTR - WTR1

c Calculate the water flows if water is added to the jig.
      IF(UNITWFEED) THEN
        IF(SOLIDCONT .LE. 0.0) THEN
          WTR1 = WTR1 + WATERADD
        ELSE
          WTR1 = TMS1*(100.0 - SOLIDCONT)/SOLIDCONT
        END IF
      END IF

      RETURN
      END

