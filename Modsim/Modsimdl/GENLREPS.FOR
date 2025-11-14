C*******************     GENLREPS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS REPORT WRITING SUBROUTINES FOR SOME OF THE       C
C  SUBROUTINES IN GENLMODS.FOR                                         C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C

      SUBROUTINE RSPLT(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT

      LU=8
      ITERM=5
      CALL HEADER (LU,ITERM,NUNIT,'SPLITTER',8,'SPLT')
      IP = NINT(PARAM(1))
      WRITE(LU,1001) IP
 1001 FORMAT(/' This unit splits uniformly ',I2,' ways')
      RETURN
      END

      SUBROUTINE RSPL1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT

      LU=8
      ITERM=5
      CALL HEADER (LU,ITERM,NUNIT,'SPLITTER',8,'SPL1')
      IP = NINT(PARAM(1))
      WRITE(LU,1001) IP
 1001 FORMAT(/' This unit splits ',I2,' ways')
      WRITE(LU,1002)PARAM(2),PARAM(3),1.0 - PARAM(2) - PARAM(3)
 1002 FORMAT(' Fractional splits requested were ',3F7.3)


      NoSplits = NINT(PARAM(1))
      IF(NoSplits .EQ. 1) Then
        SP1 = 1.0
        SP2 = 0.0
        SP3 = 0.0
      ELSE IF (NoSplits .EQ. 2) THEN
        SP1 = PARAM(2)
        IF(SP1 .LT. 0.0) THEN
          SP1 = 0.0
        END IF
        IF(SP1 .GT. 1.0) THEN
          SP1 = 1.0
        END IF
        SP2 = 1.0 - SP1
        SP3 = 0.0
      ELSE IF (NoSplits .EQ. 3) THEN
        SP1 = PARAM(2)
        SP2 = PARAM(3)
        IF(SP1+SP2 .GT. 1.0) THEN
          SP2 = 1.0 - SP1
        END IF
        SP3 = 1.0 - SP1 - SP2
      ELSE
        RETURN
      End IF
      IF(SP1 .LT. 0.0) THEN
        SP1 = 0.0
      END IF
      IF(SP2 .LT. 0.0) THEN
        SP1 = 1.0
        SP2 = 0.0
      END IF
      IF(SP3 .LT. -0.00001) THEN
        SP3 = 0.0
        SP2 = 1.0 - SP1
      END IF
      WRITE(LU,'('' Splits used were '',3F7.3)')SP1,SP2,SP3
      RETURN
      END

      SUBROUTINE RPUMP(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT

      LU=8
      ITERM=5
      CALL HEADER (LU,ITERM,NUNIT,'pump',8,'PUMP')
	Write(LU,'('' This model requires no parameters'')')
      RETURN
      END

      SUBROUTINE RMIXR(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      RETURN
      END

      SUBROUTINE RCONV(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C      *****************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL  FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL  OUT3(NDC,NGC,NSC)
      REAL  DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL PARAM(*),PPROP(1)
      REAL SIZE(1),GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      COMMON /MODELDAT/NUNIT
      PARAMETER (PI = 3.141593)

      LU=8
      ITERM=5
      CALL HEADER (LU,ITERM,NUNIT,'CONVEYOR',8,'CONV')
      WRITE(LU,1001) PARAM(1),PARAM(2)*100,PARAM(3),PARAM(4)
 1001 FORMAT(/
     *'Parameters:'/
     *' Belt width',T35,F7.2,' m'/
     *' Freeboard',T35,F6.1,' cm'/
     *' Idler trough angle',T35,F6.1,' degrees'/
     *' Belt speed',T35,F7.2,' m/s' )
      WRITE(LU,1002) PARAM(5),PARAM(6)
 1002 FORMAT(//
     *' Angle of repose',T35,F6.1,' degrees'/
     *' Bulk density',T35,F9.1,' kg/m^3')

c Calculate the belt capacity
      ALPHAT = PARAM(3)*PI/180.0
      ALPHAS = (PARAM(5) - 12.5)*PI/180.0
      IF(ALPHAS .LT. 0.0) ALPHAS = 0.0
      H = PARAM(1)*SIN(ALPHAT)/3.0
      W = (PARAM(1)/3.0 - PARAM(2))*COS(ALPHAT)
      S = (W + PARAM(1)/6.0)*TAN(ALPHAS)
      AREA = (W + PARAM(1)/6.0)*(S + 2*H) - H*W
      CAPACITY = AREA*PARAM(4)*PARAM(6)
      WRITE(LU,1003) TMS1*3.6,CAPACITY*3.6
 1003 FORMAT(//
     *' Belt capacity required:  ',G10.4,' tonnes/hr'/
     *' Calculated belt capacity:',G10.4,' tonnes/hr')
      RETURN
      END

      SUBROUTINE RNOP_(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC),OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC)
      REAL PARAM(1),PPROP(1),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC),SIZE(1)
      REAL OUT3(NDC,NGC,NSC),DER1(NDC,NGC,NSC),GRDM(1),GRDV(1)
      COMMON NPLNT,NUNIT,ITER,IW,IFLAG
C
C   THIS SUBROUTINE PASSES THE STREAM STRAIGHT THROUGH THE UNIT
C   WITHOUT CHANGE
C
C   NO PARAMETERS
C   *************
C
      RETURN
      END
