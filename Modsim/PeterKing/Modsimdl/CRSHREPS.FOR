C*******************     CRSHREPS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE REPORT GENERATORS FOR CRUSHER MODELS         C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE RGYRA(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
C  MODEL FOR GYRATORY  CRUSHER
C  *****************************
C
C  THIS SUBROUTINE ASSUMES THAT THE SIZE DISTRIBUTION IN THE
C  PRODUCT IS OF STANDARD TYPE
C  DATA FROM NORDBERG PROCESS MACHINERY REFERENCE MANUAL MAY 1976
C
C  PARAMETERS IN ORDER
C  *******************
C  1...OPEN SIDE SETTING IN METERS
C  2..."PRODUCT TYPE"
C  3...WORK INDEX OF MATERIAL
      G=PARAM(1)
      PT=PARAM(2)
C
C  WRITE THE DESIGN SPECIFICATION REPORT.
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'GYRATORY CRUSHER',16,'GYRA')
      TONS=TMSF*3.60
      WRITE(LU,1005) TONS
 1005 FORMAT(/'     Tonnage to be processed ',G10.4,' tons per hour')
      WS=G*100.0
      WRITE(LU,1006) WS
 1006 FORMAT(/'     Open side discharge setting',G10.4,' cms')
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(//5X,'Size distribution in feed'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      P=0.99
      CALL PASSSZ(DER2,DER3,NDC,P,PSIZE)
      PSIZE=PSIZE*100.00
      WRITE(LU,1160)PSIZE
 1160 FORMAT(/'     Top size in feed is approximately',F6.2,' cms')
      WS=1.25*PSIZE
      WRITE(LU,1020)WS
 1020 FORMAT(/5X,'Dimension A of crusher must be at least',F6.2,' cms')
      CALL FRPASS(DER2,DER3,NDC,P,G)
      WRITE(LU,1170)
 1170 FORMAT(/5X,'Minimum head diameter required to handle the tonnage..
     *...')
      CALL PASSSZ(DER2,DER3,NDC,0.80,F80)
      WS=P*100.0
      WRITE(LU,1022)WS
 1022 FORMAT(5X,'"Fall through" material in feed',F7.2,' percent.')
      IF(WS.GT.15.0) WRITE(LU,1023)
 1023 FORMAT(5X,'Scalping of the feed is recommended.')
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1025)
 1025 FORMAT(//5X,'Size distribution in product'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      CALL PASSSZ(DER2,DER3,NDC,0.80,P80)
      WS=PARAM(3)*10.0*(1.0/SQRT(P80*1.E6) - 1.0/SQRT(F80*1.E6))*TONS
      F80=F80*1000.0
      P80=P80*1000.0
      WRITE(LU,1024)PARAM(3),F80,P80,WS
 1024 FORMAT(/5X,'Impact work index of the material is',F6.2,' kWhr/ton'
     */5X,'80% passing size in feed is',F7.2,' mms.'/5X,'80% passing siz
     *e in product is',F7.2,' mms.'/5X,'Estimated power requirements ',
     *G9.3,' kW')
      WS=WS*1.35
      WRITE(LU,1060)WS
 1060 FORMAT(/5X,'Power requirement to allow for 35% surge capacity',G10
     *.4,' KW.')
      WRITE(LU,1070)
 1070 FORMAT(/5X,'Crusher selected.....')
      WRITE(LU,1030)
 1030 FORMAT(//5X,'Crusher capacity from manufacturer''s tables.....tons
     * per hour.')
      WRITE(LU,1040)
 1040 FORMAT(/5X,'Number of crushers required......')
      CALL NOTES(LU,5)
      RETURN
      END
C
      SUBROUTINE RCRSH(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
C
C  UNIT TYPE: STANDARD CONE CRUSHER
C  ********************************
C
C  THIS SUBROUTINE IS BASED ON THE CRUSHING ZONE AND INTERNAL CLASSIFICA
C  CLASSIFICATION MODEL DESCRIBED BY WHITEN ET AL.
C  REF:
C   1 WHITEN WJ THE SIMULATION OF CRUSHING PLANTS WITH MODELS
C     DEVELOPED USING MULTIPLE SPLINE REGRESSION. APPLICATION OF
C     COMPUTER METHODS IN THE MINERAL INDUSTRY. ED MDG SALAMON AND
C     LANCASTER. S. AFR.INST.MIN.METALL. JOHANNESBURG,1973. P317-323
C
C   2 WHITEN WJ, WALTER GW, AND WHITE ME, A BREAKAGE FUNCTION SUITABLE
C     FOR CRUSHER MODELS. 4TH TEWKSBURY SYPOSIUM,MELBOURNE,FEB 1973
C     P19.1-19.32
C
C  BREAKAGE AND CLASSIFICATION FUNCTIONS FROM REF 2 ABOVE
C
C  PARAMETERS IN ORDER
C  *******************
C  1...CLOSED SIDE SETTING IN METERS
C  2...PROPORTION OF FINES PRODUCED DURING BREAKAGE EVENTS
C  3...IMPACT WORK INDEX OF MATERIAL
C  4...FACTOR FOR CLASSIFICATION PARAMETER K1
C  5...FACTOR FOR CLASSIFICATION PARAMETER K2
      CSS=PARAM(1)
      KK=PARAM(2)
      K1=PARAM(4)*CSS
      K2=PARAM(5)*CSS
C
C  WRITE DESIGN REPORT.
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'STANDARD CONE CRUSHER',21,'CRSH')
	WRITE(LU,1029)PARAM(2),PARAM(4),PARAM(5)
 1029 FORMAT(' PARAMETERS:'/
     *       '  Fines fraction ',F7.4,/
     *       '  Classification function parameters ',F6.3,F7.3)
      TONS=TMSF*3.6
      WRITE(LU,1030)TONS
 1030 FORMAT(/5X,'Tonnage to be processed',G10.4,' tons per hour')
      G=CSS*1000.
      WRITE(LU,1040)G
 1040 FORMAT(/5X,'Closed side setting',F7.2,' mms')
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(//5X,'Size distribution in feed'/)
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      P=0.99
      CALL PASSSZ(DER2,DER3,NDC,P,PSIZE)
      PSIZE=PSIZE*1000.0
      WRITE(LU,1060)PSIZE
 1060 FORMAT(/'     Top size in feed is approximately',G9.3,' mms')
      WS=PSIZE/0.8
      WRITE(LU,1070)WS
 1070 FORMAT(/5X,'Open side feed opening must be at least',G9.3,' mms')
      WRITE(LU,1080)
 1080 FORMAT(//5X,'Size distribution in product')
      CALL PASSSZ(DER2,DER3,NDC,0.80,F80)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      WRITE(LU,1090)
 1090 FORMAT(//5X,'Select a crusher size and cavity type to suit the clo
     *sed side setting and'/5X,'feed opening requirements.')
      WRITE(LU,1100)
 1100 FORMAT(/5X,'Crusher selected is......')
      WRITE(LU,1110)
 1110 FORMAT(/5X,'Capacity in tons per hour? ....')
      WRITE(LU,1120)
 1120 FORMAT(/5X,'Number of crushers required?....')
      CALL PASSSZ(DER2,DER3,NDC,0.80,P80)
      WS=PARAM(3)*10.0*(1.0/SQRT(P80*1.E6) - 1.0/SQRT(F80*1.E6))*TONS
      F80=F80*1000.0
      P80=P80*1000.0
      WRITE(LU,1024)PARAM(3),F80,P80,WS
 1024 FORMAT(/5X,'Impact work index of the material is',F6.2,' kWhr/ton'
     */5X,'80% passing size in feed is',F7.2,' mms.'/5X,'80% passing siz
     *e in product is',F7.2,' mms.'/5X,'Estimated power requirements ',
     *G9.3,' kW')
      CALL NOTES(LU,5)
      RETURN
      END
C
C
      SUBROUTINE RSHHD(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
C
C  UNIT TYPE: SHORT HEAD CONE CRUSHER
C  **********************************
C
C  THIS SUBROUTINE IS BASED ON THE CRUSHING ZONE AND INTERNAL CLASSIFICA
C  CLASSIFICATION MODEL DESCRIBED BY WHITEN ET AL.
C  REFS:
C   1 WHITEN WJ THE SIMULATION OF CRUSHING PLANTS WITH MODELS
C     DEVELOPED USING MULTIPLE SPLINE REGRESSION. APPLICATION OF
C     COMPUTER METHODS IN THE MINERAL INDUSTRY. ED MDG SALAMON AND
C     LANCASTER. S. AFR.INST.MIN.METALL. JOHANNESBURG,1973. P317-323
C
C   2 WHITEN WJ, WALTER GW, AND WHITE ME, A BREAKAGE FUNCTION SUITABLE
C     FOR CRUSHER MODELS. 4TH TEWKSBURY SYPOSIUM,MELBOURNE,FEB 1973
C     P19.1-19.32
C
C   3 Karra V K. A process performance model for cone crushers. PROC.
C     15th INT. MINERAL PROCESSING CONGRESS. TORONTO. CAN INST.MIN.
C     METALL. 1982. pp III-6.1 - III-6.14.
C
C  BREAKAGE AND CLASSIFICATION FUNCTIONS FROM REF 3 ABOVE.
C
C  PARAMETERS IN ORDER
C  *******************
C  1...CLOSED SIDE SETTING IN METERS
C  2...PROPORTION OF FINES PRODUCED DURING BREAKAGE EVENTS
C  3...IMPACT WORK INDEX OF MATERIAL
C  4...FACTOR FOR CLASSIFICATION PARAMETER K1
C  5...FACTOR FOR CLASSIFICATION PARAMETER K2
      CSS=PARAM(1)
      KK=PARAM(2)
      K1=PARAM(4)*CSS
      K2=PARAM(5)*CSS+0.004826
C
C  WRITE AN EQIPMENT DESIGN REPORT.
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'SHORT HEAD CONE CRUSHER',23,'SHHD')
	WRITE(LU,1029)PARAM(2),PARAM(4),PARAM(5)
 1029 FORMAT(' PARAMETERS:'/
     *       '  Fines fraction ',F7.4,/
     *       '  Breakage function K(x/y)^0.518 + (1-K)(x/y)^2.475'/
     *       '  Classification function parameters ',F6.3,F7.3,' n=2')
      TONS=TMSF*3.6
      WRITE(LU,1030)TONS
 1030 FORMAT(/5X,'Tonnage to be processed',G10.4,' tons per hour')
      G=CSS*1000.
      WRITE(LU,1040)G
 1040 FORMAT(/5X,'Closed side setting',F7.2,' mms')
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(//5X,'Size distribution in feed'/)
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      P=0.99
      CALL PASSSZ(DER2,DER3,NDC,P,PSIZE)
      PSIZE=PSIZE*1000.0
      WRITE(LU,1060)PSIZE
 1060 FORMAT(/'     Top size in feed is approximately',G9.3,' mms')
      WS=PSIZE/0.8
      WRITE(LU,1070)WS
 1070 FORMAT(/5X,'Open side feed opening must be at least',G9.3,' mms')
      WRITE(LU,1080)
 1080 FORMAT(//5X,'Size distribution in product')
      CALL PASSSZ(DER2,DER3,NDC,0.80,F80)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      CALL WRSZDN(DER2,DER3,NDC,3,LU)
      WRITE(LU,1090)
 1090 FORMAT(//5X,'Select a crusher size and cavity type to suit the clo
     *sed side setting and'/5X,'feed opening requirements.')
      WRITE(LU,1100)
 1100 FORMAT(/5X,'Crusher selected is......')
      WRITE(LU,1110)
 1110 FORMAT(/5X,'Capacity in tons per hour? ....')
      WRITE(LU,1120)
 1120 FORMAT(/5X,'Number of crushers required?....')
      CALL PASSSZ(DER2,DER3,NDC,0.80,P80)
      WS=PARAM(3)*10.0*(1.0/SQRT(P80*1.E6) - 1.0/SQRT(F80*1.E6))*TONS
      F80=F80*1000.0
      P80=P80*1000.0
      WRITE(LU,1024)PARAM(3),F80,P80,WS
 1024 FORMAT(/5X,'Impact work index of the material is',F6.2,' kWhr/ton'
     */5X,'80% passing size in feed is',F7.2,' mms.'/5X,'80% passing siz
     *e in product is',F7.2,' mms.'/5X,'Estimated power requirements ',
     *G9.3,' kW')
      CALL NOTES(LU,5)
      RETURN
      END
C
C
      SUBROUTINE RCRS1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      CHARACTER*7 UNITS
      COMMON /MODELDAT/NUNIT
C
C  PARAMETERS IN ORDER
C  *******************
C  1...CLOSED SIDE SETTING IN METERS
C
      G=PARAM(1)
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'SYMONS CONE CRUSHER',19,'CRS1')
      CALL SZSCAL(G,UNITS,FACTR,IPWR)
      WRITE(LU,2000) G*FACTR,UNITS
 2000 FORMAT('Closed side setting ',F6.1,A7)
      WRITE(LU,*)
      WRITE(LU,*)' This model reports no more information'
      RETURN
      END
C
C
C
      SUBROUTINE RJAW1(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
C  THIS SUBROUTINE ASSUMES THAT THE SIZE DISTRIBUTION IN THE
C  PRODUCT IS OF STANDARD TYPE.
C  DATA FROM NORDBERG PROCESS MACHINERY REFERENCE MANUAL MAY 1976
C
C  PARAMETERS IN ORDER
C  *******************
C  1...OPEN-SIDE SETTING IN METERS
C  2...IMPACT WORK INDEX OF MATERIAL
      ITERM=0
      G=PARAM(1)
C
C  WRITE THE DESIGN SPECIFICATION REPORT.
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'JAW CRUSHER',11,'JAW1')
      TONS=TMSF*3.60
      WRITE(LU,1005) TONS
 1005 FORMAT(/'     Tonnage to be processed ',G10.4,' tons per hour')
      WS=G*100.0
      WRITE(LU,1006) WS
 1006 FORMAT(/'     Open-side setting',G10.4,' cms')
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(//5X,'Size distribution in feed'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      P=0.99
      CALL PASSSZ(DER2,DER3,NDC,P,PSIZE)
      PSIZE=PSIZE*100.00
      WRITE(LU,1060)PSIZE
 1060 FORMAT(/'     Top size in feed is approximately',F6.2,' cms')
      WRITE(LU,1020)
 1020 FORMAT(/5X,'Specify crusher size to accommodate this......')
      CALL FRPASS(DER2,DER3,NDC,P,G)
      WS=P*100.0
      WRITE(LU,1022)WS
 1022 FORMAT(5X,'"Fall through" material in feed',F7.2,' percent.')
      IF(WS.GT.15.0) WRITE(LU,1023)
 1023 FORMAT(5X,'Scalping of the feed is recommended.')
      CALL PASSSZ(DER2,DER3,NDC,0.80,F80)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1025)
 1025 FORMAT(//5X,'Size distribution in product'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      WRITE(LU,1030)
 1030 FORMAT(//5X,'Crusher capacity from manufacturer''s tables.....tons
     * per hour.')
      WRITE(LU,1040)
 1040 FORMAT(/5X,'Number of crushers required......')
      CALL PASSSZ(DER2,DER3,NDC,0.80,P80)
      WS=PARAM(2)*10.0*(1.0/SQRT(P80*1.E6) - 1.0/SQRT(F80*1.E6))*TONS
      F80=F80*1000.0
      P80=P80*1000.0
      WRITE(LU,1024)PARAM(2),F80,P80,WS
 1024 FORMAT(/5X,'Impact work index of the material is',F6.2,' kWhr/ton'
     */5X,'80% passing size in feed is',F7.2,' mms.'/5X,'80% passing siz
     *e in product is',F7.2,' mms.'/5X,'Estimated power requirements ',
     *G9.3,' kW')
      CALL NOTES(LU,5)
      RETURN
      END
C
C
C
      SUBROUTINE RJAW2(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT
C
C  MODEL FOR JAW CRUSHER
C  *********************
C
C  THIS SUBROUTINE ASSUMES THAT THE SIZE DISTRIBUTION IN THE
C  PRODUCT IS OF STANDARD TYPE.
C  DATA FROM SAMANCOR'S MAMATWAN CRUSHING PLANT.
C
C  PARAMETERS IN ORDER
C  *******************
C  1...OPEN-SIDE SETTING IN METERS
C  2...Impact work index of the material in this crusher

      G=PARAM(1)
C
C  WRITE THE DESIGN SPECIFICATION REPORT.
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'JAW CRUSHER',11,'JAW2')
      TONS=TMSF*3.60
      WRITE(LU,1005) TONS
 1005 FORMAT(/'     Tonnage to be processed ',G10.4,' tons per hour')
      WS=G*100.0
      WRITE(LU,1006) WS
 1006 FORMAT(/'     Open-side setting',G10.4,' cms')
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(//5X,'Size distribution in feed'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      P=0.99
      CALL PASSSZ(DER2,DER3,NDC,P,PSIZE)
      PSIZE=PSIZE*100.00
      WRITE(LU,1060)PSIZE
 1060 FORMAT(/'     Top size in feed is approximately',F6.2,' cms')
      CALL FRPASS(DER2,DER3,NDC,P,G)
      WS=P*100.0
      WRITE(LU,1022)WS
 1022 FORMAT(5X,'"Fall through" material in feed',F7.2,' percent.')
      IF(WS.GT.15.0) WRITE(LU,1023)
 1023 FORMAT(5X,'Scalping of the feed is recommended.')
      CALL PASSSZ(DER2,DER3,NDC,0.80,F80)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1025)
 1025 FORMAT(//5X,'Size distribution in product'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      CALL PASSSZ(DER2,DER3,NDC,0.80,P80)
      WS=PARAM(2)*10.0*(1.0/SQRT(P80*1.E6) - 1.0/SQRT(F80*1.E6))*TONS
      F80=F80*1000.0
      P80=P80*1000.0
      WRITE(LU,1024)PARAM(2),F80,P80,WS
 1024 FORMAT(/5X,'Impact work index of the material is',F6.2,' kWhr/ton'
     */5X,'80% passing size in feed is',F7.2,' mms.'/5X,'80% passing siz
     *e in product is',F7.2,' mms.'/5X,'Estimated power requirements ',
     *G9.3,' kW')
      RETURN
      END

C
      SUBROUTINE REMJC(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2
     *,DER3,NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPP
     *,GRDM,GRDV,NMIN,NGCM)
C     ******************************************************************
C
      INTEGER INDPP(NPP,2),FL
      REAL FEED(NDC,NGC,NSC)
      REAL OUT1(NDC,NGC,NSC),OUT2(NDC,NGC,NSC),OUT3(NDC,NGC,NSC)
      REAL DER1(NDC,NGC,NSC),DER2(NDC,NGC,NSC),DER3(NDC,NGC,NSC)
      REAL GRDM(NGCM,NMIN),GRDV(NGCM,NMIN)
      REAL SIZE(1),PARAM(1),PPROP(1)
      COMMON /MODELDAT/NUNIT

c Receive information about water feed to this unit.
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C
C  Empirical Model for Jaw and Gyratory Crushers

C  PARAMETERS IN ORDER
C  *******************
C  1...Crusher gap
C  2...rmax
C  3...Coefficient m
C  4...Impact work index of the material in this crusher

      GAP = PARAM(1)
      RMAX = PARAM(2)
      AM = PARAM(3)
C
C  WRITE THE DESIGN SPECIFICATION REPORT.
      LU=8
      ITERM=0
      CALL HEADER(LU,ITERM,NUNIT,'CRUSHER',7,'EMJC')
      TONS=TMSF*3.60
      WRITE(LU,1005) TONS
 1005 FORMAT(/'     Tonnage to be processed ',G10.4,' tons per hour')
      WRITE(LU,1006) GAP*100.0, RMAX, AM
 1006 FORMAT(/'     Crusher gap  ',G10.4,' cms'/
     *        '     rmax         ',G10.4/
     *        '     Coefficient m',G10.4 )
      CALL PARTSZ(SIZE,FEED,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1050)
 1050 FORMAT(//5X,'Size distribution in feed'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      P=0.99
      CALL PASSSZ(DER2,DER3,NDC,P,PSIZE)
      PSIZE=PSIZE*100.00
      WRITE(LU,1060)PSIZE
 1060 FORMAT(/'     Top size in feed is approximately',F6.2,' cms')
      CALL FRPASS(DER2,DER3,NDC,P,GAP)
      WS=P*100.0
      WRITE(LU,1022)WS
 1022 FORMAT(5X,'"Fall through" material in feed',F7.2,' percent.')
      IF(WS.GT.15.0) WRITE(LU,1023)
 1023 FORMAT(5X,'Scalping of the feed is recommended.')
      CALL PASSSZ(DER2,DER3,NDC,0.80,F80)
      CALL PARTSZ(SIZE,OUT1,NDC,NGC,NSC,DER2,DER3,DER1)
      WRITE(LU,1025)
 1025 FORMAT(//5X,'Size distribution in product'/)
      CALL WRSZDN(DER2,DER3,NDC,2,LU)
      CALL PASSSZ(DER2,DER3,NDC,0.80,P80)
      WS=PARAM(4)*10.0*(1.0/SQRT(P80*1.E6) - 1.0/SQRT(F80*1.E6))*TONS
      F80=F80*1000.0
      P80=P80*1000.0
      WRITE(LU,1024)PARAM(4),F80,P80,WS
 1024 FORMAT(/5X,'Impact work index of the material is',F6.2,' kWhr/ton'
     */5X,'80% passing size in feed is',F7.2,' mms.'/5X,'80% passing siz
     *e in product is',F7.2,' mms.'/5X,'Estimated power requirements ',
     *G9.3,' kW')
      RETURN
      END
