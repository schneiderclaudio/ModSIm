C**************************  CALC1  ***********************************C
C                                                                      C
C  MODSIM   Modular simulation of ore dressing plants.                 C
C                                                                      C
C  CONTINUATION OF FILE CALC                                           C
C                                                                      C
C  MODSIM (C) COPYRIGHT  R P KING and M A FORD  JOHANNESBURG  1985.    C
C**********************************************************************C
C
C
      SUBROUTINE STOUT(VALS,LS,NDC,NGC,NSC,NMIN,NAMES,FM,GRDM,ITYP,
     *IW,IO,ITEAR,NGCZ,NMINZ,DiagFile,STAT)
C     *************************************************************
      USE MSFLIB
      USE GLOBALS

      INTEGER STAT
      DIMENSION VALS(1),GRDM(NGCZ,NMINZ),FM(1)
      CHARACTER*4 NAMES(1)
      INTEGER*4 DiagFile
      LOGICAL*4 response
C
      LENG = LEN_TRIM(UNITJobPath)
      TMS=VALS(LS-2)
C
      IF(ITYP.EQ.6) GO TO 9
      IF(ITYP.EQ.5) GO TO 100
      IF(ITYP.NE.1.AND.ITYP.NE.3) GO TO 200
C
C **WRITE MASS IN EACH CATEGORY
C
 9    WRITE(IW,2100)
 2100 FORMAT(/3X,'MASS IN EACH CATEGORY:')
      WRITE(IW,2101) (I,I=1,NDC)
      DO 11 K=1,NSC
        DO 10 J=1,NGC
          IP1=(J-1)*NDC+(K-1)*NDC*NGC+LS
          IP2=IP1+NDC-1
          II=IP2-IP1+1
          WRITE(IW,2102) K,J,(VALS(I),I=IP1,IP2)
 8120     FORMAT(3I4,/(6G13.5))
          IF(ITYP.GT.2)WRITE(IO,8120) K,J,II,(VALS(I),I=IP1,IP2)
 8121     FORMAT(8G10.4,/(8G10.4))
          IF(STAT .EQ. 5) THEN
            WRITE(ITEAR,8121) (VALS(I),I=IP1,IP2)
          END IF
 10     CONTINUE
 11   CONTINUE
      IF(ITYP.EQ.6) WRITE(IW,2106)
C
      IF(ITYP.EQ.6) GO TO 100
C
 2101 FORMAT(35X,'D-CLASSES'/(10X,8I8))
 2106 FORMAT(1X)
 2102 FORMAT(' SCL',I2,' GCL',I2,8F8.2,/(12X,8F8.2))
      RETURN
C
C **WRITE MASS IN EACH MINERAL
C
 100  DO 101 M=1,NMIN
      FM(M)=0.0
 101  CONTINUE
C
      DO 110 J=1,NGC
      TM=0.0
      DO 108 I=1,NDC
      DO 107 K=1,NSC
      IPS=I+(J-1)*NDC+(K-1)*NDC*NGC+LS-1
      TM=TM+VALS(IPS)
 107  CONTINUE
 108  CONTINUE
      DO 109 M=1,NMIN
      FM(M)=TM*GRDM(J,M)+FM(M)
 109  CONTINUE
 110  CONTINUE
C
      WRITE(IW,2104) (NAMES(I),I=1,NMIN)
      WRITE(IW,2107) (FM(I),I=1,NMIN)
      DO 111 I=1,NMIN
      IF(TMS.NE.0.0) FM(I)=FM(I)/TMS*100.0
 111  CONTINUE
      WRITE(IW,2108) (FM(I),I=1,NMIN)
      DO 112 I=1,NMIN
      FM(I)=FM(I)*TMS/100.0
  112 CONTINUE
C
 2104 FORMAT(12X,12(4X,A4))
 2107 FORMAT(6X,'MASSES:',12F8.2)
 2108 FORMAT(5X,'GRADE %:',12F8.2)
      RETURN
C
C **WRITE ERROR MESSAGE
C
 200  WRITE(DiagFile,2105)
 2105 FORMAT(10X,'Impossible output code found in calculation phase'/)
      response = COMMITQQ(DiagFile)
      ExitValue = 35
      RETURN
      END
C
C
      SUBROUTINE CONVRT(FTVEC,POSF,NDCL,NGCL,NSCL,CNCVAL,POSC,NPLT,MPLT,
     *DIMFT,DIMCNC,NPLANT)
C     ****************************************************************
C
      INTEGER NDCL(NPLANT),NGCL(NPLANT),NSCL(NPLANT),POSF,POSC,DIMFT,DIM
     *CNC
      REAL FTVEC(DIMFT),CNCVAL(DIMCNC)
C
C
      NDM=NDCL(MPLT)
      NGM=NGCL(MPLT)
      NSM=NSCL(MPLT)
      NDN=NDCL(NPLT)
      NGN=NGCL(NPLT)
      NSN=NSCL(NPLT)
C
C **CONDENSE TO COMMON BASE
C
      DO 12 K=1,NSM
      DO 11 J=1,NGM
      DO 10 I=1,NDM
      I1=I
      J1=J
      K1=K
      L=I+(J-1)*NDM+(K-1)*NDM*NGM+POSC+1
      IF(NDN.EQ.1) I1=1
      IF(NGN.NE.NGM) J1=1
      IF(NSN.NE.NSM) K1=1
      L1=I1+(J1-1)*NDM+(K1-1)*NDM*NGM+POSC+1
      IF(L1.EQ.L) GO TO 10
      CNCVAL(L1)=CNCVAL(L1)+CNCVAL(L)
 10   CONTINUE
 11   CONTINUE
 12   CONTINUE
C
C
C **CHECK IF EXPANSION IS NECESSARY
C
      ND=0
      IF(NDN.LT.NDM.AND.NDN.GT.1) ND=NDM-NDN
      IF(NGN.NE.NGM.AND.NGN.GT.1) GO TO 29
      IF(NSN.NE.NSM.AND.NSN.GT.1) GO TO 29
C
C
C **PUT CNCVAL INTO FTVEC WITH NO EXPANSION
C
      DO 22 K=1,NSN
      DO 21 J=1,NGN
      DO 20 I=1,NDN
      LM=I+(J-1)*NDM+(K-1)*NDM*NGM+ND+POSC+1
      LN=I+(J-1)*NDN+(K-1)*NDN*NGN+POSF+1
      FTVEC(LN)=CNCVAL(LM)
 20   CONTINUE
 21   CONTINUE
 22   CONTINUE
      GO TO 39
C
C
C **EXPAND CNCVAL INTO FTVEC
C
 29   DO 32 K=1,NSN
      DO 31 J=1,NGN
      DO 30 I=1,NDN
      I1=I
      J1=J
      K1=K
      L=I+(J-1)*NDN+(K-1)*NDN*NGN+POSF+1
      IF(NDN.EQ.1) I1=1
      IF(NGN.NE.NGM) J1=1
      IF(NSN.NE.NSM) K1=1
      L1=I1+(J1-1)*NDM+(K1-1)*NDM*NGM+ND+POSC+1
      FTVEC(L)=FTVEC(L)*CNCVAL(L1)
 30   CONTINUE
 31   CONTINUE
 32   CONTINUE
C
C
C **SUM MASS IN EACH CATEGORY
C
 39   FTVEC(POSF+1)=CNCVAL(POSC+1)
      S=0.0
      NV=NDN*NGN*NSN+1
      DO 40 I=2,NV
      S=S+FTVEC(POSF+I)
 40   CONTINUE
      FTVEC(POSF)=S
C
      RETURN
      END
C
C
      SUBROUTINE CONSRV(TMSF,TMS1,TMS2,TMS3,OUT1,OUT2,OUT3,WTR,WTR1,
     $WTR2,WTR3,NVAR,STRVEC,NI,M1,DiagFile,ExitValue)
C     ****************************************************************
C  CHECKS THAT A UNIT IS CONSERVATIVE.
C
      USE MSFLIB

      REAL OUT1(1),OUT2(1),OUT3(1)
      INTEGER STRVEC(1)
      INTEGER*4 DiagFile
      LOGICAL*4 response
      Integer*4 ExitValue

      INTEGER UNIT,FLAG

      COMMON NPLT,UNIT,ITER,IW,FLAG
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT

C      Write(DiagFile,*)' In CONSRV'
C      Write(DiagFile,*) 'NI',NI
C      WRITE(DiagFile,*)' TMSF,TMS1,TMS2,TMS3',TMSF,TMS1,TMS2,TMS3
      TM=0.0
      WM=0.0
      TT1=0.0
      TT2=0.0
      TT3=0.0
      NV2=NVAR-2
      DO 261 I=1,NI
        IF(I.EQ.1) TM=TM+TMS1
        IF(I.EQ.2) TM=TM+TMS2
        IF(I.EQ.3) TM=TM+TMS3
        IF(I.EQ.1) WM=WM+WTR1
        IF(I.EQ.2) WM=WM+WTR2
        IF(I.EQ.3) WM=WM+WTR3
        DO 262 J=1,NV2
          IF(I.EQ.1) TT1=TT1+OUT1(J)
          IF(I.EQ.2) TT2=TT2+OUT2(J)
          IF(I.EQ.3) TT3=TT3+OUT3(J)
 262    CONTINUE
        ISR=STRVEC(M1+I)
        IF(ABS(TMS1-TT1).GT.(0.01*TT1).AND.I.EQ.1) THEN
          WRITE(IW,3003)UNIT,ISR,TMS1,TT1
          WRITE(DiagFile,3003)UNIT,ISR,TMS1,TT1
          response = COMMITQQ(DiagFile)
          ExitValue = 36
        END IF
        IF(ABS(TMS2-TT2).GT.(0.01*TT2).AND.I.EQ.2) THEN
          WRITE(IW,3003)UNIT,ISR,TMS2,TT2
          WRITE(DiagFile,3003)UNIT,ISR,TMS2,TT2
          response = COMMITQQ(DiagFile)
          ExitValue = 36
        END IF
        IF(ABS(TMS3-TT3).GT.(0.01*TT3).AND.I.EQ.3) THEN
          WRITE(IW,3003)UNIT,ISR,TMS3,TT3
          WRITE(DiagFile,3003)UNIT,ISR,TMS3,TT3
          response = COMMITQQ(DiagFile)
          ExitValue = 36
        END IF
 261  CONTINUE
      IF(ABS(TM-TMSF).GT.(0.001*TMSF)) THEN
        WRITE(IW,3001) UNIT
        WRITE(DiagFile,3001)UNIT
        WRITE(IW,3004)TM,TMSF
        WRITE(DiagFile,3004)TM,TMSF
 3004   FORMAT(' TOTAL SOLIDS LEAVING=',G10.4,' TOTAL SOLIDS IN FEED=',
     *  G10.4)
        response = COMMITQQ(DiagFile)
        ExitValue = 36
      END IF
 266  CONTINUE
      TEST = ABS(WM-WTR)
      IF(TEST .LE. (0.01*WTR)) GO TO 269
      WRITE(IW,3002) UNIT
      WRITE(DiagFile,3002) UNIT
      WRITE(IW,3005) WM,WTR
      WRITE(DiagFile,3005)WM,WTR
      response = COMMITQQ(DiagFile)
      ExitValue = 36
 3005 FORMAT(' WATER LEAVING=',G10.4,' WATER ENTERING=',G10.4)
 3001 FORMAT(2X,'********THE SOLIDS MASS BALANCE ON UNIT NUMBER',I3,' IS
     * WRONG')
 3002 FORMAT(2X,'********THE WATER MASS BALANCE ON UNIT NUMBER',I3,' IS
     * WRONG')
 3003 FORMAT(2X,'********THE SOLIDS MASS BALANCE ON UNIT',I3,' OUTPUT ST
     *REAM',I3,' IS WRONG'/' EXPECTED SOLID MASS FLOW ',G10.4,/
     *' MASS FLOW FROM THE MODEL ',G10.4)
C
  269 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE OPSECT(STAT,OUTP,WTRF,NUMM,PLINP,PLOUT,INDFT,FTVEC,
     $STRVEC,INDSV,NGCM,NAMES,GRDM,FMM,FM,INDVAL,TEAR,INDT,STRM,WTRFUN,
     $NMIN,STRVAL,NGCZ,NMINZ,DiagFile,ExitValue)
C     **************************************************************
C
      USE MSFLIB
      USE GLOBALS

      REAL WTRF(NUMM,2),FTVEC(1),FMM(1),FM(1),STRVAL(1),GRDM(NGCZ,NMINZ)
      INTEGER STAT,OUTP(1),WTRFUN(1),PLINP(1),PLOUT(1),INDFT(1)
      INTEGER INDVAL(1),TEAR(1),INDT(1),STRVEC(1),INDSV(1),STRM(1)
      INTEGER POSS,POSNN,POS1,POS2,OUTYP,STR
      CHARACTER*4 NAMES(1)
      INTEGER*4 DiagFile
      INTEGER*4 ios
      LOGICAL*4 response
      INTEGER UNIT,FLAG
      COMMON NPLT,UNIT,ITER,IW,FLAG
      COMMON /OPSEC/RMS,TOLIM,NUM,NPINP,NPOUT,NVAR,NDC,NGC,NSC,IO,ITWO,
     $NTT,NODE,ITW
      CHARACTER*12 FILENAME


C Prevent compiler warning about unused dumy argument
      I = NGCM

      LENG = LEN_TRIM(UnitJobPath)
C
C **OUTPUT SECTION
C ****************
      IONE=1
      ITWO=2
 700  OUTYP=OUTP(STAT)      !OUTP holds the 10 component vector of output codes
      IF(STAT.LT.7.AND.OUTYP.EQ.0) GO TO 770  !Not at convergence and nothing to be printed
      GO TO (720,750,760,710,750,760,710),STAT  !STAT=7 means at convergence
C                                               !STAT=5 means at each iteration
C **RMS AND ITER AND WATER FEEDS
C
 710  IF(OUTYP.EQ.0) GO TO 720    !Nothing to be printed
      IF(STAT.EQ.7) GO TO 715     !At convergence
      GO TO (712,711,712,770,770,770),OUTYP    !OUTYP=6 means solid & water rates, mineral grades and mass in each category.
 711  IF(RMS.GT.TOLIM) GO TO 770
 712  IF(ITER.EQ.0) WRITE(IW,2000) RMS
      IF(ITER.EQ.0) WRITE(DiagFile,2000)RMS
      IF(ITER.NE.0) WRITE(DiagFile,2001)ITER,RMS
      response = COMMITQQ(DiagFile)
      IF(ITER.NE.0) WRITE(IW,2001) ITER,RMS

      IF(OUTYP.NE.3) GO TO 714
 715  DO 713 I=1,NUM
        IF(WTRF(I,1).EQ.0.AND.WTRF(I,2).EQ.0) GO TO 713
        WRITE(IW,2002) I,WTRF(I,1)
        WRITE(DiagFile,2002) I,WTRF(I,1)
 2002   FORMAT(10X,'THE WATER FEED TO UNIT',I3,' IS ',G10.4)
        WRITE(IO,8100) IONE,IONE,WTRFUN(I),WTRF(I,1)
        response = COMMITQQ(DiagFile)
 713  CONTINUE
 714  CONTINUE
      IF(STAT.NE.7) GO TO 770          !Not at convergence.
C
C **PLANT FEEDS
C
 720  IF(STAT.EQ.7) OUTYP=OUTP(8)      !Pick up outcode for plant feeds.
      IF(OUTYP.EQ.0) GO TO 730         !Print nothing
      DO 722 J=1,NMIN
        FMM(J)=0.0
 722  CONTINUE
      DO 721 I=1,NPINP
        STR=PLINP(I)
        POSS=INDFT(STR)
        POS1=POSS+1
        POS2=POSS+2
        POSNN=POSS+NVAR-1
        WRITE(IW,2003) STR,(FTVEC(J),J=POSS,POS1)
        IF(OUTYP.EQ.2.OR.OUTYP.EQ.4) GO TO 721
        WRITE(IO,8100) IONE,ITWO,STR,(FTVEC(J),J=POSS,POS1)
 8100 FORMAT(2I2,I4,/2G10.4)
        CALL STOUT(FTVEC,POS2,NDC,NGC,NSC,NMIN,NAMES,FM,GRDM,OUTYP,IW
     *  ,IO,ITW,NGCZ,NMINZ,DiagFile,STAT)
        WRITE(IW,2015)
        IF(OUTYP.LT.5) GO TO 721
        DO 723 J=1,NMIN
          FMM(J)=FMM(J)+FM(J)
 723    CONTINUE
 721  CONTINUE
      IF(STAT.NE.7) GO TO 770
C
C **PLANT OUTPUTS
C
 730  IF(STAT.EQ.7) OUTYP=OUTP(9)     !Pick up output code for plant outputs.
      IF(OUTYP.EQ.0) GO TO 740        !Print nothing.
      DO 731 I=1,NPOUT
        STR=PLOUT(I)
        POSS=INDVAL(STR)
        POS1=POSS+1
        POS2=POSS+2
        POSNN=POSS+NVAR-1
        WRITE(IW,2005) STR,(STRVAL(J),J=POSS,POS1)
        IF(OUTYP.EQ.2) GO TO 731
        WRITE(IO,8100) IONE,ITWO,STR,(STRVAL(J),J=POSS,POS1)
        CALL STOUT(STRVAL,POS2,NDC,NGC,NSC,NMIN,NAMES,FM,GRDM,OUTYP,I
     *  W,IO,ITW,NGCZ,NMINZ,DiagFile,STAT)
        IF(OUTYP.NE.5.AND.OUTYP.NE.6) GO TO 731
        DO 732 J=1,NMIN
          IF(FMM(J).LT.0.00001) GO TO 732
          FM(J)=FM(J)/FMM(J)*100.0
 732    CONTINUE
        WRITE(IW,2014) (FM(J),J=1,NMIN)
 2014   FORMAT(2X,'% RECOVERY:',12F8.2)
 731  CONTINUE
      WRITE(IW,2015)
      IF(STAT.NE.7) GO TO 770                 !Not at convergence.
C
C **PLANT TEARS
C
 740  IF(STAT.EQ.7) OUTYP=OUTP(10)     !Pick up output code for tears.
      IF(OUTYP.EQ.0) GO TO 770         !Print nothing.
      IF(NTT.EQ.0) GO TO 770           !No tear streams in this plant.
      DO 741 I=1,NTT
        STR=TEAR(I)
        POSS=INDFT(STR)
        POS1=POSS+1
        POS2=POSS+2
        POSNN=POSS+NVAR-1
        WRITE(IW,2007) STR,(FTVEC(J),J=POSS,POS1)
        IF(OUTYP.EQ.2) GO TO 741
        WRITE(IO,8100) IONE,ITWO,STR,(FTVEC(J),J=POSS,POS1)
        CALL STOUT(FTVEC,POS2,NDC,NGC,NSC,NMIN,NAMES,FM,GRDM,OUTYP,IW
     *  ,IO,ITW,NGCZ,NMINZ,DiagFile,STAT)
        WRITE(IW,2015)
 741  CONTINUE
      GO TO 770
C
C **NODE TEARS
C
  750 CONTINUE
      GO TO (7520,7520,751,751,751,751),OUTYP
 751  IF(RMS.GT.TOLIM) GO TO 770

      !Write all the plant tears to file TEARS.OUT every iteration
 7520 IF(STAT.EQ.5) OUTYP = 1    !Pick up output code for tears.
      IF(OUTYP.EQ.0) GO TO 770         !Print nothing.
      IF(NTT.EQ.0) GO TO 770           !No tear streams in this plant.
      FILENAME = 'TEARS.OUT'
      OPEN(ITW,FILE = UnitJobPath(1:LENG)//'TEARS.OUT', ERR=9999,
     *IOSTAT=ios)
      DO I=1,NTT
        STR=TEAR(I)
        POSS=INDFT(STR)
        POS1=POSS+1
        POS2=POSS+2
        POSNN=POSS+NVAR-1
        WRITE(IW,2007) STR,(FTVEC(J),J=POSS,POS1)
 8753   FORMAT('STRM',3I4)
        WRITE(ITW,8753)STR,IONE
        PERCS = 100.0
        IF(FTVEC(POSS)+FTVEC(POS1) .GT. 0.0) THEN
          PERCS=100.0*FTVEC(POSS)/(FTVEC(POSS)+FTVEC(POS1))
        END IF
        WRITE(ITW,8754) FTVEC(POSS),PERCS
 8754   FORMAT(2G10.4)
        IF(OUTYP .EQ. 1) Then
          CALL STOUT(FTVEC,POS2,NDC,NGC,NSC,NMIN,NAMES,FM,GRDM,OUTYP,IW
     *    ,IO,ITW,NGCZ,NMINZ,DiagFile,STAT)
        END IF
        WRITE(IW,2015)
      END DO
      CLOSE(ITW)
      GO TO 770

C **UNIT OUTPUTS
C
 760  CONTINUE
      GO TO (762,762,761,761,761,761),OUTYP
 761  IF(RMS.GT.TOLIM) GO TO 770
 762  STR=STRM(UNIT)
      NN1=1
      IF(STR.NE.1) NN1=INDSV(STR-1)+1
      NN2=INDSV(STR)
      DO 763 I=NN1,NN2
        STR=STRVEC(I)
        POSS=INDVAL(STR)
        POS1=POSS+1
        POS2=POSS+2
        POSNN=POSS+NVAR-1
        WRITE(IW,2011) UNIT,STR,(STRVAL(J),J=POSS,POS1)
        IF(OUTYP.EQ.2.OR.OUTYP.EQ.4) GO TO 763
 8110   FORMAT(2I2,2I4,/2G10.4)
        IF(OUTYP.GT.2) WRITE(IO,8110) ITWO,ITWO,STR,UNIT,(STRVAL(J),
     *  J=POSS,POS1)
        CALL STOUT(STRVAL,POS2,NDC,NGC,NSC,NMIN,NAMES,FM,GRDM,OUTYP,IW,
     *  IO,ITW,NGCZ,NMINZ,DiagFile,STAT)
 763  CONTINUE
C
 2000 FORMAT(20X,'AFTER SETTING UP,THE RMS ERROR IS ',F13.6)
 2001 FORMAT(20X,'AFTER',I3,' ITERATIONS THE RMS ERROR IS ',F13.6)
 2003 FORMAT(/2X,5(1H>),'FEED STREAM',I3,' CONTAINS',G10.4,' SOLIDS AND'
     *,G10.4,' WATER')
 2005 FORMAT(/2X,6(1H<),'OUTPUT STREAM',I3,' CONTAINS',G10.4,' SOLIDS AN
     *D',G10.4,' WATER')
 2007 FORMAT(/2X,5(1H+),'TEAR STREAM',I3,' CONTAINS',G10.4,' SOLIDS AND'
     *,G10.4,' WATER')
 2009 FORMAT(/2X,5(1H+),'TEAR STREAM',I3,' CONTAINS',G10.4,' SOLIDS AND'
     *,G10.4,' WATER')
 2011 FORMAT(/10X,'UNIT',I3,' OUTPUT STREAM',I3/10X,24(1H-)/5X,'CONTAINS
     *: ',G10.4,' SOLIDS AND',G10.4,' WATER')
 2015 FORMAT(1X)
C
C **RETURN TO CALLING STATION
C
 770  CONTINUE
      RETURN

C Error handler for BACKSPACE
 9990 CONTINUE
      ExitValue = 39
      CLOSE(ITW)
      RETURN
C
9999  WRITE(DiagFile,*)
     *'Could not open file '//UnitJobPath(1:LENG)//FILENAME
      WRITE(DiagFile,*)' Run-time error number ',ios
      response = COMMITQQ(DiagFile)
      ExitValue = 31
      RETURN
C
C
      END
