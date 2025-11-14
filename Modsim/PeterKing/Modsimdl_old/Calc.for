C*******************     PROGRAM CALC     *****************************C
C                                                                      C
C    MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS                C
C                                                                      C
C  THIS IS THE CALC PHASE                                              C
C                                                                      C
C  MODSIM  (C) COPYRIGHT RP KING AND MA FORD JOHANNESBURG  1985        C
C                                                                      C
C**********************************************************************C
C    This subroutine is called from PPHO2.FOR
C
      SUBROUTINE CALC(INDPP,INDPAR,CONCT,GRDM,GRDV,WTRF,NAMES,FM,FMM,
     *NCNCZ,NPZ,NUMM,NGCZ,NMINZ,WTRFUN,
     *NDCL,NGCL,NSCL,INDCNC,NVR,INDORD,ORDUNT,INDT,
     *TEAR,INDSV,STRVEC,INDISV,INVSV,PLOUT,PLINP,STRM,INDVAL,
     *INDFT,INDPTH,PTHVEC,INDNP,COEF,UNTYP,INDCF,OUTP,
     *CNCVAL,PPROP,FTVEC,UNTPAR,STRVAL,ALFA,
     *FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,PARAM,
     *F,A,T,SIZE,NAMU,JobPath,ExitValue,DiagFile)

      USE MSFLIB
      USE GLOBALS
C
      CHARACTER*255 JobPath
      INTEGER*4 ExitValue,DiagFile
      DIMENSION FM(1),FMM(1)
      INTEGER NDCL(1),NGCL(1),NSCL(1),CONCT(NCNCZ,4),INDCNC(1),
     *INDPP(NPZ,2),INDORD(1),ORDUNT(1),INDT(1),NVR(1)
      INTEGER TEAR(1),INDSV(1),STRVEC(1),INDISV(1),INVSV(1),PLOUT(1),
     *PLINP(1),STRM(1),INDVAL(1)
      INTEGER INDFT(1),INDPTH(1),PTHVEC(1),INDNP(1),COEF(1),UNTYP(1),
     *INDPAR(NUMM,2),INDCF(1),OUTP(10)
      REAL CNCVAL(1),PPROP(1),FTVEC(1),WTRF(NUMM,2),UNTPAR(1),STRVAL(1),
     *ALFA(1)
      REAL FEED(1),OUT1(1),OUT2(1),OUT3(1),DER1(1),DER2(1),DER3(1),
     *PARAM(1)
      REAL F(1),A(1),T(1),SIZE(1),GRDM(NGCZ,NMINZ),GRDV(NGCZ,NMINZ)
      INTEGER WTRFUN(1)
C
      INTEGER STRN,POS,POSI,PL,STR,POSF,POSC,CONVM,PS,UNTY,UNIT,POSN,
     *FL,PSS,PFS,PF,COEFT,PSC,PSA,PSCI,PSAI,PSK,PCF
      INTEGER STAT,FLAG,FLS,FLGM,FLGV
      INTEGER DIMPP,DIMFT,DIMCNC

c Set up common areas to transmit data to the unit models.
      INTEGER NUNIT
      COMMON NPLT,UNIT,ITER,IW,FLAG
	COMMON /MODELDAT/ NUNIT
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
      LOGICAL SWITCH
      COMMON /AMDSwitch/SWITCH

C Set up common area to transmit data to the report files
      LOGICAL ReportFlag
      COMMON/REPORTING/ReportFlag

C Set up common block to transmit data to output section
      COMMON /OPSEC/RMS,TOLIM,NUM,NPINP,NPOUT,NVAR,NDC,NGC,NSC,IO,ITWO,
     $NTT,NODE,ITW

      CHARACTER*1 IDEN(80)
      CHARACTER*4 NAMES(1),NAMU(1),ModelCode
      CHARACTER*12 FILENAME
      LOGICAL*4 response

C Turn report flag off
      ReportFlag = .FALSE.

C Turn AMD switch off
      SWITCH = .FALSE.
C
C **PROGRAM TO CALCULATE MASS BALANCE
C ***********************************
C
 1000 FORMAT(20I4)
 1001 FORMAT(8G10.4)
 1002 FORMAT(A4)
 1003 FORMAT(80A1)
C
C **READ SYSTEM DATA
C ******************
C
      WRITE(DiagFile,*)'Starting the calculation phase'
      LENG = LEN_TRIM(JobPath)
      WRITE(DiagFile,*)'Job path is '//JobPath(1:LENG)
      response = COMMITQQ(DiagFile)
      ExitValue = -1
      UnitJobPath = JobPath
      UnitExitValue = ExitValue
      UnitDiagFile = DiagFile
      IRI=4
      IRP=5
      IW=7
      IO=1
      ITW=8
      ITWO = 2
C
      FILENAME = 'PHI4.DAT'
      INQUIRE(FILE=JobPath(1:LENG)//FILENAME,OPENED=response)
      IF(response) Then
        WRITE(DiagFile,*) JobPath(1:LENG)//FILENAME//' was found open in
     * CALC'
      END IF
      OPEN(IRI,FILE = JobPath(1:LENG)//'PHI4.DAT',ERR = 9999,IOSTAT=ios)
      FILENAME = 'PHO3.DAT'
      INQUIRE(FILE=JobPath(1:LENG)//FILENAME,OPENED=response)
      IF(response) Then
        WRITE(DiagFile,*) JobPath(1:LENG)//FILENAME//' was found open in
     * CALC'
      END IF
      OPEN(IRP,FILE = JobPath(1:LENG)//FILENAME,ERR=9999,IOSTAT=ios)
      FILENAME = 'PHC.OUT'
      OPEN(IW,FILE = JobPath(1:LENG)//'PHC.OUT',ERR = 9999,IOSTAT=ios)
      FILENAME = 'OPDISP.DAT'
      OPEN(IO,FILE = JobPath(1:LENG)//'OPDISP.DAT',ERR=9999,IOSTAT=ios)


      FILENAME = 'REPORT.DAT'
      OPEN(31,FILE = JobPath(1:LENG)//'REPORT.DAT',ERR=9999,IOSTAT=ios)
      DIMPP=1
      DIMCNC=1
      NPLANT=1
      DIMFT=1
C
      FILENAME = 'PHI4.DAT'
	!Initialize INDPP
	Do I = 1,NPZ
	  INDPP(I,1) = 0
	  INDPP(I,2) = 0
	End do
      READ(IRI,1003,ERR = 9980,END=9981)(IDEN(I),I=1,80)
      WRITE(IW,1003)(IDEN(I),I=1,80)
      WRITE(IO,1003)(IDEN(I),I=1,80)
      READ(IRI,1000,ERR = 9980,END=9981) NPLA,NCNCT,NPP,NMIN,
     *NDCM,NGCM,FLS,FLGM,FLGV,DIMCNC,IRD
 8010 FORMAT(19I4)
      WRITE(IO,8010) NPLA, NMIN,NDCM,NGCM
      IF(IRD.EQ.1) CALL READER
      READ(IRI,1002,ERR = 9980,END=9981) (NAMES(I),I=1,NMIN)
 8012 FORMAT(A4)
      WRITE(IO,8012) (NAMES(I),I=1,NMIN)
      IF(FLS.EQ.0) GO TO 2
C
      READ(IRI,1001,ERR = 9980,END=9981) (SIZE(I),I=1,NDCM)
      CALL MESH_SIZE(SIZE,NDCM)
 2    IF(FLGM.EQ.0) GO TO 4
      DO 3 I=1,NGCM
        READ(IRI,1001,ERR = 9980,END=9981) (GRDM(I,J),J=1,NMIN)
 8020   FORMAT(7G10.4)
        WRITE(IO,8020)(GRDM(I,J),J=1,NMIN)
 3    CONTINUE
 4    IF(FLGV.EQ.0) GO TO 6
      DO 5 I=1,NGCM
        READ(IRI,1001,ERR = 9980,END=9981) (GRDV(I,J),J=1,NMIN)
 5    CONTINUE
 6    IF(NCNCT.EQ.0) GO TO 8
      DO 7 I=1,NCNCT
        READ(IRI,1000,ERR = 9980,END=9981) (CONCT(I,J),J=1,4)
 7    CONTINUE
      READ(IRI,1000,ERR = 9980,END=9981) (INDCNC(I),I=1,NCNCT)
      DO 12 I=1,NCNCT
        IF(INDCNC(I).GT.DIMCNC) DIMCNC=INDCNC(I)
        IF(CONCT(I,1).GT.NPLANT) NPLANT=CONCT(I,1)
        IF(CONCT(I,3).GT.NPLANT) NPLANT=CONCT(I,3)
 12   CONTINUE
 8    CONTINUE
      IF(NPP.EQ.0) GO TO 11
      READ(IRI,'(I5)',ERR = 9980,END=9981) DIMPP
      DO 9 I=1,NPP
        READ(IRI,1000,ERR = 9980,END=9981) (INDPP(I,J),J=1,2)
        PS=INDPP(I,2)+PS
 9    CONTINUE
      READ(IRI,1001,ERR = 9980,END=9981) (PPROP(I),I=1,DIMPP)
      WRITE(IO,'(I5)')DIMPP
      WRITE(IO,'(8G10.4)')(PPROP(I),I = 1,DIMPP)
      DIMPP=PS
C
C  READ PLANT DATA
C ****************
C
 11   NPT=0
 10   NPT=NPT+1
      IF(NPT.GT.NPLA) GO TO 900
      FILENAME = 'PHO3.DAT'
      READ(IRP,1000,ERR = 9980,END=9981,IOSTAT=ios)
     * NPLT,NNOD,NTT,NU,NSM,NUM,NOPTM,NPINP,NPOUT
      If(NPINP .GT. MaxInputStreams) Then
        Write(Diagfile,'(''Too many input streams in the flowsheet'')')
        Write(Diagfile,'(''  Maximum number allowed '',I3)')
     *   MaxInputStreams
        ExitValue = 18
        RETURN
      End If
      If(NPOUT .GT. MaxOutputStreams) Then
        Write(Diagfile,'(''Too many output streams in the flowsheet'')')
        Write(Diagfile,'(''  Maximum number allowed '',I3)')
     *   MaxOutputStreams
        ExitValue = 17
        RETURN
      End If
      WRITE(IO,8010) NPLT,NNOD,NTT,NU,NSM,NPINP,NPOUT
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (INDORD(I),I=1,NNOD)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (ORDUNT(I),I=1,NU)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (INDT(I),I=1,NNOD)
      IF(NTT.GT.0) READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios)
     * (TEAR(I),I=1,NTT)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (INDSV(I),I=1,NSM)
      NI=INDSV(NSM)
      IF(NI.GT.0) READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios)
     * (STRVEC(I),I=1,NI)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (INDISV(I),I=1,NSM)
      NI=INDISV(NSM)
      IF(NI.GT.0) READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios)
     * (INVSV(I),I=1,NI)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (PLINP(I),I=1,NPINP)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (PLOUT(I),I=1,NPOUT)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (STRM(I),I=1,NUM)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (INDPTH(I),I=1,NSM)
      NI=INDPTH(NSM)
      WRITE(IO,8010)(PLINP(I),I=1,NPINP)
      WRITE(IO,8010)(PLOUT(I),I=1,NPOUT)
      IF(NI.GT.0) READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios)
     * (PTHVEC(I),I=1,NI)
      READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios) (INDNP(I),I=1,NNOD)
      NPTH=INDNP(NNOD)
      IF(NPTH.GT.0) READ(IRP,1000,ERR=9980,END=9981,IOSTAT=ios)
     * (COEF(I),I=1,NPTH)
      READ(IRP,'(16I5)',ERR=9980,END=9981,IOSTAT=ios)(INDVAL(I),I=1,NSM)
      READ(IRP,'(16I5)',ERR=9980,END=9981,IOSTAT=ios)(INDFT(I),I=1,NSM)
      FILENAME = 'PHI4.DAT'
      READ(IRI,1000,ERR=9980,END=9981,IOSTAT=ios)
     *  NDCL(NPLT),NGCL(NPLT),NSCL(NPLT),NVR(NPLT)
      NVAR=NVR(NPLT)
      NDC=NDCL(NPLT)
      NGC=NGCL(NPLT)
      NSC=NSCL(NPLT)
      WRITE(IO,8010)NDC,NGC,NSC
      WRITE(IO,8030) (SIZE(I),I=1,NDC)
 8030 FORMAT(6G12.4)
C
      WRITE(IW,2020) NPLT
 2020 FORMAT('  PLANT NUMBER',I5)
      JFLAG=0
      IF(NTT.EQ.0) GO TO 28
      NI=INDFT(TEAR(NTT))+NVAR-1
      DIMFT=NI
      DO 20 I=1,NI
        FTVEC(I)=1
        IF(MOD(I-1,NVAR).EQ.0) FTVEC(I)=NVAR-2
        IF(MOD(I-2,NVAR).EQ.0) FTVEC(I)=NVAR-2
 20   CONTINUE
 28   READ(IRI,1000,ERR=9980,END=9981,IOSTAT=ios) STRN
      IF(STRN.EQ.0) GO TO 30
      POS=INDFT(STRN)
      ICN=(NVAR-1)/5+1
      IF(POS.EQ.0) GO TO 29
      POSI=POS+NVAR-1
      READ(IRI,'(5E14.6)',ERR=9980,END=9981,IOSTAT=ios)
     * (FTVEC(I),I=POS,POSI)
      GO TO 28
 29   READ(IRI,1001,ERR=9980,END=9981,IOSTAT=ios)DUMMY
      ICN=ICN-1
      IF(ICN.NE.0) GO TO 29
      GO TO 28
 30   CONTINUE
      DO 40 I=1,NUM
        READ(IRI,8001,ERR=9980,END=9981,IOSTAT=ios)
     *   (WTRF(I,J),J=1,2),WTRFUN(I)
 8001   FORMAT(2G10.4,I4)
 40   CONTINUE
C
      IF(NCNCT.EQ.0) GO TO 50
      DO 45 I = 1,NCNCT
        PL = CONCT(I,3)
        IF(PL .NE. NPLT) GO TO 45
        STR = CONCT(I,4)
        POSF = INDFT(STR)
        IF(POSF .EQ. 0) GO TO 45
        POSC = INDCNC(I)
        MPLT = CONCT(I,1)
        CALL CONVRT(FTVEC,POSF,NDCL,NGCL,NSCL,CNCVAL,POSC,NPLT,MPLT,
     *  DIMFT,DIMCNC,NPLANT)
 45   CONTINUE
C
C
C **READ RUN DATA
C ***************
C
      GO TO 50
 899  WRITE(IW,2013) NPLT,NODE,RMS
      WRITE(DiagFile,2013) NPLT,NODE,RMS
 2013 FORMAT(2X,'******** ALLOWABLE NUMBER OF ITERATIONS EXEEDED FOR PLA
     *NT ',I3,' AT NODE ',I3,/'  ********WHEN RMS=',F8.4)
      response = COMMITQQ(DiagFile)
      ExitValue = 37
C
      CLOSE(IRI)
      CLOSE(IRP)
      CLOSE(IW)
      CLOSE(IO)

      WRITE(DiagFile,*)'Job path is '//JobPath(1:LENG)
      response = COMMITQQ(DiagFile)
      FILENAME = 'TEARS.OUT'
      OPEN(ITW,FILE = JobPath(1:LENG)//'TEARS.OUT',ACCESS = 'APPEND',
     *ERR=9999,IOSTAT=ios)
      WRITE(ITW,'(''LAST 0 0 0 1'')')
      CLOSE(ITW)
      CLOSE(31)
      RETURN
C
C
 50   READ(IRI,1000,ERR=9980,END=9981,IOSTAT=ios) CONVM,NITER,ITOL
      IF(CONVM.EQ.0) GO TO 600
      JFLAG=JFLAG+1
      WRITE(IW,2023) JFLAG
 2023 FORMAT('  RUN NUMBER',I5)
      TOLIM=10.0**ITOL
      READ(IRI,2024,ERR=9980,END=9981,IOSTAT=ios)
     * (UNTYP(I),NAMU(UNTYP(I)),I=1,NUM)
 2024 FORMAT(I4,2X,A4)
      PS=0
      DO 60 I=1,NUM
		READ(IRI,1000,ERR=9980,END=9981,IOSTAT=ios) (INDPAR(I,J),J=1,2)
		PS=PS+INDPAR(I,2)
 60   CONTINUE
      READ(IRI,2025,ERR=9980,END=9981,IOSTAT=ios) (UNTPAR(I),I=1,PS)
 2025 FORMAT(5E16.8)
      READ(IRI,1000,ERR=9980,END=9981,IOSTAT=ios) (OUTP(I),I=1,10)
C
C Write plant data to the REPORT.DAT file
c ***************************************
      WRITE(31,7001) NU
      WRITE(31,7001) NDC, NGC, NSC
      DO 55 I = 1,NDC
        WRITE(31,7002) SIZE(I)
   55 CONTINUE
      WRITE(31,7001) NPP
      DO 54 I = 1,NPP
        WRITE(31,7001) INDPP(I,1),INDPP(I,2)
        DO 53 J = INDPP(I,1),INDPP(I,1)+INDPP(I,2)-1
          WRITE(31,7002) PPROP(J)
   53   CONTINUE
   54 CONTINUE
      WRITE(31,7001) NMIN,NGCM
      WRITE(31,8012) (NAMES(I),I=1,NMIN)
      DO 52 J = 1,NGCM
        WRITE(31,7002) (GRDM(J,K),K = 1,NMIN)
   52 CONTINUE
      DO 51 J= 1,NGCM
        WRITE(31,7002) (GRDV(J,K),K = 1,NMIN)
   51 CONTINUE

C   START CALCULATION
C   *****************
C
C **PUT FEEDS INTO STRVAL
C
      DO 91 I=1,NPINP
        STR=PLINP(I)
        POS=INDVAL(STR)
        POSI=POS+NVAR-1
        POSF=INDFT(STR)-1
        DO 90 J=POS,POSI
          POSF=POSF+1
          STRVAL(J)=FTVEC(POSF)
 90     CONTINUE
 91   CONTINUE
C
C **CALL OUTPUT 1
C
      STAT=1
      CALL OPSECT(STAT,OUTP,WTRF,NUMM,PLINP,PLOUT,INDFT,FTVEC,STRVEC,
     $INDSV,NGCM,NAMES,GRDM,FMM,FM,INDVAL,TEAR,INDT,STRM,WTRFUN,NMIN,
     $STRVAL,NGCZ,NMINZ,DiagFile,ExitValue)
C
C **TAKE NEXT NODE IF ANY
C
      NODE=0
      FLAG=0
 100  NODE=NODE+1
      IF(NODE.GT.NNOD) GO TO 612
      WRITE(IW,2021) NODE
 2021 FORMAT('  NODE NUMBER',I5)
      WRITE(DiagFile,'(/''Node number '',I3)')NODE
      response = COMMITQQ(DiagFile)
      ITER=0
      FLAG=0
      RMS=1000000.0
C
C **PUT THIS NODE'S TEARS INTO STRVAL IF ANY
C
      NT1=1
      IF(NODE.NE.1) NT1=INDT(NODE-1)+1
      NT2=INDT(NODE)
      NT=NT2-NT1+1
      IF(NT.EQ.0) RMS=0.0
      IF(NT.EQ.0) GO TO 190
      DO 120 I=NT1,NT2
        STR=TEAR(I)
        POS=INDVAL(STR)
        POSI=POS+NVAR-1
        POSF=INDFT(STR)-1
        DO 110 J=POS,POSI
          POSF=POSF+1
          STRVAL(J)=FTVEC(POSF)
 110    CONTINUE
 120  CONTINUE
C
C **CALL OUTPUT 2
      STAT=2
      CALL OPSECT(STAT,OUTP,WTRF,NUMM,PLINP,PLOUT,INDFT,FTVEC,STRVEC,
     $INDSV,NGCM,NAMES,GRDM,FMM,FM,INDVAL,TEAR,INDT,STRM,WTRFUN,NMIN,
     $STRVAL,NGCZ,NMINZ,DiagFile,ExitValue)
C
C
C **TAKE NEXT UNIT IN NODE IF ANY
C
 190  N1=0
      IF(NODE.NE.1) N1=INDORD(NODE-1)
      N2=INDORD(NODE)
C
C  IF THIS IS A SIMPLE NODE, SET FLAG=1
      IF(N2-N1 .EQ. 1) FLAG=1
      NST=N1
      N11=N1+1
      KFL=0
      DO 191 I=N11,N2
        IF(WTRF(ORDUNT(I),2).EQ.0.0) GO TO 191
        KFL=1
        GO TO 170
 191  CONTINUE

C  Iteration cycles from here
C **INITIALIZE ALFA IF CONVM=3
C
 170  IF(CONVM.EQ.1.OR.CONVM.EQ.2) GO TO 200
      NI=NVAR*NOPTM
      DO 180 I=1,NI
        ALFA(I)=1
 180  CONTINUE
C
 200  CONTINUE
      N1=N1+1                     !***Take next unit in node
      IF(N1.GT.N2) GO TO 300      !***To compare starting and calculated tears
      UNIT=ORDUNT(N1)
      UNTY=UNTYP(UNIT)
      ModelCode=NAMU(UNTY)
      IF(UNTY.NE.1) GO TO 240
C
C **MIXER SUBSECTION
C ******************
C
      WRITE(DiagFile,'('' Unit '',I3,'' is a mixer'')')UNIT
      response = COMMITQQ(DiagFile)
      STR=STRM(UNIT)
      STR=STRVEC(INDSV(STR))
      M1=1
      IF(STR.NE.1) M1=INDISV(STR-1)+1
      M2=INDISV(STR)
      POSN=INDVAL(STR)-1

      DO 220 J=1,NVAR
        OUT1(J)=0.0
        DO 210 I=M1,M2
          IST=INVSV(I)
          POS=INDVAL(IST)+J-1
          OUT1(J)=OUT1(J)+STRVAL(POS)
 210    CONTINUE
 220  CONTINUE
      DO 211 J=1,NVAR
        STRVAL(POSN+J)=OUT1(J)
 211  CONTINUE
C Get the parameters to transmit to the report file
      M1=INDPAR(UNIT,1)
      M2=INDPAR(UNIT,1)+INDPAR(UNIT,2)-1
      IF(M1 .GE. 1) THEN
        J=0
        DO  I = M1,M2
          J=J+1
          PARAM(J)=UNTPAR(I)
        END DO
      END IF
      NPAR=M2-M1+1

C Write data to the report data file if convergence has been reached
      IF(FLAG .EQ. 1) THEN
        WRITE(31,7011) UNIT, UNTY, ModelCode
        WRITE(31,7001) NPAR
        WRITE(31,7002) (PARAM(I), I = 1,NPAR)
      END IF

C
      POSN=POSN+1
      IF(WTRF(UNIT,2) .EQ. 0) THEN
        STRVAL(POSN+1)=STRVAL(POSN+1)+WTRF(UNIT,1)
      ELSE
        WTR=STRVAL(POSN+1)
        STRVAL(POSN+1)=STRVAL(POSN)*(100.0/WTRF(UNIT,2)-1.0)
        WTRF(UNIT,1)=STRVAL(POSN+1)-WTR
      END IF
      IF(FLAG .EQ. 1) THEN
        WS = 0.0
        WRITE(31,7002) WS,STRVAL(POSN),WS,WS
        WRITE(31,7002) WS,STRVAL(POSN+1),WS,WS
      END IF
      GO TO 272
C **END MIXER SUBSECTION
C
C **FIND UNIT INPUTS & PARAMETERS
C
 240  POS=INDVAL(STRM(UNIT))-1
      TMSF=STRVAL(POS+1)
      WTR=STRVAL(POS+2)
      IF(ITER .EQ. 0) THEN
      END IF
      DO 250 I=3,NVAR
        FEED(I-2)=STRVAL(POS+I)
 250  CONTINUE
      M1=INDPAR(UNIT,1)
      M2=INDPAR(UNIT,1)+INDPAR(UNIT,2)-1
      IF(M1.LT.1) GO TO 257
      J=0
      DO 255 I=M1,M2
        J=J+1
        PARAM(J)=UNTPAR(I)
 255  CONTINUE
 257  CONTINUE
      NPAR=M2-M1+1
C
C **CALL UNIT
C
      WRITE(DiagFile,'('' CALLING MODEL FOR UNIT '',I3)')UNIT
	WRITE(diagfile,'(''   Feedrate to this unit '',G12.4,
     *'' kg/s'')')TMSF
c      write(DiagFile,'('' Model code is '',A4)')ModelCode
      response = COMMITQQ(DiagFile)
C Put unit number into COMMON /MODELDAT/ to send it through PPHI5.FOR to the user models.
	NUNIT = UNIT


c Transmit information to alert the model if the unit has a water
c feed stream and how much water is added.
      UNITWFEED = WTRFUN(UNIT) .GT. 1
      WATERADD = WTRF(UNIT,1)
      SOLIDCONT = WTRF(UNIT,2)
c      WRITE(DiagFile,'(''UNITWFEED,WATERADD,SOLIDCONT '',L1,2G10.4)')
c     *UNITWFEED,WATERADD,SOLIDCONT

C Initialize all model outputs
      FL=0
	TMS1 = 0.0; TMS2 = 0.0; TMS3 = 0.0
	OUT1 = 0.0; OUT2 = 0.0; OUT3 = 0.0
	DER1 = 0.0; DER2 = 0.0; DER3 = 0.0
	WTR1 = 0.0; WTR2 = 0.0; WTR3 = 0.0

      CALL U(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,NDC,
     *NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPZ,GRDM,GRDV
     *,NMIN,NGCZ,UNTY,NPAR,ModelCode)

      WRITE(DiagFile,'('' RETURNED FROM UNIT MODEL'',I3)')UNIT
      response = COMMITQQ(DiagFile)
      IF(UnitExitValue .GT. ExitValue) ExitValue = UnitExitValue

C Write data to the report data file if convergence has been reached
c ******************************************************************
      IF(FLAG .EQ. 1) THEN
        WRITE(31,7011) UNIT, UNTY, ModelCode
 7011   FORMAT(2I8,2X,A4)
        WRITE(31,7001) NPAR
        WRITE(31,7002) (PARAM(I), I = 1,NPAR)
 7001   FORMAT(3I8)
        WRITE(31,7002) TMSF, TMS1, TMS2, TMS3
        WRITE(31,7002) WTR, WTR1, WTR2, WTR3
 7002   FORMAT(4G20.8)
        DO N = 1,NVAR-2
          WRITE(31,7002) FEED(N),OUT1(N),OUT2(N),OUT3(N)
        END DO
        WRITE(31,7003) UNITWFEED, WATERADD, SOLIDCONT
 7003 FORMAT(L1,2G20.8)
      END IF

C
C Adjust the feed water to allow for any water addition to the unit.
      IF(UNITWFEED) THEN
C        WTR = WTR1 + WTR2 + WTR3 'This change made 8/22/04
        WTR = WTR + WATERADD
      END IF
C Record the value of the water that is added to the unit.
C This is necessary in case the percent solids was originally specified.
      WTRF(UNIT,1) = WATERADD

C Issue warnings at simulation time
      IF(FL.EQ.3) GO TO 898
      IF (FL.EQ.4) WRITE(IW,256) UNIT
  256 FORMAT(' UNIT',I4,'  IS OVERLOADED')
C
C PUT OUTPUTS INTO STRVAL
C ***********************
 260  STR=STRM(UNIT)
      M1=0
      IF(STR.NE.1) M1=INDSV(STR-1)
      M2=INDSV(STR)
      NI=M2-M1
C Check if unit has a pseudo output stream  eg a stream to carry information
C about the load in an autogenous mill. This does not enter the conservation
C calculation.
      NII = NI
      IF(FL .GE. 10 .AND. FL .LT. 20) THEN
        IF(NI .GT. 1) NII = NII - 1
        FL = FL - 10
      END IF
      IF(FL .GE. 20 .AND. FL .LT. 30) THEN
        IF(NI .GT. 2) NII = NII - 1
        FL = FL - 20
      END IF
C
C
C **CHECK THAT UNIT IS CONSERVATIVE
      CALL CONSRV(TMSF,TMS1,TMS2,TMS3,OUT1,OUT2,OUT3,WTR,WTR1,
     $WTR2,WTR3,NVAR,STRVEC,NII,M1,DiagFile,ExitValue)
C Only record the check at convergence
      IF(FLAG .NE. 1 .AND. ExitValue .EQ. 36) THEN
        ExitValue = -1
      END IF
C
C
      DO 271 I=1,NI
        PS=M1+I
        POSN=INDVAL(STRVEC(PS))
        IF(I.EQ.1) STRVAL(POSN)=TMS1
        IF(I.EQ.2) STRVAL(POSN)=TMS2
        IF(I.EQ.3) STRVAL(POSN)=TMS3
        IF(I.EQ.1) STRVAL(POSN+1)=WTR1
        IF(I.EQ.2) STRVAL(POSN+1)=WTR2
        IF(I.EQ.3) STRVAL(POSN+1)=WTR3
        DO 270 J=3,NVAR
          POS=POSN+J-1
          IF(I.EQ.1) STRVAL(POS)=OUT1(J-2)
          IF(I.EQ.2) STRVAL(POS)=OUT2(J-2)
          IF(I.EQ.3) STRVAL(POS)=OUT3(J-2)
 270    CONTINUE
 271  CONTINUE
      IF(ITER.EQ.0) THEN
      END IF
C
C **CALL OUTPUT 3 OR 6
C
 272  STAT=3
      IF(NT.EQ.0) STAT=6
      CALL OPSECT(STAT,OUTP,WTRF,NUMM,PLINP,PLOUT,INDFT,FTVEC,STRVEC,
     $INDSV,NGCM,NAMES,GRDM,FMM,FM,INDVAL,TEAR,INDT,STRM,WTRFUN,NMIN,
     $STRVAL,NGCZ,NMINZ,DiagFile,ExitValue)
      IF(STAT.EQ.6) GO TO 100
C
C **CHECK IF MIXER UNIT
C
      IF(UNTY.EQ.1) GO TO 200
C
C **IF CONVM=3 UPDATE ALFA
C
      IF(CONVM.NE.3) GO TO 200
      M1=M1+1
      DO 282 I=M1,M2
      II=I-M1+1
      STR=STRVEC(I)
      L1=1
      IF(STR.NE.1) L1=INDPTH(STR-1)+1
      L2=INDPTH(STR)
      IF((L1-L2).EQ.1) GO TO 282
      DO 281 J=L1,L2
      NPI=0
      IF(NODE.NE.1) NPI=INDNP(NODE-1)
      POS=(PTHVEC(J)-NPI-1)*NVAR
      IF(ABS(WTR).LT.0.00001) ALFA(POS+2)=0.0
      IF(ABS(WTR).LT.0.00001) GO TO 274
      IF(II.EQ.1) ALFA(POS+2)=ALFA(POS+2)*WTR1/WTR
      IF(II.EQ.2) ALFA(POS+2)=ALFA(POS+2)*WTR2/WTR
      IF(II.EQ.3) ALFA(POS+2)=ALFA(POS+2)*WTR3/WTR
 274  IF(FL.EQ.2) GO TO 281
      DO 280 K=3,NVAR
      IF(FL.EQ.1) GO TO 275
      IF(FEED(K-2).LT.0.0001) ALFA(POS+K)=0.0
      IF(FEED(K-2).LT.0.0001) GO TO 280
      IF(II.EQ.1) ALFA(POS+K)=ALFA(POS+K)*OUT1(K-2)/FEED(K-2)
      IF(II.EQ.2) ALFA(POS+K)=ALFA(POS+K)*OUT2(K-2)/FEED(K-2)
      IF(II.EQ.3) ALFA(POS+K)=ALFA(POS+K)*OUT3(K-2)/FEED(K-2)
      GO TO 280
 275  IF(II.EQ.1) ALFA(POS+K)=ALFA(POS+K)*DER1(K-2)
      IF(II.EQ.2) ALFA(POS+K)=ALFA(POS+K)*DER2(K-2)
      IF(II.EQ.3) ALFA(POS+K)=ALFA(POS+K)*DER3(K-2)
 280  CONTINUE
 281  CONTINUE
 282  CONTINUE
      GO TO 200
C

C **Finished calculating all units in node so
C   COMPARE STARTING & CALCULATED TEARS
C
 300  SUM=0.0
      DO 311 J=NT1,NT2
        STR=TEAR(J)
        PSS=INDVAL(STR)-1
        PFS=INDFT(STR)-1
        DO 310 I=1,NVAR
          PS=PSS+I
          PF=PFS+I
c          write(Diagfile,'(I4,2g12.4)')I,STRVAL(PS),FTVEC(PF)
          IF(ABS(STRVAL(PS)).LT.1.0E-9) THEN
            IF(ABS(FTVEC(PF)).GT.1.0E-8) THEN
              DIFF=(STRVAL(PS)-FTVEC(PF))/FTVEC(PF)
            ELSE
              DIFF=0.0
            END IF
          ELSE
            DIFF=(STRVAL(PS)-FTVEC(PF))/STRVAL(PS)
            IF(ABS(DIFF).LT.1.0E-10) DIFF=0.0
            IF(ABS(DIFF).GT.1.0E10) DIFF=1.0E10
          END IF
c	     write(Diagfile,'('' DIFF = '',g10.4)')DIFF
          DIFF=DIFF*DIFF
          SUM=SUM+DIFF
 310    CONTINUE
 311  CONTINUE
      WS=NVAR*NT
      RMS=SQRT(SUM)/WS
C
C **CALL OUTPUT 4
C     IF(FLAG.EQ.1) GO TO 840
      STAT=4
      CALL OPSECT(STAT,OUTP,WTRF,NUMM,PLINP,PLOUT,INDFT,FTVEC,STRVEC,
     $INDSV,NGCM,NAMES,GRDM,FMM,FM,INDVAL,TEAR,INDT,STRM,WTRFUN,NMIN,
     $STRVAL,NGCZ,NMINZ,DiagFile,ExitValue)
C
  840 CONTINUE
      IF(RMS.GT.TOLIM) GO TO 312
      IF(FLAG.EQ.1) GO TO 100
      FLAG=1
      IF(OUTP(3).GE.3) GO TO 400
      IF(OUTP(5).GE.3) GO TO 400
      GO TO 100
 312  CONTINUE
      GO TO (400,313,319,472),CONVM
C
C **CONVERGENCE METHOD 2   BOUNDED WEGSTEIN
C *****************************************
C
C
C **STORE DIFFERENCES IN ALFA
C
 313  IF(ITER.EQ.0) IFLA=0
      IF(IFLA.EQ.1) GO TO 316
      IFLA=1
      POS=0
      DO 315  J=NT1,NT2
        STR=TEAR(J)
        PSS=INDVAL(STR)-1
        PFS=INDFT(STR)-1
        DO 314 I=1,NVAR
          PS=PSS+I
          PF=PFS+I
          POS=POS+1
          ALFA(POS)=STRVAL(PS)-FTVEC(PF)
 314    CONTINUE
 315  CONTINUE
      GO TO 400
C
C **ACCELERATE CONVERGENCE
C
 316  IFLA=0
      POS=0
      DO 318 J=NT1,NT2
        STR=TEAR(J)
        PSS=INDVAL(STR)-1
        PFS=INDFT(STR)-1
        DO 317 I=2,NVAR
          PS=PSS+I
          PF=PFS+I
          POS=POS+1
          SF=STRVAL(PS)-FTVEC(PF)
          IF(SF*ALFA(POS).LT.0.0) GO TO 317
          IF((ALFA(POS)-SF).EQ.0.0) GO TO 317
          GJ=ALFA(POS)/(ALFA(POS)-SF)
          IF(GJ.GT.11.0) GJ=11.0
          IF(GJ.LT.1.0) GO TO 317
          STRVAL(PS)=GJ*SF+FTVEC(PF)
 317    CONTINUE
 318  CONTINUE
      GO TO 469
C
C
C
C **CONVERGENCE METHOD 3   REDUCED NEWTON
C ***************************************
C
C **CONVERT ALFA(PATH) TO ALFA(COEFFICIENT)
C   BY SUMMING PATHS EFFECTING SAME COEFFICIENTS
C
 319  NTN=NT*NT
      DO 320 I=1,NTN
      INDCF(I)=0
 320  CONTINUE
      M1=1
      IF(NODE.NE.1) M1=INDNP(NODE-1)+1
      M2=INDNP(NODE)
      DO 332 I=M1,M2
      COEFT=COEF(I)
      PSC=INDCF(COEFT)
      PSA=(I-M1)*NVAR
      IF(PSC.EQ.0) GO TO 331
      DO 330 J=2,NVAR
      PSCI=PSC+J-1
      PSAI=PSA+J
      ALFA(PSCI)=ALFA(PSCI)+ALFA(PSAI)
 330  CONTINUE
      GO TO 332
 331  INDCF(COEFT)=PSA+1
 332  CONTINUE
C
C ** DETERMINE EFFECT OF FEEDS ON TEARS
C
      DO 342 I=NT1,NT2
        STR=TEAR(I)
        POS=INDVAL(STR)-1
        DO 341 K=2,NVAR
          PSK=(I-NT1)*NVAR+K
          F(PSK)=STRVAL(POS+K)
          DO 340 J=NT1,NT2
            STR=TEAR(J)
            PCF=(J-NT1)*NT+I-NT1+1
            F(PSK)=F(PSK)-ALFA(INDCF(PCF)+K-1)*FTVEC(INDFT(STR)+K-1)
 340      CONTINUE
 341    CONTINUE
 342  CONTINUE
C
C
C **SET UP MATRIX A(I) FOR EACH VARIABLE IN TURN AND
C   SOLVE TO PREDICT NEW TEAR VALUES
C
C
      DO 490 I=2,NVAR
      IF(I.EQ.2.AND.KFL.EQ.1) GO TO 490
      M=0
      DO 410 J=1,NTN
      IF(INDCF(J).EQ.0) GO TO 405
      PS=INDCF(J)+I-1
      L=0
      MM=1+(NT+1)*M
      IF(J.EQ.MM) M=M+1
      IF(J.EQ.MM) L=1
      A(J)=L-ALFA(PS)
      GO TO 410
 405  A(J)=0.0
 410  CONTINUE
      DO 420 J=1,NT
      A(NTN+J)=F((J-1)*NVAR+I)
 420  CONTINUE
C
C **CONVERT A(I) TO UPPER TRIANGULAR
C
      NTP=NT+1
      NTM=NT-1
      IF(NTM.EQ.0) GO TO 440
      DO 432 M=1,NTM
      MM=(M-1)*NT+M
      M1=M+1
      DO 431 NN=M1,NTP
      NM=(NN-1)*NT+M
      NL1=(NN-1)*NT
      ML1=(M-1)*NT
      DO 430 L=M1,NT
      NL=NL1+L
      ML=ML1+L
      A(NL)=A(NL)*A(MM)-A(ML)*A(NM)
 430  CONTINUE
 431  CONTINUE
 432  CONTINUE
C
C **BACK SUBSTITUTE
C
 440  CONTINUE
      DO 451 MI=1,NT
      M=NT+1-MI
      MM=(M-1)*NT+M
      M1=M+1
      SS=0.0
      DO 450 NN=M1,NTP
      NM=(NN-1)*NT+M
      IF(NN.EQ.NTP) GO TO 449
      SS=SS+A(NM)*T(NN)
      GO TO 450
 449  B=A(NM)-SS
      IF(A(MM) .EQ. 0.0) THEN
        WRITE(DiagFile,*)'  The convergence matrix is singular.'
        WRITE(DiagFile,*)'  This is probably due to inappropriate model
     *parameters.'
        WRITE(DiagFile,*)'  You could try an alternative convergence met
     *hod.'
        WRITE(DiagFile,*)'  Set this up from the CONVERGENCE button on t
     *he system data form.'
        response = COMMITQQ(DiagFile)
        ExitValue = 34
C
        CLOSE(IRI)
        CLOSE(IRP)
        CLOSE(IW)
        CLOSE(IO)

        FILENAME = 'TEARS.OUT'
        OPEN(ITW,FILE = JobPath(1:LENG)//'TEARS.OUT',ACCESS = 'APPEND',
     *  ERR=9999,IOSTAT=ios)
        WRITE(ITW,'(''LAST 0 0 0 2'')')
        CLOSE(ITW)

        CLOSE(31)
        RETURN
      END IF
      T(M)=B/A(MM)
 450  CONTINUE
 451  CONTINUE
C
C **INSERT NEW START TEARS INTO STRVAL
C
      DO 460 J=NT1,NT2
        STR=TEAR(J)
        PS=INDVAL(STR)+I-1
        IF(T(J-NT1+1).LT.0.0) GO TO 460
        STRVAL(PS)=T(J-NT1+1)
 460  CONTINUE
C
C **RETURN FOR NEXT VARIABLE
C
 490  CONTINUE
C
C **CALCULATE TOTAL SOLIDS FOR EACH TEAR
C
 469  DO 471 J=NT1,NT2
        STR=TEAR(J)
        PS=INDVAL(STR)
        SUM=0
        DO 470 K=3,NVAR
          SUM=SUM+STRVAL(PS-1+K)
 470    CONTINUE
        STRVAL(PS)=SUM
 471  CONTINUE
      GO TO 400
C
C **CONVERGENCE 4   REDUCED BROYDEN
C *********************************
c 17 September 1999 Simple average method inserted
C
 472  CONTINUE
      DO I=NT1,NT2
        STR=TEAR(I)
        POS=INDVAL(STR)
        POSI=POS+NVAR-1
        POSF=INDFT(STR)-1
        DO J=POS,POSI
          POSF=POSF+1
          STRVAL(J)=0.1*STRVAL(J) + 0.9*FTVEC(POSF)
        END DO
      END DO
      GO TO 400
C
C
C **INSERT CALCULATED TEARS INTO FTVEC
C ************************************
C
 400  CONTINUE
      DO 402 I=NT1,NT2
        STR=TEAR(I)
        POS=INDVAL(STR)
        POSI=POS+NVAR-1
        POSF=INDFT(STR)-1
        DO 401 J=POS,POSI
          POSF=POSF+1
          FTVEC(POSF)=STRVAL(J)
 401    CONTINUE
 402  CONTINUE
C
C **CALL OUTPUT 5
      STAT=5
      CALL OPSECT(STAT,OUTP,WTRF,NUMM,PLINP,PLOUT,INDFT,FTVEC,STRVEC,
     $INDSV,NGCM,NAMES,GRDM,FMM,FM,INDVAL,TEAR,INDT,STRM,WTRFUN,NMIN,
     $STRVAL,NGCZ,NMINZ,DiagFile,ExitValue)
C
C
C **RESET TO FIRST UNIT OF NODE
C
 500  N1=NST
      IF(FLAG.EQ.0) ITER=ITER+1
      IF(ITER.GT.NITER) GO TO 899
      GO TO 170                      !****Start the next iteration.
C
C **Output for station 7 ie at convergence
 612  STAT=7
      IF(OUTP(7).EQ.0.AND.OUTP(8).EQ.0.AND.OUTP(9).EQ.0.AND.OUTP(10).EQ.
     *0) GO TO 50
      WRITE(IW,2022) NPLT,JFLAG
 2022 FORMAT(///20X,'RESULTS FOR PLANT ',I3,' RUN ',I3/20X,29(1H*)/)
      CALL OPSECT(STAT,OUTP,WTRF,NUMM,PLINP,PLOUT,INDFT,FTVEC,STRVEC,
     $INDSV,NGCM,NAMES,GRDM,FMM,FM,INDVAL,TEAR,INDT,STRM,WTRFUN,NMIN,
     $STRVAL,NGCZ,NMINZ,DiagFile,ExitValue)
      GO TO 50
C
C **PUT RELEVANT OUTPUTS INTO CNCVAL
C **********************************
C
C
 600  IF(NCNCT.EQ.0) GO TO 10
      DO 611 I=1,NCNCT
        PL=CONCT(I,1)
        IF(PL.NE.NPLT) GO TO 611
        STR=CONCT(I,2)
        PS=INDVAL(STR)-1
        PSC=INDCNC(I)-1
        DO 610 J=1,NVAR
          CNCVAL(PSC+J)=STRVAL(PS+J)
 610    CONTINUE
 611  CONTINUE
C
C
C **GO TO NEXT PLANT
C ******************
C
      GO TO 10
C
C
C **END
C
 898  WRITE(IW,3904) NPLT,UNIT
 3904 FORMAT(2X,'******** ERROR IN DATA OR UNIT EXECUTION OF PLANT ',I3,
     *' UNIT ',I3//)
 900  CONTINUE
      NINE=99
      ION=1
      IZE=0
      WRITE(IO,8130)ION,NINE,IZE
 8130 FORMAT(2I2,I4)
C
      CLOSE(IRI)
      CLOSE(IRP)
      CLOSE(IW)
      CLOSE(IO)

      FILENAME = 'TEARS.OUT'
      OPEN(ITW,FILE = JobPath(1:LENG)//'TEARS.OUT',ACCESS = 'APPEND',
     *ERR=9999,IOSTAT=ios)
      WRITE(ITW,'(''LAST 0 0 0 3'')')
      CLOSE(ITW)

      CLOSE(31)
      WRITE(DiagFile,'('' The calculation phase has ended'')')
      response = COMMITQQ(DiagFile)
      RETURN
C
9999  WRITE(DiagFile,*)'Could not open file '//JobPath(1:LENG)//FILENAME
      Write(DiagFile,*)'Jobpath ',JobPath(1:LENG)
      WRITE(DiagFile,*)' Run-time error number ',ios
      response = COMMITQQ(DiagFile)
      ExitValue = 31
      CLOSE(IRI)
      CLOSE(IRP)
      CLOSE(IW)
      CLOSE(IO)
	CLOSE(31)
      RETURN

 9980 CONTINUE
      WRITE(DiagFile,*)'Error reading file '//JobPath(1:LENG)//FILENAME
      WRITE(DiagFile,*)' Run-time error number ',ios
	IF(ios .EQ. 64) then
        WRITE(DiagFile,'(
     *''This could indicate an error in the tear stream data.''/
     *''Try starting the calculation without using the tear stream data
     *''/
     *''To do this, uncheck the box in the Set convergence properties sc
     *reen''/''  This is accessed from the System data screen''//
     *''Alternatively, clear the tear stream data from the main Run menu
     *''/)')
	End IF
      response = COMMITQQ(DiagFile)
      CLOSE(IRI)
      CLOSE(IRP)
      CLOSE(IW)
      CLOSE(IO)
	CLOSE(31)
      ExitValue = 32
      RETURN
 9981 WRITE(DiagFile,*)'  End of file '//JobPath(1:LENG)//FILENAME
      WRITE(DiagFile,*)' Run-time error number ',ios
      response = COMMITQQ(DiagFile)
      ExitValue = 33
      CLOSE(IRI)
      CLOSE(IRP)
      CLOSE(IW)
      CLOSE(IO)
	CLOSE(31)
      RETURN

      END
