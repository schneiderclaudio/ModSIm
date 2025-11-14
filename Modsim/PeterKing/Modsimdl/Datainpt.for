C*******************     PROGRAM DATAINPT     *************************C
C                                                                      C
C  THIS PROGRAM READS DATA FROM THE DATA FILE DATT.                    C
C                                                                      C
C  MODSIM  (C) COPYRIGHT  RP KING & MA FORD  JOHANNESBURG   1985       C
C                                                                      C
C**********************************************************************C

      SUBROUTINE INPT(NPPM,NUMM,NSMM,NCNCT,NGCM,NMIN,NCYG,CONCT,INDPP,IN
     *DPAR,SCM,WTR,GRM,GRV,NAMES,NAM,WTRFUN,
     *NDCL,NGCL,NSCL,NVARM,NVR,INDCNC,NDCMD,NGCMD,NSCMD,
     *SIZ,OUTP,UNTYP,UNUM,B,PROP,PPROP,STRV,FD,FG,FS,UNTPAR,
     *JobPath,ExitValue,DiagFile)

      USE MSFLIB

      CHARACTER*255 JobPath
      INTEGER*4 ExitValue, DiagFile
      CHARACTER*4 NAMES(*),NAM(*)
      INTEGER INDPP(NPPM,2),NDCL(*),NGCL(*),NSCL(*),NVR(*),INDCNC(*),
     *CONCT(NCNCT,4)
      INTEGER INDPAR(NUMM,2),OUTP(10),UNTYP(*),SCM(NSMM,3),UNUM(*)
      INTEGER B(*)
      REAL SIZ(*), PROP(*),GRM(NGCM,NMIN),GRV(NGCM,NMIN),PPROP(*)
      REAL STRV(*),FD(*),FG(*),FS(*),UNTPAR(*),WTR(NSMM,2)
      CHARACTER*4   KEY,NAMU
      INTEGER     PPC,PP1,PP,POS,PS,FLAG,FLS,FLGM,FLGV,DIMPAR,DIMUTP,DIM
     *PP,DIMCNC,STRN,CONVM,TOL,WTCRIT,PPS,PS1
      INTEGER WTRFUN(1)
      INTEGER IDEN(80)
      CHARACTER*4 ORE
      CHARACTER*12 FILENAME
	INTEGER FileLUNumber
      LOGICAL*4 result
      CHARACTER*80 JobName
      INTEGER JobNameLength
      COMMON /JOBDATA/ JobName,JobNameLength

      DATA ORE/' ORE'/
C
C   PROGRAM TO READ INPUT DATA AND ECHO
C   ***********************************

C This subroutine returns an exit value with these values:
C       -1....Normal termination with no errors
C        0....Termination with warnings
C  10 - 19....Fatal error occurred
C
C Prevent compiler warnings for unused arguments
      PLNT = 'PLNT'
      I = NCYG
C
      WRITE(DiagFile,'(''  Reading data for the simulation'')')
      LENG = LEN_TRIM(JobPath)
      ExitValue = -1
      WRITE(DiagFile,'(''  JobPath '',255A1)') (Jobpath(I:I),I = 1,LENG)

      IR=5
      IRM=IR
      IWP=3
      IWO=7
      IWC=8
      IFL=1
      IT=2
      IWD=4
1000  FORMAT(A4,4I4)
1001  FORMAT(A4,I4,1X,A4,I4)
3001  FORMAT(20I4)
3002  FORMAT(8G10.4)
3003  FORMAT(A4)
3004  FORMAT(80A1)
9001  FORMAT(//'****ERROR****')
C
      NAMES(1)=ORE
      NAM(1)='MIXR'
C
      NDCM=1
      NGCM1=1
      NPLA=0
      NPLANT=0
      PPC=0
      PCN=0
      NCNCT1=0
      NMIN1=1
      FLAG=0
      NPP=0
      FLS=0
      FLGM=0
      FLGV=0
      NSMM1=0
      NUUM=0
      NUMM1=0
      NSSM=0
      INU=1
      IRD=0
      DIMPAR=0
      DIMUTP=0
      DIMPP=0
      DIMCNC=0
      UNTPAR(1)=0.0
      DO I = 1,NPPM
        INDPP(I,1)=0
        INDPP(I,2)=0
      END DO

      FILENAME = 'DATT.DAT'
	FileLUNumber = IR
      OPEN(IR,FILE=JobPath(1:LENG)//'DATT.DAT',ERR = 9990,IOSTAT=ios)
      WRITE(DiagFile,*)'Opening file DATT.DAT'
      result = COMMITQQ(DiagFile)
      FILENAME = 'PHI1.ECH'
	FileLUNumber = IWP
      OPEN(IWP,FILE=JobPath(1:LENG)//'PHI1.ECH',ERR = 9990,IOSTAT=ios)
      WRITE(DiagFile,*)'Opening file PHI1.ECH'
      result = COMMITQQ(DiagFile)
C
      READ(IR,'(A4)',ERR = 9999) KEY
      DO WHILE (KEY .NE. '    ')

        SELECT CASE(KEY)
          CASE('SYST')
            WRITE(DiagFile,'(''The system data has been found'')')
            READ(IR,3004,ERR = 9999) (IDEN(I),I=1,80)
	    WRITE(DiagFile,3004) (IDEN(I),I = 1,80)
	    DO I = 1,80
	      JobName(I:I) = CHAR(IDEN(I))
	    END DO
	    JobNameLength = LEN_TRIM(JobName)

          CASE('READ')
            WRITE(DiagFile,'(''The READ keyword is now obsolete'')')

          CASE('NDGS')
            NPLA=NPLA+1
            NPLT=I1
            NDCL(NPLT)=I2
            NGCL(NPLT)=I3
            NSCL(NPLT)=I4
            NVR(NPLT)=I2*I3*I4+2
            IF(NVR(NPLT).GT.NVARM) THEN
              WRITE(DiagFile,4004)NPLT,NVR(NPLT)
 4004 FORMAT(' PLANT',I4,' HAS',I4,' STREAM VARIABLES WHICH EXCEEDS THE
     *MAXIMUM ALLOWED.'/' CHANGE VARIABLE NVARM IN PROGRAM DIMINP,RECOMP
     *ILE AND RESTART.')
              ExitValue = 11
 	        Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
             RETURN
            END IF
            IF(I2.GT.NDCMD) THEN
              WRITE(DiagFile,4005)NPLT,I2,'  SIZE','NDCM'
              ExitValue = 11
	        Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
              RETURN
            END IF
            IF(I3.GT.NGCMD) THEN
              WRITE(DiagFile,4005)NPLT,I3,' GRADE','NGCM'
              ExitValue = 11
	        Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
              RETURN
            END IF
            IF(I4.GT.NSCMD) THEN
              WRITE(DiagFile,4005)NPLT,I4,'   S  ','NSCM'
              ExitValue = 11
 	        Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
              RETURN
            END IF
 4005 FORMAT(/' Plant ',I2,' has ',I3,A6,' classes specified. This excee
     *ds the maximum allowed.'/' Change variable ',A4,' in program DIMIN
     *P, recompile link and restart OR respecify your data.'/)
            IF(I1.GT.NPLANT) NPLANT=I1
            IF(I2.GT.NDCM) NDCM=I2
            IF(I3.GT.NGCM1) NGCM1=I3

          CASE('COAL')
            WRITE(DiagFile,'(''Data is set up for coal washing'')')
            IF(I1 .EQ. 1) THEN
              WRITE(DiagFile,'('' Proximate analysis data is used'')')
            END IF

          CASE('MINS')
            NMIN1=I1
            READ(IR,3003,ERR = 9999) (NAMES(I),I=1,NMIN1)

          CASE('CNCT')
            NCNCT1=NCNCT1+1
            CONCT(NCNCT1,1)=I1
            CONCT(NCNCT1,2)=I2
            CONCT(NCNCT1,3)=I3
            CONCT(NCNCT1,4)=I4
            INDCNC(NCNCT1)=PCN+1
            PCN=PCN+NVR(I1)
            DIMCNC=PCN

          CASE('SIZE')
            FLS=1
            READ(IR,3002,ERR = 9999) (SIZ(I),I=1,NDCM)

          CASE('SPGR')
            READ(IR,3002,ERR = 9999) (PROP(I),I=1,NMIN1)
            FLAG=1
            IF(NMIN1 .EQ. 1) THEN
              PPROP(1)=PROP(1)
              DIMPP=1
              PPC=1
              INDPP(1,1)=1
              INDPP(1,2)=1
            END IF

          CASE('GRDM')
            FLGM=1
            DO I = 1,NGCM1
              READ(IR,3002,ERR = 9999) (GRM(I,J),J=1,NMIN1)
              IF(FLAG .GT. 0) THEN
                FLGV=1
                TV=0.0
                DO K = 1,NMIN1
                  TV=TV+GRM(I,K)/PROP(K)
                END DO
                PPROP(I)=1.0/TV
                DO K = 1,NMIN1
                  GRV(I,K)=GRM(I,K)/PROP(K)/TV
                END DO
              END IF
            END DO
            IF(FLAG .GT. 0) THEN
              DIMPP=NGCM1
              PPC=NGCM1
              INDPP(1,1)=1
              INDPP(1,2)=NGCM1
              NPP=1
            END IF

          CASE('GRDV')
            FLGV=1
            DO I = 1,NGCM1
              READ(IR,3002,ERR = 9999) (GRV(I,J),J=1,NMIN1)
              IF(FLAG .GT. 0) THEN
                FLGM=1
                TM=0.0
                DO K = 1,NMIN1
                  TM=TM+GRV(I,K)*PROP(K)
                END DO
                PPROP(I)=TM
                DO K = 1,NMIN1
                  GRM(I,K)=GRV(I,K)*PROP(K)/TM
                END DO
              END IF
            END DO
            IF(FLAG .GT. 0) THEN
              DIMPP=NGCM1
              PPC=NGCM1
              INDPP(1,1)=1
              INDPP(1,2)=NGCM1
              NPP=1
            END IF

          CASE('PHYP')
            IF(I1 .GT. NPP) NPP=I1
            IF(I2 .LT. 1000) THEN
              IF(I1 .EQ. 1) THEN
                PP1 = 1
                PPC = I2
                INDPP(1,1) = 1
                INDPP(1,2) = I2
              ELSE
                INDPP(I1,1)=PPC+1
                INDPP(I1,2)=I2
                PP1=PPC+1
                PPC=PPC+I2
              END IF
              READ(IR,3002,ERR = 9999) (PPROP(I),I=PP1,PPC)
              WRITE(DIAGFILE,'(2I3,(8G10.4))')
     *        PP1,PPC,(PPROP(I),I=PP1,PPC)
              DIMPP=PPC
            ELSE
              I2=I2-1000
              READ(IR,3002,ERR = 9999) (PROP(I),I=1,NMIN1)
              DO I = 1,NGCM1
                PP=PPC+I
                PPROP(PP)=0.0
                DO K = 1,NMIN1
                  IF(I2.EQ.1) PPROP(PP)=PPROP(PP)+PROP(K)*GRM(I,K)
                  IF(I2.EQ.3) PPROP(PP)=PPROP(PP)+PROP(K)*GRV(I,K)
                  IF(I2.EQ.2) PPROP(PP)=PPROP(PP)+GRM(I,K)/PROP(K)
                  IF(I2.EQ.4) PPROP(PP)=PPROP(PP)+GRV(I,K)/PROP(K)
                END DO
                IF(I2.EQ.2.OR.I2.EQ.4) PPROP(PP)=1.0/PPROP(PP)
              END DO
              INDPP(I1,1)=PPC+1
              INDPP(I1,2)=NGCM1
              PPC=PPC+NGCM1
              DIMPP=PPC
            END IF

        END SELECT
        READ(IR,1000,ERR = 9999) KEY,I1,I2,I3,I4
        WRITE(DiagFile,'(''   KEY '',A4,4I4)') KEY,I1,I2,I3,I4
        result = COMMITQQ(DiagFile)
      END DO

C
C **WRITE OUTPUT ECHO
C
   34 CONTINUE
      WRITE(IWP,2110) (IDEN(I),I=1,80)
      IF(NCNCT1.EQ.0) GO TO 36
      WRITE(IWP,2001)
      DO 35 I=1,NCNCT1
      WRITE(IWP,2002) (CONCT(I,J),J=1,4)
35    CONTINUE
36    WRITE(IWP,2000) (NAMES(I),I=1,NMIN1)
      IF(FLAG .EQ. 1) WRITE(IWP,2100)(PROP(I),I=1,NMIN1)
      IF(FLS.EQ.0) GO TO 37
      WRITE(IWP,2003) (SIZ(I),I=1,NDCM)
37    IF(FLGM.EQ.0) GO TO 39
      WRITE(IWP,2004)
      DO 38 I=1,NGCM1
      WRITE(IWP,2005) I,(GRM(I,J),J=1,NMIN1)
38    CONTINUE
39    IF(FLGV.EQ.0) GO TO 41
      WRITE(IWP,2006)
      DO 40 I=1,NGCM1
      WRITE(IWP,2007)  I,(GRV(I,J),J=1,NMIN1)
40    CONTINUE
41    IF(NPP.EQ.0) GO TO 43
      WRITE(IWP,2008)
      DO 42 I=1,NPP
		N2=INDPP(I,2)
		IF(N2.EQ.0) GO TO 42
		N1=INDPP(I,1)
		N2=N1+N2-1
		WRITE(IWP,2009) I,(PPROP(J),J=N1,N2)
42    CONTINUE
C
1999  FORMAT(6G12.4)
2000  FORMAT(//20X,'MINERAL NAMES'/20X,13(1H*)/10(4X,A4))
2100  FORMAT(//20X,'MINERAL SPECIFIC GRAVITIES'/20X,26(1H*)/
     *10(1X,6G12.4/))
2001  FORMAT(//20X,'INTER-PLANT CONNECTIONS'/20X,23(1H*)/)
2002  FORMAT(10X,'FROM PLANT',I3,2X,'STREAM',I3,2X,'TO PLANT',I3,2X,'STR
     *EAM',I3)
2003  FORMAT(//20X,'PARTICLE SIZES'/20X,17(1H*)/10(6G12.4/))
2004  FORMAT(//20X,'MASS FRACTIONS IN G-CLASSES'/20X,27(1H*)/)
2005  FORMAT(1X,'G-CLASS',I2,3X,8F7.3)
2006  FORMAT(//20X,'VOLUME FRACTIONS IN G-CLASSES'/20X,29(1H*)/)
2007  FORMAT(1X,'G-CLASS',I2,3X,8F7.3)
2008  FORMAT(//20X,'PHYSICAL PROPERTIES'/20X,19(1H*)/)
2009  FORMAT(/1X,'PHYS PROP',I2,/1X,11(1H-)/10(1X,6G12.4/))
2110  FORMAT(1X,79A1)
C
C **WRITE DATA FOR CALCULATION PHASE
C
   43 CONTINUE

      FILENAME = 'PHI4.DAT'
	FileLUNumber = IWC
      OPEN(IWC,FILE=JobPath(1:LENG)//'PHI4.DAT', ERR = 9990,IOSTAT=ios)
      WRITE(DiagFile,*)'Opening file PHI4.DAT'
      result = COMMITQQ(DiagFile)
      WRITE(IWC,3004)(IDEN(I),I=1,80)
      WRITE(IWC,3001) NPLA,NCNCT1,NPP,NMIN1,NDCM,NGCM1,FLS,FLGM,FLGV,DIM
     *CNC,IRD
      WRITE(IWC,3003) (NAMES(I),I=1,NMIN1)
      IF(FLS.EQ.1) WRITE(IWC,3002) (SIZ(I),I=1,NDCM)
      IF(FLGM.EQ.0) GO TO 45
      DO 44 I=1,NGCM1
      WRITE(IWC,3002) (GRM(I,J),J=1,NMIN1)
44    CONTINUE
45    IF(FLGV.EQ.0) GO TO 47
      DO 46 I=1,NGCM1
      WRITE(IWC,3002) (GRV(I,J),J=1,NMIN1)
46    CONTINUE
47    IF(NCNCT1.EQ.0) GO TO 49
      DO 48 I=1,NCNCT1
      WRITE(IWC,3001) (CONCT(I,J),J=1,4)
48    CONTINUE
      WRITE(IWC,3001) (INDCNC(I),I=1,NCNCT1)
49    IF(NPP.EQ.0) GO TO 50
      WRITE(IWC,'(I5)')DIMPP
      DO 54 I=1,NPP
        WRITE(IWC,3001) (INDPP(I,J),J=1,2)
54    CONTINUE
      WRITE(IWC,3002) (PPROP(I),I=1,DIMPP)
      IFL=1
50    CONTINUE
C
C
C **PLANT DATA
C ************
C **NPLT,SCM,WTCR,STWT,WATER AND SOLIDS FEED
C

      READ(IR,'(A4)',ERR = 9998) KEY
      WRITE(DiagFile,'(''   KEY '',A4)') KEY
      DO WHILE(KEY .NE. '    ')
        SELECT CASE (KEY)

          CASE ('PLAN')
            WRITE(DiagFile,'(''The plant data has been found'')')
            IF(IFL.EQ.0) WRITE(IWC,3001) IFL,IFL,IFL
            IFL=0
            NS=0
            NSM=0
            NUM=0
            NU=0
            POS=2
            NPRM=0
            DO I = 1,NSMM
              B(I)=1
              DO J = 1,2
                WTR(I,J)=0
              END DO
            END DO
            DO I = 1,NUMM
              WTRFUN(I)=0
            END DO
            WTCRIT=2

          CASE ('PLNT')
            NPLT=I1
            NDC=NDCL(NPLT)
            NGC=NGCL(NPLT)
            NSC=NSCL(NPLT)
            NVAR=NVR(NPLT)
            WRITE(IWC,3001) NDC,NGC,NSC,NVAR

          CASE ('SCMA')
            NS=NS+1
            SCM(NS,1)=I1
            SCM(NS,2)=I2
            SCM(NS,3)=I3
            IF(I1.GT.NSM) NSM=I1
            IF(I2.GT.NUM) NUM=I2
            IF(I3.GT.NUM) NUM=I3

          CASE ('WTCR')
            WTCRIT=I1

          CASE ('STWT')
            B(I1)=I2

          CASE ('WTRF')
            READ(IR,3002,ERR = 9998) (WTR(I1,J),J=1,2)
            WTRFUN(I1)=I2
            IF(I2.GT.NSM)NSM=I2

          CASE ('STRM')
            READ(IR,3002,ERR = 9998) TMS,PERS
            WRITE(DiagFile,*) '  TMS, PERS',TMS, PERS

            STRN=I1
            IF(PERS .LE. 0.0) THEN
              WRITE(IWP,9001)
              WRITE(IWP,2121) STRN
              WRITE(DiagFile,2121) STRN
 2121         FORMAT(' Stream ',I3,' has no solids!'/
     *        ' If this is a tear stream,',
     *        ' edit the convergence properties in system data and'/
     *        ' do not start iterations from the previous end point')
              ExitValue = 16
              CLOSE(IRM)
	        Write(DiagFile,*)' Closing file DATT.DAT'
              result = COMMITQQ(DiagFile)
	        Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
              RETURN
            END IF
            TWF=TMS*100.0/PERS-TMS
            IF(TWF.LT.0) TWF=0.0
            STRV(1)=TMS
            STRV(2)=TWF
            POS = 2
            SM=0.0

            IF (I2 .EQ. 1) THEN
C             **READ STREAM AS MASS IN EACH CATEGORY
              DO I = 1,NSC
                DO J = 1,NGC
                  READ(IR,3002,ERR = 9998) (STRV(POS+K),K=1,NDC)
                  DO K = 1,NDC
                    SM=SM+STRV(POS+K)
                  END DO
                  POS=POS+NDC
                END DO
              END DO
              WRITE(DiagFile,2042) STRN,SM,TMS
2042  FORMAT('Check mass balance in stream',I4,/' Sum of classes',
     *E14.6,' Total solids',E14.6)
            END IF

            IF (I2 .EQ. 2) THEN
C             **READ STREAM AS MASS FRACTIONS
              FD(1)=1.0
              FG(1)=1.0
              FS(1)=1.0
              IF(NDC.NE.1) READ(IR,*,ERR=9076,END=9076)(FD(I),I=1,NDC)
              DO I = 1,NDC
                IF(NGC.NE.1)
     *            READ(IR,*,ERR=9075,END=9075)(FG(L),L=1,NGC)
                DO J = 1,NGC
                  IF(NSC.NE.1)
     *              READ(IR,*,ERR=9074,END=9074) (FS(L),L=1,NSC)
                  DO K = 1,NSC
                    PS1=I+(J-1)*NDC+(K-1)*NDC*NGC+2
                    STRV(PS1)=TMS*FD(I)*FG(J)*FS(K)
	              SM = SM + STRV(PS1)
                  END DO
                END DO
              END DO
              WRITE(DiagFile,2042) STRN,SM,TMS
            END IF

            WRITE(IWC,3001) STRN
            WRITE(IWC,'(5E14.6)') (STRV(I),I=1,NVAR)
C
            WRITE(IWP,2021) STRN,TMS,PERS,(I,I=1,NDC)
            DO K = 1,NSC
              DO J = 1,NGC
                PS=(J-1)*NDC+(K-1)*NDC*NGC
                N1=PS+3
                N2=PS+NDC+2
                WRITE(IWP,2022) K,J, (STRV(I),I=N1,N2)
              END DO
            END DO
2021  FORMAT(//20X,'STREAM NUMBER',I3/20X,16(1H*)/10X,'TOTAL MASS SOLID
     *S=',G10.4/10X,'PERCENTAGE SOLIDS=',G10.4//3X,'MASS IN EACH CATEGOR
     *Y:'/35X,'D-CLASSES'/5(10X,8I8/))
2022  FORMAT(' SCL',I2,' GCL',I2,8F8.2/5(11X,8F8.2/))

          CASE ('TEAR')
            !Read from the TEARS.OUT file
            FILENAME = 'TEARS.OUT'
	      FileLUNumber = IT
            OPEN(IT,FILE=JobPath(1:LENG)//FILENAME, ERR=9990,IOSTAT=ios)
            WRITE(DiagFile,*)'Opening file TEARS.OUT'
            result = COMMITQQ(DiagFile)
            IR=IT

          CASE ('LAST')
            !Finished with TEARS.OUT
            WRITE(DiagFile,'(''Completed reading tear data'')')
            CLOSE (IT)
            WRITE(DiagFile,*)'Closing file TEARS.OUT'
            result = COMMITQQ(DiagFile)
            IR=IRM

        END SELECT
        READ(IR,1000,ERR = 9998) KEY,I1,I2,I3,I4
        WRITE(DiagFile,'(''   KEY '',A4,4I4)') KEY,I1,I2,I3,I4
        result = COMMITQQ(DiagFile)
      END DO

      DO I = 1,NUM
        UNUM(I)=0
        INDPAR(I,1)=0
        INDPAR(I,2)=0
        UNTYP(I)=0
      END DO
      DO I = 1,NS
        IF(SCM(I,2).NE.0) UNUM(SCM(I,2))=1
        IF(SCM(I,3).NE.0) UNUM(SCM(I,3))=1
      END DO
      DO I = 1,NUM
        NU = NU + UNUM(I)
      END DO

      FILENAME = 'PHI3.DAT'
	FileLUNumber = IWO
      OPEN(IWO,FILE=JobPath(1:LENG)//'PHI3.DAT',ERR = 9990,IOSTAT=ios)
      WRITE(DiagFile,*)'Opening file PHI3.DAT'
      result = COMMITQQ(DiagFile)
      WRITE(IWO,3001) NPLT
      WRITE(IWO,3001) NS,NU,NSM,NUM,NVAR
      DO 64 I=1,NS
      WRITE(IWO,3001) (SCM(I,J),J=1,3)
64    CONTINUE
      WRITE(IWO,3001) WTCRIT
      WRITE(IWO,3001) (B(I),I=1,NSM)
C
C **WRITE OUTPUT ECHO
C
      WRITE(IWP,2014) NPLT
      WRITE(IWP,2015) NDC,NGC,NSC
      WRITE(IWP,2016)
      DO 65 I=1,NS
      WRITE(IWP,2017)  (SCM(I,J),J=1,3)
65    CONTINUE
      WRITE(IWP,2018) WTCRIT,(B(I),I=1,NSM)
C
2014  FORMAT(//2X,66(1H*)/27X,'PLANT NUMBER ',I3/2X,66(1H*))
2015  FORMAT(10X,'NUMBER OF D-CLASSES=',I3/10X,'NUMBER OF G-CLASSES=',I3
     */10X,'NUMBER OF S-CLASSES=',I3//)
2016  FORMAT(10X,'STREAM CONNECTION MATRIX'/10X,24(1H*)//)
2017  FORMAT(5X,'STREAM',I3,' CONNECTS UNIT',I3,' TO UNIT',I3)
2018  FORMAT(/20X,'STREAM WEIGHTINGS'/20X,17(1H*)//10X,'USING CRITERION'
     *,I2,' THE FOLLOWING VALUES WERE OBTAINED:'//10(5X,15I4/))
C
      IF(NSM.GT.NSMM1) NSMM1=NSM
      IF(NUM.GT.NUMM1) NUMM1=NUM
      IF(NU.GT.NUUM) NUUM=NU
      IF(NS.GT.NSSM) NSSM=NS
      WRITE(IWP,2019)

      STRN=0
      WRITE(IWC,3001) STRN
C
      DO 68 I=1,NUM
        WRITE(IWC,3092) (WTR(I,J),J=1,2),WTRFUN(I)
 3092   FORMAT(2G10.4,I4)

        IF(WTR(I,1).NE.0.0) WRITE(IWP,2020) I,WTR(I,1)
        IF(WTR(I,2).NE.0.0) WRITE(IWP,2120) I,WTR(I,2)
68    CONTINUE
2019  FORMAT(//20X,'WATER FEEDS'/20X,11(1H*))
2020  FORMAT(10X,'Unit',I3,' is fed by',G10.4,' kg/s water')
C
 2120 FORMAT(10X,'Output from unit',I3,' has',G10.4,' % solids')
C
C **RUN DATA
C **********
C
C **CONV,ITER,TOLM,TYPE,PARAMETERS,OUTPUT CODE
C
C
C
      READ(IR,'(A4)',ERR = 9997) KEY
      DO WHILE (KEY .NE. 'STOP')
        SELECT CASE (KEY)

          CASE ('RUN ')
            WRITE(DiagFile,*)'The RUN data has been found.'
            result = COMMITQQ(DiagFile)
            OUTP(1)=0
            OUTP(2)=0
            OUTP(3)=0
            OUTP(4)=1
            OUTP(5)=0
            OUTP(6)=0
            OUTP(7)=1
            OUTP(8)=6
            OUTP(9)=6
            OUTP(10)=6
            CONVM=3
            NITER=8
            TOL=-4

          CASE ('CONV')
            CONVM=I1

          CASE ('ITER')
            NITER=I1

          CASE ('TOLM')
            TOL=I1

          CASE ('COPY')
            WRITE(DiagFile, '(''The COPY keyword is now obsolete'')')

          CASE ('TYPE')
            IF(I3.EQ.0) GO TO 93
            IF(I3.GT.DIMPAR) DIMPAR=I3
            IF(NPRM.EQ.0) GO TO 84
            NPR=INDPAR(I1,2)
            IF(NPR.EQ.0) GO TO 84
            IF(NPR.EQ.I3) GO TO 85
            NPRM=NPRM-NPR
            PS=INDPAR(I1,1)
            DO I = PS,NPRM
              UNTPAR(I)=UNTPAR(I+NPR)
            END DO
            DO I = 1,NUM
              IF(INDPAR(I,1).GT.PS) INDPAR(I,1)=INDPAR(I,1)-NPR
            END DO
84          INDPAR(I1,1)=NPRM+1
            INDPAR(I1,2)=I3
            NPRM=NPRM+I3
85          NIP2=INDPAR(I1,1)+INDPAR(I1,2)-1
            NIP=INDPAR(I1,1)
            READ(IR,*,ERR = 9997) (UNTPAR(I),I=NIP,NIP2)
C
93          CONTINUE   
            IF(NAMU .EQ. 'MIXR' .OR. NAMU .EQ. 'CONV') THEN
              UNTYP(I1)=1
            ELSE
              IF(INU.EQ.1) GO TO 95
              DO 94 I=2,INU
                IF(NAMU.NE.NAM(I)) GO TO 94
                UNTYP(I1)=I
                GO TO 96
94            CONTINUE
95            INU=INU+1
              NAM(INU)=NAMU
              UNTYP(I1)=INU
            END IF
96          CONTINUE

          CASE ('OUTC')
            READ(IR,3001,ERR = 9997) (OUTP(I),I=1,10)

        END SELECT
        READ(IR,1001,ERR = 9997) KEY,I1,NAMU,I3
        WRITE(DiagFile,'(A4,I4,1X,A4,I4)')KEY,I1,NAMU,I3
        result = COMMITQQ(DiagFile)
      END DO

C
C **WRITE DATA FOR CALC
C
C
87    WRITE(IWC,3001) CONVM,NITER,TOL
      WRITE(IWC,3012) (UNTYP(I),NAM(UNTYP(I)),I=1,NUM)
 3012 FORMAT(I4,2X,A4)
      DO 88 I=1,NUM
        WRITE(IWC,3001) (INDPAR(I,J),J=1,2)
88    CONTINUE
      WRITE(IWC,'(5E16.8)') (UNTPAR(I),I=1,NPRM)
      WRITE(IWC,3001) (OUTP(I),I=1,10)
      IF(NPRM.GT.DIMUTP) DIMUTP=NPRM
C
C **WRITE ECHO
C
      WRITE(IWP,2010) CONVM,NITER,TOL
      DO 89 I=1,NUM
        IF(UNTYP(I).EQ.0) GO TO 89
        WRITE(IWP,2011) I
        WRITE(IWP,2012) NAM(UNTYP(I)),INDPAR(I,2)
        PS=INDPAR(I,1)
        IF(PS.EQ.0) GO TO 89
        NPR=INDPAR(I,2)
        PPS=PS+NPR-1
        WRITE(IWP,1999) (UNTPAR(J),J=PS,PPS)
89    CONTINUE
      WRITE(IWP,2013) (OUTP(I),I=1,10)
C
2010  FORMAT(//20X,'RUN DATA',/20X,8(1H*)/20X,8(1H*)//10X,'CONVERGENCE M
     *ETHOD=',I3/7X,'MAX NUMBER ITERATIONS=',I3/13X,'TOLERANCE LIMIT=10*
     **',I2/)
2011  FORMAT(//10X,'UNIT NUMBER',I3/10X,14(1H=))
2012  FORMAT(/10X,'IS A ',A4,' UNIT AND NEEDS',I3,' PARAMETERS'/)
2013  FORMAT(/////10X,'THE SELECTED OUTPUT CODES ARE:'//10X,10I4//)
C
      IF(DIMPP.EQ.0) DIMPP=1
      IF(NCNCT1.EQ.0) NCNCT1=1
      IF(NPP.EQ.0) NPP=1
      IF(DIMUTP.EQ.0) DIMUTP=1
      IF(DIMPAR.EQ.0) DIMPAR=1
      IF(DIMCNC.EQ.0) DIMCNC=1
      NPLT=0
      WRITE(IWC,3001) NPLT,NPLT,NPLT
      WRITE(IWO,3001) NPLT
      WRITE(IWO,3001) NPLANT,NCNCT1,NPP,DIMPP,DIMUTP,DIMPAR,NDCM,NGCM1,N
     *MIN,DIMCNC
	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      WRITE(DiagFile,*)' Leaving INPT.'
      RETURN

9990  continue
C     This is the error trap for errors that occur when a file is
C     opened.
      WRITE(DiagFile,*)
     *'An error occurred when opening file '//JobPath(1:LENG)//FILENAME
      WRITE(DiagFile,*)' Run-time error number ',ios
      ExitValue = 10
      result = COMMITQQ(DiagFile)
	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      RETURN

9999  continue
C     This is the error trap for read errors in the system data
      WRITE(DiagFile,*)'An error occurred when reading the system data'
      ExitValue = 12
      result = COMMITQQ(DiagFile)
	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      RETURN

9998  continue
C     This is the error trap for read errors in the plant data
      WRITE(DiagFile,*)'An error occurred when reading the plant data'
      IF(IR .EQ. IT) THEN
        ExitValue = 13
        WRITE(DiagFile,*)
     *  'Program was reading data from the tear stream file.'
        WRITE(Diagfile,*)
     *  'Try restarting the simulation without using tear stream informa
     *tion.'
        WRITE(DiagFile,*)
     *  'To do this clear the appropriate check box on the convergence p
     *roperties screen or clear the tear stream data.'
        CLOSE(IRM)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file TEARS.OUT'
        result = COMMITQQ(DiagFile)
      ELSE
        ExitValue = 12
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
      END IF

	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      RETURN

9997  continue
C     This is the error trap for read errors in the run data
      WRITE(DiagFile,*)'An error occurred when reading the run data'
      ExitValue = 12
      result = COMMITQQ(DiagFile)
	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      RETURN

C Write error diagnostics to the diagnostic file file
 9076 WRITE(DiagFile,9001)
      WRITE(DiagFile,'(''Error reading size data for stream '',I4)')STRN
      result = COMMITQQ(DiagFile)
      IF(IR .EQ. IT) THEN
        ExitValue = 13
        WRITE(DiagFile,*)
     *  'Program was reading data from the tear stream file.'
        WRITE(Diagfile,*)
     *  'Try restarting the simulation without using tear stream informa
     *tion.'
        WRITE(DiagFile,*)
     *  'To do this clear the appropriate check box on the convergence p
     *roperties screen or clear the tear stream data.'
        CLOSE(IRM)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file TEARS.OUT'
        result = COMMITQQ(DiagFile)
      ELSE
        ExitValue = 12
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
      END IF
	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      RETURN

 9075 WRITE(DiagFile,9001)
      WRITE(DiagFile,'(''Error reading grade data for stream '',I4,
     *'' in size class '',I4)') STRN, I
      result = COMMITQQ(DiagFile)
      IF(IR .EQ. IT) THEN
        ExitValue = 13
        WRITE(DiagFile,*)
     *  'Program was reading data from the tear stream file.'
        WRITE(Diagfile,*)
     *  'Try restarting the simulation without using tear stream informa
     *tion.'
        WRITE(DiagFile,*)
     *  'To do this clear the appropriate check box on the convergence p
     *roperties screen or clear the tear stream data.'
        CLOSE(IRM)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file TEARS.OUT'
        result = COMMITQQ(DiagFile)
      ELSE
        ExitValue = 12
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
      END IF
	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      RETURN

 9074 WRITE(DiagFile,9001)
      WRITE(DiagFile,'(''Error reading s-class data for stream '',I4,
     *'' in grade class'',I4,'' and size class'',I4)')STRN, J, I
      result = COMMITQQ(DiagFile)
      IF(IR .EQ. IT) THEN
        ExitValue = 13
        WRITE(DiagFile,*)
     *  'Program was reading data from the tear stream file.'
        WRITE(Diagfile,*)
     *  'Try restarting the simulation without using tear stream informa
     *tion.'
        WRITE(DiagFile,*)
     *  'To do this clear the appropriate check box on the convergence p
     *roperties screen or clear the tear stream data.'
        CLOSE(IRM)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file TEARS.OUT'
        result = COMMITQQ(DiagFile)
      ELSE
        ExitValue = 12
        CLOSE(IR)
        WRITE(DiagFile,*)'Closing file DATT.DAT'
        result = COMMITQQ(DiagFile)
      END IF
	Call CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      RETURN

      END

      Subroutine CloseAllFiles(IWC,IWO,IWP,IR,IT,Diagfile)
      !***************************************************
      !Closes any files that are still open 
      INTEGER DiagFile 
      CLOSE(IWC)
      WRITE(DiagFile,*)'Closing file PHI4.DAT'
      result = COMMITQQ(DiagFile)
      CLOSE(IWO)
      WRITE(DiagFile,*)'Closing file PHI3.DAT'
      result = COMMITQQ(DiagFile)
      CLOSE(IWP)
      WRITE(DiagFile,*)'Closing file PHI1.ECH'
      result = COMMITQQ(DiagFile)
C
      CLOSE (IR)
      WRITE(DiagFile,*)'Closing file DATT.DAT'
      result = COMMITQQ(DiagFile)
      CLOSE (IT)
      WRITE(DiagFile,*)'Closing file TEARS.OUT'
      result = COMMITQQ(DiagFile)

	END
  