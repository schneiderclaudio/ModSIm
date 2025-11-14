      SUBROUTINE REPWRT(NDCM,NGCM,NSCM,NMINM,FEED,OUT1,OUT2,OUT3,DER1,
     *DER2,DER3,GRDM,GRDV,SIZE,DIMPAR,PARAM,NPPM,DIMPPM,INDPP,PPROP,
     *JobPath,ExitValue,DiagFile)
C     ****************************************************************
C     Writes the report file

      USE MSFLIB
      USE GLOBALS
      REAL FEED(*)
      REAL OUT1(*), OUT2(*)
      REAL OUT3(*)
      REAL GRDM(NGCM,NMINM)
      REAL GRDV(NGCM,NMINM)
      REAL SIZE(NDCM)
      INTEGER DIMPAR,DIMPPM
      REAL PARAM(DIMPAR)
      REAL PPROP(DIMPPM)
      INTEGER INDPP(NPPM,2)
      INTEGER UNIT,UNTY
      CHARACTER*4 NAMES(7),ModelCode
      CHARACTER*255 JobPath
      INTEGER*4 ExitValue,DiagFile
      CHARACTER*12 FILENAME
      LOGICAL*4 response

c Set up common areas to transmit data to unit model reports.
      INTEGER IDEN(80)
      COMMON /JOBDATA/IDEN
      COMMON /MODELDAT/UNIT
      COMMON/MINNAMES/NAMES
      LOGICAL UNITWFEED
      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT
C
C Get the length of the job path
c ******************************
      PathLength = LEN_TRIM(JobPath)
      UnitJobPath = JobPath
      UnitDiagFile = DiagFile
      UnitExitValue = ExitValue

      WRITE(DiagFile,*)' In REPWRT'
      WRITE(DiagFile,*)' Job path '//JobPath(1:PathLength)
      response = COMMITQQ(DiagFile)


c Open the output file.
C *********************
      FILENAME = 'REPORT1.OUT'
      OPEN(8,FILE = JobPath(1:PathLength)//'REPORT1.OUT',ERR = 9990)

c Create a blank REPORT2.OUT file to append report file data from user models
      OPEN(9,FILE = JobPath(1:PathLength)//'REPORT2.OUT')
      REWIND(9)
      ENDFILE(9)
      CLOSE(9)

c Create a blank MODELGRP.OUT file to append any special graph data from the
c individual report files.
      OPEN(30,FILE = JobPath(1:PathLength)//'MODELGRP.OUT')
      REWIND(30)
      ENDFILE(30)
      CLOSE(30)

C Open the REPORT.DAT file to get the data for each unit
c ******************************************************
      FILENAME = 'REPORT.DAT'
      OPEN(31, FILE = JobPath(1:PathLength)//'REPORT.DAT',ERR = 9990)

C Initialize INDPP to zero
	INDPP = 0

c Write the file header
      CALL HEAD(8)
C Get the data
      READ(31,7001, ERR = 9999, END= 9998) NUNITS
      READ(31,7001, ERR = 9999, END= 9998) NDC, NGC, NSC
      DO 55 I = 1,NDC
        READ(31,7002, ERR = 9999, END= 9998) SIZE(I)
   55 CONTINUE
      READ(31,7001, ERR = 9999, END= 9998) NPP
      DO 254 I = 1,NPP
        READ(31,7001, ERR = 9999, END= 9998) INDPP(I,1),INDPP(I,2)
        DO 253 J = INDPP(I,1),INDPP(I,1)+INDPP(I,2)-1
          READ(31,7002, ERR = 9999, END= 9998) PPROP(J)
  253   CONTINUE
  254 CONTINUE
      READ(31,7001, ERR = 9999, END= 9998) NMIN,NGC
      READ(31,8012, ERR = 9999, END= 9998) (NAMES(I),I=1,NMIN)
 8012 FORMAT(A4)
      DO 252 J = 1,NGC
        READ(31,7002, ERR = 9999, END= 9998) (GRDM(J,K),K = 1,NMIN)
  252 CONTINUE
      DO 251 J= 1,NGC
        READ(31,7002, ERR = 9999, END= 9998) (GRDV(J,K),K = 1,NMIN)
  251 CONTINUE
      DO 900 N = 1,NUNITS
        READ(31,7011, ERR = 9999, END= 9998) UNIT, UNTY, ModelCode
 7011   FORMAT(2I8,2X,A4)
 7001   FORMAT(3I8)
        READ(31,7001, ERR = 9999, END= 9998) NPAR
        READ(31,7002, ERR = 9999, END= 9998) (PARAM(I), I = 1,NPAR)
        READ(31,7002, ERR = 9999, END= 9998) TMSF, TMS1, TMS2, TMS3
        READ(31,7002, ERR = 9999, END= 9998) WTR, WTR1, WTR2, WTR3
        IF(UNTY. GT. 1) THEN
          NVAL = NDC*NGC*NSC
          DO 258 I = 1,NVAL
            READ(31,7002, ERR = 9999, END= 9998)
     *      FEED(I),OUT1(I),OUT2(I),OUT3(I)
  258     CONTINUE
          READ(31,7003, ERR = 9999, END= 9998)
     *    UNITWFEED, WATERADD, SOLIDCONT
        END IF
 7002   FORMAT(4G20.8)
 7003   FORMAT(L1,2G20.8)
        WRITE(DiagFile,1000)UNIT
 1000   FORMAT(' Writing a design report on unit',I4,
     *  ' to the report file.')
        response = COMMITQQ(DiagFile)

C       Call the report writer for this unit
c       ************************************
        CALL RU(TMSF,TMS1,TMS2,TMS3,FEED,OUT1,OUT2,OUT3,DER1,DER2,DER3,
     *  NDC,NGC,NSC,WTR,WTR1,WTR2,WTR3,SIZE,PARAM,PPROP,INDPP,FL,NPPM,
     *  GRDM,GRDV,NMIN,NGCM,UNTY,NPAR,ModelCode)
  900 CONTINUE
      ExitValue = UnitExitValue
      CLOSE(8)
      CLOSE(31)
      RETURN

9990  continue
C     This is the error trap for errors that occur when a file is
C     opened.
      WRITE(DiagFile,*)
     *'An error occurred opening file '//JobPath(1:PathLength)//FILENAME
      ExitValue = 40
      response = COMMITQQ(DiagFile)
      RETURN

9999  continue
C     This is the error trap for read errors in the report data
      WRITE(DiagFile,*)'An error occurred when reading the report data'
      ExitValue = 41
      response = COMMITQQ(DiagFile)
      CLOSE(8)
      CLOSE(31)
      RETURN

9998  continue
C     This is the error trap for end of file in the report data
      WRITE(DiagFile,*)
     *'An end of file occurred when reading the report data'
      ExitValue = 42
      response = COMMITQQ(DiagFile)
      CLOSE(8)
      CLOSE(31)
      RETURN

      END
