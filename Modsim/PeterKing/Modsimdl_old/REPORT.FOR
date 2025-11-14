      SUBROUTINE REPORT(JobPath,ExitValue,DiagFile)
C     *********************************************
C This program was written by subroutine PHASE5 in DIMINP.FOR
      USE MSFLIB
      CHARACTER*255 JobPath
      INTEGER*4 ExitValue,DiagFile
      REAL FEED(  35,  22,  10)
      REAL OUT1(  35,  22,  10)
      REAL OUT2(  35,  22,  10)
      REAL OUT3(  35,  22,  10)
      REAL DER1(  35,  22,  10)
      REAL DER2(  35,  22,  10)
      REAL DER3(  35,  22,  10)
      REAL GRDM(  22,   7)
      REAL GRDV(  22,   7)
      REAL SIZE(  35)
      REAL PARAM(  50)
      INTEGER INDPP(  50,2)
      REAL PPROP(1000)
      INTEGER DIMPPM,DIMPAR
      LENG = LEN_TRIM(JobPath)
      ExitValue = -1
      NDCM =   35
      NGCM =   22
      NSCM =   10
      NMINM =    7
      DIMPAR =   50
      NPPM =   50
      DIMPPM = 1000
      CALL REPWRT(NDCM,NGCM,NSCM,NMINM,FEED,OUT1,OUT2,OUT3,DER1,DER2,
     *DER3,GRDM,GRDV,SIZE,DIMPAR,PARAM,NPPM,DIMPPM,INDPP,PPROP,
     *JobPath,ExitValue,DiagFile)
      END
