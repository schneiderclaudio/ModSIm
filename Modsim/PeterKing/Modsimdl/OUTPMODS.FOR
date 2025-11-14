C*******************      outpmods.for    *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS THE REPORTING CODE FOR MODSIM MODELS.            C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C
C
C
      SUBROUTINE HEAD(LU)
C     *******************
C  WRITES THE MODSIM DESIGN REPORT HEADER TO LOGICAL UNIT LU AND
C  NOTIFIES THE USER AT THE TERMINAL. NUNIT IS THE UNIT NUMBER.
C
      CHARACTER*80 JobName
      Integer JobNameLength
      COMMON /JOBDATA/JobName,JobNameLength

      WRITE(LU,1001)
 1001 FORMAT(     //15X,'*****************************'/
     *              15X,'* MODSIM UNIT DESIGN REPORT *'/
     *              15X,'*****************************'/)
      RETURN
      END
C
C
      SUBROUTINE HEADER(LU,ITERM,NUNIT,HEADIN,NC,MODEL)
C     *************************************************
C  WRITES A DESIGN REPORT HEADER FOR A SPECIFIC UNIT.
C
      CHARACTER*50 HEADIN,WSTR
      CHARACTER*1 STRH(80),ULINE(80)
      EQUIVALENCE (WSTR,STRH(1))
      CHARACTER*4 MODEL
      CHARACTER*80 JobName
      Integer JobNameLength
      COMMON /JOBDATA/JobName,JobNameLength

      WSTR=HEADIN
      DO 10 I=1,NC
        ULINE(I)='*'
   10 CONTINUE
      WRITE(LU,1002)(STRH(I),I=1,NC)
 1002 FORMAT(//' UNIT EQUIPMENT DATA SHEET - ',50A1)
      WRITE(LU,1003)(ULINE(I),I=1,NC)
 1003 FORMAT(' ****************************',50A1)
      WRITE(LU,1004) NUNIT,MODEL
 1004 FORMAT(/'     Unit number',I4,10X,' MODSIM model name ',A4)
      WRITE(LU,1001) (JObName(I:I),I = 1,JobNameLength)
 1001 FORMAT('       Job name: ',80A1)
      RETURN
      END
C
C
      SUBROUTINE NOTES(LU,LINES)
C     **************************
C
C  WRITES THE NOTES LEGEND TO LOGICAL UNIT LU LEAVING LINES BLANK.
      WRITE(LU,1001)
 1001 FORMAT(//5X,'NOTES:')
      DO 10 I=1,LINES
      WRITE(LU,1002)
   10 CONTINUE
 1002 FORMAT(' ')
      RETURN
      END
