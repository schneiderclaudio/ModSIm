C***************************************************************************C
C                                MODSIM                                     C
C                                                                           C
C This file is the main control for the simulation                          C
C                                                                           C
C MODSIM Copyright  R P King 2001                                           C
C***************************************************************************C

      FUNCTION INORDCALC(JobPath)
C     **************************
C Calls the input, odering,calculation, reporting and output phases from
C the DLL.
C This is the main fortran control program for the simulation.

      USE PORTLIB
      USE MSFLIB
c This function must be exported from the DLL
      !MS$ATTRIBUTES STDCALL, DLLEXPORT :: INORDCALC
      !MS$ATTRIBUTES ALIAS :'INORDCALC' :: INORDCALC
      INTEGER*2 INORDCALC
      CHARACTER*255 JobPath
      !MS$ATTRIBUTES REFERENCE :: JobPath
      INTEGER*4 ExitValue, DiagFile, LENG
      LOGICAL*4 response
C      INTEGER(4) result,IERR
C      INTEGER*2 resp

      INORDCALC = -1
      LENG = LEN_TRIM(JobPath)

c Open a file to report diagnostics coming from the DLL
      DiagFile = 13
      OPEN(DiagFile, FILE = JobPath(1:LENG)//'DIAGDLL.TXT',ERR = 9999)
      WRITE(DiagFile,1001)
 1001 FORMAT(' This file shows a record of the progress through the'/
     *' DLL that controls the simulation calculation.'//
     *' If a failure occured it should be possible to pinpoint the'/
     *' cause using the information provided here.'/)
      WRITE(Diagfile,*)'Job path is '//JobPath(1:LENG)
C Call the input, ordering and calculation stages as subroutines to keep
c the diagnostic file open.

C Call the data input routine.
      CALL DMINP(JobPath(1:LENG),ExitValue,DiagFile)
      WRITE(DiagFile, 1002) ExitValue
 1002 FORMAT(' The data input phase has completed with exit value',I3)
      response = COMMITQQ(DiagFile)
      IF(ExitValue .GT. 1) THEN
        WRITE(DiagFile, 1003)
 1003   FORMAT(' This indicates that a fatal error occurred.'/
     *  '  Further information may be available in the data echo file.')
        response = COMMITQQ(DiagFile)
        INORDCALC = ExitValue
        CLOSE(DiagFile)
        RETURN
      END IF

C Call the ordering calculation routine.
      CALL PPHI2(JobPath(1:LENG),ExitValue,DiagFile)
      WRITE(DiagFile, 1004) ExitValue
 1004 FORMAT(' The ordering phase has completed with exit value',I3)
      response = COMMITQQ(DiagFile)
      IF(ExitValue .GT. 1) THEN
        WRITE(DiagFile, 1005)
 1005   FORMAT(' This indicates that a fatal error occurred.'/
     *  '  Further information may be available in the data echo file.')
        INORDCALC = ExitValue
        response = COMMITQQ(DiagFile)
        CLOSE(DiagFile)
        RETURN
      END IF

C Call the calculation routine.
c      resp = RUNQQ(JobPath(1:LENG)//'PPHO2.EXE', JobPath(1:LENG))
      WRITE(DiagFile,*)' Will now enter the calculation phase'
	WRITE(DiagFile,*)'JobPath',JobPath(1:LENG)
	Write(DiagFile,*)'ExitValue,DiagFile',ExitValue,DiagFile
	response = COMMITQQ(DiagFile)
      CALL PPHO2(JobPath(1:LENG),ExitValue,DiagFile)
	!ExitValue = 2
      WRITE(DiagFile, 1006) ExitValue
 1006 FORMAT(' The calculation phase has completed with exit value',I4)
      response = COMMITQQ(DiagFile)
c      IF(resp .EQ. -1) THEN
      IF(ExitValue .GT. 1) THEN
        WRITE(DiagFile, 1007)
 1007   FORMAT(' This indicates that a fatal error occurred.'/
     *  '  Further information may be available in the data echo file.')
        response = COMMITQQ(DiagFile)

        INORDCALC = ExitValue
        CLOSE(DiagFile)
        RETURN
      END IF

c Call the report phase
      CALL REPORT(JobPath,ExitValue,Diagfile)
      WRITE(DiagFile,1008) ExitValue
 1008 FORMAT(' The report writer has ended with exit value ',I4)
      response = COMMITQQ(DiagFile)
      IF(ExitValue .GT. 1 ) THEN
        INORDCALC = ExitValue
        WRITE(DiagFile,1009)
 1009   FORMAT('  This indicates an error in REPORT')
      END IF
      CLOSE(DiagFile)
      RETURN

c Error trap if cannot open DiagFile
 9999 CONTINUE
      INORDCALC = 1
      RETURN

      END
