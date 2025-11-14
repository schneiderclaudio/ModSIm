C*******************   PROGRAM OPGRAPH   *********************C
C                                                             C
C                                                             C
C  THIS IS THE MODULE FOR GRAPHIC OUTPUT FROM MODSIM.         C
C  SEE MODSIM MANUAL FOR USAGE.                               C
C                                                             C
C  (C) COPYRIGHT RP KING  JOHANNESBURG 1985                   C
C*************************************************************C
C
C
      PROGRAM OPGRAPH
C     ***************
      USE MSFLIB
      REAL SIZE(25),CUMSIZ(25),MEMCUM(25,0:10)
      REAL COMPSIZE(25,0:10), COMPARE(25,0:10)
      CHARACTER*30 STR_NAME(0:10),STRING
      CHARACTER*1 IDEN(80)
      CHARACTER*7 SIZUN(7)
      CHARACTER*8 JOBNAME
      CHARACTER*10 WSTRING
      CHARACTER*30 STREAM_NAME
      CHARACTER*80 MESSAGE
      CHARACTER*256 JobPath
      CHARACTER*256 Buffer
      INTEGER*4 NoArgs
      INTEGER*2 FirstArg,status
      INTEGER ISTRM(0:10),NSIZE(0:10)
      INTEGER AXTYPE
      CHARACTER*1 OPP,GRID
      REAL LINTR,ALN
      EXTERNAL LINTR,ALN
      LOGICAL INPFL(100),MFFLG(100)
      LOGICAL BOXFL,ERREND,COMPFL,COMPSTR(0:10)
      COMMON /NAME/NSTRM,ISTRM,STR_NAME
      INTEGER*4 Diagfile
      LOGICAL*4 response
      INTEGER*4 resp

      DATA SIZUN/'meters ','       ','cms    ','mms    ','       ',
     $'       ','microns'/

c Open a diagnostic file
      Diagfile = 13
      OPEN(13, FILE = "OPGDIAG.TXT")
      write(13,*)' Has entered OPGRAPH'
      response = COMMITQQ(diagfile)

c Get the job path name from the command line argument
      NoArgs = NARGS()
      write(13,*)' NoArgs',NoArgs
      response = COMMITQQ(diagfile)
      FirstArg = 1
      IF(NoArgs .GT. 1) THEN
        !Blank spaces occured in one or more directory names in the path
        CALL GETARG(FirstArg,JobPath,status)
      write(13,*)JobPath
      response = COMMITQQ(diagfile)
        DO I = 3,NoArgs
          FirstArg = INT2(I-1)
          CALL GETARG(FirstArg,Buffer,status)
      write(13,*)JobPath
      response = COMMITQQ(diagfile)
          LENG = LEN_TRIM(JobPath)
          LenBuff = LEN_TRIM(Buffer)
          JobPath = JobPath(1:LENG)//' '//Buffer(1:LenBuff)
          Buffer = ''
        END DO
      END IF

	!Activate to run fromMSDEV JobPath = 'c:\mysoftware\modsim\modsimvb\'
      LENG = LEN_TRIM(JobPath)


C        resp = MESSAGEBOXQQ(JobPath(1:LENG)//CHAR(0),
C     * 'Path for WRTAB'C,MB$OK)

      WRITE(13,*)'Job path ',JobPath(1:LENG)
      response = COMMITQQ(DiagFile)

      CALL WRTAB(JobPath)

      !Open a file to export the data to PSDPLOT
      OPEN(36, FILE = JobPath(1:LENG)//'PSDPLOT.OUT')
      WRITE(36,'(''PSD_PLOT'')')

C Open file PSDGRAPH for data coming from form PSDGRAPH.FRM
      OPEN(31, FILE = JobPath(1:LENG)//'PSDGRAPH.OUT',ERR = 9990)
C  Get number of streams, and the stream numbers for which graphs are required
      READ(31,*, END = 9991)NSTRM
        IF(NSTRM .GT. 10) THEN
        resp = MESSAGEBOXQQ('Choose no more than 10 streams per graph'C,
     *  'WARNING'C,MB$OK)
        CALL EXIT()
        END IF
      READ(31,*,End = 9991)(ISTRM(IWAS),IWAS=1,NSTRM)

      WRITE(36,'(I3)') NSTRM
      DO IWAS = 1,NSTRM
        WRITE(36,'(I3)') ISTRM(IWAS)
      END DO

C SPECIFY CODE FOR REQUIRED UNIT FOR SIZE.  0=METERS, 2=CM  3=MMS. 6=MICRONS.')
      READ(31,*,End = 9991)ICODE
C Get preferred set of axes  1...LINEAR-LOG  2...LOG-LOG
      READ(31,*,End = 9991)AXTYPE
      READ(31,'(A1)',END = 9991) GRID
      OPP = 'Y'
      BOXFL=.FALSE.
      READ(31, '(L1)',END = 9991) COMPFL
      DO IWAS = 0,NSTRM
        DO I = 1,30
         STR_NAME(IWAS)(I:I) = ' '
        END DO
      END DO
      CLOSE (31)
      WRITE(13,*)'File PSDGRAPH.OUT has been read'
      response = COMMITQQ(DiagFile)

      !WRITE(36,*)
      WRITE(36,'(I3)') ICODE
      WRITE(36,'(I3)') AXTYPE

C     Read data coming from the simulator.
C     ************************************
      OPEN(33,FILE = JobPath(1:LENG)//'OPGRAPH.DAT')
      CALL LABRDC(IDEN,NLAB,33,ERREND)
      IF(ERREND) THEN
        resp = MESSAGEBOXQQ(
     *  'There is no particle size data available'C,'ERROR',MB$ICONSTOP)
        CALL EXIT()
      END IF
      READ(33,'(17X,3I5)',END = 9992) NDC,NGC,NSC
      IF(NDC.GT.1) THEN
        READ(33,'(18X,I5)',END = 9992) NSTRE
        READ(33,'(//(80L1))',END = 9992) (INPFL(I),I=1,NSTRE)
        READ(33,'(//(80L1))',END = 9992) (MFFLG(I),I=1,NSTRE)
        READ(33,'(//(6G13.4))',END = 9992)(SIZE(I),I=1,NDC)
        READ(33,'(//(6G13.4))',END = 9992)(CUMSIZ(I),I=1,NDC)
        CUMSIZ = 10**ICODE*CUMSIZ
        DO 3040 I=1,NSTRE
          READ(33,'(6X,I5)',END = 9992) NN
C         Has this stream been chosen for plotting?
          N = 0
          DO IWAS = 1,NSTRM
            IF(NN .EQ. ISTRM(IWAS)) N = IWAS
          END DO
          READ(33,'(6G13.4)',END = 9992) (MEMCUM(J,N),J=1,NDC)
 3040   CONTINUE
        MEMCUM = 100*MEMCUM
      END IF
      ITYPE=3
      IPEN=0
      ICOLOR = 7
      CLOSE(33)
      WRITE(13,*)'File OPGRAPH.DAT has been read'
      response = COMMITQQ(DiagFile)
C
C Plot experimental size distributions for comparison?
      IF(COMPFL) THEN
        OPEN(34, FILE = JobPath(1:LENG)//'SIZECOMP.OUT')
        READ(34,'(A8)',END = 9001) JOBNAME
        IF(JOBNAME .EQ. 'END OF F') GO TO 9001
   10   CONTINUE
          READ(34,'(A10,2I4,A30)', ERR = 9001) WSTRING,NN,ID,STREAM_NAME
          IF(WSTRING .EQ. 'END OF FIL') GO TO 9001
          IF (WSTRING .EQ. 'Stream    ') THEN
   20       READ(34,'(A10)',END = 9993) WSTRING
            IF(WSTRING .EQ. 'Size dist ') THEN
C             Has this stream been chosen for plotting?
              N = 0
              DO IWAS = 1,NSTRM
                IF(NN .EQ. ISTRM(IWAS)) N = IWAS
              END DO
              STR_NAME(N) = STREAM_NAME
              BACKSPACE (34, ERR = 9001)
              READ(34,'(10X,I4)', ERR = 9001)  NSIZE(N)
              COMPSTR(N) = .TRUE.
              DO I = 1,NSIZE(N)
                READ(34,*,END = 9993) COMPSIZE(I,N), COMPARE(I,N)
              END DO
            ELSE IF (WSTRING .EQ. 'Water feed') THEN
              GO TO 10
            ELSE
              GO TO 20
            END IF
          END IF
          GO TO 10
 9001   CONTINUE
        COMPSIZE = COMPSIZE*10**ICODE
        COMPARE = COMPARE*100
        CLOSE (34)
        WRITE(13,*)'File SIZECOMP.OUT has been read'
        response = COMMITQQ(DiagFile)
      END IF

      DO 99 IWAS = 1,NSTRM
        NDC1 = NDC-1
        DO I = 1,NDC1
          MSZ=NDC-I+1
          IF(MEMCUM(MSZ,IWAS).GT.0.0) GO TO 2150
        END DO
        WRITE(MESSAGE,'(''Unable to plot size distribution for stream ''
     *  ,I3)')ISTRM(IWAS)
        resp=MESSAGEBOXQQ(MESSAGE,'WARNING'C,MB$OK)
        GO TO 99
 2150   CONTINUE
        WRITE(DIAGFILE,*)'Stream, Points retained ',IWAS,MSZ
        response = COMMITQQ(DiagFile)

        !Use the actual stream number or stream name
        IF(STR_NAME(IWAS) .EQ. '                              ') THEN
          WRITE(STRING,'(''Stream'',I4)')ISTRM(IWAS)
        ELSE
          WRITE(STRING,'(A30)') STR_NAME(IWAS)
        END IF

        WRITE(36,'(''Data set  '',I4,1X,I4,1X,A30)')
     *  ISTRM(IWAS),MSZ,STRING
        DO I = 1,MSZ
          WRITE(36,'(G10.4,1X,G10.4)')CUMSIZ(I),MEMCUM(I,IWAS)
        END DO

        IF(IWAS.EQ.1) CALL PLOTID(IDEN,NLAB)

        !Draw a box if this is the first graph
        IF(.NOT. BOXFL) THEN
          CALL BBOX(150,100,750,500)
          CALL LOGLIM(CUMSIZ(NDC-1),CUMSIZ(1),XO,XF,NDEC)
          CALL LGAXIS(XO,XF,1,OPP,GRID)
          CALL LABAX('Particle size',13,SIZUN(ICODE+1),7,13,1)
          SELECT CASE(AXTYPE)
           CASE(1)
           CALL LINAX(0.0,100.0,20,3.0,2,OPP,GRID)
           CASE(2)
           CALL LOGLIM(MEMCUM(MSZ,IWAS),100.0,YO,YF,NDECY)
           CALL LGAXIS(YO,YF,2,OPP,GRID)
          END SELECT
          CALL LABAX('Cumulative % smaller',20,' ',1,13,2)
          BOXFL = .TRUE.
        END IF

        !Draw the graph
        ITYPE=ITYPE+1
        IF(ITYPE .GT. 10) ITYPE = 4
        CALL LNTYPE(ITYPE)
        CALL CHANGECOL(ICOLOR)
        CALL LBSIZE(8,14)
        ISYM = IWAS
        SELECT CASE(AXTYPE)
          CASE(1)
	    CALL CLIPBOX
          CALL PLSMGR(CUMSIZ,MEMCUM(1,IWAS),MSZ,0,ALN,LINTR)
	    CALL CLIPOFF
          IF(COMPSTR(IWAS) .AND. NSIZE(IWAS) .GT. 0) THEN
	      CALL CLIPBOX
            CALL DISPNT(COMPSIZE(1,IWAS),COMPARE(1,IWAS),NSIZE(IWAS),
     *      ISYM,ALN,LINTR)
	      CALL CLIPOFF
            CALL STRMID(IWAS, ISYM)
          ELSE
            CALL STRMID(IWAS, 0)
          END IF

          CASE(2)
	    CALL CLIPBOX
          CALL PLSMGR(CUMSIZ,MEMCUM(1,IWAS),MSZ,0,ALN,ALN)
	    CALL CLIPOFF
          IF(COMPSTR(IWAS) .AND. NSIZE(IWAS) .GT. 0) THEN
 	      CALL CLIPBOX
            CALL DISPNT(COMPSIZE(1,IWAS),COMPARE(1,IWAS),NSIZE(IWAS),
     *      ISYM,ALN,ALN)
	      CALL CLIPOFF
            CALL STRMID(IWAS, ISYM)
          ELSE
            CALL STRMID(IWAS, 0)
          END IF
        END SELECT
   99 CONTINUE
      CLOSE(36)
      CALL GROUTCOORD
      CALL HDCOPY(1.1)
      CALL CLOSE
      CLOSE(DiagFile)
      CALL EXIT()

 9993 CONTINUE
 9992 CONTINUE
 9991 CONTINUE
      resp = MESSAGEBOXQQ('End of file '//JobPath(1:LENG)//'PSDGRAPH.OUT
     *'//CHAR(0),'ERROR',MB$ICONSTOP)
      CALL EXIT()
 9990 CONTINUE
      resp = MESSAGEBOXQQ('Error opening file '//JobPath(1:LENG)//'PSDGR
     *APH.OUT'//CHAR(0),'ERROR',MB$ICONSTOP)
      CALL EXIT()
      END
C
C
C
      SUBROUTINE STRMID(IWAS,ISYM)
C     ****************************
C
C  DRAWS A STREAM IDENTIFIER FOR A PART SIZE DISTRIBUTION CURVE.
      CHARACTER*40 STRING
      INTEGER ISTRM(0:10)
      CHARACTER*30 STR_NAME(0:10)
      COMMON /NAME/NSTRM,ISTRM,STR_NAME
C Use the actual stream number and stream name
      IF(STR_NAME(IWAS) .EQ. '                              ') THEN
        WRITE(STRING,100)ISTRM(IWAS)
  100   FORMAT('Stream',I3)
        NL=9
      ELSE
        WRITE(STRING,101)ISTRM(IWAS),STR_NAME(IWAS)
  101   FORMAT(I3,A30)
        NL = 30
      END IF
      IX = MOD((IWAS-1)*350,1050) + 20
      IY = 620 + ((IWAS-1)/3)*30
      CALL LINIDC(IX,IY,80,STRING,NL)
      CALL  LNTYPE(1)
      IF (ISYM .GT. 0) CALL SYMBOL(IX + 40,IY,ISYM)
      RETURN
      END
C
C
      SUBROUTINE PLOTID(IDEN,NLAB)
C     ****************************
C
C  PLOTS THE RUN ID ON GRAPHIC OUTPUT.
      CHARACTER*1 IDEN(80)
      CALL LBSIZE(8,16)
      CALL MGMOVE(20,30)
      CALL LABELC(IDEN,NLAB)
      RETURN
      END
