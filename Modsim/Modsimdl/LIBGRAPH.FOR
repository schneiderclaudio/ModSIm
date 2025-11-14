      PROGRAM LIBGRAPH
C     ****************
C Program to plot the liberation spectra
      USE MSFLIB

      REAL FG(15,100), GCLAS(15), CUM(15)
      REAL COMPARE(15,100)
      INTEGER NGCSTR(100)
      INTEGER STRELIST(100)
      LOGICAL STRFLAG(100), COMPFL, COMPSTR(100)
      CHARACTER*30 STR_NAME(0:100)
      REAL LINTR
      CHARACTER*48 WSTRING
      CHARACTER*8 JOBNAME
      CHARACTER*6 KEYWRD
      INTEGER*4 NoArgs
      INTEGER*2 FirstArg,status
      CHARACTER*256 JobPath,Buffer
      EXTERNAL LINTR
      COMMON /NAME/STR_NAME
      DATA STRFLAG/100*.FALSE./
      DATA COMPSTR/100*.FALSE./

c Get the job path name from the command line argument
      NoArgs = NARGS()
      FirstArg = 1
      IF(NoArgs .GT. 1) THEN
        CALL GETARG(FirstArg,JobPath,status)
        DO I = 3,NoArgs
          !Blank spaces occured in one or more directory names in the path
          FirstArg = INT2(I-1)
          CALL GETARG(FirstArg,Buffer,status)
          LENG = LEN_TRIM(JobPath)
          LenBuff = LEN_TRIM(Buffer)
          JobPath = JobPath(1:LENG)//' '//Buffer(1:LenBuff)
          Buffer = ''
        END DO
      END IF
      LENG = LEN_TRIM(JobPath)

C Read the spectra from the file
      OPEN(31, FILE = JobPath(1:LENG)//'LIBDISP.DAT')
      READ(31,*, END = 9001,ERR = 9991) NGC
   10 CONTINUE
        READ(31,1001, END = 19,ERR = 9991) KEYWRD,ISTR
 1001   FORMAT(A6,I5)
        IF(KEYWRD .NE. 'Stream') GO TO 19
        STRFLAG(ISTR) = .TRUE.
        DO J = 1,NGC
          READ(31,*,ERR = 9991) I, FG(J,ISTR)
        END DO
      GO TO 10

 9001 CONTINUE
      I = MESSAGEBOXQQ(' No liberation data is available'C,
     *  'WARNING'C,MB$OK)
        CLOSE(31)
        CALL EXIT()
 9991 CONTINUE
      I = MESSAGEBOXQQ('Error reading liberation data from simulator'C,
     *  'WARNING'C,MB$OK)
        CLOSE(31)
        CALL EXIT()

   19 CONTINUE
      CLOSE(31)

   20 CONTINUE
C  Open file LIBGRAPH.DAT to indentify the graphs to be plotted
      OPEN(32, FILE = JobPath(1:LENG)//'LIBGRAPH.DAT')
      READ(32,*,END = 9002,ERR = 9992) NSTREAMS
      READ(32,*,END = 9002,ERR = 9992) (STRELIST(I),I = 1,NSTREAMS)

c Check if experimental data to be plotted for comparison.
      READ(32,'(L1)',END = 9002,ERR = 9992) COMPFL
      DO N = 0,100
        DO I = 1,30
         STR_NAME(N)(I:I) = ' '
        END DO
      END DO
      CLOSE (32)

C      IF(COMPFL) THEN
        OPEN(34, FILE = JobPath(1:LENG)//'LIBCOMP.OUT')
        READ(34, '(A8)', END = 9003,ERR = 9993) JOBNAME
  110   CONTINUE
        READ(34,'(A48)', END = 9003,ERR = 9993) WSTRING
        IF (WSTRING .EQ. 'END OF FILE') GO TO 9003
        IF (WSTRING(1:10) .NE. 'Stream    ') THEN
          I = MESSAGEBOXQQ('Error in file LIBCOMP.OUT'C,
     *    'WARNING'C,MB$OK)
          GO TO 9003
        ELSE
          READ(WSTRING,'(10X ,2I4,A30)') N, NUNIQUE, STR_NAME(N)
          READ(34,'(A18)', END = 9003,ERR = 9993) WSTRING
          IF (WSTRING(1:10) .NE. 'Lib dist  ') THEN
            I = MESSAGEBOXQQ('Error in file LIBCOMP.OUT'C,
     *      'WARNING'C,MB$OK)
            GO TO 9003
          ELSE
            READ(WSTRING,'(10X ,I4)') NGCSTR(N)
            IF(NGCSTR(N) .GT. 1) COMPSTR(N) = .TRUE.
            DO I = 1,NGCSTR(N)
              READ(34,*,END = 9003,ERR = 9993) COMPARE(I,N)
            END DO
          END IF
          GO TO 110
        END IF
 9003   CONTINUE
        CLOSE(34)
C      END IF


C Plot the graphs
      CALL WRTAB(JobPath)
      CALL DISPLY(6)
      CALL COLOUR(8)
      CALL BBOX(150,100,500,500)
      CALL LINAX(1.0, FLOAT(NGC-1), NGC-2, 2.0, 1, 'Y', 'Y')
      CALL LABAX('Grade class',11,' ',1,10,1)
      CALL LINAX(0.0, 1.0, 10, 3.1, 2, 'Y','Y')
      CALL LABAX('Cumulative distribution',23,' ',1,10,2)
      ISYM = 0
      ITYPE = 3
      ICOLOR = 7
      IYPOS = 150
      DO J = 1,NGC
        GCLAS(J) = J
      END DO
      DO I = 1,NSTREAMS
        NOS = STRELIST(I)
        IF(STRFLAG(NOS)) THEN
          ITYPE=ITYPE+1
          IF(ITYPE.GT.10)ITYPE=4
          CALL LNTYPE(ITYPE)
          CALL CHANGECOL(ICOLOR)
          ISYM = 0
          CUM(1) = FG(1,NOS)
          DO J = 2,NGC
            CUM(J) = CUM(J-1) + FG(J,NOS)
          END DO
          CALL PLSMGR(GCLAS,CUM,NGC-1,0,LINTR,LINTR)
          IF(COMPSTR(NOS) .AND. COMPFL) THEN
            ISYM = I
            CUM(1) = COMPARE(1,NOS)
            DO J = 2,NGCSTR(NOS)
              CUM(J) = CUM(J-1) + COMPARE(J,NOS)
            END DO
            CALL DISPNT(GCLAS,CUM,NGCSTR(NOS)-1,ISYM,LINTR,LINTR)
            CALL STRMID(I,NOS,ISYM)
          ELSE
            CALL STRMID(I,NOS,0)
          END IF
        END IF
      END DO
      CALL GROUTCOORD
      CALL HDCOPY(1.0)
      CALL DISPLY(5)
      CALL CLOSE
      CALL EXIT()

 9002 CONTINUE
      I = MESSAGEBOXQQ('Data is missing'C,
     *  'WARNING'C,MB$OK)
        CLOSE(32)
        CALL EXIT()
 9992 CONTINUE
      I = MESSAGEBOXQQ('Error reading data'C,
     *  'WARNING'C,MB$OK)
        CLOSE(32)
        CALL EXIT()
 9993 CONTINUE
      I = MESSAGEBOXQQ('Error reading system data'C,
     *  'WARNING'C,MB$OK)
        CLOSE(34)
        CALL EXIT()

      END
C
C
C
      SUBROUTINE STRMID(IWAS,NOS,ISYM)
C     ********************************
C
C  DRAWS A STREAM IDENTIFIER FOR A LIBERATION DISTRIBUTION CURVE.
      CHARACTER*40 STRING
      CHARACTER*30 STR_NAME(0:100)
      COMMON /NAME/STR_NAME
C Use the actual stream number and stream name
      IF(STR_NAME(NOS) .EQ. '                              ') THEN
        WRITE(STRING,100) NOS
  100   FORMAT('Stream',I3)
        NL=9
      ELSE
        WRITE(STRING,101)NOS,STR_NAME(NOS)
  101   FORMAT(I3,A30)
        NL = 33
      END IF
      IX = MOD((IWAS-1)*350,1050) + 20
      IY = 620 + ((IWAS-1)/3)*30
      CALL LINIDC(IX,IY,80,STRING,NL)
      IF (ISYM .GT. 0) THEN
        CALL  LNTYPE(1)
        CALL SYMBOL(IX + 40,IY,ISYM)
      END IF
      RETURN
      END
