      PROGRAM TESTPSD
C     ***************
C  Plot the data in the selected coordinate system.
      USE MSFLIB
      INTEGER SET_ID(30), N_POINTS(30), SELECTED_ID(30)
      INTEGER SIZE_UNIT, COORDINATES
      INTEGER TRUNCATE
      CHARACTER*30 DATA_SET_NAME(30)
      REAL SMESH(0:25,30),FR(0:25,30)
      REAL ALN, LLRC, LINTR, GAUINV, LOGIST,TRUNCLIN,TRUNCLOG
      EXTERNAL ALN, LINTR, LLRC, GAUINV, LOGIST, TRUNCLIN,TRUNCLOG
      COMMON /TRUNCATED/TRUNCSIZE
      CHARACTER*6 LABEL, SIZE_LABEL(0:6)
      COMMON DATA_SET_NAME, SET_ID
      DATA SIZE_LABEL/'  m   ','  dm  ','  cm  ','  mm  ','      ',
     *'      ','micron'/
      CHARACTER*256 JobPath
      CHARACTER*256 Buffer
      INTEGER*4 NoArgs
      INTEGER*2 FirstArg,status
      INTEGER Diagfile
      LOGICAL*4 response
      INTEGER*4 resp

c Get the job path name from the command line argument
      NoArgs = NARGS()
      FirstArg = 1
      IF(NoArgs .GT. 1) THEN
        !Blank spaces occured in one or more directory names in the path
        CALL GETARG(FirstArg,JobPath,status)
        DO I = 3,NoArgs
          FirstArg = INT2(I-1)
          CALL GETARG(FirstArg,Buffer,status)
          LENG = LEN_TRIM(JobPath)
          LenBuff = LEN_TRIM(Buffer)
          JobPath = JobPath(1:LENG)//' '//Buffer(1:LenBuff)
          Buffer = ''
        END DO
      END IF
c	JobPath = 'c:\mysoftware\VB Projects\PSD\'
      LENG = LEN_TRIM(JobPath)
c      write(*,*)JobPath(1:LENG)
C        resp = MESSAGEBOXQQ(JobPath(1:LENG)//CHAR(0),
C     * 'Path for WRTAB'C,MB$OK)

c Open a diagnostic file
C      Diagfile = 13
C      OPEN(13, FILE = JobPath(1:LENG)//"DIAGPSD.TXT")
C      WRITE(13,*)JobPath(1:LENG)
C      response = COMMITQQ(DiagFile)


C Get the parameters to define the coordinate system
      OPEN(31, FILE = JobPath(1:LENG)//'SIZGRAPH.OUT')
      READ(31,*) N_DATA_SETS
      READ(31,*) (SELECTED_ID(I), I = 1,N_DATA_SETS)
      READ(31,*) SIZE_UNIT
      LABEL = SIZE_LABEL(SIZE_UNIT)
      READ(31,*) COORDINATES,LineType
      READ(31,*) TRUNCATE,TRUNCSIZE
      DO I = 1,N_DATA_SETS
        READ(31,'(10X,I4,1X,I4,A30)')
     *  SET_ID(I), N_POINTS(I), DATA_SET_NAME(I)
        NN = 0
        DO N = 1,N_POINTS(I)
          NN = NN +1
          READ(31,*) WSMESH, WFR
c  Dont record values larger than 0.999 for RR Log-normal or Logistic
          IF(COORDINATES .GE. 4 .AND. WFR .GT. 0.999) THEN
            NN = NN - 1
          END IF
          SMESH(NN,I) = WSMESH*10**SIZE_UNIT
          FR(NN,I) = WFR*100
        END DO
        N_POINTS(I) = NN
      END DO

c Get the upper and lower values of the ranges
      SIZE_MIN = 1.0E20
      SIZE_MAX = 0.0
      CUM_MIN = 1.0E20
      CUM_MAX = 0.0
      DO I = 1,N_DATA_SETS
        IF(SMESH(N_POINTS(I)-1,I) .LT. SIZE_MIN)
     *     SIZE_MIN = SMESH(N_POINTS(I)-1,I)
        IF(SMESH(1,I) .GT. SIZE_MAX) SIZE_MAX = SMESH(1,I)
        IF(FR(N_POINTS(I)-1,I) .LT. CUM_MIN)
     *     CUM_MIN = FR(N_POINTS(I)-1,I)
        IF(FR(1,I) .GT. CUM_MAX) CUM_MAX = FR(1,I)
      END DO
      XOL = 0.0
      XFL = ULIM(SIZE_MAX)
      IF(TRUNCATE .EQ. 1) THEN
        IF(SIZE_MAX .GE. TRUNCSIZE) THEN
          NPOINTS = NPOINTS - 1
          XFL = 0.9*TRUNCSIZE
          SIZE_MAX = 0.9*TRUNCSIZE
        END IF
      END IF
      CALL LOGLIM(SIZE_MIN,SIZE_MAX,XO,XF,NDECX)
      IF(TRUNCATE .EQ. 1 .AND. XF .GE. TRUNCSIZE) THEN
        XF = 0.9*TRUNCSIZE
      END IF
      YOL = 0.0
      YFL = 100.0
      CALL LOGLIM(CUM_MIN,CUM_MAX,YO,YF,NDECY)
      IYPOS = 590

      CALL WRTAB(JobPath)
      write(1,*)' Truncate,TRUNCSIZE,XOL,XFL',Truncate,TRUNCSIZE,XOL,XFL
      WRITE(1,*)' size_max',SIZE_MAX
      WRITE(1,*)' COORDINATES',COORDINATES
      CALL DISPLY(6)

      SELECT CASE(COORDINATES)
C
C  DRAW ON LINEAR LINEAR COORDINATES
      CASE (1)
      CALL BBOX(150,100,500,500)
      IF(TRUNCATE .EQ. 0) THEN
        CALL LINAX(XOL,XFL,10,5.2,1,'Y','Y')
      ELSE
        CALL TRUNCLINAX(XOL,XFL,10,5.2,1,'Y','Y')
      END IF
      CALL LABAX('Particle size',13,LABEL,6,13,1)
      CALL LINAX(YOL,YFL,10,3.0,2,'Y','Y')
      CALL LABAX('Percent passing',15,' ',1,13,2)
      CALL LBSIZE(10,20)
      DO I = 1,N_DATA_SETS
        CALL CLIPBOX
        SELECT CASE(LineType)
          CASE (1)
          IF(TRUNCATE .EQ. 0) THEN
            CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I),LINTR,LINTR)
          ELSE
            CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I),TRUNCLIN,LINTR)
          END IF
          CASE (2)
          IF(TRUNCATE .EQ. 0) THEN
            CALL PLSMGR(SMESH(1,I),FR(1,I),N_POINTS(I),0,LINTR,LINTR)
          ELSE
            CALL PLSMGR(SMESH(1,I),FR(1,I),N_POINTS(I),0,TRUNCLIN,LINTR)
          END IF
        END SELECT
        IF(TRUNCATE .EQ. 0) THEN
          CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I),I+20,LINTR,LINTR)
        ELSE
         CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I),I+20,TRUNCLIN,LINTR)
        END IF
        CALL CLIPOFF
        CALL IDENT(I,IYPOS)
      END DO
      CALL MGMOVE(100,650)
      IF(TRUNCATE .EQ. 0 ) THEN
        CALL LABELC('Linear-linear coordinate system',31)
      ELSE
        CALL LABELC('Truncated linear-linear coordinate system',41)
      END IF
      CALL GROUTCOORD
      CALL HDCOPY(1.0)
C
C  DRAW ON LINEAR-LOG COORDINATES
      CASE (2)
      CALL DISPLY(3)
      CALL BBOX(150,100,500,500)
      IF(TRUNCATE .EQ. 0) THEN
        CALL LGAXIS(XO,XF,1,'Y','Y')
      ELSE
        CALL TRUNCLOGAX(XO,XF,1,'Y','Y')
      END IF
      CALL LABAX('Particle size',13,LABEL,6,13,1)
      CALL LINAX(YOL,YFL,10,3.0,2,'Y','Y')
      CALL LABAX('Percent passing',15,' ',1,13,2)
      CALL LBSIZE(10,20)
      DO I = 1,N_DATA_SETS
        SELECT CASE(LineType)
          CASE (1)
          IF(TRUNCATE .EQ. 0) THEN
            CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I)-1,ALN,LINTR)
          ELSE
            CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I)-1,TRUNCLOG,LINTR)
          END IF
          CASE (2)
          CALL PLSMGR(SMESH(1,I),FR(1,I),N_POINTS(I)-1,0,ALN,LINTR)
        END SELECT
        IF (TRUNCATE .EQ. 0) THEN
          CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I)-1,I+20,ALN,LINTR)
        ELSE
          CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I)-1,I+20,
     *    TRUNCLOG,LINTR)
        END IF
        CALL IDENT(I,IYPOS)
      END DO
      CALL MGMOVE(100,650)
      IF(TRUNCATE .EQ. 0 ) THEN
        CALL LABELC('Linear-log coordinate system',28)
      ELSE
        CALL LABELC('Truncated linear-log coordinate system',38)
      END IF
      CALL GROUTCOORD
      CALL HDCOPY(1.0)
C
C  DRAW ON LOG-LOG COORDINATES
      CASE (3)
      CALL DISPLY(3)
      CALL BBOX(150,100,500,500)
      CALL LGAXIS(XO,XF,1,'Y','Y')
      CALL LABAX('Particle size',13,LABEL,5,13,1)
      CALL LGAXIS(YO,YF,2,'Y','Y')
      CALL LABAX('Percent passing',15,' ',1,13,2)
      CALL LBSIZE(10,20)
      DO I = 1,N_DATA_SETS
        SELECT CASE(LineType)
          CASE (1)
          CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I)-1,ALN,ALN)
          CASE (2)
          CALL PLSMGR(SMESH(1,I),FR(1,I),N_POINTS(I)-1,0,ALN,ALN)
        END SELECT
        CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I)-1,I+20,ALN,ALN)
        CALL IDENT(I,IYPOS)
      END DO
      CALL MGMOVE(100,650)
      CALL LABELC('Log-log coordinate system',25)
      CALL GROUTCOORD
      CALL HDCOPY(1.0)
C
C  DRAW ON ROSIN-RAMMLER COORDINATES
      CASE (4)
      CALL DISPLY(3)
      CALL BBOX(150,100,500,500)
      CALL LGAXIS(XO,XF,1,'Y','Y')
      CALL LABAX('Particle size',13,LABEL,5,13,1)
      CALL RRAXIS(YO,YF,2,'Y','Y')
      CALL LABAX('Percent passing',15,' ',1,13,2)
      CALL LBSIZE(10,20)
      DO I = 1,N_DATA_SETS
        SELECT CASE(LineType)
          CASE (1)
          CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I)-1,ALN,LLRC)
          CASE (2)
          CALL PLSMGR(SMESH(1,I),FR(1,I),N_POINTS(I)-1,0,ALN,LLRC)
        END SELECT
        CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I)-1,I+20,ALN,LLRC)
        CALL IDENT(I,IYPOS)
      END DO
      CALL MGMOVE(100,650)
      CALL LABELC('Rosin-Rammler coordinate system',31)
      CALL GROUTCOORD
      CALL HDCOPY(1.0)
C
C  DRAW ON LOG-NORMAL COORDINATES
      CASE(5)
      CALL DISPLY(3)
      CALL BBOX(150,100,500,500)
      CALL LGAXIS(XO,XF,1,'Y','Y')
      CALL LABAX('Particle size',13,LABEL,5,13,1)
      CALL PROBAX(YO,YF,2,'Y','Y')
      CALL LABAX('Percent passing',15,' ',1,13,2)
      CALL LBSIZE(10,20)
      DO I = 1,N_DATA_SETS
        SELECT CASE(LineType)
          CASE (1)
          CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I)-1,ALN,GAUINV)
          CASE (2)
          CALL PLSMGR(SMESH(1,I),FR(1,I),N_POINTS(I)-1,0,ALN,GAUINV)
        END SELECT
        CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I)-1,I+20,ALN,GAUINV)
        CALL IDENT(I,IYPOS)
      END DO
      CALL MGMOVE(100,650)
      CALL LABELC('Log-normal coordinate system',28)
      CALL GROUTCOORD
      CALL HDCOPY(1.0)
C
C  DRAW ON LOGISTIC COORDINATES
      CASE (6)
      CALL DISPLY(3)
      CALL BBOX(150,100,500,500)
      CALL LGAXIS(XO,XF,1,'Y','Y')
      CALL LABAX('Particle size',13,LABEL,5,13,1)
      CALL GISAXIS(YO,YF,2,'Y','Y')
      CALL LABAX('Percent passing',15,' ',1,13,2)
      CALL LBSIZE(10,20)
      DO I = 1,N_DATA_SETS
        SELECT CASE(LineType)
          CASE (1)
          CALL PLGRPH(SMESH(1,I),FR(1,I),N_POINTS(I)-1,ALN,LOGIST)
          CASE (2)
          CALL PLSMGR(SMESH(1,I),FR(1,I),N_POINTS(I)-1,0,ALN,LOGIST)
        END SELECT
        CALL DISPNT(SMESH(1,I),FR(1,I),N_POINTS(I)-1,I+20,ALN,LOGIST)
        CALL IDENT(I,IYPOS)
      END DO
      CALL MGMOVE(100,650)
      CALL LABELC('Logistic coordinate system',26)
      CALL GROUTCOORD
      CALL HDCOPY(1.0)

      END SELECT
      CALL CLOSE
      END

      SUBROUTINE IDENT(I,IYPOS)
C     *************************
C Set the identification labels
      INTEGER SET_ID(30)
      CHARACTER*30 DATA_SET_NAME(30)
      COMMON DATA_SET_NAME, SET_ID
      CALL SYMBOL(670,IYPOS,I+20)
      CALL MGMOVE(685,IYPOS - 5)
      CALL NUMLAB(SET_ID(I))
      CALL LABELC(DATA_SET_NAME(I),30)
      IYPOS = IYPOS - 25
      RETURN
      END
