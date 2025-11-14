      PROGRAM THICKEN_CONSTRUCT
C     *************************
C Draws the flux curve and operating line for a Kynch thickener.
      USE MSFLIB
      REAL ALPHA(10),BETA(10)
      REAL C(0:100),PSI(0:100)
      CHARACTER*36 TITLE
      LOGICAL OvLoad
      REAL LINTR
      EXTERNAL LINTR
      INTEGER*4 NoArgs
      INTEGER*2 Arg,status
      CHARACTER*256 JobPath
      CHARACTER*256 PathBuffer
      CHARACTER*10 BUFFER

c Get the job path name from the command line argument
      NoArgs = NARGS()
      IF(NoArgs .GT. 1) THEN
        Arg = 1
        IF(NoArgs .GT. 2) THEN
          !Blank spaces occured in one or more directory names in the path
          CALL GETARG(Arg,JobPath,status)
          DO I = 3,NoArgs-1
          !Blank spaces occured in one or more directory names in the path
            Arg = INT2(I-1)
            CALL GETARG(Arg,PathBuffer,status)
            LENG = LEN_TRIM(JobPath)
            LenBuff = LEN_TRIM(Buffer)
            JobPath = JobPath(1:LENG)//' '//Buffer(1:LenBuff)
            Buffer = ''
          END DO
        ELSE
          CALL GETARG(Arg,JobPath,status)
        END IF
        Arg = NoArgs-1
        CALL GETARG(Arg,BUFFER,status)
      END IF
      LENG = LEN_TRIM(JobPath)
      LENBUF = LEN_TRIM(BUFFER)

C Get the data for the construction from a file.
      OPEN(32,FILE =
     * JobPath(1:LENG)//'KYNCH'//BUFFER(1:LENBUF)//'.OUT')
      READ(32,'(A36)',END = 9999) TITLE
      LABLEN = LEN_TRIM(TITLE)
      READ(32,'(L1)',END = 9999) OvLoad
      READ(32,'(G10.4,I5)',END = 9999) Vt,NTERMS
      DO I = 1, NTERMS
        READ(32,'(2G10.4)',END = 9999) ALPHA(I),BETA(I)
      END DO
      READ(32,'(6G10.4)',END = 9999) PsiF,CI,PsiMax,Cm,Cd,CO
      CN = ULIM(Cd)
      YN = ULIM(PsiMax)
      DELC = CN/100.0
      DO I = 0,100
        C(I) = I*DELC
        PSI(I) = FLUX(C(I),ALPHA,BETA,NTERMS,Vt)
      END DO
      CLOSE(32)

      CALL WRTAB(JobPath)
      CALL LBSIZE(14,23)
      CALL MGMOVE(300,730)
      CALL LABELC(TITLE,LABLEN)
      CALL BBOX(150,100,800,600)
      CALL LINAX(0.0,CN,10,5.0,1,'Y','Y')
      CALL LINAX(0.0,YN,10,5.2,2,'Y','Y')
      CALL LABAX('Slurry concentration',20,'kg/m~3~',7,13,1)
      CALL LABAX('Settling flux',13,'kg/m~2~ s',9,13,2)
      CALL PLSMGR(C,PSI,101,1,LINTR,LINTR)
      C(0) = 0.0
      PSI(0) = PsiF
      C(1) = Cd
      PSI(1) = 0.0
      CALL PLGRPH(C,PSI,2,LINTR,LINTR)

c Plot the upper zone operating line if the thickener is overloaded.
      IF(OvLoad) Then
        C(0) = 0.0
        PSI(0) = 0.0
        C(1) = CO
        PSI(1) = Flux(CO,ALPHA,BETA,NTERMS,Vt)
        CALL PLGRPH(C,PSI,2,LINTR,LINTR)
        CALL MGMOVE(500,600)
        CALL LBSIZE(13,23)
        CALL LABELC('The thickener is overloaded',27)
      END IF

      CALL GROUT
      CALL HDCOPY(1.0)
      CALL CLOSE
      CALL EXIT(-1)

 9999 CALL EXIT(1)
      END

      FUNCTION Flux(C,ALPHA,BETA,NTERMS,Vt)
C     *************************************
C Calculates the flux.
      REAL ALPHA(*),BETA(*)
      WS1 = 1.0
      DO I = 1,NTERMS
        WS1 = WS1 + ALPHA(I)*C**BETA(I)
      END DO
      Flux = Vt*C/WS1
      RETURN
      END
