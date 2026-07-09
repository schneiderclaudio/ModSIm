C*******************     GENLSUBS.FOR     *****************************C
C                                                                      C
C          MODSIM   MODULAR SIMULATOR FOR ORE DRESSING PLANTS          C
C                                                                      C
C  THIS FILE CONTAINS GENERAL UTILTY ROUTINES                          C
C                                                                      C
C  MODSIM  (C) RP KING  JOHANNESBURG  1985                             C
C                                                                      C
C**********************************************************************C

      SUBROUTINE MATHERRQQ(name,length,info,retcode)
      !*********************************************
C Error handling for run time FORTRAN errors
      USE MSFLIB
c      USE DIALOGM
c      INCLUDE 'DIALOGMessage.fd'
c      TYPE (dialog)dlg
c      LOGICAL return
c      INTEGER retint

      INTEGER(2) length, retcode
      CHARACTER(length) name
C     IFX does not expose MTH$E_INFO; declare info as a dummy integer array
      INTEGER(4) info(16)

c     return = DLGINIT(IDD_DIALOGMessage, dlg)
c
c     !Activate the message dialog
c     retint = DlgModal(dlg)
c
c     !Deactivate the message dialog
c     CALL DlgUnInit(dlg)

C     IF(info.ftype .EQ. TY$REAL8) THEN
C       !Double precision variable
C     END IF
      IF(name .EQ. ' ') THEN
      END IF

C      write(*,*)' In run time error handler'

      retcode = 18
      END
C
      SUBROUTINE SETUP
C     ****************
C  SETS UP THE DATA IN TAB FOR INTERNAL REPRESENTATION OF ASCII
C  CHARACTERS.  THIS SUBROUTINE IS CALLED ONLY IN PROGRAMS THAT REQUIRE
C  SUBROUTINES IN THIS FILE AND DONT USE THE GRAPHICS SUBROUTINES.
C
      INTEGER TAB(128)
      COMMON /TEK/TAB
      CHARACTER*6 CODE
C
C  IDENTIFY CHARACTER REPRESENTATION CODE.  ASCII OR EBCDIC.
      IF(ICHAR('A').EQ.65)  CODE='ASCII'
      IF(ICHAR('A').EQ.193) CODE='EBCDIC'
      IF(CODE.EQ.'ASCII') THEN
        DO 10 I=1,128
          TAB(I)=I
   10   CONTINUE
      ELSEIF (CODE.EQ.'EBCDIC') THEN
        CALL ASCEBC(TAB)
      ELSE
        WRITE(13,*)'METGRAPH does not recognize internal character code'
        STOP
      END IF
      RETURN
      END
C
      SUBROUTINE ASCEBC(TAB)
C     **********************
C  SETS UP THE ASCII-EBCDIC TRANSLATION TABLE
C
      INTEGER TAB(128),EBCD(127)
      DATA EBCD/1,2,3,55,45,46,47,22,5,37,11,12,13,14,15,16,17,18,19,60,
     &61,50,38,24,25,63,39,28,29,30,31,64,90,127,123,91,108,80,125,77,
     &93,92,78,107,96,75,97,240,241,242,243,244,245,246,247,248,249,122,
     &94,76,126,110,111,124,193,194,195,196,197,198,199,200,201,209,210,
     &211,212,213,214,215,216,217,226,227,228,229,230,231,232,233,173,
     &224,189,95,109,121,129,130,131,132,133,134,135,136,137,145,146,
     &147,148,149,150,151,152,153,162,163,164,165,166,167,168,169,192,
     &106,208,161,7/
      DO 10 I=1,127
        TAB(I)=EBCD(I)
   10 CONTINUE
      RETURN
      END
C
      SUBROUTINE FORMT(I,IDIG,NUM)
C     ****************************
C  THIS SUBROUTINE FORMATS THE INTEGER I AND RETURNS
C  THE ASCII EQUIVALENT IN NUM WHICH HAS IDIG CHARACTERS
      INTEGER TAB(128)
      COMMON /TEK/TAB
      INTEGER NUM(10)
      IF(I.EQ.0)IDIG=1
      II=I
      IF(I.LT.0)II=-I
      ZI= II
      IF(II.GT.0)IDIG=IFIX(ALOG10(ZI)+0.00001) + 1
      DO 10 K=1,IDIG
      J = 10**(IDIG-K)
      N=II/J
      NUM(K)=TAB(48+N)
   10 II = II-N*J
      IF(I.GE.0) GO TO 30
      IDIG=IDIG+1
      DO 40 K=2,IDIG
      KK =IDIG+1-K
      NUM(KK+1)=NUM(KK)
   40 CONTINUE
      NUM(1)=TAB(45)
   30 RETURN
      END
C
C
      SUBROUTINE LABRDC(LABEL,LEN,LU,EREND)
C     *************************************
C
C     READS A LABEL IN CHARACTER MODE ON LOGICAL UNIT LU AND RETURNS IT.
C     THE LABEL LENGTH IS RETURNED IN LEN
C     EREND IS SET TRUE IF AN END-OF-FILE IS READ.
C
      LOGICAL EREND
      CHARACTER*1 LABEL(80),BL
      DATA BL/' '/
      EREND=.FALSE.
      READ (LU,101,END=9999)(LABEL(I),I=1,80)
  101 FORMAT(80 A1)
      LEN = 0
      DO 10 I=1,80
        IF(ICHAR(LABEL(I)).NE.ICHAR(BL)) LEN=I
   10 CONTINUE
      RETURN
 9999 EREND=.TRUE.
      RETURN
      END
C
      SUBROUTINE INSRTC(OP,NROWS,WORD,NC,IROW,ICOL)
C     *********************************************
C  INSERTS A WORD OF NC CHARACTERS INTO ARRAY OP WHICH HAS NROWS AND 80
C  COLUMNS. WORD IS RIGHT JUSTIFIED AT COLUMN ICOL.
      CHARACTER*1 OP(NROWS,156),WRD(80)
      CHARACTER*80 WORD,WORDWS
      EQUIVALENCE (WORDWS,WRD(1))
      WORDWS=WORD
      DO 10 I=1,NC
      IS=ICOL-NC+I
      OP(IROW,IS)=WRD(I)
   10 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE INSERT (OP,NROWS,NUM,ROW,COL)
C     ****************************************
C     INSERTS INTEGER NUMBER NUM AS A CHARACTER STRING INTO CHARACTER
C     ARRAY OP WHICH HAS NROWS ROWS.THE CHARACTER STRING IS INSERTED
C     IN ROW ROW AND IS RIGHT JUSTIFIED AT COLUMN COL.
C
      INTEGER ROW,COL,FNUM(10)
      CHARACTER*1 OP(NROWS,156)
      CHARACTER*1 CHAR
      CALL FORMT(NUM,ICH,FNUM)
      DO 800 I=1,ICH
      IS=COL-ICH+I
      OP(ROW,IS)=CHAR(FNUM(I))
  800 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE INSRTR(OP,NROWS,X,F,ROW,COL)
C     ***************************************
C     INSERTS A REAL NUMBER X AS A CHARACTER STRING USING FORMAT F INTO
C     CHARACTER ARRAY OP WHICH HAS NROWS AND 80 COLUMNS.THE CHARACTER
C     STRING IS INSERTED IN ROW ROW AND IS RIGHT JUSTIFIED AT COLUMN COL
C
      INTEGER ROW,COL,FNUM(80)
      CHARACTER*1 CHAR
      CHARACTER*1 OP(NROWS,156)
      CHARACTER*80 OUTPUT
      CHARACTER*7 FORM
      INTEGER COLUMN_WIDTH
      DATA FORM /'(G 7.2)'/

      COLUMN_WIDTH = INT(F)
      CALL FORMTR (X,F,FNUM,ICH)
      IF(ICH .GT. COLUMN_WIDTH) THEN
        WRITE(FORM(3:4),'(I2)') COLUMN_WIDTH
        WRITE(OUTPUT,FORM) X
        DO I = 1,COLUMN_WIDTH
          IS=COL+I-COLUMN_WIDTH
          OP(ROW,IS) = OUTPUT(I:I)
        END DO
      ELSE
        DO I=1,ICH
          IS=COL-ICH+I
          OP(ROW,IS)=CHAR(FNUM(I))
        END DO
      END IF
      RETURN
      END
C
C
      SUBROUTINE MCLEAR(OP,IROWS)
C     ***************************
C  CLEARS THE LOGICAL ARRAY OP,WHICH IROWS ROWS, TO SPACES.
C
      CHARACTER*1 OP(IROWS,156)
      CHARACTER*1 SPACE
      DATA SPACE/' '/
      DO 10 I=1,IROWS
      DO 10 J=1,156
      OP(I,J)=SPACE
   10 CONTINUE
      RETURN
      END
C
C
      SUBROUTINE FORMTR(XX,F,IDENT,ICHR)
C     **********************************
C   THIS SUBROUTINE FORMATS REAL VARIABLE X ACCORDING TO  F  FORMAT
C   AND RETURNS THE ASCII EQUIVALENT IN  IDENT WHICH HAS ICHR CHARACTER
C
      INTEGER  IDENT(20),NUM(10)
      ICHR=0
      X=XX
      IF(X.LT.0.0) THEN
        X=-X
        IDENT(1)=ICHAR('-')
        ICHR=1
      END IF
   20 CONTINUE
      J=IFIX(F*10-IFIX(F)*10+0.001)
      X=IFIX(X*(10**J)+0.5)
      X=X/(10**J)
C
C  INTEGRAL PART
C  *************
      II=IFIX(X)
      CALL FORMT(II,IDIG,NUM)
      DO 30 I=1,IDIG
      ICHR=ICHR+1
      IDENT(ICHR)=NUM(I)
   30 CONTINUE
      IF(J.EQ.0) GO TO 41
C
C  DECIMAL INDICATOR
C  *****************
      ICHR=ICHR+1
      IDENT(ICHR)=ICHAR('.')
C
C  FRACTIONAL PART
C  ***************
      XFRACT=X-IFIX(X)
      II=IFIX(XFRACT*(10**J)+0.5)
      CALL FORMT(II,IDIG,NUM)
      IF(IDIG.GE.J) GO TO 45
      II=J-IDIG
      DO 44 I=1,II
      ICHR=ICHR+1
      IDENT(ICHR)=ICHAR('0')
   44 CONTINUE
   45 CONTINUE
      DO 40 I=1,IDIG
      ICHR=ICHR+1
      IDENT(ICHR)=NUM(I)
   40 CONTINUE
   41 CONTINUE
C
C  RIGHT JUSTIFY
C  *************
      J=IFIX(F)
      IF(ICHR.GE.J) GO TO 60
      DO 50 I=1,J
      JJ=J-I+1
      II=ICHR-I+1
      IF(II.GE.1) IDENT(JJ)=IDENT(II)
      IF(II.LT.1) IDENT(JJ)=ICHAR(' ')
   50 CONTINUE
      ICHR=J
   60 RETURN
      END
