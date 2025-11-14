      FUNCTION LJUBAMD(JobPath, JobName)
C     **********************************
C Calculation of the Andrews-Mika diagram using the Ljubljana model.
c Size normalization is not assumed.

      USE PORTLIB
      USE MSFLIB
c This function must be exported from the DLL
      !MS$ATTRIBUTES STDCALL, DLLEXPORT :: LJUBAMD
      !MS$ATTRIBUTES ALIAS :'LJUBAMD' :: LJUBAMD
      INTEGER*4 LJUBAMD
      CHARACTER*255 JobPath
      CHARACTER*255 JobName
      !MS$ATTRIBUTES REFERENCE :: JobPath
      !MS$ATTRIBUTES REFERENCE :: JobName
      INTEGER*2 Bvg(35,22,35,22)
      CHARACTER*80 WSTRING
      CHARACTER*4 KEY
      REAL SIZE(35)
      REAL GRDMV(22,7)
      REAL GRDM(22,7)
      REAL SPGR(10)
      REAL LIB1,LIB2
      CHARACTER*4 MASSVOL
      LOGICAL GOLDFL
      INTEGER INDPP(50,2)
      REAL PPROP(1000)
      REAL PROP(22)
      CHARACTER*4 NAMES(10)
      INTEGER DiagFile

      LJUBAMD = -1
c Open a file for diagnostic output
      LENG = LEN_TRIM(JobPath)
      DiagFile = 13
      OPEN(DiagFile,FILE = JobPath(1:LENG)//'diagljub.txt',ERR = 9993)
      WRITE(DiagFile,*)'Starting to calculate "Ljubljana" Andrews-Mika d
     *iagram"'
C Open file CURRDATA.SYD to get the necessary system data
      OPEN(43, FILE = JobPath(1:LENG)//'CURRDATA.SYD', ERR = 9991)
      GOLDFL = .FALSE.

C Read the data
      NPP = 0
      NPPC = 0
1000  FORMAT(A4,4I4)
3002  FORMAT(8G10.4)
      READ(43,'(A11)',END = 9992,ERR = 9999) WSTRING
      IF(WSTRING .NE. 'SYSTEM DATA') THEN
        LJUBAMD = 114
        CLOSE (43)
        RETURN
      END IF
      READ(43,'(A80)',END = 9992,ERR = 9999) WSTRING
      LEN1 = LEN_TRIM(WSTRING)
      LEN2 = LEN_TRIM(JobName)
      IF(WSTRING(1:LEN1) .NE. JobName(1:LEN2)) THEN
        WRITE(Diagfile,*)WSTRING(1:LEN1),' ',JobName(1:LEN2)
        LJUBAMD = 115
      END IF
      READ(43,1000,END = 9992,ERR = 9999)KEY,I1,I2,I3,I4
      DO WHILE (KEY .NE. '    ')

        SELECT CASE(KEY)
          CASE('NDGS')
            NDC = I2
            NGC = I3
            NSC = I4

          CASE('COAL')
            IF(I1 .EQ. 1) THEN
              ! Proximate analysis data is used
            END IF

          CASE('MINS')
            NOMIN = I1
            READ(43,'(A4)',END = 9992, ERR = 9999)(NAMES(I),I =1,NOMIN)

          CASE('SIZE')
            READ(43,3002,END = 9992, ERR = 9999) (SIZE(I),I=1,NDC)

          CASE('SPGR')
            READ(43,3002,END = 9992, ERR = 9999) (SPGR(I),I=1,NOMIN)

          CASE('GRDM')
            MASSVOL = 'GRDM'
            DO I = 1,NGC
              READ(43,3002,END = 9992, ERR = 9999)(GRDMV(I,J),J=1,NOMIN)
            END DO

          CASE('GRDV')
            MASSVOL = 'GRDV'
            DO I = 1,NGC
              READ(43,3002,END = 9992, ERR = 9999)(GRDMV(I,J),J=1,NOMIN)
            END DO

          CASE('PHYP')
            IF(I1 .GT. NPP) NPP=I1
            IF(I2 .LT. 1000) THEN
              INDPP(I1,1)=NPPC+1
              INDPP(I1,2)=I2
              NPP1=NPPC+1
              NPPC=NPPC+I2
              READ(43,3002,END = 9992, ERR = 9999)(PPROP(I),I=NPP1,NPPC)
            ELSE
              I2=I2-1000
              READ(43,3002,END = 9992, ERR = 9999) (PROP(I),I=1,NOMIN)
              DO I = 1,NGC
                M=NPPC+I
                PPROP(M)=0.0
                DO K = 1,NOMIN
                  IF(I2.EQ.1) PPROP(M)=PPROP(M)+PROP(K)*GRDMV(I,K)
                  IF(I2.EQ.3) PPROP(M)=PPROP(M)+PROP(K)*GRDMV(I,K)
                  IF(I2.EQ.2) PPROP(M)=PPROP(M)+GRDMV(I,K)/PROP(K)
                  IF(I2.EQ.4) PPROP(M)=PPROP(M)+GRDMV(I,K)/PROP(K)
                END DO
                IF(I2.EQ.2.OR.I2.EQ.4) PPROP(M)=1.0/PPROP(M)
              END DO
              INDPP(I1,1)=NPPC+1
              INDPP(I1,2)=NGC
              NPPC=NPPC+NGC
            END IF

        END SELECT

        READ(43,1000,END = 9992, ERR = 9999) KEY,I1,I2,I3,I4
      END DO
      CLOSE (43)
	write(13,*)MASSVOL
	do I = 1,12
      write(13,*)(GRDMV(I,J),J=1,NOMIN)
	end do
      WRITE(Diagfile,*)'Data input complete'


      PHIA =PPROP(INDPP(2,1))

C The Ljubljana model can handle only 2 minerals
      IF(NOMIN .NE. 2) THEN
        LJUBAMD = 110
        RETURN
      END IF
C ...and must have at least 5 grade classes.
      IF(NGC .LT. 5 .OR. NGC .GT. 12) THEN
        LJUBAMD = 111
        RETURN
      END IF

C Open a file to receive the calculated A-M diagram
      OPEN(42,FILE = JobPath(1:LENG)//'LJUBAMD.DAT',ERR = 9990)

c Initialize the A-M diagram for every progeny size
      DO K = 1,NDC
        DO L = 1,NGC
          DO I = 1,NDC
            DO J = 1,NGC
              Bvg(I,J,K,L) = 0.0
            END DO
          END DO
        END DO
      END DO


      IF(MASSVOL .EQ. 'GRDM') THEN
        DO J = 1,NGC
          GRDM(J,1) = GRDMV(J,1)
          GRDM(J,2) = GRDMV(J,2)
        END DO
      END IF
      IF(MASSVOL .EQ. 'GRDV') THEN
        DO J = 1,NGC
          SPMASS = GRDMV(J,1)*SPGR(1) + GRDMV(J,2)*SPGR(2)
	  GRDM(J,2) = GRDMV(J,2)*SPGR(2)/SPMASS
	  GRDM(J,1) = GRDMV(J,1)*SPGR(1)/SPMASS
        END DO
      END IF

      SIGRDM = 0.0
      DO 15 J = 3,NGC-2
        SIGRDM = SIGRDM+GRDM(J,1)
   15 CONTINUE

      DO 50 K = 1,NDC
      DO 40 L = 1,NGC
        DO 30 I = K+1,NDC
          IF(L .GT. 1 .AND. L .LT. NGC) THEN
            IF(GOLDFL) THEN
              CALL FRLIB_AU(SIZE(I),LIB1,LIB2,GRDM(L,1),
     *        SIZE(NDC))
            ELSE
              CALL FRLIB(SIZE(I),SIZE(K),LIB1,LIB2,GRDM(L,1),PHIA)
            END IF
          END IF
C
C  TRANSFER OUT OF LIBERATED CLASSES CAN GO ONLY TO A LIBERATED CLASS
          IF(L.EQ.1) THEN
            Bvg(I,1,K,L) = 10000
            DO J = 2,NGC
              Bvg(I,J,K,L) = 0
            END DO
          ELSE IF(L .EQ. NGC) THEN
            DO J = 1,NGC-1
              Bvg(I,J,K,L) = 0
            END DO
            Bvg(I,NGC,K,L) = 10000
          ELSE IF(LIB1 .GE. 0.00001 .OR. LIB2 .GE. 0.00001) THEN
C
C  CALCULATE TRANSFERS TO LIBERATED CLASSES
            IF(GRDM(1,1) .LT. GRDM(1,2)) THEN
              BL1=LIB2*GRDM(L,2)
              BL12=LIB1*GRDM(L,1)
            ELSE
              BL12=LIB2*GRDM(L,2)
              BL1=LIB1*GRDM(L,1)
            END IF
C
C  DISTRIBUTE THE REST TO THE INTERMEDIATE CLASSES
            WS=1.0-BL1-BL12
            BRL=MIN(GRDM(L,1),GRDM(L,2))/FLOAT(NGC-4)
            A1=WS-BRL*(NGC-4)*WS
            A2=GRDM(L,1)-BL1*GRDM(1,1)-BL12*GRDM(NGC,1)-
     &         BRL*WS*SIGRDM
            BL11=(A2-A1*GRDM(2,1))/(GRDM(NGC-1,1)-GRDM(2,1))
            BL2=A1-BL11
            Bvg(I,1,K,L)=10000*BL1
            Bvg(I,2,K,L)=10000*BL2
            DO J = 3,NGC-2
              Bvg(I,J,K,L)=NINT(BRL*(1.0-BL1-BL12)*10000.0)
            END DO
            Bvg(I,NGC-1,K,L) = NINT(BL11*10000)
            Bvg(I,NGC,K,L) = NINT(10000*BL12)
          ELSE
            BLL = 0.6
            WS = 1.0/(GRDM(L+1,1)-GRDM(L-1,1))
            F1 = (GRDM(L+1,1)-GRDM(L,1))*WS
            F2 = (GRDM(L,1)-GRDM(L-1,1))*WS
            DO J = 1,NGC
              Bvg(I,J,K,L) = 0
            END DO
            Bvg(I,L-1,K,L) = NINT(10000*F1*(1.0-BLL))
            Bvg(I,L,K,L) = NINT(10000*BLL)
            Bvg(I,L+1,K,L) = NINT(10000*F2*(1.0-BLL))
          END IF
          WRITE(42,'(42I6)')(Bvg(I,J,K,L),J = 1,NGC)
	IF(I.EQ.K+1)write(13,*)'BL1,LIB1,BL12,LIB2',BL1,LIB1,BL12,LIB2
          IF(I.EQ.K+1)WRITE(13,'(42I6)')(Bvg(I,J,K,L),J = 1,NGC)
   30   CONTINUE
        WRITE(42,*)
   40 CONTINUE
   50 CONTINUE
      CLOSE(42)
      WRITE(DiagFile,*)'Terminating normally'
      CLOSE(DiagFile)
      RETURN

 9990 CONTINUE
      WRITE(DiagFile,*)'Error opening file LJUBAMD.DAT'
      LJUBAMD = 112
      CLOSE(DiagFile)
      RETURN
 9991 CONTINUE
      WRITE(DiagFile,*)'Error opening file CURRDATA.SYD'
      LJUBAMD = 113
      CLOSE(DiagFile)
      RETURN
 9992 CONTINUE
      WRITE(DiagFile,*)'End of file CURRDATA.SYD'
      CLOSE (43)
      CLOSE(DiagFile)
      LJUBAMD = 116
      RETURN
 9993 CONTINUE
      LJUBAMD = 118
      RETURN
 9999 CONTINUE
      WRITE(DiagFile,*)'Error reading file CURRDATA.SYD'
      LJUBAMD = 117
      CLOSE(43)
      CLOSE(DiagFile)
      RETURN
      END

      SUBROUTINE FRLIB(SIZE2,SIZE1,LIB1,LIB2,GRDMJ1,PHIA)
C     ***************************************************
C  CALCULATE THE FRACTIONAL LIBERATION OF EACH PHASE.
C  MODEL IS BASED ON THE SIMULATED FRACTURE OF PARGEN PARTICLES.
C  Model modified following more extensive PARGEN simulation with larger
C  particles  see reference 2 below.
C  REFERENCES: 1 Lin CL, Cortes A, King RP, Miller JD, Breakage character-
C              istics of multiphase PARGEN particles as described by
C              computer simulation.  PROCESS MINERALOGY VIII, The Metal-
C              lurgical Society, (1988) p195.
C              2 King RP and Schneider CL  Calculation of mineral liberation
C              in a continuously operating ball mill.
      REAL LIB1,LIB2
      CDPSVAB = 4.5
c      CDPSVBA = 4.5
      CDPSVBA = 9.8
      LIB1=0.0
      LIB2=0.0
      DPSVAB=PHIA*(1.0-GRDMJ1)
      DPSVBA=PHIA*GRDMJ1
      IF(DPSVAB.LE.0.0) THEN
        LIB1=1.0
      ELSE
        ADISP=EXP(-0.15404*CDPSVAB)
        ADISP=ADISP/(1.0-ADISP)
        X=DPSVAB*SIZE2/SIZE1
        IF(X.LT.CDPSVAB) LIB1=(1.0+ADISP)*EXP(-0.15404*X)-ADISP
      END IF
      IF(DPSVBA.LE.0.0) THEN
        LIB2=1.0
      ELSE
        ACONT=EXP(-0.15404*CDPSVBA)
        ACONT=ACONT/(1.0-ACONT)
        X=DPSVBA*SIZE2/SIZE1
        IF(X.LT.CDPSVBA) LIB2=(1.0+ACONT)*EXP(-0.15404*X)-ACONT
      END IF
      RETURN
      END


      SUBROUTINE FRLIB_AU(SIZE2,LIB1,LIB2,GRDMJ1,SIZEN)
C     *************************************************
C  CALCULATE THE FRACTIONAL LIBERATION OF EACH PHASE.
C  MODEL IS BASED ON A SIMPLE MODEL FOR THE LIBERATION OF
C  GRAVITY-RECOVERABLE GOLD
C  This was developed for Mosquito Gold Corporation.  April 1997

      REAL LIB1,LIB2
      LIB2 = 0.9*((SIZEN/SIZE2)**0.1)
      F = 5
      LIB1 = ( F*GRDMJ1 - F + 1)/GRDMJ1
      RETURN
      END
