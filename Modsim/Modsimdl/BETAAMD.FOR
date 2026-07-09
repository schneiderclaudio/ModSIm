      FUNCTION BETAAMD(JobPath, JobName,Parameters)
C     *********************************************
C Calculation of the Andrews-Mika diagram using the beta function model.
c Size normalization is not assumed.

      USE PORTLIB
      USE MSFLIB
c This function must be exported from the DLL
      !MS$ATTRIBUTES STDCALL, DLLEXPORT :: BETAAMD
      !MS$ATTRIBUTES ALIAS :'BETAAMD' :: BETAAMD
      INTEGER*4 BETAAMD
      CHARACTER*255 JobPath
      CHARACTER*255 JobName
      !MS$ATTRIBUTES REFERENCE :: JobPath
      !MS$ATTRIBUTES REFERENCE :: JobName
      REAL*4 Parameters(4)
      !MS$ATTRIBUTES REFERENCE :: Parameters
      INTEGER*2 Bvg(35,12,35,12)
      REAL BB(35,12,35)
      CHARACTER*80 WSTRING
      CHARACTER*4 KEY
      REAL SIZE(35)
      REAL GRDMV(12,10)
      REAL GRDM(12,10)
      REAL grade(12),CUMGRADE(0:12)
      REAL SPGR(10)
      CHARACTER*4 MASSVOL
      INTEGER INDPP(100,2)
      REAL PPROP(1000)
      REAL PROP(12)
      CHARACTER*4 NAMES(10)
      INTEGER DiagFile
      COMMON /Diagnostics/ Diagfile
      REAL LiberationSize
      COMMON /AMDParameters/ PowerFactor,PowerExponent,GammaFactor,
     *GammaExponent,AsymmFactor

      BETAAMD = -1
c Open a file for diagnostic output
      LENG = LEN_TRIM(JobPath)
      DiagFile = 13
      OPEN(DiagFile,FILE = JobPath(1:LENG)//'diagbeta.txt',ERR = 9993)
      WRITE(DiagFile,*)'Starting to calculate "Beta function" Andrews-Mi
     *ka diagram'

C Open file CURRDATA.SYD to get the necessary system data
      OPEN(43, FILE = JobPath(1:LENG)//'CURRDATA.SYD', ERR = 9991)

      WRITE(Diagfile,'(''Breakage function parameters'',4F10.4)')
     *(Parameters(I), I = 1,4)
C Read the data
      NPP = 0
      NPPC = 0
1000  FORMAT(A4,4I4)
3002  FORMAT(8G10.4)
      READ(43,'(A11)',END = 9992,ERR = 9999) WSTRING
      IF(WSTRING .NE. 'SYSTEM DATA') THEN
        BETAAMD = 114
        CLOSE (43)
        RETURN
      END IF
      READ(43,'(A80)',END = 9992,ERR = 9999) WSTRING
      LEN1 = LEN_TRIM(WSTRING)
      LEN2 = LEN_TRIM(JobName)
	Write(DiagFile,*)'WSTRING(1:LEN1)',WSTRING(1:LEN1)
	Write(DiagFile,*)'JobName(1:LEN2)',JobName(1:LEN2)
      IF(WSTRING(1:LEN1) .NE. JobName(1:LEN2)) THEN
        BETAAMD = 115
      END IF
      READ(43,1000,END = 9992,ERR = 9999)KEY,I1,I2,I3,I4
	Write(DiagFile,'(''KEY  '',A4,4I4)')KEY,I1,I2,I3,I4
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
	  Write(DiagFile,'(''KEY  '',A4,4I4)')KEY,I1,I2,I3,I4
      END DO
      CLOSE (43)
      WRITE(Diagfile,*)'Data input complete'
      CALL FLUSH(DiagFile)

CSet up the parameters for the A-M diagram model.
      PHIA =PPROP(INDPP(2,1))
      LiberationSize = 1.0e-6*PPROP(INDPP(2,1)+1)
      PrefBreakageFactor = PPROP(INDPP(2,1)+2)
      PowerFactor = PPROP(INDPP(2,1)+3)
      PowerExponent = PPROP(INDPP(2,1)+4)
      GammaFactor = PPROP(INDPP(2,1)+5)
      GammaExponent = PPROP(INDPP(2,1)+6)
      AsymmFactor = PPROP(INDPP(2,1)+7)
      write(Diagfile,'(
     * ''PHIA = '',G10.4,/,
     * ''LiberationSize = '',G10.4,/,
     * ''PrefBreakageFactor = '',G10.4,/,
     * ''PowerFactor = '',G10.4,/,
     * ''PowerExponent = '',G10.4,/,
     * ''GammaFactor = '',G10.4,/,
     * ''GammaExponent = '',G10.4,/,
     * ''AsymmFactor = '',G10.4)')
     *PHIA,LiberationSize,PrefBreakageFactor,PowerFactor,PowerExponent,
     *GammaFactor,GammaExponent,AsymmFactor
      CALL FLUSH(Diagfile)


C The model can handle only 2 minerals
      IF(NOMIN .NE. 2) THEN
        BETAAMD = 110
        RETURN
      END IF
C ...and must have at least 5 grade classes.
      IF(NGC .LT. 5 .OR. NGC .GT. 12) THEN
        BETAAMD = 111
        RETURN
      END IF

C Open a file to receive the calculated A-M diagram
      OPEN(42,FILE = JobPath(1:LENG)//'BETAAMD.DAT',ERR = 9990)

c Initialize the A-M diagram for every progeny size
      DO L = 1,NGC
        DO K = 1,NDC
          DO J = 1,NGC
            DO I = 1,NDC
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

C Calculate the grades at the boundaries of the grade classes to
C accommodate unequal class widths.
      Grade(1) = GRDM(1,2)
      CUMGRADE(0) = GRDM(1,2)
      CUMGRADE(1) = GRDM(1,2)
      CUMGRADE(NGC-1) = GRDM(NGC,2)
      CUMGRADE(NGC) = GRDM(NGC,2)
      Grade(NGC-1) = GRDM(NGC-1,2)
      Grade(NGC) = GRDM(NGC,2)
      DO J = 2,NGC-2
        Grade(J) = GRDM(J,2)
        CUMGRADE(J) = 0.5*(GRDM(J,2) + GRDM(J+1,2))
      END DO
      WRITE(DiagFile,'(''Cumulative grades'')')
      WRITE(Diagfile,'(12F6.3)') (CUMGRADE(J),J = 1,NGC)
      CALL FLUSH(Diagfile)

C Calculate the size breakage function
      BETA = Parameters(1)
      GAMMA = Parameters(2)
      DELTA = Parameters(3)
      PHI1 = Parameters(4)
      ! Parameters for Rolf Fandrich's breakage data.
      E = 7.4
      DO K = 1,NGC
c        MugE = exp(-(E/(3.02*Grade(K) + 8.58))**0.45)
c        sigma = (0.16*Grade(K) + 1.7)*(E/3.03)**0.074
        DO L = 1,NDC
          BB(L,K,L) = 0.0
          DO J = L+1,NDC
c            BB(J,K,L)=TruncatedLogNormal(SIZE(J-1),SIZE(L),MugE,Sigma)
            BB(J,K,L) = AUSTINBR(SIZE(J-1)/SIZE(L),GAMMA,BETA,PHI1)
            IF(J .LT. NDC) THEN
c              WS = TruncatedLogNormal(SIZE(J),SIZE(L),MugE,Sigma)
              WS = AUSTINBR(SIZE(J)/SIZE(L),GAMMA,BETA,PHI1)
              BB(J,K,L) = BB(J,K,L) - WS
            END IF
          END DO
        END DO
      END DO

c     Write the breakage function
      DO K = 1,NGC
        WRITE(Diagfile,'(''Breakage function for parent grade '',I3)')K
        DO J = 1,NDC
          WRITE(Diagfile,'(16F6.2)')(BB(J,K,L),L= 1,NDC)
        END DO
      END DO
      CALL FLUSH(Diagfile)

      WRITE(Diagfile,'(''Entering MAKEAMD'')')
      CALL FLUSH(Diagfile)

      CALL MAKEAMD(NDC,NGC,Bvg,Grade,SIZE,BB,LiberationSize,
     *PrefBreakageFactor,CUMGRADE)

      DO 50 L = 1,NDC
		DO 40 K = 1,NGC
			DO 30 J = L+1,NDC
				WRITE(42,'(42I6)')(Bvg(J,I,L,K),I = 1,NGC)
   30     CONTINUE
          WRITE(42,*)
   40   CONTINUE
   50 CONTINUE
      CLOSE(42)
      WRITE(DiagFile,*)'Terminating normally'
      CLOSE(DiagFile)
      RETURN

 9990 CONTINUE
      WRITE(DiagFile,*)'Error opening file BETAAMD.DAT'
      BETAAMD = 112
      CLOSE(DiagFile)
      RETURN
 9991 CONTINUE
      WRITE(DiagFile,*)'Error opening file CURRDATA.SYD'
      BETAAMD = 113
      CLOSE(DiagFile)
      RETURN
 9992 CONTINUE
      WRITE(DiagFile,*)'End of file CURRDATA.SYD'
      CLOSE (43)
      CLOSE(DiagFile)
      BETAAMD = 116
      RETURN
 9993 CONTINUE
      BETAAMD = 118
      RETURN
 9999 CONTINUE
      WRITE(DiagFile,*)'Error reading file CURRDATA.SYD'
      BETAAMD = 117
      CLOSE(43)
      CLOSE(DiagFile)
      RETURN
      END

      SUBROUTINE MakeAMD(NDC,NGC,A,Grade,SIZE,BB,LiberationSize,
     *Factor,CUMGRADE)
C     **********************************************************
C Calculation of the Andrews-Mika diagram using a Beta function model.
c Size normalization is not assumed.

      REAL BB(35,12,35),SIZE(35)
      INTEGER*2 A(35,12,35,12)
      REAL Grade(12),CUMGRADE(0:12)
      REAL AMB(12)
      REAL Vu(35,12,35)
      REAL LiberationSize
      INTEGER DiagFile
      COMMON /Diagnostics/ Diagfile

c Initialize the A-M diagram for every progeny size
      DO L = 1,NGC
        DO K = 1,NDC
          DO J = 1,NGC
            DO I = 1,NDC
              A(I,J,K,L) = 0
            END DO
          END DO
        END DO
      END DO

C Check the consistency condition for variation of first moment of the
C conditional A-M diagram with size and calculate vu. But do it only if necessary
      IF(ABS(Factor) .GT. 0.001) Then
        DO K = 1,NGC
          DO L = 1,NDC
            DELTAmin = 0.548  !0.548
            DELTA0 = 0.9*DELTAmin
            Umin = (1.0 - DELTAmin)/(1.0 - DELTA0)
            alpha2 = -1.0/ALOG(Umin)
            Dalpha2Ddelta0 = alpha2*alpha2/(1.0 - DELTA0)
            DEL = 1.0
            IterNo = 0
            DO WHILE (ABS(DEL) > 0.0001)
              IterNo = IterNo + 1
              TestSum = 0.0
              DSumDdelta0 = 0.0
              DO J = L+1,NDC
                SizeRatio = SIZE(J)/SIZE(L)
                u = (1.0 - SizeRatio)/(1.0 - DELTA0)
                WS = u**alpha2
                vu(J,K,L) = WS*ALOG(u)
                DvuDu = (alpha2*vu(J,K,L) + WS)/u
                DuDdelta0 = u/(1 - DELTA0)
                DvuDalpha2 = vu(J,K,L)*ALOG(u)
                DvuDdelta0 = DvuDu*DuDdelta0 + DvuDalpha2*Dalpha2Ddelta0
                TestSum = TestSum + vu(J,K,L)*BB(J,K,L)
                DSumDdelta0 = DSumDdelta0 + DvuDdelta0*BB(J,K,L)
              END DO
              IF(L .LT. NDC-1) THEN
                !Iterate to make the sum zero
                DEL = TestSum/DSumDdelta0
                DELTA0 = DELTA0 - DEL
                IF(DELTA0 .GE. DELTAmin) DELTA0 = DELTAmin - 0.00001
                IF(DELTA0 .LE. 0.0) DELTA0 = 0.0001
C              write(13,*)'DELTA0',DELTA0
              ELSE
                vu(NDC,K,L) = 0.0
                DEL = 0.0
              END IF
            END DO
            Write(13,*) 'K,L, IterNo ',K,L,IterNo

          END DO
          Write(13,'(''The Vu matrix for parent grade class '',I2)')K
          DO J = 1,NDC
            WRITE(13,'(16F6.2)')(vu(J,K,L),L = 1,NDC)
          END DO
        END DO
	ELSE 
	  vu = 0.0
	END IF
c Calculate the transfer coefficients for the AM diagram.
      DO 50 L = 1,NDC
        WRITE(13,*)'Parent size',SIZE(L)
        DO K = 1,NGC
          WRITE(13,*)
          WRITE(13,*)'Parent grade',Grade(K)
          DO J = L+1,NDC
            CALL AMDIAGRAM(Grade(K),K,SIZE(L),L,SIZE(J),J,AMB,NGC,
     *      vu,Factor,LiberationSize,CUMGRADE,Grade)
            DO I = 1,NGC
              A(J,I,L,K) = NINT(10000*AMB(I))
            END DO
            write(13,'(12I6)')(A(J,I,L,K),I = 1,NGC)
          END DO

C         Fill up the reference row
          DO I = 1,NGC
            A(L,I,L,K) = 0
            IF (I .EQ. K) A(L,I,L,K) = 10000
          END DO
        END DO
   50 END DO
      RETURN


 9990 CONTINUE

      WRITE(DiagFile,*)'Error opening file BETAAMD.DAT'
      CALL FLUSH(DiagFile)
      RETURN
      END


      SUBROUTINE AMDIAGRAM(ParentGrade,K,ParentSize,L,ProgenySize,J,AMB,
     *NGC,vu,Factor,LibSize,CUMGRADE,Grade)
!     ****************************************************************
! Calculates one row of the Andrews-Mika diagram
! Factor is the preferential breakage factor.
      REAL AMB(NGC)
      REAL lower, upper
      REAL LibSize,ParentSize,ProgenySize,ParentGrade
      REAL n1, n1M
      REAL vu(35,12,35)
      REAL CUMGRADE(0:12),Grade(12)
      REAL dAMBdAlpha(12),dAMBdBeta(12)

      COMMON /AMDParameters/ PowerFactor,PowerExp,GammaFactor,
     *GammaExponent,AsymFactor
      INTEGER DiagFile
      COMMON /Diagnostics/ Diagfile

! Define the variable transformation
      Xi(x)  = (x - lower)/(upper - lower)

      DO I = 1,NGC
        AMB(I) = 0.0
      END DO

      CUMGRADE(0) = 0.0

! set up the parameters for the bounds
      delta = PowerFactor*(LibSize/ParentSize)**PowerExp
      IF (AsymFactor .GT. 1.0) THEN
        delta_L = delta/asymFactor
        delta_U = delta
      ELSE
        delta_U = delta*asymFactor 
        delta_L = delta
      END IF              
      IF (delta_U .GT. 3.0) delta_U = 3.0
      IF (delta_L .GT. 3.0) delta_L = 3.0

! Liberated particles produce only liberated progeny
      IF(Parentgrade .EQ. 0.0) THEN
        AMB(1) = 1.0
        DO I = 2,NGC
          AMB(I) = 0.0
        END DO
        RETURN
      END IF
      IF(ParentGrade .EQ. 1.0) THEN
        AMB(NGC) = 1.0
        DO I = 1,NGC-1
          AMB(I) = 0.0
        END DO
        RETURN
      END IF

! Model the variation of moments with size
      !Allow first moment to vary with progeny size
      ReferenceGrade = ParentGrade +
     *Factor*(1 - ParentGrade)*ParentGrade*vu(J,K,L)
      IF(ReferenceGrade < 0.0) ReferenceGrade = 0.0
      IF(ReferenceGrade > 1.0) ReferenceGrade = 1.0

      n1 = ReferenceGrade
      f = 1.0/(1.0 + (ProgenySize/LibSize)*GammaExponent)
      gammaa = (1 - f)/f


! Get the two AM bounds
      dp = ProgenySize
      CALL AM_BOUNDS(dp,ParentSize,LibSize,ParentGrade,
     *delta_U,delta_L,upper,lower)

c      IF(lower > ReferenceGrade - 0.051) Then
c        lower = ReferenceGrade - 0.051
c        write(13,*)'lower',lower
c      END IF
c      IF(upper < ReferenceGrade + 0.051) Then
c        upper = ReferenceGrade + 0.051
c        write(13,*)'upper',upper
c      END IF

! Find the indices of the boundaries
      ifirst = 1
      last = NGC
      DO I = 2,NGC-1
        IF(CUMGRADE(I) .LT. lower) ifirst = I+1
      END DO
      DO I = NGC,2,-1
        IF(CUMGRADE(I) .GT. upper) last = I
      END DO

      IF (ifirst .EQ. last) THEN
        !write(13,*)'ifirst = last=',ifirst
      END IF

! Find the parameters of the beta function
      alpha = n1*gammaa
      betaa = (1 - n1)*gammaa

      IF(Abs(ParentGrade - 0.25) .LE. 0.001) then
	write(diagfile,*)'Lower,Upper',lower,upper
	write(diagfile,*)'n1,alpha,betaa,gammaa',n1,alpha,betaa,gammaa
      END IF    
      iteration = 0

! Get the AM coefficients
      IF (lower .GE. 0.0 ) THEN
        AMB(1) = 0.0
      ELSE
	  AMB(1) = BETAI(Xi(0.0),alpha,betaa)
      END IF

      IF (upper .LT. 1.0) THEN
        AMB(NGC) = 0.0
      ELSE
	AMB(NGC) = 1.0 - BETAI(Xi(1.0),alpha,betaa)
      END IF

      Test1 = (ReferenceGrade - GRADE(ifirst))/(1-GRADE(ifirst))
      Test2 = (GRADE(last) - ReferenceGrade)/GRADE(last)
      IF (AMB(1) .GE. Test2) THEN
        AMB(1) = max(Test2,0.0)
        AMB(last) = 1.0 - AMB(1)
        DO I = 2,last-1
          AMB(I) = 0.0
        END DO
      ELSE IF (AMB(NGC) .GE. Test1) THEN
        AMB(NGC) = Max(Test1,0.0)
        AMB(ifirst) = 1.0 - AMB(NGC)
        DO I = ifirst+1,NGC-1
          AMB(I) = 0.0
        END DO
      ELSE
c       Calculate alpha and beta for the internal distribution
        n1 = (ReferenceGrade - AMB(NGC))/(1.0 - AMB(1) - AMB(NGC))
        IF(lower .LT. 0.0) lower = 0.0
        IF(upper .GT. 1.0) upper = 1.0
	  alpha = n1*gammaa
	  DalphaDn1 = gammaa
	  betaa = (1 - n1)*gammaa
	  DbetaDn1 = -gammaa

c       Re-entry point after correcting alpha
   10   CONTINUE
        iteration = iteration + 1

        DO I = 2,NGC-1
          XiU = Xi(CUMGRADE(I))
          XiL = Xi(CUMGRADE(I-1))
          IF(XiU .LE. 0.0) THEN
            WS1 = 0.0
            DWS1Dalpha = 0.0
            DWS1Dbeta = 0.0
          END IF
          IF(XiU .GT. 0.0 .AND. XiU .LE. 1.0) THEN
	    WS1 = BETAI(XiU,alpha,betaa)
	    DWS1Dalpha = dBetaIdAlpha(XiU,alpha,betaa)
	    DWS1Dbeta = dBetaIdBeta(XiU,alpha,betaa)
          END IF
          IF(XiU .GT. 1.0) THEN
            WS1 = 1.0
            DWS1Dalpha = 0.0
            DWS1Dbeta = 0.0
          END IF

          IF(XiL .LE. 0.0) THEN
            WS = 0.0
            DWSDalpha = 0.0
            DWSDbeta = 0.0
          END IF
          IF(XiL .GT. 0.0 .AND. XiL .LE. 1.0) THEN
	    WS = BETAI(XiL,alpha,betaa)
	    DWSDalpha = dBetaIdAlpha(XiL,alpha,betaa)
	    DWSDbeta = dBetaIdBeta(XiL,alpha,betaa)
          END IF
          IF(XiL .GT. 1.0) THEN
            WS = 1.0
            DWSDalpha = 0.0
            DWSDbeta = 0.0
          END IF
          AMB(I) = (WS1 - WS)*(1 - AMB(1) - AMB(NGC))
          dAMBdAlpha(I)=(DWS1Dalpha - DWSDalpha)*(1 - AMB(1) - AMB(NGC))
          dAMBdBeta(I) = (DWS1Dbeta - DWSDbeta)*(1 - AMB(1) - AMB(NGC))
        END DO

        ! Calculate the average grade
        AverageGrade = AMB(NGC)
        DAvgrdDalpha = 0  !dAMBdAlpha(NGC)
        DAvgrdDbeta  = 0  !dAMBdBeta(NGC)
        DO I = 2,NGC-1
          AverageGrade = AverageGrade + Grade(I)*AMB(I)
          DAvgrdDalpha = DAvgrdDalpha + Grade(I)*dAMBdAlpha(I)
          DAvgrdDbeta  = DAvgrdDbeta + Grade(I)*dAMBdBeta(I)
        END DO
        DAvgrdDn1 = DAvgrdDAlpha*DalphaDn1 + DAvgrdDbeta*DbetaDn1
        If(iteration .GT. 1 .AND. Abs(n1-n1m) .GT. 0.0) THEN
          Del = (AverageGrade - AverageGradeM)
          DAvgrdDn1 = Del/(n1 - n1M)
        ELSE
          DAvgrdDn1 = 0.0
        END IF

	! Refine the values of alpha and betaa if necessary.
        IF (ABS(AverageGrade - ReferenceGrade) .LT. 0.003
     *    .OR. DAvgrdDn1 .EQ. 0.0) Then
          RETURN
        ELSE
          AverageGradeM = AverageGrade
          n1M = n1
          n1 = n1 - (AverageGrade - ReferenceGrade)/DAvgrdDn1
          IF(n1 - n1M .LE. 1e-6) RETURN
          IF(n1 .LE. 0.001) THEN
            n1 = 0.001
	    alpha = n1*gammaa
	    betaa = gammaa - alpha
            !RETURN
          END IF
          IF(n1 .GT. 0.999) then
            n1 = 0.999
            !RETURN
          end if
	    alpha = n1*gammaa
	    betaa = gammaa - alpha
          GO TO 10
        END IF
      END IF
C      IF(Abs(ParentGrade - 0.25) .LE. 0.001) then
C      write(diagfile,*)'n1,alpha,betaa,gammaa after refine'
C      write(diagfile,*) n1,alpha,betaa,gammaa
C      END IF    
      END

      SUBROUTINE AM_BOUNDS(dp,ParentSize,LibSize,ParentGrade,
     *delta_U,delta_L,Gu,Gl)
      !******************************************************
      !Calculate the upper and lower bounds for the attainable region
      REAL LibSize
      SR = ParentSize/dp
      IF (ParentSize .GT. LibSize .AND. dp .GT. LibSize) THEN
        Gu = ParentGrade*SR**0.2
        Gl = 1 - (1 - ParentGrade)*SR**0.2
      Else IF(ParentSize .GT. LibSize ) THEN
        Gu = max(ParentGrade*(SR)**0.2,
     *  ParentGrade*(Libsize/dp)**delta_U)
        Gl = 1 - max((1-ParentGrade)*SR**0.2,
     *  (1 - ParentGrade)*(Libsize/dp)**delta_L)
      ELSE
        Gu = ParentGrade*SR**delta_U
        Gl = 1 - (1 - ParentGrade)*SR**delta_L
      END IF
      RETURN
      End

      FUNCTION TruncatedLogNormal(ProgenySize,ParentSize,mu,sigma)
!     ************************************************************
! Calculates the breakage function using the truncated log-normal model.
      REAL mu
      xi = ProgenySize/ParentSize
      IF (xi .LT. 1.0) THEN
        eta = xi/(1.0 - xi)
        eta50 = mu/(1.0 - mu)
        TruncatedLogNormal = ANORDF(ALOG(ETA) - ALOG(eta50)/sigma)
      ELSE
        TruncatedLogNormal = 1.0
      END IF
      RETURN
      END

      FUNCTION dBetaIdAlpha(x,alpha,betaa)
c     ***********************************
c     Function to calculate the gradient of the incomplete beta function
      REAL*8 xx,a,b
      xx = x
      a = alpha
      b = betaa
      dBetaIdAlpha = (DBETAI(xx,1.01*a,b) - DBETAI(xx,a,b))/(0.01*a)
      RETURN
      END

      FUNCTION dBetaIdBeta(x,alpha,betaa)
c     **********************************
c     Function to calculate the gradient of the incomplete beta function
      REAL*8 xx,a,b
      xx = x
      a = alpha
      b = betaa
      dBetaIdBeta = (DBETAI(xx,a,1.01*b) - DBETAI(xx,a,b))/(0.01*betaa)
      RETURN
      END

C ======================================================================
C  Pure-Fortran replacements for IMSL routines BETAI, DBETAI, ANORDF
C ======================================================================

C ----------------------------------------------------------------------
      FUNCTION BETAI(x, a, b)
C     Regularised incomplete beta function I_x(a,b), single precision.
C     Uses continued-fraction representation (Lentz's method).
C     Ref: Numerical Recipes in Fortran 77, Ch. 6.4
      REAL x, a, b
      REAL BETAI
      REAL*8 dx, da, db, res
      dx = DBLE(x)
      da = DBLE(a)
      db = DBLE(b)
      CALL BETAI_DP(dx, da, db, res)
      BETAI = REAL(res)
      RETURN
      END

C ----------------------------------------------------------------------
      FUNCTION DBETAI(x, a, b)
C     Regularised incomplete beta function I_x(a,b), double precision.
      REAL*8 x, a, b
      REAL*8 DBETAI
      REAL*8 res
      CALL BETAI_DP(x, a, b, res)
      DBETAI = res
      RETURN
      END

C ----------------------------------------------------------------------
C  Internal workhorse — not exported
      SUBROUTINE BETAI_DP(x, a, b, result)
C     Computes the regularised incomplete beta I_x(a,b) in double precision.
      IMPLICIT NONE
      REAL*8, INTENT(IN)  :: x, a, b
      REAL*8, INTENT(OUT) :: result
      REAL*8 :: lbeta, bt, cf
      REAL*8 :: BETACF_DP   ! forward reference

      IF (x .LT. 0.0D0 .OR. x .GT. 1.0D0) THEN
        result = 0.0D0
        RETURN
      END IF
      IF (x .EQ. 0.0D0) THEN
        result = 0.0D0
        RETURN
      END IF
      IF (x .EQ. 1.0D0) THEN
        result = 1.0D0
        RETURN
      END IF

      lbeta = LOG_GAMMA(a + b) - LOG_GAMMA(a) - LOG_GAMMA(b)
      bt    = EXP(lbeta + a*LOG(x) + b*LOG(1.0D0 - x))

C     Use symmetry relation when x > (a+1)/(a+b+2)
      IF (x .LT. (a + 1.0D0)/(a + b + 2.0D0)) THEN
        cf = BETACF_DP(x, a, b)
        result = bt * cf / a
      ELSE
        cf = BETACF_DP(1.0D0 - x, b, a)
        result = 1.0D0 - bt * cf / b
      END IF
      RETURN
      END

C ----------------------------------------------------------------------
      FUNCTION BETACF_DP(x, a, b)
C     Continued fraction for incomplete beta (Lentz's method), double precision.
      IMPLICIT NONE
      REAL*8, INTENT(IN) :: x, a, b
      REAL*8 :: BETACF_DP
      INTEGER :: m, m2
      REAL*8  :: aa, c, d, del, h, qab, qam, qap
      REAL*8, PARAMETER :: FPMIN = 1.0D-30
      INTEGER, PARAMETER :: MAXIT = 200
      REAL*8, PARAMETER  :: EPS   = 3.0D-7

      qab = a + b
      qap = a + 1.0D0
      qam = a - 1.0D0
      c   = 1.0D0
      d   = 1.0D0 - qab*x/qap
      IF (ABS(d) .LT. FPMIN) d = FPMIN
      d   = 1.0D0/d
      h   = d

      DO m = 1, MAXIT
        m2 = 2*m
        aa = DBLE(m)*(b - DBLE(m))*x /
     *       ((qam + DBLE(m2))*(a + DBLE(m2)))
        d  = 1.0D0 + aa*d
        IF (ABS(d) .LT. FPMIN) d = FPMIN
        c  = 1.0D0 + aa/c
        IF (ABS(c) .LT. FPMIN) c = FPMIN
        d  = 1.0D0/d
        h  = h*d*c

        aa = -(a + DBLE(m))*(qab + DBLE(m))*x /
     *        ((a + DBLE(m2))*(qap + DBLE(m2)))
        d  = 1.0D0 + aa*d
        IF (ABS(d) .LT. FPMIN) d = FPMIN
        c  = 1.0D0 + aa/c
        IF (ABS(c) .LT. FPMIN) c = FPMIN
        d  = 1.0D0/d
        del = d*c
        h   = h*del
        IF (ABS(del - 1.0D0) .LT. EPS) EXIT
      END DO

      BETACF_DP = h
      RETURN
      END

C ----------------------------------------------------------------------
      FUNCTION ANORDF(x)
C     Standard normal CDF Phi(x), single precision.
C     Uses the relation  Phi(x) = 0.5 * erfc(-x / sqrt(2))
      REAL x
      REAL ANORDF
      REAL*8 :: dx, val
      dx  = DBLE(x)
      val = 0.5D0 * ERFC(-dx / SQRT(2.0D0))
      ANORDF = REAL(val)
      RETURN
      END
