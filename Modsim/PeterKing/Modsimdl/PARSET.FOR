      PROGRAM PARSET
C     **************
C
C THIS PROGRAM SETS UP THE PROGRAM TO CALL THE PARAMETER INPUT
C  SUBROUTINES FOR THE UNIT MODELS.
C
      INTEGER TIMES
      CHARACTER*6 CALL
      CHARACTER*13 U
      CHARACTER*4 SUBR
      CHARACTER*4 ModelCodes(2000)
      CHARACTER*7 RepIndicator
      CHARACTER*67 UnitClass
      CHARACTER*80 WSTRING

      IRD=1
      OPEN(IRD,FILE='..\..\modsimvb\PLANT.DAT')

      !READ DATA RELATING TO ICONS AND MODELS AND make the default
      !UserPlant.txt file.
      !***********************************************************
      OPEN(31,FILE = '..\..\modsimvb\UserPlant.txt')
      READ(IRD,104) NOTYPE
      WRITE(31,104) NOTYPE
  104 FORMAT(I2)
  304 FORMAT(A4)
      NumberOfModels = 0
      DO 303 J =1,NOTYPE
        READ(IRD,'(I2,11X,A67)')I,UnitClass
        WRITE(31,'(I2,11X,A67)')I,UnitClass
        READ(IRD,104) MO
        IF(UnitClass .EQ. 'Black box') then
          write(31,'(A2)')' 2'
          WRITE(31,'(A4)')'BLBX'
          WRITE(31,'(A4)')'CBOX'
        ELSE IF (UnitClass .EQ. 'Hydrocyclone') then
	    Write(31,'(A2)')' 1'
	    Write(31,'(A4)')'NAGE'
        ELSE
          WRITE(31,104) 0
        END IF
        DO 6303 M=1,MO
          READ(IRD,304) SUBR
          Do N = 1,NumberOfModels
            If(SUBR .EQ. ModelCodes(N)) Go to 6303
          End Do
          NumberOfModels = NumberOfModels + 1
          ModelCodes(NumberOfModels) = SUBR
 6303   CONTINUE
  303 CONTINUE
      CLOSE(31)

C     First get the maximum dimensions from DMINP,FOR
      Open(21,FILE = '..\DMINP.FOR')
	Do N = 1,18
	  READ(21,*)
	End Do
	READ(21,'(6X,A5,I4)')WSTRING,NDCMM
	READ(21,'(6X,A5,I4)')WSTRING,NGCMM
	READ(21,'(6X,A5,I4)')WSTRING,NSCMM
      CLOSE(21)

      !Write the subroutine to call the unit models from inside CALC.
      !**************************************************************
      IWS = 35
      OPEN(IWS,FILE='..\PPHI5.FOR')
      Write(IWS,'(''!This file is created by program PARSET.FOR'')')
      Write(IWS,'(''!It must not be changed by the user'')')
      U = 'SUBROUTINE  U'
      CALL = 'CALL  '
      RepIndicator = '.FALSE.'
      DO TIMES = 1,2
        WRITE(IWS,2036) U
2036    FORMAT(
     *  6X,A13,'(T,T1,T2,T3,F,O1,O2,O3,D1,D2,D3,ND,NG,NS,W,W1,W2,W3,'/
     *  5X,'*SZ,P,PP,IPP,FL,NPP,GM,GV,NM,NGM,II,NP,ModelCode)')
        WRITE(IWS,*)'      USE GLOBALS'
        WRITE(IWS,*)'      USE MODELVARIABLES'
        WRITE(IWS,*)'      COMMON /MODELDAT/NUNIT'
        Write(IWS,*)'      CHARACTER*80 JobN'
        WRITE(IWS,*)'      COMMON /JOBDATA/JobN,JobNameLength'
        WRITE(IWS,2039)
2039    FORMAT(
     *  6x,'REAL F(1),O1(1),O2(1),O3(1),D1(1),D2(1),D3(1),SZ(1),P(1)'/
     *  6x,'REAL PP(1),GM(NGM,NM),GV(NGM,NM)'/
     *  6X,'INTEGER FL,IPP(NPP,2),II'/
     *  6x,'CHARACTER*4 ModelCode')
        WRITE(IWS,8015)
 8015   FORMAT(
     *  6x,'INTERFACE'/
     *  6x,'  SUBROUTINE MODELSEQUENCE(ModelCode)'/
     *  6x,'  !MS$ ATTRIBUTES DLLIMPORT :: MODELSEQUENCE'/
     *  6x,'    CHARACTER*4 ModelCode'/
     *  6X,'  END SUBROUTINE MODELSEQUENCE'/
     *  6X,'END INTERFACE')
        WRITE(IWS,*)
        WRITE(IWS,2040)
 2040   FORMAT(6X,'SELECT CASE (ModelCode)')
        DO 5031 I = 1,NumberOfModels
          WRITE(IWS,7011) ModelCodes(I)
 7011     FORMAT(6X,'  CASE (''',A4,''')')
          WRITE(IWS,7050) CALL,ModelCodes(I)
 7050     FORMAT(10X,A6,A4,'(T,T1,T2,T3,F,O1,O2,O3,D1,D2,D3,ND,NG,NS,'
     *    /5X,'*',4X,'W,W1,W2,W3,SZ,P,PP,IPP,FL,NPP,GM,GV,NM,NGM)')
 5031   CONTINUE
        WRITE(IWS,7060)
 7060   FORMAT(
     *  6X,'  CASE DEFAULT'/
     *  6x,'    NumSizeClasses = ND'/
     *  6X,'    NumGClasses = NG'/
     *  6X,'    NumSClasses = NS'/
     *  6X,'    NumberOfMinerals = NM'/
     *  6X,'    TotalSolidsF = T'/
     *  6X,'    CALL WaterFlow(FeedWater,W,T)'/
     *  6X,'    CALL PackIn(F_,F,ND,NG,NS)'/
     *  6X,'    CALL EQUATE(Parameters,P,1,NP)'/
     *  6X,'    CALL EQUATE(RepSize,SZ,1,ND)'/
     *  6X,'    Do I = 1,NGM'/
     *  6X,'      Do J = 1,NM'/
     *  6X,'        GradeM(I,J) = GM(I,J)'/
     *  6X,'        GradeV(I,J) = GV(I,J)'/
     *  6X,'      End Do'/
     *  6X,'    End Do')

        CALL SetUpPP('SolidSpGr',9,1,IWS)
        CALL SetUpPP('Texture',7,2,IWS)
        CALL SetUpPP('MagnSusceptG',12,3,IWS)
        CALL SetUpPP('OtherPropG',10,4,IWS)
        CALL SetUpPP('FlotnRateConst',14,5,IWS)
        CALL SetUpPP('MagnSusceptS',12,6,IWS)
        CALL SetUpPP('OtherPropS',10,7,IWS)
        CALL SetUpPP('CalValue',8,8,IWS)
        CALL SetUpPP('TotalSulfur',11,9,IWS)
        CALL SetUpPP('PyriticSulf',11,10,IWS)

        WRITE(IWS,7065)RepIndicator
 7065   FORMAT(
     *  6X,'    NumberOfMessages = 0'/
     *  6X,'    Reporting = ',A7 /
     *  6X,'    UnitNumber = NUNIT'/
     *  6X,'    UnitType = II'/
     *  6X,'    JobName = JobN'/
     *  6x,'    CALL MODELSEQUENCE(ModelCode)'/)
        WRITE(IWS,7066)
 7066   FORMAT(
     *  6x,'    T1 = TotalSolidsT; T2 = TotalSolidsC; T3 = TotalSolidsM'
     *  /6x,'    W2 = ConcentrateWater ; W1 = TailingsWater'/
     *  6x,'    W3 = MiddlingsWater'/
     *  6X,'    CALL EQUATE(O1,T_,1,ND*NG*NS)'/
     *  6X,'    CALL EQUATE(O2,C_,1,ND*NG*NS)'/
     *  6X,'    CALL EQUATE(O3,M_,1,ND*NG*NS)'/)
      WRITE(IWS,7070)
 7070   FORMAT(
     *  6X,'    IF(NumberOfMessages .GT. 0 ) Then'/
     *  6X,'      LENG = LEN_TRIM(UnitJobPath)'/
     *  6X,'      OPEN(14,FILE=UnitJobPath(1:LENG)//''UserDiagFile.txt''
     *)'/
     *  6X,'      Do I = 1,NumberOfMessages'/
     *  6X,'        READ(14,''(A80)'') Message'/
     *  6X,'        LENG = LEN_TRIM(Message)'/
     *  6X,'        Write(UnitDiagFile,''(80A1)'') (Message(J:J),J = 1,L
     *ENG)'/
     *  6X,'      End Do'/
     *  6X,'      Close(14)'/
     *  6X,'    End If')

        WRITE(IWS,7080)
 7080   FORMAT(
     *  6x,'END SELECT')
        WRITE(IWS,7090)
 7090   FORMAT(6X,'RETURN'/6X,'END')
        U = 'SUBROUTINE RU'
        CALL = 'CALL R'
        RepIndicator = ' .TRUE.'
      END DO

      WRITE(IWS,*)
      WRITE(IWS,8100)
 8100 FORMAT(
     *6X,'SUBROUTINE EQUATE(ARRAY1,ARRAY2,ISTART,N)'/
     *6X,'!****************************************'/
     *6X,'REAL ARRAY1(*),ARRAY2(*)'/
     *6X,'INTEGER N, I'/
     *6X,'DO I = 1,N'/
     *6X,'  ARRAY1(I) = ARRAY2(I+ISTART-1)'/
     *6X,'END DO'/)
      WRITE(IWS,7090)

      WRITE(IWS,*)
	WRITE(IWS,8150)NDCMM,NDCMM,NGCMM
 8150 FORMAT(
     *6X,'SUBROUTINE PackIn(ARRAY1,ARRAY2,NDC,NGC,NSC)'/
     *6X,'!*******************************************'/
     *6X,'REAL ARRAY1(*),ARRAY2(*)'/
     *6X,'INTEGER NDC,NGC,NSC,N,M'/
     *6X,''/
     *6X,'N = 0'/
     *6X,'Do K = 1,NSC'/
     *6X,'  Do J = 1,NGC'/
     *6X,'    Do I = 1,NDC'/
     *6X,'      N = N + 1'/
     *6X,'      M = I + (J-1)*',I3,' + (K-1)*',I3,'*',I3/
     *6X,'      ARRAY1(M) = ARRAY2(N)'/
     *6X,'    End Do'/
     *6X,'  End Do'/
     *6X,'End Do'/
     *6X,'RETURN'/
     *6X,'END'/)

c Calculate the water flows if water is added to the unit
      WRITE(IWS,*)
      WRITE(IWS,8200)
 8200 FORMAT(
     *6X,'SUBROUTINE WaterFlow(Water,W,Solids)'/
     *6X,'!***********************************')
      WRITE(IWS,*)'      LOGICAL UNITWFEED'
      WRITE(IWS,*)'      REAL WATERADD, SOLIDCONT'
      WRITE(IWS,*)
     *'      COMMON /WATERFEED/ UNITWFEED, WATERADD, SOLIDCONT'
      WRITE(IWS,*)'      IF(UNITWFEED) THEN'
      WRITE(IWS,*)'        IF(SOLIDCONT .LE. 0.0) THEN'
      WRITE(IWS,*)'          Water = W + WATERADD'
      WRITE(IWS,*)'        ELSE'
      WRITE(IWS,*)'          WTR1 = Solids*(100.0-SOLIDCONT)/SOLIDCONT'
      WRITE(IWS,*)'          WATERADD = WTR1 - W'
      WRITE(IWS,*)'          Water = WTR1'
      WRITE(IWS,*)'        END IF'
      WRITE(IWS,*)'      ELSE'
      WRITE(IWS,*)'        Water = W'
      WRITE(IWS,*)'      END IF'
      WRITE(IWS,*)'      END'

      CLOSE(IWS)

	STOP "PARSET.EXE has been run"

      END

      Subroutine SetUpPP(Name,LENG,Num,LU)
      !***********************************
      CHARACTER*80 Name,TEXT,TEXT1
      INTEGER NUM
      WRITE(TEXT1,1010)NUM,NUM
 1010 FORMAT(
     *',PP,IPP(',I3,',1),IPP(',I3,',2))')
      TEXT = '         CALL EQUATE('//Name(1:LENG)//TEXT1
      WRITE(LU,*)TRIM(TEXT)
      END
