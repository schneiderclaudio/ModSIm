Real Function ModelResponse(NOPAR,Parameters,I)
!**********************************************
  USE WorkingCommon

	INTERFACE
	  FUNCTION INORDCALC(JobPath)
      !MS$ATTRIBUTES STDCALL, DLLIMPORT :: INORDCALC
      !MS$ATTRIBUTES ALIAS :'INORDCALC' :: INORDCALC
      INTEGER*2 INORDCALC
      CHARACTER*255 JobPath
      !MS$ATTRIBUTES REFERENCE :: JobPath
    END FUNCTION INORDCALC
  END INTERFACE

	INTERFACE
	  FUNCTION SIMOP(JobPath,CumOut)
      !MS$ATTRIBUTES STDCALL, DLLEXPORT :: SIMOP
      !MS$ATTRIBUTES ALIAS :'SIMOP' :: SIMOP
      INTEGER*2 SIMOP,CumOut
      CHARACTER*255 JobPath
      !MS$ATTRIBUTES REFERENCE :: JobPath
      !MS$ATTRIBUTES REFERENCE :: CumOut
    END FUNCTION SIMOP
  END INTERFACE
  Integer*2 CumOut

  REAL Parameters(NOPAR)
  Integer Stream

  CumOut = 0
  IF (I .EQ. 1) Then
    !Put the required parameters into DATT.DAT
    Call SetModsimParameters(NOPAR,Parameters)
    resp1 = INORDCALC(WCJobPath)
    Write(113,*)'Left INORDCALC   resp1',resp1
    If (resp1 .NE. -1) then
      Write(113,'(''Modsim did not converge properly so the objective function could not be calculated'')')
    End if
    resp2 = SIMOP(WCJobPath,CumOut)
    If (resp2 .NE. -1) then
      Write(113,'(''Modsim did not produce valid output so the objective function could not be calculated'')')
    End if
    Write(113,*)'Left SIMOP   resp2',resp2
    Call PSDObjective
  End if
	ModelResponse = Objectives(I)
End Function ModelResponse

Subroutine SetModsimParameters(NOPAR,Parameters)
!***********************************************
  USE WorkingCommon
  REAL Parameters(NOPAR)
  Character*80 LineOfText
  Character*4 WSTRING,Model
  Integer Unit,NoPARAM,ThisUnitID
  Real PARAM(50)

  OPEN(30, FILE = WCJobPath(1:PathLength)//'DATT.DAT')
  OPEN(31, FILE = WCJobPath(1:PathLength)//'CURRDATA.RUN')
  OPEN(32, FILE = WCJobPath(1:PathLength)//'Currdata.Syd')

  DO While (.NOT. EOF(32))
    Read(32,'(A80)') LineOfText
    Write(30,'(A80)') LineOfText
  End Do
  Close(32)
  Read(31,'(A80)')LineOfText
  !Write(113,'(A80)')LineOfText
  Do While (.NOT. EOF(31))
    IF (LineOfText(1:4) .EQ. 'TYPE' ) Then
      Read(LineOfText,'(A4,1X,I3,1X,A4,1X,I3,1X,I3)') WSTRING,Unit,Model,NoPARAM,ThisUnitID 
      !Write(113,*)' WSTRING,Unit,Model,NoPARAM,ThisUnitID', WSTRING,Unit,Model,NoPARAM,ThisUnitID
      Write(30,'(A80)') LineOfText
      If(NoPARAM .GT. 0) Then
        Read(31,*)(Param(I),I = 1,NoPARAM)
        Do J = 1,NOPAR
          IF(UnitNumber(J) .EQ. Unit) Then
            PARAM(ParameterNumber(J)) = Parameters(J)
          End If
        End Do
        Write(30,'(5E16.8)')(Param(I),I = 1,NoPARAM)
        !Write(113,'(5E16.8)')(Param(I),I = 1,NoPARAM)
      End If
    End If
    IF (LineOfText(1:4) .EQ. 'OUTC' ) Then
      Write(30,'(A80)') LineOfText
      Read(31,'(A80)')LineOfText
      Write(30,'(A80)') LineOfText
      Write(30,'(A4)')'STOP'
      Exit
    End If
    Read(31,'(A80)',IOSTAT=iVar)LineOfText
    !Write(113,'(A80)') LineOfText
    If (iVar .LT. 0) Exit
  End Do
  Close(30)
  Close(31)
End Subroutine SetModsimParameters

Subroutine PSDObjective
!**********************
!Calculates the objective function for the cumulative size distribution

	USE WorkingCommon
	USE GLOBALS
  Real CUM(125)
	Real X


	!Write(113,*)' In PSDObjective'
!  Write(113,*)' MeshSizes ' ,(MeshSizes(I),I = 1,NDC)
  Objectives = 0.0
  
  I = 0
	Do N = 1,NoSizeDistributions
    IF(resp1 .EQ. -1  .AND. resp2 .EQ. -1) then
      Call GetSizeDistribution(StreamNumbers(N),CUM)
      Do J = 1,StreamNDC(N) - 1 
        I = I+1
	  	  X = Observations(I)%MeshSize
		    !Write(113,*)'X ',X
        Call FRPASS(MeshSizes,CUM,NDC,P,X)
		    Objectives(I) = P
		    !Write(113,*)'I, Objectives(I)',I, Objectives(I)
      End Do
    Else
      Do J = 1,StreamNDC(N) - 1 
        I = I+1
		    Objectives(I) = 1.0
      End Do
    End if  
  End Do
End

Subroutine GetSizeDistribution(Stream,CUM)
!*****************************************
  !Get the size ditribution for stream from the stream properties file
  USE WorkingCommon
  USE Globals
  CHARACTER*80 WSTRING
  Integer Stream,StreamNumber
  Real CUM(NDC)

  LENG = LEN_TRIM(WCJobPath)
  Open(31,FILE = WCJobPath(1:LENG)//'STREAMPROPS.TXT',ERR = 9999)
  Do While (.NOT. EOF(31))
    READ(31,'(A80)') WSTRING
    If (WSTRING(1:14) .EQ. 'Stream number:') then
      READ(WSTRING,'(30X,I3)') StreamNumber
      If (StreamNumber .EQ. Stream) Then
        Do While (.NOT. EOF(31))
          READ(31,'(A80)') WSTRING
          If (WSTRING(1:27) .EQ. 'Simulated size distribution') then
            READ(31,'(30X,I3)') NDC
            READ(31,'(A80)') WSTRING
            READ(31,'(A80)') WSTRING
            READ(31,'(A80)') WSTRING
            READ(31,'(A80)') WSTRING
            READ(31,'(A80)') WSTRING
            Do I = 1,NDC
              READ(31,*)MeshSizes(I),CUM(I)
              CUM(I) = 0.01*CUM(I)
              !Write(113,*)'I,MeshSizes(I),CUM(I)',I,MeshSizes(I),CUM(I)
            End Do
            Close (31)
            RETURN
          End If
        End Do
      End If
    End If
  End Do
  Close(31)
  RETURN
  9999 Write(113,*)' Could not open file STREAMPROPS.TXT'
END