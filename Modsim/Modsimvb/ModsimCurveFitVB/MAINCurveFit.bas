Attribute VB_Name = "MAINCurveFit"
Option Explicit

Public Const MaximumSizeClasses As Integer = 35
Public Const MaximumGClasses As Integer = 22
Public Const MaximumSClasses As Integer = 10

'Public Type SIZEDATA
'    NDC As Integer
'    ID As Integer
'    SET_NAME As String * 30
'    CUMSIZE(1 To MaximumSizeClasses) As Single
'    size(1 To MaximumSizeClasses) As Single
'    fraction(1 To MaximumSizeClasses) As Single
'End Type
'Public CURRDATA(30) As SIZEDATA
Public Type STREAMDATA
    NSTR As Integer
    NDC As Integer
    ID As Integer
    SET_NAME As String * 50
    CUMSIZE(1 To MaximumSizeClasses) As Single
    fraction(0 To MaximumSizeClasses) As Single
End Type
Public CURRODATA(30) As STREAMDATA

Public Type GraphSetupData
  Number As Integer
  GraphList(30) As Integer
  SizeCode As Integer
  CoordinateCode As Integer
  LineCode As Integer
  TruncationCode As Integer
  TruncationSize As Single
  GraphData(30) As STREAMDATA
End Type
Public SetupData As GraphSetupData

Public Type ConversionFactor
  Number As Integer
  Factor(1 To 10) As Double
End Type
Public CFactor As ConversionFactor
Public PARAMIndex As Integer
Public m_cm_ft As ConversionFactor
Public m_feet As ConversionFactor
Public m_cm_inch As ConversionFactor
Public cm_m_inch_ft As ConversionFactor
Public mm_inch As ConversionFactor
Public mm_cm_m As ConversionFactor
Public mm_cm_m_inch As ConversionFactor
Public mm_cm_inch As ConversionFactor
Public micron_mm_cm_m_inch As ConversionFactor
Public micron_mm_cm_m_inch_ft As ConversionFactor
Public cubm_cubft As ConversionFactor
Public kgps_stonphr_tonnephr_ltonphr As ConversionFactor
Public kgps_tonnephr_stonphr_ltonphr As ConversionFactor
Public kgps_tonnephr_stonphr_Mtonnepyr As ConversionFactor
Public Pas_cp As ConversionFactor
Public cubmpers_cubmperhr_USgalpermin As ConversionFactor
Public cubmps_cubmphr_USgalpmin_ImpGalpmin_cubftpmin As ConversionFactor
Public kgpcubm_lbpcubft As ConversionFactor
Public gpercc_kgpcubm_lbpcubft As ConversionFactor
Public mps_ftpmin As ConversionFactor
Public kWhpert_kWhperst As ConversionFactor

Public JobPath As String * 255
Public JobFileName As String * 255
Public VGPath As String
Public JOBNAME As String
Public JobSavePath As String
Public FileReadPath As String
Public EnvString As String
Public FastParameterChange As Boolean
Public OptionShowFlowsheet As Boolean
Public ProfessionalVersion As Boolean
Public AcademicVersion As Boolean

Public CurrentIndex As Integer
Public NumberOfDataSets As Integer
Public ChangedA_MDiagramParameters As Boolean
Public NameOfJob As String * 255
Public WSTRING As String

Public SavedJob As Boolean
Public JobFlowsheet As Boolean
Public JobSystemData As Boolean
Public JobModelData As Boolean
Public JobSystemInputData As Boolean
Public JobModelParameterData As Boolean
Public JobUnitParametersNeeded As Boolean
Public JobSystemDataNeeded As Boolean
Public JobCurrentData As Boolean
Public JobSizeData As Boolean
Public JobGCDistribution As Boolean
Public JobWashabilityData As Boolean
Public JobSCDistribution As Boolean
Public JobLibcomp As Boolean
Public JobOutputFormat As Boolean
Public JobSimopData As Boolean
Public JobLjubAMD As Boolean
Public JobBetaAMD As Boolean
Public DontShowHelp As Boolean

Public ChangedFlowsheet As Boolean
Public ChangedModelParameterData As Boolean
Public NOSPLIT(100) As Integer
Public NOCELL(20) As Integer

Public DisplayOffsetX As Integer
Public DisplayOffsetY As Integer

'Public Declare Sub LevMarCurveFit Lib "C:\Modsim3617\modsimdl\ModsimCurveFitDll\Release\ModsimCurveFit.dll" (ByVal JobPath As String, ByVal JobFileName As String, ByRef NOPAR As Long, ByRef Parameters As Single, ByRef LowB As Single, ByRef UpB As Single, ByRef SumOfSquares As Single, ByRef LowerConfLim As Single, ByRef UpperConfLim As Single, ByRef ConfLevel As Single)
'Public Declare Function LJUBAMD Lib "c:\modsim3617\modsimdl\MODSIM.DLL" (ByVal JobPath As String, ByVal NameOfJob As String) As Long
'Public Declare Function BetaAMD Lib "c:\modsim3617\modsimdl\MODSIM.DLL" Alias "BETAAMD" (ByVal JobPath As String, ByVal NameOfJob As String, ByRef Parameters As Single) As Long
Public Declare Sub LevMarCurveFit Lib "ModsimCurveFit.dll" (ByVal JobPath As String, ByVal JobFileName As String, ByRef NOPAR As Long, ByRef Parameters As Single, ByRef LowB As Single, ByRef UpB As Single, ByRef SumOfSquares As Single, ByRef LowerConfLim As Single, ByRef UpperConfLim As Single, ByRef ConfLevel As Single)
Public Declare Function LJUBAMD Lib "MODSIM.DLL" (ByVal JobPath As String, ByVal NameOfJob As String) As Long
Public Declare Function BetaAMD Lib "MODSIM.DLL" Alias "BETAAMD" (ByVal JobPath As String, ByVal NameOfJob As String, ByRef Parameters As Single) As Long


Sub Main()
'*********
  Dim TestNumber As Single
  'VGPath = "C:\Vetgraph\"
  VGPath = CurDir & "\"
  'FileReadPath = "C:\Modsim3619\Modsimvb\"
  FileReadPath = CurDir & "\"
  
  JobPath = CurDir & "\"
  'Get the MODSIM environment variable
  EnvString = Environ("MODSIM")
  If EnvString <> "" Then
    JobPath = EnvString
  Else
    'Get the TEMP environment variable
    EnvString = Environ("TEMP")
    If EnvString <> "" Then
      JobPath = EnvString & "\"
    Else
      'Get the Tmp environment variable
      EnvString = Environ("Tmp")
      If EnvString <> "" Then
        JobPath = EnvString & "\"
      End If
    End If
  End If
  Call ReadInitializationFile
  
  'Write the path data to the diagnostic file in case the DLL cannot be found.
  Open RTrim(JobPath) & "DiagDLL.txt" For Output As 13
  Print #13, " Path information before calling the DLL"
  Print #13, "Job path " & RTrim(JobPath)
  Print #13, "Vetgraph path " & RTrim(VGPath)
  Print #13, "File read path " & RTrim(FileReadPath)
  Print #13, "Job save path " & RTrim(JobSavePath)
  Print #13, "The DLL was not entered"
  Close 13

  'Check the decimal indicator
  Open RTrim(JobPath) & "TempFile" For Output As #31
  TestNumber = 1.2345
  Print #31, Format(TestNumber, "0.0000")
  Close #31
  Open RTrim(JobPath) & "TempFile" For Input As 31
  Line Input #31, WSTRING
  Close #31
  If Mid(WSTRING, 2, 1) = "," Then
    MsgBox "Decimal indicator should be set to . in Regional Settings before running MODSIM" & Chr(13) & Chr(10) & "Check your system documentation for information on this setting", vbInformation, "Information"
    End
  End If
  
  ProfessionalVersion = True
  AcademicVersion = True
  FastParameterChange = True
  ChangedFlowsheet = False
  ChangedA_MDiagramParameters = False
  Call SetConversionFactors
  MODSIM.Show
  CurveFit.Top = 0
  CurveFit.Left = 0
  DisplayOffsetY = 160
  DisplayOffsetX = 80
  CurveFit.Show
  
End Sub

Private Sub SetConversionFactors()
'*********************************
With m_cm_ft
  .Number = 3
  .Factor(1) = 1#
  .Factor(2) = 0.01
  .Factor(3) = 0.3048
End With
With m_cm_inch
  .Number = 3
  .Factor(1) = 1#
  .Factor(2) = 0.01
  .Factor(3) = 0.0254
End With
With mm_cm_inch
  .Number = 3
  .Factor(1) = 0.001
  .Factor(2) = 0.01
  .Factor(3) = 0.0254
End With
With m_feet
  .Number = 2
  .Factor(1) = 1
  .Factor(2) = 0.3048
End With
With mm_inch
  .Number = 2
  .Factor(1) = 0.001
  .Factor(2) = 0.0254
End With
With mm_cm_m
  .Number = 3
  .Factor(1) = 0.001
  .Factor(2) = 0.01
  .Factor(3) = 1
End With
With mm_cm_m_inch
  .Number = 4
  .Factor(1) = 0.001
  .Factor(2) = 0.01
  .Factor(3) = 1
  .Factor(4) = 0.0254
End With
With micron_mm_cm_m_inch
  .Number = 5
  .Factor(1) = 0.000001
  .Factor(2) = 0.001
  .Factor(3) = 0.01
  .Factor(4) = 1
  .Factor(5) = 0.0254
End With
With micron_mm_cm_m_inch_ft
  .Number = 6
  .Factor(1) = 0.000001
  .Factor(2) = 0.001
  .Factor(3) = 0.01
  .Factor(4) = 1
  .Factor(5) = 0.0254
  .Factor(6) = 0.3048
End With
With cm_m_inch_ft
  .Number = 4
  .Factor(1) = 0.01
  .Factor(2) = 1
  .Factor(3) = 0.0254
  .Factor(4) = 0.3048
End With
With cubm_cubft
  .Number = 2
  .Factor(1) = 1#
  .Factor(2) = 0.028317
End With
With kgps_stonphr_tonnephr_ltonphr
  .Number = 4
  .Factor(1) = 1
  .Factor(3) = 0.27778
  .Factor(2) = 0.252
  .Factor(4) = 0.28224
End With
With kgps_tonnephr_stonphr_ltonphr
  .Number = 4
  .Factor(1) = 1
  .Factor(2) = 0.27778
  .Factor(3) = 0.252
  .Factor(4) = 0.28224
End With
With kgps_tonnephr_stonphr_Mtonnepyr
  .Number = 4
  .Factor(1) = 1
  .Factor(2) = 0.27778
  .Factor(3) = 0.25199576
  .Factor(4) = 31.71
End With
With cubmpers_cubmperhr_USgalpermin
  .Number = 3
  .Factor(1) = 1#
  .Factor(2) = 0.00027778
  .Factor(3) = 0.000063089
End With
With cubmps_cubmphr_USgalpmin_ImpGalpmin_cubftpmin
  .Number = 5
  .Factor(1) = 1#
  .Factor(2) = 0.00027778
  .Factor(3) = 0.000063089
  .Factor(4) = 0.000075766
  .Factor(5) = 0.00047195
End With
With Pas_cp
  .Number = 2
  .Factor(1) = 1#
  .Factor(2) = 0.001
End With
With kgpcubm_lbpcubft
  .Number = 2
  .Factor(1) = 1#
  .Factor(2) = 16.018
End With
With gpercc_kgpcubm_lbpcubft
  .Number = 3
  .Factor(1) = 1000#
  .Factor(2) = 1#
  .Factor(3) = 16.018
End With
With mps_ftpmin
  .Number = 2
  .Factor(1) = 1#
  .Factor(2) = 0.00508
End With
With kWhpert_kWhperst
  .Number = 2
  .Factor(1) = 1#
  .Factor(2) = 1.1025
End With
End Sub

Sub ReadInitializationFile()
'***************************
  'Reads the initialization file
  On Error GoTo ErrHandler
  Open RTrim(JobPath) & "MODSIM.INI" For Input As #31
  Input #31, OptionShowFlowsheet
  Input #31, JobSavePath
  Close #31
  Exit Sub

ErrHandler:
  'MsgBox "Could not read initialization file" & vbCrLf & "Defaults will be used", vbExclamation, "Information"
  OptionShowFlowsheet = False
  JobSavePath = CurDir & "\"
  Close #31
  Exit Sub
End Sub

Function ROSIN_RAMM(CUMSIZE As Single, D632 As Single, Lambda As Single) As Single
'************************************************************************
  'Calculates the Rosin Rammler cumulative distribution
  ROSIN_RAMM = 1 - Exp(-(CUMSIZE / D632) ^ Lambda)
End Function

Sub PRNT_8G104(Vector() As Single, START As Integer, N As Integer, NFILE As Integer)
'***********************************************************************************
  'Print a vector in FORTRAN 8G10.4 format. Prints N elements starting at START.
  Dim I As Integer
  Dim count As Integer
  On Error GoTo ErrHandler
  count = 0
  For I = 1 To N
    count = count + 1
    If count = 8 Or I = N Then
      Print #NFILE, Format$(Vector(START + I - 1), "#.0000E+#") + " "
      count = 0
    Else
      Print #NFILE, Format$(Vector(START + I - 1), "#.0000E+#") + " ";
    End If
  Next I
  Exit Sub
  
ErrHandler:
  MsgBox "Error writng to file", vbCritical, "ERROR"
  Exit Sub
End Sub

Function MATCH(I As Integer, J As Integer) As Integer
'****************************************************
'This is a dummy to stand in place of the equivalent function in mdlFlowsheet in MODSIM.
MATCH = 0
End Function


Sub RightButtonProc(Which As String, X As Single, Y As Single)
'Activate this dummy procedure for applications that do not define a right button procedure
End Sub

