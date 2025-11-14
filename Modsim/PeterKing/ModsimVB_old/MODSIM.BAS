Attribute VB_Name = "MdlMODSIM"
Option Explicit
Public ScreenBorder As Integer
Public ScreenStatusHeight As Integer
Public Const TITLE As String = "MODSIM - Modular Simulator for Ore Dressing Plants    "

'Declare functions in the MODSIM dll.
'Note: Pass the VB strings (BSTRs) by value because the DLL expects an LSTR. See VB Programmer's Guide p649.
Public Declare Function INORDCALC Lib "c:\MySoftware\modsim\modsimdl\MODSIM.DLL" (ByVal JobPath As String) As Integer
Public Declare Function SIMOP Lib "c:\MySoftware\modsim\modsimdl\MODSIM.DLL" (ByVal JobPath As String, ByRef CumOut As Integer) As Integer
Public Declare Function LJUBAMD Lib "c:\MySoftware\modsim\modsimdl\MODSIM.DLL" (ByVal JobPath As String, ByVal NameOfJob As String) As Long
Public Declare Function BetaAMD Lib "c:\MySoftware\modsim\modsimdl\MODSIM.DLL" Alias "BETAAMD" (ByVal JobPath As String, ByVal NameOfJob As String, ByRef Parameters As Single) As Long
'Public Declare Function INORDCALC Lib "MODSIM.DLL" (ByVal JobPath As String) As Integer
'Public Declare Function SIMOP Lib "MODSIM.DLL" (ByVal JobPath As String, ByRef CumOut As Integer) As Integer
'Public Declare Function LJUBAMD Lib "MODSIM.DLL" (ByVal JobPath As String, ByVal NameOfJob As String) As Long
'Public Declare Function BetaAMD Lib "MODSIM.DLL" Alias "BETAAMD" (ByVal JobPath As String, ByVal NameOfJob As String, ByRef Parameters As Single) As Long

Public Static Sub Main()
'***********************
'This starts MODSIM
  Dim EnvString As String, TestNumber As Single, N As Integer
  
  On Error GoTo ErrHandler
  
  'Diagnostic.Show  'Used to find hidden sequencing bugs

  'FileReadPath = "C:\Mysoftware\Modsim\Modsimvb\"
  FileReadPath = App.Path & "\"
  VGPath = "C:\Mysoftware\vetgraph\"
  'VGPath = App.Path & "\"
                                                     
  'Set demo version flag as required
  DemoVersion = False
  'Set Student version flag
  StudentVersion = False
  'Set Butterworth-Heinemann version flag
  BHVersion = False
  'Set academic version
  AcademicVersion = False
  'Set professional version flag as required
  ProfessionalVersion = True
  PreviousURL = ""
  PreviousUsername = ""
                                                     
  'Set a default job path
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
  
  App.HelpFile = RTrim(FileReadPath) & "Modsim.chm"
  
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
  
  Call SetConversionFactors
  
  ScreenBorder = 50
  ScreenStatusHeight = 400
  Set MGPage(0) = New FLSheet
  PageNumber = 0
  'Set the coal flag default
  CURRENT.Flags = "FFFFF"
  
  FlyoutSizeFactor = 1
  
  Load MODSIM
  MODSIM.Left = ScreenBorder
  MODSIM.Top = ScreenBorder
  If Not OptionShowFullScreen And Screen.width > 12000 Then
    MODSIM.width = 12000 - 2 * ScreenBorder
    MODSIM.height = 9000 - ScreenBorder - ScreenStatusHeight
  Else
    MODSIM.height = Screen.height - ScreenBorder - ScreenStatusHeight
    MODSIM.width = Screen.width - 2 * ScreenBorder
  End If
  MODSIM.Show
  DisplayOffsetX = MODSIM.Left + 80
  DisplayOffsetY = MODSIM.Top + 660
  Exit Sub
  
ErrHandler:
  If Err.Number = 75 Or Err.Number = 76 Then
    MsgBox "Could not write to path " & RTrim(JobPath), vbCritical, "ERROR"
  Else
    MsgBox "Error when starting MODSIM", vbCritical, "ERROR"
  End If
End Sub

Private Sub SetConversionFactors()
'*********************************
With m_cm_ft
  .Number = 3
  .Factor(1) = 1#
  .Factor(2) = 0.01
  .Factor(3) = 0.3048
End With
With mm_cm_inch
  .Number = 3
  .Factor(1) = 0.001
  .Factor(2) = 0.01
  .Factor(3) = 0.0254
End With
With m_cm_inch
  .Number = 3
  .Factor(1) = 1#
  .Factor(2) = 0.01
  .Factor(3) = 0.0254
End With
With cm_m_inch
  .Number = 3
  .Factor(1) = 0.01
  .Factor(2) = 1
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
With mm_cm_m_inch
  .Number = 4
  .Factor(1) = 0.001
  .Factor(2) = 0.01
  .Factor(3) = 1
  .Factor(4) = 0.0254
End With
With mm_cm_m
  .Number = 3
  .Factor(1) = 0.001
  .Factor(2) = 0.01
  .Factor(3) = 1
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
With kgps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin
  .Number = 5
  .Factor(1) = 0.001
  .Factor(2) = 0.00027778
  .Factor(3) = 0.00047195
  .Factor(4) = 0.000063089
  .Factor(5) = 0.000075766
End With
With kgps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin_Lpermin
  .Number = 6
  .Factor(1) = 0.001
  .Factor(2) = 0.00027778
  .Factor(3) = 0.00047195
  .Factor(4) = 0.000063089
  .Factor(5) = 0.000075766
  .Factor(6) = 0.000016667
End With
With cubmps_cubmphr_cubftpmin_USgalpmin_ImpGalpmin
  .Factor(1) = 1#
  .Factor(2) = 0.00027778
  .Factor(3) = 0.00047195
  .Factor(4) = 0.000063089
  .Factor(5) = 0.000075766
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
  .Factor(2) = 1 / 0.90718
End With
With cubmpkg_emupg
  .Number = 2
  .Factor(1) = 1#
  .Factor(2) = 0.004 * Pi
End With
End Sub

Sub ReadInitializationFile()
'***************************
  'Reads the initialization file
  On Error GoTo ErrHandler
  Open RTrim(JobPath) & "MODSIM.INI" For Input As #31
  Input #31, OptionShowFlowsheet
  Input #31, OptionShowFullScreen
  Input #31, JobSavePath
  Close #31
  Exit Sub

ErrHandler:
  'MsgBox "Could not read initialization file" & vbCrLf & "Defaults will be used", vbExclamation, "Information"
  OptionShowFlowsheet = False
  OptionShowFullScreen = False
  JobSavePath = JobPath
  Close #31
  Exit Sub
End Sub

Sub WriteInitializationFile()
'****************************
  'Write initialization file for re-entry
  On Error GoTo ErrHandler
  Open RTrim(JobPath) & "MODSIM.INI" For Output As #31
  Write #31, OptionShowFlowsheet
  Write #31, OptionShowFullScreen
  Write #31, JobSavePath
  Close #31
  Exit Sub
ErrHandler:
  'MsgBox "Could not write initialization file", vbExclamation, "Information"
  Close #31
  Exit Sub
End Sub
