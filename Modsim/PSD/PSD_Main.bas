Attribute VB_Name = "PSD_Main"
Option Explicit

Public Const cdlOFNHideReadOnlyFlag As Integer = 4

Public JobPath As String * 256
Public VGPath As String
Public FileNumber As Integer
Public WSTRING As String
Public JOBNAME As String
Public ID As Integer
Public NumberOfDataSets As Integer
Public REGModel As String

Public Type SIZEDATA
    NDC As Integer
    ID As Integer
    SET_NAME As String * 30
    CUMSIZE(1 To 25) As Single
    size(1 To 25) As Single
    fraction(1 To 25) As Single
End Type
Public CURRDATA(30) As SIZEDATA
Public BMProduct(10) As SIZEDATA

Public Type GraphSetupData
  Number As Integer
  GraphList(30) As Integer
  SizeCode As Integer
  CoordinateCode As Integer
  LineCode As Integer
  TruncationCode As Integer
  TruncationSize As Single
  GraphData(30) As SIZEDATA
End Type
Public SetupData As GraphSetupData

Public Type ModelData
  ID As Integer
  MODEL As String * 4
  Unit  As Integer
  NOPAR As Integer
  PARAM(1 To 320) As Single
  TEXTSTRING(1 To 30) As String
End Type
Public CURRMODELDATA(0 To 10) As ModelData
Public CURRMODELIndex(10) As Integer

Public Type ThreeComponent
  Defined As Boolean
  HEADING As String
  ChkHeading As Boolean
  SUBHEADING As String
  ChkSubHeading As Boolean
  size As Single
  Azimuth As Single
  Colatitude As Single
  Component(3) As String
  Value(1 To 43) As Single
  MaxValue As Single
  SCALEValue As Single
End Type
Public MMIAIndex(0 To 42) As Integer
Public ValIndex(0 To 42) As Integer
Public CURR3COMPDATA As ThreeComponent

Public Type ConversionFactor
  Number As Integer
  Factor(1 To 10) As Double
End Type
Public CFactor As ConversionFactor

Public PARAMIndex As Integer
Public m_cm_ft As ConversionFactor
Public m_cm_inch As ConversionFactor
Public micron_mm_cm_m_inch As ConversionFactor
Public Pas_cp As ConversionFactor
Public cubmpers_cubmperhr_USgalpermin As ConversionFactor
Public cubmps_cubmphr_USgalpmin_ImpGalpmin_cubftpmin As ConversionFactor

Public ProfessionalVersion As Boolean

Public CurrentIndex As Integer
Public FeedIndex As Integer
Public EnvString As String
Public ProcessID As Long

'Public Declare Function BetaFunction Lib "c:\MSDEV\Projects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (X As Double, Alpha As Double, beta As Double) As Double
'Public Declare Function GammaFunction Lib "c:\MSDEV\Projects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (Byref Z As Single) As single
'Public Declare Sub Kolmogorov_Smirnov Lib "c:\MSDEV\Projects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (ByRef NOBSX as long, ByRef X As single,ByRef NOBSY as long,ByRef Y as single,Byref D as single,ByRef Prob as single)
'Public Declare Sub TandFtest Lib "c:\MSDEV\Projects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (ByVal JobPath as string,ByRef NOBSX as long, ByRef X As single,ByRef NOBSY as long,ByRef Y as single,Byref T as single,ByRef F as single,Byref ProbT as single,ByRef ProbF as single)
'Public Declare Sub LevMarREG Lib "C:\MSDEV\Projects\LevMarREG\Debug\LevMarREG.dll" (ByVal JobPath As String, ByRef NOPAR As Long, ByRef Parameters As Double, ByRef LowerConfLim As Single, ByRef UpperConfLim As Single, ByRef ConfLevel As Single)
Public Declare Function BetaFunction Lib "c:\MSDEVProjects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (X As Double, Alpha As Double, beta As Double) As Double
Public Declare Function GammaFunction Lib "c:\MSDEVProjects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (ByRef Z As Single) As Single
Public Declare Sub Kolmogorov_Smirnov Lib "c:\MSDEVProjects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (ByRef NOBSX As Long, ByRef X As Single, ByRef NOBSY As Long, ByRef Y As Single, ByRef d As Single, ByRef Prob As Single)
Public Declare Sub TandFtest Lib "c:\MSDEVProjects\GraphicsForMinProcDLL\Debug\GraphicsForMinProc.dll" (ByVal JobPath As String, ByRef NOBSX As Long, ByRef X As Single, ByRef NOBSY As Long, ByRef Y As Single, ByRef T As Single, ByRef F As Single, ByRef ProbT As Single, ByRef ProbF As Single)
Public Declare Sub LevMarREG Lib "C:\MSDEVProjects\LevMarREG\Debug\LevMarREG.dll" (ByVal JobPath As String, ByRef NOPAR As Long, ByRef Parameters As Double, ByRef LowerConfLim As Single, ByRef UpperConfLim As Single, ByRef ConfLevel As Single)
'Public Declare Function BetaFunction Lib "GraphicsForMinProc.dll" (X As Double, Alpha As Double, beta As Double) As Double
'Public Declare Function GammaFunction Lib "GraphicsForMinProc.dll" (ByRef Z As Single) As single
'Public Declare Kolmogorov_Smirnov Lib "GraphicsForMinProc.dll" (ByRef NOBSX as long, ByRef X As single,ByRef NOBSY as long,ByRef Y as single,Byref D as single,ByRef Prob as single)
'Public Declare Sub TandFtest Lib "GraphicsForMinProc.dll" (ByVal JobPath as string,ByRef NOBSX as long, ByRef X As single,ByRef NOBSY as long,ByRef Y as single,Byref T as single,ByRef F as single,Byref ProbT as single,ByRef ProbF as single)
'Public Declare Sub LevMarREG Lib "LevMarREG.dll" (ByVal JobPath As String, ByRef NOPAR As Long, ByRef Parameters As Double, ByRef LowerConfLim As Single, ByRef UpperConfLim As Single, ByRef ConfLevel As Single)

Public Sub Main()
'****************
  Dim TestFile As String
  
  'Start up the particle size plotting program
  VGPath = "c:\Mysoftware\Vetgraph\"
  'VGPath = CurDir & "\"
  
  JobPath = CurDir & "\"
  'Get the PSD environment variable
  EnvString = Environ("PSD")
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
   'MsgBox "Job path " & RTrim(JobPath)
  Call SetConversionFactors
  
  MDIPSD.Left = 0
  MDIPSD.Top = 0
  'MDIPSD.Width = Screen.Width
  'MDIPSD.Height = Screen.Height - 20
  MDIPSD.Show
  CurrentIndex = 0
  NumberOfDataSets = 0
End Sub

Function ROSIN_RAMM(CUMSIZE As Single, D632 As Single, Lambda As Single) As Single
'************************************************************************
  'Calculates the Rosin Rammler cumulative distribution
  ROSIN_RAMM = 1 - Exp(-(CUMSIZE / D632) ^ Lambda)
End Function

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
With micron_mm_cm_m_inch
  .Number = 5
  .Factor(1) = 0.000001
  .Factor(2) = 0.001
  .Factor(3) = 0.01
  .Factor(4) = 1
  .Factor(5) = 0.0254
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
End Sub
