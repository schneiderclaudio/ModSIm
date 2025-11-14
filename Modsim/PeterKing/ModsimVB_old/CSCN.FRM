VERSION 5.00
Begin VB.Form CSCN 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model"
   ClientHeight    =   4305
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   9000
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "CSCN.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   Picture         =   "CSCN.frx":030A
   ScaleHeight     =   4305
   ScaleWidth      =   9000
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   3480
      TabIndex        =   42
      Top             =   1920
      Width           =   855
   End
   Begin VB.Frame fraThree 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify mesh and length for each compound section"
      Height          =   1575
      Left            =   120
      TabIndex        =   33
      Top             =   2640
      Width           =   5415
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   9
         Left            =   2790
         TabIndex        =   41
         Top             =   1140
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   8
         Left            =   2790
         TabIndex        =   39
         Top             =   660
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   7
         Left            =   870
         TabIndex        =   37
         Top             =   1140
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   6
         Left            =   870
         TabIndex        =   35
         Top             =   660
         Width           =   855
      End
      Begin VB.Label Label5 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Discharge end"
         Height          =   495
         Left            =   4440
         TabIndex        =   46
         Top             =   600
         Width           =   735
      End
      Begin VB.Label Label 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Feed end"
         Height          =   495
         Left            =   120
         TabIndex        =   45
         Top             =   600
         Width           =   375
      End
      Begin VB.Label LblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Length 2"
         Height          =   255
         Index           =   9
         Left            =   2790
         TabIndex        =   40
         Top             =   900
         Width           =   975
      End
      Begin VB.Label LblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mesh size 2"
         Height          =   255
         Index           =   8
         Left            =   2790
         TabIndex        =   38
         Top             =   420
         Width           =   975
      End
      Begin VB.Label LblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Length 1"
         Height          =   255
         Index           =   7
         Left            =   870
         TabIndex        =   36
         Top             =   900
         Width           =   975
      End
      Begin VB.Label LblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mesh size 1"
         Height          =   255
         Index           =   6
         Left            =   870
         TabIndex        =   34
         Top             =   420
         Width           =   975
      End
      Begin VB.Line Line3 
         X1              =   2295
         X2              =   2295
         Y1              =   360
         Y2              =   1440
      End
      Begin VB.Line Line1 
         X1              =   2265
         X2              =   2265
         Y1              =   360
         Y2              =   1440
      End
      Begin VB.Shape Shape2 
         BorderWidth     =   2
         FillColor       =   &H00808080&
         FillStyle       =   6  'Cross
         Height          =   1095
         Left            =   720
         Top             =   360
         Width           =   3615
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of velocity"
      Height          =   615
      Left            =   4920
      TabIndex        =   30
      Top             =   1800
      Width           =   1935
      Begin VB.OptionButton optVelocity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "ft/min"
         Height          =   255
         Index           =   1
         Left            =   840
         TabIndex        =   32
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton optVelocity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m/s"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   31
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   615
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   3480
      TabIndex        =   28
      Top             =   1320
      Width           =   855
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of aperture size"
      ForeColor       =   &H00000000&
      Height          =   615
      Index           =   1
      Left            =   4800
      TabIndex        =   24
      Top             =   360
      Width           =   2295
      Begin VB.OptionButton OptApertureSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   255
         Index           =   1
         Left            =   840
         TabIndex        =   27
         Tag             =   "ApSize"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   735
      End
      Begin VB.OptionButton OptApertureSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   26
         Tag             =   "ApSize"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   600
      End
      Begin VB.OptionButton OptApertureSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   1560
         TabIndex        =   25
         TabStop         =   0   'False
         Tag             =   "ApSize"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   720
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   3480
      TabIndex        =   22
      Top             =   1560
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   3480
      TabIndex        =   19
      Top             =   1080
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   3480
      TabIndex        =   17
      Top             =   840
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   3480
      TabIndex        =   15
      Top             =   600
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   3480
      TabIndex        =   10
      Top             =   360
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   3480
      TabIndex        =   1
      Top             =   120
      Width           =   855
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   7920
      TabIndex        =   8
      Top             =   1200
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7920
      TabIndex        =   7
      Top             =   1920
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   7200
      TabIndex        =   0
      Top             =   0
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   9
         TabStop         =   0   'False
         Tag             =   "New"
         ToolTipText     =   "Display data set that you are constructing"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   2
         TabStop         =   0   'False
         Tag             =   "Current"
         ToolTipText     =   "Display data set used in the previous simulation"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   3
         Tag             =   "Default"
         ToolTipText     =   "Display the default data set for this unit model"
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of screen size"
      ForeColor       =   &H00000000&
      Height          =   615
      Index           =   0
      Left            =   4920
      TabIndex        =   4
      Top             =   1080
      Width           =   1935
      Begin VB.OptionButton OptScreenSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "feet"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   960
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   960
      End
      Begin VB.OptionButton OptScreenSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   6
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   600
      End
   End
   Begin VB.Label Label4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "kg/kg"
      Height          =   255
      Left            =   4440
      TabIndex        =   44
      Top             =   1560
      Width           =   495
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of screens in parallel"
      Height          =   255
      Index           =   11
      Left            =   120
      TabIndex        =   43
      Top             =   1920
      Width           =   3375
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Velocity of travel down screen"
      Height          =   255
      Index           =   5
      Left            =   120
      TabIndex        =   29
      Top             =   1320
      Width           =   3375
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Attachment factor for fine particles"
      Height          =   255
      Index           =   10
      Left            =   120
      TabIndex        =   23
      Top             =   1560
      Width           =   3375
   End
   Begin VB.Label Label3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "%"
      Height          =   255
      Left            =   4440
      TabIndex        =   21
      Top             =   1080
      Width           =   255
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Surface water on screen oversize"
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   20
      Top             =   1080
      Width           =   3375
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Screen width"
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   18
      Top             =   840
      Width           =   3375
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Power exponent for separated region  sigma"
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   16
      Top             =   600
      Width           =   3375
   End
   Begin VB.Label Label2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "1/s"
      Height          =   255
      Left            =   4440
      TabIndex        =   14
      Top             =   360
      Width           =   375
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "1/s"
      Height          =   255
      Left            =   4440
      TabIndex        =   13
      Top             =   120
      Width           =   375
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Kinetic constant for separated region s0"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   12
      Top             =   360
      Width           =   3375
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Kinetic constant for crowded region  k0"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   11
      Top             =   120
      Width           =   3375
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "CSCN"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Sub CmdAccept_Click()
'*********************
  Call MakeNew(CURRMODELDATA(ID))
  Unload Me
End Sub

Sub CmdCancel_Click()
'*********************
  Unload Me
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  Me.Hide  'Must hide me before doing anything on any other form because I'm modal
  Unload Help  'In case help was called
  If FastParameterChange Then
    UnitMods.cmdQuit_Click
    FastParameterChange = False
  End If
End Sub

Sub Form_Load()
'***************
  On Error GoTo ErrHandler
  Caption = Caption & " CSCN for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "CSCN" Then
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  Exit Sub
  
ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  Exit Sub
End Sub

Sub LoadDefault()
'*****************
  Parameter(0).text = CStr(1)
  Parameter(1).text = CStr(0.5)
  Parameter(2).text = CStr(2)
  Parameter(3).text = CStr(1.2)
  Parameter(4).text = CStr(2)
  Parameter(5).text = CStr(0.5) 'Nominal 90 - 120 ft/min Nordberg Process Machinery Reference Manual 1st edition 1976 p.14-9
  Parameter(6).text = CStr(1)
  Parameter(7).text = CStr(0.5)
  Parameter(8).text = CStr(5)
  Parameter(9).text = CStr(0.5)
  Parameter(10).text = CStr(0.005)
  Parameter(11).text = CStr(1)
  OptScreenSize(0).Value = True
  OptApertureSize(0).Value = True
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
Dim I As Integer
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
  Next I
  OptScreenSize(0).Value = True
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Dim ScaleFactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "CSCN"
  NDATA.NOPAR = 12
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  'Convert the screen size to meters
  ScaleFactor = 1#
  If OptScreenSize(1).Value Then ScaleFactor = 0.3048
  NDATA.PARAM(4) = NDATA.PARAM(4) * ScaleFactor
  NDATA.PARAM(8) = NDATA.PARAM(8) * ScaleFactor
  NDATA.PARAM(10) = NDATA.PARAM(10) * ScaleFactor
  ScaleFactor = 1
  If OptApertureSize(1).Value Then ScaleFactor = 10
  If OptApertureSize(2).Value Then ScaleFactor = 25.4
  NDATA.PARAM(7) = NDATA.PARAM(7) * ScaleFactor
  NDATA.PARAM(9) = NDATA.PARAM(9) * ScaleFactor
  ScaleFactor = 1
  If Me.optVelocity(1).Value Then ScaleFactor = 0.00508
  NDATA.PARAM(6) = NDATA.PARAM(6) * ScaleFactor
End Sub

Private Sub MnuAccept_Click()
'****************************
   Call CmdAccept_Click
End Sub

Private Sub MnuClose_Click()
'***************************
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form
PrintForm
End Sub

Sub optApertureSize_Click(Index As Integer)
'****************************************
  If Screen.ActiveControl.Tag = "ApSize" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub optApertureSize_LostFocus(Index As Integer)
'********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optApertureSize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*********************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = mm_cm_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptApertureSize(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(6).text = Format(Val(Parameter(6).text) * Factor, "#0.####")
  Parameter(8).text = Format(Val(Parameter(8).text) * Factor, "#0.####")
  OptApertureSize(Index).Value = True
End If

End Sub

Sub OptCurrent_Click()
'**********************
  If Screen.ActiveControl.Tag = "Current" Then
        Call LoadNew(CURRMODELDATA(ID))
  End If
End Sub

Sub OptDefault_Click()
'**********************
  If Screen.ActiveControl.Tag = "Default" Then
        Call LoadDefault
  End If
End Sub

Sub OptNew_Click()
'******************
  'Must trigger a load only when NEW is switched on with a click.
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Sub optVelocity_Click(Index As Integer)
'****************************************
  If Screen.ActiveControl.Tag = "Velocity" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub optVelocity_LostFocus(Index As Integer)
'********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optVelocity_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*********************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = mps_ftpmin
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If optVelocity(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(5).text = Format(Val(Parameter(5).text) * Factor, "##0.###")
  optVelocity(Index).Value = True
End If

End Sub

Sub Parameter_Change(Index As Integer)
'**************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub Parameter_LostFocus(Index As Integer)
'*****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Sub optScreenSize_Click(Index As Integer)
'****************************************
  If Screen.ActiveControl.Tag = "Size" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub optScreenSize_LostFocus(Index As Integer)
'********************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptScreensize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = m_feet
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptScreenSize(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(3).text = Format(Val(Parameter(3).text) * Factor, "#0.####")
  Parameter(7).text = Format(Val(Parameter(7).text) * Factor, "#0.####")
  Parameter(9).text = Format(Val(Parameter(9).text) * Factor, "#0.####")
  OptScreenSize(Index).Value = True
End If
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation or for curve fitting.
Dim I As Integer, IDIFF As Integer
Dim ParameterCanBeUsed As Boolean
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  ParameterCanBeUsed = False
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = Me.LblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  If Index < 12 Then
    ParameterCanBeUsed = True
    IDIFF = 1
  End If
  
  If ParameterCanBeUsed Then
    'Check if this parameter is already selected and display if necessary.
    For I = 1 To 5
      If UnitID(I) = ID Then
        If ParameterNumber(I) = Index + IDIFF Then
          Repeat.LstLevel.AddItem CStr(I), 0
          Repeat.LstLevel.ListIndex = 0
          Repeat.LstLevel.Enabled = False
          Repeat.TxtParameterName = ParameterName(I)
          Repeat.TxtValue(0) = CStr(StartValue(I))
          Repeat.TxtValue(1) = CStr(EndValue(I))
          Repeat.TxtValue(2) = CStr(StepValue(I))
        End If
      End If
    Next I
    Repeat.Show 1
    If Repeat.Level > 0 Then
      RepUnitNumber(Repeat.Level) = UnitMods.Unit
      UnitID(Repeat.Level) = ID
      ParameterNumber(Repeat.Level) = Index + IDIFF
    End If
  End If
End If
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub
