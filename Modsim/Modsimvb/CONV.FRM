VERSION 5.00
Begin VB.Form Conv 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Specify parameters for model CONV"
   ClientHeight    =   4845
   ClientLeft      =   45
   ClientTop       =   1095
   ClientWidth     =   9480
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
   Icon            =   "Conv.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4845
   ScaleWidth      =   9480
   Begin VB.CommandButton CmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   7080
      TabIndex        =   31
      Top             =   3840
      Width           =   960
   End
   Begin VB.CommandButton CmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   8280
      TabIndex        =   30
      Top             =   3840
      Width           =   960
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of size"
      Height          =   855
      Left            =   4320
      TabIndex        =   7
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   600
      Width           =   2280
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   240
         Index           =   0
         Left            =   240
         TabIndex        =   4
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "feet"
         Height          =   240
         Index           =   3
         Left            =   1200
         TabIndex        =   23
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   480
         Width           =   960
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   240
         Index           =   2
         Left            =   240
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   480
         Width           =   960
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for belt speed"
      Height          =   720
      Left            =   4320
      TabIndex        =   24
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1560
      Width           =   2295
      Begin VB.OptionButton OptBeltSpeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "ft/min"
         Height          =   240
         Index           =   1
         Left            =   1080
         TabIndex        =   26
         Tag             =   "Speed"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton OptBeltSpeed 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m/s"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   25
         TabStop         =   0   'False
         Tag             =   "Speed"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   4
      Left            =   2400
      TabIndex        =   19
      Top             =   3120
      Width           =   1440
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   5
      Left            =   2400
      TabIndex        =   20
      Top             =   3360
      Width           =   1440
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for bulk density"
      Height          =   600
      Left            =   4320
      TabIndex        =   27
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   3000
      Width           =   2415
      Begin VB.OptionButton OptBulkDensity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "lbs/ft^3"
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   29
         Tag             =   "Bulk"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton OptBulkDensity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kg.m^3"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   28
         TabStop         =   0   'False
         Tag             =   "Bulk"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   1095
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   3
      Left            =   2640
      TabIndex        =   18
      Top             =   1560
      Width           =   1440
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   0
      Left            =   2640
      TabIndex        =   10
      Top             =   720
      Width           =   1440
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   1
      Left            =   2640
      TabIndex        =   16
      Top             =   960
      Width           =   1440
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   2
      Left            =   2640
      TabIndex        =   17
      Top             =   1200
      Width           =   1440
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   7800
      TabIndex        =   3
      Top             =   360
      Width           =   1575
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   0
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   2
         TabStop         =   0   'False
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         Height          =   240
         Left            =   120
         TabIndex        =   1
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1440
      End
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Angle of repose  degrees"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   0
      TabIndex        =   14
      Top             =   3120
      Width           =   2295
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Material properties:"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   0
      TabIndex        =   22
      Top             =   2880
      Width           =   2400
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Bulk density"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   0
      TabIndex        =   15
      Top             =   3360
      Width           =   2295
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Belt parameters:"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   0
      TabIndex        =   21
      Top             =   480
      Width           =   1920
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Width of belt"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   0
      TabIndex        =   9
      Top             =   720
      Width           =   2535
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Freeboard"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   0
      TabIndex        =   11
      Top             =   960
      Width           =   2535
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Belt speed"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   0
      TabIndex        =   13
      Top             =   1560
      Width           =   2535
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Idler trough angle  degrees"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   0
      TabIndex        =   12
      Top             =   1200
      Width           =   2535
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   0
      TabIndex        =   8
      Top             =   0
      Width           =   9360
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "Conv"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
  Call MakeNew(CURRMODELDATA(ID))
  Unload Conv
End Sub

Private Sub CmdCancel_Click()
  Unload Conv
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

Private Sub Form_Load()
'**********************
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "CONV" Then
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  Exit Sub

ErrHandler:
  Exit Sub
End Sub

Private Sub LoadDefault()
  Parameter(0).text = Str$(1)
  Parameter(1).text = Str$(0.2)
  Parameter(2).text = Str$(20)
  Parameter(3).text = Str$(2)
  Parameter(4).text = Str$(35)
  Parameter(5).text = Str$(1600)

  OptSize(1).Value = -1
  OptBeltSpeed(0).Value = -1
  OptBulkDensity(0).Value = -1
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).text = Str$(NDATA.PARAM(I))
  Next I
  OptSize(1).Value = -1
  OptBeltSpeed(0).Value = -1
  OptBulkDensity(0).Value = -1
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA.Model = "CONV"
  NDATA.NOPAR = 6
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  'Convert the size to meters
  If OptSize(0).Value Then
    NDATA.PARAM(1) = NDATA.PARAM(1) * 0.01
    NDATA.PARAM(2) = NDATA.PARAM(2) * 0.01
  ElseIf OptSize(2).Value Then
   NDATA.PARAM(1) = NDATA.PARAM(1) * 0.3048
   NDATA.PARAM(2) = NDATA.PARAM(2) * 0.3048
  ElseIf OptSize(3).Value Then
    NDATA.PARAM(1) = NDATA.PARAM(1) * 0.0254
    NDATA.PARAM(2) = NDATA.PARAM(2) * 0.0254
  End If
  If OptBeltSpeed(1).Value Then NDATA.PARAM(4) = NDATA.PARAM(4) * 0.00508
  If OptBulkDensity(1).Value Then NDATA.PARAM(6) = NDATA.PARAM(6) * 16.018
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form.
PrintForm
End Sub

Private Sub OptCurrent_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Current" Then
    Call LoadNew(CURRMODELDATA(ID))
  End If
End Sub

Private Sub OptDefault_Click()
'*****************************
  If Screen.ActiveControl.Tag = "Default" Then
    Call LoadDefault
  End If
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub optSize_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Size" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optBeltSpeed_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Speed" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optBeltSpeed_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optBulkDensity_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Bulk" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optBulkDensity_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Parameter_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
Dim I As Integer, IDIFF As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = ""
  Repeat.TxtValue(0) = ""
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
    'Check if this paprameter is already selected and display if necessary.
    For I = 1 To 5
      If UnitID(I) = ID Then
        If ParameterNumber(I) = Index + 1 Then
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
      ParameterNumber(Repeat.Level) = Index + 1
    End If
End If
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub Optsize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = cm_m_inch_ft
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptSize(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(0).text = Format(Val(Parameter(0).text) * Factor, "0.000E+#")
  Parameter(1).text = Format(Val(Parameter(1).text) * Factor, "0.000E+#")
  OptSize(Index).Value = True
End If
End Sub

Private Sub OptBeltSpeed_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = mps_ftpmin
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptBeltSpeed(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(3).text = Format(Val(Parameter(3).text) * Factor, "0.000E+#")
  OptBeltSpeed(Index).Value = True
End If
End Sub

Private Sub OptBulkDensity_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = kgpcubm_lbpcubft
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptBulkDensity(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(5).text = Format(Val(Parameter(5).text) * Factor, "0.000E+#")
  OptBulkDensity(Index).Value = True
End If
End Sub

