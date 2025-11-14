VERSION 5.00
Begin VB.Form DSC1 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Specify parameters for model DSC1"
   ClientHeight    =   4755
   ClientLeft      =   345
   ClientTop       =   1770
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
   Icon            =   "Dsc1.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4755
   ScaleWidth      =   9480
   Begin VB.Frame fmeSize2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of screen size"
      Height          =   735
      Left            =   3960
      TabIndex        =   36
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   2640
      Width           =   2415
      Begin VB.OptionButton OptSize2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "feet"
         Height          =   375
         Index           =   1
         Left            =   1320
         TabIndex        =   38
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   975
      End
      Begin VB.OptionButton OptSize2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   375
         Index           =   0
         Left            =   240
         TabIndex        =   37
         Tag             =   "Size2"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   6120
      TabIndex        =   35
      Top             =   2160
      Width           =   495
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   480
      Left            =   8280
      TabIndex        =   7
      Top             =   3600
      Width           =   960
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   480
      Left            =   7080
      TabIndex        =   8
      Top             =   3600
      Width           =   960
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Both decks"
      Height          =   600
      Left            =   120
      TabIndex        =   27
      Top             =   3480
      Width           =   3615
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   8
         Left            =   2640
         TabIndex        =   29
         Top             =   240
         Visible         =   0   'False
         Width           =   855
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Width of screen"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   8
         Left            =   120
         TabIndex        =   28
         Top             =   240
         Visible         =   0   'False
         Width           =   1935
      End
   End
   Begin VB.CheckBox ChkDimensions 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specify screen dimensions"
      Height          =   240
      Left            =   3840
      TabIndex        =   12
      Tag             =   "Dims"
      Top             =   1440
      Width           =   2775
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of mesh size"
      Height          =   855
      Left            =   3840
      TabIndex        =   10
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   480
      Width           =   2760
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   3
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   480
         Width           =   735
      End
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   0
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   240
         Index           =   1
         Left            =   1080
         TabIndex        =   1
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   2
         Left            =   1920
         TabIndex        =   2
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   600
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Lower deck"
      Height          =   1320
      Index           =   1
      Left            =   120
      TabIndex        =   20
      Top             =   2040
      Width           =   3615
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   6
         Left            =   2640
         TabIndex        =   33
         Top             =   720
         Visible         =   0   'False
         Width           =   850
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   7
         Left            =   2640
         TabIndex        =   24
         Top             =   960
         Visible         =   0   'False
         Width           =   850
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   5
         Left            =   2640
         TabIndex        =   23
         Top             =   480
         Width           =   850
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   4
         Left            =   2640
         TabIndex        =   22
         Top             =   240
         Width           =   850
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Surface water on oversize  %"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   6
         Left            =   120
         TabIndex        =   31
         Top             =   720
         Visible         =   0   'False
         Width           =   2415
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Transmission efficiency %"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   5
         Left            =   120
         TabIndex        =   25
         Top             =   480
         Width           =   2175
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Length of screen"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   7
         Left            =   120
         TabIndex        =   26
         Top             =   960
         Visible         =   0   'False
         Width           =   2175
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mesh size"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   4
         Left            =   120
         TabIndex        =   21
         Top             =   240
         Width           =   2175
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Upper deck"
      Height          =   1335
      Index           =   0
      Left            =   120
      TabIndex        =   13
      Top             =   480
      Width           =   3615
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   2
         Left            =   2640
         TabIndex        =   32
         Top             =   720
         Visible         =   0   'False
         Width           =   800
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   3
         Left            =   2640
         TabIndex        =   17
         Top             =   960
         Visible         =   0   'False
         Width           =   800
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   1
         Left            =   2640
         TabIndex        =   16
         Top             =   480
         Width           =   800
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   0
         Left            =   2640
         TabIndex        =   15
         Top             =   240
         Width           =   800
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Surface water on oversize  %"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   30
         Top             =   720
         Visible         =   0   'False
         Width           =   2415
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Transmission efficiency %"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   18
         Top             =   480
         Width           =   2175
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Length of screen"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   19
         Top             =   960
         Visible         =   0   'False
         Width           =   2175
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mesh size"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   14
         Top             =   240
         Width           =   2175
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1200
      Left            =   7560
      TabIndex        =   9
      Top             =   480
      Width           =   1695
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   6
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         Height          =   240
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         Height          =   240
         Left            =   120
         TabIndex        =   4
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of screens in parallel"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   9
      Left            =   3840
      TabIndex        =   34
      Top             =   2160
      Width           =   2175
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
      TabIndex        =   11
      Top             =   0
      Width           =   8160
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
Attribute VB_Name = "DSC1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub ChkDimensions_Click()
'********************************
Dim I As Integer
If Screen.ActiveControl.Tag = "Dims" Then
  If OptNew.Value = 0 Then OptNew.Value = -1
  CHANGE_FLAG = True
  If ChkDimensions.Value = 0 Then
        lblParameter(8).Visible = 0
        lblParameter(3).Visible = 0
        lblParameter(7).Visible = 0
        Parameter(8).Visible = 0
        Parameter(3).Visible = 0
        Parameter(7).Visible = 0
        fmeSize2.Visible = False
  Else
        For I = 1 To 8
          lblParameter(I).Visible = True
          Parameter(I).Visible = True
        Next I
        fmeSize2.Visible = True
  End If
End If
End Sub

Private Sub ChkDimensions_GotFocus()
  Instruction.Caption = "Click if you want to specify the screen dimensions"
  Instruction.Visible = -1
End Sub

Private Sub ChkDimensions_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  Instruction.Visible = 0
End Sub

Private Sub CmdAccept_Click()
  Call MakeNew(CURRMODELDATA(ID))
  Unload DSC1
End Sub

Private Sub CmdCancel_Click()
  Unload DSC1
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
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  CHANGE_FLAG = 0
  ID = UnitMods.ID
' Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "DSC1" Then
        OptCurrent.Enabled = 0
        LoadDefault
        Call MakeNew(NEWDATA)
  Else
        Call LoadNew(CURRMODELDATA(ID))
        Call MakeNew(NEWDATA)
  End If
  Exit Sub

ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  Exit Sub
End Sub

Private Sub LoadDefault()
  Parameter(0).text = CStr(0.05)
  Parameter(1).text = CStr(90)
  Parameter(2).text = CStr(0)
  Parameter(3).text = CStr(2)
  Parameter(4).text = CStr(0.01)
  Parameter(5).text = CStr(90)
  Parameter(6).text = CStr(0)
  Parameter(7).text = CStr(2)
  Parameter(8).text = CStr(1)
  Parameter(9).text = CStr(1)
  OptSize1(2).Value = -1
  optSize2(0).Value = -1
  ChkDimensions.Value = 0
  lblParameter(3).Visible = 0
  lblParameter(7).Visible = 0
  lblParameter(8).Visible = 0
  Parameter(3).Visible = 0
  Parameter(7).Visible = 0
  Parameter(8).Visible = 0
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Parameter(0).text = CStr(NDATA.PARAM(1))
  Parameter(1).text = CStr(100 * NDATA.PARAM(2))
  Parameter(2).text = CStr(NDATA.PARAM(3))
  Parameter(4).text = CStr(NDATA.PARAM(4))
  Parameter(5).text = CStr(100 * NDATA.PARAM(5))
  Parameter(6).text = CStr(NDATA.PARAM(6))
  Parameter(9).text = CStr(NDATA.PARAM(11))
  If NDATA.PARAM(7) > 0 Then
    ChkDimensions.Value = 1
    For I = 1 To 8
      lblParameter(I).Visible = True
      Parameter(I).Visible = True
    Next I
    Parameter(3).text = CStr(NDATA.PARAM(8))
    Parameter(7).text = CStr(NDATA.PARAM(9))
    Parameter(8).text = CStr(NDATA.PARAM(10))
    fmeSize2.Visible = True
  Else
    ChkDimensions.Value = 0
    lblParameter(0).Visible = True
    lblParameter(1).Visible = True
    lblParameter(2).Visible = True
    lblParameter(3).Visible = 0
    lblParameter(4).Visible = True
    lblParameter(5).Visible = True
    lblParameter(6).Visible = True
    lblParameter(7).Visible = 0
    lblParameter(8).Visible = 0
    Parameter(0).Visible = True
    Parameter(1).Visible = True
    Parameter(2).Visible = True
    Parameter(3).Visible = 0
    Parameter(4).Visible = True
    Parameter(5).Visible = True
    Parameter(6).Visible = True
    Parameter(7).Visible = 0
    Parameter(8).Visible = 0
    fmeSize2.Visible = False
  End If
  OptSize1(2).Value = -1
  optSize2(0).Value = True
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim ScaleFactor As Single
  CHANGE_FLAG = 0
  NDATA.Model = "DSC1"
  NDATA.NOPAR = 11
  NDATA.PARAM(1) = Val(Parameter(0).text)
  NDATA.PARAM(2) = 0.01 * Val(Parameter(1).text)
  NDATA.PARAM(3) = Val(Parameter(2).text)
  NDATA.PARAM(4) = Val(Parameter(4).text)
  NDATA.PARAM(5) = 0.01 * Val(Parameter(5).text)
  NDATA.PARAM(6) = Val(Parameter(6).text)
  NDATA.PARAM(7) = ChkDimensions.Value
  NDATA.PARAM(8) = Val(Parameter(3).text)
  NDATA.PARAM(9) = Val(Parameter(7).text)
  NDATA.PARAM(10) = Val(Parameter(8).text)
  NDATA.PARAM(11) = Val(Parameter(9).text)
  'Convert the size to meters
  ScaleFactor = 1!
  If OptSize1(0).Value Then ScaleFactor = 0.001
  If OptSize1(1).Value Then ScaleFactor = 0.01
  If OptSize1(3).Value Then ScaleFactor = 0.0254
  NDATA.PARAM(1) = NDATA.PARAM(1) * ScaleFactor
  NDATA.PARAM(4) = NDATA.PARAM(4) * ScaleFactor
  If optSize2(0).Value Then ScaleFactor = 1
  If optSize2(1).Value Then ScaleFactor = 0.3048
  NDATA.PARAM(8) = NDATA.PARAM(8) * ScaleFactor
  NDATA.PARAM(9) = NDATA.PARAM(9) * ScaleFactor
  NDATA.PARAM(10) = NDATA.PARAM(10) * ScaleFactor
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
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

Private Sub optSize1_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Size" Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize1_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optsize1_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = mm_cm_m_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptSize1(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(0).text = Format(Val(Parameter(0).text) * Factor, "0.000E+#")
  Parameter(4).text = Format(Val(Parameter(4).text) * Factor, "0.000E+#")
  OptSize1(Index).Value = True
End If
End Sub

Private Sub optSize2_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Size2" Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize2_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optsize2_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = m_feet
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If optSize2(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(3).text = Format(Val(Parameter(3).text) * Factor, "0.000E+#")
  Parameter(7).text = Format(Val(Parameter(7).text) * Factor, "0.000E+#")
  Parameter(8).text = Format(Val(Parameter(8).text) * Factor, "0.000E+#")
  optSize2(Index).Value = True
End If
End Sub

Private Sub Parameter_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
  Select Case Index
        Case 0
          Instruction.Caption = "% transmission efficieny to undersize for all sizes smaller than the mesh"
        Case 1
          Instruction.Caption = "Mesh size for the upper deck"
        Case 2
          Instruction.Caption = "Length of the screen on the upper deck"
        Case 3
          Instruction.Caption = "% transmission efficieny to undersize for all sizes smaller than the mesh"
        Case 4
          Instruction.Caption = "Mesh size for the lower deck"
        Case 5
          Instruction.Caption = "Length of the screen on the lower deck"
        Case 6
          Instruction.Caption = "Width of the screen frame"
  End Select
  Instruction.Visible = -1
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  Instruction.Visible = 0
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
Dim I As Integer, IDIFF As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = Me.lblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  If Index <= 2 Then
    IDIFF = 1
  ElseIf Index = 3 Then
    IDIFF = 5
  ElseIf Index <= 6 Then
    IDIFF = 0
  Else
    IDIFF = 2
  End If
    'Check if this paprameter is already selected and display if necessary.
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
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub

