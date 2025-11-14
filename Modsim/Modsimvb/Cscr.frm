VERSION 5.00
Begin VB.Form CSCR 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model CSCR"
   ClientHeight    =   4875
   ClientLeft      =   180
   ClientTop       =   1350
   ClientWidth     =   9225
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
   Icon            =   "Cscr.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4875
   ScaleWidth      =   9225
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Surface condition of material"
      Height          =   1335
      Left            =   2400
      TabIndex        =   35
      Top             =   2400
      Width           =   2535
      Begin VB.OptionButton optSurfaceCondition 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Artificially dried"
         Height          =   255
         Index           =   3
         Left            =   240
         TabIndex        =   39
         Top             =   960
         Width           =   2055
      End
      Begin VB.OptionButton optSurfaceCondition 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Dry crushed"
         Height          =   255
         Index           =   2
         Left            =   240
         TabIndex        =   38
         Top             =   720
         Value           =   -1  'True
         Width           =   2055
      End
      Begin VB.OptionButton optSurfaceCondition 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Surface wet"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   37
         Top             =   480
         Width           =   2055
      End
      Begin VB.OptionButton optSurfaceCondition 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Sticky"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   36
         Top             =   240
         Width           =   2055
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Aperture shape"
      Height          =   1575
      Left            =   240
      TabIndex        =   29
      Top             =   2160
      Width           =   1815
      Begin VB.OptionButton optShape 
         BackColor       =   &H00C0C0C0&
         Caption         =   "4:1 rectangle"
         Height          =   210
         Index           =   4
         Left            =   240
         TabIndex        =   34
         Top             =   1200
         Width           =   1455
      End
      Begin VB.OptionButton optShape 
         BackColor       =   &H00C0C0C0&
         Caption         =   "3:1 rectangle"
         Height          =   210
         Index           =   3
         Left            =   240
         TabIndex        =   33
         Top             =   960
         Width           =   1455
      End
      Begin VB.OptionButton optShape 
         BackColor       =   &H00C0C0C0&
         Caption         =   "2:1 rectangle"
         Height          =   210
         Index           =   2
         Left            =   240
         TabIndex        =   32
         Top             =   720
         Width           =   1455
      End
      Begin VB.OptionButton optShape 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Square"
         Height          =   210
         Index           =   1
         Left            =   240
         TabIndex        =   31
         Top             =   480
         Value           =   -1  'True
         Width           =   1215
      End
      Begin VB.OptionButton optShape 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Round"
         Height          =   210
         Index           =   0
         Left            =   240
         TabIndex        =   30
         Top             =   240
         Width           =   975
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   5
      Left            =   3000
      TabIndex        =   27
      Top             =   1680
      Width           =   900
   End
   Begin VB.Frame frmScreenSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of screen size"
      Height          =   615
      Left            =   4560
      TabIndex        =   24
      Tag             =   "Size"
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1440
      Width           =   1815
      Begin VB.OptionButton optSize2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "feet"
         Height          =   255
         Index           =   1
         Left            =   840
         TabIndex        =   26
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   735
      End
      Begin VB.OptionButton optSize2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   25
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   495
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   6
      Left            =   2520
      TabIndex        =   23
      Top             =   3960
      Width           =   600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   2
      Left            =   3000
      TabIndex        =   20
      Top             =   960
      Width           =   900
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   7080
      TabIndex        =   10
      Top             =   3720
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   8160
      TabIndex        =   9
      Top             =   3720
      Width           =   960
   End
   Begin VB.Frame Frame1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   7440
      TabIndex        =   13
      Top             =   360
      Width           =   1695
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   8
         Tag             =   "Default"
         Top             =   720
         Width           =   1095
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Left            =   120
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   975
      End
   End
   Begin VB.Frame Frame2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of mesh size"
      ForeColor       =   &H00000000&
      Height          =   855
      Left            =   4560
      TabIndex        =   14
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   480
      Width           =   2535
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   480
         Width           =   735
      End
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   2
         Left            =   1800
         TabIndex        =   4
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   600
      End
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   1
         Left            =   960
         TabIndex        =   3
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
      Begin VB.OptionButton OptSize1 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   2
         TabStop         =   0   'False
         Tag             =   "Size"
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   720
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   0
      Left            =   3000
      TabIndex        =   1
      Top             =   480
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   3
      Left            =   3000
      TabIndex        =   18
      Top             =   1200
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   4
      Left            =   3000
      TabIndex        =   19
      Top             =   1440
      Width           =   900
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   3000
      TabIndex        =   0
      Top             =   720
      Width           =   900
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Width of screen"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   120
      TabIndex        =   28
      Top             =   1680
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of screens in parallel"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   8
      Left            =   240
      TabIndex        =   22
      Top             =   3960
      Width           =   2175
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Angle of inclination  deg."
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   120
      TabIndex        =   21
      Top             =   960
      Width           =   2775
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
      TabIndex        =   15
      Top             =   0
      Width           =   9135
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mesh size"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   120
      TabIndex        =   11
      Top             =   480
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Surface water on screen oversize  %"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   120
      TabIndex        =   16
      Top             =   1200
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Length of screen"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   120
      TabIndex        =   17
      Top             =   1440
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Open Area  %"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   120
      TabIndex        =   12
      Top             =   720
      Width           =   2775
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
Attribute VB_Name = "CSCR"
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
  Unload CSCR
End Sub

Private Sub CmdCancel_Click()
  Unload CSCR
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
  Caption = Caption & " on unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "CSCR" Then
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
'************************
Dim I As Integer
  Parameter(0).text = CStr(0.05)
  Parameter(1).text = CStr(50)
  Parameter(2).text = CStr(0)
  For I = 0 To 2
    Parameter(I).Visible = True
    lblParameter(I).Visible = True
  Next I
  optShape(1).Value = True
  optSurfaceCondition(2).Value = True
  Parameter(3).text = CStr(0)
  Parameter(4).text = CStr(1.2)
  Parameter(5).text = CStr(1#)
  OptSize1(2).Value = -1
  Parameter(6) = CStr(1)
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  
  On Error GoTo ErrHandler
  Parameter(0).text = CStr(NDATA.PARAM(1))
  Parameter(1).text = CStr(NDATA.PARAM(2))
  Parameter(2).text = CStr(NDATA.PARAM(3))
  I = CInt(NDATA.PARAM(4))
  optShape(I - 1).Value = True
  I = CInt(NDATA.PARAM(5))
  optSurfaceCondition(I - 1) = True
  Parameter(3).text = CStr(NDATA.PARAM(6))
  Parameter(4).text = CStr(NDATA.PARAM(7))
  Parameter(5).text = CStr(NDATA.PARAM(8))
  OptSize1(2).Value = True
  optSize2(0).Value = True
  Parameter(6).text = CStr(NDATA.PARAM(9))
  Exit Sub
  
ErrHandler:
  MsgBox "An inconsistency exists in this data", vbExclamation, "ERROR"
  Exit Sub
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim ScaleFactor As Single
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA.Model = "CSCR"
  NDATA.NOPAR = 9
  NDATA.PARAM(1) = Val(Parameter(0).text)
  NDATA.PARAM(2) = Val(Parameter(1).text)
  NDATA.PARAM(3) = Val(Parameter(2).text)
  NDATA.PARAM(6) = Val(Parameter(3).text)
  For I = 1 To 5
    If optShape(I - 1).Value = True Then NDATA.PARAM(4) = I
  Next I
  For I = 1 To 4
    If optSurfaceCondition(I - 1).Value = True Then NDATA.PARAM(5) = I
  Next I
  For I = 7 To 9
        NDATA.PARAM(I) = Val(Parameter(I - 3).text)
  Next I
  'Convert the size to meters
  ScaleFactor = 1#
  If OptSize1(0).Value Then ScaleFactor = 0.001
  If OptSize1(1).Value Then ScaleFactor = 0.01
  If OptSize1(3).Value Then ScaleFactor = 0.0254
  NDATA.PARAM(1) = NDATA.PARAM(1) * ScaleFactor
  If optSize2(0).Value Then ScaleFactor = 1
  If optSize2(1).Value Then ScaleFactor = 0.3048
  NDATA.PARAM(7) = NDATA.PARAM(7) * ScaleFactor
  NDATA.PARAM(8) = NDATA.PARAM(8) * ScaleFactor
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
  Parameter(0).text = Format(Val(Parameter(0).text) * Factor, "#0.####")
  OptSize1(Index).Value = True
End If
End Sub

Private Sub optSize2_Click(Index As Integer)
  If Screen.ActiveControl.Tag = "Size" Then
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
  Parameter(4).text = Format(Val(Parameter(4).text) * Factor, "#0.##")
  Parameter(5).text = Format(Val(Parameter(5).text) * Factor, "#0.##")
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
  If Index = 1 Then
       ' Instruction.Caption = "% transmission efficieny to undersize for all sizes smaller than the mesh"
       ' Instruction.Visible = -1
  End If
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
  IDIFF = 1
  If Index >= 3 Then IDIFF = 3
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
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub


