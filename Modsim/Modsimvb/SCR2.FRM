VERSION 5.00
Begin VB.Form SCR2 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model SCR2"
   ClientHeight    =   5385
   ClientLeft      =   45
   ClientTop       =   855
   ClientWidth     =   8280
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
   Icon            =   "Scr2.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5385
   ScaleWidth      =   8280
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   3120
      TabIndex        =   38
      Top             =   1440
      Width           =   855
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Screen characteristics"
      Height          =   1815
      Left            =   120
      TabIndex        =   19
      Top             =   240
      Width           =   4680
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   8
         Left            =   3000
         TabIndex        =   3
         Top             =   1440
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   5
         Left            =   3000
         TabIndex        =   2
         Text            =   "Text2"
         Top             =   960
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   4
         Left            =   3000
         TabIndex        =   1
         Text            =   "Text1"
         Top             =   720
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   3
         Left            =   3000
         TabIndex        =   30
         Text            =   "Text1"
         Top             =   480
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   2
         Left            =   3000
         TabIndex        =   0
         Text            =   "Text1"
         Top             =   240
         Width           =   855
      End
      Begin VB.Label Label4 
         BackColor       =   &H00C0C0C0&
         Caption         =   "%"
         Height          =   255
         Left            =   3960
         TabIndex        =   40
         Top             =   1200
         Width           =   375
      End
      Begin VB.Label lblParameter 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Surface water retained on oversize %"
         Height          =   255
         Index           =   7
         Left            =   240
         TabIndex        =   39
         Top             =   1200
         Width           =   2775
      End
      Begin VB.Label Label8 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "degrees"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   3960
         TabIndex        =   21
         Top             =   240
         Width           =   600
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Number of screens in parallel"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   8
         Left            =   240
         TabIndex        =   24
         Top             =   1440
         Width           =   2775
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Length of screen"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   240
         TabIndex        =   29
         Top             =   480
         Width           =   2775
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Width of screen"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   4
         Left            =   240
         TabIndex        =   22
         Top             =   720
         Width           =   2775
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Bulk density of material"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   5
         Left            =   240
         TabIndex        =   23
         Top             =   960
         Width           =   2775
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Angle of inclination"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   240
         TabIndex        =   20
         Top             =   240
         Width           =   2775
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mesh characteristics"
      Height          =   1335
      Left            =   120
      TabIndex        =   16
      Top             =   2400
      Width           =   4575
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   0
         Left            =   1320
         TabIndex        =   4
         Text            =   "Text1"
         Top             =   240
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   1
         Left            =   1320
         TabIndex        =   5
         Text            =   "Text1"
         Top             =   480
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   6
         Left            =   3960
         TabIndex        =   33
         Text            =   "Text3"
         Top             =   960
         Width           =   480
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "0=resilient 1=woven wire 2=punched steel"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   6
         Left            =   240
         TabIndex        =   32
         Top             =   960
         Width           =   3735
      End
      Begin VB.Label Label10 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Screen type:"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   31
         Top             =   720
         Width           =   3120
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Wire diameter"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   18
         Top             =   480
         Width           =   1215
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mesh size"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   17
         Top             =   240
         Width           =   1215
      End
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for screen dimensions"
      Height          =   615
      Left            =   5280
      TabIndex        =   25
      Top             =   1320
      Width           =   2895
      Begin VB.OptionButton OptScreen 
         BackColor       =   &H00C0C0C0&
         Caption         =   "feet"
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   7
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   960
      End
      Begin VB.OptionButton OptScreen 
         BackColor       =   &H00C0C0C0&
         Caption         =   "meter"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   6
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
   End
   Begin VB.Frame Frame5 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for mesh size"
      Height          =   615
      Left            =   5280
      TabIndex        =   26
      Top             =   2040
      Width           =   2895
      Begin VB.OptionButton Optmesh 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   9
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   960
      End
      Begin VB.OptionButton Optmesh 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   8
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
   End
   Begin VB.Frame Frame6 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for wire diameter"
      Height          =   615
      Left            =   5280
      TabIndex        =   27
      Top             =   2760
      Width           =   2895
      Begin VB.OptionButton Optwire 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   11
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   960
      End
      Begin VB.OptionButton Optwire 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   10
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   720
      End
   End
   Begin VB.Frame Frame7 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for bulk density"
      Height          =   615
      Left            =   5280
      TabIndex        =   28
      Top             =   3480
      Width           =   2895
      Begin VB.OptionButton OptDensity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "lbs/ft^3"
         Height          =   240
         Index           =   1
         Left            =   1320
         TabIndex        =   13
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton OptDensity 
         BackColor       =   &H00C0C0C0&
         Caption         =   "kg/m^3"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   12
         TabStop         =   0   'False
         ToolTipText     =   "Left click to select: Right click to convert to."
         Top             =   240
         Value           =   -1  'True
         Width           =   1095
      End
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7200
      TabIndex        =   14
      Top             =   4320
      Width           =   960
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   5640
      TabIndex        =   15
      Top             =   4320
      Width           =   960
   End
   Begin VB.Frame Frame8 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   6480
      TabIndex        =   34
      Top             =   120
      Width           =   1695
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   37
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         Height          =   240
         Left            =   120
         TabIndex        =   36
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
         TabIndex        =   35
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu Mnuprint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "SCR2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub CmdAccept_Click()
'****************************
  Dim response As Long
  'Check validity of the wire diameter
  If Val(Parameter(1).text) >= Val(Parameter(0).text) Then
    MsgBox "Wire diameter cannot exceed the mesh size", vbExclamation, "WARNING"
    Parameter(1).SetFocus
    Exit Sub
  End If
  Call MakeNew(CURRMODELDATA(ID))
  If CURRMODELDATA(ID).PARAM(1) < 0.001 Then
    MsgBox "This model is not suitable for screens with mesh size less than 1mm", vbExclamation, "WARNING"
    Parameter(0).SetFocus
    Exit Sub
  End If
  If CURRMODELDATA(ID).PARAM(6) < 500 Then
    response = MsgBox("Bulk desity is unusually low. Is this OK?", vbYesNo, "WARNING")
    If response = vbNo Then
      Parameter(5).SetFocus
      Exit Sub
    End If
  End If
  Unload SCR2
End Sub

Private Sub CmdCancel_Click()
  Unload SCR2
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
  If CURRMODELDATA(ID).Model <> "SCR2" Then
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
        Parameter(0).text = Str$(50.8)
        Parameter(1).text = Str$(12.5)
        Parameter(2).text = Str$(0)
        Parameter(3).text = Str$(3.66)
        Parameter(4).text = Str$(1.22)
        Parameter(5).text = Str$(1603)
        Parameter(6).text = Str$(1)
        Parameter(7).text = Str$(0)
        Parameter(8).text = CStr(1)
  OptScreen(0).Value = True
  OptDensity(0).Value = True
  Optmesh(0).Value = True
  Optwire(0).Value = True
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  For I = 1 To NDATA.NOPAR
        Parameter(I - 1).text = Str$(NDATA.PARAM(I))
  Next I
  OptScreen(0).Value = -1
  Optmesh(0).Value = -1
  Parameter(0).text = Str$(Val(Parameter(0).text) * 1000)
  Optwire(0).Value = -1
  Parameter(1).text = Str$(Val(Parameter(1).text) * 1000)
  OptDensity(0).Value = -1
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Dim Factor As Single
  CHANGE_FLAG = 0
  NDATA.Model = "SCR2"
  NDATA.NOPAR = 9
  For I = 1 To 9
        NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  If (Optmesh(0).Value = -1) Then Factor = 0.001
  If (Optmesh(1).Value = -1) Then Factor = 0.0254
  NDATA.PARAM(1) = NDATA.PARAM(1) * Factor
  If (Optwire(1).Value = -1) Then Factor = 0.001
  If (Optwire(1).Value = -1) Then Factor = 0.0254
  NDATA.PARAM(2) = NDATA.PARAM(2) * Factor
  Factor = 1!
  If (OptScreen(1).Value = -1) Then Factor = 0.3048
  NDATA.PARAM(4) = NDATA.PARAM(4) * Factor
  NDATA.PARAM(5) = NDATA.PARAM(5) * Factor
  Factor = 1!
  If (OptDensity(1).Value = -1) Then Factor = 16.02
  NDATA.PARAM(6) = NDATA.PARAM(6) * Factor
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

Private Sub Optfeet_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optfeet_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optinch_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optinch_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optkgm3_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optkgm3_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optlbsft3_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optlbsft3_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub Optmeter_Click()
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub Optmeter_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub optmesh_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optmesh_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub
Private Sub Optmesh_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = mm_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If Optmesh(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(0).text = Format(Val(Parameter(0).text) * Factor, "##.000")
  Optmesh(Index).Value = True
End If
End Sub

Private Sub optwire_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optwire_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub
Private Sub Optwire_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = mm_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If Optwire(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(1).text = Format(Val(Parameter(1).text) * Factor, "##.000")
  Optwire(Index).Value = True
End If
End Sub

Private Sub optScreen_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optScreen_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptScreen_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = m_feet
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptScreen(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(3).text = Format(Val(Parameter(3).text) * Factor, "##.000")
  Parameter(4).text = Format(Val(Parameter(4).text) * Factor, "##.000")
  OptScreen(Index).Value = True
End If
End Sub


Private Sub optDensity_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optDensity_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub

Private Sub OptDensity_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = kgpcubm_lbpcubft
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptDensity(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(5).text = Format(Val(Parameter(5).text) * Factor, "#000.0")
  OptDensity(Index).Value = True
End If
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
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
  Repeat.TxtParameterName = Me.lblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  IDIFF = 1
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


