VERSION 5.00
Begin VB.Form Cone 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model "
   ClientHeight    =   6345
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
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   6345
   ScaleWidth      =   9000
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for concentrate flowrate model"
      Height          =   1095
      Left            =   120
      TabIndex        =   30
      Top             =   4680
      Width           =   6135
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   11
         Left            =   1560
         TabIndex        =   37
         Top             =   720
         Width           =   615
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   10
         Left            =   4320
         TabIndex        =   33
         Top             =   360
         Width           =   495
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   9
         Left            =   3240
         TabIndex        =   32
         Top             =   360
         Width           =   495
      End
      Begin VB.Label Label6 
         BackColor       =   &H00C0C0C0&
         Caption         =   "tonne/hr"
         Height          =   255
         Left            =   5040
         TabIndex        =   38
         Top             =   360
         Width           =   615
      End
      Begin VB.Label Label5 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Slope coefficient"
         Height          =   255
         Left            =   120
         TabIndex        =   36
         Top             =   720
         Width           =   1335
      End
      Begin VB.Label Label4 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Y"
         Height          =   255
         Left            =   4080
         TabIndex        =   35
         Top             =   360
         Width           =   135
      End
      Begin VB.Label Label3 
         BackColor       =   &H00C0C0C0&
         Caption         =   "X"
         Height          =   255
         Left            =   3000
         TabIndex        =   34
         Top             =   360
         Width           =   135
      End
      Begin VB.Label Label2 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Coordinates of point of convergence"
         Height          =   255
         Left            =   120
         TabIndex        =   31
         Top             =   360
         Width           =   2775
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   285
      Index           =   8
      Left            =   7560
      TabIndex        =   27
      Top             =   2475
      Width           =   375
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cone configuration and variable slot settings"
      Height          =   3495
      Left            =   120
      TabIndex        =   10
      ToolTipText     =   "Choose the configuration of the cone bank"
      Top             =   1080
      Width           =   4335
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   7
         Left            =   3720
         TabIndex        =   25
         Top             =   240
         Width           =   255
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   6
         Left            =   3360
         TabIndex        =   24
         Top             =   240
         Width           =   255
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   5
         Left            =   3000
         TabIndex        =   23
         Top             =   240
         Width           =   255
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   4
         Left            =   2640
         TabIndex        =   22
         Top             =   240
         Width           =   255
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   3
         Left            =   2280
         TabIndex        =   21
         Top             =   240
         Width           =   255
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   2
         Left            =   1920
         TabIndex        =   20
         Top             =   240
         Width           =   255
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "2DSVSV.DSV"
         Height          =   300
         Index           =   8
         Left            =   120
         TabIndex        =   19
         Tag             =   "Config"
         Top             =   3120
         Width           =   1575
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "3DSVSV"
         Height          =   300
         Index           =   7
         Left            =   120
         TabIndex        =   18
         Tag             =   "Config"
         Top             =   2760
         Width           =   1455
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "2DSVSV"
         Height          =   300
         Index           =   6
         Left            =   120
         TabIndex        =   17
         Tag             =   "Config"
         Top             =   2400
         Width           =   1455
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "DSVSV"
         Height          =   300
         Index           =   5
         Left            =   120
         TabIndex        =   16
         Tag             =   "Config"
         Top             =   2040
         Width           =   1455
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "4DSV"
         Height          =   300
         Index           =   4
         Left            =   120
         TabIndex        =   15
         Tag             =   "Config"
         Top             =   1680
         Width           =   1095
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "3DSV"
         Height          =   300
         Index           =   3
         Left            =   120
         TabIndex        =   14
         Tag             =   "Config"
         Top             =   1320
         Width           =   1695
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "2DSV"
         Height          =   300
         Index           =   2
         Left            =   120
         TabIndex        =   13
         Tag             =   "Config"
         Top             =   960
         Width           =   1575
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "DSV"
         Height          =   300
         Index           =   1
         Left            =   120
         TabIndex        =   12
         Tag             =   "Config"
         Top             =   600
         Width           =   1335
      End
      Begin VB.OptionButton OptConfiguration 
         BackColor       =   &H00C0C0C0&
         Caption         =   "SV"
         Height          =   300
         Index           =   0
         Left            =   120
         TabIndex        =   11
         Tag             =   "Config"
         Top             =   240
         Value           =   -1  'True
         Width           =   1215
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   2400
      TabIndex        =   7
      Top             =   720
      Width           =   975
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   2400
      TabIndex        =   1
      Top             =   480
      Width           =   975
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   6480
      TabIndex        =   5
      Top             =   5160
      Width           =   1080
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7920
      TabIndex        =   4
      Top             =   5160
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   7200
      TabIndex        =   0
      Top             =   360
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "New"
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
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slot 6"
      Height          =   255
      Index           =   7
      Left            =   5520
      TabIndex        =   44
      Top             =   1080
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slot 5"
      Height          =   255
      Index           =   6
      Left            =   4800
      TabIndex        =   43
      Top             =   1080
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slot 4"
      Height          =   255
      Index           =   5
      Left            =   5520
      TabIndex        =   42
      Top             =   720
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slot 3"
      Height          =   255
      Index           =   4
      Left            =   4800
      TabIndex        =   41
      Top             =   720
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slot 2"
      Height          =   255
      Index           =   3
      Left            =   5520
      TabIndex        =   40
      Top             =   360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slot 1"
      Height          =   255
      Index           =   2
      Left            =   4800
      TabIndex        =   39
      Top             =   360
      Visible         =   0   'False
      Width           =   495
   End
   Begin VB.Label Instruction 
      BackColor       =   &H0080FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   29
      Top             =   0
      Width           =   8505
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Slot setting for double cones is always 5 and these need not be specified here."
      Height          =   375
      Left            =   4680
      TabIndex        =   28
      Top             =   1560
      Width           =   4095
   End
   Begin VB.Label LblMiddlings 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of stage concentrates making up the middlings stream"
      Height          =   495
      Left            =   4680
      TabIndex        =   26
      Top             =   2400
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of cones in parallel"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   9
      Top             =   720
      Width           =   2175
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Specific stratification constant"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   8
      Top             =   480
      Width           =   2175
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
Attribute VB_Name = "Cone"
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
  Dim N As Integer
  Call MakeNew(NEWDATA)
  If NEWDATA.PARAM(3) <= 5 Then N = 5
  If NEWDATA.PARAM(3) = 6 Then N = 6
  If NEWDATA.PARAM(3) = 7 Then N = 7
  If NEWDATA.PARAM(3) = 8 Then N = 8
  If NEWDATA.PARAM(3) = 9 Then N = 7
  If Val(Parameter(8).text) > NEWDATA.NOPAR - N Then
    Parameter(8).SetFocus
    MsgBox "Too many concentrates to middlings for this configuration", 16, "ERROR"
    Exit Sub
  End If
  Call MakeNew(CURRMODELDATA(ID))
  Unload Cone
End Sub

Sub CmdCancel_Click()
'*********************
  Unload Cone
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
  Caption = Caption & " CONE for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
  If NOSPLIT(UnitMods.Unit) <= 2 Then
    Parameter(8).Enabled = False
    LblMiddlings.Enabled = False
  Else
    LblMiddlings.Enabled = True
    Parameter(8).Enabled = True
  End If
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "CONE" Then
    OptCurrent.Enabled = False
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
  Dim I As Integer
  Parameter(0).text = CStr(0.002)
  Parameter(1).text = CStr(1)
  OptConfiguration(0).Value = True
  Parameter(2).Top = 240
  Parameter(2).Visible = True
  Parameter(2).text = CStr(5)
  For I = 3 To 7
    Parameter(I).Visible = False
  Next I
  Parameter(8).text = CStr(0)
  Parameter(9).text = CStr(-7.3)
  Parameter(10).text = CStr(6)
  Parameter(11).text = CStr(0.0359)
  LblMiddlings.Enabled = False
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
Dim I As Integer, J As Integer
Dim Top As Integer
  For J = 2 To 7
    Parameter(J).Visible = False
  Next J
  For I = 1 To 2
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
  Next I
  I = NDATA.PARAM(3)
  OptConfiguration(I - 1).Value = True
  Top = 240 + (I - 1) * 360
  For J = 4 To NDATA.NOPAR - 4
    Parameter(J - 2).Top = Top
    Parameter(J - 2).Visible = True
    Parameter(J - 2).Enabled = True
    If NDATA.PARAM(J) > 9 Then
      Parameter(J - 2).text = CStr(9)
    ElseIf NDATA.PARAM(J) < 1 Then
      Parameter(J - 2).text = CStr(1)
    Else
      Parameter(J - 2).text = CStr(NDATA.PARAM(J))
    End If
  Next J
  If NDATA.NOPAR < 9 Or NOSPLIT(UnitMods.Unit) <= 2 Then
    LblMiddlings.Enabled = False
    Parameter(8).Enabled = False
    Parameter(8).text = CStr(0)
  Else
    LblMiddlings.Enabled = True
    Parameter(8).Enabled = True
    Parameter(8).text = CStr(NDATA.PARAM(NDATA.NOPAR - 3))
  End If
  If NOSPLIT(UnitMods.Unit) > 2 Then
    Parameter(8).Enabled = True
    LblMiddlings.Enabled = True
    OptConfiguration(0).Enabled = False
    OptConfiguration(1).Enabled = False
    OptConfiguration(5).Enabled = False
  End If

  Parameter(9) = NDATA.PARAM(NDATA.NOPAR - 2)
  Parameter(10) = NDATA.PARAM(NDATA.NOPAR - 1)
  Parameter(11) = NDATA.PARAM(NDATA.NOPAR)
   
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer, J As Integer
  Dim ScaleFactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "CONE"
  If OptConfiguration(0).Value Then
    NDATA.NOPAR = 8
    NDATA.PARAM(3) = 1
  End If
  If OptConfiguration(1).Value Then
    NDATA.NOPAR = 8
    NDATA.PARAM(3) = 2
  End If
  If OptConfiguration(2).Value Then
    NDATA.NOPAR = 9
    NDATA.PARAM(3) = 3
  End If
  If OptConfiguration(3).Value Then
    NDATA.NOPAR = 10
    NDATA.PARAM(3) = 4
  End If
  If OptConfiguration(4).Value Then
    NDATA.NOPAR = 11
    NDATA.PARAM(3) = 5
  End If
  If OptConfiguration(5).Value Then
    NDATA.NOPAR = 9
    NDATA.PARAM(3) = 6
  End If
  If OptConfiguration(6).Value Then
    NDATA.NOPAR = 11
    NDATA.PARAM(3) = 7
  End If
  If OptConfiguration(7).Value Then
    NDATA.NOPAR = 13
    NDATA.PARAM(3) = 8
  End If
  If OptConfiguration(8).Value Then
    NDATA.NOPAR = 12
    NDATA.PARAM(3) = 9
  End If
  
  For I = 1 To 2
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  
  For I = 4 To NDATA.NOPAR - 4
    NDATA.PARAM(I) = Val(Parameter(I - 2).text)
  Next I
  
  For I = NDATA.NOPAR - 3 To NDATA.NOPAR
    J = I - NDATA.NOPAR + 3
    NDATA.PARAM(I) = Val(Parameter(8 + J).text)
  Next I
  
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

Private Sub OptConfiguration_GotFocus(Index As Integer)
  Instruction.Visible = True
  Instruction.Caption = "Choose the cone configuration"
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
  'Must trigger a load every time NEW is switched on except when changing a text field
  If Not TypeOf Screen.ActiveControl Is TextBox Then
    Call LoadNew(NEWDATA)
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

Sub optConfiguration_Click(Index As Integer)
'*******************************************
  If Screen.ActiveControl.Tag = "Config" Then
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub optConfiguration_LostFocus(Index As Integer)
'***********************************************
  Instruction.Visible = False
  Instruction.Caption = ""
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
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
    If Index <= 1 Then
      IDIFF = 1
    ElseIf Index <= 8 Then
      IDIFF = 2
    End If
    If Index <= 8 Then
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
  End If
Exit Sub

ErrHandler:
  MsgBox "Error in level data", vbCritical, "ERROR"
  Exit Sub
End Sub
