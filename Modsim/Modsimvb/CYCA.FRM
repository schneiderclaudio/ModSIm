VERSION 5.00
Begin VB.Form Cyca 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM - Specify the parameter values for model "
   ClientHeight    =   4230
   ClientLeft      =   780
   ClientTop       =   1725
   ClientWidth     =   8040
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
   Icon            =   "Cyca.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4230
   ScaleWidth      =   8040
   Begin VB.OptionButton optSize 
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
      Index           =   4
      Left            =   6240
      TabIndex        =   7
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1680
      Width           =   1080
   End
   Begin VB.OptionButton optSize 
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
      Index           =   3
      Left            =   5520
      TabIndex        =   6
      TabStop         =   0   'False
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1680
      Width           =   600
   End
   Begin VB.OptionButton optSize 
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
      Index           =   2
      Left            =   4680
      TabIndex        =   5
      TabStop         =   0   'False
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1680
      Width           =   720
   End
   Begin VB.OptionButton optSize 
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
      Index           =   1
      Left            =   3720
      TabIndex        =   4
      TabStop         =   0   'False
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1680
      Width           =   840
   End
   Begin VB.OptionButton optSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "microns"
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
      Left            =   2640
      TabIndex        =   3
      TabStop         =   0   'False
      ToolTipText     =   "Left click to select: Right click to convert to."
      Top             =   1680
      Value           =   -1  'True
      Width           =   975
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   480
      Left            =   5520
      TabIndex        =   15
      Top             =   3120
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   480
      Left            =   6720
      TabIndex        =   14
      Top             =   3120
      Width           =   960
   End
   Begin VB.CheckBox Chkd50 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Allow d50 to vary with density"
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
      TabIndex        =   9
      Top             =   2880
      Width           =   4200
   End
   Begin VB.ComboBox Combo1 
      Appearance      =   0  'Flat
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   315
      Left            =   120
      TabIndex        =   8
      ToolTipText     =   "Choose your classification function"
      Top             =   2400
      Width           =   3000
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   0
      Left            =   2520
      TabIndex        =   0
      Top             =   480
      Width           =   1095
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   1
      Left            =   2520
      TabIndex        =   1
      Top             =   840
      Width           =   1095
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   3
      Left            =   2640
      TabIndex        =   10
      Top             =   3240
      Visible         =   0   'False
      Width           =   720
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   220
      Index           =   2
      Left            =   840
      TabIndex        =   2
      Top             =   1680
      Width           =   1215
   End
   Begin VB.Frame Frame1 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   6120
      TabIndex        =   21
      Top             =   360
      Width           =   1815
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
         Left            =   240
         TabIndex        =   13
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
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
         Left            =   240
         TabIndex        =   12
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
         Left            =   240
         TabIndex        =   11
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Label Instruction 
      BackColor       =   &H0080FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   22
      Top             =   0
      Visible         =   0   'False
      Width           =   7815
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Short-circuit to underflow"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   120
      TabIndex        =   16
      Top             =   480
      Width           =   2415
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Sharpness index.  Range (0-1)"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   120
      TabIndex        =   17
      Top             =   840
      Width           =   2295
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cut size"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   120
      TabIndex        =   18
      Top             =   1680
      Width           =   720
   End
   Begin VB.Label Label5 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Classification function"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   0
      TabIndex        =   19
      Top             =   2160
      Width           =   3000
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameter for d50 vs density"
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   120
      TabIndex        =   20
      Top             =   3240
      Visible         =   0   'False
      Width           =   2535
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
Attribute VB_Name = "Cyca"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub Chkd50_Click()
  If TypeOf Screen.ActiveControl Is CheckBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
  If (Chkd50.Value = 1) Then
    Parameter(3).Visible = -1
    lblParameter(3).Visible = -1
  Else
    Parameter(3).Visible = 0
    lblParameter(3).Visible = 0
  End If
End Sub

Private Sub Chkd50_GotFocus()
'****************************
  Instruction.Visible = True
  Instruction.Caption = "Check here to allow the d50 size to vary with particle density"
End Sub

Private Sub Chkd50_LostFocus()
  Instruction.Visible = False
  Instruction.Caption = ""
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub CmdAccept_Click()
  Call MakeNew(CURRMODELDATA(ID))
  Unload Me
End Sub

Private Sub CmdCancel_Click()
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

Private Sub Combo1_Click()
  If TypeOf Screen.ActiveControl Is ComboBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    Combo1.SetFocus
  End If
End Sub

Private Sub Combo1_GotFocus()
  Instruction.Visible = True
  Instruction.Caption = "Select the classification function for this cyclone"
End Sub

Private Sub Combo1_LostFocus()
  Instruction.Visible = False
  Instruction.Caption = ""
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Form_Load()
'***************
On Error GoTo ErrHandler
  Caption = Caption & UnitMods.Model & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
  'Set up the partition function selection list
  Combo1.AddItem "Exponential sum"
  Combo1.AddItem "Rosin-Rammler"
  Combo1.AddItem "Logistic"

  'Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "CYCA" And CURRMODELDATA(ID).Model <> "CYCB" Then
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
  OptDefault.Value = -1
  Parameter(0).text = Str$(0.2)
  Parameter(1).text = Str$(0.8)
  Parameter(2).text = Str$(0.0001)
  Parameter(3).text = Str$(0.5)
  Combo1.ListIndex = 2
  OptSize(3).Value = -1
  Parameter(3).Visible = -1
  lblParameter(3).Visible = -1
  Chkd50.Value = 1
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  For I = 1 To 4
    Parameter(I - 1).text = Str$(NDATA.PARAM(I))
  Next I
  If NDATA.PARAM(4) > 0! Then
    Parameter(3).Visible = -1
    lblParameter(3).Visible = -1
    Chkd50.Value = 1
  Else
    Parameter(3).Visible = 0
    lblParameter(3).Visible = 0
    Chkd50.Value = 0
  End If
  Combo1.ListIndex = NDATA.PARAM(5) - 1
  OptSize(3).Value = -1
  End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA.Model = UnitMods.Model
  NDATA.NOPAR = 5
  For I = 1 To 4
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  If Chkd50.Value = 0 Then NDATA.PARAM(4) = 0!
  ' Convert the size to meters
  If OptSize(0).Value Then NDATA.PARAM(3) = NDATA.PARAM(3) * 0.000001
  If OptSize(1).Value Then NDATA.PARAM(3) = NDATA.PARAM(3) * 0.001
  If OptSize(2).Value Then NDATA.PARAM(3) = NDATA.PARAM(3) * 0.01
  If OptSize(4).Value Then NDATA.PARAM(3) = NDATA.PARAM(3) * 0.0254
  NDATA.PARAM(5) = Combo1.ListIndex + 1
End Sub

Private Sub MnuAccept_Click()
'****************************
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

Private Sub optSize_Click(Index As Integer)
  If TypeOf Screen.ActiveControl Is OptionButton Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub optSize_LostFocus(Index As Integer)
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Optsize_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'*****************************************************************************************************************
Dim I As Integer
Dim Factor As Double

CFactor = micron_mm_cm_m_inch
If Button = vbRightButton Then
  'Find the currently set index
  For I = 0 To CFactor.Number - 1
    If OptSize(I).Value = True Then
      Factor = CFactor.Factor(I + 1) / CFactor.Factor(Index + 1)
    End If
  Next I
  Parameter(2).text = Format(Val(Parameter(2).text) * Factor, "0.000E+#")
  OptSize(Index).Value = True
End If
End Sub

Private Sub Parameter_Change(Index As Integer)
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
  If Index = 3 Then
    Instruction.Visible = True
    Instruction.Caption = "Specify the exponent for the power-law dependence of d50 on size"
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
  Instruction.Visible = False
  Instruction.Caption = ""
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

