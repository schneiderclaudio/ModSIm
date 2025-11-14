VERSION 5.00
Begin VB.Form FAGM 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model"
   ClientHeight    =   6000
   ClientLeft      =   45
   ClientTop       =   615
   ClientWidth     =   7635
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
   ScaleHeight     =   6000
   ScaleWidth      =   7635
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   20
      Left            =   3120
      TabIndex        =   50
      Top             =   5160
      Width           =   855
   End
   Begin VB.Frame frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mill properties"
      Height          =   1335
      Left            =   4680
      TabIndex        =   41
      Top             =   1320
      Width           =   2775
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   12
         Left            =   1800
         TabIndex        =   45
         Text            =   "Text7"
         Top             =   240
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   13
         Left            =   1800
         TabIndex        =   44
         Text            =   "Text7"
         Top             =   480
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   14
         Left            =   1800
         TabIndex        =   43
         Text            =   "Text7"
         Top             =   720
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   15
         Left            =   1800
         TabIndex        =   42
         Text            =   "Text7"
         Top             =   960
         Width           =   855
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mill diameter  m "
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   12
         Left            =   120
         TabIndex        =   49
         Top             =   240
         Width           =   1575
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Load volume  % "
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   13
         Left            =   120
         TabIndex        =   48
         Top             =   480
         Width           =   1575
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mill speed  %critical "
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   14
         Left            =   120
         TabIndex        =   47
         Top             =   720
         Width           =   1575
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Grate opening   mm "
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   15
         Left            =   120
         TabIndex        =   46
         Top             =   960
         Width           =   1575
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for fracture energy"
      Height          =   1455
      Left            =   4320
      TabIndex        =   34
      Top             =   2760
      Width           =   3135
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   19
         Left            =   2400
         TabIndex        =   39
         Top             =   1080
         Width           =   615
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   18
         Left            =   2400
         TabIndex        =   37
         Top             =   720
         Width           =   615
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   17
         Left            =   2400
         TabIndex        =   35
         Top             =   360
         Width           =   615
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0C0C0&
         Caption         =   "Exponent phi "
         Height          =   255
         Index           =   19
         Left            =   240
         TabIndex        =   40
         Top             =   1080
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0C0C0&
         Caption         =   "Reference size d0  mm "
         Height          =   255
         Index           =   18
         Left            =   240
         TabIndex        =   38
         Top             =   720
         Width           =   2055
      End
      Begin VB.Label lblParameter 
         Alignment       =   1  'Right Justify
         BackColor       =   &H00C0C0C0&
         Caption         =   "Particle fracture energy for large particles  J/kg "
         Height          =   495
         Index           =   17
         Left            =   240
         TabIndex        =   36
         Top             =   240
         Width           =   2055
      End
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   6480
      TabIndex        =   0
      Top             =   4800
      Width           =   960
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   5280
      TabIndex        =   1
      Top             =   4800
      Width           =   960
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   16
      Left            =   3120
      TabIndex        =   10
      Text            =   "Text7"
      Top             =   4800
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   3120
      TabIndex        =   12
      Text            =   "Text7"
      Top             =   4320
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   3120
      TabIndex        =   11
      Text            =   "Text7"
      Top             =   4080
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   9
      Left            =   3120
      TabIndex        =   33
      Text            =   "Text7"
      Top             =   3360
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   8
      Left            =   3120
      TabIndex        =   32
      Text            =   "Text7"
      Top             =   3120
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   7
      Left            =   3120
      TabIndex        =   5
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
      Index           =   6
      Left            =   3120
      TabIndex        =   4
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
      Index           =   5
      Left            =   3120
      TabIndex        =   9
      Text            =   "Text6"
      Top             =   2400
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   4
      Left            =   3120
      TabIndex        =   2
      Text            =   "Text5"
      Top             =   240
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   3
      Left            =   3120
      TabIndex        =   8
      Text            =   "Text4"
      Top             =   2160
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   2
      Left            =   3120
      TabIndex        =   7
      Text            =   "Text3"
      Top             =   1920
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   3120
      TabIndex        =   6
      Text            =   "Text2"
      Top             =   1680
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   3120
      TabIndex        =   3
      Text            =   "Text1"
      Top             =   480
      Width           =   855
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   5760
      TabIndex        =   27
      Top             =   0
      Width           =   1695
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         Height          =   240
         Left            =   120
         TabIndex        =   15
         Tag             =   "Default"
         Top             =   720
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
         Height          =   240
         Left            =   120
         TabIndex        =   14
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
         TabIndex        =   13
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Trommel mesh size  mm "
      Height          =   255
      Index           =   20
      Left            =   240
      TabIndex        =   51
      Top             =   5160
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "T10 parameter b "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   9
      Left            =   240
      TabIndex        =   31
      Top             =   3360
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "T10 parameter A "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   8
      Left            =   240
      TabIndex        =   30
      Top             =   3120
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Residence time in the mill  mins "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   16
      Left            =   240
      TabIndex        =   23
      Top             =   4800
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Largest size for attrition products  mm "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   10
      Left            =   120
      TabIndex        =   28
      Top             =   4080
      Width           =   2895
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Attrition parameter Ta "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   11
      Left            =   240
      TabIndex        =   29
      Top             =   4320
      Width           =   2775
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for selection function:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   240
      TabIndex        =   16
      Top             =   0
      Width           =   2775
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for breakage function:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   240
      TabIndex        =   19
      Top             =   1440
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "lambda "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   7
      Left            =   240
      TabIndex        =   26
      Top             =   960
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Delta "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   3
      Left            =   240
      TabIndex        =   24
      Top             =   2160
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Phi at 5mm "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   5
      Left            =   240
      TabIndex        =   22
      Top             =   2400
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Gamma "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   2
      Left            =   240
      TabIndex        =   21
      Top             =   1920
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   240
      TabIndex        =   20
      Top             =   1680
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "S1   1/min "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   4
      Left            =   240
      TabIndex        =   18
      Top             =   240
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   240
      TabIndex        =   17
      Top             =   480
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      Alignment       =   1  'Right Justify
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Mu in mm "
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   6
      Left            =   240
      TabIndex        =   25
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
Attribute VB_Name = "FAGM"
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
  Unload FAGM
End Sub

Private Sub CmdCancel_Click()
  Unload FAGM
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
  Caption = Caption & " " & UnitMods.MODEL & " for unit " & CStr(UnitMods.Unit)

  ID = UnitMods.ID
  CHANGE_FLAG = 0
  If UnitMods.MODEL = "FAGT" Then
    LblParameter(20).Visible = True
    Parameter(20).Visible = True
  Else
    LblParameter(20).Visible = False
    Parameter(20).Visible = False
  End If
  'Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> UnitMods.MODEL Then
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
    'Use default data for Austin functions calculated from conceptual
    'model with average impact energy = 0.6 J in the mill.
    Parameter(0).text = Str$(0.5)
    Parameter(1).text = Str$(3.723)
    Parameter(2).text = Str$(0.748)
    Parameter(3).text = Str$(0)
    Parameter(4).text = Str$(0.3994)
    Parameter(5).text = Str$(0.72)
    Parameter(6).text = Str$(10)
    Parameter(7).text = Str$(2.513)
    Parameter(8).text = Str$(50)
    Parameter(9).text = Str$(1)
    Parameter(10).text = Str$(1)
    Parameter(11).text = Str$(1)
    Parameter(12).text = Str$(5)
    Parameter(13).text = Str$(40)
    Parameter(14).text = Str$(75)
    Parameter(15).text = Str$(40)
    Parameter(16).text = Str$(20)
    Parameter(17).text = CStr(96)
    Parameter(18).text = CStr(1.17)
    Parameter(19).text = CStr(1.26)
    Parameter(20).text = CStr(50)
End Sub

Private Sub LoadNew(NDATA As ModelData)
  Dim I As Integer
    For I = 1 To NDATA.NOPAR
      Parameter(I - 1).text = Str$(NDATA.PARAM(I))
    Next I
  End Sub

Private Sub MakeNew(NDATA As ModelData)
  Dim I As Integer
  CHANGE_FLAG = 0
  If UnitMods.MODEL = "FAGT" Then
    NDATA.MODEL = "FAGT"
    NDATA.NOPAR = 21
  Else
    NDATA.MODEL = "FAGM"
    NDATA.NOPAR = 20
  End If
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call MnuCancel_Click
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
    Repeat.TxtParameterName = Me.LblParameter(Index).Caption
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
    Repeat.Show vbModal
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

