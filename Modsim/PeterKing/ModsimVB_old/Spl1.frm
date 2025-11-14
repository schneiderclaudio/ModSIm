VERSION 5.00
Begin VB.Form Spl1 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model SPL1"
   ClientHeight    =   2865
   ClientLeft      =   165
   ClientTop       =   1335
   ClientWidth     =   8160
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
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2865
   ScaleWidth      =   8160
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
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
      Height          =   225
      Index           =   1
      Left            =   2640
      TabIndex        =   1
      Text            =   "Text1"
      Top             =   480
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
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
      Height          =   225
      Index           =   0
      Left            =   2640
      TabIndex        =   0
      Text            =   "Text1"
      Top             =   240
      Width           =   855
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
      Height          =   615
      Left            =   5880
      TabIndex        =   3
      Top             =   1680
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
      Height          =   615
      Left            =   7080
      TabIndex        =   2
      Top             =   1680
      Width           =   960
   End
   Begin VB.Frame Frame1 
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
      Height          =   1095
      Left            =   6360
      TabIndex        =   11
      Top             =   240
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
         TabIndex        =   10
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
         Left            =   120
         TabIndex        =   9
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
         TabIndex        =   8
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      Caption         =   "This splitter has no outlet stream!"
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
      Left            =   240
      TabIndex        =   7
      Top             =   1200
      Visible         =   0   'False
      Width           =   3135
   End
   Begin VB.Label Label1 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
      Caption         =   "This splitter has only one outlet stream"
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
      Left            =   240
      TabIndex        =   6
      Top             =   840
      Visible         =   0   'False
      Width           =   3615
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Split to next outlet"
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
      Index           =   1
      Left            =   0
      TabIndex        =   5
      Top             =   480
      Width           =   2535
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Split to the lowest numbered outlet"
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
      Index           =   0
      Left            =   0
      TabIndex        =   4
      Top             =   240
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
      Begin VB.Menu mnuCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "Spl1"
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
  Unload Spl1
End Sub

Private Sub CmdCancel_Click()
  Unload Spl1
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
  If NOSPLIT(UnitMods.Unit) = 0 Then
    'Issue warning that the splitter has no outlets.
    lblParameter(0).Visible = 0
    Parameter(0).Visible = 0
    lblParameter(1).Visible = 0
    Parameter(1).Visible = 0
    Label2.Visible = -1
    cmdAccept.Visible = 0
  Else
    If NOSPLIT(UnitMods.Unit) <= 2 Then
      lblParameter(1).Visible = 0
      Parameter(1).Visible = 0
    Else
      lblParameter(1).Visible = -1
      Parameter(1).Visible = -1
    End If
    If (NOSPLIT(UnitMods.Unit) = 1) Then
      lblParameter(0).Visible = 0
      Parameter(0).Visible = 0
      Label1.Visible = -1
    End If
    'Find the current data for this model if any
    If CURRMODELDATA(ID).MODEL <> "SPL1" Then
      OptCurrent.Enabled = 0
      LoadDefault
      Call MakeNew(NEWDATA)
    Else
      Call LoadNew(CURRMODELDATA(ID))
      Call MakeNew(NEWDATA)
    End If
  End If
  Exit Sub

ErrHandler:
  Exit Sub
End Sub

Private Sub LoadDefault()
    'Use default data
    If NOSPLIT(UnitMods.Unit) > 0 Then
      Parameter(0).text = Str$(1 / NOSPLIT(UnitMods.Unit))
      Parameter(1).text = Str$(1 / NOSPLIT(UnitMods.Unit))
      cmdAccept.Enabled = -1
      cmdAccept.BackColor = 14
    End If
End Sub

Private Sub LoadNew(NDATA As ModelData)
  Dim sum As Single
  Dim I As Integer, response As Integer
    sum = 0!
    For I = 2 To 3
      Parameter(I - 2).text = Str$(NDATA.PARAM(I))
      sum = sum + NDATA.PARAM(I)
    Next I
    If sum > 1! Then
      response% = MsgBox("Sum of split fractions is greater than 1", 0, "WARNING")
      cmdAccept.Enabled = 0
      cmdAccept.BackColor = 15
    Else
      cmdAccept.Enabled = -1
      cmdAccept.BackColor = 14
    End If
End Sub

Private Sub MakeNew(NDATA As ModelData)
  Dim sum As Single
  Dim I As Integer, response As Integer
  CHANGE_FLAG = 0
  NDATA.MODEL = "SPL1"
  NDATA.NOPAR = 3
  NDATA.PARAM(1) = NOSPLIT(UnitMods.Unit)
  If (NOSPLIT(UnitMods.Unit) = 1) Then
    NDATA.PARAM(2) = 1!
    NDATA.PARAM(3) = 0!
  ElseIf (NOSPLIT(UnitMods.Unit) = 2) Then
    NDATA.PARAM(2) = Val(Parameter(0).text)
    NDATA.PARAM(3) = 1! - NDATA.PARAM(2)
  Else
    NDATA.PARAM(2) = Val(Parameter(0).text)
    NDATA.PARAM(3) = Val(Parameter(1).text)
  End If
  sum = 0!
  For I = 2 To 3
    If (NDATA.PARAM(I) < 0!) Then
      response% = MsgBox("Split fractions cannot be negative", 0, "WARNING")
      NDATA.PARAM(I) = 0!
      Parameter(I - 2).text = Str$(NDATA.PARAM(I))
    End If
    sum = sum + NDATA.PARAM(I)
  Next I
  If sum > 1! Then
      response% = MsgBox("Sum of split fractions is greater than 1", 0, "WARNING")
      cmdAccept.Enabled = 0
      cmdAccept.BackColor = 15
  Else
      cmdAccept.Enabled = -1
      cmdAccept.BackColor = 14
  End If
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
  IDIFF = 2
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

