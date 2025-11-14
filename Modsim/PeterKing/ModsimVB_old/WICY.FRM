VERSION 5.00
Begin VB.Form WICY 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model"
   ClientHeight    =   2760
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   9045
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
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   2760
   ScaleWidth      =   9045
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   2160
      TabIndex        =   24
      Top             =   1920
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   2040
      TabIndex        =   17
      Top             =   720
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   2040
      TabIndex        =   16
      Top             =   960
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   2040
      TabIndex        =   13
      Top             =   480
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   2040
      TabIndex        =   1
      Top             =   240
      Width           =   1215
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   6720
      TabIndex        =   11
      Top             =   1680
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7920
      TabIndex        =   10
      Top             =   1680
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   7200
      TabIndex        =   0
      Top             =   120
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   12
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
      Caption         =   "Units of size"
      ForeColor       =   &H00000000&
      Height          =   975
      Left            =   3720
      TabIndex        =   4
      Top             =   1200
      Visible         =   0   'False
      Width           =   2895
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "micron"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   1200
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   1320
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   2160
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   615
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   4
         Left            =   1320
         TabIndex        =   8
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   600
         Width           =   960
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   9
         Tag             =   "Size"
         Top             =   600
         Value           =   -1  'True
         Width           =   600
      End
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of units in parallel"
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   25
      Top             =   1920
      Width           =   2055
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "cm"
      Height          =   255
      Index           =   3
      Left            =   3360
      TabIndex        =   23
      Top             =   720
      Width           =   375
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "kPa"
      Height          =   255
      Index           =   2
      Left            =   3360
      TabIndex        =   22
      Top             =   960
      Width           =   375
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "cm"
      Height          =   255
      Index           =   1
      Left            =   3360
      TabIndex        =   21
      Top             =   480
      Width           =   375
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "cm"
      Height          =   255
      Index           =   0
      Left            =   3360
      TabIndex        =   20
      Top             =   240
      Width           =   375
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Vortex finder diameter"
      Height          =   255
      Index           =   3
      Left            =   120
      TabIndex        =   19
      Top             =   720
      Width           =   1935
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Feed pressure"
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   18
      Top             =   960
      Width           =   1935
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Truncated cone diameter"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   15
      Top             =   480
      Width           =   1935
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Apex diameter"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   14
      Top             =   240
      Width           =   1935
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
Attribute VB_Name = "WICY"
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
  Caption = Caption & " WICY for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "WICY" Then
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
  Parameter(0).text = CStr(1.745)
  Parameter(1).text = CStr(2.545)
  Parameter(2).text = CStr(2.545)
  Parameter(3).text = CStr(172)
  Parameter(4) = 1
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
Dim I As Integer
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
  Next I
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Dim ScaleFactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "WICY"
  NDATA.NOPAR = 5
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
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

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
  Dim I As Integer, IDIFF As Integer
  Dim ParameterCanBeUsed As Boolean
  On Error GoTo ErrHandler
  
  If Shift = 2 And ProfessionalVersion Then
    ParameterCanBeUsed = False
    Call MakeNew(NEWDATA)
    Repeat.TxtParameterName = ""
    Repeat.TxtValue(0) = ""
    Repeat.TxtValue(1) = ""
    Repeat.TxtValue(2) = ""
    If Index < 4 Then
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

Sub optSize_Click(Index As Integer)
'***********************************
  If Screen.ActiveControl.Tag = "Size" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub optSize_LostFocus(Index As Integer)
'***************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
End Sub
