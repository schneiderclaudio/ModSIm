VERSION 5.00
Begin VB.Form ADOR 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model ADOR"
   ClientHeight    =   6390
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   8685
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
   ScaleHeight     =   6390
   ScaleWidth      =   8685
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   6960
      TabIndex        =   56
      Top             =   3120
      Width           =   765
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   6960
      TabIndex        =   54
      Top             =   2880
      Width           =   765
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   6960
      TabIndex        =   52
      Top             =   2640
      Width           =   765
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   6960
      TabIndex        =   50
      Top             =   2400
      Width           =   765
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   15
      Left            =   1080
      TabIndex        =   48
      Top             =   5400
      Width           =   1600
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Select model for the solid stress"
      Height          =   615
      Left            =   4440
      TabIndex        =   44
      Top             =   1560
      Width           =   3975
      Begin VB.OptionButton OptStressModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Becker"
         Height          =   255
         Index           =   2
         Left            =   2760
         TabIndex        =   47
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton OptStressModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Shirato"
         Height          =   255
         Index           =   1
         Left            =   1440
         TabIndex        =   46
         Top             =   240
         Width           =   1095
      End
      Begin VB.OptionButton OptStressModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Adorjan"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   45
         Top             =   240
         Value           =   -1  'True
         Width           =   1095
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Select model for the settling flux"
      Height          =   615
      Left            =   -120
      TabIndex        =   41
      Top             =   1560
      Width           =   4215
      Begin VB.OptionButton OptFluxModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Wilhelm-Naide"
         Height          =   255
         Index           =   1
         Left            =   2280
         TabIndex        =   43
         Top             =   240
         Value           =   -1  'True
         Width           =   1575
      End
      Begin VB.OptionButton OptFluxModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Richardson-Zaki"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   42
         Top             =   240
         Width           =   1815
      End
   End
   Begin VB.OptionButton OptSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "ft"
      Height          =   255
      Index           =   1
      Left            =   5400
      TabIndex        =   38
      Tag             =   "Size"
      Top             =   600
      Width           =   495
   End
   Begin VB.OptionButton OptSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "m"
      Height          =   255
      Index           =   0
      Left            =   4680
      TabIndex        =   37
      Tag             =   "Size"
      Top             =   600
      Value           =   -1  'True
      Width           =   495
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   1080
      TabIndex        =   33
      Top             =   4920
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   1080
      TabIndex        =   32
      Top             =   5160
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   1080
      TabIndex        =   29
      Top             =   4680
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   1080
      TabIndex        =   28
      Top             =   4440
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   1080
      TabIndex        =   25
      Top             =   4200
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   1080
      TabIndex        =   24
      Top             =   3960
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   1080
      TabIndex        =   21
      Top             =   3720
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   1080
      TabIndex        =   20
      Top             =   3480
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   1080
      TabIndex        =   17
      Top             =   3240
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   1080
      TabIndex        =   16
      Top             =   3000
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   1080
      TabIndex        =   13
      Top             =   2760
      Width           =   1600
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   345
      Index           =   3
      Left            =   2640
      TabIndex        =   12
      Top             =   2280
      Width           =   525
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   3360
      TabIndex        =   10
      Top             =   960
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   3360
      TabIndex        =   7
      Top             =   720
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   3360
      TabIndex        =   1
      Top             =   480
      Width           =   1215
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   6120
      TabIndex        =   5
      Top             =   5280
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7440
      TabIndex        =   4
      Top             =   5280
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   6840
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
   Begin VB.Label Label2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "N/m^2"
      Height          =   255
      Left            =   7920
      TabIndex        =   57
      Top             =   2400
      Width           =   735
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Ultimate concentration"
      Height          =   255
      Index           =   19
      Left            =   4440
      TabIndex        =   55
      Top             =   3120
      Width           =   2415
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Critical concentration"
      Height          =   255
      Index           =   18
      Left            =   4440
      TabIndex        =   53
      Top             =   2880
      Width           =   2535
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Exponent"
      Height          =   255
      Index           =   17
      Left            =   4440
      TabIndex        =   51
      Top             =   2640
      Width           =   2535
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Stress at mean concentration"
      Height          =   255
      Index           =   16
      Left            =   4440
      TabIndex        =   49
      Top             =   2400
      Width           =   2535
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Depth of compression zone"
      Height          =   255
      Index           =   1
      Left            =   120
      TabIndex        =   40
      Top             =   720
      Width           =   3135
   End
   Begin VB.Label Instruction 
      BackColor       =   &H0080FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   39
      Top             =   0
      Width           =   8415
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "mm/s"
      Height          =   255
      Left            =   4680
      TabIndex        =   36
      Top             =   960
      Width           =   615
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 6"
      Height          =   255
      Index           =   15
      Left            =   120
      TabIndex        =   35
      Top             =   5400
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 6"
      Height          =   255
      Index           =   14
      Left            =   120
      TabIndex        =   34
      Top             =   5160
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 5"
      Height          =   255
      Index           =   13
      Left            =   120
      TabIndex        =   31
      Top             =   4920
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 5"
      Height          =   255
      Index           =   12
      Left            =   120
      TabIndex        =   30
      Top             =   4680
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 4"
      Height          =   255
      Index           =   11
      Left            =   120
      TabIndex        =   27
      Top             =   4440
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 4"
      Height          =   255
      Index           =   10
      Left            =   120
      TabIndex        =   26
      Top             =   4200
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 3"
      Height          =   255
      Index           =   9
      Left            =   120
      TabIndex        =   23
      Top             =   3960
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 3"
      Height          =   255
      Index           =   8
      Left            =   120
      TabIndex        =   22
      Top             =   3720
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 2"
      Height          =   255
      Index           =   7
      Left            =   120
      TabIndex        =   19
      Top             =   3480
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 2"
      Height          =   255
      Index           =   6
      Left            =   120
      TabIndex        =   18
      Top             =   3240
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 1"
      Height          =   255
      Index           =   5
      Left            =   120
      TabIndex        =   15
      Top             =   3000
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 1"
      Height          =   255
      Index           =   4
      Left            =   120
      TabIndex        =   14
      Top             =   2760
      Width           =   855
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of terms in the extended Wilhelm-Naide expression"
      Height          =   495
      Index           =   3
      Left            =   120
      TabIndex        =   11
      Top             =   2280
      Width           =   2415
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Terminal settling velocity of an isolated floc"
      Height          =   255
      Index           =   2
      Left            =   120
      TabIndex        =   9
      Top             =   960
      Width           =   3135
   End
   Begin VB.Label LblParam 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Diameter of thickener"
      Height          =   255
      Index           =   0
      Left            =   120
      TabIndex        =   8
      Top             =   480
      Width           =   3135
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
Attribute VB_Name = "ADOR"
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
  Unload ADOR
End Sub
Sub CmdCancel_Click()
'*********************
  Unload ADOR
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
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)
  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "ADOR" Then
    LoadDefault
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
'****************
  Dim I As Integer
  Parameter(0).text = CStr(15)
  Parameter(1).text = CStr(2)
  Parameter(2).text = CStr(10)
  Parameter(3).text = CStr(2)
  Parameter(4).text = CStr(0.01)
  Parameter(5).text = CStr(1.5)
  Parameter(6).text = CStr(0.00000000005)
  Parameter(7).text = CStr(5)
  For I = 0 To 7
    Parameter(I).Visible = True
    LblParam(I).Visible = True
  Next I
  For I = 8 To 15
    Parameter(I).Visible = False
    LblParam(I).Visible = False
  Next I
  Parameter(16).text = CStr(182.6)
  Parameter(17).text = CStr(1.61)
  Parameter(18).text = CStr(0.15)
  Parameter(19).text = CStr(0.4855)
  For I = 16 To 19
    Parameter(I).Visible = True
    LblParam(I).Visible = True
  Next I

  OptSize(0).Value = True
End Sub

 Private Sub LoadNew(NDATA As ModelData)
'***************************************
  Dim I As Integer, N As Integer
  For I = 1 To 3
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
    Parameter(I - 1).Visible = True
    LblParam(I - 1).Visible = True
  Next I
  If NDATA.PARAM(4) = 1 Then
    OptFluxModel(1) = True
    N = 6 + 2 * NDATA.PARAM(6)
    Parameter(3).text = CStr(NDATA.PARAM(6))
    Parameter(3).Visible = True
    LblParam(3).Visible = True
    For I = 7 To N
      Parameter(I - 3).text = CStr(NDATA.PARAM(I))
      Parameter(I - 3).Visible = True
      LblParam(I - 3).Visible = True
    Next I
  End If
  If NDATA.PARAM(5) = 1 Then
    OptStressModel(0) = True
    For I = 17 To 20
      Parameter(I - 1).text = CStr(NDATA.PARAM(I))
      Parameter(1 - 1).Visible = True
      LblParam(1 - 1).Visible = True
    Next I
  End If
  'Display the terminal settling velocity as mm/s
  Parameter(2).text = CStr(1000 * NDATA.PARAM(3))
  For I = N + 1 To 18
    Parameter(I - 3).Visible = False
    LblParam(I - 3).Visible = False
  Next I
  OptSize(0).Value = True
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer, N As Integer
  CHANGE_FLAG = 0
  NDATA.MODEL = "ADOR"
  NDATA.NOPAR = 20
  For I = 1 To 3
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  If OptFluxModel(1) Then
    NDATA.PARAM(4) = 2
    NDATA.PARAM(6) = Val(Parameter(3).text)
    N = 6 + 2 * Val(Parameter(3).text)
    For I = 7 To N
      NDATA.PARAM(I) = Val(Parameter(I - 3).text)
    Next I
  End If
  If OptStressModel(0) Then
    NDATA.PARAM(5) = 1
    For I = 16 To 20
      NDATA.PARAM(I) = Val(Parameter(I - 1).text)
    Next I
  End If
  'Convert the size to meters
  If OptSize(1).Value Then NDATA.PARAM(1) = NDATA.PARAM(1) * 0.3048
  'Convert the terminal settling velocity to m/s
  NDATA.PARAM(3) = 0.001 * NDATA.PARAM(3)
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

Private Sub Parameter_GotFocus(Index As Integer)
  If Index > 2 Then
    Instruction.Visible = True
    Instruction.Caption = "Specify the parameters in the Wilhelm-Naide equation for the settling velocity"
  End If
End Sub

Sub Parameter_LostFocus(Index As Integer)
'****************************************
  Instruction.Visible = False
  Instruction.Caption = ""
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  End If
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
