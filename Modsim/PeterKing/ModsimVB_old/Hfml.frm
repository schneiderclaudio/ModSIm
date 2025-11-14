VERSION 5.00
Begin VB.Form HFML 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model HFML"
   ClientHeight    =   5985
   ClientLeft      =   240
   ClientTop       =   1770
   ClientWidth     =   9105
   BeginProperty Font 
      Name            =   "Arial"
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
   ScaleHeight     =   5985
   ScaleWidth      =   9105
   Begin VB.CheckBox ChkMakeAMDiagram 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Make Andrews-Mika diagram on exit using the beta function model"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   120
      TabIndex        =   35
      Top             =   5040
      Width           =   5175
   End
   Begin VB.ComboBox CombFunction 
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   330
      Left            =   3840
      TabIndex        =   33
      Tag             =   "ClassificationFunction"
      Top             =   3360
      Visible         =   0   'False
      Width           =   2535
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   9
      Left            =   8160
      TabIndex        =   32
      Top             =   3600
      Visible         =   0   'False
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   8
      Left            =   8160
      TabIndex        =   31
      Top             =   3360
      Visible         =   0   'False
      Width           =   855
   End
   Begin VB.CheckBox ChkClassification 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Allow for post classification"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3720
      TabIndex        =   28
      Tag             =   "Classification"
      Top             =   2760
      Width           =   2775
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   3
      Left            =   2160
      TabIndex        =   26
      Top             =   2880
      Width           =   855
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Choose a liberation model"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   720
      Left            =   120
      TabIndex        =   23
      Top             =   4080
      Width           =   4215
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "None"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   27
         Tag             =   "Liberation"
         Top             =   360
         Value           =   -1  'True
         Width           =   975
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Beta function"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   2
         Left            =   2520
         TabIndex        =   25
         Tag             =   "Liberation"
         Top             =   360
         Width           =   1575
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Ljubljana"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   240
         Index           =   1
         Left            =   1200
         TabIndex        =   24
         Tag             =   "Liberation"
         Top             =   360
         Width           =   1095
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   7
      Left            =   2880
      TabIndex        =   0
      Top             =   720
      Width           =   855
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   600
      Left            =   6600
      TabIndex        =   11
      Top             =   4800
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   600
      Left            =   8040
      TabIndex        =   10
      Top             =   4800
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1095
      Left            =   7320
      TabIndex        =   22
      Top             =   120
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
         TabIndex        =   9
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
         TabIndex        =   8
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
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   6
      Left            =   2160
      TabIndex        =   6
      Top             =   3600
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   5
      Left            =   2160
      TabIndex        =   5
      Top             =   3360
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   4
      Left            =   2160
      TabIndex        =   4
      Top             =   3120
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   2
      Left            =   4320
      TabIndex        =   3
      Top             =   2160
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   1
      Left            =   4320
      TabIndex        =   2
      Top             =   1920
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
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
      Height          =   225
      Index           =   0
      Left            =   4320
      TabIndex        =   1
      Top             =   1680
      Width           =   855
   End
   Begin VB.Label LblClassificationFunction 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Classification function"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Left            =   3720
      TabIndex        =   34
      Top             =   3120
      Visible         =   0   'False
      Width           =   2775
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Sharpness index"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   9
      Left            =   6720
      TabIndex        =   30
      Top             =   3600
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "D50  mm"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   255
      Index           =   8
      Left            =   6720
      TabIndex        =   29
      Top             =   3360
      Visible         =   0   'False
      Width           =   1335
   End
   Begin VB.Label Label5 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for breakage function:"
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
      Height          =   240
      Left            =   120
      TabIndex        =   15
      Top             =   2640
      Width           =   3015
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Zeta 1"
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
      Height          =   240
      Index           =   1
      Left            =   120
      TabIndex        =   13
      Top             =   1920
      Width           =   4095
   End
   Begin VB.Label Label2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for energy-specific selection function:"
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
      Height          =   240
      Left            =   120
      TabIndex        =   12
      Top             =   1440
      Width           =   4440
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Net power drawn by mill charge  kW"
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
      Height          =   240
      Index           =   7
      Left            =   120
      TabIndex        =   19
      Top             =   720
      Width           =   2655
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Energy-specific selection function at 1mm  tonnes/kWhr"
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
      Height          =   240
      Index           =   0
      Left            =   120
      TabIndex        =   14
      Top             =   1680
      Width           =   4095
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Zeta 2"
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
      Height          =   240
      Index           =   2
      Left            =   120
      TabIndex        =   21
      Top             =   2160
      Width           =   4095
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta"
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
      Height          =   240
      Index           =   3
      Left            =   120
      TabIndex        =   16
      Top             =   2880
      Width           =   1560
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Gamma"
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
      Height          =   240
      Index           =   4
      Left            =   120
      TabIndex        =   17
      Top             =   3120
      Width           =   1560
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Delta"
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
      Height          =   240
      Index           =   5
      Left            =   120
      TabIndex        =   20
      Top             =   3360
      Width           =   1680
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Phi at 5mm"
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
      Height          =   240
      Index           =   6
      Left            =   120
      TabIndex        =   18
      Top             =   3600
      Width           =   1680
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Cancel"
      End
   End
End
Attribute VB_Name = "HFML"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Private Sub ChkClassification_Click()
'************************************
  If Screen.ActiveControl.Tag = "Classification" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    If ChkClassification.Value = 1 Then
      CombFunction.Visible = True
      LblClassificationFunction.Visible = True
      lblParameter(8).Visible = True
      lblParameter(9).Visible = True
      Parameter(8).Visible = True
      Parameter(9).Visible = True
    Else
      CombFunction.Visible = False
      LblClassificationFunction.Visible = False
      lblParameter(8).Visible = False
      lblParameter(9).Visible = False
      Parameter(8).Visible = False
      Parameter(9).Visible = False
    End If
  End If
End Sub

Private Sub ChkClassification_LostFocus()
'****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim response As Integer, I As Integer
  Dim RetVal As Long
  Dim BrFuncParameters(1 To 4) As Single
  
  'Check that sharpness index is within proper limits
  If Val(ChkClassification) = 1 Then
    If Val(Parameter(9).text) <= 0 Or Val(Parameter(9)) >= 1 Then
       MsgBox "Sharpnes index must be > 0 and < 1", 16, "ERROR"
       Parameter(9).SetFocus
       Exit Sub
    End If
  End If
  Call MakeNew(CURRMODELDATA(ID))
  
  'Now make the A-M diagram if necessary
  For I = 1 To 4
    BrFuncParameters(I) = CURRMODELDATA(ID).PARAM(I + 3)
  Next I
  If (ChangedA_MDiagramParameters Or ChkMakeAMDiagram.Value = 1) And OptLibModel(2).Value Then
    response = MsgBox("Andrews-Mika and/or breakage function parameters have changed." & Chr(13) & Chr(10) & "Make Andrews-Mika diagram using the beta function model now?", vbYesNoCancel, "INFORMATION")
    If response = vbYes Then
      Me.MousePointer = 11
      NameOfJob = MODSIM.JOBNAME
      RetVal = BetaAMD(JobPath, NameOfJob, BrFuncParameters(1))
      Me.MousePointer = 1
      If RetVal > -1 Then
        MsgBox " Error in Andrews-Mika diagram calculation " & CStr(RetVal), 64, "INFORMATION"
        JobBetaAMD = False
      Else
        JobBetaAMD = True
        ChangedA_MDiagramParameters = False
      End If
    ElseIf response = vbCancel Then
      Exit Sub
    End If
  End If
  Unload HFML
End Sub

Private Sub CmdCancel_Click()
  Unload HFML
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  Me.Hide  'Must hide me before doing anything on any other form because I'm modal
  'Unload Help  'In case help was called. Not necessary if help does not show simultaneously
  If FastParameterChange Then
    UnitMods.cmdQuit_Click
    FastParameterChange = False
  End If
End Sub

Private Sub CombFunction_Change()
'********************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub CombFunction_Click()
'*******************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub CombFunction_LostFocus()
'***********************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Form_Load()
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)

  ID = UnitMods.ID
  CHANGE_FLAG = 0
  CombFunction.AddItem "Logistic"
  CombFunction.AddItem "Rosin Rammler"
  CombFunction.AddItem "Exponential sum"
  'Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "HFML" Then
    OptCurrent.Enabled = 0
    Call LoadDefault
    Call MakeNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  If Val(Sysdata.NGCM.text) < 5 Then
    OptLibModel(1).Enabled = False
    OptLibModel(2).Enabled = False
  End If
  'Must unload Sysdata now otherwise it remains loaded and will not initialize properly when next needed.
  Unload Sysdata
  Exit Sub

ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  Exit Sub
  
End Sub

Private Sub LoadDefault()
    OptDefault.Value = -1
    'Use default data from Lo and Herbst, Adv Mineral Process SME1986 p33
    Parameter(0).text = CStr(0.8)
    Parameter(1).text = CStr(0.185)
    Parameter(2).text = CStr(-0.312)
    Parameter(3).text = CStr(3.723)
    Parameter(4).text = CStr(0.748)
    Parameter(5).text = CStr(0)
    Parameter(6).text = CStr(0.72)
    Parameter(7).text = CStr(2000)
    Parameter(8).text = CStr(3)
    Parameter(9).text = CStr(0.8)
    OptLibModel(0).Value = True
    ChkClassification.Value = 0
    CombFunction.Visible = False
    lblParameter(8).Visible = False
    lblParameter(9).Visible = False
    Parameter(8).Visible = False
    Parameter(9).Visible = False
    CombFunction.text = CombFunction.List(0)
    CombFunction.ListIndex = 0
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  For I = 1 To 8
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
  Next I
  If NDATA.PARAM(9) = 0 Then OptLibModel(0).Value = True
  If NDATA.PARAM(9) = 1 Then OptLibModel(1).Value = True
  If NDATA.PARAM(9) = 2 Then OptLibModel(2).Value = True
  If NDATA.PARAM(10) > 0 Then
    ChkClassification.Value = 1
    CombFunction.Visible = True
    lblParameter(8).Visible = True
    lblParameter(9).Visible = True
    Parameter(8).Visible = True
    Parameter(9).Visible = True
    CombFunction.ListIndex = NDATA.PARAM(10) - 1
  Else
    ChkClassification.Value = 0
    CombFunction.Visible = False
    lblParameter(8).Visible = False
    lblParameter(9).Visible = False
    Parameter(8).Visible = False
    Parameter(9).Visible = False
  End If
  Parameter(8).text = CStr(NDATA.PARAM(11))
  Parameter(9).text = CStr(NDATA.PARAM(12))
    
  End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA.MODEL = "HFML"
  NDATA.NOPAR = 12
  For I = 1 To 8
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  If OptLibModel(0).Value Then NDATA.PARAM(9) = 0
  If OptLibModel(1).Value Then NDATA.PARAM(9) = 1
  If OptLibModel(2).Value Then NDATA.PARAM(9) = 2
  If ChkClassification.Value = 0 Then
    NDATA.PARAM(10) = 0
  Else
    NDATA.PARAM(10) = CombFunction.ListIndex + 1
  End If
  NDATA.PARAM(11) = Val(Parameter(8).text)
  NDATA.PARAM(12) = Val(Parameter(9).text)
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
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

Private Sub OptLibModel_Click(Index As Integer)
'**********************************************
  If Screen.ActiveControl.Tag = "Liberation" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    If OptLibModel(2).Value Then
      ChkMakeAMDiagram.Value = 1
    Else
      ChkMakeAMDiagram.Value = 0
    End If
  End If
End Sub

Private Sub OptLibModel_LostFocus(Index As Integer)
'**************************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub Parameter_Change(Index As Integer)
'*********************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_LostFocus(Index As Integer)
'************************************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    If Index >= 3 And Index <= 6 Then
      ChkMakeAMDiagram.Value = 1
    End If
  End If
End Sub

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
Dim I As Integer, IDIFF As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = lblParameter(Index).Caption
  Repeat.TxtValue(0) = Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  IDIFF = 1
  If Index >= 8 Then IDIFF = 3
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

