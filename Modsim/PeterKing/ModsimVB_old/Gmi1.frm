VERSION 5.00
Begin VB.Form Gmi1 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify the parameters for model GMI1"
   ClientHeight    =   7125
   ClientLeft      =   165
   ClientTop       =   1095
   ClientWidth     =   8640
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
   ScaleHeight     =   7125
   ScaleWidth      =   8640
   Begin VB.CheckBox ChkMakeAMDiagram 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Compute Andrews-Mika diagram on exit using beta function model."
      Height          =   255
      Left            =   360
      TabIndex        =   45
      Top             =   6120
      Width           =   5295
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   11
      Left            =   4560
      TabIndex        =   43
      Top             =   5640
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   10
      Left            =   4560
      TabIndex        =   42
      Top             =   5400
      Width           =   855
   End
   Begin VB.ComboBox ComboFunction 
      Height          =   330
      Left            =   360
      TabIndex        =   40
      Tag             =   "ClassificationFunction"
      Top             =   5520
      Width           =   2535
   End
   Begin VB.CheckBox ChkClassification 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Allow for post classification"
      Height          =   255
      Left            =   360
      TabIndex        =   39
      Tag             =   "ClassificationFunction"
      Top             =   4920
      Width           =   2775
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for the selection function"
      Height          =   1335
      Index           =   1
      Left            =   120
      TabIndex        =   30
      Top             =   1680
      Width           =   3855
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   9
         Left            =   1680
         TabIndex        =   34
         Top             =   960
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   8
         Left            =   1680
         TabIndex        =   33
         Top             =   720
         Width           =   855
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   7
         Left            =   1680
         TabIndex        =   32
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
         Left            =   1680
         TabIndex        =   31
         Top             =   240
         Width           =   855
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Specific rate at 1mm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   6
         Left            =   120
         TabIndex        =   38
         Top             =   240
         Width           =   1695
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Alpha"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   7
         Left            =   120
         TabIndex        =   37
         Top             =   480
         Width           =   1695
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Mu in mm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   8
         Left            =   120
         TabIndex        =   36
         Top             =   720
         Width           =   1695
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Lambda"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   9
         Left            =   120
         TabIndex        =   35
         Top             =   960
         Width           =   1455
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameters for the breakage function"
      Height          =   1335
      Index           =   0
      Left            =   4680
      TabIndex        =   21
      Top             =   1680
      Width           =   3615
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00FFFFFF&
         BorderStyle     =   0  'None
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   5
         Left            =   2640
         TabIndex        =   28
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
         Left            =   2640
         TabIndex        =   26
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
         Left            =   2640
         TabIndex        =   24
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
         Left            =   2640
         TabIndex        =   22
         Top             =   240
         Width           =   855
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Phi at 5 mm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   5
         Left            =   120
         TabIndex        =   29
         Top             =   960
         Width           =   1215
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Delta"
         ForeColor       =   &H00000000&
         Height          =   225
         Index           =   4
         Left            =   120
         TabIndex        =   27
         Top             =   720
         Width           =   1335
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Gamma"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   25
         Top             =   480
         Width           =   1335
      End
      Begin VB.Label lblParameter 
         Appearance      =   0  'Flat
         BackColor       =   &H00C0C0C0&
         Caption         =   "Beta"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   23
         Top             =   240
         Width           =   1455
      End
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   6120
      TabIndex        =   13
      Top             =   6000
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7560
      TabIndex        =   12
      Top             =   6000
      Width           =   960
   End
   Begin VB.CheckBox chkOverfill 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Allow for overfilling?"
      Height          =   240
      Left            =   360
      TabIndex        =   7
      Tag             =   "Over"
      Top             =   4080
      Width           =   2295
   End
   Begin VB.ComboBox Combo2 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Enabled         =   0   'False
      ForeColor       =   &H00000000&
      Height          =   330
      Left            =   4680
      TabIndex        =   4
      Text            =   "Combo2"
      Top             =   1200
      Width           =   3840
   End
   Begin VB.ComboBox Combo1 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H00000000&
      Height          =   330
      Left            =   120
      Style           =   2  'Dropdown List
      TabIndex        =   3
      Top             =   1200
      Width           =   3840
   End
   Begin VB.OptionButton optShort 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Short tons"
      Height          =   240
      Left            =   4080
      TabIndex        =   2
      Tag             =   "Short"
      Top             =   360
      Width           =   1215
   End
   Begin VB.OptionButton optMetric 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Metric tons"
      Height          =   240
      Left            =   2760
      TabIndex        =   1
      TabStop         =   0   'False
      Tag             =   "Metric"
      Top             =   360
      Value           =   -1  'True
      Width           =   1215
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   1
      Left            =   2520
      TabIndex        =   8
      Top             =   4440
      Visible         =   0   'False
      Width           =   855
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      BorderStyle     =   0  'None
      ForeColor       =   &H00000000&
      Height          =   225
      Index           =   0
      Left            =   1680
      TabIndex        =   0
      Top             =   360
      Width           =   855
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      Height          =   1095
      Left            =   6840
      TabIndex        =   19
      Top             =   0
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
         TabIndex        =   11
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
         TabIndex        =   10
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
         TabIndex        =   9
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
   End
   Begin VB.Frame Frame2 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Liberation model"
      ForeColor       =   &H00000000&
      Height          =   615
      Left            =   240
      TabIndex        =   18
      Top             =   3360
      Width           =   5535
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "None"
         BeginProperty Font 
            Name            =   "MS Sans Serif"
            Size            =   8.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   20
         Tag             =   "Liberation"
         Top             =   240
         Width           =   855
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Beta function model"
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
         Left            =   3360
         TabIndex        =   6
         Tag             =   "Liberation"
         Top             =   240
         Value           =   -1  'True
         Width           =   2055
      End
      Begin VB.OptionButton OptLibModel 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Ljubljana model"
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
         Left            =   1320
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Liberation"
         Top             =   240
         Width           =   1695
      End
   End
   Begin VB.Label lblClassification 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Classification function"
      Height          =   255
      Index           =   2
      Left            =   360
      TabIndex        =   46
      Top             =   5280
      Width           =   2535
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Shapness index"
      Height          =   255
      Index           =   11
      Left            =   3120
      TabIndex        =   44
      Top             =   5640
      Width           =   1335
   End
   Begin VB.Label lblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "D50  mm"
      Height          =   255
      Index           =   10
      Left            =   3120
      TabIndex        =   41
      Top             =   5400
      Width           =   1335
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Hold up in the mill"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   0
      Left            =   120
      TabIndex        =   14
      Top             =   360
      Width           =   1455
   End
   Begin VB.Label Label4 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Breakage function:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   4680
      TabIndex        =   16
      Top             =   960
      Width           =   2280
   End
   Begin VB.Label Label3 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Selection function:"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   15
      Top             =   960
      Width           =   2400
   End
   Begin VB.Label lblParameter 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Hardgrove grindability index"
      ForeColor       =   &H00000000&
      Height          =   240
      Index           =   1
      Left            =   360
      TabIndex        =   17
      Top             =   4440
      Visible         =   0   'False
      Width           =   2055
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
Attribute VB_Name = "Gmi1"
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
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
    If ChkClassification.Value = 1 Then
      ComboFunction.Visible = True
      lblParameter(10).Visible = True
      lblParameter(11).Visible = True
      LblClassification(2).Visible = True
      Parameter(10).Visible = True
      Parameter(11).Visible = True
    Else
      ComboFunction.Visible = False
      lblParameter(10).Visible = False
      lblParameter(11).Visible = False
      LblClassification(2).Visible = False
      Parameter(10).Visible = False
      Parameter(11).Visible = False
    End If
  End If
End Sub

Private Sub ChkClassification_LostFocus()
'****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub chkOverfill_Click()
  If Screen.ActiveControl.Tag = "Over" Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub chkOverfill_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub CmdAccept_Click()
'****************************
  Dim response As Integer, I As Integer
  Dim RetVal As Long
  Dim BrFuncParameters(1 To 4) As Single
  'Check that sharpness index is within proper limits
  If Val(ChkClassification) = 1 Then
    If Val(Parameter(11).text) <= 0 Or Val(Parameter(11)) >= 1 Then
       MsgBox "Sharpnes index must be > 0 and < 1", 16, "ERROR"
       Parameter(11).SetFocus
       Exit Sub
    End If
  End If
  Call MakeNew(CURRMODELDATA(ID))
  For I = 1 To 4
    BrFuncParameters(I) = CURRMODELDATA(ID).PARAM(I + 10)
  Next I
  If ChkMakeAMDiagram.Value = 1 And OptLibModel(2).Value Then
    response = MsgBox("Make Andrews-Mika diagram?", 65, "INFORMATION")
    If response = vbOK Then
      Me.MousePointer = 11
      NameOfJob = MODSIM.JOBNAME
      RetVal = BetaAMD(JobPath, NameOfJob, BrFuncParameters(1))
      Me.MousePointer = 1
      If RetVal > -1 Then
        MsgBox " Error in Anrews-Mika diagram calculation " & CStr(RetVal), 64, "INFORMATION"
        JobBetaAMD = False
      Else
        JobBetaAMD = True
      End If
    End If
  End If
  Unload Gmi1
End Sub

Private Sub CmdCancel_Click()
  Unload Gmi1
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

Private Sub Combo2_LostFocus()
'*****************************
  If CHANGE_FLAG Then Call MakeNew(NEWDATA)
End Sub

Private Sub ComboFunction_Change()
'********************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub ComboFunction_Click()
'*******************************
  If Screen.ActiveControl.Tag = "ClassificationFunction" Then
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub ComboFunction_LostFocus()
'***********************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub Combo1_Click()
'*************************
  Dim HGI As Single, AT As Single, Alpha As Single, MuT As Single
  If TypeOf Screen.ActiveControl Is ComboBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = True
  End If
  If Combo1.ListIndex = 2 Then
        'Make the Hardgrove index parameter visible
        lblParameter(1).Visible = -1
        Parameter(1).Visible = -1
  Else
   'Make it invisible
        lblParameter(1).Visible = 0
        Parameter(1).Visible = 0
  End If
  Select Case Combo1.ListIndex
    Case 0  'Standard selection function for quartz
    Parameter(6).text = CStr(0.356)
    Parameter(7).text = CStr(0.491)
    Parameter(8).text = CStr(10)
    Parameter(9).text = CStr(2#)
    Combo2.ListIndex = 0
    Case 1  'Phosphate
    Parameter(6).text = CStr(0.71)
    Parameter(7).text = CStr(0.82)
    Parameter(8).text = CStr(7.076)
    Parameter(9).text = CStr(2.38)
    Combo2.ListIndex = 1
    Case 2  'Coal in 3m diameter mill
    HGI = Val(Parameter(1).text)
    AT = 1.1 + 0.076 * (HGI - 36#)
    Parameter(6).text = Format(AT * 1.365, "0.000")
    Alpha = 0.55 + 0.0055 * HGI
    Parameter(7).text = Format(Alpha, "0.000")
    MuT = (HGI / 10 - 1.66) * (Alpha / (3# - Alpha)) ^ 0.3333
    Parameter(8).text = Format(6.91 * MuT, "#0.00")
    Parameter(9).text = CStr(2.2)
    Combo2.ListIndex = 2
    Case 3  'Taconite
    Parameter(6).text = CStr(1.441)
    Parameter(7).text = CStr(1.213)
    Parameter(8).text = CStr(5.46)
    Parameter(9).text = CStr(5.06)
    Combo2.ListIndex = 3
    Case 4  'Dolomite - sphalerite stage 1
    Parameter(6).text = CStr(2.34)
    Parameter(7).text = CStr(2.24)
    Parameter(8).text = CStr(6.14)
    Parameter(9).text = CStr(3.368)
    Combo2.ListIndex = 4
    Case 5  'Dolomite - sphalerite stage 3
    Parameter(6).text = CStr(0.9015)
    Parameter(7).text = CStr(1.589)
    Parameter(8).text = CStr(2.223)
    Parameter(9).text = CStr(0.597)
    Combo2.ListIndex = 5
    Case 6  'Limestone
    Parameter(6).text = CStr(1.56)
    Parameter(7).text = CStr(0.768)
    Parameter(8).text = CStr(1.567)
    Parameter(9).text = CStr(2.81)
    Combo2.ListIndex = 6
    Case 7  'Copper porphyry
    Parameter(6).text = CStr(0.148)
    Parameter(7).text = CStr(2.167)
    Parameter(8).text = CStr(0.626)
    Parameter(9).text = CStr(2.167)
    Combo2.ListIndex = 7
    Case 8  'User defined
    Combo2.ListIndex = 8
    Parameter(6).text = NEWDATA.PARAM(7)
    Parameter(7).text = NEWDATA.PARAM(8)
    Parameter(8).text = NEWDATA.PARAM(9)
    Parameter(9).text = NEWDATA.PARAM(10)
  End Select
End Sub

Private Sub Combo1_LostFocus()
'*****************************
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
  End If
End Sub

Private Sub Combo2_Click()
'*************************
  Dim HGI As Single
  Select Case Combo2.ListIndex
    Case 0  'Standard breakage function for quartzite
    Parameter(2).text = CStr(3.2)
    Parameter(3).text = CStr(0.52)
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.158)
    Case 1  'Phosphate
    Parameter(2).text = CStr(3.86)
    Parameter(3).text = CStr(0.86)
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.43)
    Case 2  'Coal in 3m diameter mill
    HGI = Val(Parameter(1).text)
    Parameter(2).text = Format(2.8, "0.000")
    Parameter(3).text = Format(1.2 - 0.0045 * HGI, "0.000")
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.49)
    Case 3  'Taconite
    Parameter(2).text = CStr(0.5)
    Parameter(3).text = CStr(1.329)
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.599)
    Case 4  'Dolomite - sphalerite stage 1
    Parameter(2).text = CStr(9.24)
    Parameter(3).text = CStr(0.704)
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.604)
    Case 5  'Dolomite - sphalerite stage 3
    Parameter(2).text = CStr(2.615)
    Parameter(3).text = CStr(0.625)
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.582)
    Case 6  'Limestone
    Parameter(2).text = CStr(0.441)
    Parameter(3).text = CStr(1.714)
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.501)
    Case 7  'Copper porphyry
    Parameter(2).text = CStr(0.606)
    Parameter(3).text = CStr(0.542)
    Parameter(4).text = CStr(0)
    Parameter(5).text = CStr(0.719)
    Case 8  'User defined
    Parameter(2).text = NEWDATA.PARAM(11)
    Parameter(3).text = NEWDATA.PARAM(12)
    Parameter(4).text = NEWDATA.PARAM(13)
    Parameter(5).text = NEWDATA.PARAM(14)
  End Select
End Sub

Private Sub Form_Load()
'**********************
On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)

  ID = UnitMods.ID
  CHANGE_FLAG = 0
  Combo1.AddItem "Quartzite"
  Combo1.AddItem "Phosphate"
  Combo1.AddItem "Coal in 3m diameter mill with 5 cm balls"
  Combo1.AddItem "Taconite"
  Combo1.AddItem "Dolomite-sphalerite  stage 1"
  Combo1.AddItem "Dolomite-sphalerite  stage 3"
  Combo1.AddItem "Limestone"
  Combo1.AddItem "Copper porphyry"
  Combo1.AddItem "User defined"
  Combo2.AddItem "Quartzite"
  Combo2.AddItem "Phosphate"
  Combo2.AddItem "Coal in 3m diameter mill with 5 cm balls"
  Combo2.AddItem "Taconite"
  Combo2.AddItem "Dolomite-sphalerite  stage 1"
  Combo2.AddItem "Dolomite-sphalerite  stage 3"
  Combo2.AddItem "Limestone"
  Combo2.AddItem "Copper porphyry"
  Combo2.AddItem "User defined"
  ComboFunction.AddItem "Logistic"
  ComboFunction.AddItem "Rosin Rammler"
  ComboFunction.AddItem "Exponential sum"
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "GMI1" Then
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
'************************
  Parameter(0).text = Str$(70)
  optMetric.Value = -1
  Combo1.ListIndex = 0
  Combo2.ListIndex = 0
  chkOverfill.Value = 0
  Parameter(1).text = Str$(55)
  lblParameter(1).Visible = False
  Parameter(1).Visible = False
  OptLibModel(0).Value = True
  ChkClassification.Value = 0
  ComboFunction.Visible = False
  ComboFunction.ListIndex = 0
  lblParameter(10).Visible = False
  lblParameter(11).Visible = False
  LblClassification(2).Visible = False
  Parameter(10).Visible = False
  Parameter(11).Visible = False
  Parameter(10).text = CStr(2)
  Parameter(11).text = CStr(0.7)
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
  Parameter(0).text = Str$(NDATA.PARAM(1))
  optMetric.Value = True
  Combo1.ListIndex = NDATA.PARAM(2) - 1
  Combo2.ListIndex = NDATA.PARAM(3) - 1
  chkOverfill.Value = NDATA.PARAM(4)
  Parameter(1).text = Str$(NDATA.PARAM(5))
  If NDATA.PARAM(6) = 0 Then OptLibModel(0).Value = -1
  If NDATA.PARAM(6) = 1 Then OptLibModel(1).Value = -1
  If NDATA.PARAM(6) = 2 Then OptLibModel(2).Value = -1
  Parameter(6).text = CStr(NDATA.PARAM(7))
  Parameter(7).text = CStr(NDATA.PARAM(8))
  Parameter(8).text = CStr(NDATA.PARAM(9))
  Parameter(9).text = CStr(NDATA.PARAM(10))
  Parameter(2).text = CStr(NDATA.PARAM(11))
  Parameter(3).text = CStr(NDATA.PARAM(12))
  Parameter(4).text = CStr(NDATA.PARAM(13))
  Parameter(5).text = CStr(NDATA.PARAM(14))
  If NDATA.PARAM(15) > 0 Then
    ChkClassification.Value = 1
    ComboFunction.Visible = True
    lblParameter(10).Visible = True
    lblParameter(11).Visible = True
    LblClassification(2).Visible = True
    Parameter(10).Visible = True
    Parameter(11).Visible = True
    ComboFunction.ListIndex = NDATA.PARAM(15) - 1
    Parameter(10).text = NDATA.PARAM(16)
    Parameter(11).text = NDATA.PARAM(17)
  Else
    ChkClassification.Value = 0
    ComboFunction.Visible = False
    lblParameter(10).Visible = False
    lblParameter(11).Visible = False
    LblClassification(2).Visible = False
    Parameter(10).Visible = False
    Parameter(11).Visible = False
  End If
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  CHANGE_FLAG = 0
  NDATA.MODEL = "GMI1"
  NDATA.PARAM(1) = Val(Parameter(0).text)
  If optMetric.Value = -1 Then NDATA.PARAM(1) = NDATA.PARAM(1) * 1#
  If optShort.Value = -1 Then NDATA.PARAM(1) = NDATA.PARAM(1) * 0.90718
  NDATA.PARAM(2) = Combo1.ListIndex + 1
  NDATA.PARAM(3) = Combo2.ListIndex + 1
  NDATA.PARAM(4) = chkOverfill.Value
  NDATA.PARAM(5) = Val(Parameter(1).text)
  If OptLibModel(0).Value Then NDATA.PARAM(6) = 0
  If OptLibModel(1).Value Then NDATA.PARAM(6) = 1
  If OptLibModel(2).Value Then NDATA.PARAM(6) = 2
  NDATA.PARAM(7) = Val(Parameter(6).text)
  NDATA.PARAM(8) = Val(Parameter(7).text)
  NDATA.PARAM(9) = Val(Parameter(8).text)
  NDATA.PARAM(10) = Val(Parameter(9).text)
  NDATA.PARAM(11) = Val(Parameter(2).text)
  NDATA.PARAM(12) = Val(Parameter(3).text)
  NDATA.PARAM(13) = Val(Parameter(4).text)
  NDATA.PARAM(14) = Val(Parameter(5).text)
  If ChkClassification.Value = 0 Then
    NDATA.NOPAR = 15
    NDATA.PARAM(15) = 0
  Else
    NDATA.PARAM(15) = ComboFunction.ListIndex + 1
    NDATA.NOPAR = 17
    NDATA.PARAM(16) = Val(Parameter(10).text)
    NDATA.PARAM(17) = Val(Parameter(11).text)
  End If
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Dim CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
PrintForm
End Sub

Private Sub OptLibModel_Click(Index As Integer)
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

Private Sub optMetric_Click()
  If Screen.ActiveControl.Tag = "Metric" Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optMetric_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
End Sub

Private Sub OptNew_Click()
'*************************
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Private Sub optShort_Click()
  If Screen.ActiveControl.Tag = "Short" Then
        If OptNew.Value = 0 Then OptNew.Value = -1
        CHANGE_FLAG = -1
  End If
End Sub

Private Sub optShort_LostFocus()
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
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
    If Index >= 6 And Index <= 9 And Combo1.ListIndex < 8 Then
      Combo1.ListIndex = 8
    End If
    If Index >= 2 And Index <= 5 And Combo2.ListIndex < 8 Then
      Combo1.ListIndex = 8
    End If
    If Index >= 2 And Index <= 5 Then
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
  If Index = 1 Then IDIFF = 4
  If Index <= 5 And Index >= 2 Then IDIFF = 9
  If Index >= 10 Then IDIFF = 6
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

