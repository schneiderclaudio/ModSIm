VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form CurveFit 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "MODSIM Curve fit"
   ClientHeight    =   7410
   ClientLeft      =   150
   ClientTop       =   435
   ClientWidth     =   10440
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   ScaleHeight     =   7410
   ScaleWidth      =   10440
   Begin VB.CommandButton cmdRemove 
      Caption         =   "Remove"
      Height          =   375
      Left            =   3120
      TabIndex        =   33
      Top             =   4560
      Width           =   975
   End
   Begin VB.CommandButton cmdAdd 
      Appearance      =   0  'Flat
      Caption         =   "Add"
      Height          =   375
      Left            =   3120
      TabIndex        =   32
      Top             =   4080
      Width           =   975
   End
   Begin VB.ListBox lstSizeDistributions 
      Height          =   3210
      Left            =   120
      TabIndex        =   31
      Top             =   3600
      Width           =   2895
   End
   Begin VB.ComboBox cboOptMethod 
      Height          =   330
      Left            =   6600
      TabIndex        =   30
      Text            =   "Select optimization method"
      Top             =   3600
      Width           =   3615
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H80000016&
      Caption         =   "Estimation results"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3255
      Left            =   120
      TabIndex        =   1
      Top             =   0
      Width           =   10215
      Begin VB.CommandButton cmdReset 
         Caption         =   "Reset parameter selections"
         Height          =   325
         Left            =   7560
         TabIndex        =   38
         Top             =   2880
         Width           =   2175
      End
      Begin VB.CommandButton cmdAcceptPars 
         Caption         =   "Accept these parameters"
         Enabled         =   0   'False
         Height          =   325
         Left            =   7560
         TabIndex        =   36
         Top             =   2520
         Width           =   2175
      End
      Begin VB.CheckBox chkConfidenceInterval 
         Caption         =   "Report confidence intervals"
         Height          =   255
         Left            =   120
         TabIndex        =   35
         Top             =   1800
         Width           =   2415
      End
      Begin VB.Label lblSOFSQ 
         Height          =   255
         Index           =   1
         Left            =   3360
         TabIndex        =   29
         Top             =   2880
         Width           =   1335
      End
      Begin VB.Label lblSOFSQ 
         Height          =   255
         Index           =   0
         Left            =   3360
         TabIndex        =   28
         Top             =   2520
         Width           =   1335
      End
      Begin VB.Label lblConfInterval 
         Height          =   255
         Index           =   4
         Left            =   8640
         TabIndex        =   27
         Top             =   2160
         Visible         =   0   'False
         Width           =   1455
      End
      Begin VB.Label lblConfInterval 
         Height          =   255
         Index           =   3
         Left            =   7080
         TabIndex        =   26
         Top             =   2160
         Visible         =   0   'False
         Width           =   1455
      End
      Begin VB.Label lblConfInterval 
         Height          =   255
         Index           =   2
         Left            =   5520
         TabIndex        =   25
         Top             =   2160
         Visible         =   0   'False
         Width           =   1455
      End
      Begin VB.Label lblConfInterval 
         Height          =   255
         Index           =   1
         Left            =   3960
         TabIndex        =   24
         Top             =   2160
         Visible         =   0   'False
         Width           =   1455
      End
      Begin VB.Label lblConfInterval 
         Height          =   255
         Index           =   0
         Left            =   2400
         TabIndex        =   23
         Top             =   2160
         Visible         =   0   'False
         Width           =   1455
      End
      Begin VB.Label lblParvalsAtEnd 
         Height          =   255
         Index           =   4
         Left            =   8640
         TabIndex        =   22
         Top             =   1320
         Width           =   1455
      End
      Begin VB.Label lblParvalsAtEnd 
         Height          =   255
         Index           =   3
         Left            =   7080
         TabIndex        =   21
         Top             =   1320
         Width           =   1455
      End
      Begin VB.Label lblParvalsAtEnd 
         Height          =   255
         Index           =   2
         Left            =   5520
         TabIndex        =   20
         Top             =   1320
         Width           =   1455
      End
      Begin VB.Label lblParvalsAtEnd 
         Height          =   255
         Index           =   1
         Left            =   3960
         TabIndex        =   19
         Top             =   1320
         Width           =   1455
      End
      Begin VB.Label lblParvalsAtEnd 
         Height          =   255
         Index           =   0
         Left            =   2400
         TabIndex        =   18
         Top             =   1320
         Width           =   1455
      End
      Begin VB.Label lblParValsAtStart 
         Height          =   255
         Index           =   4
         Left            =   8640
         TabIndex        =   17
         Top             =   960
         Width           =   1455
      End
      Begin VB.Label lblParValsAtStart 
         Height          =   255
         Index           =   3
         Left            =   7080
         TabIndex        =   16
         Top             =   960
         Width           =   1455
      End
      Begin VB.Label lblParValsAtStart 
         Height          =   255
         Index           =   2
         Left            =   5520
         TabIndex        =   15
         Top             =   960
         Width           =   1455
      End
      Begin VB.Label lblParValsAtStart 
         Height          =   255
         Index           =   1
         Left            =   3960
         TabIndex        =   14
         Top             =   960
         Width           =   1455
      End
      Begin VB.Label lblParValsAtStart 
         Height          =   255
         Index           =   0
         Left            =   2400
         TabIndex        =   13
         Top             =   960
         Width           =   1455
      End
      Begin VB.Label lblResults 
         Alignment       =   1  'Right Justify
         Caption         =   "Sum of squared deviations at end"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   5
         Left            =   120
         TabIndex        =   12
         Top             =   2880
         Width           =   3135
      End
      Begin VB.Label lblResults 
         Alignment       =   1  'Right Justify
         Caption         =   "Sum of squared deviations at start"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   4
         Left            =   120
         TabIndex        =   11
         Top             =   2520
         Width           =   3135
      End
      Begin VB.Label lblResults 
         Alignment       =   1  'Right Justify
         Caption         =   "95% confidence interval"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   10
         Top             =   2160
         Visible         =   0   'False
         Width           =   2200
      End
      Begin VB.Label lblResults 
         Alignment       =   1  'Right Justify
         Caption         =   "Values after estimation"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   9
         Top             =   1320
         Width           =   2200
      End
      Begin VB.Label lblResults 
         Alignment       =   1  'Right Justify
         Caption         =   "Values at start"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   8
         Top             =   960
         Width           =   2200
      End
      Begin VB.Label lblParNames 
         Height          =   645
         Index           =   4
         Left            =   8640
         TabIndex        =   7
         Top             =   240
         Width           =   1455
      End
      Begin VB.Label lblParNames 
         Height          =   645
         Index           =   3
         Left            =   7080
         TabIndex        =   6
         Top             =   240
         Width           =   1455
      End
      Begin VB.Label lblParNames 
         Height          =   645
         Index           =   2
         Left            =   5520
         TabIndex        =   5
         Top             =   240
         Width           =   1455
      End
      Begin VB.Label lblParNames 
         Height          =   645
         Index           =   1
         Left            =   3960
         TabIndex        =   4
         Top             =   240
         Width           =   1455
      End
      Begin VB.Label lblParNames 
         Height          =   645
         Index           =   0
         Left            =   2400
         TabIndex        =   3
         Top             =   240
         Width           =   1455
      End
      Begin VB.Label lblResults 
         Alignment       =   1  'Right Justify
         Caption         =   "Parameters"
         BeginProperty Font 
            Name            =   "Arial"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   2
         Top             =   360
         Width           =   2200
      End
   End
   Begin VB.ComboBox cboSelectPSD 
      Height          =   330
      Left            =   3120
      TabIndex        =   0
      Text            =   "Select size distributions to fit"
      Top             =   3600
      Width           =   3375
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   9600
      Top             =   4680
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label Label2 
      Caption         =   "Optimization  method"
      Height          =   255
      Left            =   6600
      TabIndex        =   37
      Top             =   3360
      Width           =   3015
   End
   Begin VB.Label Label1 
      Caption         =   "Size distributions to fit"
      Height          =   255
      Left            =   120
      TabIndex        =   34
      Top             =   3360
      Width           =   2895
   End
   Begin VB.Menu mnuFile 
      Caption         =   "File"
      Begin VB.Menu mnuOpenJob 
         Caption         =   "Open job"
      End
      Begin VB.Menu mnuKeepPars 
         Caption         =   "Accept these parameters"
      End
      Begin VB.Menu mnuSave 
         Caption         =   "Save job"
      End
      Begin VB.Menu mnuprint 
         Caption         =   "Print"
      End
      Begin VB.Menu mnuExit 
         Caption         =   "Exit"
      End
   End
   Begin VB.Menu mnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu mnuEditSize 
         Caption         =   "Edit size data"
      End
      Begin VB.Menu mnuEditPars 
         Caption         =   "Edit parameters"
      End
   End
   Begin VB.Menu mnuRun 
      Caption         =   "Run"
      Begin VB.Menu mnuRunEstimator 
         Caption         =   "Run estimator"
      End
   End
   Begin VB.Menu mnuView 
      Caption         =   "View"
      Begin VB.Menu mnuShowPSD 
         Caption         =   "Size distribution"
      End
   End
End
Attribute VB_Name = "CurveFit"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
  Private NumberOfParameters As Long
  Private N_DATA_SETS As Integer
  Private Parameters(1 To 10) As Single
  Public NDC As Integer, NGC As Integer, NSC As Integer

Private Sub chkConfidenceInterval_Click()
'****************************************
  Dim I As Integer
  If Me.chkConfidenceInterval = vbChecked Then
    lblResults(3).Visible = True
    For I = 0 To 4
      lblConfInterval(I).Visible = True
    Next I
  Else
    lblResults(3).Visible = False
    For I = 0 To 4
      lblConfInterval(I).Visible = False
    Next I
  End If
End Sub

Private Sub cmdAcceptPars_Click()
'********************************
  Dim I As Integer, ID As Integer, N As Integer
  
  On Error GoTo ErrHandler
  For I = 1 To NumberOfParameters
    ID = RepUnitNumber(I)
    CURRMODELDATA(ID).PARAM(ParameterNumber(I)) = Parameters(I)
    StartValue(I) = Parameters(I)
    Me.lblParValsAtStart(I - 1).Caption = Parameters(I)
    Me.lblParvalsAtEnd(I - 1).Caption = ""
  Next I
  Me.lblSOFSQ(0).Caption = ""
  Me.lblSOFSQ(1).Caption = ""
  
  'Write the parameters so that they can be read by the DLL
  Call UnitMods.cmdQuit_Click
  Exit Sub
  
ErrHandler:
  MsgBox " Error saving parameter data", vbCritical, "ERROR"
  Exit Sub
  
End Sub

Private Sub cmdAdd_Click()
'*************************
Me.lstSizeDistributions.AddItem Me.cboSelectPSD.text
End Sub

Private Sub cmdRemove_Click()
'****************************
If Me.lstSizeDistributions.ListIndex >= 0 Then
  Me.lstSizeDistributions.RemoveItem Me.lstSizeDistributions.ListIndex
End If
End Sub

Private Sub cmdReset_Click()
'***************************
  Call ResetLevels
  Call ClearDisplay
  Me.cmdAcceptPars.Enabled = False
End Sub

Private Sub Form_Load()
'**********************
  'Fill the optimization method list
  Me.cboOptMethod.AddItem "UNLSF Unconstrained Levenberg-Marquardt"
  Me.cboOptMethod.AddItem "UMCGF Unconstrained congugate gradient"
  Me.cboOptMethod.AddItem "BCLSF Constrained Levenberg-Marquardt"
End Sub

Private Sub mnuEditPars_Click()
'******************************
  Dim I As Integer
  FastParameterChange = False
  Call StartUnitMods
  UnitMods.Show 1
  Call ClearDisplay
  
  'Display parameter values
  For I = 1 To Repeat.Level
    CurveFit.lblParNames(I - 1).Caption = ParameterName(I)
    CurveFit.lblParValsAtStart(I - 1).Caption = StartValue(I)
  Next I
  For I = Repeat.Level + 1 To 5
    CurveFit.lblParNames(I - 1).Caption = ""
    CurveFit.lblParValsAtStart(I - 1).Caption = ""
  Next I
End Sub

Private Sub mnuKeepPars_Click()
  Call cmdAcceptPars_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
  Me.PrintForm
End Sub

Private Sub mnuRunEstimator_Click()
'**********************************
'Call the estimator DLL
  Dim LowerConflimit(1 To 10) As Single, UpperConfLimit(1 To 10) As Single
  Dim ConfLevel As Single
  Dim SumOfSquares(2) As Single
  Dim LowB(1 To 10) As Single, UpB(1 To 10) As Single
  Dim NumberOfDistributions As Integer
  Dim I As Integer, N1 As Integer, N2 As Integer, NN As Integer
  Dim N As Integer, ID As Integer
    
  On Error GoTo ErrHandler
  If Me.cboOptMethod.text = "Select optimization method" Then
    MsgBox "An optimization method has not been selected", vbInformation, "WARNING"
    Me.cboOptMethod.SetFocus
    Exit Sub
  End If
  If Me.lstSizeDistributions.ListCount = 0 Then
    MsgBox "No size distributions have not been selected", vbExclamation, "WARNING"
    Me.cboSelectPSD.SetFocus
    Exit Sub
  End If
  If Repeat.Level = 0 Then
    MsgBox "Parameters to be estimated have not been selected." & vbCrLf & "Edit parameters using the Edit menu", vbExclamation, "WARNING"
    Exit Sub
  End If
  
  'Clear the data display
  For I = 1 To 5
    Me.lblParvalsAtEnd(I - 1).Caption = ""
    Me.lblConfInterval(I - 1).Caption = ""
  Next I
  Me.lblSOFSQ(0).Caption = ""
  Me.lblSOFSQ(1).Caption = ""
  
  NumberOfParameters = Repeat.Level
  For I = 1 To NumberOfParameters
    Parameters(I) = StartValue(I)
    If Mid(cboOptMethod.text, 1, 5) = "BCLSF" Then
      If (StepValue(I) <= Parameters(I)) Or (Parameters(I) <= EndValue(I)) Then
        MsgBox "Impermissible bounds have been specified for parameter " & CStr(I), vbCritical, "ERROR"
        Exit Sub
      End If
      LowB(I) = EndValue(I)
      UpB(I) = StepValue(I)
    End If
  Next I
  
  'Write parameter data for regression program
  Open RTrim(JobPath) & "LevMarCurveFit.dat" For Output As 37
  Print #37, Trim(MODSIM.JOBNAME)
  Print #37, Mid(Me.cboOptMethod.text, 1, 5)
    For I = 1 To NumberOfParameters
      Print #37, RepUnitNumber(I), UnitID(I), ParameterNumber(I)
  Next I
  'Write the data to match by regression
  NumberOfDistributions = Me.lstSizeDistributions.ListCount
  Print #37, NumberOfDistributions, NDC
  For I = 1 To NumberOfDistributions
    For ID = 1 To N_DATA_SETS
      If CURRODATA(ID).NSTR = Val(Me.lstSizeDistributions.List(I - 1)) Then
        Print #37, CURRODATA(ID).SET_NAME
        Print #37, CURRODATA(ID).NSTR; CURRODATA(ID).NDC
        For N = 1 To CURRODATA(ID).NDC - 1
          Print #37, CURRODATA(ID).CUMSIZE(N), CURRODATA(ID).fraction(N)
        Next N
      End If
    Next ID
  Next I
  Close 37
  If Me.chkConfidenceInterval = vbChecked Then
    ConfLevel = 95
  Else
    ConfLevel = 0
  End If
  Screen.MousePointer = 11
  Call LevMarCurveFit(JobPath, JobFileName, NumberOfParameters, Parameters(1), LowB(1), UpB(1), SumOfSquares(1), LowerConflimit(1), UpperConfLimit(1), ConfLevel)
  Screen.MousePointer = 0
  
    'Display parameter values
  For I = 1 To NumberOfParameters
    CurveFit.lblParvalsAtEnd(I - 1).Caption = Parameters(I)
  Next I
  'Display the confidence limits
  For I = 1 To NumberOfParameters
    CurveFit.lblConfInterval(I - 1).Caption = Format(LowerConflimit(I), "0.00e-0") & " to " & Format(UpperConfLimit(I), "0.00e-0")
  Next I
  CurveFit.lblSOFSQ(0).Caption = SumOfSquares(1)
  CurveFit.lblSOFSQ(1).Caption = SumOfSquares(2)
  Me.cmdAcceptPars.Enabled = True
  Exit Sub
  
ErrHandler:
  MsgBox "Error occured during the estimation calculation", vbCritical, "ERROR"
  Screen.MousePointer = 0
  Exit Sub
End Sub

Private Sub ReadPSD()
'********************
  'Get the size distribution data to fit from the job file
  Dim N As Integer
  Dim SET_NAME As String, JOBNAME As String
  Dim ID As Integer
  Dim StreamNumber As Integer, NumberOfSizes As Integer
  Dim I As Integer
  
  On Error GoTo ErrHandler:
  Open RTrim(JobPath) & "SizeComp.out" For Input As #35
  For N = 1 To 30
    CURRODATA(N).ID = 0
  Next N
  N_DATA_SETS = 0
  ID = 0
  Line Input #35, WSTRING
  JOBNAME = RTrim(WSTRING)
  Line Input #35, WSTRING
  If Mid(WSTRING, 1, 6) <> "Stream" Then
    MsgBox "The job file contains no size distributions", vbCritical, "ERROR"
    Exit Sub
  Else
    Do While Mid(WSTRING, 1, 11) <> "END OF FILE"
      SET_NAME = RTrim(Mid(WSTRING, 20))
      StreamNumber = Val(Mid(WSTRING, 11, 4))
      Line Input #35, WSTRING
      If Mid(WSTRING, 1, 8) = "Feedrate" Then
        Line Input #35, WSTRING
      End If
      If Mid(WSTRING, 1, 10) = "Water feed" Then
        Line Input #35, WSTRING
      End If
      If Mid(WSTRING, 1, 9) = "Size dist" Then
        ID = ID + 1
        N_DATA_SETS = N_DATA_SETS + 1
        If N_DATA_SETS > 30 Then
          MsgBox "Too many size distributions are in the job file", vbCritical, "ERROR"
          Exit Sub
        End If
        CURRODATA(ID).SET_NAME = SET_NAME
        CURRODATA(ID).NSTR = StreamNumber
        NumberOfSizes = Val(Mid(WSTRING, 11, 4))
        CURRODATA(ID).NDC = NumberOfSizes + 1
        CURRODATA(ID).ID = ID
        If CURRODATA(ID).NDC > 1 Then
          Sizgraph.StreamList.AddItem CStr(ID) & " " & CURRODATA(ID).SET_NAME
          Me.cboSelectPSD.AddItem CStr(StreamNumber) & " " & CURRODATA(ID).SET_NAME
        End If
        For N = 1 To NumberOfSizes
          Input #35, CURRODATA(ID).CUMSIZE(N), CURRODATA(ID).fraction(N)
        Next N
        If CURRODATA(ID).fraction(1) > 1 Then
          For N = 1 To NumberOfSizes
            CURRODATA(ID).fraction(N) = CURRODATA(ID).fraction(N) * 0.01
          Next N
        End If
        Line Input #35, WSTRING
      End If
    Loop
  End If
  Close #35
  Load Sizgraph
  If N_DATA_SETS > 0 Then
    Sizgraph.CmdEditData.Enabled = True
    Sizgraph.CmdSaveData.Enabled = True
    Sizgraph.CmdSaveDataAs.Enabled = True
  End If

  NumberOfDataSets = N_DATA_SETS
  Sizgraph.Hide
  Exit Sub
ErrHandler:
  MsgBox "Error reading size distribution data", vbCritical, "ERROR"
  Exit Sub
  
End Sub

Private Sub mnuSave_Click()
'**************************
  On Error GoTo ErrHandler
  FileCopy RTrim(JobPath) & "CURRDATA.RUN", RTrim(JobFileName) & ".cur"
  FileCopy RTrim(JobPath) & "SIZECOMP.OUT", RTrim(JobFileName) & ".siz"
  Exit Sub
  
ErrHandler:
  MsgBox "Error saving job", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub mnuShowPSD_Click()
'*****************************
  Dim N As Integer, I As Integer
  Dim SET_NAME As String, JOBNAME As String
  Dim ID As Integer
  Dim StreamNumber As Integer, NumberOfSizes As Integer
  
  On Error GoTo ErrHandler
  ID = N_DATA_SETS
  Open RTrim(JobPath) & "LevMarCurveFit.out" For Input As #35
  Line Input #35, WSTRING
  JOBNAME = RTrim(WSTRING)
  Line Input #35, WSTRING
  If Mid(WSTRING, 1, 6) <> "Stream" Then
    MsgBox "The job file contains no size distributions", vbCritical, "ERROR"
    Exit Sub
  Else
    Do While Mid(WSTRING, 1, 11) <> "END OF FILE"
      SET_NAME = RTrim(Mid(WSTRING, 20))
      StreamNumber = Val(Mid(WSTRING, 7, 5))
      'Find the name of the stream
      For I = 1 To ID
        If CURRODATA(I).NSTR = StreamNumber Then
          SET_NAME = Trim(CURRODATA(I).SET_NAME) & SET_NAME
          Exit For
        End If
      Next I
      
      Line Input #35, WSTRING
      If Mid(WSTRING, 1, 8) = "Feedrate" Then
        Line Input #35, WSTRING
      End If
      If Mid(WSTRING, 1, 10) = "Water feed" Then
        Line Input #35, WSTRING
      End If
      If Mid(WSTRING, 1, 9) = "Size dist" Then
        ID = ID + 1
        N_DATA_SETS = N_DATA_SETS + 1
        If N_DATA_SETS > 30 Then
          MsgBox "Too many size distributions have been specified", vbCritical, "ERROR"
          Exit Sub
        End If
        CURRODATA(ID).SET_NAME = SET_NAME
        NumberOfSizes = Val(Mid(WSTRING, 11, 4))
        CURRODATA(ID).ID = ID
        Sizgraph.StreamList.AddItem CStr(ID) & " " & CURRODATA(ID).SET_NAME
        For N = 1 To NumberOfSizes
          Input #35, CURRODATA(ID).CUMSIZE(N), CURRODATA(ID).fraction(N)
        Next N
        CURRODATA(ID).NDC = NumberOfSizes + 1
        If CURRODATA(ID).fraction(1) > 1 Then
          For N = 1 To NumberOfSizes
            CURRODATA(ID).fraction(N) = CURRODATA(ID).fraction(N) * 0.01
          Next N
        End If
        Line Input #35, WSTRING
      End If
    Loop
  End If
  Close #35
  Sizgraph.CmdReturn.Caption = "Exit"
  Sizgraph.Show
  Exit Sub
  
ErrHandler:
  MsgBox "Error displaying size distribution data", vbCritical, "ERROR"
  Close #35
  Exit Sub
End Sub

Private Sub mnuEditSize_Click()
'********************************
  Sizgraph.Show
End Sub

Private Sub MnuExit_Click()
'**************************
  End
End Sub

Private Sub mnuOpenJob_Click()
'*****************************
  'Find the the job file name for the saved job
  On Error GoTo ErrHandler
  Dim I As Integer
    Call ExistingJob_Click
    Call ReadParameterData
    Call ReadPSD
    Call ReadSystemData
'  Else
    Exit Sub
'  End If
  Exit Sub
ErrHandler:
  Exit Sub
End Sub

Sub ClearDisplay()
'*****************
  'Clears the result display
  Dim I As Integer
  For I = 0 To 4
    Me.lblParNames(I).Caption = ""
    Me.lblParValsAtStart(I).Caption = ""
    Me.lblParvalsAtEnd(I).Caption = ""
    Me.lblConfInterval(I).Caption = ""
  Next I
  Me.lblSOFSQ(0).Caption = ""
  Me.lblSOFSQ(1).Caption = ""
End Sub

Private Sub ExistingJob_Click()
'******************************
'Load an existing job
'This subroutine taken from MODSIM
  Dim I As Integer, K As Integer
  Dim JobFlag As Boolean
  On Error GoTo ErrHandler1
  ' Set filters
  CommonDialog1.CancelError = True
  CommonDialog1.Filter = "Job files (*.job)|*.job|All files|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.InitDir = Trim(JobSavePath)
  CommonDialog1.ShowOpen
  WSTRING = Trim(CommonDialog1.FileName)
  
  On Error GoTo ErrHandler
  Call ClearWorkspace
  'Strip off any file type extension
  I = InStr(WSTRING, ".")
  If I > 0 Then
    JobFileName = Mid(WSTRING, 1, I - 1)
  Else
    JobFileName = WSTRING
  End If
  JOBNAME = JobFileName
  I = 1
  K = 0
  Do While I > 0
    I = InStr(JOBNAME, "\")
    K = K + I
    JOBNAME = Mid(JOBNAME, I + 1)
  Loop
  MODSIM.JOBNAME = JOBNAME
  Me.MousePointer = 11
  Open RTrim(JobFileName) & ".JOB" For Input As #31
  Input #31, WSTRING
  If WSTRING = "Job name" Then
    Input #31, WSTRING
    JOBNAME = Trim(WSTRING)
    JobSavePath = Mid(JobFileName, 1, K)
    Me.Caption = "MODSIM parameter estimation for " & Trim(JobSavePath) & JOBNAME
  Else
    MsgBox "Incorrect file type", 16, "ERROR"
    Close #31
    Me.MousePointer = 1
    Exit Sub
  End If
    
  Do While Not EOF(31)
    Input #31, WSTRING, JobFlag
    Select Case WSTRING
    Case "Flowsheet"
      JobFlowsheet = JobFlag
      If JobFlowsheet Then
        FileCopy RTrim(JobFileName) & ".TRN", RTrim(JobPath) & "PLNTR.OUT"
        'Set the saved flag to true because a freshly loaded job does not require saving
        SavedJob = True
      End If

    Case "System data"
      JobSystemData = JobFlag
      If JobSystemData Then
        FileCopy RTrim(JobFileName) & ".syd", RTrim(JobPath) & "SYSDATA.OUT"
      End If
  
    Case "Model data"
      JobModelData = JobFlag
      If JobModelData Then
        FileCopy RTrim(JobFileName) & ".mod", RTrim(JobPath) & "MODSDATA.OUT"
      End If
  
    Case "System input data"
      JobSystemInputData = JobFlag
      If JobSystemInputData Then
        FileCopy RTrim(JobFileName) & ".sid", RTrim(JobPath) & "CURRDATA.SYD"
      End If

    Case "Size data"
      JobSizeData = JobFlag
      If JobSizeData Then
        FileCopy RTrim(JobFileName) & ".siz", RTrim(JobPath) & "SIZECOMP.OUT"
      End If

    Case "G-class distribution"
      JobGCDistribution = JobFlag
      If JobGCDistribution Then
        FileCopy RTrim(JobFileName) & ".gcd", RTrim(JobPath) & "GC_DISTR.OUT"
      End If

    Case "Liberation data"
      JobLibcomp = JobFlag
      If JobLibcomp Then
        FileCopy RTrim(JobFileName) & ".lib", RTrim(JobPath) & "LIBCOMP.OUT"
      End If

    Case "Ljubljana model"
      JobLjubAMD = JobFlag
      If JobLjubAMD Then
        FileCopy RTrim(JobFileName) & ".lju", RTrim(JobPath) & "LJUBAMD.DAT"
      End If

    Case "Beta function liberation model"
      JobBetaAMD = JobFlag
      If JobBetaAMD Then
        FileCopy RTrim(JobFileName) & ".amd", RTrim(JobPath) & "BetaAMD.DAT"
      End If

    Case "S-class distribution"
      JobSCDistribution = JobFlag
      If JobSCDistribution Then
        FileCopy RTrim(JobFileName) & ".scd", RTrim(JobPath) & "SC_DISTR.OUT"
      End If

    Case "Washability data"
     JobWashabilityData = JobFlag
     If JobWashabilityData Then
        FileCopy RTrim(JobFileName) & ".was", RTrim(JobPath) & "WASHDATA.OUT"
      End If
      
    Case "System data needed"
      JobSystemDataNeeded = JobFlag

    Case "Model parameter data"
      JobModelParameterData = JobFlag
      If JobModelParameterData Then
        FileCopy RTrim(JobFileName) & ".cur", RTrim(JobPath) & "CURRDATA.RUN"
      End If
      
    Case "Unit parameters needed"
      JobUnitParametersNeeded = JobFlag

    Case "Output format"
      JobOutputFormat = JobFlag
      If JobOutputFormat Then
        FileCopy RTrim(JobFileName) & ".mat", RTrim(JobPath) & "FORMAT.OUT"
      End If
 
    Case "SIMOP data"
      JobSimopData = JobFlag
      If JobSimopData Then
        FileCopy RTrim(JobFileName) & ".mop", RTrim(JobPath) & "SIMOP.DAT"
      End If
 
    Case Else
      GoTo ErrHandler
      
    End Select
  Loop
  Close #31
  
  On Error GoTo ErrHandler2
  FileCopy RTrim(JobFileName) & ".TEA", RTrim(JobPath) & "TEARS.OUT"
  
  Me.MousePointer = 0
  Exit Sub
  
ErrHandler:
  MsgBox "Job was not loaded properly", 16, "WARNING"
  Close #31
  Me.MousePointer = 0
  Exit Sub
  
ErrHandler1:
  'User pressed Cancel button
  Me.MousePointer = 0
  Exit Sub
  
ErrHandler2:
  'Tear stream data is not available
  Open "TEARS.OUT" For Output As #41
  Print #41, "LAST 0 0 0 0"
  Close #41
  Me.MousePointer = 0
  Exit Sub
  
End Sub

Private Sub ReadParameterData()
'******************************
  Dim I As Integer, ID As Integer
  
  On Error GoTo ErrHandler
  Open RTrim(JobPath) & "CURRDATA.RUN" For Input As 32
  Do
    Line Input #32, WSTRING
    If Mid(WSTRING, 1, 4) = "TYPE" Then
      ID = Val(Mid(WSTRING, 19, 3))
      UnitMods.ID = ID
      CURRMODELDATA(ID).ID = ID
      CURRMODELDATA(ID).MODEL = Mid(WSTRING, 10, 4)
      CURRMODELDATA(ID).Unit = Val(Mid(WSTRING, 6, 3))
      CURRMODELDATA(ID).NOPAR = Val(Mid(WSTRING, 15, 3))
      For I = 1 To CURRMODELDATA(ID).NOPAR
        Input #32, CURRMODELDATA(ID).PARAM(I)
      Next I
    End If
  Loop While Mid(WSTRING, 1, 4) <> "STOP"
  Close 32
  Exit Sub
  
ErrHandler:
  MsgBox "Eror reading model paprameter data", vbCritical, "ERROR"
  Close #32
  Exit Sub
End Sub

Private Sub ReadSystemData()
'***************************
  Open RTrim(JobPath) & "CURRDATA.SYD" For Input As 31
  Line Input #31, WSTRING
  If WSTRING <> "SYSTEM DATA" Then
    MsgBox "Cannot read system data in file " & RTrim(JobPath) & "CURRDATA.SYD"
    Exit Sub
  End If
  Line Input #31, WSTRING
  Line Input #31, WSTRING
  If Mid(WSTRING, 1, 4) = "NDGS" Then
    NDC = Val(Mid(WSTRING, 9, 4))
    NGC = Val(Mid(WSTRING, 13, 4))
    NSC = Val(Mid(WSTRING, 17, 4))
  Else
    MsgBox "Cannot read system data in file " & RTrim(JobPath) & "CURRDATA.SYD"
    Exit Sub
  End If
  Close 31
  Sysdata.NGCM.text = CStr(NGC)
End Sub

Public Sub ClearWorkspace()
'**************************
'Clears workspace to start a new job
On Error GoTo ErrHandler
JOBNAME = ""
Caption = "MODSIM - Modular Simulator for Ore Dressing Plants "
'Reset all repeat simulation data
Call ResetLevels
DeleteFile RTrim(JobPath) & "PLNTR.OUT"
DeleteFile RTrim(JobPath) & "FlyData.OUT"
DeleteFile RTrim(JobPath) & "MODSDATA.OUT"
DeleteFile RTrim(JobPath) & "SYSDATA.OUT"
DeleteFile RTrim(JobPath) & "REPORT.OUT"
DeleteFile RTrim(JobPath) & "STREAMPROPS.TXT"
DeleteFile RTrim(JobPath) & "S_CLASS.OUT"
DeleteFile RTrim(JobPath) & "SizeComp.OUT"
DeleteFile RTrim(JobPath) & "GC_DISTR.OUT"
DeleteFile RTrim(JobPath) & "LIBCOMP.OUT"
DeleteFile RTrim(JobPath) & "SC_DISTR.OUT"
DeleteFile RTrim(JobPath) & "PSDGRAPH.OUT"
DeleteFile RTrim(JobPath) & "WASHDATA.OUT"
DeleteFile RTrim(JobPath) & "FORMAT.OUT"
DeleteFile RTrim(JobPath) & "SIM.OUT"
DeleteFile RTrim(JobPath) & "PHO1.OUT"
DeleteFile RTrim(JobPath) & "CURRDATA.SYD"
DeleteFile RTrim(JobPath) & "CURRDATA.RUN"
DeleteFile RTrim(JobPath) & "Libdisp.DAT"
DeleteFile RTrim(JobPath) & "Libdispm.DAT"
DeleteFile RTrim(JobPath) & "Opgraph.DAT"
DeleteFile RTrim(JobPath) & "Opdisp.DAT"
DeleteFile RTrim(JobPath) & "Report.DAT"
DeleteFile RTrim(JobPath) & "Phi1.ech"
DeleteFile RTrim(JobPath) & "Phi3.dat"
DeleteFile RTrim(JobPath) & "Phi4.DAT"
DeleteFile RTrim(JobPath) & "Pho3.DAT"
DeleteFile RTrim(JobPath) & "Datt.DAT"
DeleteFile RTrim(JobPath) & "Simop.DAT"
DeleteFile RTrim(JobPath) & "AM_COEFF.DAT"
DeleteFile RTrim(JobPath) & "LJUBAMD.DAT"
DeleteFile RTrim(JobPath) & "BetaAMD.DAT"
DeleteFile RTrim(JobPath) & "PSFILE??"
DeleteFile RTrim(JobPath) & "EPSI????"
Open RTrim(JobPath) & "TEARS.OUT" For Output As #14
Print #14, "LAST 0 0 0 0"
Close #14
Exit Sub

ErrHandler:
  MsgBox "An error occured while clearing the workspace", 48, "WARNING"
  Resume Next
End Sub

Public Sub DeleteFile(FileName As String)
'****************************************
  'Deletes files but checks for existence.
  'Will accept wild cards.
  Dim FileExists As String
  FileExists = Dir(FileName)
  Do While FileExists <> ""
    Kill RTrim(JobPath) & FileExists
    FileExists = Dir
  Loop
End Sub

