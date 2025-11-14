VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.MDIForm MODSIM 
   AutoShowChildren=   0   'False
   BackColor       =   &H00FFFFFF&
   Caption         =   "MODSIM - Modular Simulator for Ore Dressing Plants  "
   ClientHeight    =   6930
   ClientLeft      =   375
   ClientTop       =   870
   ClientWidth     =   10710
   Icon            =   "MDIMod.frx":0000
   LinkTopic       =   "MDIForm1"
   Picture         =   "MDIMod.frx":030A
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   9480
      Top             =   720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Menu File 
      Caption         =   "File"
      Begin VB.Menu New 
         Caption         =   "Start a new job"
      End
      Begin VB.Menu ExistingJob 
         Caption         =   "Open an existing job"
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close this job"
      End
      Begin VB.Menu SaveAs 
         Caption         =   "Save this job as ..."
      End
      Begin VB.Menu SaveJob 
         Caption         =   "Save this job"
      End
      Begin VB.Menu MnuPackJob 
         Caption         =   "Pack this job"
      End
      Begin VB.Menu MnuUnpackJob 
         Caption         =   "Unpack a job"
      End
      Begin VB.Menu mnuInternet 
         Caption         =   "Load a job from the Internet"
      End
      Begin VB.Menu MnuSeparator 
         Caption         =   "-"
      End
      Begin VB.Menu MnuOptions 
         Caption         =   "Options"
      End
      Begin VB.Menu MnuFileseparator 
         Caption         =   "-"
      End
      Begin VB.Menu Exit 
         Caption         =   "Exit"
      End
   End
   Begin VB.Menu Edit 
      Caption         =   "Edit"
      Begin VB.Menu EditFlowsheet 
         Caption         =   "Edit flowsheet"
      End
      Begin VB.Menu EditSysdata 
         Caption         =   "Edit system data"
      End
      Begin VB.Menu ModelParameters 
         Caption         =   "Edit unit model parameters"
      End
      Begin VB.Menu MnuEditSeparator 
         Caption         =   "-"
      End
      Begin VB.Menu MnuEditRepeatSim 
         Caption         =   "Edit repetitive simulation data"
         Visible         =   0   'False
      End
      Begin VB.Menu OutputFormat 
         Caption         =   "Edit output format"
      End
      Begin VB.Menu MnuChangeJobName 
         Caption         =   "Change job name"
      End
   End
   Begin VB.Menu View 
      Caption         =   "View"
      Begin VB.Menu ViewFlowsheet 
         Caption         =   "Flowsheet"
      End
      Begin VB.Menu MnuSimulationResults 
         Caption         =   "Simulation results"
      End
      Begin VB.Menu ViewOutputFile 
         Caption         =   "Data output file"
      End
      Begin VB.Menu MnuAccumulated 
         Caption         =   "Accumulated output data"
         Visible         =   0   'False
      End
      Begin VB.Menu ReportFile 
         Caption         =   "Report file"
      End
      Begin VB.Menu MnuStreamProps 
         Caption         =   "Stream properties"
      End
      Begin VB.Menu MnuViewSeparator 
         Caption         =   "-"
      End
      Begin VB.Menu PSDGraphs 
         Caption         =   "Size distribution graphs"
      End
      Begin VB.Menu MnuLiberationGraphs 
         Caption         =   "Liberation distribution graphs"
      End
   End
   Begin VB.Menu Run 
      Caption         =   "Run"
      Begin VB.Menu Simulate 
         Caption         =   "Run simulation"
      End
      Begin VB.Menu MnuRepeat 
         Caption         =   "Run repeat simulations"
         Visible         =   0   'False
      End
      Begin VB.Menu MnuRunSeparator 
         Caption         =   "-"
      End
      Begin VB.Menu ViewDiagDLL 
         Caption         =   "View DLL diagnostic file"
      End
      Begin VB.Menu mnuShowPath 
         Caption         =   "Show local path for temporary files"
      End
      Begin VB.Menu DataEcho 
         Caption         =   "View data echo file"
      End
      Begin VB.Menu ViewCalcOrder 
         Caption         =   "Review calculation order"
      End
      Begin VB.Menu ViewTearStreams 
         Caption         =   "View tear streams"
      End
      Begin VB.Menu mnuViewUserDiag 
         Caption         =   "View user model diagnostic file"
         Visible         =   0   'False
      End
   End
   Begin VB.Menu mnuHelp 
      Caption         =   "Help"
      Begin VB.Menu mnuShowHelp 
         Caption         =   "Help"
      End
      Begin VB.Menu mnuHelpSeparator 
         Caption         =   "-"
      End
      Begin VB.Menu MnuAbout 
         Caption         =   "About"
      End
   End
End
Attribute VB_Name = "MODSIM"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private CloseJob As Boolean
Public JOBNAME As String
Public ViewFlowsheetFlag As Boolean
Public EditFlowsheetFlag As Boolean
Public SizgraphIsActive As Boolean
Public LibGraphIsActive As Boolean
Private resp As Integer, SimopResp As Integer
Private MsgResponse As Integer

Private Sub DataEcho_Click()
'***************************
  EchoFileName = "PHI1.ECH"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub Edit_Click()
'***********************
End Sub

Private Sub EditFlowsheet_Click()
'********************************
'Redraw the flowsheet using the data stored in file PLNTR
  EditFlowsheetFlag = True
  ViewFlowsheetFlag = False
  Call MGPage(0).ResetFlags
  Call StartFlowsheet
End Sub

Private Sub EditSysdata_Click()
'******************************
  FastFeedStreamChange = False
  FastWaterStreamChange = False
  FastOtherStreamChange = False
  Call StartSysdata
End Sub

Private Sub ExistingJob_Click()
'******************************
'Load an existing job
  Dim I As Integer, K As Integer
  Dim JobFlag As Boolean
  On Error GoTo ErrHandler1
  ' Set filters
  CommonDialog1.CancelError = True
  CommonDialog1.Filter = "Job files (*.job)|*.job|Packed jobs(*.PAK)|*.PAK|All files|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.InitDir = Trim(JobSavePath)
  CommonDialog1.ShowOpen
  WSTRING = Trim(CommonDialog1.FileName)
  
  On Error GoTo ErrHandler
  Call ClearWorkspace
  
  I = InStr(WSTRING, ".")
  'Is this a packed job?
  If Mid(WSTRING, I + 1, 3) = "PAK" Or Mid(WSTRING, I + 1, 3) = "pak" Then
    Call UnpackJobFile(WSTRING)
    Exit Sub
  End If
  
  'Strip off any file type extension
  If I > 0 Then
    JobFileName = Mid(WSTRING, 1, I - 1)
  Else
    JobFileName = WSTRING
  End If
  JOBNAME = JobFileName
  'Strip off the path
  I = 1
  K = 0
  Do While I > 0
    I = InStr(JOBNAME, "\")
    K = K + I
    JOBNAME = Mid(JOBNAME, I + 1)
  Loop
  Me.MousePointer = 11
  Open JobFileName & ".JOB" For Input As #31
  Input #31, WSTRING
  If WSTRING = "Job name" Then
    Input #31, WSTRING
    JOBNAME = Trim(WSTRING)
    JobSavePath = Mid(JobFileName, 1, K)
    Caption = TITLE & Trim(JobSavePath) & JOBNAME
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
        FileCopy JobFileName & ".TRN", RTrim(JobPath) & "PLNTR.OUT"
        MGPage(0).NewFlowsheet = False
        'Set the saved flag to true because a freshly loaded job does not require saving
        SavedJob = True
      End If

    Case "System data"
      JobSystemData = JobFlag
      If JobSystemData Then
        FileCopy JobFileName & ".syd", RTrim(JobPath) & "SYSDATA.OUT"
      End If
  
    Case "Model data"
      JobModelData = JobFlag
      If JobModelData Then
        FileCopy JobFileName & ".mod", RTrim(JobPath) & "MODSDATA.OUT"
      End If
  
    Case "System input data"
      JobSystemInputData = JobFlag
      If JobSystemInputData Then
        FileCopy JobFileName & ".sid", RTrim(JobPath) & "CURRDATA.SYD"
      End If

    Case "Size data"
      JobSizeData = JobFlag
      If JobSizeData Then
        FileCopy JobFileName & ".siz", RTrim(JobPath) & "SIZECOMP.OUT"
      End If

    Case "G-class distribution"
      JobGCDistribution = JobFlag
      If JobGCDistribution Then
        FileCopy JobFileName & ".gcd", RTrim(JobPath) & "GC_DISTR.OUT"
      End If

    Case "Liberation data"
      JobLibcomp = JobFlag
      If JobLibcomp Then
        FileCopy JobFileName & ".lib", RTrim(JobPath) & "LIBCOMP.OUT"
      End If

    Case "Ljubljana model"
      JobLjubAMD = JobFlag
      If JobLjubAMD Then
        FileCopy JobFileName & ".lju", RTrim(JobPath) & "LJUBAMD.DAT"
      End If

    Case "Beta function liberation model"
      JobBetaAMD = JobFlag
      If JobBetaAMD Then
        FileCopy JobFileName & ".amd", RTrim(JobPath) & "BetaAMD.DAT"
      End If

    Case "S-class distribution"
      JobSCDistribution = JobFlag
      If JobSCDistribution Then
        FileCopy JobFileName & ".scd", RTrim(JobPath) & "SC_DISTR.OUT"
      End If

    Case "Washability data"
     JobWashabilityData = JobFlag
     If JobWashabilityData Then
        FileCopy JobFileName & ".was", RTrim(JobPath) & "WASHDATA.OUT"
      End If
      
    Case "System data needed"
      JobSystemDataNeeded = JobFlag

    Case "Model parameter data"
      JobModelParameterData = JobFlag
      If JobModelParameterData Then
        FileCopy JobFileName & ".cur", RTrim(JobPath) & "CURRDATA.RUN"
      End If
      
    Case "Unit parameters needed"
      JobUnitParametersNeeded = JobFlag

    Case "Output format"
      JobOutputFormat = JobFlag
      If JobOutputFormat Then
        FileCopy JobFileName & ".mat", RTrim(JobPath) & "FORMAT.OUT"
      End If
 
    Case "SIMOP data"
      JobSimopData = JobFlag
      If JobSimopData Then
        FileCopy JobFileName & ".mop", RTrim(JobPath) & "SIMOP.DAT"
      End If
   
    Case Else
      GoTo ErrHandler
      
    End Select
  Loop
  Close #31
  
  'Show the flowsheet at job startup.
  If OptionShowFlowsheet Then
    Call ViewFlowsheet_Click
  End If
  
  On Error GoTo ErrHandler2
  FileCopy JobFileName & ".TEA", RTrim(JobPath) & "TEARS.OUT"
  
  Me.MousePointer = 1
  Exit Sub
  
ErrHandler:
  MsgBox "Job was not loaded properly", 16, "WARNING"
  Close #31
  Me.MousePointer = 1
  Exit Sub
  
ErrHandler1:
  'User pressed Cancel button
  Me.MousePointer = 1
  Exit Sub
  
ErrHandler2:
  'Tear stream data is not available
  Open "TEARS.OUT" For Output As #41
  Print #41, "LAST 0 0 0 0"
  Close #41
  Me.MousePointer = 1
  Exit Sub
  
End Sub

Private Sub Exit_Click()
'***********************
  Dim NL As String * 2
  Dim response As Integer
  
  'Block any file saving if this is a Demo version
  If DemoVersion Then
     If CloseJob Then
       Call ClearWorkspace
       Exit Sub
     Else
       Call WriteInitializationFile
       End
     End If
  End If

  'Has this job been saved?
  If Not SavedJob Then
    NL = Chr(13) & Chr(10)
    WSTRING = "The following data components were touched since this job was last saved:"
    If ChangedFlowsheet Then
      WSTRING = WSTRING & NL & "  The flowsheet"
    End If
    If ChangedSystemInputData Then
      WSTRING = WSTRING & NL & "  The system data"
    End If
    If ChangedModelParameterData Then
      WSTRING = WSTRING & NL & "  The model parameters"
    End If
    If ChangedOutputFormat Then
      WSTRING = WSTRING & NL & "  The output format"
    End If
      WSTRING = WSTRING & NL & "Do you want to save any of these data components?"
    
    If WSTRING <> "The following data components were touched since this job was last saved:" & NL & "Do you want to save any of these data components?" Then
      response = MsgBox(WSTRING, 35, "MODSIM")
      If response = vbYes Then
        response = MsgBox("Save the entire job?", 36, "MODSIM")
        If response = vbYes Then
          If JobSavePath = "" Then
            Call SaveAs_Click
          Else
            Call SaveJob_Click
          End If
          If CloseJob Then
            Call ClearWorkspace
            Exit Sub
          Else
            Call WriteInitializationFile
            End
          End If
        End If
      ElseIf response = vbCancel Then
        Exit Sub
      Else
        'Exit from MODSIM or close job
        If CloseJob Then
          Call ClearWorkspace
          Exit Sub
        Else
          Call WriteInitializationFile
          End
        End If
      End If
    Else
      'Exit from MODSIM or close job
      If CloseJob Then
        Call ClearWorkspace
        Exit Sub
      Else
        Call WriteInitializationFile
        End
      End If
    End If
  
    'Now check for any changed files to save
    JobFileName = RTrim(JobSavePath) & JOBNAME
  
    If ChangedFlowsheet Then
      response = MsgBox("Save changes to flowsheet?", 4, "WARNING: The flowsheet was edited")
      If response = vbYes Then
        FileCopy RTrim(JobPath) & "PLNTR.OUT", JobFileName & ".TRN"
        FileCopy RTrim(JobPath) & "SysData.out", JobFileName & ".syd"
        FileCopy RTrim(JobPath) & "ModsData.out", JobFileName & ".mod"
      End If
    End If
    If ChangedSystemInputData Then
      response = MsgBox("Save changes to system data?", 4, "WARNING: The system data has been edited")
      If response = vbYes Then
        FileCopy RTrim(JobPath) & "CURRDATA.SYD", JobFileName & ".sid"
        FileCopy RTrim(JobPath) & "SIZECOMP.OUT", JobFileName & ".siz"
        FileCopy RTrim(JobPath) & "GC_Distr.out", JobFileName & ".gcd"
        FileCopy RTrim(JobPath) & "LIBCOMP.OUT", JobFileName & ".lib"
        FileCopy RTrim(JobPath) & "SC_DISTR.OUT", JobFileName & ".scd"
        If JobWashabilityData Then
          FileCopy RTrim(JobPath) & "Washdata.out", JobFileName & ".was"
        End If
      End If
    End If
    If ChangedModelParameterData Then
      response = MsgBox("Save changes to model parameters?", 4, "WARNING: Model parameters have been edited")
      If response = vbYes Then
        FileCopy RTrim(JobPath) & "CurrData.Run", JobFileName & ".cur"
      End If
    End If
    If ChangedOutputFormat Then
      response = MsgBox("Save changes to the output format?", 4, "WARNING: The output format has been edited")
      If response = vbYes Then
        FileCopy RTrim(JobPath) & "FORMAT.OUT", JobFileName & ".mat"
      End If
    End If
  End If
  
  'Exit from MODSIM or close job
  If CloseJob Then
    Call ClearWorkspace
    Exit Sub
  Else
    Call WriteInitializationFile
    End
  End If
End Sub

Private Sub MDIForm_Activate()
  'Diagnostic.lstDiagnostic.AddItem "Activating MDI form MODSIM"
  PSFlagOn = False 'Required for drawing flowsheets
End Sub

Private Sub MDIForm_Load()
'*************************
  If BHVersion Then
     mnuInternet.Enabled = False
     mnuInternet.Visible = False
  End If
  If DemoVersion Then
     SaveJob.Enabled = False
     SaveAs.Enabled = False
     MnuPackJob.Enabled = False
     MnuUnpackJob.Enabled = False
     Caption = "MODSIM - Modular Simulator for Ore Dressing Plants.  Demonstration version"
  End If
  If ProfessionalVersion Then
     MnuRepeat.Visible = True
     MnuAccumulated.Visible = True
     MnuEditRepeatSim.Visible = True
     mnuViewUserDiag.Visible = True
     Caption = "MODSIM - Modular Simulator for Ore Dressing Plants.  Professional version"
  End If
  If AcademicVersion Then
     MnuRepeat.Visible = True
     MnuAccumulated.Visible = True
     MnuEditRepeatSim.Visible = True
     mnuViewUserDiag.Visible = True
     Caption = "MODSIM - Modular Simulator for Ore Dressing Plants.  Academic version"
  End If
  Call ResetMDIFlags
End Sub

Private Sub ResetMDIFlags()
'**************************
  CommonDialog1.CancelError = True
  JobFileName = ""
  SavedJob = False
  CloseJob = False
  
  MGPage(0).NewFlowsheet = True
  JobFlowsheet = False
  JobSystemData = False
  JobModelData = False
  JobSystemInputData = False
  JobCurrentData = False
  JobSizeData = False
  JobGCDistribution = False
  JobSCDistribution = False
  JobLibcomp = False
  JobWashabilityData = False
  JobModelParameterData = False
  JobOutputFormat = False
  JobSimopData = False
  JobLjubAMD = False
  JobBetaAMD = False
  
  ChangedJobname = False
  ChangedFlowsheet = False
  ChangedSystemInputData = False
  ChangedModelParameterData = False
  ChangedOutputFormat = False
  ChangedA_MDiagramParameters = False
  SimulationHasBeenRun = False
  SizgraphIsActive = False
  LibGraphIsActive = False
  Definedflowsheet = False
End Sub


Private Sub MDIForm_Unload(Cancel As Integer)
Call Exit_Click
End Sub

Private Sub mnuAbout_Click()
'***************************
  'About.Caption = "About MODSIM"
  'About.lblTitle = "MODSIM"
  'About.lblVersion = "Version " & App.Major & "." & App.Minor & "." & App.Revision
  If ProfessionalVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Professional version"
  ElseIf DemoVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Demonstration version"
  ElseIf StudentVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Student version"
  ElseIf AcademicVersion Then
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants. Academic version"
    
  Else
    About.lblDescription = "MODSIM - Modular Simulator for Ore Dressing Plants."
  End If
  About.lblDisclaimer = "Copyright 1978 - 2004 R. P. King and M. A. Ford"
  About.Show
End Sub

Private Sub MnuAccumulated_Click()
'*********************************
  EchoFileName = "CUMDATA.OUT"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub MnuChangeJobName_Click()
'***********************************
Me.JOBNAME = InputBox("Specify the job name", , JOBNAME)
MODSIM.Caption = "MODSIM - Modular Simulator for Ore Dressing Plants    " & JOBNAME
'Update the jobname in the system data
ChangedJobname = True
End Sub

Private Sub MnuClose_Click()
'***************************
'Close the active job
CloseJob = True
Unload Sysdata
Call Exit_Click
Close
CloseJob = False
End Sub

Private Sub MnuEditRepeatSim_Click()
'***********************************
RepPanel.Top = 0
RepPanel.Left = 0
RepPanel.Show
End Sub

Private Sub mnuInternet_Click()
'******************************
FTP.Show
End Sub

Private Sub MnuLiberationGraphs_Click()
'**************************************
  libgraph.Top = 0
  libgraph.Left = 0
  libgraph.Show
  End Sub

Private Sub MnuOptions_Click()
'*****************************
' Show the options dialog
   Options.Show 1
End Sub

Private Sub MnuRepeat_Click()
'****************************
'Drives the repetive simulation
  Dim I As Integer, response As Long
  Dim CumOut As Integer
  Dim FlagCode As String * 5
  Dim Value1 As Single, Value2 As Single, Value3 As Single, Value4 As Single, Value5 As Single
  FlagCode = "FFFFF"
  For I = 1 To 5
    If LevelFlag(I) Then Mid(FlagCode, I, 1) = "T"
  Next I
  'Check if format for output has been specified
  If OutFormat.ChkAccumulate = False Then
    response = MsgBox("Output format has not been specified for repetitive output" & vbCrLf & "Proceed anyway?", vbOKCancel, "WARNING")
    If response = vbCancel Then
      Exit Sub
    End If
  End If
  Open RTrim(JobPath) & "Repeat.out" For Output As #37
  Print #37, FlagCode
  For I = 1 To 5
    If LevelFlag(I) Then
      Print #37, I, RepUnitNumber(I)
      Print #37, ParameterName(I)
    Else
      StepValue(I) = 1
    End If
  Next I
  CumOut = 0
  Load UnitMods
  FrmShowProgress.Top = 0
  FrmShowProgress.Left = 0
  FrmShowProgress.Show
  For I = 1 To 5
    If LevelFlag(I) Then
      FrmShowProgress.LblUnitNumber(I).Caption = CStr(RepUnitNumber(I))
      FrmShowProgress.LblParameterName(I).Caption = ParameterName(I)
    Else
      FrmShowProgress.LblUnitNumber(I).Caption = ""
      FrmShowProgress.LblParameterName(I).Caption = ""
      FrmShowProgress.LblParamValue(I).Caption = ""
    End If
  Next I
  WSTRING = ""
  For Value5 = StartValue(5) To EndValue(5) Step StepValue(5)
    If LevelFlag(5) Then
      CURRMODELDATA(UnitID(5)).PARAM(ParameterNumber(5)) = Value5
      FrmShowProgress.LblParamValue(5).Caption = CStr(Value5)
    End If
    For Value4 = StartValue(4) To EndValue(4) Step StepValue(4)
      If LevelFlag(4) Then
        CURRMODELDATA(UnitID(4)).PARAM(ParameterNumber(4)) = Value4
        FrmShowProgress.LblParamValue(4).Caption = CStr(Value4)
      End If
      For Value3 = StartValue(3) To EndValue(3) Step StepValue(3)
        If LevelFlag(3) Then
          CURRMODELDATA(UnitID(3)).PARAM(ParameterNumber(3)) = Value3
          FrmShowProgress.LblParamValue(3).Caption = CStr(Value3)
        End If
        For Value2 = StartValue(2) To EndValue(2) Step StepValue(2)
          If LevelFlag(2) Then
            CURRMODELDATA(UnitID(2)).PARAM(ParameterNumber(2)) = Value2
            FrmShowProgress.LblParamValue(2).Caption = CStr(Value2)
        End If
          For Value1 = StartValue(1) To EndValue(1) Step StepValue(1)
            If LevelFlag(1) Then
              CURRMODELDATA(UnitID(1)).PARAM(ParameterNumber(1)) = Value1
              FrmShowProgress.LblParamValue(1).Caption = CStr(Value1)
              WSTRING = WSTRING & "  " & CStr(Value1)
            End If
            If LevelFlag(2) Then WSTRING = WSTRING & " " & CStr(Value2)
            If LevelFlag(3) Then WSTRING = WSTRING & " " & CStr(Value3)
            If LevelFlag(4) Then WSTRING = WSTRING & " " & CStr(Value4)
            If LevelFlag(5) Then WSTRING = WSTRING & " " & CStr(Value5)
            CumOut = CumOut + 1
            FrmShowProgress.LblProgress.Caption = "Simulation number " & CStr(CumOut)
            FrmShowProgress.Refresh
            Call UnitMods.cmdQuit_Click
            Print #37, RTrim(WSTRING)
            WSTRING = ""
            Close (37)
            Call RunSimulation(CumOut)
            If MsgResponse = vbNo Then GoTo Abort:
            Open RTrim(JobPath) & "Repeat.out" For Output As #37
          Next Value1
        Next Value2
      Next Value3
    Next Value4
  Next Value5
  Close (37)
  FrmShowProgress.LblComplete.Caption = "Simulations have been completed"
  Unload UnitMods
  Exit Sub

Abort:
  Unload UnitMods
  Exit Sub
End Sub

Private Sub mnuShowHelp_Click()
'******************************
  Dim RetVal As Long
  RetVal = Shell("HH " & RTrim(FileReadPath) & "MODSIM.chm", vbNormalFocus)
End Sub

Private Sub mnuShowPath_Click()
'******************************
MsgBox "MODSIM local path is " & RTrim(JobPath), vbInformation, "INFORMATION"
End Sub

Private Sub MnuSimulationResults_Click()
'***************************************
Dim WSTRING1 As String, WSTRING2 As String, WSTRING3 As String
Dim IRow As Integer, Icol As Integer, NColumns As Integer, I As Integer
On Error GoTo ErrHandler

Open RTrim(JobPath) & "SIM.OUT" For Input As 39
OutData.Grid.Clear
Input #39, WSTRING1
OutData.Caption = "Simulation results for job " & WSTRING1
Line Input #39, WSTRING1
Line Input #39, WSTRING2
Line Input #39, WSTRING3
NColumns = Len(WSTRING2) \ 7 - 1
OutData.Grid.Cols = NColumns + 1
IRow = 0
OutData.Grid.Row = 0
OutData.Grid.RowHeight(0) = 500
OutData.Grid.Col = 0
OutData.Grid.ColWidth(0) = 600
OutData.Grid.text = Mid(WSTRING1, 1, 6)
If NColumns > 0 Then
  OutData.Grid.Col = 1
  OutData.Grid.ColWidth(1) = 900
  OutData.Grid.text = Trim(Mid(WSTRING1, 8, 13)) & " " & Trim(Mid(WSTRING2, 8, 13)) & " " & Mid(WSTRING3, 8, 13)
End If
If NColumns > 1 Then
  OutData.Grid.Col = 2
  OutData.Grid.ColWidth(2) = 900
  OutData.Grid.text = Trim(Mid(WSTRING1, 22, 8)) & " " & Trim(Mid(WSTRING2, 22, 7)) & " " & Mid(WSTRING3, 22, 8)
End If
For I = 3 To NColumns
  OutData.Grid.Col = I
  OutData.Grid.ColWidth(I) = 800
  WSTRING = Trim(Mid(WSTRING1, 7 * (I - 3) + 29, 7))
  If WSTRING = "Pr" Then WSTRING = "     ------Pr"
  If WSTRING = "is" Then WSTRING = "is------     "
  OutData.Grid.text = WSTRING & " " & Trim(Mid(WSTRING2, 7 * (I - 3) + 29, 7)) & " " & Trim(Mid(WSTRING3, 7 * (I - 3) + 29, 7))
Next I
Do
  IRow = IRow + 1
  Line Input #39, WSTRING1
  If Mid(WSTRING1, 2, 8) = "PARTICLE" Then
    OutData.Grid.AddItem ""
    OutData.Grid.Row = IRow
    OutData.Grid.Col = 1
    OutData.Grid.text = "Stream"
    OutData.Grid.Col = 2
    OutData.Grid.text = Mid(WSTRING1, 39, 3)
    NColumns = 2
  ElseIf Mid(WSTRING1, 8, 4) = "Size" Then
    OutData.Grid.AddItem ""
    OutData.Grid.Row = IRow
    OutData.Grid.Col = 1
    OutData.Grid.text = "Size"
    OutData.Grid.Col = 2
    OutData.Grid.text = "% passing"
    
  ElseIf Mid(WSTRING1, 2, 10) = "LIBERATION" Then Exit Do
  Else
    OutData.Grid.AddItem ""
    OutData.Grid.Row = IRow
    OutData.Grid.Col = 0
    OutData.Grid.text = Mid(WSTRING1, 1, 6)
    If NColumns > 0 Then
      OutData.Grid.Col = 1
      OutData.Grid.text = Trim(Mid(WSTRING1, 8, 12))
    End If
    If NColumns > 1 Then
      OutData.Grid.Col = 2
      OutData.Grid.text = Trim(Mid(WSTRING1, 21, 7))
    End If
    For I = 3 To NColumns
      OutData.Grid.Col = I
      OutData.Grid.text = Trim(Mid(WSTRING1, 7 * (I - 3) + 29, 7))
    Next I
  End If
Loop Until EOF(39)
Close #39
OutData.Top = 0
OutData.Left = 0
OutData.Show
Exit Sub

ErrHandler:
  MsgBox "Error setting up data grid", vbCritical, "ERROR"
  Close #39
  Exit Sub
End Sub

Private Sub MnuStreamProps_Click()
  EchoFileName = "STREAMPROPS.TXT"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub MnuUnpackJob_Click()
'*******************************
' Unpack an existing packed job into the workspace.
  On Error GoTo ErrHandler1
  ' Set filters
  CommonDialog1.CancelError = True
  CommonDialog1.Filter = "Packed job files (*.PAK)|*.PAK"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.InitDir = Trim(JobSavePath)
  CommonDialog1.ShowOpen
  Call ClearWorkspace
  JobFileName = Trim(CommonDialog1.FileName)
  Call UnpackJobFile(JobFileName)
  If OptionShowFlowsheet Then
    Call ViewFlowsheet_Click
  End If
  Exit Sub
  
ErrHandler1:
  'User pressed Cancel button
  Exit Sub
End Sub
  
Public Sub UnpackJobFile(FileName As String)
'********************************************
  Dim JobFlag As Boolean
  Dim I As Integer, K As Integer
  On Error GoTo ErrHandler
  Open FileName For Input As #31

  Me.MousePointer = 11
  'Strip off any file type extension
  I = InStr(FileName, ".")
  If I > 0 Then
    FileName = Mid(FileName, 1, I - 1)
  End If

  ' Find the job save path for future saves or packs
  WSTRING = FileName
  I = 1
  K = 0
  Do While I > 0
    I = InStr(WSTRING, "\")
    K = K + I
    WSTRING = Mid(WSTRING, I + 1)
  Loop
  JobSavePath = Mid(FileName, 1, K)
  Input #31, WSTRING
  If WSTRING = "Job name" Then
    Input #31, WSTRING
    JOBNAME = Trim(WSTRING)
    Caption = TITLE & Trim(FileName) & ".PAK"
  Else
    MsgBox "Incorrect file type", 16, "ERROR"
    Close #31
    Me.MousePointer = 1
    Exit Sub
  End If
    
  Do While Not EOF(31)
    Line Input #31, WSTRING
    'Flowsheet data
    If Mid(WSTRING, 2, 9) = "Flowsheet" Then
      If Mid(WSTRING, 14, 4) = "TRUE" Then
        JobFlowsheet = True
        Open RTrim(JobPath) & "PLNTR.OUT" For Output As #32
        MGPage(0).NewFlowsheet = False
        'Set saved flag to true because a freshly unpacked job does not require saving.
        SavedJob = True
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 11) <> "System data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'System data from the flowsheet
    If Mid(WSTRING, 2, 11) = "System data" Then
      If Mid(WSTRING, 16, 4) = "TRUE" Then
        JobSystemData = True
        Open RTrim(JobPath) & "SysData.OUT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 10) <> "Model data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Model data from the flowsheet
    If Mid(WSTRING, 2, 10) = "Model data" Then
      If Mid(WSTRING, 15, 4) = "TRUE" Then
        JobModelData = True
        Open RTrim(JobPath) & "ModsData.OUT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 17) <> "System input data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'System input data for SYSDATA module
    If Mid(WSTRING, 2, 17) = "System input data" Then
      If Mid(WSTRING, 22, 4) = "TRUE" Then
        JobSystemInputData = True
        Open RTrim(JobPath) & "CURRDATA.SYD" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 9) <> "Size data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Particle size data from the SYSDATA module
    If Mid(WSTRING, 2, 9) = "Size data" Then
      If Mid(WSTRING, 14, 4) = "TRUE" Then
        JobSizeData = True
        Open RTrim(JobPath) & "SIZECOMP.OUT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 20) <> "G-class distribution" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If
    
    'G-class distribution
    If Mid(WSTRING, 2, 20) = "G-class distribution" Then
      If Mid(WSTRING, 25, 4) = "TRUE" Then
        JobGCDistribution = True
        Open RTrim(JobPath) & "GC_Distr.out" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 15) <> "Liberation data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Liberation data
    If Mid(WSTRING, 2, 15) = "Liberation data" Then
      If Mid(WSTRING, 20, 4) = "TRUE" Then
        JobLibcomp = True
        Open RTrim(JobPath) & "LIBCOMP.OUT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 15) <> "Ljubljana model" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Andrews-Mika daigrams: Ljubljana model
    If Mid(WSTRING, 2, 15) = "Ljubljana model" Then
      If Mid(WSTRING, 20, 4) = "TRUE" Then
        JobLjubAMD = True
        Open RTrim(JobPath) & "LJUBAMD.DAT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 30) <> "Beta function liberation model" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Andrews-Mika diagrams:Beta function model
    If Mid(WSTRING, 2, 30) = "Beta function liberation model" Then
      If Mid(WSTRING, 35, 4) = "TRUE" Then
        JobBetaAMD = True
        Open RTrim(JobPath) & "BetaAMD.DAT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 20) <> "S-class distribution" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'S-class specifications

    'S-class distributions
    If Mid(WSTRING, 2, 20) = "S-class distribution" Then
      If Mid(WSTRING, 25, 4) = "TRUE" Then
        JobSCDistribution = True
        Open RTrim(JobPath) & "SC_DISTR.OUT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 16) <> "Washability data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Washability data
    If Mid(WSTRING, 2, 16) = "Washability data" Then
      If Mid(WSTRING, 21, 4) = "TRUE" Then
        JobWashabilityData = True
        Open RTrim(JobPath) & "Washdata.out" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 20) <> "Model parameter data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Model parameter data
    If Mid(WSTRING, 2, 20) = "Model parameter data" Then
      If Mid(WSTRING, 25, 4) = "TRUE" Then
        JobModelParameterData = True
        Open RTrim(JobPath) & "CurrData.RUN" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 13) <> "Output format" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If

    'Output format
    If Mid(WSTRING, 2, 13) = "Output format" Then
      If Mid(WSTRING, 18, 4) = "TRUE" Then
        JobOutputFormat = True
        Open RTrim(JobPath) & "FORMAT.OUT" For Output As #32
        Line Input #31, WSTRING
        Do While Mid(WSTRING, 2, 10) <> "SIMOP data" And Not EOF(31)
          Print #32, WSTRING
          Line Input #31, WSTRING
        Loop
        Close #32
      Else
        Line Input #31, WSTRING
      End If
    End If
 
    'SIMOP data to go directly to SIMOP.FOR
    If Mid(WSTRING, 2, 10) = "SIMOP data" Then
      If Mid(WSTRING, 15, 4) = "TRUE" Then
        JobSimopData = True
        Open RTrim(JobPath) & "SIMOP.DAT" For Output As #32
        Line Input #31, WSTRING
        Print #32, WSTRING
        Do While Not EOF(31)
          Line Input #31, WSTRING
          Print #32, WSTRING
        Loop
        Close #32
      End If
    End If

  Loop

  Me.MousePointer = 1
  Close #31
  Exit Sub
  
ErrHandler:
  MsgBox "Job was not completely unpacked", 16, "WARNING"
  Close #31
  Close #32
  Me.MousePointer = 1
  Exit Sub
  
End Sub

Private Sub ModelParameters_Click()
'**********************************
  If ChangedFlowsheet Then
    Call ResetLevels
    MsgBox "The flowsheet has changed" & vbCrLf & "Edit the system data before proceeding", vbExclamation, "WARNING"
    Exit Sub
  End If
  Call StartUnitMods
  FastParameterChange = False
  UnitMods.Top = DisplayOffsetY
  UnitMods.Left = DisplayOffsetX
  UnitMods.Show 1
End Sub

Private Sub New_Click()
'**********************
  Call ClearWorkspace
  JobUnitParametersNeeded = True
  JobSystemDataNeeded = True
  EditFlowsheetFlag = True
  ViewFlowsheetFlag = False
  MGPage(0).NewFlowsheet = True
  Call StartFlowsheet
  SavedJob = True
End Sub

Private Sub OutputFormat_Click()
'*******************************
  OutFormat.Top = DisplayOffsetY
  OutFormat.Left = DisplayOffsetX
  OutFormat.Show 1
End Sub

Private Sub PSDGraphs_Click()
'****************************
  Sizgraph.Top = 0
  Sizgraph.Left = 0
  Sizgraph.Show
End Sub

Private Sub ReportFile_Click()
'*****************************
  EchoFileName = "REPORT.OUT"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub SaveAs_Click()
'************************
' Save job files
  Dim I As Integer, K As Integer
  Dim ReturnFile As String, resp As Integer
  
  On Error GoTo ErrHandler1
  If JOBNAME = "" Then
    JOBNAME = InputBox("Specify a job name")
    If JOBNAME = "" Then Exit Sub
  End If
  ' Set filters
  CommonDialog1.Filter = "Job files (*.job)|*.job"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.FileName = Trim(JOBNAME) & ".JOB"
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.InitDir = Trim(JobSavePath)
  CommonDialog1.ShowSave
  
  On Error GoTo ErrHandler
  'Strip off any file type extension
  WSTRING = Trim(CommonDialog1.FileName)
  I = InStr(WSTRING, ".")
  If I > 0 Then
    JobFileName = Mid(WSTRING, 1, I - 1)
  Else
    JobFileName = WSTRING
  End If
  ReturnFile = Dir(JobFileName & ".job")
  If ReturnFile <> "" Then
    resp = MsgBox("A job with name " & JobFileName & ".JOB already exists.  Overwrite?", 33, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If

  'Separate the JobSavePath from the JOBNAME
  WSTRING = JobFileName
  I = 1
  K = 0
  Do While I > 0
    I = InStr(WSTRING, "\")
    K = K + I
    WSTRING = Mid(WSTRING, I + 1)
  Loop
  JobSavePath = Mid(JobFileName, 1, K)
  
  Caption = TITLE & Trim(JobSavePath) & WSTRING
  JOBNAME = WSTRING
  Me.MousePointer = 11
  Call SaveJobFiles
  MousePointer = 0
  Exit Sub

ErrHandler:
  MsgBox "Error occurred while saving job", 16, "WARNING"
  Exit Sub
  
ErrHandler1:
  'User pressed Cancel button
  Exit Sub
End Sub

Private Sub SaveJob_Click()
'**************************
Me.MousePointer = 11
If JOBNAME = "" Then
  Call SaveAs_Click
Else
  Call SaveJobFiles
End If
MousePointer = 0
End Sub
Private Sub MnuPackJob_Click()
'*****************************
' Pack job
  Dim I As Integer, K As Integer
  Dim ReturnFile As String, resp As Integer
  
  On Error GoTo ErrHandler1
  ' Set filters
  CommonDialog1.Filter = "Packed job files (*.PAK)|*.PAK"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.FileName = Trim(JOBNAME) & ".PAK"
  CommonDialog1.InitDir = Trim(JobSavePath)
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.ShowSave
  
  On Error GoTo ErrHandler
'Strip off any file type extension
  WSTRING = Trim(CommonDialog1.FileName)
  I = InStr(WSTRING, ".")
  If I > 0 Then
    JobFileName = Mid(WSTRING, 1, I - 1)
  Else
    JobFileName = WSTRING
  End If
  ReturnFile = Dir(JobFileName & ".PAK")
  If ReturnFile <> "" Then
    resp = MsgBox("A packed job with name " & JobFileName & ".PAK already exists.  Overwrite?", 33, "WARNING")
    If resp = vbCancel Then Exit Sub
  End If
  
  'Separate the JobSavePath from the JOBNAME
  WSTRING = JobFileName
  I = 1
  K = 0
  Do While I > 0
    I = InStr(WSTRING, "\")
    K = K + I
    WSTRING = Mid(WSTRING, I + 1)
  Loop
  JobSavePath = Mid(JobFileName, 1, K)
  Caption = TITLE & Trim(JobSavePath) & WSTRING
  JOBNAME = WSTRING
  
  If JOBNAME = "" Then
    Call MnuChangeJobName_Click
  End If
  Me.MousePointer = 11
  Call PackJobFiles
  Me.MousePointer = 1
  Exit Sub

ErrHandler:
  MsgBox "Error occurred while packing job", 16, "WARNING"
  Exit Sub
  
ErrHandler1:
  'User pressed Cancel button
  Exit Sub
End Sub

Public Sub RunSimulation(CumOut As Integer)
'*******************************************
  'Run the simulation by calling INORDCALC in the DLL
  Dim response As Integer
  Dim ReturnFile As String, ReturnFile2 As String
  Dim NL As String * 2
  Dim resp As Integer
  
  On Error GoTo ErrorCode
  MsgResponse = vbYes
  
  'Call each of the data forms to ensure that they are all consistent
  'This will load each form in turn, write the output files and unload the form.
  'Call MdlFlwsheet.AcceptFlowsheet
  'Call Sysdata.CmdReturn_Click
  'Call UnitMods.cmdQuit_Click
  
  NL = Chr(13) & Chr(10)
  ReturnFile = Dir(RTrim(JobPath) + "CURRDATA.SYD")
  If ReturnFile = "" Or Not JobSystemInputData Then
    MsgBox "The system data has not been defined", 16, "ERROR"
    MsgResponse = vbNo
    Exit Sub
  End If

  ReturnFile = Dir(RTrim(JobPath) + "CURRDATA.RUN")
  If ReturnFile = "" Or Not JobModelParameterData Then
    MsgBox "The model parameter data has not been defined", 16, "ERROR"
    MsgResponse = vbNo
    Exit Sub
  End If
  
  If JobSystemDataNeeded Then
    MsgBox "The flowsheet has been changed." & NL & "System data must be edited.", 16, "ERROR"
    MsgResponse = vbNo
    Exit Sub
  End If
  
  If ChangedJobname Then
    MsgBox "The job name has changed." & NL & "Flowsheet and system data must be edited.", 16, "ERROR"
    MsgResponse = vbNo
    Exit Sub
  End If
  
  If JobUnitParametersNeeded Then
    MsgBox "The flowsheet has been changed." & NL & "Unit model parameters must be edited.", 16, "ERROR"
    MsgResponse = vbNo
    Exit Sub
  End If
  
  Call FileConCat(RTrim(JobPath) + "CURRDATA.SYD", RTrim(JobPath) + "CURRDATA.RUN", RTrim(JobPath) + "DATT.DAT")

  resp = -1
  
  'Change the mouse pointer
  Me.MousePointer = 11
  
  'Specify the full JobPath variable here because the DLL must receive a 255 byte string.
  'The spaces will be stripped in INORDCALC
  resp = INORDCALC(JobPath)
  Me.MousePointer = 0
  If resp = -1 And CumOut <= 1 Then
    MsgBox "Simulation was completed successfully", 64, "INFORMATION"
    SimulationHasBeenRun = True
  Else
    SimulationHasBeenRun = False
  End If
 
  'Concatenate the two report files if necessary
  ReturnFile = Dir(RTrim(JobPath) + "REPORT1.OUT")
  ReturnFile2 = Dir(RTrim(JobPath) + "REPORT2.OUT")
  If ReturnFile <> "" Then
    If ReturnFile2 <> "" Then
      Call FileConCat(RTrim(JobPath) + "REPORT1.OUT", RTrim(JobPath) + "REPORT2.OUT", RTrim(JobPath) + "REPORT.OUT")
    Else
      FileCopy RTrim(JobPath) + "REPORT1.OUT", RTrim(JobPath) + "REPORT.OUT"
    End If
  ElseIf ReturnFile2 <> "" Then
    FileCopy RTrim(JobPath) + "REPORT2.OUT", RTrim(JobPath) + "REPORT.OUT"
  End If
  
  If resp = 37 Then
    MsgBox "Allowable number of iterations exceeded. " & NL & "View the DLL diagnostic file for details.", vbExclamation, "WARNING"
    Exit Sub
  ElseIf resp = 36 Then
    MsgBox "There was an unbalanced material flow around at least one unit." & NL & "View the DLL diagnostic file for details", vbExclamation, "WARNING"
    Exit Sub
  ElseIf resp = 13 Then
    response = MsgBox("An error occurred when reading the tear stream data." & NL & "Would you like to clear the tear stream data now?", 20, "ERROR")
    If response = vbYes Then
      Open RTrim(JobPath) + "TEARS.OUT" For Output As #36
      Print #36, "LAST 0 0 0 0"
      Close #36
      MsgResponse = vbNo
    End If
    Exit Sub
  ElseIf resp >= 10 Then
    MsgBox "The simulation phase did not complete properly." & NL & "View the DLL diagnostic file from the RUN menu for more details", 16, "Error"
    Exit Sub
  End If

On Error GoTo SimopError
  Me.MousePointer = 11
  SimopResp = SIMOP(JobPath, CumOut)
  MousePointer = 0
  If SimopResp = -1 And CumOut <= 1 Then
    MsgBox "Data output completed successfully", 64, "INFORMATION"
  ElseIf SimopResp = -1 And CumOut > 1 Then
    Exit Sub
  ElseIf SimopResp = 2 Then
    MsgBox "No output data is available", 16, "ERROR"
    Exit Sub
  ElseIf SimopResp = 3 Then
    MsgBox "Insufficient data from the simulator", 16, "ERROR"
    Exit Sub
  ElseIf SimopResp = 7 Or SimopResp = 8 Then
    MsgBox "Error in output format" & Chr(13) & Chr(10) & "Edit the output format", vbOKOnly, "ERROR"
  Else
    MsgBox "Error in setting up output data", 16, "ERROR"
    Exit Sub
  End If

  Exit Sub
  
ErrorCode:
  If Err.Number = 48 Then
    MsgBox "The dynamic link library containing your model code could not be found" & NL & "or your model code is not in the library.", vbCritical, "ERROR"
    MousePointer = 0
    Exit Sub
  End If
  MsgBox "An error occured in the simulation phase", 16, "ERROR"
  MousePointer = 0
  Exit Sub
SimopError:
  MsgBox "An error occured in the data output phase", 16, "ERROR"
  MousePointer = 0
  Exit Sub
  
  
End Sub

Public Sub SaveJobFiles()
'************************
'Save the job under JOBNAME
  On Error GoTo ErrHandler
  
  Dim ReturnFile As String
  Dim resp As Integer
  If JOBNAME = "" Then
    JOBNAME = InputBox("Specify a job name")
    JobFileName = Trim(JobSavePath) & JOBNAME & ".JOB"
    ReturnFile = Dir(JobFileName)
    If ReturnFile <> "" Then
      resp = MsgBox("A job with this name already exists.  Overwrite?", 17, "WARNING")
      If resp = vbCancel Then Exit Sub
    End If
  End If
  JobFileName = Trim(JobSavePath) & JOBNAME
  Caption = "MODSIM - Modular Simulator for Ore Dressing Plants  " & JobFileName
  Open JobFileName & ".JOB" For Output As #31
  Write #31, "Job name"; JOBNAME
  
  'Flowsheet data
  Write #31, "Flowsheet"; JobFlowsheet
  If JobFlowsheet Then
    FileCopy RTrim(JobPath) & "PLNTR.OUT", JobFileName & ".TRN"
    ChangedFlowsheet = False
  End If
  
  'System data coming from the flowsheet
  Write #31, "System data"; JobSystemData
  If JobSystemData Then
    FileCopy RTrim(JobPath) & "SysData.out", JobFileName & ".syd"
  End If
  
  'Model data from the flowsheet
  Write #31, "Model data"; JobModelData
  If JobModelData Then
    FileCopy RTrim(JobPath) & "ModsData.out", JobFileName & ".mod"
  End If
  
  'System input data from the SYSDATA module
  Write #31, "System input data"; JobSystemInputData
  If JobSystemInputData Then
    FileCopy RTrim(JobPath) & "CURRDATA.SYD", JobFileName & ".sid"
  End If
  ChangedSystemInputData = False
  
  'Particle size data from the SYSDATA module
  Write #31, "Size data"; JobSizeData
  If JobSizeData Then
    FileCopy RTrim(JobPath) & "SIZECOMP.OUT", JobFileName & ".siz"
  End If

  'G-class specifications
  
  'G-class distributions
  Write #31, "G-class distribution"; JobGCDistribution
  If JobGCDistribution Then
    FileCopy RTrim(JobPath) & "GC_Distr.out", JobFileName & ".gcd"
  End If
  
  'Liberation data
  Write #31, "Liberation data"; JobLibcomp
  If JobLibcomp Then
    FileCopy RTrim(JobPath) & "LIBCOMP.OUT", JobFileName & ".lib"
  End If
  
  'Andrews-Mika diagrams
  Write #31, "Ljubljana model"; JobLjubAMD
  If JobLjubAMD Then
    FileCopy RTrim(JobPath) & "LJUBAMD.DAT", JobFileName & ".lju"
  End If

  Write #31, "Beta function liberation model"; JobBetaAMD
  If JobBetaAMD Then
    FileCopy RTrim(JobPath) & "BetaAMD.DAT", JobFileName & ".amd"
  End If
    
  'S-class specifications
  
  'S-class distributions
  Write #31, "S-class distribution"; JobSCDistribution
  If JobSCDistribution Then
    FileCopy RTrim(JobPath) & "SC_DISTR.OUT", JobFileName & ".scd"
  End If
  
  'Washability data if approppriate
  Write #31, "Washability data"; JobWashabilityData
  If JobWashabilityData Then
    FileCopy RTrim(JobPath) & "Washdata.out", JobFileName & ".was"
  End If
  
  Write #31, "System data needed", JobSystemDataNeeded
  
  'Model parameters
  Write #31, "Model parameter data"; JobModelParameterData
  If JobModelParameterData Then
    FileCopy RTrim(JobPath) & "CurrData.Run", JobFileName & ".cur"
  End If
  Write #31, "Unit parameters needed", JobUnitParametersNeeded
  ChangedModelParameterData = False

  'Output format
  Write #31, "Output format"; JobOutputFormat
  If JobOutputFormat Then
    FileCopy RTrim(JobPath) & "FORMAT.OUT", JobFileName & ".mat"
  End If
  ChangedOutputFormat = False
  
  'SIMOP data that goes directly to SIMOP.FOR
  Write #31, "SIMOP data"; JobSimopData
  If JobSimopData Then
    FileCopy RTrim(JobPath) & "SIMOP.DAT", JobFileName & ".mop"
  End If
  
  Close #31
  
  'Tear streams
  FileCopy RTrim(JobPath) & "TEARS.OUT", JobFileName & ".TEA"

  SavedJob = True
  Exit Sub
ErrHandler:
  MsgBox "Error when saving job", 16, "ERROR"
  Close #31
  Exit Sub
End Sub

Private Sub PackJobFiles()
'*************************
'Pack the job files into a single file under jobfilename
  Dim LinesFromFile As String
  Dim ReturnFile As String
  Dim resp As Integer
  On Error GoTo ErrHandler

  If JOBNAME = "" Then
    JOBNAME = InputBox("Specify a job name")
    JobFileName = Trim(JobSavePath) & JOBNAME & ".PAK"
    ReturnFile = Dir(JobFileName)
    If ReturnFile <> "" Then
      resp = MsgBox("A job with this name already exists.  Overwrite?", 17, "WARNING")
      If resp = vbCancel Then Exit Sub
    End If
  End If
  Open JobFileName & ".PAK" For Output As #31
  Write #31, "Job name"; JOBNAME
  
  'Flowsheet data
  Write #31, "Flowsheet"; JobFlowsheet
  If JobFlowsheet Then
    Open RTrim(JobPath) & "PLNTR.OUT" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'System data coming from the flowsheet
  Write #31, "System data"; JobSystemData
  If JobSystemData Then
    Open RTrim(JobPath) & "SysData.out" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'Model data from the flowsheet
  Write #31, "Model data"; JobModelData
  If JobModelData Then
    Open RTrim(JobPath) & "ModsData.out" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'System input data from the SYSDATA module
  Write #31, "System input data"; JobSystemInputData
  If JobSystemInputData Then
    Open RTrim(JobPath) & "CURRDATA.SYD" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'Particle size data from the SYSDATA module
  Write #31, "Size data"; JobSizeData
  If JobSizeData Then
    Open RTrim(JobPath) & "SIZECOMP.OUT" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If

  'G-class specifications
  
  'G-class distributions
  Write #31, "G-class distribution"; JobGCDistribution
  If JobGCDistribution Then
    Open RTrim(JobPath) & "GC_Distr.out" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'Liberation data
  Write #31, "Liberation data"; JobLibcomp
  If JobLibcomp Then
    Open RTrim(JobPath) & "LIBCOMP.OUT" For Input As 32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'Andrews-Mika diagrams
  Write #31, "Ljubljana model"; JobLjubAMD
  If JobLjubAMD Then
    Open RTrim(JobPath) & "LJUBAMD.DAT" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If

  Write #31, "Beta function liberation model"; JobBetaAMD
  If JobBetaAMD Then
    Open RTrim(JobPath) & "BetaAMD.DAT" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
    
  'S-class specifications
  
  'S-class distributions
  Write #31, "S-class distribution"; JobSCDistribution
  If JobSCDistribution Then
    Open RTrim(JobPath) & "SC_DISTR.OUT" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'Washability data if approppriate
  Write #31, "Washability data"; JobWashabilityData
  If JobWashabilityData Then
    Open RTrim(JobPath) & "Washdata.out" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'Model parameters
  Write #31, "Model parameter data"; JobModelParameterData
  If JobModelParameterData Then
    Open RTrim(JobPath) & "CurrData.Run" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If

  'Output format
  Write #31, "Output format"; JobOutputFormat
  If JobOutputFormat Then
    Open RTrim(JobPath) & "FORMAT.OUT" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  
  'SIMOP data that goes directly to SIMOP.FOR
  Write #31, "SIMOP data"; JobSimopData
  If JobSimopData Then
    Open RTrim(JobPath) & "SIMOP.DAT" For Input As #32
    LinesFromFile = Input(LOF(32) - 2, #32)
    Print #31, LinesFromFile
    Close 32
  End If
  Close #31
  
  Exit Sub
ErrHandler:
  MsgBox "Error when packing job", 16, "ERROR"
  Close 31
  Close 32
  Exit Sub
End Sub

Private Sub Simulate_Click()
'***************************
  'Run a single simulation
  Call RunSimulation(0)
End Sub



Private Sub ViewCalcOrder_Click()
'***************************
  EchoFileName = "PHO1.OUT"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub ViewDiagDLL_Click()
'***************************
  EchoFileName = "DiagDLL.txt"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub mnuViewUserDiag_Click()
'**********************************
  EchoFileName = "UserDiagFile.txt"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub ViewFlowsheet_Click()
'********************************
  ViewFlowsheetFlag = True
  EditFlowsheetFlag = False
  Call StartFlowsheet
  End Sub

Private Sub ViewOutputFile_Click()
'*********************************
  EchoFileName = "SIM.OUT"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Private Sub ViewTearStreams_Click()
'**********************************
  EchoFileName = "TEARS.OUT"
  If SetUpViewFile = 0 Then Exit Sub
  ViewPage(ViewPageNumber).Top = DisplayOffsetY + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Left = DisplayOffsetX + 200 * (ViewPageNumber - 1)
  ViewPage(ViewPageNumber).Show
End Sub

Public Sub ClearWorkspace()
'**************************
'Clears workspace to start a new job
On Error GoTo ErrHandler
JOBNAME = ""
Caption = "MODSIM - Modular Simulator for Ore Dressing Plants "
Call ResetMDIFlags
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
Close #14

On Error GoTo ErrHandler2
Open RTrim(JobPath) & "TEARS.OUT" For Output As #14
Print #14, "LAST 0 0 0 0"
Close #14
Exit Sub

ErrHandler:
  MsgBox "An error occured while clearing the workspace", 48, "WARNING"
  Resume Next
  
ErrHandler2:
  MsgBox "Could not open TEARS file", vbCritical, "ERROR"
  Exit Sub
End Sub

Public Sub DeleteFile(FileName As String)
'****************************************
  'Deletes files but checks for existence.
  'Will accept wild cards.
  Dim FileExists As String
  
  On Error GoTo ErrHandler
  FileExists = Dir(FileName)
  Do While FileExists <> ""
    Kill FileName
    FileExists = Dir
  Loop
  Exit Sub
  
ErrHandler:
  MsgBox "Could not access file " & FileName & vbCrLf & "This file must be closed before running Modsim", vbOKOnly, "ERROR"
  Exit Sub
End Sub
