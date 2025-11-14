VERSION 4.00
Begin VB.Form Modsim 
   BackColor       =   &H00C0C0C0&
   Caption         =   "MODSIM - Modular Simulator for Ore Dressing Plants"
   ClientHeight    =   1170
   ClientLeft      =   2040
   ClientTop       =   1800
   ClientWidth     =   6630
   Height          =   1860
   Left            =   1980
   LinkTopic       =   "Form1"
   MDIChild        =   -1  'True
   ScaleHeight     =   1170
   ScaleWidth      =   6630
   Top             =   1170
   Width           =   6750
   Begin VB.TextBox TxtJobName 
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   1080
      TabIndex        =   1
      Top             =   120
      Width           =   4455
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   120
      Top             =   480
      _Version        =   65536
      _ExtentX        =   847
      _ExtentY        =   847
      _StockProps     =   0
   End
   Begin VB.Label LblJobName 
      Alignment       =   2  'Center
      Caption         =   "Job name:"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   855
   End
   Begin VB.Menu File 
      Caption         =   "File"
      Begin VB.Menu New 
         Caption         =   "Start a new job"
      End
      Begin VB.Menu ExistingJob 
         Caption         =   "Open an existing job"
      End
      Begin VB.Menu SaveAs 
         Caption         =   "Save this job as ..."
      End
      Begin VB.Menu SaveJob 
         Caption         =   "Save this job"
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
      Begin VB.Menu OutputFormat 
         Caption         =   "Edit output format"
      End
   End
   Begin VB.Menu View 
      Caption         =   "View"
      Begin VB.Menu Flowsheet 
         Caption         =   "Flowsheet"
      End
      Begin VB.Menu DataEcho 
         Caption         =   "Data echo file"
      End
      Begin VB.Menu ViewOutputFile 
         Caption         =   "Data output file"
      End
      Begin VB.Menu ReportFile 
         Caption         =   "Report file"
      End
      Begin VB.Menu PSDGraphs 
         Caption         =   "Size distribution graphs"
      End
   End
   Begin VB.Menu Run 
      Caption         =   "Run"
      Begin VB.Menu Simulate 
         Caption         =   "Run simulation"
      End
      Begin VB.Menu ViewCalcOrder 
         Caption         =   "Review calculation order"
      End
      Begin VB.Menu ViewDiagDLL 
         Caption         =   "View DLL diagnostic file"
      End
   End
   Begin VB.Menu Window 
      Caption         =   "Window"
      WindowList      =   -1  'True
   End
End
Attribute VB_Name = "Modsim"
Attribute VB_Creatable = False
Attribute VB_Exposed = False


Private Sub DataEcho_Click()
'***************************
  EchoFileName = "PHI1.ECH"
  ViewFile.Show
  Modsim.Hide
End Sub

Private Sub Edit_Click()
'***********************
Call FLSheet.ResetFlags

End Sub

Private Sub EditFlowsheet_Click()
'********************************
'Redraw the flowsheet using the data stored in file PLNTR
  'Modsim.Hide
  Call StartFlowsheet
End Sub

Private Sub EditSysdata_Click()
  Call StartSysdata
End Sub


Private Sub ExistingJob_Click()
'******************************
'Load an existing job
  Dim I As Integer
  On Error GoTo ErrHandler
  ' Set filters
  CommonDialog1.Filter = "Job files (*.job)|*.job|All files|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.ShowOpen
  WSTRING = Trim(CommonDialog1.FileName)
  I = InStr(WSTRING, ".")
  If I > 0 Then
    JobFileName = Mid(WSTRING, 1, I - 1)
  End If
  Open JobFileName & ".JOB" For Input As #31
  Input #31, WSTRING
  If WSTRING = "Job name" Then
    Input #31, WSTRING
    TxtJobName.TEXT = WSTRING
  Else
    MsgBox "Incorrect file type", 16, "ERROR"
    Close #31
    Exit Sub
  End If
    
  Input #31, WSTRING, JobFlowsheet
  If JobFlowsheet Then
    FileCopy JobFileName & ".TR", RTrim(JobPath) & "PLNTR.OUT"
  End If

  Input #31, WSTRING, JobSystemData
  If JobSystemData Then
    FileCopy JobFileName & ".syd", RTrim(JobPath) & "SYSDATA.OUT"
  End If
  
  Input #31, WSTRING, JobModelData
  If JobModelData Then
    FileCopy JobFileName & ".mod", RTrim(JobPath) & "MODSDATA.OUT"
  End If
  
  Input #31, WSTRING, JobSystemInputData
  If JobSystemInputData Then
    FileCopy JobFileName & ".sid", RTrim(JobPath) & "CURRDATA.SYD"
  End If

  Input #31, WSTRING, JobSizeData
  If JobSizeData Then
    FileCopy JobFileName & ".siz", RTrim(JobPath) & "SIZECOMP.OUT"
  End If

  Input #31, WSTRING, JobGCDistribution
  If JobGCDistribution Then
    FileCopy JobFileName & ".gcd", RTrim(JobPath) & "GC_DISTR.OUT"
  End If

  Input #31, WSTRING, JobLibcomp
  If JobLibcomp Then
    FileCopy JobFileName & ".lib", RTrim(JobPath) & "LIBCOMP.OUT"
  End If

  Input #31, WSTRING, JobSCDistribution
  If JobSCDistribution Then
    FileCopy JobFileName & ".scd", RTrim(JobPath) & "SC_DISTR.OUT"
  End If

 Input #31, WSTRING, JobWashabiltyData
  If JobWashabilityData Then
    FileCopy JobFileName & ".was", RTrim(JobPath) & "WASHDATA.OUT"
  End If

  Input #31, WSTRING, JobModelParameterData
  If JobModelParameterData Then
    FileCopy JobFileName & ".cur", RTrim(JobPath) & "CURRDATA.RUN"
  End If

  Input #31, WSTRING, JobOutputFormat
  If JobOutputFormat Then
    FileCopy JobFileName & ".mat", RTrim(JobPath) & "FORMAT.OUT"
  End If
  Close #31
  
  'Load the sysdata form so that the information is available for any other form.
  Load Sysdata
  Exit Sub
  
ErrHandler:
  'User pressed Cancel button
  MsgBox "Job was not loaded properly", 16, "WARNING"
  Close #31
  Exit Sub
End Sub

Private Sub Exit_Click()
'***********************
  Dim Response As Integer
  'Has this job been saved?
  If Not SavedJob Then
    Response = MsgBox("Save this job before exit?", 36, "")
    If Response = vbYes Then
      Call SaveJob_Click
    End If
  End If
  
  'Now check for any changed files to save
  If ChangedFlowsheet Then
    Response = MsgBox("Save changes to flowsheet?", 4, "WARNING: The flowsheet was edited")
    If Response = vbYes Then
      FileCopy RTrim(JobPath) & "PLNTR.OUT", JobFileName & ".tr"
      FileCopy RTrim(JobPath) & "SysData.out", JobFileName & ".syd"
      FileCopy RTrim(JobPath) & "ModsData.out", JobFileName & ".mod"
    End If
  End If
  If ChangedSystemInputData Then
    Response = MsgBox("Save changes to system data?", 4, "WARNING: The system data has been edited")
    If Response = vbYes Then
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
    Response = MsgBox("Save changes to model paramters?", 4, "WARNING: Model parameters have been edited")
    If Response = vbYes Then
      FileCopy RTrim(JobPath) & "CurrData.Run", JobFileName & ".cur"
    End If
  End If
  If ChangedOutputFormat Then
    Response = MsgBox("Save changes to the output format?", 4, "WARNING: The output format has been edited")
    If Response = vbYes Then
      FileCopy RTrim(JobPath) & "FORMAT.OUT", JobFileName & ".mat"
    End If
  End If
  
  End
End Sub




Private Sub Flowsheet_Click()
  Modsim.Hide
  Load FLSheet
  Call StartFlowsheet
End Sub


Private Sub Form_Load()
'**********************
  JobPath = "d:\Progra~1\Micros~1\Projects\Modsim\"
  SavedJob = False
  
  Newflowsheet = True
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
  
  ChangedFlowsheet = False
  ChangedSystemInputData = False
  ChangedModelParameterData = False
  ChangedOutputFormat = False
End Sub

Private Sub ModelParameters_Click()
'**********************************
  Modsim.Hide
  Call StartUnitMods
End Sub

Private Sub New_Click()
  LblJobName.Visible = True
  TxtJobName.Visible = True
  'TxtJobName.TEXT = "Job1"
  Modsim.Hide
  FLSheet.Newflowsheet = True
  Call StartFlowsheet
End Sub


Private Sub OutputFormat_Click()
'*******************************
  Modsim.Hide
  OutFormat.Show
End Sub

Private Sub PSDGraphs_Click()
  Modsim.Hide
  Load Sizgraph
  Sizgraph.Show
End Sub

Private Sub ReportFile_Click()
'*****************************
  EchoFileName = "REPORT.OUT"
  ViewFile.Show
  Modsim.Hide
End Sub

Private Sub SaveAs_Click()
'*************************
' Save job files
  Dim I As Integer
  On Error GoTo ErrHandler
  ' Set filters
  CommonDialog1.Filter = "Job files (*.job)|*.job"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.ShowSave
  
  'Strip off any file type extension
  WSTRING = Trim(CommonDialog1.FileName)
  I = InStr(WSTRING, ".")
  If I > 0 Then
    JobFileName = Mid(WSTRING, 1, I - 1)
  End If
  Call SaveJobFiles
  Exit Sub

ErrHandler:
  'User pressed Cancel button
  Exit Sub
End Sub

Private Sub SaveJob_Click()
'**************************
If Modsim.TxtJobName = "" Then
  Call SaveAs_Click
Else
  Call SaveJobFiles
End If
End Sub


Private Sub Simulate_Click()
'***************************
  'Run the simulation by calling INORDCALC in the DLL
  Dim resp As Integer
  
  On Error GoTo ErrorCode
  Call FileConCat(RTrim(JobPath) + "CURRDATA.SYD", RTrim(JobPath) + "CURRDATA.RUN", RTrim(JobPath) + "DATT.DAT")

  resp = -1
  
  'Specify the full JobPath variable here. The spaces will be stripped in INORDCALC
  resp = INORDCALC(JobPath)
  If resp = -1 Then
    MsgBox "Back from INORDCALC", 32, "INFORMATION"
  End If
  If resp = 1 Then
    MsgBox "An error occured in the simulation phase. View the DLL diagnostic file for more details", 16, "Error"
    Exit Sub
  End If
 'If resp = 101 Then
    'resp = MsgBox("Allowable number of iterations exceeded", 0, "MESSAGE")
  'End If
  'If resp = 101 Then
    'resp = MsgBox("Insufficient data from calculation phase", 0, "ERROR")
  'End If
  'If resp = 102 Then
    'resp = MsgBox("Error reading file OPDISP.DAT", 0, "ERROR")
  'End If
  'If resp = 103 Then
    'resp = MsgBox("No mineral liberation data is available", 0, "MESSAGE")
  'End If
  resp = SIMOP(JobPath)
  If resp = -1 Then
    MsgBox "Back from SIMOP", 32, "INFORMATION"
  Else
    resp = MsgBox("Error in setting up output data", 16, "ERROR")
  End If

  resp = 0
  Exit Sub
  
ErrorCode:
  MsgBox "An error occured in the simulation phase", 16, "ERROR"
  
End Sub




Public Sub SaveJobFiles()
'************************
'Save the job under JobFilename
  JobFileName = Modsim.TxtJobName.TEXT
  Open JobFileName & ".JOB" For Output As #31
  If TxtJobName.TEXT = "" Then
    TxtJobName.TEXT = InputBox("Specify a job name")
    LblJobName.Visible = True
    TxtJobName.Visible = True
  End If
  Write #31, "Job name"; TxtJobName.TEXT
  
  'Flowsheet data
  Write #31, "Flowsheet"; JobFlowsheet
  If JobFlowsheet Then
    FileCopy RTrim(JobPath) & "PLNTR.OUT", JobFileName & ".tr"
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
  
  'Model parameters
  Write #31, "Model parameter data"; JobModelParameterData
  If JobModelParameterData Then
    FileCopy RTrim(JobPath) & "CurrData.Run", JobFileName & ".cur"
  End If
  ChangedModelParameterData = False

  'Output format
  Write #31, "Output format"; JobOutputFormat
  If JobOutputFormat Then
    FileCopy RTrim(JobPath) & "FORMAT.OUT", JobFileName & ".mat"
  End If
  ChangedOutputFormat = False
  Close #31

  SavedJob = True
End Sub

Private Sub TxtJobName_Change()
'******************************
  SavedJob = False
End Sub

Private Sub ViewCalcOrder_Click()
'***************************
  EchoFileName = "PHO1.OUT"
  ViewFile.Show
  Modsim.Hide

End Sub

Private Sub ViewDiagDLL_Click()
'***************************
  EchoFileName = "DiagDLL.txt"
  ViewFile.Show
  Modsim.Hide

End Sub


Private Sub ViewOutputFile_Click()
'*********************************
  EchoFileName = "SIM.OUT"
  ViewFile.Show
  Modsim.Hide
    
End Sub
