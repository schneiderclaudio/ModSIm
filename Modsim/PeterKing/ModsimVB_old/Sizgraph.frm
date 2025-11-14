VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form Sizgraph 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Plot size distributions"
   ClientHeight    =   5430
   ClientLeft      =   2100
   ClientTop       =   1605
   ClientWidth     =   8865
   BeginProperty Font 
      Name            =   "Arial"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00C0C0C0&
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5430
   ScaleWidth      =   8865
   Begin VB.CommandButton cmdGetDistribution 
      Caption         =   "Get distribution from file"
      Height          =   495
      Left            =   5040
      TabIndex        =   18
      Top             =   4800
      Width           =   2295
   End
   Begin VB.CommandButton cmdSaveDistribution 
      Caption         =   "Save distribution for plotting"
      Height          =   495
      Left            =   5040
      TabIndex        =   17
      ToolTipText     =   "Save a size distribution to plot later"
      Top             =   4200
      Width           =   2295
   End
   Begin VB.CheckBox ChkShowGridLines 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Show grid lines"
      Height          =   255
      Left            =   5040
      TabIndex        =   16
      Top             =   3240
      Value           =   1  'Checked
      Width           =   1695
   End
   Begin VB.ListBox StreamList 
      Appearance      =   0  'Flat
      ForeColor       =   &H00000000&
      Height          =   4020
      Left            =   120
      TabIndex        =   0
      ToolTipText     =   "Double click to select"
      Top             =   720
      Width           =   4800
   End
   Begin VB.CommandButton CmdView 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "View graph"
      Enabled         =   0   'False
      Height          =   495
      Left            =   5040
      TabIndex        =   9
      Top             =   3600
      Width           =   2295
   End
   Begin VB.CheckBox ChkExp 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Show experimental data"
      Height          =   240
      Left            =   5040
      TabIndex        =   8
      Top             =   2880
      Value           =   1  'Checked
      Width           =   2415
   End
   Begin VB.CommandButton CmdReturn 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Close"
      Height          =   615
      Left            =   7560
      TabIndex        =   10
      Top             =   4080
      Width           =   1215
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for size"
      Height          =   855
      Left            =   6120
      TabIndex        =   15
      Top             =   1800
      Width           =   2640
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "microns"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   4
         Top             =   240
         Value           =   -1  'True
         Width           =   1320
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   3
         Left            =   1680
         TabIndex        =   7
         TabStop         =   0   'False
         Top             =   480
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   6
         TabStop         =   0   'False
         Top             =   480
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         Height          =   240
         Index           =   1
         Left            =   1680
         TabIndex        =   5
         TabStop         =   0   'False
         Top             =   240
         Width           =   720
      End
   End
   Begin VB.ListBox GraphList 
      ForeColor       =   &H00000000&
      Height          =   1740
      IntegralHeight  =   0   'False
      Left            =   5040
      TabIndex        =   1
      ToolTipText     =   "Double click to remove"
      Top             =   720
      Width           =   975
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Coordinates"
      Height          =   960
      Left            =   6120
      TabIndex        =   14
      Top             =   720
      Width           =   2640
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Linear-log"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   2
         Top             =   240
         Value           =   -1  'True
         Width           =   1800
      End
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Log-log"
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   3
         TabStop         =   0   'False
         Top             =   600
         Width           =   1800
      End
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   8160
      Top             =   2880
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0FFFF&
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   120
      TabIndex        =   11
      Top             =   120
      Visible         =   0   'False
      Width           =   8655
   End
   Begin VB.Label LblGraph 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Graph list"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   5040
      TabIndex        =   13
      Top             =   480
      Width           =   975
   End
   Begin VB.Label LblStreams 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Select plant streams to plot"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   0
      TabIndex        =   12
      Top             =   480
      Width           =   5400
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuExportData 
         Caption         =   "Export data file"
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "Sizgraph"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Public N_DATA_STREAMS As Integer
Private CURRDATA(30) As SIZEDATA
Private WSTRING As String
Private STREAM_TYPE As String * 1
Private STREAM_NAME As String * 30
Private RetVal As Integer
Private DataSetName As String
Private AdditionalDistributionNumber As Integer

Private Sub cmdGetDistribution_Click()
'*************************************
  Dim I As Integer
  Dim NDC As Integer
  Dim CumSize(MaximumSizeClasses) As Single, Cum(MaximumSizeClasses) As Single
  
  If AdditionalDistributionNumber >= 110 Then
    MsgBox "Only 10 additional distributions can be used", vbExclamation, "Warning"
    Exit Sub
  End If
  Call GetSavedSizeDistribution(DataSetName, NDC, CumSize, Cum)
  If DataSetName <> "" Then
    AdditionalDistributionNumber = AdditionalDistributionNumber + 1
    AdditionalDistribution(AdditionalDistributionNumber - 100).NSTR = AdditionalDistributionNumber
    AdditionalDistribution(AdditionalDistributionNumber - 100).NDC = NDC
    For I = 1 To NDC
      AdditionalDistribution(AdditionalDistributionNumber - 100).CumSize(I) = CumSize(I)
      AdditionalDistribution(AdditionalDistributionNumber - 100).Fraction(I) = Cum(I)
    Next I
    Me.StreamList.AddItem CStr(AdditionalDistributionNumber) & " " & DataSetName
  End If
End Sub

Private Sub CmdReturn_Click()
'****************************
  Unload Sizgraph
End Sub

Private Sub cmdSaveDistribution_Click()
'**************************************
  Dim StreamNumber As Integer
  On Error GoTo ErrHandler
  
  If StreamList.ListIndex = -1 Then
    MsgBox "Select a stream for which size disribution must be saved", vbExclamation, "WARNING"
    Exit Sub
  End If
  
  StreamNumber = Val(Me.StreamList.text)
  Call SaveSizeDistribution(StreamNumber)
  Exit Sub
ErrHandler:
  Close #31
  MsgBox "Error saving file", vbCritical, "ERROR"
  Exit Sub

End Sub

Private Sub CmdView_Click()
'**************************
  On Error GoTo ErrHandler
  
  Call OPGRAPH
  Exit Sub
  
ErrHandler:
  MsgBox "Error occurred when running the graph plotting program", 16, "ERROR"
  Exit Sub
  
End Sub

Private Sub Form_Load()
'**********************
  Dim response As Integer
  Dim FileExists As String
  Dim N As Integer, NSTR As Integer, ID As Integer
  Dim Feed As Single, Percent As Single
  Dim M As Integer, Nstream As Integer
  Dim STREAM_ID As Integer, From_unit As Integer, To_unit As Integer
  Dim NL As String * 2
  On Error GoTo ErrHandler
  CommonDialog1.CancelError = True
  
  MODSIM.SizgraphIsActive = True
  NL = Chr(13) & Chr(10)
  
  AdditionalDistributionNumber = 100
  FileExists = Dir(RTrim(JobPath) & "PSDPLOT.OUT")
  If FileExists <> "" Then
    Kill RTrim(JobPath) & "PSDPLOT.OUT"
  End If
  FileExists = Dir(RTrim(JobPath) & "PSFILE01")
  If FileExists <> "" Then
    Kill RTrim(JobPath) & "PSFILE01"
  End If
  FileExists = Dir(RTrim(JobPath) & "EPSI0001")
  If FileExists <> "" Then
    Kill RTrim(JobPath) & "EPSI0001"
  End If
  
  'Open file SIZECOMP.OUT to get current data for size distributions
  Open RTrim(JobPath) & "SIZECOMP.OUT" For Input As 35
  For N = 1 To 30
    CURRDATA(N).ID = 0
  Next N
  Line Input #35, WSTRING
  If Mid$(WSTRING, 1, 8) <> "END OF F" Then
    If Trim(WSTRING) <> Trim(MODSIM.JOBNAME) Then
    WSTRING = "Size data has jobname" & Trim(WSTRING) & " This could indicate a data mismatch"
    response = MsgBox(WSTRING, 1, "WARNING")
    If response = 2 Then
      Close #31
      Exit Sub
    End If
  End If

    N = 0
    WSTRING = Input$(10, #35)
    Do While Not EOF(35)
      If WSTRING = "END OF FIL" Then Exit Do
      If N > 30 Then
        MsgBox "Too many data sets", 0, "WARNING"
        Exit Do
      End If
      If WSTRING = "Stream    " Then
        N = N + 1
        Input #35, NSTR, ID
        Line Input #35, STREAM_NAME
        CURRDATA(N).NSTR = NSTR
        CURRDATA(N).ID = ID
        CURRDATA(N).STREAM_NAME = STREAM_NAME
        WSTRING = Input$(10, #35)
        If WSTRING = "Feedrate  " Then
          Input #35, Feed, Percent
          CURRDATA(N).FEED_RATE = Feed
          CURRDATA(N).PERCENT_SOLIDS = Percent
          WSTRING = Input$(10, #35)
        End If
        If WSTRING = "Size dist " Then
          Input #35, CURRDATA(N).NDC
          For M = 1 To CURRDATA(N).NDC
            Input #35, CURRDATA(N).CumSize(M), CURRDATA(N).Fraction(M)
          Next M
          CURRDATA(N).NDC = CURRDATA(N).NDC + 1
          WSTRING = Input$(10, #35)
        End If
        If WSTRING = "Water feed" Then
          Input #35, Feed, Percent
          CURRDATA(N).WATER_RATE = Feed
          CURRDATA(N).PERCENT_SOLIDS = Percent
          WSTRING = Input$(10, #35)
        End If
      Else
        WSTRING = Input$(10, #35)
        If WSTRING <> "Stream    " Then
          WSTRING = "File jobname.SIZ is obsolete."

          WSTRING = WSTRING & NL & "Add jobname, reformat headings and add stream names"
  
          WSTRING = WSTRING & NL & "Or delete this file and re-enter all data."
          MsgBox WSTRING, 0, "ERROR"
        Exit Do
          End
        End If
      End If
    Loop
    N_DATA_STREAMS = N
  End If
  Close #35

  'Open file to get the stream data coming from the flowsheet
  Open RTrim(JobPath) + "SYSDATA.OUT" For Input As #31
  Line Input #31, WSTRING
  If Trim(WSTRING) <> Trim(MODSIM.JOBNAME) Then
    WSTRING = "Flowsheet data has jobname" & Trim(WSTRING) & " This could indicate a data mismatch"
    response = MsgBox(WSTRING, 1, "WARNING")
    If response = 2 Then
      Close #31
      Exit Sub
    End If
  End If
  Input #31, Nstream
  For N = 1 To Nstream
    Input #31, NSTR
    Input #31, STREAM_ID
    STREAM_TYPE = Input$(1, #31)
    Input #31, From_unit
    Input #31, To_unit
    WSTRING = Str$(NSTR)
    For M = 1 To N_DATA_STREAMS
      If CURRDATA(M).ID = STREAM_ID Then
        WSTRING = WSTRING + " " + CURRDATA(M).STREAM_NAME
      End If
    Next M
    If STREAM_TYPE = "F" Then
      StreamList.AddItem WSTRING
    End If
    If STREAM_TYPE = "O" Then
      StreamList.AddItem WSTRING
    End If
  Next N
  Close #31
  Exit Sub
  
ErrHandler:
  MsgBox "Error in loading particle size graph form", 16, "ERROR"
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  MODSIM.SizgraphIsActive = False
End Sub

Private Sub GraphList_DblClick()
  GraphList.RemoveItem GraphList.ListIndex
  If GraphList.ListCount = 0 Then
    CmdView.Enabled = 0
  End If
End Sub

Private Sub GraphList_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "List of streams to be included on the graph.  Double click to remove one"
End Sub

Private Sub GraphList_LostFocus()
  Instruction.Visible = 0
End Sub

Private Sub MnuClose_Click()
'***************************
Unload Sizgraph
End Sub

Private Sub MnuExportData_Click()
'******************************
  Dim FileExists As String
  Dim RetVal As Integer
  On Error GoTo ErrHandler
  CommonDialog1.CancelError = True
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|All files (*.*)|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.Flags = cdlOFNHideReadOnly
  CommonDialog1.ShowSave
  FileExists = Dir(CommonDialog1.FileName)
  If FileExists <> "" Then
    RetVal = MsgBox(CommonDialog1.FileName & " already exists. Overwrite?", 33, "WARNING")
    If RetVal = vbCancel Then
      Exit Sub
    End If
  End If
  FileCopy RTrim(JobPath) & "PSDPLOT.OUT", CommonDialog1.FileName
  Kill RTrim(JobPath) & "PSDPLOT.OUT"
  MnuExportData.Enabled = False
  Exit Sub
  
ErrHandler:
  Exit Sub

End Sub

Private Sub MnuFile_Click()
'**************************
  Dim FileExists As String
  FileExists = Dir(RTrim(JobPath) & "PSDPLOT.OUT")
  If FileExists = "" Then
    MnuExportData.Enabled = False
  Else
    MnuExportData.Enabled = True
  End If
  End Sub

Private Sub OptCoord_GotFocus(Index As Integer)
  Instruction.Visible = -1
  If Index = 0 Then
    Instruction.Caption = "Plot graph in the linear-log coordinate system"
  End If
  If Index = 1 Then
    Instruction.Caption = "Plot graph in the log-log coordinate system"
End If
End Sub

Private Sub OptCoord_LostFocus(Index As Integer)
  Instruction.Visible = 0
End Sub

Private Sub OptSize_GotFocus(Index As Integer)
  Instruction.Visible = -1
  Instruction.Caption = "Choose units for the size axis"
End Sub

Private Sub optSize_LostFocus(Index As Integer)
  Instruction.Visible = 0
End Sub

Private Function SETUP_GRAPH() As Integer
'****************************************
  Dim I As Integer
  'Open a file to transmit data to OPGRAPH
  If GraphList.ListCount > 10 Then
    MsgBox "Choose no more than 10 streams to display on a single graph", 16, "ERROR"
    SETUP_GRAPH = 0
    Exit Function
  End If
  Open RTrim(JobPath) & "PSDGRAPH.OUT" For Output As 32
  Print #32, GraphList.ListCount
  For I = 1 To GraphList.ListCount
    Print #32, Val(GraphList.List(I - 1));
  Next I
  Print #32,
  If OptSize(0).Value = -1 Then Print #32, " 6"
  If OptSize(1).Value = -1 Then Print #32, " 3"
  If OptSize(2).Value = -1 Then Print #32, " 2"
  If OptSize(3).Value = -1 Then Print #32, " 0"
  If OptCoord(0).Value = -1 Then Print #32, " 1"
  If OptCoord(1).Value = -1 Then Print #32, " 2"
  If ChkShowGridLines.Value = 1 Then
    Print #32, "Y"
  Else
    Print #32, "N"
  End If
  If ChkExp.Value = 1 Then
    Print #32, "T"
  Else
    Print #32, "F"
  End If
  SETUP_GRAPH = -1
  Close #32
End Function

Private Sub StreamList_DblClick()
'********************************
  Dim NSTR As Integer
  NSTR = Val(StreamList.text)
  GraphList.AddItem Str$(NSTR)
  CmdView.Enabled = -1
End Sub

Private Sub StreamList_GotFocus()
  Instruction.Visible = -1
  Instruction.Caption = "List of streams in the plant.  Double click to select for display on the graph"
End Sub

Private Sub StreamList_LostFocus()
  Instruction.Visible = 0
End Sub

