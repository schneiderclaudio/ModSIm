VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Sizgraph 
   Appearance      =   0  'Flat
   AutoRedraw      =   -1  'True
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Plot size distributions"
   ClientHeight    =   5625
   ClientLeft      =   105
   ClientTop       =   1770
   ClientWidth     =   9960
   ForeColor       =   &H00C0C0C0&
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5625
   ScaleWidth      =   9960
   Begin VB.CommandButton cmdRemoveDataSet 
      Caption         =   "Remove data set"
      Height          =   500
      Left            =   4200
      TabIndex        =   31
      Top             =   4080
      Width           =   1155
   End
   Begin VB.CommandButton cmdGetDatafromSprdSht 
      Caption         =   "Get data from spreadsheet"
      Height          =   500
      Left            =   2880
      TabIndex        =   30
      ToolTipText     =   "Copy and paste from spreadsheet"
      Top             =   4080
      Width           =   1155
   End
   Begin VB.CheckBox ChkTruncated 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Plot as a truncated distribution"
      Height          =   255
      Left            =   240
      TabIndex        =   28
      Top             =   4800
      Width           =   3135
   End
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   1800
      TabIndex        =   27
      Top             =   5160
      Width           =   735
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Line type"
      Height          =   615
      Left            =   5520
      TabIndex        =   23
      Top             =   3240
      Width           =   4335
      Begin VB.OptionButton OptSmoothCurve 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Smooth curve"
         Height          =   255
         Left            =   2760
         TabIndex        =   26
         Top             =   240
         Width           =   1335
      End
      Begin VB.OptionButton OptStraightLines 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Straight lines"
         Height          =   255
         Left            =   1200
         TabIndex        =   25
         Top             =   240
         Width           =   1215
      End
      Begin VB.OptionButton OptNone 
         BackColor       =   &H00C0C0C0&
         Caption         =   "None"
         Height          =   255
         Left            =   240
         TabIndex        =   24
         Top             =   240
         Value           =   -1  'True
         Width           =   855
      End
   End
   Begin VB.CommandButton CmdSaveDataAs 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "Save data as..."
      Enabled         =   0   'False
      Height          =   500
      Left            =   1560
      TabIndex        =   17
      Top             =   4080
      Width           =   1155
   End
   Begin VB.CommandButton CmdReturn 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Exit"
      Height          =   465
      Left            =   8040
      TabIndex        =   18
      Top             =   5040
      Width           =   1680
   End
   Begin VB.CommandButton CmdSaveData 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "Save data"
      Enabled         =   0   'False
      Height          =   500
      Left            =   240
      TabIndex        =   16
      Top             =   4080
      Width           =   1155
   End
   Begin VB.ListBox StreamList 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H00000000&
      Height          =   2370
      Left            =   240
      TabIndex        =   1
      ToolTipText     =   "Double click to select"
      Top             =   600
      Width           =   5160
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units for size"
      Height          =   840
      Left            =   5520
      TabIndex        =   22
      Top             =   2280
      Width           =   2775
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         Height          =   240
         Index           =   3
         Left            =   1680
         TabIndex        =   12
         Top             =   480
         Value           =   -1  'True
         Width           =   600
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   11
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
         TabIndex        =   10
         TabStop         =   0   'False
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "microns"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   9
         TabStop         =   0   'False
         Top             =   240
         Width           =   1320
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Coordinates"
      Height          =   1815
      Left            =   6960
      TabIndex        =   21
      Top             =   360
      Width           =   1815
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Linear-log"
         Height          =   240
         Index           =   1
         Left            =   120
         TabIndex        =   4
         Top             =   480
         Value           =   -1  'True
         Width           =   1095
      End
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Logistic"
         Height          =   240
         Index           =   5
         Left            =   120
         TabIndex        =   8
         TabStop         =   0   'False
         Top             =   1440
         Width           =   1440
      End
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Log-normal"
         Height          =   240
         Index           =   4
         Left            =   120
         TabIndex        =   7
         TabStop         =   0   'False
         Top             =   1200
         Width           =   1215
      End
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Rosin_Rammler"
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   6
         TabStop         =   0   'False
         Top             =   960
         Width           =   1455
      End
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Log-log"
         Height          =   240
         Index           =   2
         Left            =   120
         TabIndex        =   5
         TabStop         =   0   'False
         Top             =   720
         Width           =   975
      End
      Begin VB.OptionButton OptCoord 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Linear-linear"
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   3
         TabStop         =   0   'False
         Top             =   240
         Width           =   1215
      End
   End
   Begin VB.CommandButton CmdView 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "View graph"
      Enabled         =   0   'False
      Height          =   585
      Left            =   8160
      TabIndex        =   15
      Top             =   3960
      Width           =   1680
   End
   Begin VB.CommandButton CmdEditData 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "Edit data"
      Enabled         =   0   'False
      Height          =   465
      Left            =   3720
      TabIndex        =   14
      ToolTipText     =   "Edit the selected data set"
      Top             =   3360
      Width           =   1680
   End
   Begin VB.CommandButton CmdEnterData 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Enter data"
      Height          =   465
      Left            =   1920
      TabIndex        =   13
      ToolTipText     =   "Manual data entry"
      Top             =   3360
      Width           =   1560
   End
   Begin VB.CommandButton CmdGetData 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Get data from file"
      Height          =   465
      Left            =   240
      TabIndex        =   0
      Top             =   3360
      Width           =   1440
   End
   Begin VB.ListBox GraphList 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      ForeColor       =   &H00000000&
      Height          =   1590
      Left            =   5520
      TabIndex        =   2
      ToolTipText     =   "Double click to delete"
      Top             =   600
      Width           =   855
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   8520
      Top             =   2400
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
      Flags           =   6
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Truncation size"
      Height          =   255
      Left            =   240
      TabIndex        =   29
      Top             =   5160
      Width           =   1455
   End
   Begin VB.Label LblStreams 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Select data sets"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   0
      TabIndex        =   19
      Top             =   360
      Width           =   5400
   End
   Begin VB.Label LblGraph 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Graph"
      ForeColor       =   &H00000000&
      Height          =   240
      Left            =   5520
      TabIndex        =   20
      Top             =   360
      Width           =   855
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuReadData 
         Caption         =   "Read data from file"
      End
      Begin VB.Menu MnuSaveData 
         Caption         =   "Save data"
      End
      Begin VB.Menu MnuSaveDataAS 
         Caption         =   "Save data as..."
      End
      Begin VB.Menu MnuExit 
         Caption         =   "Exit"
      End
   End
   Begin VB.Menu MnuView 
      Caption         =   "View"
      Enabled         =   0   'False
      Begin VB.Menu MnuViewGraph 
         Caption         =   "View the Graph"
      End
   End
   Begin VB.Menu MnuEdit 
      Caption         =   "Edit"
      Begin VB.Menu MnuEnterData 
         Caption         =   "Enter data"
      End
      Begin VB.Menu MnuEditData 
         Caption         =   "Edit data"
      End
   End
End
Attribute VB_Name = "Sizgraph"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub CmdEditData_Click()
'******************************
  If StreamList.ListIndex = -1 Then
    MsgBox "No data set has been selected", 48, "WARNING"
  Else
    CurrentIndex = Val(StreamList.text)
    StreamList.RemoveItem StreamList.ListIndex
    PSD.Show
  End If
End Sub

Private Sub CmdEnterData_Click()
'*******************************
  CurrentIndex = 0
  PSD.Show
End Sub

Private Sub cmdGetData_Click()
'*****************************
  Call MnuReadData_Click
  Exit Sub
End Sub

Private Sub cmdGetDatafromSprdSht_Click()
'****************************************
DataGrid.Show
End Sub

Private Sub cmdRemoveDataSet_Click()
'***********************************
'Remove a single set of data
On Error GoTo ErrHandler
StreamList.RemoveItem StreamList.ListIndex
GraphList.Clear
Exit Sub

ErrHandler:
Exit Sub
End Sub

Private Sub CmdReturn_Click()
'****************************
  Sizgraph.Hide
End Sub

Private Sub CmdSaveDataAs_Click()
'********************************
  'Open file  to save current data for size distributions
  Call MnuSaveDataAS_Click
End Sub

Private Sub CmdSaveData_Click()
'******************************
  Call MnuSaveData_Click
  Exit Sub
  
End Sub

Private Sub CmdView_Click()
'**************************
  Dim resp As Long, TestFile As String
  On Error GoTo ErrHandler
  If ChkTruncated = vbChecked Then
    If Val(Parameter.text) <= 0 Then
      MsgBox "Truncation size must be specified", 16, "ERROR"
      Parameter.SetFocus
      Exit Sub
    End If
  End If
  If Me.GraphList.ListCount = 0 Then
    MsgBox "No data sets have been selected for plotting", vbExclamation, "ERROR"
    Exit Sub
  Else
    If SETUP_GRAPH() = 0 Then Exit Sub
    Call PlotPSDGraph
  End If
  Exit Sub
ErrHandler:
    MsgBox "An error has occured when attempting to plot the requested graph", 16, "ERROR"
  Exit Sub
End Sub

Private Sub Form_Load()
  On Error GoTo ErrHandler
  
  Sizgraph.Top = 0
  Sizgraph.Left = 0
  'Sizgraph.Width = MDIPSD.ScaleWidth
  Exit Sub

ErrHandler:
 Exit Sub
End Sub

Private Sub GraphList_DblClick()
  GraphList.RemoveItem GraphList.ListIndex
  If GraphList.ListCount = 0 Then
    CmdView.Enabled = 0
    MnuView.Enabled = False
  End If
End Sub

Private Sub MnuEditData_Click()
'******************************
 If StreamList.ListIndex = -1 Then
    MsgBox "No data set has been selected", 48, "WARNING"
  Else
    CurrentIndex = Val(StreamList.text)
    StreamList.RemoveItem StreamList.ListIndex
    PSD.Show
  End If
End Sub

Private Sub MnuEnterData_Click()
'*******************************
  CurrentIndex = 0
  PSD.Show
End Sub

Private Sub MnuExit_Click()
'**************************
Call CmdReturn_Click
End Sub

Private Sub MnuReadData_Click()
'*****************************
  Dim N As Integer, M As Integer, N_DATA_SETS As Integer
  Dim ID As Integer, Index As Integer, SizeIndex As Integer
  Dim NDC As Integer
  Dim SizeFactor As Single, CumFactor As Single
  Dim FileName As String, SET_NAME As String
  'Open file  to get current data for size distributions
  On Error GoTo ErrHandler
  'Set filters
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|All files(*.*)|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.ShowOpen
  'CommonDialog1.Flags = cdlOFNHideReadOnlyFlag
  FileName = CommonDialog1.FileName
  Open FileName For Input As #35
  For N = 1 To 30
    CURRODATA(N).ID = 0
  Next N
  Line Input #35, WSTRING
  JOBNAME = Mid$(WSTRING, 1, 8)
  If JOBNAME = "PSD_PLOT" Then
    Input #35, N_DATA_SETS
    If N_DATA_SETS > 0 Then
      CmdEditData.Enabled = -1
      CmdSaveData.Enabled = -1
      CmdSaveDataAs.Enabled = -1
    End If
    For N = NumberOfDataSets + 1 To NumberOfDataSets + N_DATA_SETS
      Input #35, ID
    Next N
    Input #35, SizeIndex
    If SizeIndex = 6 Then OptSize(0).Value = -1
    If SizeIndex = 3 Then OptSize(1).Value = -1
    If SizeIndex = 2 Then OptSize(2).Value = -1
    If SizeIndex = 0 Then OptSize(3).Value = -1
    Input #35, Index
    OptCoord(Index - 1).Value = -1
    For N = NumberOfDataSets + 1 To NumberOfDataSets + N_DATA_SETS
      If N > 30 Then
        MsgBox "Too many data sets", 0, "WARNING"
        Exit For
      End If
      WSTRING = Input$(10, #35)
      If WSTRING = "Data set  " Then
        Input #35, ID
        Input #35, NDC
        Line Input #35, SET_NAME
        CURRODATA(N).ID = Val(StreamList.ListCount) + 1
        CURRODATA(N).SET_NAME = SET_NAME
        WSTRING = Str$(CURRODATA(N).ID) + " " + SET_NAME
        StreamList.AddItem WSTRING
        For M = 1 To NDC
          Input #35, CURRODATA(N).CUMSIZE(M), CURRODATA(N).fraction(M)
          CURRODATA(N).CUMSIZE(M) = CURRODATA(N).CUMSIZE(M)
        Next M
        CURRODATA(N).NDC = NDC
        If CURRODATA(N).fraction(1) > 1 Then
          For M = 1 To NDC
            CURRODATA(N).fraction(M) = CURRODATA(N).fraction(M) * 0.01
          Next M
        End If
      End If
    Next N
  Else
    MsgBox "Incorrect file type", 16, "ERROR"
  End If
  Close #35
  NumberOfDataSets = NumberOfDataSets + N_DATA_SETS
  Exit Sub
  
ErrHandler:
  Exit Sub

End Sub

Private Sub MnuSaveData_Click()
'******************************
  Dim FileName As String
  On Error GoTo ErrHandler
  'Open a file to save data
  If FileName = "" Then
   'Open file  to save current data for size distributions
    'Set filters
    CommonDialog1.Filter = "Data files (*.dat)|*.dat|All files(*.*)|*.*"
    CommonDialog1.FilterIndex = 1
    'CommonDialog1.Flags = cdlOFNHideReadOnly + cdlOFNOverwritePrompt
    CommonDialog1.ShowSave
    FileName = CommonDialog1.FileName
  End If
  Open FileName For Output As #32
  Print #32, "PSD_PLOT"
  Call SAVE_DATA
  Exit Sub
  
ErrHandler:
  Exit Sub
End Sub

Private Sub MnuSaveDataAS_Click()
'********************************
  Dim FileName As String
  'Open file  to get current data for size distributions
  On Error GoTo ErrHandler
  'Set filters
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|All files(*.*)|*.*"
  CommonDialog1.FilterIndex = 1
  'CommonDialog1.Flags = cdlOFNHideReadOnly + cdlOFNOverwritePrompt
  CommonDialog1.ShowSave
  FileName = CommonDialog1.FileName
  Open FileName For Output As #32
  Print #32, "PSD_PLOT"
  Call SAVE_DATA
  Exit Sub
ErrHandler:
  Exit Sub
End Sub

Private Sub MnuViewGraph_Click()
'*******************************
  Call CmdView_Click
End Sub

Private Function SETUP_GRAPH() As Integer
'****************************************
Dim I As Integer, J As Integer, N As Integer
Dim MaxSize As Single, Factor As Single, NDC As Integer

'Fill the setupdata variable
  SetupData.Number = GraphList.ListCount
  For I = 1 To GraphList.ListCount
    SetupData.GraphList(I) = Val(GraphList.List(I - 1))
  Next I
  If OptSize(0).Value Then
    SetupData.SizeCode = 6
    Factor = 0.000001
  ElseIf OptSize(1).Value Then
    SetupData.SizeCode = 3
    Factor = 0.001
  ElseIf OptSize(2).Value Then
    SetupData.SizeCode = 2
    Factor = 0.01
  ElseIf OptSize(3).Value Then
    SetupData.SizeCode = 0
    Factor = 1
  End If
  If OptCoord(0).Value Then SetupData.CoordinateCode = 1
  If OptCoord(1).Value Then SetupData.CoordinateCode = 2
  If OptCoord(2).Value Then SetupData.CoordinateCode = 3
  If OptCoord(3).Value Then SetupData.CoordinateCode = 4
  If OptCoord(4).Value Then SetupData.CoordinateCode = 5
  If OptCoord(5).Value Then SetupData.CoordinateCode = 6
  
  If OptNone.Value Then SetupData.LineCode = 0
  If OptStraightLines.Value Then SetupData.LineCode = 1
  If OptSmoothCurve.Value Then SetupData.LineCode = 2
  
  If ChkTruncated = vbChecked Then
    SetupData.TruncationCode = 1
    SetupData.TruncationSize = Val(Parameter.text)
  Else
    SetupData.TruncationCode = 0
  End If

  MaxSize = 0#
  For J = 1 To GraphList.ListCount
    N = Val(GraphList.List(J - 1))
    For I = 1 To CURRODATA(N).NDC
      If MaxSize < CURRODATA(N).CUMSIZE(I) Then
        MaxSize = CURRODATA(N).CUMSIZE(I)
      End If
    Next I
    SetupData.GraphData(J) = CURRODATA(N)
  Next J
  SETUP_GRAPH = -1
  If ChkTruncated = vbChecked Then
    If Factor * SetupData.TruncationSize <= MaxSize Then
        MsgBox "Truncation size must be larger that the largest size in the data", vbCritical, "ERROR"
        SETUP_GRAPH = 0
        Exit Function
    Else
      SETUP_GRAPH = -1
    End If
  End If
End Function

Private Sub SAVE_DATA()
'**********************
Dim I As Integer, J As Integer, N As Integer
  Print #32, StreamList.ListCount
  For I = 1 To StreamList.ListCount
    Print #32, Val(StreamList.List(I - 1));
  Next I
  Print #32,
  If OptSize(0).Value = -1 Then Print #32, " 6"
  If OptSize(1).Value = -1 Then Print #32, " 3"
  If OptSize(2).Value = -1 Then Print #32, " 2"
  If OptSize(3).Value = -1 Then Print #32, " 0"
  If OptCoord(0).Value = -1 Then Print #32, " 1"
  If OptCoord(1).Value = -1 Then Print #32, " 2"
  If OptCoord(2).Value = -1 Then Print #32, " 3"
  If OptCoord(3).Value = -1 Then Print #32, " 4"
  If OptCoord(4).Value = -1 Then Print #32, " 5"
  If OptCoord(5).Value = -1 Then Print #32, " 6"
  For J = 1 To StreamList.ListCount
    N = Val(StreamList.List(J - 1))
    Print #32, "Data set  ";
    Print #32, Format(CURRODATA(N).ID, "0000");
    Print #32, " ";
    Print #32, Format(CURRODATA(N).NDC, "0000");
    Print #32, " " + CURRODATA(N).SET_NAME
    For I = 1 To CURRODATA(N).NDC
      Print #32, Format$(CURRODATA(N).CUMSIZE(I), "#.0000E+#"),
      Print #32, Format$(CURRODATA(N).fraction(I), "#.0000E+#")
    Next I
  Next J
  Close #32
End Sub

Private Sub StreamList_DblClick()
'********************************
Dim NSTR As Integer
  NSTR = Val(StreamList.text)
  GraphList.AddItem Str$(NSTR)
  MnuView.Enabled = True
  CmdView.Enabled = -1
End Sub
