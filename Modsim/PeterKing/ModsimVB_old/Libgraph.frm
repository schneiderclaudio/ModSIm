VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form libgraph 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Plot liberation distributions"
   ClientHeight    =   5460
   ClientLeft      =   300
   ClientTop       =   1650
   ClientWidth     =   8130
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H0000FFFF&
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MDIChild        =   -1  'True
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5460
   ScaleWidth      =   8130
   Begin VB.ListBox StreamList 
      Appearance      =   0  'Flat
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
      Height          =   4230
      Left            =   120
      TabIndex        =   0
      ToolTipText     =   "Double click to select"
      Top             =   720
      Width           =   5280
   End
   Begin VB.CommandButton CmdView 
      Appearance      =   0  'Flat
      BackColor       =   &H00FFFFFF&
      Caption         =   "View graph"
      Enabled         =   0   'False
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   615
      Left            =   5520
      TabIndex        =   3
      Top             =   3120
      Width           =   2535
   End
   Begin VB.CheckBox ChkExp 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Show experimental data"
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
      Left            =   5520
      TabIndex        =   2
      Top             =   2640
      Width           =   2655
   End
   Begin VB.CommandButton CmdReturn 
      Appearance      =   0  'Flat
      BackColor       =   &H0000FFFF&
      Caption         =   "Close"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   735
      Left            =   6120
      TabIndex        =   4
      Top             =   4200
      Width           =   1935
   End
   Begin VB.ListBox GraphList 
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
      Height          =   1740
      Left            =   5520
      TabIndex        =   1
      ToolTipText     =   "Double click to remove"
      Top             =   720
      Width           =   975
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   7200
      Top             =   720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      CancelError     =   -1  'True
   End
   Begin VB.Label Instruction 
      Appearance      =   0  'Flat
      BackColor       =   &H0080FFFF&
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
      Left            =   240
      TabIndex        =   5
      Top             =   0
      Visible         =   0   'False
      Width           =   7800
   End
   Begin VB.Label LblGraph 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Graph"
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
      Left            =   5520
      TabIndex        =   7
      Top             =   360
      Width           =   840
   End
   Begin VB.Label LblStreams 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Select plant streams to plot"
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
      Left            =   0
      TabIndex        =   6
      Top             =   360
      Width           =   5400
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "libgraph"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

Option Explicit
Private CURRDATA(30) As LIBDATA
Private WSTRING As String
Private STREAM_TYPE As String * 1
Private STREAM_NAME As String * 30

Private Sub CmdReturn_Click()
 Unload libgraph
End Sub

Private Sub CmdView_Click()
'**************************
  Dim RetVal As Double
  On Error GoTo ErrHandler
  Call LibgraphPlot
  Exit Sub
  
ErrHandler:
  MsgBox "Error occurred when running the graph plotting program", 16, "ERROR"
  Exit Sub

End Sub

Private Sub Form_Load()
'*********************
  Dim N As Integer, M As Integer
  Dim RetVal As Integer
  Dim NSTR As Integer, ID As Integer
  Dim N_DATA_STREAMS As Integer
  Dim Nstream As Integer
  Dim STREAM_ID As Integer
  Dim From_unit As Integer, To_unit As Integer
  
  On Error GoTo ErrHandler
  
  MODSIM.LibGraphIsActive = True
  
  'Open file LIBCOMP.OUT to get current data for liberation spectra
  Open RTrim(JobPath) & "LIBCOMP.OUT" For Input As 35
  For N = 1 To 30
    CURRDATA(N).ID = 0
  Next N
  Line Input #35, WSTRING
  If Mid$(WSTRING, 1, 8) <> "END OF F" Then
    N = 0
    WSTRING = Input$(10, #35)
    Do While Not EOF(35)
      If WSTRING = "END OF FIL" Then Exit Do
      If N > 30 Then
        MsgBox "Too many data sets in file LIBCOMP.DAT", 0, "WARNING"
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
        If WSTRING = "Lib dist  " Then
          Input #35, CURRDATA(N).NGC
          For M = 1 To CURRDATA(N).NGC
            Input #35, CURRDATA(N).Fraction(M)
          Next M
        End If
      End If
      WSTRING = Input$(10, #35)
    Loop
    N_DATA_STREAMS = N
  End If
  Close #35

  'Open file to get the stream data coming from the flowsheet
  Open RTrim(JobPath) & "SYSDATA.out" For Input As #35
  Line Input #35, WSTRING
  Input #35, Nstream
  For N = 1 To Nstream
    Input #35, Stream
    Input #35, STREAM_ID
    STREAM_TYPE = Input$(1, #35)
    Input #35, From_unit
    Input #35, To_unit
    WSTRING = Str$(Stream)
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
  Close #35
  Exit Sub
  
ErrHandler:
  MsgBox " Could not load liberation data", vbExclamation, "WARNING"
  Exit Sub
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
  MODSIM.LibGraphIsActive = False
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
Unload libgraph
End Sub

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

