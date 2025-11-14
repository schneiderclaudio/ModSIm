VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form BrFuncRegression 
   Caption         =   "Specify data for breakage function"
   ClientHeight    =   7110
   ClientLeft      =   60
   ClientTop       =   345
   ClientWidth     =   9390
   LinkTopic       =   "Form1"
   ScaleHeight     =   7110
   ScaleWidth      =   9390
   StartUpPosition =   3  'Windows Default
   Begin VB.Frame Frame2 
      Caption         =   "Parameter estimates"
      Height          =   1815
      Left            =   2280
      TabIndex        =   68
      Top             =   3840
      Width           =   6975
      Begin VB.TextBox txtSumOfSquares 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Left            =   1440
         TabIndex        =   82
         Top             =   1440
         Width           =   1215
      End
      Begin VB.TextBox txtEstimate 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   0
         Left            =   1320
         TabIndex        =   76
         Top             =   720
         Width           =   1575
      End
      Begin VB.TextBox txtEstimate 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   1
         Left            =   1320
         TabIndex        =   75
         Top             =   960
         Width           =   1575
      End
      Begin VB.TextBox txtLowerConfLim 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   0
         Left            =   3240
         TabIndex        =   74
         Top             =   720
         Width           =   1575
      End
      Begin VB.TextBox txtLowerConfLim 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   1
         Left            =   3240
         TabIndex        =   73
         Top             =   960
         Width           =   1575
      End
      Begin VB.TextBox txtUpperConfLim 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   0
         Left            =   5160
         TabIndex        =   72
         Top             =   720
         Width           =   1575
      End
      Begin VB.TextBox txtUpperConfLim 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   1
         Left            =   5160
         TabIndex        =   71
         Top             =   960
         Width           =   1575
      End
      Begin VB.TextBox txtParameterNumber 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   0
         Left            =   240
         TabIndex        =   70
         Top             =   720
         Width           =   800
      End
      Begin VB.TextBox txtParameterNumber 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   1
         Left            =   240
         TabIndex        =   69
         Top             =   960
         Width           =   800
      End
      Begin VB.Label Label8 
         Caption         =   "Sum of squares"
         Height          =   255
         Left            =   240
         TabIndex        =   81
         Top             =   1440
         Width           =   1095
      End
      Begin VB.Label lblEstimate 
         Caption         =   "Least squares estimate"
         Height          =   255
         Left            =   1320
         TabIndex        =   80
         Top             =   360
         Width           =   1695
      End
      Begin VB.Label Label7 
         Caption         =   "Lower confidence limit"
         Height          =   255
         Left            =   3240
         TabIndex        =   79
         Top             =   360
         Width           =   1695
      End
      Begin VB.Label Label6 
         Caption         =   "Upper confidence limit"
         Height          =   255
         Left            =   5160
         TabIndex        =   78
         Top             =   360
         Width           =   1695
      End
      Begin VB.Label Label5 
         Alignment       =   2  'Center
         Caption         =   "Parameter"
         Height          =   255
         Left            =   240
         TabIndex        =   77
         Top             =   360
         Width           =   855
      End
   End
   Begin VB.CommandButton cmdPlotGraph 
      Caption         =   "Plot breakage function"
      Height          =   495
      Left            =   7800
      TabIndex        =   67
      Top             =   2040
      Width           =   1455
   End
   Begin VB.CommandButton cmdClose 
      Caption         =   "Close"
      Height          =   495
      Left            =   7800
      TabIndex        =   66
      Top             =   3240
      Width           =   1455
   End
   Begin MSComDlg.CommonDialog CommonDialog1 
      Left            =   8640
      Top             =   5880
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.TextBox txtDataSetID 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   1800
      TabIndex        =   1
      Top             =   120
      Width           =   7455
   End
   Begin VB.CommandButton cmdSaveData 
      Caption         =   "Save data to file"
      Height          =   495
      Left            =   7800
      TabIndex        =   64
      Top             =   1440
      Width           =   1455
   End
   Begin VB.CommandButton cmdGetData 
      Caption         =   "Get data from file"
      Height          =   495
      Left            =   7800
      TabIndex        =   63
      Top             =   840
      Width           =   1455
   End
   Begin VB.Frame Frame1 
      Caption         =   "Model"
      Height          =   1335
      Left            =   5280
      TabIndex        =   59
      Top             =   1080
      Width           =   2055
      Begin VB.OptionButton optCoordSystem 
         Caption         =   "Log-Normal"
         Height          =   255
         Index           =   2
         Left            =   240
         TabIndex        =   62
         Top             =   960
         Width           =   1455
      End
      Begin VB.OptionButton optCoordSystem 
         Caption         =   "Logistic"
         Height          =   255
         Index           =   1
         Left            =   240
         TabIndex        =   61
         Top             =   600
         Width           =   1455
      End
      Begin VB.OptionButton optCoordSystem 
         Caption         =   "Rosin-Rammler"
         Height          =   255
         Index           =   0
         Left            =   240
         TabIndex        =   60
         Top             =   240
         Value           =   -1  'True
         Width           =   1455
      End
   End
   Begin VB.ComboBox cboConfLevel 
      Height          =   315
      Left            =   6240
      TabIndex        =   58
      Top             =   2640
      Width           =   1335
   End
   Begin VB.TextBox txtNumber 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   5880
      TabIndex        =   3
      Top             =   600
      Width           =   615
   End
   Begin VB.TextBox txtParentSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   3240
      TabIndex        =   2
      Top             =   600
      Width           =   855
   End
   Begin VB.CommandButton cmdEstimate 
      Caption         =   "Estimate parameters"
      Height          =   495
      Left            =   7800
      TabIndex        =   55
      Top             =   2640
      Width           =   1455
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   24
      Left            =   1080
      TabIndex        =   53
      Top             =   6720
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   23
      Left            =   1080
      TabIndex        =   52
      Top             =   6480
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   22
      Left            =   1080
      TabIndex        =   51
      Top             =   6240
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   21
      Left            =   1080
      TabIndex        =   50
      Top             =   6000
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   20
      Left            =   1080
      TabIndex        =   49
      Top             =   5760
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   1080
      TabIndex        =   48
      Top             =   5520
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   1080
      TabIndex        =   47
      Top             =   5280
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   1080
      TabIndex        =   46
      Top             =   5040
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   1080
      TabIndex        =   45
      Top             =   4800
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   15
      Left            =   1080
      TabIndex        =   44
      Top             =   4560
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   1080
      TabIndex        =   43
      Top             =   4320
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   1080
      TabIndex        =   42
      Top             =   4080
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   1080
      TabIndex        =   41
      Top             =   3840
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   1080
      TabIndex        =   40
      Top             =   3600
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   1080
      TabIndex        =   39
      Top             =   3360
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   1080
      TabIndex        =   38
      Top             =   3120
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   1080
      TabIndex        =   37
      Top             =   2880
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   1080
      TabIndex        =   36
      Top             =   2640
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   1080
      TabIndex        =   35
      Top             =   2400
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   1080
      TabIndex        =   34
      Top             =   2160
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   1080
      TabIndex        =   33
      Top             =   1920
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   1080
      TabIndex        =   32
      Top             =   1680
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   1080
      TabIndex        =   31
      Top             =   1440
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   1080
      TabIndex        =   30
      Top             =   1200
      Width           =   855
   End
   Begin VB.TextBox txtPercentPassing 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   1080
      TabIndex        =   29
      Top             =   960
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   24
      Left            =   120
      TabIndex        =   28
      Top             =   6720
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   23
      Left            =   120
      TabIndex        =   27
      Top             =   6480
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   22
      Left            =   120
      TabIndex        =   26
      Top             =   6240
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   21
      Left            =   120
      TabIndex        =   25
      Top             =   6000
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   20
      Left            =   120
      TabIndex        =   24
      Top             =   5760
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   19
      Left            =   120
      TabIndex        =   23
      Top             =   5520
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   18
      Left            =   120
      TabIndex        =   22
      Top             =   5280
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   17
      Left            =   120
      TabIndex        =   21
      Top             =   5040
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   16
      Left            =   120
      TabIndex        =   20
      Top             =   4800
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   15
      Left            =   120
      TabIndex        =   19
      Top             =   4560
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   14
      Left            =   120
      TabIndex        =   18
      Top             =   4320
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   13
      Left            =   120
      TabIndex        =   17
      Top             =   4080
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   12
      Left            =   120
      TabIndex        =   16
      Top             =   3840
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   11
      Left            =   120
      TabIndex        =   15
      Top             =   3600
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   10
      Left            =   120
      TabIndex        =   14
      Top             =   3360
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   9
      Left            =   120
      TabIndex        =   13
      Top             =   3120
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   8
      Left            =   120
      TabIndex        =   12
      Top             =   2880
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   120
      TabIndex        =   11
      Top             =   2640
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   6
      Left            =   120
      TabIndex        =   10
      Top             =   2400
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   5
      Left            =   120
      TabIndex        =   9
      Top             =   2160
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   120
      TabIndex        =   8
      Top             =   1920
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   120
      TabIndex        =   7
      Top             =   1680
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   120
      TabIndex        =   6
      Top             =   1440
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   120
      TabIndex        =   5
      Top             =   1200
      Width           =   855
   End
   Begin VB.TextBox txtMeshSize 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   120
      TabIndex        =   4
      Top             =   960
      Width           =   855
   End
   Begin VB.Label Label4 
      Caption         =   "Data set identification"
      Height          =   255
      Left            =   120
      TabIndex        =   65
      Top             =   120
      Width           =   1575
   End
   Begin VB.Label Label3 
      Alignment       =   1  'Right Justify
      Caption         =   "Number of sizes"
      Height          =   255
      Left            =   4560
      TabIndex        =   57
      Top             =   600
      Width           =   1215
   End
   Begin VB.Label lblParentSize 
      Alignment       =   1  'Right Justify
      Caption         =   "Parent size"
      Height          =   255
      Left            =   2280
      TabIndex        =   56
      Top             =   600
      Width           =   855
   End
   Begin VB.Label Label2 
      Alignment       =   2  'Center
      Caption         =   "% passing"
      Height          =   255
      Left            =   1080
      TabIndex        =   54
      Top             =   600
      Width           =   855
   End
   Begin VB.Label Label1 
      Alignment       =   2  'Center
      Caption         =   "Mesh size"
      Height          =   255
      Left            =   120
      TabIndex        =   0
      Top             =   600
      Width           =   855
   End
End
Attribute VB_Name = "BrFuncRegression"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub cmdClose_Click()
'***************************
Unload Me
End Sub

Private Sub cmdEstimate_Click()
'******************************
  Dim X(101) As Single, Y(101) As Single
  Dim NDC As Integer, I As Integer
  
  On Error GoTo ErrHandler
  If Val(Me.txtNumber.text) < 3 Then
    MsgBox "No data has been specified ", vbCritical, "ERROR"
    Exit Sub
  End If
  NDC = Val(Me.txtNumber.text)
  If Val(Me.cboConfLevel.text) <= 0 Or Val(Me.cboConfLevel.text) >= 100 Then
    MsgBox "Specify an appropriate confidence level", vbCritical, "ERROR"
    Me.cboConfLevel.SetFocus
    Exit Sub
  End If
  For I = 1 To 2
    Me.txtParameterNumber(I - 1).text = ""
    Me.txtEstimate(I - 1).text = ""
    Me.txtLowerConfLim(I - 1).text = ""
    Me.txtUpperConfLim(I - 1).text = ""
  Next I
  For I = 1 To NDC
    X(I) = Val(Me.txtMeshSize(I - 1).text) / Val(Me.txtParentSize.text)
    If X(I) > 1# Then
      MsgBox "Mesh size cannot be greater than parent size"
      Exit Sub
    End If
    Y(I) = 0.01 * Val(Me.txtPercentPassing(I - 1).text)
  Next I
  If optCoordSystem(0).Value Then
    REGModel = "RR-BrFunc"
  ElseIf optCoordSystem(1).Value Then
    REGModel = "LogisticBrFunc"
  ElseIf optCoordSystem(2).Value Then
    REGModel = "LognormalBrFunc"
  End If
  If CHPAGE() = 0 Then Exit Sub
  Call BBOX(100, 100, 600, 500)
  Call LGAXIS(0.01, 1#, 1, "Y", "Y")
  Call MGLABAX("Relative particle size  D/d`p`'", 13, 1)
  Call LGAXIS(0.001, 1#, 2, "Y", "Y")
  Call MGLABAX("Breakage function", 13, 2)
  Call DISPNT(X, Y, NDC, 1, "ALN", "ALN")
  
  Call MGMOVE(100, 650)
  Call MGLABEL(Me.txtDataSetID)
  Call GROUTRightButtonEvent
  FirstTime = True
  MsgBox "Right click on the approximate t10 point"
  Exit Sub
ErrHandler:
  MsgBox "Error plotting data", vbCritical, "ERROR"
  Exit Sub
End Sub

Private Sub cmdGetData_Click()
'*****************************
  Dim I As Integer, N As Integer
  Dim SavedFile As String
  Dim WSTRING As String, WS1 As Single, WS2 As Single
  
  On Error GoTo ErrHandler
  CommonDialog1.Filter = "Text files (*.txt)|*.txt|Data files (*.dat)|*.dat|All files (*.*)|*.*"
  CommonDialog1.FilterIndex = 1
  CommonDialog1.ShowOpen
  SavedFile = CommonDialog1.FileName
  Open SavedFile For Input As #32
  Line Input #32, WSTRING
  Me.txtDataSetID.text = WSTRING
  Input #32, WS1
  Me.txtParentSize.text = CStr(WS1)
  Input #32, N
  Me.txtNumber.text = CStr(N)
  For I = 1 To N
    Input #32, WS1, WS2
    Me.txtMeshSize(I - 1).text = CStr(WS1)
    Me.txtMeshSize(I - 1).Visible = True
    Me.txtPercentPassing(I - 1).text = CStr(WS2)
    Me.txtPercentPassing(I - 1).Visible = True
  Next I
  For I = N To 24
    Me.txtMeshSize(I).Visible = False
    Me.txtPercentPassing(I).Visible = False
  Next I
  Close (32)
  Exit Sub
ErrHandler:
  MsgBox "Error reading data file", vbCritical, "ERROR"
  Exit Sub

End Sub

Private Sub cmdPlotGraph_Click()
'*******************************
  Dim X(101) As Single, Y(101) As Single
  Dim NDC As Integer, I As Integer
  NDC = Val(Me.txtNumber.text)
  For I = 1 To NDC
    X(I) = Val(Me.txtMeshSize(I - 1).text) / Val(Me.txtParentSize.text)
    Y(I) = 0.01 * Val(Me.txtPercentPassing(I - 1).text)
  Next I
  If CHPAGE() = 0 Then Exit Sub
  Call BBOX(100, 100, 600, 500)
  Call LGAXIS(0.01, 1#, 1, "Y", "Y")
  Call MGLABAX("Relative particle size  D/d`p`'", 13, 1)
  Call LGAXIS(0.001, 1#, 2, "Y", "Y")
  Call MGLABAX("Breakage function", 13, 2)
  Call DISPNT(X, Y, NDC, 1, "ALN", "ALN")
  
  Call MGMOVE(100, 650)
  Call MGLABEL(Me.txtDataSetID)
  Call GROUT
End Sub

Private Sub CmdSaveData_Click()
'******************************
  Dim I As Integer, N As Integer
  Dim response As Long
  Dim FileName As String
  
  On Error GoTo ErrHandler
  CommonDialog1.Filter = "Data files (*.dat)|*.dat|Text files (*.txt)|*.txt"
  CommonDialog1.Flags = 4
  CommonDialog1.FilterIndex = 2
  CommonDialog1.ShowSave
  FileName = Trim(CommonDialog1.FileName)
  'Strip off any file extensions
  I = InStr(CommonDialog1.FileName, ".")
  FileName = Trim(Mid(CommonDialog1.FileName, 1, I - 1))
  'Add the chosen extension
  If CommonDialog1.FilterIndex = 1 Then
    FileName = FileName & ".dat"
  Else
    FileName = FileName & ".txt"
  End If
  'Check if file exists
  If Dir(FileName) <> "" Then
    response = MsgBox("File " & FileName & " already exists. OverWrite?", vbOKCancel, "WARNING")
    If response = vbCancel Then Exit Sub
  End If
  Open FileName For Output As #31
  Print #31, Me.txtDataSetID.text
  Print #31, Me.txtParentSize.text
  Print #31, Me.txtNumber.text
  For I = 1 To Val(Me.txtNumber.text)
    Print #31, Me.txtMeshSize(I - 1).text, Me.txtPercentPassing(I - 1).text
  Next I
  Close (31)
  Exit Sub
ErrHandler:
  MsgBox "Error when saving file"
  Exit Sub
End Sub

Private Sub Form_Load()
'**********************
  Me.cboConfLevel.AddItem "90%"
  Me.cboConfLevel.AddItem "95%"
  Me.cboConfLevel.AddItem "99%"
  Me.cboConfLevel.ListIndex = 1
End Sub

Private Sub txtMeshSize_KeyPress(Index As Integer, keyAscii As Integer)
'**********************************************************************
  If keyAscii = 13 Then
    If Index < Val(Me.txtNumber.text) - 1 Then
      Me.txtMeshSize(Index + 1).text = ""
      Me.txtMeshSize(Index + 1).SetFocus
      keyAscii = 0
    End If
  End If
End Sub

Private Sub txtNumber_LostFocus()
'************************************
Dim I As Integer
  For I = 0 To Val(Me.txtNumber.text) - 1
    Me.txtPercentPassing(I).Visible = True
    Me.txtMeshSize(I).Visible = True
  Next I
  For I = Val(Me.txtNumber.text) To 24
    Me.txtPercentPassing(I).Visible = False
    Me.txtMeshSize(I).Visible = False
  Next I
End Sub


Private Sub txtPercentPassing_KeyPress(Index As Integer, keyAscii As Integer)
'****************************************************************************
  If keyAscii = 13 Then
    If Index < Val(Me.txtNumber.text) - 1 Then
      Me.txtPercentPassing(Index + 1).text = ""
      Me.txtPercentPassing(Index + 1).SetFocus
      keyAscii = 0
    End If
  End If
End Sub

Private Sub txtPercentPassing_LostFocus(Index As Integer)
'********************************************************
  Dim I As Integer
  For I = 0 To 1
    Me.txtEstimate(I).text = ""
    Me.txtLowerConfLim(I).text = ""
    Me.txtUpperConfLim(I).text = ""
  Next I
End Sub
