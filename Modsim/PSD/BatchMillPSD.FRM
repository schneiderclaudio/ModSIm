VERSION 5.00
Begin VB.Form BatchMill 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model"
   ClientHeight    =   5565
   ClientLeft      =   225
   ClientTop       =   810
   ClientWidth     =   9045
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
   MDIChild        =   -1  'True
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   5565
   ScaleWidth      =   9045
   Begin VB.TextBox Parameter 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   7
      Left            =   1800
      TabIndex        =   30
      Top             =   3240
      Width           =   1215
   End
   Begin VB.Frame Frame4 
      Caption         =   "Parameters for the breakage function"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1215
      Left            =   240
      TabIndex        =   21
      Top             =   1920
      Width           =   3495
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   6
         Left            =   1200
         TabIndex        =   27
         Top             =   840
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   5
         Left            =   1200
         TabIndex        =   26
         Top             =   600
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   4
         Left            =   1200
         TabIndex        =   25
         Top             =   360
         Width           =   1215
      End
      Begin VB.Label Label1 
         Caption         =   "Phi"
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   24
         Top             =   840
         Width           =   855
      End
      Begin VB.Label Label1 
         Caption         =   "n2"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   23
         Top             =   600
         Width           =   855
      End
      Begin VB.Label Label1 
         Caption         =   "n1"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   22
         Top             =   360
         Width           =   855
      End
   End
   Begin VB.Frame Frame3 
      Caption         =   "Parameters for the selection function"
      BeginProperty Font 
         Name            =   "Arial"
         Size            =   8.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   1335
      Left            =   240
      TabIndex        =   12
      Top             =   360
      Width           =   3495
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   3
         Left            =   1200
         TabIndex        =   20
         Top             =   960
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   2
         Left            =   1200
         TabIndex        =   19
         Top             =   720
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   0
         Left            =   1200
         TabIndex        =   14
         Top             =   240
         Width           =   1215
      End
      Begin VB.TextBox Parameter 
         Appearance      =   0  'Flat
         BorderStyle     =   0  'None
         Height          =   225
         Index           =   1
         Left            =   1200
         TabIndex        =   13
         Top             =   480
         Width           =   1215
      End
      Begin VB.Label Label3 
         Caption         =   "mm"
         Height          =   255
         Left            =   2640
         TabIndex        =   29
         Top             =   720
         Width           =   255
      End
      Begin VB.Label Label2 
         Caption         =   "1/min"
         Height          =   255
         Left            =   2640
         TabIndex        =   28
         Top             =   240
         Width           =   495
      End
      Begin VB.Label LblParam 
         Caption         =   "Lambda"
         Height          =   255
         Index           =   3
         Left            =   120
         TabIndex        =   18
         Top             =   960
         Width           =   1000
      End
      Begin VB.Label LblParam 
         Caption         =   "mu"
         Height          =   255
         Index           =   2
         Left            =   120
         TabIndex        =   17
         Top             =   720
         Width           =   1000
      End
      Begin VB.Label LblParam 
         Caption         =   "S1"
         Height          =   255
         Index           =   0
         Left            =   120
         TabIndex        =   16
         Top             =   240
         Width           =   1000
      End
      Begin VB.Label LblParam 
         Caption         =   "alpha"
         Height          =   255
         Index           =   1
         Left            =   120
         TabIndex        =   15
         Top             =   480
         Width           =   1000
      End
   End
   Begin VB.CommandButton cmdCancel 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Cancel"
      Height          =   600
      Left            =   6720
      TabIndex        =   10
      Top             =   4800
      Width           =   960
   End
   Begin VB.CommandButton cmdAccept 
      Appearance      =   0  'Flat
      BackColor       =   &H00C0C0C0&
      Caption         =   "Accept"
      Height          =   600
      Left            =   7920
      TabIndex        =   9
      Top             =   4800
      Width           =   960
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Data set"
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   7200
      TabIndex        =   0
      Top             =   240
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   11
         TabStop         =   0   'False
         Tag             =   "New"
         ToolTipText     =   "Display data set that you are constructing"
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
         TabIndex        =   1
         TabStop         =   0   'False
         Tag             =   "Current"
         ToolTipText     =   "Display data set used in the previous simulation"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
         ForeColor       =   &H00000000&
         Height          =   240
         Left            =   120
         TabIndex        =   2
         Tag             =   "Default"
         ToolTipText     =   "Display the default data set for this unit model"
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Units of size"
      ForeColor       =   &H00000000&
      Height          =   975
      Left            =   3720
      TabIndex        =   3
      Top             =   4440
      Visible         =   0   'False
      Width           =   2895
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "micron"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   0
         Left            =   120
         TabIndex        =   4
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   1200
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "mm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   1
         Left            =   1320
         TabIndex        =   5
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   720
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "cm"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   2
         Left            =   2160
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   240
         Width           =   615
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "inch"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   4
         Left            =   1320
         TabIndex        =   7
         TabStop         =   0   'False
         Tag             =   "Size"
         Top             =   600
         Width           =   960
      End
      Begin VB.OptionButton OptSize 
         BackColor       =   &H00C0C0C0&
         Caption         =   "m"
         ForeColor       =   &H00000000&
         Height          =   240
         Index           =   3
         Left            =   120
         TabIndex        =   8
         Tag             =   "Size"
         Top             =   600
         Value           =   -1  'True
         Width           =   600
      End
   End
   Begin VB.Label Label4 
      Caption         =   "Grinding time  min"
      Height          =   255
      Left            =   240
      TabIndex        =   31
      Top             =   3240
      Width           =   1455
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu MnuPrint 
         Caption         =   "Print"
      End
      Begin VB.Menu MnuClose 
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "BatchMill"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private LoadingForm As Boolean

Sub CmdAccept_Click()
'********************
  Call MakeNew(CURRMODELDATA(ID))
  Unload Me
  FeedIndex = Val(Sizgraph.StreamList.text)
  If FeedIndex <= 0 Then
    MsgBox "Please select the size distribution for the batch mill feed", vbCritical, "ERROR"
    Sizgraph.Show
    Exit Sub
  End If
  Call SimulateBatchMill
End Sub

Sub CmdCancel_Click()
'*********************
  Unload Me
End Sub

Sub Form_Load()
'***************
  On Error GoTo ErrHandler
  LoadingForm = True
  Caption = "Specify parameters for the batch mill"
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).MODEL <> "BATM" Then
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  LoadingForm = False
  Exit Sub
  
ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  LoadingForm = False
  Exit Sub
End Sub

Sub LoadDefault()
'*****************
  Parameter(0).text = CStr(1)
  Parameter(1).text = CStr(0.73)
  Parameter(2).text = CStr(10)
  Parameter(3).text = CStr(3)
  Parameter(4).text = CStr(0.95)
  Parameter(5).text = CStr(4.4)
  Parameter(6).text = CStr(0.65)
  Parameter(7).text = CStr(2)
  OptSize(3).Value = -1
End Sub

Private Sub LoadNew(NDATA As ModelData)
'**************************************
Dim I As Integer
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
  Next I
  OptSize(3).Value = -1
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  Dim Scalefactor As Single
  CHANGE_FLAG = 0
  NDATA.MODEL = "BATM"
  NDATA.NOPAR = 8
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  'Convert the size to meters
  Scalefactor = 1#
  If OptSize(0).Value Then Scalefactor = 0.000001
  If OptSize(1).Value Then Scalefactor = 0.001
  If OptSize(2).Value Then Scalefactor = 0.01
  If OptSize(4).Value Then Scalefactor = 0.0254
  NDATA.PARAM(1) = NDATA.PARAM(1) * Scalefactor
End Sub

Private Sub MnuAccept_Click()
'****************************
   Call CmdAccept_Click
End Sub

Private Sub mnuClose_Click()
'***************************
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
'***************************
'Print the form
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
  'Must trigger a load only when NEW is switched on with a click.
  If Screen.ActiveControl.Tag = "New" Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Sub Parameter_Change(Index As Integer)
'**************************************
  If LoadingForm Then Exit Sub
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Sub Parameter_LostFocus(Index As Integer)
'*****************************************
  If CHANGE_FLAG = -1 Then Call MakeNew(NEWDATA)
  CHANGE_FLAG = 0
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
