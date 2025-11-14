VERSION 5.00
Begin VB.Form KYNC 
   Appearance      =   0  'Flat
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameters for model KYNC"
   ClientHeight    =   4815
   ClientLeft      =   1230
   ClientTop       =   2055
   ClientWidth     =   9240
   BeginProperty Font 
      Name            =   "MS Sans Serif"
      Size            =   8.25
      Charset         =   0
      Weight          =   700
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   ForeColor       =   &H00000000&
   Icon            =   "Kync.frx":0000
   LinkMode        =   1  'Source
   MaxButton       =   0   'False
   MinButton       =   0   'False
   PaletteMode     =   1  'UseZOrder
   ScaleHeight     =   4815
   ScaleWidth      =   9240
   Begin VB.OptionButton OptSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "ft"
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
      Index           =   1
      Left            =   6600
      TabIndex        =   38
      Tag             =   "Size"
      Top             =   480
      Width           =   495
   End
   Begin VB.OptionButton OptSize 
      BackColor       =   &H00C0C0C0&
      Caption         =   "m"
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
      Left            =   5880
      TabIndex        =   37
      Tag             =   "Size"
      Top             =   480
      Value           =   -1  'True
      Width           =   495
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
      Height          =   225
      Index           =   13
      Left            =   1080
      TabIndex        =   33
      Top             =   3720
      Width           =   1600
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
      Height          =   225
      Index           =   14
      Left            =   1080
      TabIndex        =   32
      Top             =   3960
      Width           =   1600
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
      Height          =   225
      Index           =   12
      Left            =   1080
      TabIndex        =   29
      Top             =   3480
      Width           =   1600
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
      Height          =   225
      Index           =   11
      Left            =   1080
      TabIndex        =   28
      Top             =   3240
      Width           =   1600
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
      Height          =   225
      Index           =   10
      Left            =   1080
      TabIndex        =   25
      Top             =   3000
      Width           =   1600
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
      Height          =   225
      Index           =   9
      Left            =   1080
      TabIndex        =   24
      Top             =   2760
      Width           =   1600
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
      Height          =   225
      Index           =   8
      Left            =   1080
      TabIndex        =   21
      Top             =   2520
      Width           =   1600
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
      Height          =   225
      Index           =   7
      Left            =   1080
      TabIndex        =   20
      Top             =   2280
      Width           =   1600
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
      Height          =   225
      Index           =   6
      Left            =   1080
      TabIndex        =   17
      Top             =   2040
      Width           =   1600
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
      Height          =   225
      Index           =   5
      Left            =   1080
      TabIndex        =   16
      Top             =   1800
      Width           =   1600
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
      Height          =   225
      Index           =   4
      Left            =   1080
      TabIndex        =   13
      Top             =   1560
      Width           =   1600
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
      Height          =   225
      Index           =   3
      Left            =   1080
      TabIndex        =   12
      Top             =   1320
      Width           =   1600
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
      Height          =   225
      Index           =   2
      Left            =   4560
      TabIndex        =   10
      Top             =   960
      Width           =   1215
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
      Height          =   225
      Index           =   1
      Left            =   4560
      TabIndex        =   7
      Top             =   720
      Width           =   1215
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
      Height          =   225
      Index           =   0
      Left            =   4560
      TabIndex        =   1
      Top             =   480
      Width           =   1215
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
      Left            =   6360
      TabIndex        =   5
      Top             =   3720
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
      Left            =   7680
      TabIndex        =   4
      Top             =   3720
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
      ForeColor       =   &H00000000&
      Height          =   1095
      Left            =   7440
      TabIndex        =   0
      Top             =   360
      Width           =   1695
      Begin VB.OptionButton OptNew 
         BackColor       =   &H00C0C0C0&
         Caption         =   "New"
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
         TabIndex        =   6
         TabStop         =   0   'False
         Tag             =   "New"
         Top             =   240
         Value           =   -1  'True
         Width           =   1440
      End
      Begin VB.OptionButton OptCurrent 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Current data"
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
         TabIndex        =   2
         TabStop         =   0   'False
         Tag             =   "Current"
         Top             =   480
         Width           =   1440
      End
      Begin VB.OptionButton OptDefault 
         BackColor       =   &H00C0C0C0&
         Caption         =   "Default"
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
         TabIndex        =   3
         Tag             =   "Default"
         Top             =   720
         Width           =   975
      End
   End
   Begin VB.Label Instruction 
      BackColor       =   &H0080FFFF&
      Height          =   255
      Left            =   120
      TabIndex        =   39
      Top             =   0
      Width           =   9015
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "mm/s"
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
      Left            =   5880
      TabIndex        =   36
      Top             =   720
      Width           =   615
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 6"
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
      Index           =   14
      Left            =   120
      TabIndex        =   35
      Top             =   3960
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 6"
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
      Index           =   13
      Left            =   120
      TabIndex        =   34
      Top             =   3720
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 5"
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
      Index           =   12
      Left            =   120
      TabIndex        =   31
      Top             =   3480
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 5"
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
      Index           =   11
      Left            =   120
      TabIndex        =   30
      Top             =   3240
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 4"
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
      Index           =   10
      Left            =   120
      TabIndex        =   27
      Top             =   3000
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 4"
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
      Left            =   120
      TabIndex        =   26
      Top             =   2760
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 3"
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
      Left            =   120
      TabIndex        =   23
      Top             =   2520
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 3"
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
      Index           =   7
      Left            =   120
      TabIndex        =   22
      Top             =   2280
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 2"
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
      Index           =   6
      Left            =   120
      TabIndex        =   19
      Top             =   2040
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 2"
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
      Index           =   5
      Left            =   120
      TabIndex        =   18
      Top             =   1800
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Beta 1"
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
      Index           =   4
      Left            =   120
      TabIndex        =   15
      Top             =   1560
      Width           =   855
   End
   Begin VB.Label LblParameter 
      Alignment       =   1  'Right Justify
      BackColor       =   &H00C0C0C0&
      Caption         =   "Alpha 1"
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
      Index           =   3
      Left            =   120
      TabIndex        =   14
      Top             =   1320
      Width           =   855
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Number of terms in the extended Wilhelm-Naide expression"
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
      Index           =   2
      Left            =   120
      TabIndex        =   11
      Top             =   960
      Width           =   4335
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Terminal settling velocity of an isolated floc"
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
      Index           =   1
      Left            =   120
      TabIndex        =   9
      Top             =   720
      Width           =   4335
   End
   Begin VB.Label LblParameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Diameter of thickener"
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
      TabIndex        =   8
      Top             =   480
      Width           =   4335
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
         Caption         =   "Close"
      End
   End
End
Attribute VB_Name = "KYNC"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private CHANGE_FLAG As Integer
Private NEWDATA As ModelData
Private ID As Integer

Sub CmdAccept_Click()
'*********************
  Call MakeNew(CURRMODELDATA(ID))
  Unload KYNC
End Sub
Sub CmdCancel_Click()
'*********************
  Unload KYNC
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

Sub Form_Load()
'***************
  On Error GoTo ErrHandler
  Caption = Caption & " for unit " & CStr(UnitMods.Unit)

  ID = UnitMods.ID
  CHANGE_FLAG = 0
' Find the current data for this model if any
  If CURRMODELDATA(ID).Model <> "KYNC" Then
    LoadDefault
    OptCurrent.Enabled = 0
    LoadDefault
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  Else
    Call LoadNew(CURRMODELDATA(ID))
    Call MakeNew(NEWDATA)
  End If
  Exit Sub
  
ErrHandler:
  MsgBox "Error loading model parameter data", 16, "ERROR"
  Exit Sub
End Sub

Sub LoadDefault()
'****************
  Parameter(0).text = CStr(15)
  Parameter(1).text = CStr(10)
  Parameter(2).text = CStr(2)
  Parameter(3).text = CStr(0.01)
  Parameter(4).text = CStr(1.5)
  Parameter(5).text = CStr(0.00000000005)
  Parameter(6).text = CStr(5)

  OptSize(0).Value = True
End Sub

 Private Sub LoadNew(NDATA As ModelData)
'***************************************
  Dim I As Integer
  For I = 1 To NDATA.NOPAR
    Parameter(I - 1).text = CStr(NDATA.PARAM(I))
    Parameter(I - 1).Visible = True
    lblParameter(I - 1).Visible = True
  Next I
  'Display the terminal settling velocity as mm/s
  Parameter(1).text = CStr(1000 * NDATA.PARAM(2))
  For I = NDATA.NOPAR + 1 To 15
    Parameter(I - 1).Visible = False
    lblParameter(I - 1).Visible = False
  Next I
  OptSize(0).Value = True
End Sub

Private Sub MakeNew(NDATA As ModelData)
'**************************************
  Dim I As Integer
  CHANGE_FLAG = 0
  NDATA.Model = "KYNC"
  NDATA.NOPAR = 3 + 2 * Val(Parameter(2).text)
  For I = 1 To NDATA.NOPAR
    NDATA.PARAM(I) = Val(Parameter(I - 1).text)
  Next I
  'Convert the size to meters
  If OptSize(1).Value Then NDATA.PARAM(1) = NDATA.PARAM(1) * 0.3048
  'Convert the terminal settling velocity to m/s
  NDATA.PARAM(2) = 0.001 * NDATA.PARAM(2)
End Sub

Private Sub MnuAccept_Click()
  Call CmdAccept_Click
End Sub

Private Sub MnuCancel_Click()
  Call CmdCancel_Click
End Sub

Private Sub MnuPrint_Click()
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
  'Must trigger a load every time NEW is switched on except when changing a text field
  If Not TypeOf Screen.ActiveControl Is TextBox Then
    Call LoadNew(NEWDATA)
  End If
End Sub

Sub Parameter_Change(Index As Integer)
'**************************************
  If TypeOf Screen.ActiveControl Is TextBox Then
    If OptNew.Value = 0 Then OptNew.Value = -1
    CHANGE_FLAG = -1
  End If
End Sub

Private Sub Parameter_GotFocus(Index As Integer)
  If Index > 2 Then
    Instruction.Visible = True
    Instruction.Caption = "Specify the parameters in the Wilhelm-Naide equation for the settling velocity"
  End If
End Sub

Sub Parameter_LostFocus(Index As Integer)
'****************************************
  Instruction.Visible = False
  Instruction.Caption = ""
  If CHANGE_FLAG = -1 Then
    Call MakeNew(NEWDATA)
    Call LoadNew(NEWDATA)
  End If
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

Private Sub Parameter_MouseDown(Index As Integer, Button As Integer, Shift As Integer, X As Single, Y As Single)
'***************************************************************************************************************
'Select parameters for repetitive simulation
Dim I As Integer, IDIFF As Integer
On Error GoTo ErrHandler

If Shift = 2 And ProfessionalVersion Then
  Call MakeNew(NEWDATA)
  Repeat.TxtParameterName = Me.lblParameter(Index).Caption
  Repeat.TxtValue(0) = Me.Parameter(Index).text
  Repeat.TxtValue(1) = ""
  Repeat.TxtValue(2) = ""
  IDIFF = 1
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


