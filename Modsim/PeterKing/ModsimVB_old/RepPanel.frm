VERSION 5.00
Begin VB.Form RepPanel 
   BackColor       =   &H00C0C0C0&
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Control panel for repetitive simulations"
   ClientHeight    =   2085
   ClientLeft      =   150
   ClientTop       =   435
   ClientWidth     =   9765
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
   ScaleHeight     =   2085
   ScaleWidth      =   9765
   Begin VB.TextBox TxtStep 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   4
      Left            =   8520
      TabIndex        =   31
      Top             =   1440
      Width           =   855
   End
   Begin VB.TextBox TxtEndingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   4
      Left            =   7440
      TabIndex        =   30
      Top             =   1440
      Width           =   975
   End
   Begin VB.TextBox TxtstartingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   4
      Left            =   6360
      TabIndex        =   29
      Top             =   1440
      Width           =   975
   End
   Begin VB.TextBox TxtStep 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   3
      Left            =   8520
      TabIndex        =   26
      Top             =   1200
      Width           =   855
   End
   Begin VB.TextBox TxtEndingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   3
      Left            =   7440
      TabIndex        =   25
      Top             =   1200
      Width           =   975
   End
   Begin VB.TextBox TxtstartingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   3
      Left            =   6360
      TabIndex        =   24
      Top             =   1200
      Width           =   975
   End
   Begin VB.TextBox TxtStep 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   8520
      TabIndex        =   20
      Top             =   960
      Width           =   855
   End
   Begin VB.TextBox TxtEndingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   2
      Left            =   7440
      TabIndex        =   19
      Top             =   960
      Width           =   975
   End
   Begin VB.TextBox TxtstartingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   2
      Left            =   6360
      TabIndex        =   18
      Top             =   960
      Width           =   975
   End
   Begin VB.TextBox TxtStep 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   8520
      TabIndex        =   14
      Top             =   720
      Width           =   855
   End
   Begin VB.TextBox TxtEndingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   1
      Left            =   7440
      TabIndex        =   13
      Top             =   720
      Width           =   975
   End
   Begin VB.TextBox TxtstartingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   1
      Left            =   6360
      TabIndex        =   12
      Top             =   720
      Width           =   975
   End
   Begin VB.TextBox TxtStep 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   8520
      TabIndex        =   8
      Top             =   480
      Width           =   855
   End
   Begin VB.TextBox TxtEndingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   0
      Left            =   7440
      TabIndex        =   7
      Top             =   480
      Width           =   975
   End
   Begin VB.TextBox TxtstartingValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      BeginProperty Font 
         Name            =   "MS Sans Serif"
         Size            =   8.25
         Charset         =   0
         Weight          =   400
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   225
      Index           =   0
      Left            =   6360
      TabIndex        =   6
      Top             =   480
      Width           =   975
   End
   Begin VB.Label LblUnitNumber 
      BackColor       =   &H00C0C0C0&
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
      Index           =   4
      Left            =   960
      TabIndex        =   32
      Top             =   1440
      Width           =   615
   End
   Begin VB.Label LblParameterNum 
      BackColor       =   &H00C0C0C0&
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
      Index           =   4
      Left            =   1920
      TabIndex        =   28
      Top             =   1440
      Width           =   4455
   End
   Begin VB.Label LblLevel 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Level 5"
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
      Index           =   4
      Left            =   240
      TabIndex        =   27
      ToolTipText     =   "Double click to clear this level"
      Top             =   1440
      Width           =   615
   End
   Begin VB.Label LblParameterNum 
      BackColor       =   &H00C0C0C0&
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
      Index           =   3
      Left            =   1920
      TabIndex        =   23
      Top             =   1200
      Width           =   4335
   End
   Begin VB.Label LblUnitNumber 
      BackColor       =   &H00C0C0C0&
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
      Index           =   3
      Left            =   960
      TabIndex        =   22
      Top             =   1200
      Width           =   615
   End
   Begin VB.Label LblLevel 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Level 4"
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
      Index           =   3
      Left            =   240
      TabIndex        =   21
      ToolTipText     =   "Double click to clear this level"
      Top             =   1200
      Width           =   615
   End
   Begin VB.Label LblParameterNum 
      BackColor       =   &H00C0C0C0&
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
      Index           =   2
      Left            =   1920
      TabIndex        =   17
      Top             =   960
      Width           =   4335
   End
   Begin VB.Label LblUnitNumber 
      BackColor       =   &H00C0C0C0&
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
      Index           =   2
      Left            =   960
      TabIndex        =   16
      Top             =   960
      Width           =   615
   End
   Begin VB.Label LblLevel 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Level 3"
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
      Index           =   2
      Left            =   240
      TabIndex        =   15
      ToolTipText     =   "Double click to clear this level"
      Top             =   960
      Width           =   615
   End
   Begin VB.Label LblParameterNum 
      BackColor       =   &H00C0C0C0&
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
      Index           =   1
      Left            =   1920
      TabIndex        =   11
      Top             =   720
      Width           =   4335
   End
   Begin VB.Label LblUnitNumber 
      BackColor       =   &H00C0C0C0&
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
      Index           =   1
      Left            =   960
      TabIndex        =   10
      Top             =   720
      Width           =   615
   End
   Begin VB.Label LblLevel 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Level 2"
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
      Index           =   1
      Left            =   240
      TabIndex        =   9
      ToolTipText     =   "Double click to clear level"
      Top             =   720
      Width           =   615
   End
   Begin VB.Label Label1 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Starting value   Ending value   Step"
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
      Left            =   6360
      TabIndex        =   5
      Top             =   120
      Width           =   2655
   End
   Begin VB.Label LblParameterNum 
      BackColor       =   &H00C0C0C0&
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
      Left            =   1920
      TabIndex        =   4
      Top             =   480
      Width           =   4335
   End
   Begin VB.Label Lblparameter 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Parameter"
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
      Left            =   2040
      TabIndex        =   3
      Top             =   120
      Width           =   855
   End
   Begin VB.Label LblUnitNumber 
      BackColor       =   &H00C0C0C0&
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
      Left            =   960
      TabIndex        =   2
      Top             =   480
      Width           =   615
   End
   Begin VB.Label LblUnit 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Unit number"
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
      Left            =   840
      TabIndex        =   1
      Top             =   120
      Width           =   975
   End
   Begin VB.Label LblLevel 
      BackColor       =   &H00C0C0C0&
      Caption         =   "Level 1"
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
      TabIndex        =   0
      ToolTipText     =   "Double click to clear this level"
      Top             =   480
      Width           =   615
   End
   Begin VB.Menu MnuFile 
      Caption         =   "File"
      Begin VB.Menu MnuAccept 
         Caption         =   "Accept"
      End
      Begin VB.Menu mnuClear 
         Caption         =   "Clear"
      End
      Begin VB.Menu MnuCancel 
         Caption         =   "Cancel"
      End
      Begin VB.Menu MnuPrintForm 
         Caption         =   "Print form"
      End
   End
End
Attribute VB_Name = "RepPanel"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit

Private Sub Form_Load()
'**********************
Dim I As Integer
For I = 1 To 5
  If LevelFlag(I) Then
    LblUnitNumber(I - 1) = CStr(RepUnitNumber(I))
    LblParameterNum(I - 1) = CStr(ParameterNumber(I)) & " " & ParameterName(I)
    TxtstartingValue(I - 1) = CStr(StartValue(I))
    TxtEndingValue(I - 1) = CStr(EndValue(I))
    TxtStep(I - 1) = CStr(StepValue(I))
  Else
    LblUnitNumber(I - 1) = ""
    LblParameterNum(I - 1) = ""
    TxtstartingValue(I - 1) = ""
    TxtEndingValue(I - 1) = ""
    TxtStep(I - 1) = ""
  End If
Next I
End Sub

Private Sub Form_Unload(Cancel As Integer)
'*****************************************
Dim I As Integer
For I = 1 To 5
  If LevelFlag(I) Then
    StartValue(I) = Val(TxtstartingValue(I - 1).text)
    EndValue(I) = Val(TxtEndingValue(I - 1).text)
    StepValue(I) = Val(TxtStep(I - 1).text)
  End If
Next I
End Sub

Private Sub LblLevel_DblClick(Index As Integer)
'**********************************************
Dim resp As Integer
resp = MsgBox("Clear this level?", vbYesNo)
If resp = vbYes Then
  LevelFlag(Index + 1) = False
  LblUnitNumber(Index) = ""
  LblParameterNum(Index) = ""
  TxtstartingValue(Index) = ""
  TxtEndingValue(Index) = ""
  TxtStep(Index) = ""
End If
End Sub

Private Sub MnuAccept_Click()
'****************************
Dim I As Integer
For I = 0 To 4
  If Val(TxtEndingValue(I)) <> 0 And Val(TxtStep(I)) = 0 Then
    MsgBox "Step length must not be zero", vbCritical, "ERROR"
    TxtStep(I).SetFocus
    Exit Sub
  End If
Next I
Unload Me
End Sub

Private Sub MnuCancel_Click()
'****************************
Unload Me
End Sub

Private Sub MnuClear_Click()
'***************************
Dim I As Integer
For I = 0 To 4
  LevelFlag(I + 1) = False
  TxtstartingValue(I) = ""
  TxtEndingValue(I) = ""
  TxtStep(I) = ""
Next I
End Sub

Private Sub MnuPrintForm_Click()
'*******************************
PrintForm
End Sub
