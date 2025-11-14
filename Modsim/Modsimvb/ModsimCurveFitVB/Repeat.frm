VERSION 5.00
Begin VB.Form Repeat 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "Specify parameter value"
   ClientHeight    =   1920
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   4680
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1920
   ScaleWidth      =   4680
   StartUpPosition =   2  'CenterScreen
   Begin VB.CommandButton CmdClear 
      Caption         =   "Clear"
      Height          =   375
      Left            =   3840
      TabIndex        =   7
      Top             =   960
      Width           =   735
   End
   Begin VB.CommandButton CmdAccept 
      Caption         =   "Accept"
      Height          =   375
      Left            =   3840
      TabIndex        =   5
      Top             =   480
      Width           =   735
   End
   Begin VB.CommandButton CmdCancel 
      Caption         =   "Cancel"
      Height          =   375
      Left            =   3840
      TabIndex        =   6
      Top             =   1440
      Width           =   735
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   2
      Left            =   2640
      TabIndex        =   3
      Top             =   960
      Width           =   1095
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   1
      Left            =   2640
      TabIndex        =   2
      Top             =   720
      Width           =   1095
   End
   Begin VB.TextBox TxtValue 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Index           =   0
      Left            =   2640
      TabIndex        =   1
      Top             =   480
      Width           =   1095
   End
   Begin VB.ListBox LstLevel 
      Appearance      =   0  'Flat
      Height          =   1005
      Left            =   960
      TabIndex        =   4
      Top             =   480
      Width           =   375
   End
   Begin VB.TextBox TxtParameterName 
      Appearance      =   0  'Flat
      BorderStyle     =   0  'None
      Height          =   225
      Left            =   1200
      TabIndex        =   0
      Top             =   120
      Width           =   3255
   End
   Begin VB.Label Label3 
      Caption         =   "*Required for constrained searches only"
      Height          =   375
      Left            =   1440
      TabIndex        =   13
      Top             =   1320
      Width           =   2295
   End
   Begin VB.Label Label2 
      Caption         =   "Maximum value*"
      Height          =   255
      Index           =   2
      Left            =   1440
      TabIndex        =   12
      Top             =   960
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "Minimum value*"
      Height          =   255
      Index           =   1
      Left            =   1440
      TabIndex        =   11
      Top             =   720
      Width           =   1215
   End
   Begin VB.Label Label2 
      Caption         =   "Starting value"
      Height          =   255
      Index           =   0
      Left            =   1440
      TabIndex        =   10
      Top             =   480
      Width           =   975
   End
   Begin VB.Label Label1 
      Caption         =   "Parameter number"
      Height          =   615
      Left            =   120
      TabIndex        =   9
      Top             =   480
      Width           =   735
   End
   Begin VB.Label LblName 
      Caption         =   "Parameter name"
      Height          =   255
      Left            =   0
      TabIndex        =   8
      Top             =   120
      Width           =   1335
   End
End
Attribute VB_Name = "Repeat"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Option Explicit
Public Level As Integer

Private Sub CmdAccept_Click()
'****************************
If Me.LstLevel.ListIndex = -1 Then
  MsgBox "A number must be selected for this parameter", vbExclamation, "WARNING"
  Exit Sub
End If
Level = Val(LstLevel.text)
LstLevel.RemoveItem LstLevel.ListIndex
LevelFlag(Level) = True
ParameterName(Level) = TxtParameterName.text
StartValue(Level) = Val(TxtValue(0))
EndValue(Level) = Val(TxtValue(1))
StepValue(Level) = Val(TxtValue(2))
Unload Me
End Sub

Private Sub CmdCancel_Click()
'****************************
Level = 0
Unload Me
End Sub

Private Sub CmdClear_Click()
'***************************
Me.LstLevel.ListIndex = 0
Level = Val(LstLevel.text)
LevelFlag(Level) = False
UnitID(Level) = 0
ParameterNumber(Level) = 0
ParameterName(Level) = ""
StartValue(Level) = 0
EndValue(Level) = 0
StepValue(Level) = 1
Level = 0
Unload Me
End Sub

Private Sub Form_Load()
'**********************
Dim I As Integer
For I = 1 To 5
  If Not LevelFlag(I) Then LstLevel.AddItem CStr(I)
Next I
End Sub

Private Sub TxtParameterName_KeyPress(KeyAscii As Integer)
'*********************************************************
If KeyAscii = 13 Then
  TxtValue(0).SetFocus
  KeyAscii = 0
End If
End Sub

Private Sub TxtValue_KeyPress(Index As Integer, KeyAscii As Integer)
'*******************************************************************
If KeyAscii = 13 Then
  If Index < 2 Then
    TxtValue(Index + 1).SetFocus
  End If
  KeyAscii = 0
End If
End Sub
